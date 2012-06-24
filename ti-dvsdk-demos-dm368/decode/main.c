/*
 * main.c
 *
 * This source file has the main() for the 'decode demo' on DM365 platform
 *
 * Copyright (C) 2010 Texas Instruments Incorporated - http://www.ti.com/ 
 * 
 * 
 *  Redistribution and use in source and binary forms, with or without 
 *  modification, are permitted provided that the following conditions 
 *  are met:
 *
 *    Redistributions of source code must retain the above copyright 
 *    notice, this list of conditions and the following disclaimer.
 *
 *    Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the 
 *    documentation and/or other materials provided with the   
 *    distribution.
 *
 *    Neither the name of Texas Instruments Incorporated nor the names of
 *    its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 *  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 *  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <strings.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include <xdc/std.h>

#include <ti/sdo/ce/CERuntime.h>

#include <ti/sdo/dmai/Dmai.h>
#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "display.h"
#include "video.h"
#include "audio.h"
#include "speech.h"
#include "loader.h"
#include "../ctrl.h"
#include "../demo.h"
#include "../ui.h"

#include <ti/sdo/fc/rman/rman.h>

/* The levels of initialization */
#define LOGSINITIALIZED         0x1
#define DISPLAYTHREADCREATED    0x2
#define VIDEOTHREADCREATED      0x4
#define SPEECHTHREADCREATED     0x8
#define LOADERTHREADCREATED     0x10
#define AUDIOTHREADCREATED      0x20

/* Thread priorities */
#define LOADER_THREAD_PRIORITY  sched_get_priority_max(SCHED_FIFO) - 2
#define SPEECH_THREAD_PRIORITY  sched_get_priority_max(SCHED_FIFO) - 2
#define AUDIO_THREAD_PRIORITY   sched_get_priority_max(SCHED_FIFO) - 2
#define VIDEO_THREAD_PRIORITY   sched_get_priority_max(SCHED_FIFO) - 1
#define DISPLAY_THREAD_PRIORITY sched_get_priority_max(SCHED_FIFO) 

/* Maximum arguments length to the qtinterface application */
#define MAX_INTERFACE_ARGS_LENGTH 1000

/* Add argument number x of string y */
#define addArg(x, y)                     \
    argv[(x)] = malloc(strlen((y)) + 1); \
    if (argv[(x)] == NULL)               \
        return FAILURE;                  \
    strcpy(argv[(x)++], (y))

typedef struct Args {
    Display_Output displayOutput;
    VideoStd_Type  videoStd;
    Char          *videoStdString;
    Char          *speechFile;
    Char          *audioFile;
    Char          *videoFile;
    Codec         *audioDecoder;
    Codec         *speechDecoder;
    Codec         *videoDecoder;
    Int            loop;
    Int            keyboard;
    Int            time;
    Int            osd;
} Args;

#define DEFAULT_ARGS { Display_Output_COUNT, VideoStd_720P_60, "720P 60Hz", \
    NULL, NULL, NULL, NULL, NULL, NULL, FALSE, FALSE, FOREVER, FALSE }

/* Global variable declarations for this application */
GlobalData gbl = GBL_DATA_INIT;

/******************************************************************************
 * getCodec
 ******************************************************************************/
static Codec *getCodec(Char *extension, Codec *codecs)
{
    Codec *codec = NULL;
    Int i, j;

    i = 0;
    while (codecs[i].codecName) {
        j = 0;
        while (codecs[i].fileExtensions[j]) {
            if (strcmp(extension, codecs[i].fileExtensions[j]) == 0) {
                codec = &codecs[i];
            }
            j++;
        }
        i++;
    }

    return codec;
}

/******************************************************************************
 * usage
 ******************************************************************************/
static Void usage(void)
{
    fprintf(stderr, "Usage: decode [options]\n\n"
      "Options:\n"
      "-a | --audiofile        Audio file to play\n"
      "-s | --speechfile       Speech file to play\n"
      "-v | --videofile        Video file to play\n"
      "-y | --display_standard Video standard to use for display (see below).\n"
      "-O | --display_output   Video output to use (see below).\n"
      "-k | --keyboard         Enable keyboard interface [off]\n"
      "-t | --time             Number of seconds to run the demo [infinite]\n"
      "-l | --loop             Loop to beginning of files when done [off]\n"
      "-o | --osd              Show demo data on an OSD [off]\n"
      "-h | --help             Print this message\n\n"
      "Video standards available:\n"
      "\t1\tD1 @ 30 fps (NTSC)\n"
      "\t2\tD1 @ 25 fps (PAL)\n"
      "\t3\t720P @ 60 fps [Default]\n"
      "\t5\t1080I @ 30 fps - for DM368\n"
      "Video outputs available:\n"
      "\tcomposite\n"
      "\tcomponent (Only 720P and 1080I available) [Default]\n"
      "You must supply at least a video or a speech or an audio file\n"
      "with appropriate extensions for the file formats.\n"
      "You must NOT supply BOTH an audio and a speech file.\n\n");
}

/******************************************************************************
 * parseArgs
 ******************************************************************************/
static Void parseArgs(Int argc, Char *argv[], Args *argsp)
{
    const Char shortOptions[] = "a:s:v:y:O:kt:lfoh";
    const struct option longOptions[] = {
        {"audiofile",        required_argument, NULL, 'a'},
        {"speechfile",       required_argument, NULL, 's'},
        {"videofile",        required_argument, NULL, 'v'},
        {"display_standard", required_argument, NULL, 'y'},
        {"display_output",   required_argument, NULL, 'O'},
        {"keyboard",         no_argument,       NULL, 'k'},
        {"time",             required_argument, NULL, 't'},
        {"loop",             no_argument,       NULL, 'l'},
        {"osd",              no_argument,       NULL, 'o'},
        {"help",             no_argument,       NULL, 'h'},
        {"exit",             no_argument,       NULL, 'e'},            
        {0, 0, 0, 0}
    };

    Int   index;
    Int   c;
    Char *extension;

    for (;;) {
        c = getopt_long(argc, argv, shortOptions, longOptions, &index);

        if (c == -1) {
            break;
        }

        switch (c) {
            case 0:
                break;

            case 'a':
                extension = rindex(optarg, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Audio file without extension: %s\n",
                            optarg);
                    exit(EXIT_FAILURE);
                }

                argsp->audioDecoder =
                    getCodec(extension, engine->audioDecoders);

                if (!argsp->audioDecoder) {
                    fprintf(stderr, "Unknown audio file extension: %s\n",
                            extension);
                    exit(EXIT_FAILURE);
                }
                argsp->audioFile = optarg;

                break;

            case 's':
                extension = rindex(optarg, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Speech file without extension: %s\n",
                            optarg);
                    exit(EXIT_FAILURE);
                }

                argsp->speechDecoder =
                    getCodec(extension, engine->speechDecoders);

                if (!argsp->speechDecoder) {
                    fprintf(stderr, "Unknown speech file extension: %s\n",
                            extension);
                    exit(EXIT_FAILURE);
                }
                argsp->speechFile = optarg;

                break;

            case 'v':
                extension = rindex(optarg, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Video file without extension: %s\n",
                            optarg);
                    exit(EXIT_FAILURE);
                }

                argsp->videoDecoder =
                    getCodec(extension, engine->videoDecoders);

                if (!argsp->videoDecoder) {
                    fprintf(stderr, "Unknown video file extension: %s\n",
                            extension);
                    exit(EXIT_FAILURE);
                }
                argsp->videoFile = optarg;

                break;

            case 'y':
                switch (atoi(optarg)) {
                    case 1:
                        argsp->videoStd = VideoStd_D1_NTSC;
                        argsp->videoStdString = "D1 NTSC";
                        break;
                    case 2:
                        argsp->videoStd = VideoStd_D1_PAL;
                        argsp->videoStdString = "D1 PAL";
                        break;
                    case 3:
                        argsp->videoStd = VideoStd_720P_60;
                        argsp->videoStdString = "720P 60Hz";
                        break;
                    case 5:
                        argsp->videoStd = VideoStd_1080I_30;
                        argsp->videoStdString = "1080I 30Hz";
                        break;
                    default:
                        fprintf(stderr, "Unknown display resolution\n");
                        usage();
                        exit(EXIT_FAILURE);
                }
                break;

            case 'O':
                if (strcmp(optarg, "component") == 0) {
                    argsp->displayOutput = Display_Output_COMPONENT;
                } else if (strcmp(optarg, "composite") == 0) {
                    argsp->displayOutput = Display_Output_COMPOSITE;
                } else {
                    fprintf(stderr, "Unknown video output: %s\n", optarg);
                    usage();
                    exit(EXIT_FAILURE);
                }
                break;

            case 'k':
                argsp->keyboard = TRUE;
                break;

            case 't':
                argsp->time = atoi(optarg);
                break;

            case 'l':
                argsp->loop = TRUE;
                break;

            case 'o':
                argsp->osd = TRUE;
                break;

            case 'h':
                usage();
                exit(EXIT_SUCCESS);

            default:
                usage();
                exit(EXIT_FAILURE);
        }
    }

    if (argsp->displayOutput == Display_Output_COUNT) {
        if ((argsp->videoStd == VideoStd_D1_NTSC) || 
            (argsp->videoStd == VideoStd_D1_PAL)) {
            argsp->displayOutput = Display_Output_COMPOSITE;
        } else {
            argsp->displayOutput = Display_Output_COMPONENT;
        }
    }
}

/******************************************************************************
 * validateArgs
 ******************************************************************************/
static Int validateArgs(Args *argsp)
{
    /* Need at least one file to decode and only one sound file */
    if ((!argsp->videoFile && !(argsp->audioFile || argsp->speechFile)) ||
        (argsp->audioFile && argsp->speechFile)) {
        usage();
        return FAILURE;
    }
    
    return SUCCESS;
}

/******************************************************************************
 * uiSetup
 ******************************************************************************/
static Void uiSetup(UI_Handle hUI, Args *argsp)
{
    /* Initialize values */
    UI_updateValue(hUI, UI_Value_DemoName, "Decode");
    UI_updateValue(hUI, UI_Value_DisplayType, argsp->videoStdString);

    if (argsp->videoDecoder) {
        UI_updateValue(hUI, UI_Value_VideoCodec, argsp->videoDecoder->uiString);
    }
    else {
        UI_updateValue(hUI, UI_Value_VideoCodec, "N/A");
    }

    UI_updateValue(hUI, UI_Value_ImageResolution, "N/A");

    if (argsp->audioDecoder) {
        UI_updateValue(hUI, UI_Value_SoundCodec, argsp->audioDecoder->uiString);
    }
    else if (argsp->speechDecoder) {
        UI_updateValue(hUI, UI_Value_SoundCodec,
                       argsp->speechDecoder->uiString);
    }
    else {
        UI_updateValue(hUI, UI_Value_SoundCodec, "N/A");
    }

    UI_updateValue(hUI, UI_Value_SoundFrequency, "N/A");
}

/******************************************************************************
 * launchInterface
 ******************************************************************************/
static Int launchInterface(Args * argsp)
{
    Char *argv[MAX_INTERFACE_ARGS_LENGTH];
    Int i = 0;
    Int pid;

    /* 
     * Setup the command line to launch the qtInterface program with the 
     * appropriate arguments to inform it of the options that were already
     * set
     */
    addArg(i, "./qtInterface");
    addArg(i, "-qws");
    addArg(i, "-d");
    addArg(i, "Decode");

    if (argsp->speechFile) {
        addArg(i, "-s");
        addArg(i, argsp->speechFile);
    }

    if (argsp->videoFile) {
        addArg(i, "-v");
        addArg(i, argsp->videoFile);
    }

    if (argsp->audioFile) {
        addArg(i, "-a");
        addArg(i, argsp->audioFile);
    }

    if (argsp->loop) {
        addArg(i, "-l");
    }

    /* Provide Video Standard */
    addArg(i, "-y");
    switch(argsp->videoStd) {
        case VideoStd_D1_NTSC:
            addArg(i, "1");
            break;
        case VideoStd_D1_PAL:
            addArg(i, "2");
            break;
        case VideoStd_720P_60:
            addArg(i, "3");
            break;
        case VideoStd_1080I_30:
            addArg(i, "5");
            break;
        default:
            ERR("Invalid video standard\n");
            return FAILURE;
    }

    addArg(i, "-O");
    switch(argsp->displayOutput) {
        /*
         * Translate output into numeric argument to qtInterface
         * See qtInterface/main.cpp for correspondance.
         */
        case Display_Output_COMPONENT:
            addArg(i, "3");    
            break;
        case Display_Output_COMPOSITE:
            addArg(i, "1");
            break;
        default:
            ERR("Invalid display output\n");
            return FAILURE;
    }

    pid = fork();
    if (pid == -1) {
        ERR("Could not fork child process.\n");
        return FAILURE;
    }
    else if (pid == 0) {
        if (execv("./qtInterface", argv) == -1) {
            ERR("Could not execute QT Interface\n");
            return FAILURE;
        }
    }

    return SUCCESS;
}

/******************************************************************************
 * getConfigFromInterface
 ******************************************************************************/
static Int getConfigFromInterface(Args * argsp, UI_Handle hUI, Bool * stopped)
{
    Char * extension;
    Char * cfgString;
    Char option = 0;

    *stopped = FALSE;

    UI_getConfig(hUI, &option, &cfgString);

    /* Keep getting configuration strings until ETB */
    while (option != '\27') {
        switch(option) {
           case 'a':
                if (strcmp(cfgString, "") == 0) {
                    /* 
                     * If string is empty, cancel sound file options selected 
                     * on command line. 
                     */
                    argsp->audioDecoder = NULL;
                    argsp->audioFile = NULL;
                    argsp->speechDecoder = NULL;
                    argsp->speechFile = NULL;
                    break;
                }
                extension = rindex(cfgString, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Audio file without extension: %s\n",
                            optarg);
                    return FAILURE;
                }

                argsp->audioDecoder =
                    getCodec(extension, engine->audioDecoders);

                if (!argsp->audioDecoder) {
                    fprintf(stderr, "Unknown audio file extension: %s\n",
                            extension);
                    return FAILURE;
                }
                argsp->audioFile = cfgString;

                /* Override speech file if any selected on command line */
                argsp->speechDecoder = NULL;
                argsp->speechFile = NULL;

                break;

            case 's':
                if (strcmp(cfgString, "") == 0) {
                    /* 
                     * If string is empty, cancel sound file options selected 
                     * on command line. 
                     */
                    argsp->speechDecoder = NULL;
                    argsp->speechFile = NULL;
                    argsp->speechDecoder = NULL;
                    argsp->speechFile = NULL;
                    break;
                }
                extension = rindex(cfgString, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Speech file without extension: %s\n",
                            optarg);
                    return FAILURE;
                }

                argsp->speechDecoder =
                    getCodec(extension, engine->speechDecoders);

                if (!argsp->speechDecoder) {
                    fprintf(stderr, "Unknown speech file extension: %s\n",
                            extension);
                    return FAILURE;
                }
                argsp->speechFile = cfgString;

                /* Override audio file if any selected on command line */
                argsp->audioDecoder = NULL;
                argsp->audioFile = NULL;

                break;

            case 'v':
                if (strcmp(cfgString, "") == 0) {
                    /* 
                     * If string is empty, cancel video file option selected 
                     * on command line. 
                     */
                    argsp->videoDecoder = NULL;
                    argsp->videoFile = NULL;
                    break;
                }
                extension = rindex(cfgString, '.');
                if (extension == NULL) {
                    fprintf(stderr, "Video file without extension: %s\n",
                            optarg);
                    return FAILURE;
                }

                argsp->videoDecoder =
                    getCodec(extension, engine->videoDecoders);

                if (!argsp->videoDecoder) {
                    fprintf(stderr, "Unknown video file extension: %s\n",
                            extension);
                    return FAILURE;
                }
                argsp->videoFile = cfgString;

                break;

            case 'y':
                switch (atoi(cfgString)) {
                    case 1:
                        argsp->videoStd = VideoStd_D1_NTSC;
                        argsp->videoStdString = "D1 NTSC";
                        break;
                    case 2:
                        argsp->videoStd = VideoStd_D1_PAL;
                        argsp->videoStdString = "D1 PAL";
                        break;
                    case 3:
                        argsp->videoStd = VideoStd_720P_60;
                        argsp->videoStdString = "720P 60Hz";
                        break;
                    case 5:
                        argsp->videoStd = VideoStd_1080I_30;
                        argsp->videoStdString = "1080I 30Hz";
                        break;
                    default:
                        fprintf(stderr, "Invalid display standard set by"
                            " interface\n");
                        exit(EXIT_FAILURE);
                }
                break;

            case 'l':
                argsp->loop = TRUE;
                break;

            case '\33':
                *stopped = TRUE;
                return SUCCESS;

            default:
                ERR("Error in getting configuration from interface\n");
                return FAILURE;
        }
        UI_getConfig(hUI, &option, &cfgString);
    }
    return SUCCESS;
}

/******************************************************************************
 * main
 ******************************************************************************/
Int main(Int argc, Char *argv[])
{
    Args                    args                = DEFAULT_ARGS;
    Uns                     initMask            = 0;
    Int                     status              = EXIT_SUCCESS;
    Pause_Attrs             pAttrs              = Pause_Attrs_DEFAULT;
    Rendezvous_Attrs        rzvAttrs            = Rendezvous_Attrs_DEFAULT;
    Fifo_Attrs              fAttrs              = Fifo_Attrs_DEFAULT;
    UI_Attrs                uiAttrs;
    Rendezvous_Handle       hRendezvousInit     = NULL;
    Rendezvous_Handle       hRendezvousCleanup  = NULL;
    Rendezvous_Handle       hRendezvousLoop     = NULL;
    Rendezvous_Handle       hRendezvousLoader   = NULL;
    Pause_Handle            hPauseProcess       = NULL;
    Pause_Handle            hPausePrime         = NULL;
    UI_Handle               hUI                 = NULL;
    Int                     syncCnt             = 0;
    struct sched_param      schedParam;
    pthread_attr_t          attr;
    pthread_t               displayThread;
    pthread_t               videoThread;
    pthread_t               speechThread;
    pthread_t               audioThread;
    pthread_t               loaderThread;
    LoaderEnv               loaderEnv;
    DisplayEnv              displayEnv;
    VideoEnv                videoEnv;
    SpeechEnv               speechEnv;
    AudioEnv                audioEnv;
    CtrlEnv                 ctrlEnv;
    Int                     numThreads;
    Void                   *ret;
    Bool                    stopped;

    /* Zero out the thread environments */
    Dmai_clear(loaderEnv);
    Dmai_clear(displayEnv);
    Dmai_clear(videoEnv);
    Dmai_clear(speechEnv);
    Dmai_clear(audioEnv);
    Dmai_clear(ctrlEnv);

    /* Parse the arguments given to the app and set the app environment */
    parseArgs(argc, argv, &args);

    printf("Decode demo started.\n");

    /* Launch interface app */
    if (args.osd) {
        if (launchInterface(&args) == FAILURE) {
            exit(EXIT_FAILURE);
        }
    }

    /* Initialize the mutex which protects the global data */
    pthread_mutex_init(&gbl.mutex, NULL);

    /* Set the priority of this whole process to max (requires root) */
    setpriority(PRIO_PROCESS, 0, -20);

    /* Initialize Codec Engine runtime */
    CERuntime_init();

    /* Initialize Davinci Multimedia Application Interface */
    Dmai_init();

    initMask |= LOGSINITIALIZED;

    /* Create the user interface */
    uiAttrs.osd = args.osd;
    uiAttrs.videoStd = args.videoStd;

    hUI = UI_create(&uiAttrs);

    if (hUI == NULL) {
        cleanup(EXIT_FAILURE);
    }

    /* Get configuration from QT interface if necessary */
    if (args.osd) {
        status = getConfigFromInterface(&args, hUI, &stopped);
        if (status == FAILURE) {
            ERR("Failed to get valid configuration from the GUI\n");
            cleanup(EXIT_FAILURE);
        }
        else if (stopped == TRUE) {
            cleanup(EXIT_SUCCESS);
        }
    }

    /* Validate arguments */
    if (validateArgs(&args) == FAILURE) {
        cleanup(EXIT_FAILURE);
    }
    
    /* Set up the user interface */
    uiSetup(hUI, &args);
    
    /* Create the Pause objects */
    hPauseProcess = Pause_create(&pAttrs);
    hPausePrime = Pause_create(&pAttrs);

    if (hPauseProcess == NULL || hPausePrime == NULL) {
        ERR("Failed to create Pause objects\n");
        cleanup(EXIT_FAILURE);
    }

    /* Determine the number of threads needing synchronization */
    numThreads = 1;

    if (args.videoFile) {
        numThreads += 3;
        syncCnt++;
    }

    if (args.speechFile || args.audioFile) {
        numThreads += 1;
        syncCnt++;
    }

    /* Create the objects which synchronizes the thread init and cleanup */
    hRendezvousInit = Rendezvous_create(numThreads, &rzvAttrs);
    hRendezvousCleanup = Rendezvous_create(numThreads, &rzvAttrs);
    hRendezvousLoop = Rendezvous_create(syncCnt, &rzvAttrs);
    hRendezvousLoader = Rendezvous_create(2, &rzvAttrs);

    if (hRendezvousInit == NULL || hRendezvousCleanup == NULL ||
        hRendezvousLoop == NULL || hRendezvousLoader == NULL) {

        ERR("Failed to create Rendezvous objects\n");
        cleanup(EXIT_FAILURE);
    }

    /* Initialize the thread attributes */
    if (pthread_attr_init(&attr)) {
        ERR("Failed to initialize thread attrs\n");
        cleanup(EXIT_FAILURE);
    }

    /* Force the thread to use custom scheduling attributes */
    if (pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED)) {
        ERR("Failed to set schedule inheritance attribute\n");
        cleanup(EXIT_FAILURE);
    }

    /* Set the thread to be fifo real time scheduled */
    if (pthread_attr_setschedpolicy(&attr, SCHED_FIFO)) {
        ERR("Failed to set FIFO scheduling policy\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the video threads if a file name is supplied */
    if (args.videoFile) {
        /* Create the display fifos */
        displayEnv.hInFifo = Fifo_create(&fAttrs);
        displayEnv.hOutFifo = Fifo_create(&fAttrs);

        if (displayEnv.hInFifo == NULL || displayEnv.hOutFifo == NULL) {
            ERR("Failed to create display fifos\n");
            cleanup(EXIT_FAILURE);
        }

        /* Set the display thread priority */
        schedParam.sched_priority = DISPLAY_THREAD_PRIORITY;
        if (pthread_attr_setschedparam(&attr, &schedParam)) {
            ERR("Failed to set scheduler parameters\n");
            cleanup(EXIT_FAILURE);
        }

        /* Create the display thread */
        displayEnv.displayOutput      = args.displayOutput;        
        displayEnv.videoStd           = args.videoStd;
        displayEnv.hRendezvousInit    = hRendezvousInit;
        displayEnv.hRendezvousCleanup = hRendezvousCleanup;
        displayEnv.hPauseProcess      = hPauseProcess;
        displayEnv.hPausePrime        = hPausePrime;
        displayEnv.osd                = args.osd;

        if (pthread_create(&displayThread, &attr, displayThrFxn, &displayEnv)) {
            ERR("Failed to create display thread\n");
            cleanup(EXIT_FAILURE);
        }

        initMask |= DISPLAYTHREADCREATED;

        /* Set the video thread priority */
        schedParam.sched_priority = VIDEO_THREAD_PRIORITY;
        if (pthread_attr_setschedparam(&attr, &schedParam)) {
            ERR("Failed to set scheduler parameters\n");
            cleanup(EXIT_FAILURE);
        }

        /* Create the video thread */
        videoEnv.hRendezvousInit    = hRendezvousInit;
        videoEnv.hRendezvousCleanup = hRendezvousCleanup;
        videoEnv.hRendezvousLoop    = hRendezvousLoop;
        videoEnv.hRendezvousLoader  = hRendezvousLoader;
        videoEnv.hPauseProcess      = hPauseProcess;
        videoEnv.hPausePrime        = hPausePrime;
        videoEnv.hDisplayInFifo     = displayEnv.hInFifo;
        videoEnv.hDisplayOutFifo    = displayEnv.hOutFifo;
        videoEnv.videoFile          = args.videoFile;
        videoEnv.videoDecoder       = args.videoDecoder->codecName;
        videoEnv.params             = args.videoDecoder->params;
        videoEnv.dynParams          = args.videoDecoder->dynParams;
        videoEnv.loop               = args.loop;
        videoEnv.engineName         = engine->engineName;
        videoEnv.videoStd           = args.videoStd;        

        if (pthread_create(&videoThread, &attr, videoThrFxn, &videoEnv)) {
            ERR("Failed to create video thread\n");
            cleanup(EXIT_FAILURE);
        }

        initMask |= VIDEOTHREADCREATED;

        /*
         * Wait for the Loader to be created in the video thread before
         * launching the loader thread.
         */
        Rendezvous_meet(hRendezvousLoader);

        /* Set the loader thread priority */
        schedParam.sched_priority = LOADER_THREAD_PRIORITY;
        if (pthread_attr_setschedparam(&attr, &schedParam)) {
            ERR("Failed to set scheduler parameters\n");
            return -1;
        }

        /* Create the loader thread */
        loaderEnv.hRendezvousInit    = hRendezvousInit;
        loaderEnv.hRendezvousCleanup = hRendezvousCleanup;
        loaderEnv.loop               = args.loop;
        loaderEnv.hLoader            = videoEnv.hLoader;

        if (pthread_create(&loaderThread, &attr, loaderThrFxn, &loaderEnv)) {
            ERR("Failed to create loader thread\n");
            cleanup(EXIT_FAILURE);
        }

        initMask |= LOADERTHREADCREATED;
    }

    /* Create the speech thread if a file name is supplied */
    if (args.speechFile) {
        /* Set the thread priority */
        schedParam.sched_priority = SPEECH_THREAD_PRIORITY;
        if (pthread_attr_setschedparam(&attr, &schedParam)) {
            ERR("Failed to set scheduler parameters\n");
            cleanup(EXIT_FAILURE);
        }

        /* Create the speech thread */
        speechEnv.hRendezvousInit       = hRendezvousInit;
        speechEnv.hRendezvousCleanup    = hRendezvousCleanup;
        speechEnv.hRendezvousLoop       = hRendezvousLoop;
        speechEnv.hPauseProcess         = hPauseProcess;
        speechEnv.speechFile            = args.speechFile;
        speechEnv.speechDecoder         = args.speechDecoder->codecName;
        speechEnv.params                = args.speechDecoder->params;
        speechEnv.dynParams             = args.speechDecoder->dynParams;
        speechEnv.loop                  = args.loop;
        speechEnv.engineName            = engine->engineName;

        if (pthread_create(&speechThread, &attr, speechThrFxn, &speechEnv)) {
            ERR("Failed to create speech thread\n");
            cleanup(EXIT_FAILURE);
        }

        initMask |= SPEECHTHREADCREATED;
    }

    /* Create the audio thread if a file name is supplied */
    if (args.audioFile) {
        /* Set the thread priority */
        schedParam.sched_priority = AUDIO_THREAD_PRIORITY;
        if (pthread_attr_setschedparam(&attr, &schedParam)) {
            ERR("Failed to set scheduler parameters\n");
            cleanup(EXIT_FAILURE);
        }

        /* Create the audio thread */
        audioEnv.hRendezvousInit    = hRendezvousInit;
        audioEnv.hRendezvousCleanup = hRendezvousCleanup;
        audioEnv.hRendezvousLoop    = hRendezvousLoop;
        audioEnv.hPauseProcess      = hPauseProcess;
        audioEnv.audioFile          = args.audioFile;
        audioEnv.audioDecoder       = args.audioDecoder->codecName;
        audioEnv.params             = args.audioDecoder->params;
        audioEnv.dynParams          = args.audioDecoder->dynParams;
        audioEnv.loop               = args.loop;
        audioEnv.engineName         = engine->engineName;

        if (pthread_create(&audioThread, &attr, audioThrFxn, &audioEnv)) {
            ERR("Failed to create audio thread\n");
            cleanup(EXIT_FAILURE);
        }

        initMask |= AUDIOTHREADCREATED;
    }

    /* Main thread becomes the control thread */
    ctrlEnv.hRendezvousInit    = hRendezvousInit;
    ctrlEnv.hRendezvousCleanup = hRendezvousCleanup;
    ctrlEnv.hPauseProcess      = hPauseProcess;
    ctrlEnv.keyboard           = args.keyboard;
    ctrlEnv.time               = args.time;
    ctrlEnv.hUI                = hUI;
    ctrlEnv.engineName         = engine->engineName;
    ctrlEnv.osd                = args.osd;

    ret = ctrlThrFxn(&ctrlEnv);

    if (ret == THREAD_FAILURE) {
        status = EXIT_FAILURE;
    }

cleanup:
    if (args.osd) {
        int rv;
        if (hUI) {
            /* Stop the UI */
            UI_stop(hUI);
        }
        wait(&rv);      /* Wait for child process to end */
    }

    /* Make sure the other threads aren't waiting for us */
    if (hRendezvousInit) Rendezvous_force(hRendezvousInit);
    if (hRendezvousLoader) Rendezvous_force(hRendezvousLoader);
    if (hRendezvousLoop) Rendezvous_force(hRendezvousLoop);
    if (hPauseProcess) Pause_off(hPauseProcess);
    if (hPausePrime) Pause_off(hPausePrime);

     /* Wait until the other threads terminate */
    if (initMask & AUDIOTHREADCREATED) {
        if (pthread_join(audioThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

   if (initMask & SPEECHTHREADCREATED) {
        if (pthread_join(speechThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (initMask & LOADERTHREADCREATED) {
        if (pthread_join(loaderThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (initMask & VIDEOTHREADCREATED) {
        if (pthread_join(videoThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (initMask & DISPLAYTHREADCREATED) {
        if (pthread_join(displayThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (displayEnv.hOutFifo) {
        Fifo_delete(displayEnv.hOutFifo);
    }

    if (displayEnv.hInFifo) {
        Fifo_delete(displayEnv.hInFifo);
    }

    if (hRendezvousLoop) {
        Rendezvous_delete(hRendezvousLoop);
    }

    if (hRendezvousCleanup) {
        Rendezvous_delete(hRendezvousCleanup);
    }

    if (hRendezvousInit) {
        Rendezvous_delete(hRendezvousInit);
    }

    if (hPauseProcess) {
        Pause_delete(hPauseProcess);
    }

    if (hPausePrime) {
        Pause_delete(hPausePrime);
    }

    if (hUI) {
        UI_delete(hUI);
    }

    /* 
     * In the past, there were instances where we have seen system memory
     * continually reduces by 28 bytes at a time whenever there are file 
     * reads or file writes. This is for the application to recapture that
     * memory (SDOCM00054899)
     */
    system("sync");
    system("echo 3 > /proc/sys/vm/drop_caches");
    
    pthread_mutex_destroy(&gbl.mutex);

    exit(status);
}

