/*
 * main.c
 *
 * This source file has the main() for the 'encodedecode demo' on DM365 platform
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

#include <ti/sdo/ce/trace/gt.h>
#include <ti/sdo/ce/CERuntime.h>

#include <ti/sdo/fc/rman/rman.h>

#include <ti/sdo/dmai/Dmai.h>
#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Capture.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "display.h"
#include "capture.h"
#include "video.h"
#include "../ctrl.h"
#include "../demo.h"

/* The levels of initialization */
#define DISPLAYTHREADCREATED    0x2
#define CAPTURETHREADCREATED    0x4
#define VIDEOTHREADCREATED      0x8

/* Thread priorities */
#define CAPTURE_THREAD_PRIORITY sched_get_priority_max(SCHED_FIFO) - 1
#define VIDEO_THREAD_PRIORITY   sched_get_priority_max(SCHED_FIFO) - 1
#define DISPLAY_THREAD_PRIORITY sched_get_priority_max(SCHED_FIFO)

/* Add argument number x of string y */
#define addArg(x, y)                     \
    argv[(x)] = malloc(strlen((y)) + 1); \
    if (argv[(x)] == NULL)               \
        return FAILURE;                  \
    strcpy(argv[(x)++], (y))

typedef struct Args {
    VideoStd_Type           videoStd;
    Char                   *videoStdString;
    Int                     videoBitRate;
    Char                   *videoBitRateString;
    VideoCodec              videoCodec;
    Char                   *videoCodecString;
    Capture_Input           videoInput;
    Int                     passThrough;
    Int                     keyboard;
    Int                     time;
    Int                     osd;
    Int32                   imageWidth;
    Int32                   imageHeight;
} Args;

#define DEFAULT_ARGS \
    { VideoStd_720P_60, "720P 60Hz", -1, NULL, MPEG4, NULL, \
      Capture_Input_COUNT, FALSE, FALSE, FOREVER, FALSE, 0, 0}

/* Global variable declarations for this application */
GlobalData gbl = GBL_DATA_INIT;

/******************************************************************************
 * usage
 ******************************************************************************/
static Void usage(Void)
{
    fprintf(stderr, "Usage: encodedecode [options]\n\n"
      "Options:\n"
      "-y | --display_standard Video standard to use for display (see below)\n"
      "-r | --resolution       Captured video resolution ('width'x'height')\n"
      "                        [video standard default]\n"
      "-b | --bitrate          Bit rate to encode video at [variable]\n"
      "-v | --videocodec       Video codec to use mpeg4/h264/mpeg2 [mpeg4]\n"
      "-p | --passthrough      Pass video through without encoding [off]\n"
      "-I | --video_input      Video input source [video standard default]\n"
      "-k | --keyboard         Enable keyboard interface [off]\n"
      "-t | --time             Number of seconds to run the demo [infinite]\n"
      "-o | --osd              Show demo data on an OSD [off]\n"
      "-h | --help             Print this message\n\n"
      "Video standards available:\n"
      "\t1\tD1 @ 30 fps (NTSC)\n"
      "\t2\tD1 @ 25 fps (PAL)\n"
      "\t3\t720P @ 60 fps [Default]\n"
      "\t7\t480P @ 60 fps\n"
      "Video inputs available:\n"
      "\t1\tComposite\n"
      "\t2\tS-video\n"
      "\t3\tComponent\n"
      "\t4\tImager/Camera - for DM368\n");
}

/******************************************************************************
 * parseArgs
 ******************************************************************************/
static Void parseArgs(Int argc, Char *argv[], Args *argsp)
{
    const Char shortOptions[] = "y:r:b:v:dpI:kt:oh";
    const struct option longOptions[] = {
        {"display_standard", required_argument, NULL, 'y'},
        {"resolution",       required_argument, NULL, 'r'},
        {"bitrate",          required_argument, NULL, 'b'},
        {"videocodec",       required_argument, NULL, 'v'},
        {"passthrough",      no_argument,       NULL, 'p'},
        {"video_input",      required_argument, NULL, 'I'},
        {"keyboard",         no_argument,       NULL, 'k'},
        {"time",             required_argument, NULL, 't'},
        {"osd",              no_argument,       NULL, 'o'},
        {"help",             no_argument,       NULL, 'h'},
        {0, 0, 0, 0}
    };
    Int     index;
    Int     c;

    for (;;) {
        c = getopt_long(argc, argv, shortOptions, longOptions, &index);

        if (c == -1) {
            break;
        }

        switch (c) {
            case 0:
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
                    case 7:
                        argsp->videoStd = VideoStd_480P;
                        argsp->videoStdString = "480P 60Hz";
                        break;
                    default:
                        fprintf(stderr, "Unsupported display resolution\n\n");
                        usage();
                        exit(EXIT_FAILURE);
                }
                break;

            case 'I':
                switch (atoi(optarg)) {
                    case 1:
                        argsp->videoInput = Capture_Input_COMPOSITE;
                        break;
                    case 2:
                        argsp->videoInput = Capture_Input_SVIDEO;
                        break;
                    case 3:
                        argsp->videoInput = Capture_Input_COMPONENT;
                        break;
                    case 4:
                        argsp->videoInput = Capture_Input_CAMERA;
                        break;
                    default:
                        fprintf(stderr, "Unknown video input\n");
                        usage();
                        exit(EXIT_FAILURE);
                }
                break;

            case 'r':
            {
                if (sscanf(optarg, "%ldx%ld", &argsp->imageWidth,
                                              &argsp->imageHeight) != 2) {
                    fprintf(stderr, "Invalid resolution supplied (%s)\n",
                            optarg);
                    usage();
                    exit(EXIT_FAILURE);
                }
                /* Sanity check resolution */
                if (argsp->imageWidth < 2UL || argsp->imageHeight < 2UL ||
                    argsp->imageWidth > VideoStd_720P_WIDTH ||
                    argsp->imageHeight > VideoStd_720P_HEIGHT) {
                    fprintf(stderr, "Video resolution must be between %dx%d "
                            "and %dx%d\n", 2, 2, VideoStd_720P_WIDTH,
                            VideoStd_720P_HEIGHT);
                    exit(EXIT_FAILURE);
                }
                break;
            }

            case 'b':
                argsp->videoBitRate = atoi(optarg);
                argsp->videoBitRateString = optarg;
                break;
            
            case 'v':
                if (strcmp(optarg, "h264") == 0) {
                    argsp->videoCodec = H264;
                    argsp->videoCodecString = optarg;
                }
                else if (strcmp(optarg, "mpeg4") == 0) {
                    argsp->videoCodec = MPEG4;
                    argsp->videoCodecString = optarg;
                }
                else if (strcmp(optarg, "mpeg2") == 0) {
                    argsp->videoCodec = MPEG2;
                    argsp->videoCodecString = optarg;
                }
                else {
                    usage();
                    exit(EXIT_FAILURE);
                }
                break; 

            case 'p':
                argsp->passThrough = TRUE;
                break;

            case 'k':
                argsp->keyboard = TRUE;
                break;

            case 't':
                argsp->time = atoi(optarg);
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
    if ((argsp->videoStd == VideoStd_480P) && (argsp->imageWidth == 0)){
        argsp->imageWidth = VideoStd_480P_WIDTH;
        argsp->imageHeight = VideoStd_480P_HEIGHT;
    }
       
    /* 
     * If video input is not set, set it to the default based on display
     * video standard.
     */
    if (argsp->videoInput == Capture_Input_COUNT) {
        switch (argsp->videoStd) {
            case VideoStd_D1_NTSC:
            case VideoStd_D1_PAL:
                argsp->videoInput = Capture_Input_COMPOSITE;
                break;
            case VideoStd_720P_60:
            case VideoStd_480P:
                argsp->videoInput = Capture_Input_COMPONENT;
                break;
            default:
                fprintf(stderr, "Unknown display standard\n");
                usage();
                exit(EXIT_FAILURE);
                break;
        }
    } 
}

/******************************************************************************
 * uiSetup
 ******************************************************************************/
static Void uiSetup(UI_Handle hUI, Args *argsp)
{
    /* Initialize values */
    UI_updateValue(hUI, UI_Value_DemoName, "Encode Decode");
    UI_updateValue(hUI, UI_Value_DisplayType, argsp->videoStdString);

    if (argsp->passThrough) {
        UI_updateValue(hUI, UI_Value_VideoCodec, "N/A");
        UI_setRow(hUI, UI_Value_VideoKbps, UI_Row_NONE);
    }
    else {
        UI_updateValue(hUI, UI_Value_VideoCodec,
                       engine->videoDecoders[argsp->videoCodec].uiString);
    }

    /* No sound support */
    UI_setRow(hUI, UI_Value_SoundCodec, UI_Row_NONE);
    UI_setRow(hUI, UI_Value_SoundKbps, UI_Row_NONE);
    UI_setRow(hUI, UI_Value_SoundFrequency, UI_Row_NONE);
    UI_setRow(hUI, UI_Value_Time, UI_Row_5);
}

/******************************************************************************
 * launchInterface
 ******************************************************************************/
static Int launchInterface(Args * argsp)
{
    Char *argv[1000];
    Int i = 0;
    Int pid;

    addArg(i, "./qtInterface");
    addArg(i, "-qws");
    addArg(i, "-d");
    addArg(i, "Encodedecode");

    if (argsp->passThrough) {
        addArg(i, "-p");
    }

    if (argsp->videoCodecString) {
        addArg(i, "-v");
        addArg(i, argsp->videoCodecString);
    }

    if (argsp->videoBitRateString) {
        addArg(i, "-b");
        addArg(i, argsp->videoBitRateString);
    }

    /* 
     * Provide Video Standard and set display output
     * based on it. See qtInterface/main.cpp for semantics of numerical
     * arguments used.
     */
    addArg(i, "-y");
    switch(argsp->videoStd) {
        case VideoStd_D1_NTSC:
            addArg(i, "1");
            addArg(i, "-O");
            addArg(i, "1"); /* Composite */
            break;
        case VideoStd_D1_PAL:
            addArg(i, "2");
            addArg(i, "-O");
            addArg(i, "1"); /* Composite */
            break;
        case VideoStd_720P_60:
            addArg(i, "3");
            addArg(i, "-O");
            addArg(i, "3"); /* Component */
            break;
        case VideoStd_480P:
            addArg(i, "7");
            addArg(i, "-O");
            addArg(i, "3"); /* Component */
            break;
        default:
            ERR("Invalid video standard\n");
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
    Char * cfgString;
    Char option = 0;

    *stopped = FALSE;

    UI_getConfig(hUI, &option, &cfgString);

    /* Keep getting configuration strings until ETB */
    while (option != '\27') {
        switch(option) {
            case 'b':
                argsp->videoBitRate = atoi(cfgString);
                argsp->videoBitRateString = cfgString;
                break;
            
            case 'v':
                if (strcmp(cfgString, "h264") == 0) {
                    argsp->videoCodec = H264;
                    argsp->videoCodecString = cfgString;
                }
                else if (strcmp(cfgString, "mpeg4") == 0) {
                    argsp->videoCodec = MPEG4;
                    argsp->videoCodecString = cfgString;
                }
                else if (strcmp(cfgString, "mpeg2") == 0) {
                    argsp->videoCodec = MPEG2;
                    argsp->videoCodecString = cfgString;
                }
                else {
                    usage();
                    exit(EXIT_FAILURE);
                }
                break; 

            case 'p':
                argsp->passThrough = TRUE;
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
 * validateArgs
 ******************************************************************************/
static Int validateArgs(Args *argsp)
{
    Bool failed = FALSE;

    /* Verify video standard is supported by input */
    switch (argsp->videoInput) {
        case Capture_Input_COMPOSITE:
        case Capture_Input_SVIDEO:
            if ((argsp->videoStd != VideoStd_D1_PAL) && (argsp->videoStd !=
                VideoStd_D1_NTSC)) {
                failed = TRUE;
            }
            break;
        case Capture_Input_COMPONENT:
            if ((argsp->videoStd != VideoStd_720P_60)&&(argsp->videoStd != 
                VideoStd_480P)) {
                failed = TRUE;
            }
            break;
        case Capture_Input_CAMERA:
            break;
        default:
            fprintf(stderr, "Invalid video input found in validation.\n");
            usage();
            return FAILURE;
    }

    if (failed) {
            fprintf(stderr, "This combination of video input and video" 
                "standard is not supported.\n");
            usage();
            return FAILURE;
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
    Rendezvous_Handle       hRendezvousCapStd   = NULL;
    Rendezvous_Handle       hRendezvousInit     = NULL;
    Rendezvous_Handle       hRendezvousCleanup  = NULL;
    Pause_Handle            hPauseProcess       = NULL;
    Pause_Handle            hPausePrime         = NULL;
    UI_Handle               hUI                 = NULL;
    struct sched_param      schedParam;
    Int                     numThreads;
    pthread_t               videoThread;
    pthread_t               displayThread;
    pthread_t               captureThread;
    pthread_attr_t          attr;
    VideoEnv                videoEnv;
    CaptureEnv              captureEnv;
    DisplayEnv              displayEnv;
    CtrlEnv                 ctrlEnv;
    Void                   *ret;
    Bool                    stopped;

    /* Zero out the thread environments */
    Dmai_clear(videoEnv);
    Dmai_clear(captureEnv);
    Dmai_clear(displayEnv);
    Dmai_clear(ctrlEnv);

    /* Parse the arguments given to the app and set the app environment */
    parseArgs(argc, argv, &args);

    printf("Encodedecode demo started.\n");

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

    numThreads = 4;

    /* Create the objects which synchronizes the thread init and cleanup */
    hRendezvousCapStd  = Rendezvous_create(2, &rzvAttrs);
    hRendezvousInit = Rendezvous_create(numThreads, &rzvAttrs);
    hRendezvousCleanup = Rendezvous_create(numThreads, &rzvAttrs);

    if (hRendezvousCapStd  == NULL || hRendezvousInit == NULL || 
        hRendezvousCleanup == NULL) {
        ERR("Failed to create Rendezvous objects\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the display fifos */
    displayEnv.hInFifo = Fifo_create(&fAttrs);
    displayEnv.hOutFifo = Fifo_create(&fAttrs);

    if (displayEnv.hInFifo == NULL || displayEnv.hOutFifo == NULL) {
        ERR("Failed to create display fifos\n");
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

    /* Set the display thread priority */
    schedParam.sched_priority = DISPLAY_THREAD_PRIORITY;
    if (pthread_attr_setschedparam(&attr, &schedParam)) {
        ERR("Failed to set scheduler parameters\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the display thread */
    displayEnv.videoStd           = args.videoStd;
    displayEnv.hRendezvousInit    = hRendezvousInit;
    displayEnv.hRendezvousCleanup = hRendezvousCleanup;
    displayEnv.hPauseProcess      = hPauseProcess;
    displayEnv.hPausePrime        = hPausePrime;
    displayEnv.osd                = args.osd;
    displayEnv.imageWidth         = args.imageWidth;
    displayEnv.imageHeight        = args.imageHeight;
    displayEnv.passThrough        = args.passThrough;

    if (pthread_create(&displayThread, &attr, displayThrFxn, &displayEnv)) {
        ERR("Failed to create display thread\n");
        cleanup(EXIT_FAILURE);
    }

    initMask |= DISPLAYTHREADCREATED;

    /* Set the capture thread priority */
    schedParam.sched_priority = CAPTURE_THREAD_PRIORITY;
    if (pthread_attr_setschedparam(&attr, &schedParam)) {
        ERR("Failed to set scheduler parameters\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the capture fifos */
    captureEnv.hInFifo = Fifo_create(&fAttrs);
    captureEnv.hOutFifo = Fifo_create(&fAttrs);

    if (captureEnv.hInFifo == NULL || captureEnv.hOutFifo == NULL) {
        ERR("Failed to create capture fifos\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the capture thread */
    captureEnv.hRendezvousInit    = hRendezvousInit;
    captureEnv.hRendezvousCapStd  = hRendezvousCapStd;
    captureEnv.hRendezvousCleanup = hRendezvousCleanup;
    captureEnv.hPauseProcess      = hPauseProcess;
    captureEnv.imageWidth         = args.imageWidth;
    captureEnv.imageHeight        = args.imageHeight;
    captureEnv.videoInput         = args.videoInput;
    captureEnv.passThrough        = args.passThrough;
    captureEnv.videoStd           = args.videoStd;
    if (pthread_create(&captureThread, &attr, captureThrFxn, &captureEnv)) {
        ERR("Failed to create capture thread\n");
        cleanup(EXIT_FAILURE);
    }

    initMask |= CAPTURETHREADCREATED;
        /*
         * Once the capture thread has deteceted the vide standard, make it
         * available to other threads and update the user interface.  The
         * capture thread will set the detected video standard in the capture
         * environment.
         */
    Rendezvous_meet(hRendezvousCapStd);
    /* Set the video thread priority */
    schedParam.sched_priority = VIDEO_THREAD_PRIORITY;
    if (pthread_attr_setschedparam(&attr, &schedParam)) {
        ERR("Failed to set scheduler parameters\n");
        cleanup(EXIT_FAILURE);
    }

    /* Create the video thread */
    videoEnv.hRendezvousInit    = hRendezvousInit;
    videoEnv.hRendezvousCleanup = hRendezvousCleanup;
    videoEnv.hPauseProcess      = hPauseProcess;
    videoEnv.hPausePrime        = hPausePrime;
    videoEnv.hCaptureOutFifo    = captureEnv.hOutFifo;
    videoEnv.hCaptureInFifo     = captureEnv.hInFifo;
    videoEnv.hDisplayOutFifo    = displayEnv.hOutFifo;
    videoEnv.hDisplayInFifo     = displayEnv.hInFifo;
    videoEnv.videoBitRate       = args.videoBitRate;
    videoEnv.passThrough        = args.passThrough;
    videoEnv.engineName         = engine->engineName;
    videoEnv.videoDecoder       = engine->videoDecoders[args.videoCodec].codecName;
    videoEnv.decParams          = engine->videoDecoders[args.videoCodec].params;
    videoEnv.decDynParams       = engine->videoDecoders[args.videoCodec].dynParams;
    videoEnv.videoEncoder       = engine->videoEncoders[args.videoCodec].codecName;
    videoEnv.encParams          = engine->videoEncoders[args.videoCodec].params;
    videoEnv.encDynParams       = engine->videoEncoders[args.videoCodec].dynParams;
    videoEnv.imageWidth         = captureEnv.imageWidth;
    videoEnv.imageHeight        = captureEnv.imageHeight;
    videoEnv.videoStd           = args.videoStd;    

    if (pthread_create(&videoThread, &attr, videoThrFxn, (Void *) &videoEnv)) {
        ERR("Failed to create video thread\n");
        cleanup(EXIT_FAILURE);
    }

    initMask |= VIDEOTHREADCREATED;

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
    if (hRendezvousCapStd) Rendezvous_force(hRendezvousCapStd);
    if (hRendezvousInit) Rendezvous_force(hRendezvousInit);
    if (hPauseProcess) Pause_off(hPauseProcess);
    if (hPausePrime) Pause_off(hPausePrime);

    /* Wait for the threads to complete */
    if (initMask & VIDEOTHREADCREATED) {
        if (pthread_join(videoThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (initMask & CAPTURETHREADCREATED) {
        if (pthread_join(captureThread, &ret) == 0) {
            if (ret == THREAD_FAILURE) {
                status = EXIT_FAILURE;
            }
        }
    }

    if (captureEnv.hOutFifo) {
        Fifo_delete(captureEnv.hOutFifo);
    }

    if (captureEnv.hInFifo) {
        Fifo_delete(captureEnv.hInFifo);
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
