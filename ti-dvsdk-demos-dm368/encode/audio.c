/*
 * audio.c
 *
 * ============================================================================
 * Copyright (c) Texas Instruments Inc 2009
 *
 * Use of this software is controlled by the terms and conditions found in the
 * license agreement under which this software has been supplied or provided.
 * ============================================================================
 */

#include <stdio.h>
#include <string.h>

#include <xdc/std.h>

#include <ti/sdo/ce/Engine.h>

#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Sound.h>
#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/Loader.h>
#include <ti/sdo/dmai/Rendezvous.h>
#include <ti/sdo/dmai/ce/Aenc1.h>


#include "audio.h"
#include "../demo.h"

/******************************************************************************
 * audioThrFxn
 ******************************************************************************/
Void *audioThrFxn(Void *arg)
{
    AudioEnv               *envp                = (AudioEnv *) arg;
    Void                   *status              = THREAD_SUCCESS;
    Sound_Attrs             sAttrs              = Sound_Attrs_STEREO_DEFAULT;
    Buffer_Attrs            bAttrs              = Buffer_Attrs_DEFAULT;
    AUDENC1_Params          defaultParams       = Aenc1_Params_DEFAULT;
    AUDENC1_DynamicParams   defaultDynParams    = Aenc1_DynamicParams_DEFAULT;
    Engine_Handle           hEngine             = NULL;
    Sound_Handle            hSound              = NULL;
    Aenc1_Handle            hAe1                = NULL;
    Buffer_Handle           hOutBuf             = NULL;
    Buffer_Handle           hInBuf              = NULL;
    Buffer_Handle           hEncInBuf           = NULL;
    FILE                   *outFile             = NULL;
    AUDENC1_Params         *params;
    AUDENC1_DynamicParams  *dynParams;
    static Int offset = 0;
    static Int wmacodec = 0;


    /* Open the output file for writing */
    outFile = fopen(envp->audioFile, "w");

    if (outFile == NULL) {
        ERR("Failed to open %s for writing\n", envp->audioFile);
        cleanup(THREAD_FAILURE);
    }

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams;

    params->sampleRate = dynParams->sampleRate = envp->sampleRate;
    params->bitRate = dynParams->bitRate = envp->soundBitRate;
    
    /* Create the audio encoder */
    hAe1 = Aenc1_create(hEngine, envp->audioEncoder, params, dynParams);

    if (hAe1 == NULL) {
        ERR("Failed to create audio encoder: %s\n", envp->audioEncoder);
        cleanup(THREAD_FAILURE);
    }

    /* Ask the codec how much space it needs for output data */
    hOutBuf = Buffer_create(Aenc1_getOutBufSize(hAe1), &bAttrs);

    /* Ask the codec how much input data it needs */
    hInBuf = Buffer_create(Aenc1_getInBufSize(hAe1), &bAttrs);
    
    /* Buffer specifically for WMA to read 8KBytes of data*/
    hEncInBuf = Buffer_create(Aenc1_getInBufSize(hAe1)/4, &bAttrs);


    if (hInBuf == NULL || hOutBuf == NULL || hEncInBuf == NULL) {
        ERR("Failed to allocate audio buffers\n");
        cleanup(THREAD_FAILURE);
    }

    /* Set the sample rate for the user interface */
    gblSetSamplingFrequency(envp->sampleRate);

    /* Create the sound device */
    sAttrs.sampleRate = envp->sampleRate;
    sAttrs.soundInput = envp->soundInput;
    sAttrs.mode = Sound_Mode_INPUT;
    hSound = Sound_create(&sAttrs);

    if (hSound == NULL) {
        ERR("Failed to create audio device\n");
        cleanup(THREAD_FAILURE);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);


    while (!gblGetQuit()) {
        /* Pause processing? */
        Pause_test(envp->hPauseProcess);
        if(!wmacodec)
        {       
            /* Read samples from the Sound device */
            if (Sound_read(hSound, hInBuf) < 0) {
                ERR("Failed to write audio buffer\n");
                cleanup(THREAD_FAILURE);
            }
            /* Encode the audio buffer */
            if (Aenc1_process(hAe1, hInBuf, hOutBuf) < 0) {
                ERR("Failed to encode audio buffer\n");
                cleanup(THREAD_FAILURE);
            }
            if(!strcmp(envp->audioEncoder,"wmaenc"))
            {
            if (Buffer_getNumBytesUsed(hInBuf)) {
                  memcpy(Buffer_getUserPtr(hInBuf),Buffer_getUserPtr(hInBuf)+ 
                      Buffer_getNumBytesUsed(hInBuf), 
                      Buffer_getSize(hInBuf)-Buffer_getNumBytesUsed(hInBuf));
                  offset = Buffer_getSize(hInBuf)-Buffer_getNumBytesUsed(hInBuf);
            }
            else
            offset = 0;
          
            wmacodec = 1;
            }
      }
      else
      {
            /* For the wma encoder each time 8 Kbytes will be read and appended to the input */

            /* Read samples from the Sound device */
            if (Sound_read(hSound, hEncInBuf) < 0) {
                ERR("Failed to write audio buffer\n");
                cleanup(THREAD_FAILURE);
            }

            memcpy(Buffer_getUserPtr(hInBuf) + offset, Buffer_getUserPtr(hEncInBuf),Buffer_getNumBytesUsed(hEncInBuf));
            Buffer_setNumBytesUsed(hInBuf,Buffer_getSize(hInBuf));

            /* Encode the audio buffer */
            if (Aenc1_process(hAe1, hInBuf, hOutBuf) < 0) {
                ERR("Failed to encode audio buffer\n");
                cleanup(THREAD_FAILURE);
            }
            if (Buffer_getNumBytesUsed(hEncInBuf)) {
                memcpy(Buffer_getUserPtr(hEncInBuf),Buffer_getUserPtr(hEncInBuf) + Buffer_getNumBytesUsed(hEncInBuf), Buffer_getSize(hEncInBuf)  - Buffer_getNumBytesUsed(hEncInBuf));
                offset = Buffer_getSize(hEncInBuf)  - Buffer_getNumBytesUsed(hEncInBuf); 
                }
             else
                offset = 0;
        }
         
        /* Write encoded buffer to the speech file */
        if (!envp->writeDisabled) {
            if (Buffer_getNumBytesUsed(hOutBuf)) {
                if (fwrite(Buffer_getUserPtr(hOutBuf),
                       Buffer_getNumBytesUsed(hOutBuf), 1, outFile) != 1) {
                    ERR("Error writing the encoded data to speech file.\n");
                    cleanup(THREAD_FAILURE);
                }
            }
            else {
                printf("Warning, zero bytes audio encoded\n");
            }
        }

        /* Increment the number of bytes encoded for the user interface */
        gblIncSoundBytesProcessed(Buffer_getNumBytesUsed(hOutBuf));
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Pause_off(envp->hPauseProcess);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hAe1) {
        Aenc1_delete(hAe1);
    }

    if (hSound) {
        Sound_delete(hSound);
    }

    if (hInBuf) {
        Buffer_delete(hInBuf);
    }
    if (hEncInBuf) {
        Buffer_delete(hEncInBuf);
    }

    if (hOutBuf) {
        Buffer_delete(hOutBuf);
    }

    if (hEngine) {
        Engine_close(hEngine);
    }

    if (outFile) {
        fclose(outFile);
    }

    return status;
}
