/*
 * audio.c
 *
 * This source file has the implementations for the Audio Thread implemented 
 * for the decode demo on DM6467 platform.
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

#include <xdc/std.h>

#include <ti/sdo/ce/Engine.h>

#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Sound.h>
#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/Loader.h>
#include <ti/sdo/dmai/Rendezvous.h>
#include <ti/sdo/dmai/ce/Adec1.h>

#include "../demo.h"
#include "audio.h"

/******************************************************************************
 * audioThrFxn
 ******************************************************************************/
Void *audioThrFxn(Void *arg)
{
    AudioEnv               *envp                = (AudioEnv *) arg;
    Void                   *status              = THREAD_SUCCESS;
    Sound_Attrs             sAttrs              = Sound_Attrs_STEREO_DEFAULT;
    Buffer_Attrs            bAttrs              = Buffer_Attrs_DEFAULT;
    Loader_Attrs            lAttrs              = Loader_Attrs_DEFAULT;
    AUDDEC1_Params          defaultParams       = Adec1_Params_DEFAULT;
    AUDDEC1_DynamicParams   defaultDynParams    = Adec1_DynamicParams_DEFAULT;
    Engine_Handle           hEngine             = NULL;
    Sound_Handle            hSound              = NULL;
    Loader_Handle           hLoader             = NULL;
    Adec1_Handle            hAd1                = NULL;
    Buffer_Handle           hOutBuf             = NULL;
    Buffer_Handle           hInBuf;
    AUDDEC1_Params         *params;
    AUDDEC1_DynamicParams  *dynParams;
    Int                     ret;

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams; 

    params->dataEndianness = XDM_LE_16;

    /* Create the audio decoder */
    hAd1 = Adec1_create(hEngine, envp->audioDecoder, params, dynParams);

    if (hAd1 == NULL) {
        ERR("Failed to create audio decoder %s\n", envp->audioDecoder);
        cleanup(THREAD_FAILURE);
    }

    /* Create an output buffer for decoded data */
    hOutBuf = Buffer_create(Adec1_getOutBufSize(hAd1), &bAttrs);

    if (hOutBuf == NULL) {
        ERR("Failed to allocate output buffer\n");
        cleanup(THREAD_FAILURE);
    }

    /* Ask the codec how much input data it needs */
    lAttrs.readSize = Adec1_getInBufSize(hAd1);

    /* Make the total ring buffer larger */
    lAttrs.readBufSize = lAttrs.readSize * 10;

    /* Create the file loader for reading encoded data */
    hLoader = Loader_create(envp->audioFile, &lAttrs);

    if (hLoader == NULL) {
        ERR("Failed to create loader\n");
        cleanup(THREAD_FAILURE);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    /* Prime the file loader */
    Loader_prime(hLoader, &hInBuf);

    /* Decode the audio buffer */
    ret = Adec1_process(hAd1, hInBuf, hOutBuf);

    if ((ret == Dmai_EFAIL)||
        (ret == Dmai_EBITERROR && Buffer_getNumBytesUsed(hInBuf) == 0)) {
            ERR("Failed to decode audio buffer. Stream may have ended \
                with partial frame.\n");
            cleanup(THREAD_FAILURE);
    }

    /* Set the sample rate for the user interface */
    gblSetSamplingFrequency(Adec1_getSampleRate(hAd1));

    /* Create the sound device */
    sAttrs.sampleRate = Adec1_getSampleRate(hAd1);
    sAttrs.leftGain  = 127;
    sAttrs.rightGain = 127;
    hSound = Sound_create(&sAttrs);

    if (hSound == NULL) {
        ERR("Failed to create audio device\n");
        cleanup(THREAD_FAILURE);
    }

    while (!gblGetQuit()) {
        /* Increment the number of bytes decoded for the user interface */
        gblIncSoundBytesProcessed(Buffer_getNumBytesUsed(hInBuf));

        /* Write the decoded samples to the sound device */
        if (Sound_write(hSound, hOutBuf) < 0) {
            ERR("Failed to write audio buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Load a new frame from the file system */
        if (Loader_getFrame(hLoader, hInBuf) < 0) {
            ERR("Failed to read a frame of encoded data\n");
            cleanup(THREAD_FAILURE);
        }

        /* Check if the clip has ended */
        if (Buffer_getUserPtr(hInBuf) == NULL) {
            /* Wait for the video clip to finish, if applicable */
            Rendezvous_meet(envp->hRendezvousLoop);

            /* If we are to loop the clip, start over */
            if (envp->loop) {
                /* Recreate the audio codec */
                Adec1_delete(hAd1);
                hAd1 = Adec1_create(hEngine, envp->audioDecoder,
                                    params, dynParams);

                if (hAd1 == NULL) {
                    ERR("Failed to create audio decoder %s\n",
                        envp->audioDecoder);
                    cleanup(THREAD_FAILURE);
                }

                /* Re-prime the file loader */
                Loader_prime(hLoader, &hInBuf);
            }
            else {
                printf("End of clip reached, exiting..\n");
                cleanup(THREAD_SUCCESS);
            }
        }

        /* Decode the audio buffer */
        ret = Adec1_process(hAd1, hInBuf, hOutBuf);

        if ((ret == Dmai_EFAIL)||
            (ret == Dmai_EBITERROR && Buffer_getNumBytesUsed(hInBuf) == 0)) {
            ERR("Failed to decode audio buffer. Stream may have ended with a \
                partial frame.\n");
            cleanup(THREAD_FAILURE);
        }
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Rendezvous_force(envp->hRendezvousLoop);
    Pause_off(envp->hPauseProcess);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hLoader) {
        Loader_delete(hLoader);
    }

    if (hAd1) {
        Adec1_delete(hAd1);
    }

    if (hSound) {
        Sound_delete(hSound);
    }

    if (hOutBuf) {
        Buffer_delete(hOutBuf);
    }

    if (hEngine) {
        Engine_close(hEngine);
    }

    return status;
}

