/*
 * speech.c
 *
 * This source file has the speech thread functions implemented for 
 * 'decode demo' on DM365 platform
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
#include <ti/sdo/ce/osal/Memory.h>
#include <ti/sdo/ce/speech/sphdec.h>

#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Sound.h>
#include <ti/sdo/dmai/Loader.h>
#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/ce/Sdec1.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "speech.h"
#include "../demo.h"

/*
 * Since the g711 example codec returns a minimum input size of 1 byte and
 * minimum output size of 2 bytes, we force a higher value. Otherwise the
 * overhead of calling the codec and driver for each byte of data will be
 * excessive.
 */
#define INBUFSIZE   160
#define OUTBUFSIZE  INBUFSIZE * 2

/******************************************************************************
 * speechThrFxn
 ******************************************************************************/
Void *speechThrFxn(Void *arg)
{
    SpeechEnv              *envp                = (SpeechEnv *) arg;
    SPHDEC1_Params          defaultParams       = Sdec1_Params_DEFAULT;
    SPHDEC1_DynamicParams   defaultDynParams    = Sdec1_DynamicParams_DEFAULT;
    Void                   *status              = THREAD_SUCCESS;
    Sound_Attrs             sAttrs              = Sound_Attrs_MONO_DEFAULT;
    Loader_Attrs            lAttrs              = Loader_Attrs_DEFAULT;
    Buffer_Attrs            bAttrs              = Buffer_Attrs_DEFAULT;
    Sdec1_Handle            hSd1                = NULL;
    Sound_Handle            hSound              = NULL;
    Loader_Handle           hLoader             = NULL;
    Engine_Handle           hEngine             = NULL;
    Buffer_Handle           hOutBuf             = NULL;
    SPHDEC1_Params         *params;
    SPHDEC1_DynamicParams  *dynParams;
    Buffer_Handle           hInBuf;

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* Create the sound device */
    sAttrs.sampleRate = 8000;
    sAttrs.mode = Sound_Mode_OUTPUT;
    sAttrs.leftGain  = 127;
    sAttrs.rightGain = 127;
    sAttrs.bufSize   = 128;
    hSound = Sound_create(&sAttrs);

    if (hSound == NULL) {
        ERR("Failed to create audio device\n");
        cleanup(THREAD_FAILURE);
    }

    /* Set the sample rate for the user interface */
    gblSetSamplingFrequency(sAttrs.sampleRate);

    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams;

    /* Create the speech decoder */
    hSd1 = Sdec1_create(hEngine, envp->speechDecoder, params, dynParams);

    if (hSd1 == NULL) {
        ERR("Failed to create speech decoder: %s\n", envp->speechDecoder);
        cleanup(THREAD_FAILURE);
    }

    /*
     * Make the output buffer size twice the size of what the codec needs
     * as the codec needs mono and the Sound module converts the decoded
     * mono samples to stereo before writing to the device driver.
     */
    hOutBuf = Buffer_create(OUTBUFSIZE, &bAttrs);

    if (hOutBuf == NULL) {
        ERR("Failed to allocate output buffer\n");
        cleanup(THREAD_FAILURE);
    }

    /* How much encoded data to feed the codec each process call */
    lAttrs.readSize = INBUFSIZE;

    /* Make the total ring buffer larger */
    lAttrs.readBufSize = lAttrs.readSize * 512;

    /* Create the file loader for reading encoded data */
    hLoader = Loader_create(envp->speechFile, &lAttrs);

    if (hLoader == NULL) {
        ERR("Failed to create loader\n");
        cleanup(THREAD_FAILURE);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    /* Prime the file loader */
    Loader_prime(hLoader, &hInBuf);

    while (!gblGetQuit()) {
        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Decode the audio buffer */
        if (Sdec1_process(hSd1, hInBuf, hOutBuf) < 0) {
            ERR("Failed to decode audio buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* Increment statistics for user interface */
        gblIncSoundBytesProcessed(Buffer_getNumBytesUsed(hInBuf));

        /*
         * Force the output buffer size since we are forcing the size of the
         * output buffer allocated as opposed to asking the codec for a size.
         */
        Buffer_setNumBytesUsed(hOutBuf, OUTBUFSIZE);

        /* Write the decoded samples to the sound device */
        if (Sound_write(hSound, hOutBuf) < 0) {
            ERR("Failed to write audio buffer\n");
            cleanup(THREAD_FAILURE);
        }

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
                /* Recreate the speech codec */
                Sdec1_delete(hSd1);
                hSd1 = Sdec1_create(hEngine, envp->speechDecoder,
                                    params, dynParams);

                if (hSd1 == NULL) {
                    ERR("Failed to create speech decoder: %s\n",
                        envp->speechDecoder);
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

    if (hSd1) {
        Sdec1_delete(hSd1);
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
