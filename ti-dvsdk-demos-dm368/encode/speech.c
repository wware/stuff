/*
 * speech.c
 *
 * This source file has the speech thread functions implemented for 
 * 'encode demo' on DM365 platform
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

#include <xdc/std.h>

#include <ti/sdo/ce/Engine.h>

#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Sound.h>
#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/Loader.h>
#include <ti/sdo/dmai/Rendezvous.h>
#include <ti/sdo/dmai/ce/Senc1.h>

#include "speech.h"
#include "../demo.h"

/*
 * Since the g711 example codec returns a minimum input size of 2 bytes and
 * minimum output size of 1 byte, we force a higher value. Otherwise the
 * overhead of calling the codec and driver for each byte of data will be
 * excessive.
 */
#define OUTBUFSIZE   160
#define INBUFSIZE    OUTBUFSIZE * 2

/******************************************************************************
 * speechThrFxn
 ******************************************************************************/
Void *speechThrFxn(Void *arg)
{
    SpeechEnv              *envp                = (SpeechEnv *) arg;
    Void                   *status              = THREAD_SUCCESS;
    Sound_Attrs             sAttrs              = Sound_Attrs_MONO_DEFAULT;
    Buffer_Attrs            bAttrs              = Buffer_Attrs_DEFAULT;
    SPHENC1_Params          defaultParams       = Senc1_Params_DEFAULT;
    SPHENC1_DynamicParams   defaultDynParams    = Senc1_DynamicParams_DEFAULT;
    Engine_Handle           hEngine             = NULL;
    Sound_Handle            hSound              = NULL;
    Senc1_Handle            hSe1                = NULL;
    Buffer_Handle           hOutBuf             = NULL;
    Buffer_Handle           hInBuf              = NULL;
    FILE                   *outFile             = NULL;
    SPHENC1_Params         *params;
    SPHENC1_DynamicParams  *dynParams;

    /* Open the output file for writing */
    outFile = fopen(envp->speechFile, "w");

    if (outFile == NULL) {
        ERR("Failed to open %s for writing\n", envp->speechFile);
        cleanup(THREAD_FAILURE);
    }

    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams;

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* Create the speech decoder */
    hSe1 = Senc1_create(hEngine, envp->speechEncoder, params, dynParams);

    if (hSe1 == NULL) {
        ERR("Failed to create speech decoder: %s\n", envp->speechEncoder);
        cleanup(THREAD_FAILURE);
    }

    /* Create the output buffer */
    hOutBuf = Buffer_create(OUTBUFSIZE, &bAttrs);

    if (hOutBuf == NULL) {
        ERR("Failed to allocate output buffer\n");
        cleanup(THREAD_FAILURE);
    }

    /*
     * Make the input buffer size twice the size of what the codec needs
     * as the codec needs mono and the Sound module converts the captured
     * stereo samples to mono leaving half the buffer filled with data.
     */
    hInBuf = Buffer_create(INBUFSIZE, &bAttrs);

    if (hInBuf == NULL) {
        ERR("Failed to allocate input buffer\n");
        cleanup(THREAD_FAILURE);
    }

    /* Set the sample rate for the user interface */
    gblSetSamplingFrequency(sAttrs.sampleRate);

    /* Create the sound device */
    sAttrs.soundInput = envp->soundInput;
    sAttrs.mode = Sound_Mode_INPUT;
    sAttrs.bufSize   = 128;
    hSound = Sound_create(&sAttrs);

    if (hSound == NULL) {
        ERR("Failed to create speech device\n");
        cleanup(THREAD_FAILURE);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    while (!gblGetQuit()) {
        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Read samples from the Sound device */
        if (Sound_read(hSound, hInBuf) < 0) {
            ERR("Failed to read speech buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* Encode the speech buffer */
        if (Senc1_process(hSe1, hInBuf, hOutBuf) < 0) {
            ERR("Failed to encode speech buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /*
         * Force the output buffer size since we are forcing the size of the
         * output buffer allocated as opposed to asking the codec for a size.
         */
        Buffer_setNumBytesUsed(hOutBuf, OUTBUFSIZE);

        /* Write encoded buffer to the speech file */
        if (Buffer_getNumBytesUsed(hOutBuf)) {
            if (fwrite(Buffer_getUserPtr(hOutBuf),
                       Buffer_getNumBytesUsed(hOutBuf), 1, outFile) != 1) {
                ERR("Error writing the encoded data to speech file.\n");
                cleanup(THREAD_FAILURE);
            }
        }
        else {
            printf("Warning, zero bytes speech encoded\n");
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
    if (hSe1) {
        Senc1_delete(hSe1);
    }

    if (hSound) {
        Sound_delete(hSound);
    }

    if (hInBuf) {
        Buffer_delete(hInBuf);
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

