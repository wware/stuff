/*
 * video.c
 *
 * This source file has the implementations for the video thread
 * functions implemented for 'DVSDK encode demo' on DM365 platform
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
 */

#include <xdc/std.h>

#include <ti/sdo/ce/Engine.h>

#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/BufTab.h>
#include <ti/sdo/dmai/VideoStd.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>
#include <ti/sdo/dmai/ce/Venc1.h>

#include "video.h"
#include "../demo.h"

#ifndef YUV_420SP
#define YUV_420SP 256
#endif 

/* Number of buffers in the pipe to the capture thread */
/* Note: It needs to match capture.c pipe size */
#define VIDEO_PIPE_SIZE           4 

/******************************************************************************
 * videoThrFxn
 ******************************************************************************/
Void *videoThrFxn(Void *arg)
{
    VideoEnv               *envp                = (VideoEnv *) arg;
    Void                   *status              = THREAD_SUCCESS;
    VIDENC1_Params          defaultParams       = Venc1_Params_DEFAULT;
    VIDENC1_DynamicParams   defaultDynParams    = Venc1_DynamicParams_DEFAULT;
    BufferGfx_Attrs         gfxAttrs            = BufferGfx_Attrs_DEFAULT;
    Venc1_Handle            hVe1                = NULL;
    Engine_Handle           hEngine             = NULL;
    BufTab_Handle           hBufTab             = NULL;
    Int                     frameCnt            = 0;
    Buffer_Handle           hCapBuf, hDstBuf;
    VIDENC1_Params         *params;
    VIDENC1_DynamicParams  *dynParams;
    Int                     fifoRet;
    Int                     bufIdx;
    ColorSpace_Type         colorSpace = ColorSpace_YUV420PSEMI;
    Bool                    localBufferAlloc = TRUE;
    BufferGfx_Dimensions    dim;
    Int32                   bufSize;

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* In case of HD resolutions the video buffer will be allocated
       by capture thread. */
    if (((envp->imageWidth == VideoStd_720P_WIDTH) && 
        (envp->imageHeight == VideoStd_720P_HEIGHT)) ||
      ((envp->imageWidth == VideoStd_1080I_WIDTH) &&
        (envp->imageHeight == VideoStd_1080I_HEIGHT))) {
        localBufferAlloc = FALSE;
    } 
    
    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams;

    /* 
     * Set up codec parameters. We round up the height to accomodate for 
     * alignment restrictions from codecs 
     */
    params->maxWidth        = envp->imageWidth;
    params->maxHeight       = Dmai_roundUp(envp->imageHeight, CODECHEIGHTALIGN);
    params->encodingPreset  = XDM_HIGH_SPEED;
    if (colorSpace ==  ColorSpace_YUV420PSEMI) { 
        params->inputChromaFormat = XDM_YUV_420SP;
    } else {
        params->inputChromaFormat = XDM_YUV_422ILE;
    }
    params->reconChromaFormat = XDM_YUV_420SP;
    params->maxFrameRate      = envp->videoFrameRate;

    /* Set up codec parameters depending on bit rate */
    if (envp->videoBitRate < 0) {
        /* Variable bit rate */
        params->rateControlPreset = IVIDEO_NONE;

        /*
         * If variable bit rate use a bogus bit rate value (> 0)
         * since it will be ignored.
         */
        params->maxBitRate        = 2000000;
    }
    else {
        /* Constant bit rate */
        params->rateControlPreset = IVIDEO_STORAGE;
        params->maxBitRate        = envp->videoBitRate;
    }

    dynParams->targetBitRate   = params->maxBitRate;
    dynParams->inputWidth      = envp->imageWidth;
    dynParams->inputHeight     = envp->imageHeight;    
    dynParams->refFrameRate    = params->maxFrameRate;
    dynParams->targetFrameRate = params->maxFrameRate;
    dynParams->interFrameInterval = 0;
    
    /* Create the video encoder */
    hVe1 = Venc1_create(hEngine, envp->videoEncoder, params, dynParams);

    if (hVe1 == NULL) {
        ERR("Failed to create video encoder: %s\n", envp->videoEncoder);
        cleanup(THREAD_FAILURE);
    }

    /* Store the output buffer size in the environment */
    envp->outBufSize = Venc1_getOutBufSize(hVe1);

    /* Signal that the codec is created and output buffer size available */
    Rendezvous_meet(envp->hRendezvousWriter);
    if (localBufferAlloc == TRUE) {
        gfxAttrs.colorSpace = colorSpace;
        gfxAttrs.dim.width  = envp->imageWidth;
        gfxAttrs.dim.height = envp->imageHeight;
        gfxAttrs.dim.lineLength = 
            Dmai_roundUp(BufferGfx_calcLineLength(gfxAttrs.dim.width,
                                   gfxAttrs.colorSpace), 32);
        
        /* 
         *Calculate size of codec buffers based on rounded LineLength 
         *This will allow to share the buffers with Capture thread. If the
         *size of the buffers was obtained from the codec through 
         *Venc1_getInBufSize() there would be a mismatch with the size of
         * Capture thread buffers when the width is not 32 byte aligned.
         */
        if (colorSpace ==  ColorSpace_YUV420PSEMI) {
            bufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height * 3 / 2;
        } 
        else {
            bufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height * 2;
        } 

        /* Create a table of buffers with size based on rounded LineLength */
        hBufTab = BufTab_create(VIDEO_PIPE_SIZE, bufSize,
                                BufferGfx_getBufferAttrs(&gfxAttrs));

        if (hBufTab == NULL) {
            ERR("Failed to allocate contiguous buffers\n");
            cleanup(THREAD_FAILURE);
        }

        /* Send buffers to the capture thread to be ready for main loop */
        for (bufIdx = 0; bufIdx < VIDEO_PIPE_SIZE; bufIdx++) {
            if (Fifo_put(envp->hCaptureInFifo,
                         BufTab_getBuf(hBufTab, bufIdx)) < 0) {
                ERR("Failed to send buffer to display thread\n");
                cleanup(THREAD_FAILURE);
            }
        }
    } else {
        /* Send buffers to the capture thread to be ready for main loop */
        for (bufIdx = 0; bufIdx < VIDEO_PIPE_SIZE; bufIdx++) {
            fifoRet = Fifo_get(envp->hCaptureOutFifo, &hCapBuf);

            if (fifoRet < 0) {
                ERR("Failed to get buffer from capture thread\n");
                cleanup(THREAD_FAILURE);
            }

            /* Did the capture thread flush the fifo? */
            if (fifoRet == Dmai_EFLUSH) {
                cleanup(THREAD_SUCCESS);
            }
            /* Return buffer to capture thread */
            if (Fifo_put(envp->hCaptureInFifo, hCapBuf) < 0) {
                ERR("Failed to send buffer to display thread\n");
                cleanup(THREAD_FAILURE);
            }
        }
    }
    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);
    
    while (!gblGetQuit()) {
        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Get a buffer to encode from the capture thread */
        fifoRet = Fifo_get(envp->hCaptureOutFifo, &hCapBuf);

        if (fifoRet < 0) {
            ERR("Failed to get buffer from video thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Did the capture thread flush the fifo? */
        if (fifoRet == Dmai_EFLUSH) {
            cleanup(THREAD_SUCCESS);
        }

        /* Get a buffer to encode to from the writer thread */
        fifoRet = Fifo_get(envp->hWriterOutFifo, &hDstBuf);

        if (fifoRet < 0) {
            ERR("Failed to get buffer from video thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Did the writer thread flush the fifo? */
        if (fifoRet == Dmai_EFLUSH) {
            cleanup(THREAD_SUCCESS);
        }

        /* Make sure the whole buffer is used for input */
        BufferGfx_resetDimensions(hCapBuf);

        /* Ensure that the video buffer has dimensions accepted by codec */
        BufferGfx_getDimensions(hCapBuf, &dim);
        dim.height = Dmai_roundUp(dim.height, CODECHEIGHTALIGN);
        BufferGfx_setDimensions(hCapBuf, &dim);

        /* Encode the video buffer */
        if (Venc1_process(hVe1, hCapBuf, hDstBuf) < 0) {
            ERR("Failed to encode video buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* Reset the dimensions to what they were originally */
        BufferGfx_resetDimensions(hCapBuf);

        /* Send encoded buffer to writer thread for filesystem output */
        if (Fifo_put(envp->hWriterInFifo, hDstBuf) < 0) {
            ERR("Failed to send buffer to display thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Return buffer to capture thread */
        if (Fifo_put(envp->hCaptureInFifo, hCapBuf) < 0) {
            ERR("Failed to send buffer to display thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Increment statistics for the user interface */
        gblIncVideoBytesProcessed(Buffer_getNumBytesUsed(hDstBuf));

        frameCnt++;
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Rendezvous_force(envp->hRendezvousWriter);
    Pause_off(envp->hPauseProcess);
    Fifo_flush(envp->hWriterInFifo);
    Fifo_flush(envp->hCaptureInFifo);

    /* Make sure the other threads aren't waiting for init to complete */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    if (hVe1) {
        Venc1_delete(hVe1);
    }

    if (hEngine) {
        Engine_close(hEngine);
    }

    return status;
}

