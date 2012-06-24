/*
 * capture.c
 *
 * This source file implements the video capture for the encodedecode demo 
 * for DM365 platform.
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

#include <string.h>
#include <xdc/std.h>

#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Capture.h>
#include <ti/sdo/dmai/VideoStd.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "capture.h"
#include "../demo.h"
#include "../ui.h"

/* Triple buffering for the capture driver */
#define NUM_CAPTURE_BUFS         3

/******************************************************************************
 * captureThrFxn
 ******************************************************************************/
Void *captureThrFxn(Void *arg)
{
    CaptureEnv          *envp           = (CaptureEnv *) arg;
    Void                *status         = THREAD_SUCCESS;
    Capture_Attrs        cAttrs         = Capture_Attrs_DM365_DEFAULT;
    BufferGfx_Attrs      gfxAttrs       = BufferGfx_Attrs_DEFAULT;
    Capture_Handle       hCapture       = NULL;
    BufTab_Handle        hBufTab        = NULL;
    BufferGfx_Dimensions dim;
    Buffer_Handle        hDstBuf, hCapBuf;
    Int32                width, height, bufSize;
    Int                  fifoRet;
    ColorSpace_Type      colorSpace = ColorSpace_YUV420PSEMI;

    /* Create capture device driver instance */
    cAttrs.numBufs = NUM_CAPTURE_BUFS;
    cAttrs.videoInput = envp->videoInput;    
    cAttrs.videoStd   = envp->videoStd;
    cAttrs.colorSpace = colorSpace;

    if (VideoStd_getResolution(envp->videoStd, &width, &height) < 0) {
        ERR("Failed to calculate resolution of video standard\n");
        cleanup(THREAD_FAILURE);
    }

    if (envp->imageWidth > 0 && envp->imageHeight > 0) {
        if (width < envp->imageWidth && height < envp->imageHeight) {
            ERR("User resolution (%ldx%ld) larger than detected (%ldx%ld)\n",
                envp->imageWidth, envp->imageHeight, width, height);
            cleanup(THREAD_FAILURE);
        }

       /*
        * Capture driver provides 32-byte aligned data. We 32-byte align the
        * capture and video buffers to perform zero copy encoding.
        */
        envp->imageWidth = Dmai_roundUp(envp->imageWidth, 32);
    }
    else {
        /* Resolution was not set on command line. Set to defaults. */
        envp->imageHeight = height;

       /*
        * Capture driver provides 32-byte aligned data. We 32-byte align the
        * capture and video buffers to perform zero copy encoding.
        */
        envp->imageWidth  = Dmai_roundUp(width, 32); 
    }

    Dmai_clear(dim);
    dim.width      = envp->imageWidth;
    dim.height     = envp->imageHeight;

    dim.lineLength = Dmai_roundUp(dim.width, 32);
    if (colorSpace ==  ColorSpace_YUV420PSEMI) {
        bufSize = dim.lineLength * dim.height * 3 / 2;
    } else {
        bufSize = dim.lineLength * dim.height * 2;
    }
    gfxAttrs.dim = dim;

    /* Report the video standard and image size back to the main thread */
    Rendezvous_meet(envp->hRendezvousCapStd);

    gfxAttrs.colorSpace = colorSpace;
    hBufTab = BufTab_create(NUM_CAPTURE_BUFS, bufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));
    if (hBufTab == NULL) {
        ERR("Failed to create buftab\n");
        cleanup(THREAD_FAILURE);
    }

    if ((envp->videoStd == VideoStd_720P_60) && (!envp->passThrough)) {
        cAttrs.videoStd = VideoStd_720P_30;
    }
    else {
        cAttrs.videoStd = envp->videoStd;    
    }
    cAttrs.colorSpace = colorSpace;
    cAttrs.captureDimension = &dim;
    cAttrs.numBufs = NUM_CAPTURE_BUFS;
    hCapture = Capture_create(hBufTab, &cAttrs);

    if (hCapture == NULL) {
        ERR("Failed to create capture device\n");
        cleanup(THREAD_FAILURE);
    }

    /* Get a buffer from the video thread */
    fifoRet = Fifo_get(envp->hInFifo, &hDstBuf);

    if (fifoRet < 0) {
        ERR("Failed to get buffer from video thread\n");
        cleanup(THREAD_FAILURE);
    }

    if (fifoRet == Dmai_EFLUSH) {
        cleanup(THREAD_SUCCESS);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    while (!gblGetQuit()) {
        
        /* Get a buffer from the capture driver to encode */
        if (Capture_get(hCapture, &hCapBuf) < 0) {
            ERR("Failed to get capture buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* Send captured buffer to video thread for encoding */
        if (Fifo_put(envp->hOutFifo, hCapBuf) < 0) {
            ERR("Failed to send buffer to video thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Get a buffer from the video thread */
        fifoRet = Fifo_get(envp->hInFifo, &hDstBuf);

        if (fifoRet < 0) {
            ERR("Failed to get buffer from video thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Did the video thread flush the fifo? */
        if (fifoRet == Dmai_EFLUSH) {
            cleanup(THREAD_SUCCESS);
        }

        /* Return a buffer to the capture driver */
        if (Capture_put(hCapture, hDstBuf) < 0) {
            ERR("Failed to put capture buffer\n");
            cleanup(THREAD_FAILURE);
        }
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousCapStd);
    Rendezvous_force(envp->hRendezvousInit);
    Pause_off(envp->hPauseProcess);
    Fifo_flush(envp->hOutFifo);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    if (hCapture) {
        Capture_delete(hCapture);
    }

    /* Clean up the thread before exiting */
    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    return status;
}

