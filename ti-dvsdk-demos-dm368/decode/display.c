/*
 * display.c
 *
 * This source file has the implementations for the 'display' function
 * for the DVSDK decode demos on DM365 platform.
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
#include <string.h>
#include <unistd.h>
#include <unistd.h>

#include <xdc/std.h>

#include <ti/sdo/dmai/Time.h>
#include <ti/sdo/dmai/Framecopy.h>
#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Display.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "display.h"
#include "../demo.h"

/* Display loop delay in us */
#define DISPLAYLOOPLATENCY 33332

/* Buffering for the display driver */
#define NUM_DISPLAY_BUFS        4 

/******************************************************************************
 * displayThrFxn
 ******************************************************************************/
Void *displayThrFxn(Void *arg)
{
    DisplayEnv             *envp       = (DisplayEnv *) arg;
    Display_Attrs           dAttrs     = Display_Attrs_DM365_VID_DEFAULT;
    Display_Handle          hDisplay   = NULL;
    Framecopy_Handle        hFc        = NULL;
    Void                   *status     = THREAD_SUCCESS;
    Uns                     frameCnt   = 0;
    BufferGfx_Dimensions    srcDim;
    Buffer_Handle           hSrcBuf, hDstBuf;
    Int                     fifoRet;
    ColorSpace_Type         colorSpace = ColorSpace_YUV420PSEMI;
    BufferGfx_Attrs         gfxAttrs = BufferGfx_Attrs_DEFAULT;
    BufTab_Handle           hBufTab  = NULL;
    Int32                   bufSize;
    Time_Attrs              tAttrs   = Time_Attrs_DEFAULT;
    Time_Handle             hTime    = NULL;
    Int32                   time, waitTime;
    Int                     bufCnt = 1;

    hTime = Time_create(&tAttrs);

    if (hTime == NULL) {
        ERR("Failed to create Time object\n");
        cleanup(THREAD_FAILURE);
    }

    if(Time_reset(hTime) != Dmai_EOK) {
        ERR("Failed to reset timer\n");
        cleanup(THREAD_FAILURE);
    }

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    while (!gblGetQuit()) {
        /* Pause processing? */
        Pause_test(envp->hPauseProcess);

        /* Pause for priming? */
        Pause_test(envp->hPausePrime);

        /* Get decoded video frame */
        fifoRet = Fifo_get(envp->hInFifo, &hSrcBuf);

        if (fifoRet < 0) {
            ERR("Failed to get buffer from video thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Did the video thread flush the fifo? */
        if (fifoRet == Dmai_EFLUSH) {
            cleanup(THREAD_SUCCESS);
        }
        
        BufferGfx_getDimensions(hSrcBuf, &srcDim);

        /* Prime the display driver with the first NUM_DISPLAY_BUFS buffers */
        if (bufCnt <= NUM_DISPLAY_BUFS) { 
            if (bufCnt == 1) {  // Create the Display at the first frame
                gfxAttrs.dim.width = srcDim.width;
                gfxAttrs.dim.height = srcDim.height;
                gfxAttrs.dim.lineLength = srcDim.lineLength;
                gfxAttrs.dim.x = srcDim.x;
                gfxAttrs.dim.y = srcDim.y;
                if (colorSpace ==  ColorSpace_YUV420PSEMI) {
                    bufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height * 
                        3 / 2;
                } else {
                    bufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height * 2;
                }

                /* Create a table of buffers to use with the device drivers */
                gfxAttrs.colorSpace = colorSpace;
                hBufTab = BufTab_create(NUM_DISPLAY_BUFS, bufSize,
                    BufferGfx_getBufferAttrs(&gfxAttrs));
                if (hBufTab == NULL) {
                    ERR("Failed to create buftab\n");
                    cleanup(THREAD_FAILURE);
                }
	
                /* Create the display device instance */
                dAttrs.delayStreamon = TRUE;
                dAttrs.numBufs = NUM_DISPLAY_BUFS;
                dAttrs.videoStd = envp->videoStd;
                /* 
                 * Round down the width to a multiple of 32 as required by 
                 * display driver. Otherwise, the driver would internally round
                 * up the width, resulting in the codec padding showing up
                 * on the display when the image width is not a multiple of 32.
                 */
                dAttrs.width = ((gfxAttrs.dim.width & 0x1f) ?
                    (gfxAttrs.dim.width & ~(0x1f)) : gfxAttrs.dim.width);
                dAttrs.height = gfxAttrs.dim.height;
                dAttrs.videoOutput = envp->displayOutput;
                dAttrs.colorSpace  = colorSpace;
                hDisplay = Display_create(hBufTab, &dAttrs);

                if (hDisplay == NULL) {
                    ERR("Failed to create display device\n");
                    cleanup(THREAD_FAILURE);
                }
            }

            bufCnt++;
        }
        else {
            /* Get a buffer from the display device driver */
            if (Display_get(hDisplay, &hDstBuf) < 0) {
                ERR("Failed to get display buffer\n");
                cleanup(THREAD_FAILURE);
            }

            /* Send buffer back to the video thread */
            if (Fifo_put(envp->hOutFifo, hDstBuf) < 0) {
                ERR("Failed to send buffer to video thread\n");
                cleanup(THREAD_FAILURE);
            }
        }

        if (envp->videoStd == VideoStd_720P_60) {
            if (Time_delta(hTime, (UInt32*)&time) < 0) {
                ERR("Failed to get timer delta\n");
                cleanup(THREAD_FAILURE);
            }
            waitTime = DISPLAYLOOPLATENCY - time;
            if(waitTime > 0) {
                usleep(waitTime);
            }
            if(Time_reset(hTime) != Dmai_EOK) {
                ERR("Failed to reset timer\n");
                cleanup(THREAD_FAILURE);
            }
        }

        /* Incremement statistics for the user interface */
        gblIncFrames();           

        /* Give a filled buffer back to the display device driver */
        if (Display_put(hDisplay, hSrcBuf) < 0) {
            ERR("Failed to put display buffer\n");
            cleanup(THREAD_FAILURE);
        }

        frameCnt++;
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Pause_off(envp->hPauseProcess);
    Pause_off(envp->hPausePrime);
    Fifo_flush(envp->hOutFifo);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hFc) {
        Framecopy_delete(hFc);
    }

    if (hDisplay) {
        Display_delete(hDisplay);
    }

    /* Clean up the thread before exiting */
    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    if(hTime) {
        Time_delete(hTime);
    }

    return status;
}
