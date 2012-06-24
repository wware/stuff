/*
 * video.c
 *
 * This source file has the implementations for the video thread
 * functions implemented for 'DVSDK encodedecode demo' on DM365 platform
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

#include <stdio.h>
#include <string.h>

#include <xdc/std.h>

#include <ti/sdo/ce/Engine.h>
#include <ti/sdo/ce/osal/Memory.h>

#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/BufTab.h>
#include <ti/sdo/dmai/VideoStd.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>
#include <ti/sdo/dmai/ce/Vdec2.h>
#include <ti/sdo/dmai/ce/Venc1.h>

#include <ti/sdo/codecs/h264dec/ih264vdec.h>

#include "video.h"
#include "../demo.h"

#define CAPTURE_PIPE_SIZE       4   
#define DISPLAY_PIPE_SIZE       7
#define NUM_VIDEO_BUFS          CAPTURE_PIPE_SIZE + DISPLAY_PIPE_SIZE

/* The masks to use for knowing when a buffer is free */
#define CODEC_FREE              0x1
#define DISPLAY_FREE            0x2

#ifndef YUV_420SP
#define YUV_420SP 256
#endif 

/******************************************************************************
 * encodedecode
 ******************************************************************************/
static Int encodedecode(Venc1_Handle hVe1, Vdec2_Handle hVd2,
                        Buffer_Handle hVidBuf, Buffer_Handle hEncBuf,
                        Buffer_Handle hDecBuf, Fifo_Handle displayFifo)
{
    Buffer_Handle           hOutBuf, hFreeBuf;
    Int                     ret;
    BufferGfx_Dimensions    dim;

    /* Make sure the whole buffer is used for input */
    BufferGfx_resetDimensions(hVidBuf);

    /* Make sure the whole buffer is used for input */
    BufferGfx_resetDimensions(hDecBuf);

    /* Ensure that the video buffer has dimensions accepted by codec */
    BufferGfx_getDimensions(hVidBuf, &dim);
    dim.height = Dmai_roundUp(dim.height, CODECHEIGHTALIGN);
    BufferGfx_setDimensions(hVidBuf, &dim);

    /* Encode the video buffer */
    if (Venc1_process(hVe1, hVidBuf, hEncBuf) < 0) {
        ERR("Failed to encode video buffer\n");
        return FAILURE;
    }

    /* Reset the dimensions to what they were originally */
    BufferGfx_resetDimensions(hVidBuf);

    if (Buffer_getNumBytesUsed(hEncBuf) == 0) {
        ERR("Encoder created 0 sized output frame\n");
        return FAILURE;
    }

    /* Update global data for user interface */
    gblIncVideoBytesProcessed(Buffer_getNumBytesUsed(hEncBuf));

    /* Decode the video buffer */
    ret = Vdec2_process(hVd2, hEncBuf, hDecBuf);

    if (ret != Dmai_EOK) {
        ERR("Failed to decode video buffer\n");
        return FAILURE;
    }

    /* Send display frames to display thread */
    hOutBuf = Vdec2_getDisplayBuf(hVd2);
    while (hOutBuf) {
        if (Fifo_put(displayFifo, hOutBuf) < 0) {
            ERR("Failed to send buffer to display thread\n");
            return FAILURE;
        }
        hOutBuf = Vdec2_getDisplayBuf(hVd2);
    }

    /* Free up released frames */
    hFreeBuf = Vdec2_getFreeBuf(hVd2);
    while (hFreeBuf) {
        Buffer_freeUseMask(hFreeBuf, CODEC_FREE);
        hFreeBuf = Vdec2_getFreeBuf(hVd2);
    }

    return SUCCESS;
}

/******************************************************************************
 * videoThrFxn
 ******************************************************************************/
Void *videoThrFxn(Void *arg)
{
    VideoEnv               *envp                 = (VideoEnv *) arg;
    Void                   *status               = THREAD_SUCCESS;
    VIDDEC2_Params          defaultDecParams     = Vdec2_Params_DEFAULT;
    VIDDEC2_DynamicParams   defaultDecDynParams  = Vdec2_DynamicParams_DEFAULT;
    VIDENC1_Params          defaultEncParams     = Venc1_Params_DEFAULT;
    VIDENC1_DynamicParams   defaultEncDynParams  = Venc1_DynamicParams_DEFAULT;
    BufferGfx_Attrs         gfxAttrs             = BufferGfx_Attrs_DEFAULT;
    Buffer_Attrs            bAttrs               = Buffer_Attrs_DEFAULT;
    Vdec2_Handle            hVd2                 = NULL;
    Venc1_Handle            hVe1                 = NULL;
    BufTab_Handle           hBufTab              = NULL;
    Engine_Handle           hEngine              = NULL;
    Buffer_Handle           hEncBuf              = NULL;
    Int                     ret                  = Dmai_EOK;
    Buffer_Handle           hVidBuf, hDispBuf, hDstBuf;
    VIDDEC2_Params         *decParams;
    VIDDEC2_DynamicParams  *decDynParams;
    VIDENC1_Params         *encParams;
    VIDENC1_DynamicParams  *encDynParams;
    Int32                   rawBufSize;
    Int                     fifoRet;
    Int                     bufIdx;
    ColorSpace_Type         colorSpace = ColorSpace_YUV420PSEMI;
    IH264VDEC_Params        extnParams;


    Int32                   nbDecVideoBufs;
    BufTab_Handle           hDecBufTab           = NULL;
    Buffer_Handle           hDecBuf              = NULL;
    Int32                   decBufSize;

    /* Use supplied params if any, otherwise use defaults */
    decParams = envp->decParams ? envp->decParams : &defaultDecParams;
    decDynParams = envp->decDynParams ? envp->decDynParams :
                                        &defaultDecDynParams;

    encParams = envp->encParams ? envp->encParams : &defaultEncParams;
    encDynParams = envp->encDynParams ? envp->encDynParams :
                                        &defaultEncDynParams;

    gblSetImageWidth(envp->imageWidth);
    gblSetImageHeight(envp->imageHeight);
    gfxAttrs.colorSpace     = colorSpace;
    gfxAttrs.dim.width      = envp->imageWidth;
    gfxAttrs.dim.height     = envp->imageHeight;
    gfxAttrs.dim.lineLength =
            Dmai_roundUp(BufferGfx_calcLineLength(gfxAttrs.dim.width,
                                   gfxAttrs.colorSpace), 32);

    /* Size of buffers for raw data that will be encoded */
    if (colorSpace != ColorSpace_YUV420PSEMI) {
        rawBufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height;
    } else {
        rawBufSize = gfxAttrs.dim.lineLength * gfxAttrs.dim.height * 3 / 2;
    }

    if (envp->passThrough) {
        /* Only the display thread can own a buffer since no codecs are used */
        gfxAttrs.bAttrs.useMask = DISPLAY_FREE;
    }
    else {
        /* Open the codec engine */
        hEngine = Engine_open(envp->engineName, NULL, NULL);

        if (hEngine == NULL) {
            ERR("Failed to open codec engine %s\n", envp->engineName);
            cleanup(THREAD_FAILURE);
        }
        decParams->maxBitRate = 10485760;
        decParams->maxWidth   = envp->imageWidth;
        decParams->maxHeight  = Dmai_roundUp(envp->imageHeight, 
            CODECHEIGHTALIGN);

        if (envp->videoStd == VideoStd_D1_PAL) {
            decParams->maxFrameRate = 25000;
        } else {
            decParams->maxFrameRate = 30000;
        }

        if (colorSpace ==  ColorSpace_YUV420PSEMI) { 
            decParams->forceChromaFormat = XDM_YUV_420SP;
        } else {
            decParams->forceChromaFormat = XDM_YUV_422ILE;
        }

        if (!strcmp(envp->videoDecoder, "h264dec")) {
            extnParams.displayDelay = 8;
            extnParams.levelLimit = 0;
            extnParams.disableHDVICPeveryFrame = 0;
            extnParams.inputDataMode = 1;
            extnParams.sliceFormat = 1;
            extnParams.frame_closedloop_flag = 1;
            decParams->size = sizeof(IH264VDEC_Params);
        }
                
        extnParams.viddecParams = *decParams;

        /* Create the video decoder */
        hVd2 = Vdec2_create(hEngine, envp->videoDecoder,
                             (VIDDEC2_Params*)&extnParams, decDynParams);

        if (hVd2 == NULL) {
            ERR("Failed to create video decoder: %s\n", envp->videoDecoder);
            cleanup(THREAD_FAILURE);
        }

        /* Which output buffer size does the decoder require? */
        decBufSize = Vdec2_getOutBufSize(hVd2);

        /* Allocate buffer for encoded data */
        hEncBuf = Buffer_create(Vdec2_getInBufSize(hVd2), &bAttrs); 

        if( hEncBuf == NULL) {
            ERR("Failed to allocate buffer for encoded data\n");
            cleanup(THREAD_FAILURE);
        }

        /* Set the resolution to match the specified resolution */
        encParams->maxWidth          = envp->imageWidth;
        encParams->maxHeight         = Dmai_roundUp(envp->imageHeight, 
            CODECHEIGHTALIGN);
        encParams->maxFrameRate      = decParams->maxFrameRate;
        encParams->encodingPreset    = XDM_HIGH_SPEED;    
        
        if (colorSpace ==  ColorSpace_YUV420PSEMI) { 
            encParams->inputChromaFormat = XDM_YUV_420SP;
        } else {
            encParams->inputChromaFormat = XDM_YUV_422ILE;
        }
        
        encParams->reconChromaFormat = XDM_YUV_420SP;

        /* Set up codec parameters depending on bit rate */
        if (envp->videoBitRate < 0) {
            /* Variable bit rate */
            encParams->rateControlPreset = IVIDEO_NONE;

            /*
             * If variable bit rate use a bogus bit rate value (> 0)
             * since it will be ignored.
             */
            encParams->maxBitRate        = 0;
        }
        else {
            /* Constant bit rate */
            encParams->rateControlPreset = IVIDEO_STORAGE;
            encParams->maxBitRate        = envp->videoBitRate;
        }

        encDynParams->targetBitRate = encParams->maxBitRate;
        encDynParams->inputWidth    = encParams->maxWidth;
        encDynParams->inputHeight   = encParams->maxHeight;
        encDynParams->refFrameRate  = encParams->maxFrameRate;
        encDynParams->targetFrameRate = encParams->maxFrameRate;
        encDynParams->interFrameInterval = 0;

        /* Create the video encoder */
        hVe1 = Venc1_create(hEngine, envp->videoEncoder,
                            encParams, encDynParams);
        if (hVe1 == NULL) {
            ERR("Failed to create video encoder: %s\n", envp->videoEncoder);
            cleanup(THREAD_FAILURE);
        }
    }

    /* A hBufTab buffer can be owned only by the codec */
    gfxAttrs.bAttrs.useMask = CODEC_FREE;

    /* Allocate buffers for raw encoder input */
    hBufTab = BufTab_create(NUM_VIDEO_BUFS, rawBufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));

    if (hBufTab == NULL) {
        ERR("Failed to create BufTab for video encode\n");
        cleanup(THREAD_FAILURE);
    }

    /* A hDecBufTab buffer can be owned either by codec or display */
    gfxAttrs.bAttrs.useMask = CODEC_FREE | DISPLAY_FREE;

    /* Allocate 16 buffers for decoder when encode/decode D1 */
    if ((encParams->maxWidth <= 736)&&(encParams->maxHeight<= 480)){
    nbDecVideoBufs = 16;
    } else {
    nbDecVideoBufs = DISPLAY_PIPE_SIZE;
    }

    /* Allocate buffers for decoded output. It is not possible to use
     * the buffers from the encoder input BufTab because buffers have 
     * different sizes.
     */
    hDecBufTab = BufTab_create(nbDecVideoBufs, decBufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));

    if (hDecBufTab == NULL) {
        ERR("Failed to create BufTab for display pipe\n");
        cleanup(THREAD_FAILURE);
    }

    if (!envp->passThrough) {
        /* The decoder is going to use this BufTab for output buffers */
        Vdec2_setBufTab(hVd2, hDecBufTab);
    }

    /* Prime the capture pipe with buffers */
    for (bufIdx = 0; bufIdx < NUM_VIDEO_BUFS; bufIdx++) {
        
        hDstBuf = BufTab_getFreeBuf(hBufTab);

        if (hDstBuf == NULL) {
            ERR("Failed to get free buffer from BufTab\n");
            BufTab_print(hBufTab);
            cleanup(THREAD_FAILURE);
        }

        if (Fifo_put(envp->hCaptureInFifo, hDstBuf) < 0) {
            ERR("Failed to send buffer to capture thread\n");
            cleanup(THREAD_FAILURE);
        }
    }

    /* Make sure the display thread is stopped when it's unlocked */
    Pause_on(envp->hPausePrime);

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    /* Prime the display thread */
    for (bufIdx=0; bufIdx < NUM_VIDEO_BUFS - CAPTURE_PIPE_SIZE  - 1; bufIdx++) {
         if (ret != Dmai_EFIRSTFIELD) {
            /* Get a video buffer from the capture thread */
            fifoRet = Fifo_get(envp->hCaptureOutFifo, &hVidBuf);

            if (fifoRet < 0) {
                ERR("Failed to get buffer from video thread\n");
                cleanup(THREAD_FAILURE);
            }

            /* Did the capture thread flush the fifo? */
            if (fifoRet == Dmai_EFLUSH) {
                cleanup(THREAD_SUCCESS);
            }
        }

        if (!envp->passThrough) {

            /* Get a free buffer from hDecBufTab */
            hDecBuf = BufTab_getFreeBuf(hDecBufTab);

            if (hDecBuf == NULL) {
                ERR("Failed to get free buffer from BufTab\n");
                BufTab_print(hDecBufTab);
                cleanup(THREAD_FAILURE);
            }

            /*
             * Encode and decode the buffer from the capture thread and
             * send any display buffers to the display thread.
             */
            ret = encodedecode(hVe1, hVd2, hVidBuf, hEncBuf, hDecBuf,
                               envp->hDisplayInFifo);

            if (ret < 0) {
                cleanup(THREAD_FAILURE);
            }

            /* Return Video Buffer to Capture Thread */ 
            if (Fifo_put(envp->hCaptureInFifo, hVidBuf) < 0) {
                ERR("Failed to send buffer to capture thread\n");
                cleanup(THREAD_FAILURE);
            }   
        }
        else {
            /* Send the buffer through to the display thread unmodified */
            if (Fifo_put(envp->hDisplayInFifo, hVidBuf) < 0) {
                ERR("Failed to send buffer to display thread\n");
                cleanup(THREAD_FAILURE);
            }
        }
    }

    /* Release the display thread, it is now fully primed */
    Pause_off(envp->hPausePrime);

    /* Main loop */
    while (!gblGetQuit()) {

        /* Get a buffer from the capture thread */
        fifoRet = Fifo_get(envp->hCaptureOutFifo, &hVidBuf);

        if (fifoRet < 0) {
            ERR("Failed to get buffer from capture thread\n");
            cleanup(THREAD_FAILURE);
        }

        /* Did the capture thread flush the fifo? */
        if (fifoRet == Dmai_EFLUSH) {
            cleanup(THREAD_SUCCESS);
        }

        if (!envp->passThrough) {

            /* Get a free buffer from hDecBufTab */
            hDecBuf = BufTab_getFreeBuf(hDecBufTab);

            if (hDecBuf == NULL) {
                ERR("Failed to get free buffer from BufTab\n");
                BufTab_print(hDecBufTab);
                cleanup(THREAD_FAILURE);
            }

            /*
             * Encode and decode the buffer from the capture thread and
             * send any display buffers to the display thread.
             */

            ret = encodedecode(hVe1, hVd2, hVidBuf, hEncBuf, hDecBuf,
                               envp->hDisplayInFifo);

            if (ret < 0) {
                cleanup(THREAD_FAILURE);
            }

            BufferGfx_resetDimensions(hVidBuf);

            /* Return Video Buffer to Capture Thread */
            if (Fifo_put(envp->hCaptureInFifo, hVidBuf) < 0) {
                ERR("Failed to send buffer to capture thread\n");
                cleanup(THREAD_FAILURE);
            }
        }
        else {
            /* Send the buffer through to the display thread unmodified */
            if (Fifo_put(envp->hDisplayInFifo, hVidBuf) < 0) {
                ERR("Failed to send buffer to display thread\n");
                cleanup(THREAD_FAILURE);
            }
        }

        if (ret != Dmai_EFIRSTFIELD) {
            do {
                fifoRet = Fifo_get(envp->hDisplayOutFifo, &hDispBuf);

                if (fifoRet < 0) {
                    ERR("Failed to get buffer from video thread\n");
                    cleanup(THREAD_FAILURE);
                }

                /* Did the display thread flush the fifo? */
                if (fifoRet == Dmai_EFLUSH) {
                    cleanup(THREAD_SUCCESS);
                }
                
                if (!envp->passThrough) {

                    /* The display thread is no longer using the buffer */
                    Buffer_freeUseMask(BufTab_getBuf(hDecBufTab,
                                   Buffer_getId(hDispBuf)),DISPLAY_FREE);
                } else {

                    /* Return Buffer to Capture Thread */
                    if (Fifo_put(envp->hCaptureInFifo, hDispBuf) < 0) {
                        ERR("Failed to send buffer to capture thread\n");
                        cleanup(THREAD_FAILURE);
                    }   
                }

            } while (Fifo_getNumEntries(envp->hDisplayOutFifo) > 0);
        }
    }

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Pause_off(envp->hPauseProcess);
    Pause_off(envp->hPausePrime);
    Fifo_flush(envp->hDisplayInFifo);
    Fifo_flush(envp->hCaptureInFifo);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hEncBuf) {
        Buffer_delete(hEncBuf);
    }

    if (hVd2) {
        Vdec2_delete(hVd2);
    }

    if (hVe1) {
        Venc1_delete(hVe1);
    }

    if (hEngine) {
        Engine_close(hEngine);
    }

    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    return status;
}

