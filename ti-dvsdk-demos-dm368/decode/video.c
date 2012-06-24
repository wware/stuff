/*
 * video.c
 *
 * This source file has the implementations for the video thread
 * functions implemented for 'DVSDK decode demo' on DM365 platform
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
#include <ti/sdo/dmai/Loader.h>
#include <ti/sdo/dmai/VideoStd.h>
#include <ti/sdo/dmai/ce/Vdec2.h>
#include <ti/sdo/dmai/BufferGfx.h>
#include <ti/sdo/dmai/Rendezvous.h>

#include "video.h"
#include "../demo.h"

/* Buffering for the display driver */
#define NUM_DISPLAY_BUFS        4 

/* The masks to use for knowing when a buffer is free */
#define CODEC_FREE              0x1
#define DISPLAY_FREE            0x2

#ifndef YUV_420SP
#define YUV_420SP 256
#endif 

/******************************************************************************
 * resizeBufTab
******************************************************************************/
static Int resizeBufTab(Vdec2_Handle hVd2, Int displayBufs)
{
    BufTab_Handle hBufTab = Vdec2_getBufTab(hVd2);
    Int numBufs, numCodecBuffers, numExpBufs;
    Buffer_Handle hBuf;
    Int32 frameSize;

    /* How many buffers can the codec keep at one time? */
    numCodecBuffers = Vdec2_getMinOutBufs(hVd2);

    if (numCodecBuffers < 0) {
        ERR("Failed to get buffer requirements\n");
        return FAILURE;
    }

    /*
     * Total number of frames needed are the number of buffers the codec
     * can keep at any time, plus the number of frames in the display pipe.
     */
    numBufs = numCodecBuffers + displayBufs;

    /* Get the size of output buffers needed from codec */
    frameSize = Vdec2_getOutBufSize(hVd2);

    /*
     * Get the first buffer of the BufTab to determine buffer characteristics.
     * All buffers in a BufTab share the same characteristics.
     */
    hBuf = BufTab_getBuf(hBufTab, 0);

    /* Do we need to resize the BufTab? */
    if (numBufs > BufTab_getNumBufs(hBufTab) ||
        frameSize < Buffer_getSize(hBuf)) {

        /* Should we break the current buffers in to many smaller buffers? */
        if (frameSize < Buffer_getSize(hBuf)) {

            /*
             * Chunk the larger buffers of the BufTab in to smaller buffers
             * to accomodate the codec requirements.
             */
            numExpBufs = BufTab_chunk(hBufTab, numBufs, frameSize);

            if (numExpBufs < 0) {
                ERR("Failed to chunk %d bufs size %ld to %d bufs size %ld\n",
                    BufTab_getNumBufs(hBufTab), Buffer_getSize(hBuf),
                    numBufs, frameSize);
                return FAILURE;
            }

            /*
             * Did the current BufTab fit the chunked buffers,
             * or do we need to expand the BufTab (numExpBufs > 0)?
             */
            if (BufTab_expand(hBufTab, numExpBufs) < 0) {
                ERR("Failed to expand BufTab with %d buffers after chunk\n",
                    numExpBufs);
                return FAILURE;
            }
        }
        else {
            /* Just expand the BufTab with more buffers */
            if (BufTab_expand(hBufTab, (numBufs - BufTab_getNumBufs(hBufTab))) < 0) {
                ERR("Failed to expand BufTab with %d buffers\n",
                    (numBufs - BufTab_getNumBufs(hBufTab)));
                return FAILURE;
            }
        }
    }

    return numBufs;
}

/******************************************************************************
 * handleCodecBufs
******************************************************************************/
static Int handleCodecBufs(Vdec2_Handle hVd2, Fifo_Handle hFifo)
{
    Buffer_Handle hOutBuf, hFreeBuf;
    Int numDisplayBufs = 0;
    UInt16 useMask;

    /* Get a buffer for display from the codec */
    hOutBuf = Vdec2_getDisplayBuf(hVd2);
    while (hOutBuf) {
        /* Mark the buffer as being used by display thread */
        useMask = Buffer_getUseMask(hOutBuf);
        Buffer_setUseMask(hOutBuf, useMask | DISPLAY_FREE);

        /* Send buffer to display thread */
        if (Fifo_put(hFifo, hOutBuf) < 0) {
            ERR("Failed to send buffer to display thread\n");
            return FAILURE;
        }

        numDisplayBufs++;
        
        /* Get another buffer for display from the codec */
        hOutBuf = Vdec2_getDisplayBuf(hVd2);
    }

    /* Get a buffer to free from the codec */
    hFreeBuf = Vdec2_getFreeBuf(hVd2);
    while (hFreeBuf) {
        /* The codec is no longer using the buffer */
        Buffer_freeUseMask(hFreeBuf, CODEC_FREE);
        hFreeBuf = Vdec2_getFreeBuf(hVd2);
    }

    return numDisplayBufs;
}

/******************************************************************************
 * blackFill
 ******************************************************************************/
static Void blackFill(Buffer_Handle hBuf)
{
    Int8  *yPtr     = Buffer_getUserPtr(hBuf);
    Int32  ySize    = Buffer_getSize(hBuf) * 2 / 3;
    Int8  *cbcrPtr  = yPtr + ySize;
    Int    bpp = ColorSpace_getBpp(ColorSpace_YUV420PSEMI);
    Int    y;
    BufferGfx_Dimensions dim;

    BufferGfx_getDimensions(hBuf, &dim); 

    yPtr += dim.y * dim.lineLength + dim.x * bpp / 8;
    for (y = 0; y < dim.height; y++) {
        memset(yPtr, 0x0, dim.width * bpp / 8);
        yPtr += dim.lineLength;
    }

    cbcrPtr += dim.y * dim.lineLength / 2 + dim.x * bpp / 8;
    for (y = 0; y < dim.height / 2; y++) {
        memset(cbcrPtr, 0x80, dim.width * bpp / 8);
        cbcrPtr += dim.lineLength;
    }
}

/******************************************************************************
 * flushDisplayPipe
******************************************************************************/
static Int flushDisplayPipe(Fifo_Handle hFifo, BufTab_Handle hBufTab,
    BufferGfx_Dimensions * dim)
{
   /* Flush the Display pipe with blank buffers */
   Buffer_Handle hBuf;
   Int i;

   for (i = 0; i < NUM_DISPLAY_BUFS; i++) {
       hBuf = BufTab_getFreeBuf(hBufTab);
       if (hBuf == NULL) {
           ERR("No free buffer found for flushing display pipe.\n");
           return FAILURE;
       }

       BufferGfx_setDimensions(hBuf, dim);

       blackFill(hBuf);

       /* Send buffer to display thread */
       if (Fifo_put(hFifo, hBuf) < 0) {
           ERR("Failed to send buffer to display thread\n");
           return FAILURE;
       }
   }

   return SUCCESS;
}

/******************************************************************************
 * findClipDimensions
 ******************************************************************************/
Int findClipDimensions(Loader_Handle hLoader, Char * videoFile, 
    BufTab_Handle hBufTab, Vdec2_Handle hVd2, BufferGfx_Dimensions * dim)
{
    Buffer_Handle           hInBuf, hDstBuf, hOutBuf, hFreeBuf;
    Int                     numBufs        = NUM_DISPLAY_BUFS;
    Int                     frameNbr       = 0;
    Int                     bufIdx;
    Int                     ret;

    /* Prime the file loader and start reading from beginning of file */
    if (Loader_prime(hLoader, &hInBuf) < 0) {
        ERR("Failed to prime loader for file %s\n", videoFile);
        return FAILURE;
    }

    /* 
     * We run thru the clip until we get the first display frame, and use it to 
     * determine the true dimensions of the clip. Then we can re-create the 
     * codec using the true dimensions of the clip as maxHeight and maxWidth.
     * In this manner, the codec's output buffer array can be created at once  
     * with the right dimension without need for chunking the buffers (which 
     * does not resize the buffers that are already in use by the codec). This 
     * is important so that all buffers in the array are of the same size, with 
     * the chroma data located at the same offset, thereby allowing the 
     * display driver to display these buffers directly without having to do 
     * a frame copy. (The driver requires all buffers to use the same chroma 
     * offset.)
     */  
    for (bufIdx=0; bufIdx < numBufs; ) {
        if (ret != Dmai_EFIRSTFIELD) {
            /* Get a free buffer from the BufTab */
            hDstBuf = BufTab_getFreeBuf(hBufTab);

            if (hDstBuf == NULL) {
                ERR("Failed to get free buffer from display pipe BufTab\n");
                BufTab_print(hBufTab);
                return FAILURE;
            }
            bufIdx++;
        }

        /* Make sure the whole buffer is used for output */
        BufferGfx_resetDimensions(hDstBuf);

        /* Make sure we have some data in input buffer, otherwise DMAI will generate assertion */
        if (Buffer_getNumBytesUsed(hInBuf) == 0) {
            ERR("Failed to read data from loader, may be we reached EOF ???\n");
            return FAILURE;
        }

        /* Decode the video buffer */
        ret = Vdec2_process(hVd2, hInBuf, hDstBuf);

        if (ret < 0) {
            ERR("Failed to decode video buffer\n");
            return FAILURE;
        }

        /* If no encoded data was used we cannot find the next frame */
        if (ret == Dmai_EBITERROR && Buffer_getNumBytesUsed(hInBuf) == 0) {
            ERR("Fatal bit error\n");
            return FAILURE;
        }

        /* Get a buffer for display from the codec */
        hOutBuf = Vdec2_getDisplayBuf(hVd2);
        if (hOutBuf) {
            /* Get the clip's actual width and height */
            BufferGfx_getDimensions(hOutBuf, dim);
            break;
        }

        /* Get a buffer to free from the codec */
        hFreeBuf = Vdec2_getFreeBuf(hVd2);
        while (hFreeBuf) {
            /* The codec is no longer using the buffer */
            BufTab_freeBuf(hFreeBuf);
            hFreeBuf = Vdec2_getFreeBuf(hVd2);
        }

        if (frameNbr == 0) {
            /*
             * Resize the BufTab after the first frame has been processed.
             * This because the codec may not know it's buffer requirements
             * before the first frame has been decoded.
             */
            numBufs = resizeBufTab(hVd2, NUM_DISPLAY_BUFS);

            if (numBufs < 0) {
                return FAILURE;
            }
        }

        /* Load a new encoded frame from the file system */
        if (Loader_getFrame(hLoader, hInBuf) < 0) {
            ERR("Failed to get frame of encoded data during clip size estimation\n");
            return FAILURE;
        }

        /* End of clip? */
        if (Buffer_getUserPtr(hInBuf) == NULL) {
            printf("Clip ended, exiting demo..\n");
            return FAILURE;
        }

        frameNbr++;
    }

    if (hOutBuf == NULL) {
        ERR("Failed to determine video clip's dimensions. Stream may be "
            "unsupported by the video decoder.\n");
        return FAILURE;
    }

    /* Record clip's width and height for OSD display */
    gblSetImageWidth(dim->width);
    gblSetImageHeight(dim->height);

    return SUCCESS;
}

/******************************************************************************
 * videoThrFxn
 ******************************************************************************/
Void *videoThrFxn(Void *arg)
{
    VideoEnv               *envp                = (VideoEnv *) arg;
    Void                   *status              = THREAD_SUCCESS;
    VIDDEC2_Params          defaultParams       = Vdec2_Params_DEFAULT;
    VIDDEC2_DynamicParams   defaultDynParams    = Vdec2_DynamicParams_DEFAULT;
    BufferGfx_Attrs         gfxAttrs            = BufferGfx_Attrs_DEFAULT;
    Loader_Attrs            lAttrs              = Loader_Attrs_DEFAULT;
    Vdec2_Handle            hVd2                = NULL;
    Loader_Handle           hLoader             = NULL;
    BufTab_Handle           hBufTab             = NULL;
    BufTab_Handle           hBufTabFlush        = NULL;
    Engine_Handle           hEngine             = NULL;
    Buffer_Handle           hDstBuf, hInBuf, hDispBuf, hBuf;
    Int                     bufIdx, bufsSent, numDisplayBufs, numBufs;
    Int                     totalNumBufs;
    Int                     fifoRet, ret, frameNbr;
    VIDDEC2_Params         *params;
    VIDDEC2_DynamicParams  *dynParams;
    Int32                   bufSize;
    ColorSpace_Type         colorSpace = ColorSpace_YUV420PSEMI;
    Int                     numFlushBufsSent    = 0;
    Int                     idx;
    BufferGfx_Dimensions    dim;

    /* Open the codec engine */
    hEngine = Engine_open(envp->engineName, NULL, NULL);

    if (hEngine == NULL) {
        ERR("Failed to open codec engine %s\n", envp->engineName);
        cleanup(THREAD_FAILURE);
    }

    /* Use supplied params if any, otherwise use defaults */
    params = envp->params ? envp->params : &defaultParams;
    dynParams = envp->dynParams ? envp->dynParams : &defaultDynParams;

    params->maxWidth     = VideoStd_1080I_WIDTH;
    params->maxHeight    = VideoStd_1080I_HEIGHT;

    if (envp->videoStd == VideoStd_D1_PAL) {
        params->maxFrameRate = 25000;
    } 
    else {
        params->maxFrameRate = 30000;
    }
    
    if (colorSpace ==  ColorSpace_YUV420PSEMI) { 
        params->forceChromaFormat = XDM_YUV_420SP;
    } else {
        params->forceChromaFormat = XDM_YUV_422ILE;
    }

    /* Create the video decoder */
    hVd2 = Vdec2_create(hEngine, envp->videoDecoder, params, dynParams);

    if (hVd2 == NULL) {
        ERR("Failed to create video decoder: %s\n", envp->videoDecoder);
        cleanup(THREAD_FAILURE);
    }

    /* Which output buffer size does the codec require? */
    bufSize = Vdec2_getOutBufSize(hVd2);

    /* Both the codec and the display thread can own a buffer */
    gfxAttrs.bAttrs.useMask = CODEC_FREE;

    /* Color space */
    gfxAttrs.colorSpace = colorSpace;

    /* Set the original dimensions of the Buffers to the max */
    gfxAttrs.dim.width = params->maxWidth;
    gfxAttrs.dim.height = params->maxHeight;
    gfxAttrs.dim.lineLength = BufferGfx_calcLineLength(gfxAttrs.dim.width,
                                                       colorSpace);

    /* Create a table of buffers for decoded data */
    hBufTab = BufTab_create(NUM_DISPLAY_BUFS, bufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));

    if (hBufTab == NULL) {
        ERR("Failed to create BufTab for display pipe\n");
        cleanup(THREAD_FAILURE);
    }

    /* The codec is going to use this BufTab for output buffers */
    Vdec2_setBufTab(hVd2, hBufTab);

    /* Ask the codec how much input data it needs */
    lAttrs.readSize = Vdec2_getInBufSize(hVd2);

    /* Let the loader thread read 300000 bytes extra */
    lAttrs.readAhead = 300000;

    /* Make the total ring buffer larger */
    lAttrs.readBufSize = (lAttrs.readSize + lAttrs.readAhead) * 2;

    /* Use asynchronous mode since we have a separate loader thread */
    lAttrs.async = TRUE;

    /* Create the file loader for reading encoded data */
    hLoader = Loader_create(envp->videoFile, &lAttrs);

    if (hLoader == NULL) {
        ERR("Failed to create loader for file %s\n", envp->videoFile);
        cleanup(THREAD_FAILURE);
    }

    /* The environment copy will be shared with the loader thread */
    envp->hLoader = hLoader;

    /* Signal that the Loader is created */
    Rendezvous_meet(envp->hRendezvousLoader);

    /* Make sure the display thread is stopped when it's unlocked */
    Pause_on(envp->hPausePrime);

    /* Signal that initialization is done and wait for other threads */
    Rendezvous_meet(envp->hRendezvousInit);

    /* Find the clip's dimensions */
    if (findClipDimensions(hLoader, envp->videoFile, hBufTab, hVd2, &dim) 
        == FAILURE) {
        cleanup(THREAD_FAILURE);
    };

    /* Adjust width and height to match the clip's dimensions */
    params->maxWidth     = dim.width;
    params->maxHeight    = dim.height;

    /* Recreate the codec instance with actual clip's width and height */
    if (hVd2) {
        Vdec2_delete(hVd2);
    }

    /* Create the video decoder */
    hVd2 = Vdec2_create(hEngine, envp->videoDecoder, params, dynParams);

    if (hVd2 == NULL) {
        ERR("Failed to create video decoder: %s\n", envp->videoDecoder);
        cleanup(THREAD_FAILURE);
    }

    /* Which output buffer size does the codec require? */
    bufSize = Vdec2_getOutBufSize(hVd2);

    /* Recreate the BufTab */
    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    /* Set the original dimensions of the Buffers to the max */
    gfxAttrs.dim.width = params->maxWidth;
    gfxAttrs.dim.height = params->maxHeight;
    gfxAttrs.dim.lineLength = BufferGfx_calcLineLength(gfxAttrs.dim.width,
                                                       colorSpace);

    /* Create a table of buffers for decoded data */
    hBufTab = BufTab_create(NUM_DISPLAY_BUFS, bufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));

    if (hBufTab == NULL) {
        ERR("Failed to create BufTab for display pipe\n");
        cleanup(THREAD_FAILURE);
    }

    /* 
     * The codec is going to use the new BufTab for output buffers 
     */
    Vdec2_setBufTab(hVd2, hBufTab);

    frameNbr = 0;
    numDisplayBufs = 0;

prime: 
    /* Initialize the state of the decode */
    ret = Dmai_EOK;

    if (frameNbr == 0) {
        numBufs = NUM_DISPLAY_BUFS;
    }
    else {
        /* when looping, the display thread was previously primed */
        numBufs = totalNumBufs - NUM_DISPLAY_BUFS;
    }

    /* Prime the file loader and start reading from beginning of file */
    if (Loader_prime(hLoader, &hInBuf) < 0) {
        ERR("Failed to prime loader for file %s\n", envp->videoFile);
        cleanup(THREAD_FAILURE);
    }

    /* Prime the display thread with video buffers */
    for (bufIdx=0; bufIdx < numBufs; ) {
        if (ret != Dmai_EFIRSTFIELD) {
            /* Get a free buffer from the BufTab */
            hDstBuf = BufTab_getFreeBuf(hBufTab);

            if (hDstBuf == NULL) {
                ERR("Failed to get free buffer from display pipe BufTab\n");
                BufTab_print(hBufTab);
                cleanup(THREAD_FAILURE);
            }
            bufIdx++;
        }

        /* Make sure the whole buffer is used for output */
        BufferGfx_resetDimensions(hDstBuf);

        /* Decode the video buffer */
        ret = Vdec2_process(hVd2, hInBuf, hDstBuf);

        if (ret < 0) {
            ERR("Failed to decode video buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* If no encoded data was used we cannot find the next frame */
        if (ret == Dmai_EBITERROR && Buffer_getNumBytesUsed(hInBuf) == 0) {
            ERR("Fatal bit error\n");
            cleanup(THREAD_FAILURE);
        }

        /* Increment statistics for the user interface */
        gblIncVideoBytesProcessed(Buffer_getNumBytesUsed(hInBuf));

        /* Send frames to display thread */
        bufsSent = handleCodecBufs(hVd2, envp->hDisplayInFifo);

        if (bufsSent < 0) {
            cleanup(THREAD_FAILURE);
        }

        /* Keep track of the number of buffers sent to the display thread */
        numDisplayBufs += bufsSent;

        if (frameNbr == 0) {
            /*
             * Resize the BufTab after the first frame has been processed.
             * This because the codec may not know it's buffer requirements
             * before the first frame has been decoded.
             */
            numBufs = resizeBufTab(hVd2, NUM_DISPLAY_BUFS);

            if (numBufs < 0) {
                cleanup(THREAD_FAILURE);
            }

            /* 
             * Set the total number of buffers used between video and display 
             * threads.
             */
            totalNumBufs = numBufs;
        }

        /* Load a new encoded frame from the file system */
        if (Loader_getFrame(hLoader, hInBuf) < 0) {
            ERR("Failed to get frame of encoded data during priming\n");
            cleanup(THREAD_FAILURE);
        }

        /* End of clip? */
        if (Buffer_getUserPtr(hInBuf) == NULL) {
            printf("Clip ended, exiting demo..\n");
            cleanup(THREAD_SUCCESS);
        }

        frameNbr++;
    }

    /* Release the display thread, it is now fully primed */
    Pause_off(envp->hPausePrime);

    /* Main loop */
    while (!gblGetQuit()) {
        if (ret != Dmai_EFIRSTFIELD) {
            /* Get a displayed frame from the display thread */
            fifoRet = Fifo_get(envp->hDisplayOutFifo, &hDispBuf);

            if (fifoRet != Dmai_EOK) {
                cleanup(THREAD_FAILURE);
            }

            /* Did the display thread flush the fifo? */
            if (fifoRet == Dmai_EFLUSH) {
                cleanup(THREAD_SUCCESS);
            }

            /* The display thread is no longer using the buffer */
            Buffer_freeUseMask(hDispBuf, DISPLAY_FREE);

            /* Keep track of the number of buffers sent to the display thread */
            numDisplayBufs--;

            /* Get a free buffer from the BufTab to give to the codec */
            hDstBuf = BufTab_getFreeBuf(hBufTab);

            if (hDstBuf == NULL) {
                ERR("Failed to get free buffer from BufTab\n");
                BufTab_print(hBufTab);
                cleanup(THREAD_FAILURE);
            }
        }

        /* Make sure the whole buffer is used for output */
        BufferGfx_resetDimensions(hDstBuf);

        /* Decode the video buffer */
        ret = Vdec2_process(hVd2, hInBuf, hDstBuf);

        if (ret < 0) {
            ERR("Failed to decode video buffer\n");
            cleanup(THREAD_FAILURE);
        }

        /* If no encoded data was used we cannot find the next frame */
        if (ret == Dmai_EBITERROR && Buffer_getNumBytesUsed(hInBuf) == 0) {
            ERR("Fatal bit error\n");
            cleanup(THREAD_FAILURE);
        }

        /* Increment statistics for the user interface */
        gblIncVideoBytesProcessed(Buffer_getNumBytesUsed(hInBuf));
 
        
        /* Send frames to display thread */
        bufsSent = handleCodecBufs(hVd2, envp->hDisplayInFifo);

        if (bufsSent < 0) {
            cleanup(THREAD_FAILURE);
        }

        /* Keep track of the number of buffers sent to the display thread */
        numDisplayBufs += bufsSent;

        /* Load a new encoded frame from the file system */
        if (Loader_getFrame(hLoader, hInBuf) < 0) {
            ERR("Failed to get frame of encoded data from file system\n");
            cleanup(THREAD_FAILURE);
        }

        frameNbr++;

        /* End of clip? */
        if (Buffer_getUserPtr(hInBuf) == NULL) {           
            /* Flush the codec for display frames */
            Vdec2_flush(hVd2);

            bufsSent = 0;
            do {
                /*
                 * Temporarily create a dummy buffer for the process call.
                 * After a flush the codec ignores the input buffer, but since 
                 * Codec Engine still address translates the buffer, it needs 
                 * to exist.
                 */
                hInBuf = Buffer_create(1, BufferGfx_getBufferAttrs(&gfxAttrs));

                if (hInBuf == NULL) {
                    ERR("Failed to allocate dummy buffer\n");
                    cleanup(THREAD_FAILURE);
                }

                Buffer_setNumBytesUsed(hInBuf, 1);

                ret = Vdec2_process(hVd2, hInBuf, hDstBuf);

                if (ret < 0) {
                    ERR("Failed to decode video buffer\n");
                    cleanup(THREAD_FAILURE);
                }

                Buffer_delete(hInBuf);
            
                /* Keep track of the # of buffers sent to the display thread */
                numDisplayBufs += bufsSent;

                /* Send frames to display thread */
                bufsSent = handleCodecBufs(hVd2, envp->hDisplayInFifo);

            } while(bufsSent > 0);

            /* Flush the display pipe if not looping */
            if (!envp->loop) {
                /* Create a table of buffers for decoded data */
                gfxAttrs.dim = dim;

                hBufTabFlush = BufTab_create(NUM_DISPLAY_BUFS, bufSize,
                            BufferGfx_getBufferAttrs(&gfxAttrs));

                if (hBufTabFlush == NULL) {
                    ERR("Failed to create BufTab for flushing\n");
                    cleanup(THREAD_FAILURE);
                }

                if (flushDisplayPipe(envp->hDisplayInFifo, hBufTabFlush, &dim) 
                    == FAILURE) {
                    cleanup(THREAD_FAILURE);
                }

                numFlushBufsSent = NUM_DISPLAY_BUFS;
            }

            /* Drain the display thread making sure all frames are displayed */
            while (numDisplayBufs > (NUM_DISPLAY_BUFS - numFlushBufsSent)) {
                /* Get a displayed frame from the display thread */
                fifoRet = Fifo_get(envp->hDisplayOutFifo, &hDispBuf);

                if (fifoRet != Dmai_EOK) {
                    cleanup(THREAD_FAILURE);
                }

                /* Did the display thread flush the fifo? */
                if (fifoRet == Dmai_EFLUSH) {
                    cleanup(THREAD_SUCCESS);
                }

                /* The display thread is no longer using the buffer */
                Buffer_freeUseMask(hDispBuf, DISPLAY_FREE);

                /* Keep track of number of buffers sent to the display thread */
                numDisplayBufs--;
            }

            /* Wait for audio to complete if applicable */
            Rendezvous_meet(envp->hRendezvousLoop);

            /* Loop the clip or quit? */
            if (envp->loop) {
                /* Make sure the decoder has no state by recreating it */
                Vdec2_delete(hVd2);

                /* Make sure any buffers kept by the codec are freed */
                for (idx = 0; idx < BufTab_getNumBufs(hBufTab); idx++) {
                    hBuf = BufTab_getBuf(hBufTab, idx);
                    Buffer_freeUseMask(hBuf, CODEC_FREE);
                }

                hVd2 = Vdec2_create(hEngine, envp->videoDecoder,
                                    params, dynParams);
                if (hVd2 == NULL) {
                    ERR("Failed to create video decoder: %s\n",
                        envp->videoDecoder);
                    cleanup(THREAD_FAILURE);
                }

                /* The codec is going to use this BufTab for output buffers */
                Vdec2_setBufTab(hVd2, hBufTab);

                /* Halt the display thread for priming */
                Pause_on(envp->hPausePrime);

                goto prime;
            }
            else {
                printf("Clip ended, exiting demo..\n");
                gblSetQuit();
            }
        } /* End of clip? */
    } /* Main loop */

cleanup:
    /* Make sure the other threads aren't waiting for us */
    Rendezvous_force(envp->hRendezvousInit);
    Rendezvous_force(envp->hRendezvousLoop);
    Rendezvous_force(envp->hRendezvousLoader);
    Pause_off(envp->hPauseProcess);
    Pause_off(envp->hPausePrime);
    Fifo_flush(envp->hDisplayInFifo);
    if (hLoader) Loader_flush(hLoader);

    /* Meet up with other threads before cleaning up */
    Rendezvous_meet(envp->hRendezvousCleanup);

    /* Clean up the thread before exiting */
    if (hLoader) {
        Loader_delete(hLoader);
    }

    if (hVd2) {
        Vdec2_delete(hVd2);
    }

    if (hEngine) {
        Engine_close(hEngine);
    }

    if (hBufTab) {
        BufTab_delete(hBufTab);
    }

    if (hBufTabFlush) {
        BufTab_delete(hBufTabFlush);
    }

    return status;
}

