/*
 * video.h
 *
 * This header file has the declaration for the video thread
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

#ifndef _VIDEO_H
#define _VIDEO_H

#include <xdc/std.h>

#include <ti/sdo/dmai/Fifo.h>
#include <ti/sdo/dmai/Pause.h>
#include <ti/sdo/dmai/Rendezvous.h>

/* Environment passed when creating the thread */
typedef struct VideoEnv {
    Rendezvous_Handle hRendezvousInit;
    Rendezvous_Handle hRendezvousCleanup;
    Rendezvous_Handle hRendezvousWriter;
    Pause_Handle      hPauseProcess;
    Fifo_Handle       hWriterInFifo;
    Fifo_Handle       hWriterOutFifo;
    Fifo_Handle       hCaptureInFifo;
    Fifo_Handle       hCaptureOutFifo;
    Char             *videoEncoder;
    Char             *engineName;
    Void             *params;
    Void             *dynParams;
    Int32             outBufSize;
    Int               videoBitRate;
    Int               videoFrameRate;
    Int32             imageWidth;
    Int32             imageHeight;
} VideoEnv;

/* Thread function prototype */
extern Void *videoThrFxn(Void *arg);

#endif /* _VIDEO_H */
