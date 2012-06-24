/*
 * codecs.c
 *
 * This source file has the implementations for the multimedia codecs integrated 
 * for the encodedecode demo on DM365 platform.
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

#include "../demo.h"

/*
 * NULL terminated list of video encoders in the engine to use in the demo.
 * Only the first codec will be used in this demo.
 */
static Codec videoEncoders[] = {
    {
        "mpeg4enc",       /* String name of codec for CE to locate it */
        "MPEG4 SP Video", /* The string to show on the UI for this codec */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,             /* Use default params */
        NULL              /* Use default dynamic params */
    },
    {
        "h264enc",        /* String name of codec for CE to locate it */
        "H.264 HP Video", /* The string to show on the UI for this codec */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,             /* Use default params */
        NULL              /* Use default dynamic params */
    },
    {
        "mpeg2enc",       /* String name of codec for CE to locate it */
        "MPEG2 Video",    /* The string to show on the UI for this codec */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,             /* Use default params */
        NULL              /* Use default dynamic params */
    },
    { NULL }
};

/*
 * NULL terminated list of video decoders in the engine to use in the demo.
 * Only the first codec will be used in this demo, and should be able to
 * decode what's been encoded.
 */
static Codec videoDecoders[] = {
    {
        "mpeg4dec",       /* String name of codec for CE to locate it */
        "MPEG4 SP Video", /* Should match the string given for encode */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,
        NULL
    },
    {
        "h264dec",        /* String name of codec for CE to locate it */
        "H.264 HP Video", /* Should match the string given for encode */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,
        NULL
    },
    {
        "mpeg2dec",       /* String name of codec for CE to locate it */
        "MPEG2 Video",    /* Should match the string given for encode */
        NULL,             /* No file extensions as no files used in this demo */
        NULL,
        NULL
    },
    { NULL }
};

/* Declaration of the production engine and encoders shipped with the DVSDK */
static Engine encodeDecodeEngine = {
    "encodedecode",     /* Engine string name used by CE to find the engine */
    NULL,               /* Speech decoders in engine (not supported) */
    NULL,               /* Audio decoders in engine (not supported) */
    videoDecoders,      /* NULL terminated list of video decoders in engine */
    NULL,               /* Speech encoders in engine (not supported) */
    NULL,               /* Audio encoders in engine (Not supported) */
    videoEncoders       /* NULL terminated list of video encoders in engine */
};

/*
 * This assignment selects which engine will be used by the demo. Note that
 * this file can contain several engine declarations, but this declaration
 * determines which one to use.
 */
Engine *engine = &encodeDecodeEngine;
