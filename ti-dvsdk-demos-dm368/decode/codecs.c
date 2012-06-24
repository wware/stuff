/*
 * codecs.c
 *
 * This source file has the implementations for the multimedia codecs integrated 
 * for the decode demo on DM365 platform.
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

#include <ti/xdais/dm/iauddec1.h>
#include <ti/xdais/dm/ividdec2.h>
#include "../demo.h"

/* File extensions for G.711 */
static Char *g711Extensions[] = { ".g711", NULL };

/* NULL terminated list of speech decoders in the engine to use in the demo */
static Codec speechDecoders[] = {
    {
        "g711dec",            /* String name of codec for CE to locate it */
        "G.711 Speech",       /* The string to show on the UI for this codec */
        g711Extensions,       /* NULL terminated list of file extensions */
        NULL,                 /* Use default params */
        NULL                  /* Use default dynamic params */
   },
    { NULL }
};

/* File extensions for AAC and mpeg1l2 */
static Char *aacExtensions[] = { ".aac", NULL };

//#include <ittiam/app/aac_dec_app/ienhaacpdec.h>
//extern ITTIAM_ENHAACPDEC_Params ENHAACPDECODER_ITTIAM_PARAMS;
static IAUDDEC1_Params AAC_PARAMS = {
    sizeof(IAUDDEC1_Params),
    16,
    IAUDIO_INTERLEAVED,
    XDM_LE_16,
};

#include <ti/sdo/codecs/h264dec/ih264vdec.h>
static IH264VDEC_Params H264_PARAMS = {
    { sizeof(IH264VDEC_Params),
      576,
      720,
      30000,
      6000000,
      XDM_BYTE,
      XDM_YUV_420P,
    },
    16,    /* display delay */
    NULL,  /* hdvicpHandle */
    0,     /* disableHDVICPeveryFrame */ 
    0,     /* maximum level limit */
    1,     /* closed loop mode */
    1,     /* input data mode */
    1,     /* slice format */
};

/* NULL terminated list of audio decoders in the engine to use in the demo */
static Codec audioDecoders[] = {
    {
        "aacdec",
        "AAC",
        aacExtensions,
        &AAC_PARAMS,
        NULL
    },
    { NULL }
};

/* File extensions for MPEG 4 */
static Char *mpeg4Extensions[] = { ".mpeg4", ".m4v", NULL };

/* File extensions for H.264 */
static Char *h264Extensions[] = { ".264", NULL };

/* File extensions for MPEG 2 */
static Char *mpeg2Extensions[] = { ".mpeg2", ".m2v", NULL };

/* NULL terminated list of video decoders in the engine to use in the demo */
static Codec videoDecoders[] = {
    {
        "mpeg4dec",
        "MPEG4 SP Video",
        mpeg4Extensions,
        NULL,
        NULL
    },
    {
        "h264dec",
        "H.264 HP Video",
        h264Extensions,
        &H264_PARAMS,
        NULL
    },
    {
        "mpeg2dec",
        "MPEG2 Video",
        mpeg2Extensions,
        NULL,
        NULL
    },    
    { NULL }
};

/* Declaration of the production engine and decoders shipped with the DVSDK */
static Engine decodeEngine = {
    "decode",           /* Engine string name used by CE to find the engine */
    speechDecoders,     /* NULL terminated list of speech decoders in engine */
    audioDecoders,      /* NULL terminated list of audio decoders in engine */ 
    videoDecoders,      /* NULL terminated list of video decoders in engine */
    NULL,               /* NULL terminated list of speech encoders in engine */
    NULL,               /* NULL terminated list of audio encoders in engine */
    NULL                /* NULL terminated list of video encoders in engine */
};

/*
 * This assignment selects which engine will be used by the demo. Note that
 * this file can contain several engine declarations, but this declaration
 * determines which one to use.
 */
Engine *engine = &decodeEngine;
