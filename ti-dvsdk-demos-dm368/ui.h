/*
 * ui.h
 *
 * This header file has the declarations for the user interface functions
 * implemented for 'DVSDK demo' on DM365 platform
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

#ifndef _UI_H
#define _UI_H

#include <xdc/std.h>

#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/VideoStd.h>

#include "../qtInterface/qtinterface.h"

typedef enum {
    UI_Row_NONE  = -1,
    UI_Row_1     = 50,
    UI_Row_2     = 80,
    UI_Row_3     = 110,
    UI_Row_4     = 140,
    UI_Row_5     = 170,
    UI_Row_6     = 200,
    UI_Row_7     = 230,
    UI_Row_8     = 260,
    UI_Row_9     = 290,
    UI_Row_10    = 320,
    UI_Row_11    = 350,
    UI_Row_12    = 380,
} UI_Row;

typedef enum {
    UI_Value_ArmLoad = 0,
    UI_Value_Fps,
    UI_Value_VideoKbps,
    UI_Value_SoundKbps,
    UI_Value_Time,
    UI_Value_DemoName,
    UI_Value_DisplayType,
    UI_Value_VideoCodec,
    UI_Value_ImageResolution,
    UI_Value_SoundCodec,
    UI_Value_SoundFrequency,
    UI_Num_Values
} UI_Value;

typedef enum {
    UI_Key_INC             = 0x3010,
    UI_Key_DEC             = 0x3011,
    UI_Key_HIDE            = 0x300f,
    UI_Key_PLAY            = 0x3175,
    UI_Key_RECORD          = 0x3177,
    UI_Key_STOP            = 0x3176,
    UI_Key_PAUSE           = 0x3169,
} UI_Key;

typedef struct UI_Attrs {
    Int             osd;
    VideoStd_Type   videoStd;
} UI_Attrs;

typedef struct UI_Object *UI_Handle;

#if defined (__cplusplus)
extern "C" {
#endif

extern UI_Handle UI_create(UI_Attrs *attrs);

extern Void UI_updateValue(UI_Handle hUI, UI_Value type, Char *valString);

extern Void UI_setRow(UI_Handle hUI, UI_Value type, UI_Row row);

extern Void UI_play(UI_Handle hUI);

extern Void UI_pause(UI_Handle hUI);

extern Void UI_stop(UI_Handle hUI);

extern Void UI_toggleVisibility(UI_Handle hUI);

extern Void UI_decTransparency(UI_Handle hUI);

extern Void UI_incTransparency(UI_Handle hUI);

extern Void UI_update(UI_Handle hUI);

extern Void UI_sendKbCmd(UI_Handle hUI, QtInterface_Command cmd);

extern Int UI_getConfig(UI_Handle hUI, Char * option, Char ** cfgString);

extern Int UI_getCmd(UI_Handle hUI, UI_Key * key);

extern Int UI_delete(UI_Handle hUI);

#if defined (__cplusplus)
}
#endif

#endif /* _UI_H */
