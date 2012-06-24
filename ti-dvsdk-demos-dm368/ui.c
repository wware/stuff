/*
 * ui.c
 *
 * This source file has the implementations for the user interface functions
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

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/stat.h>

#include <xdc/std.h>

#include <ti/sdo/dmai/Buffer.h>
#include <ti/sdo/dmai/Display.h>

#include "demo.h"
#include "ui.h"
#include "../qtInterface/qtinterface.h"

#define MAX_STRING_LENGTH       QTINTERFACE_MAXSTRINGSIZE

#define yScale(std, y) std == VideoStd_D1_NTSC ? y : y * 12 / 10

typedef enum {
    UI_BufType_WORK = 0,
    UI_BufType_DISPLAY,
    UI_BufType_NUMTYPES
} UI_Buftype;

typedef struct UI_Object {
    Display_Handle      hAttr;
    FILE *              fpCmd;
    FILE *              fpConfig;
    FILE *              fpKbCmd;
    FILE *              fpStatus;
    Int                 osd;
    VideoStd_Type       videoStd;
} UI_Object;

typedef struct UI_String_Object {
    Int                 y;
    Int                 valid;
    Char                col1String[MAX_STRING_LENGTH];
    Char                col2String[MAX_STRING_LENGTH];
} UI_String_Object;

static UI_String_Object stringAttrs[UI_Num_Values] = {
    { UI_Row_1,  FALSE, "ARM Load:"         }, /* ARM load */
    { UI_Row_2,  FALSE, "Video fps:"        }, /* Video FPS */
    { UI_Row_3,  FALSE, "Video bit rate:"   }, /* Video bitrate */
    { UI_Row_4,  FALSE, "Sound bit rate:"   }, /* Sound bitrate */
    { UI_Row_5,  FALSE, "Time:"             }, /* Time */
    { UI_Row_7,  FALSE, "Demo:"             }, /* Demo name */
    { UI_Row_8,  FALSE, "Display:"          }, /* Display type */
    { UI_Row_9,  FALSE, "Video Codec:"      }, /* Video Codec */
    { UI_Row_10, FALSE, "Resolution:"       }, /* Image resolution */
    { UI_Row_11, FALSE, "Sound Codec:"      }, /* Sound codec */
    { UI_Row_12, FALSE, "Sampling Freq:"    }, /* Sound sampling frequency */
};

typedef struct UI_ConfigString {
    char string[QTINTERFACE_MAXSTRINGSIZE];
} UI_ConfigString;

static UI_ConfigString configStrings[QTINTERFACE_MAXCONFIGITEMS];

/******************************************************************************
 * fgetstr
 ******************************************************************************/
static char * fgetstr(char *string, int n, FILE *stream)
{
    char *result;
    result = fgets(string, n, stream);

    if(!result) {
        return(result);
    }
    
    if(string[strlen(string) - 1] == '\n') {
        string[strlen(string) - 1] = 0;
    }
    
    return(string);
}

/******************************************************************************
 * drawValue
 ******************************************************************************/
static Void drawValue(UI_Handle hUI, UI_Value type)
{
    switch (type)
    {
        case UI_Value_ArmLoad:
            fputc(1, hUI->fpStatus);
            break;
        case UI_Value_Fps:
            fputc(2, hUI->fpStatus);
            break;
        case UI_Value_VideoKbps:
            fputc(3, hUI->fpStatus);
            break;
        case UI_Value_SoundKbps:
            fputc(4, hUI->fpStatus);
            break;
        case UI_Value_Time:
            fputc(6, hUI->fpStatus);
            break;
        case UI_Value_DemoName:
            fputc(7, hUI->fpStatus);
            break;
        case UI_Value_DisplayType:
            fputc(8, hUI->fpStatus);
            break;
        case UI_Value_VideoCodec:
            fputc(9, hUI->fpStatus);
            break;
        case UI_Value_ImageResolution:
            fputc(10, hUI->fpStatus);
            break;
        case UI_Value_SoundCodec:
            fputc(11, hUI->fpStatus);
            break;
        case UI_Value_SoundFrequency:
            fputc(12, hUI->fpStatus);
            break;
        default:
            printf("Bad UI item value\n");
            return;
    }
    fputs(stringAttrs[type].col1String, hUI->fpStatus);
    fputc('\n', hUI->fpStatus);
    fputs(stringAttrs[type].col2String, hUI->fpStatus);
    fputc('\n', hUI->fpStatus);
}

/******************************************************************************
 * cleanupUI
 ******************************************************************************/
static Int cleanupUI(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Close opened pipes */
        fclose(hUI->fpCmd);
        fclose(hUI->fpConfig);
        fclose(hUI->fpKbCmd);
        fclose(hUI->fpStatus);
    }

    free(hUI);

    return SUCCESS;
}

/******************************************************************************
 * setOsdTransparency
 ******************************************************************************/
static Int setOsdTransparency(UI_Handle hUI, Char trans)
{
    Buffer_Handle hBuf;

    if (Display_get(hUI->hAttr, &hBuf) < 0) {
        ERR("Failed to get attribute window buffer\n");
        return FAILURE;
    }

    memset(Buffer_getUserPtr(hBuf), trans, Buffer_getSize(hBuf));

    if (Display_put(hUI->hAttr, hBuf) < 0) {
        ERR("Failed to put display buffer\n");
        return FAILURE;
    }

    return SUCCESS;
}

/******************************************************************************
 * UI_create
 ******************************************************************************/
UI_Handle UI_create(UI_Attrs *attrs)
{
    Display_Attrs          aAttrs      = Display_Attrs_DM365_ATTR_DEFAULT;
    UI_Handle              hUI;

    hUI = calloc(1, sizeof(UI_Object));

    if (hUI == NULL) {
        ERR("Failed to allocate space for UI Object\n");
        return NULL;
    }

    hUI->osd = attrs->osd;
    hUI->videoStd = attrs->videoStd;

    if (!attrs->osd) {
        aAttrs.videoStd = hUI->videoStd;
        /* Create display device driver instance */
        if ( (aAttrs.videoStd == VideoStd_CIF) ||
            (aAttrs.videoStd == VideoStd_SIF_NTSC) ||
            (aAttrs.videoStd == VideoStd_SIF_PAL) ||
            (aAttrs.videoStd == VideoStd_VGA) ||
            (aAttrs.videoStd == VideoStd_D1_NTSC) ||        
            (aAttrs.videoStd == VideoStd_D1_PAL) ) {
            aAttrs.videoOutput = Display_Output_COMPOSITE;
        } 
        else {
            aAttrs.videoStd = VideoStd_576P;
            aAttrs.videoOutput = Display_Output_COMPONENT;
        }
    
        hUI->hAttr = Display_create(NULL, &aAttrs);

        if (hUI->hAttr == NULL) {
            ERR("Failed to create attribute window device\n");
            cleanupUI(hUI);
            return NULL;
        }

        /* Clear the OSD if using keyboard interface */
        setOsdTransparency(hUI, 0);
    }

    /* Create the FIFOs if it does not exist */
    if (attrs->osd) {
        umask(0);

        mknod(CMD_FIFO_FILE, S_IFIFO|0666, 0);
        mknod(CONFIG_FIFO_FILE, S_IFIFO|0666, 0);
        mknod(KB_CMD_FIFO_FILE, S_IFIFO|0666, 0);
        mknod(STATUS_FIFO_FILE, S_IFIFO|0666, 0);

        hUI->fpCmd = fopen(CMD_FIFO_FILE, "r");
        hUI->fpConfig = fopen(CONFIG_FIFO_FILE, "r");
        hUI->fpKbCmd = fopen(KB_CMD_FIFO_FILE, "w");
        hUI->fpStatus = fopen(STATUS_FIFO_FILE, "w");

        if ((hUI->fpCmd == NULL) || (hUI->fpConfig == NULL) || 
            (hUI->fpKbCmd == NULL) || (hUI->fpStatus == NULL)) {
            ERR("Failed to open pipes\n");
            cleanupUI(hUI);
            return NULL;
        }  

        setvbuf(hUI->fpCmd, NULL, _IONBF, 0);
        setvbuf(hUI->fpConfig, NULL, _IONBF, 0);
        setvbuf(hUI->fpKbCmd, NULL, _IONBF, 0);
        setvbuf(hUI->fpStatus, NULL, _IONBF, 0);
    }

    return hUI;
}

/******************************************************************************
 * UI_sendKbCmd
 ******************************************************************************/
Void UI_sendKbCmd(UI_Handle hUI, QtInterface_Command cmd)
{
    fputc(cmd, hUI->fpKbCmd);
}

/******************************************************************************
 * UI_getConfig
 ******************************************************************************/
Int UI_getConfig(UI_Handle hUI, Char * option, Char ** cfgString)
{
    static Int i = 0;
    
    if (i < QTINTERFACE_MAXCONFIGITEMS) {
        *option = fgetc(hUI->fpConfig);
        /* Got config option. Process it */
        if (*option != '\27') {
            /* Option is not ETB. Get the config string */
            fgetstr(configStrings[i].string, (int)QTINTERFACE_MAXSTRINGSIZE, 
                hUI->fpConfig);
            *cfgString = configStrings[i].string;
        }
        else {
            /* ETB received. End of configuration */
        } 
        i++;
        return SUCCESS;
    }
    else {
        ERR("Maximum number of configuration items exceeded.\n");
        return FAILURE;
    }
}

/******************************************************************************
 * UI_getCmd
 ******************************************************************************/
Int UI_getCmd(UI_Handle hUI, UI_Key * key)
{
    fd_set rfds;
    struct timeval tv;
    int retval;
    int cmd;

    FD_ZERO(&rfds);
    FD_SET(fileno(hUI->fpCmd), &rfds);
    tv.tv_sec = 1;
    tv.tv_usec = 0;

    /* peek every second to see if there is any status message from demo */
    retval = select(fileno(hUI->fpCmd) + 1, &rfds, NULL, NULL, &tv);
    if (retval == -1) {
        ERR("select error\n");
        return FAILURE;
    }
    else if (retval) {
        /* Command is available. Process it */
        cmd = fgetc(hUI->fpCmd);
        switch (cmd) 
        {
            case QtInterface_Play:
                *key = UI_Key_PLAY;
                break;
            case QtInterface_Pause:
                *key = UI_Key_PAUSE;
                break;
            case QtInterface_Stop:
                *key = UI_Key_STOP;
                break;
            default:
                printf("Unrecognized key. Ignored...\n");
                *key = 0;
                break;
        }
    }
    else {
        *key = 0; 
    }

    return SUCCESS; 
}

/******************************************************************************
 * UI_play
 ******************************************************************************/
Void UI_play(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Play);
    }

}

/******************************************************************************
 * UI_pause
 ******************************************************************************/
Void UI_pause(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Pause);
    }

}

/******************************************************************************
 * UI_stop
 ******************************************************************************/
Void UI_stop(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Stop);
    }
}

/******************************************************************************
 * UI_toggleVisibility
 ******************************************************************************/
Void UI_toggleVisibility(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Hide);
    }
}

/******************************************************************************
 * UI_incTransparency
 ******************************************************************************/
Void UI_incTransparency(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Inc);
    }
}

/******************************************************************************
 * UI_decTransparency
 ******************************************************************************/
Void UI_decTransparency(UI_Handle hUI)
{
    if (hUI->osd) {
        /* Send command to update GUI interface */
        UI_sendKbCmd(hUI, QtInterface_Dec);
    }
}

/******************************************************************************
 * UI_updateValue
 ******************************************************************************/
Void UI_updateValue(UI_Handle hUI, UI_Value type, Char *valString)
{
    if (stringAttrs[type].y >= 0) {
        strncpy(stringAttrs[type].col2String, valString, MAX_STRING_LENGTH);
        stringAttrs[type].valid = TRUE;
    }
}

/******************************************************************************
 * UI_setRow
 ******************************************************************************/
Void UI_setRow(UI_Handle hUI, UI_Value type, UI_Row row)
{
    stringAttrs[type].y = row;
}

/******************************************************************************
 * UI_update
 ******************************************************************************/
Void UI_update(UI_Handle hUI)
{
    Int i;

    for (i = 0; i < UI_Num_Values; i++) {
        if (stringAttrs[i].y >= 0 && stringAttrs[i].valid) {
            if (hUI->osd) {
                drawValue(hUI, i);
            }
            else {
                printf("%s %s ", stringAttrs[i].col1String,
                       stringAttrs[i].col2String);
            }
        }
    }

    if (!hUI->osd) {
        printf("\n\n");
    }
}

/******************************************************************************
 * UI_delete
 ******************************************************************************/
Int UI_delete(UI_Handle hUI)
{
    Int ret = SUCCESS;

    if (hUI) {
        ret = cleanupUI(hUI);
    }

    return ret; 
}
