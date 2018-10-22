/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_mini_header.h"

/******************************************************************************
 *				_LBL_MINI_HEADER
 *
 *	This module contains routines to help create, read/write and print
 *  a MINI_HEADE Rroperty label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblMiniHeader.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblMiniHeader_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {

    {"IMAGE_ID",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ImageId.Value),
        LBL_OFFSET(LblMiniHeader_typ, ImageId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageId.Value)},

    {"MAGIC_NUMBERS",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, MagicNumbers[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, MagicNumbers[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(MagicNumbers[0].Value)},

    {"MAGIC_NUMBERS",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, MagicNumbers[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, MagicNumbers[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(MagicNumbers[1].Value)},

	{"SPACECRAFT_CLOCK_START_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblMiniHeader_typ, SpacecraftClockStartCount.Value),
        LBL_OFFSET(LblMiniHeader_typ, SpacecraftClockStartCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SpacecraftClockStartCount.Value)},

	{"DETECTOR_ERASE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblMiniHeader_typ, DetectorEraseCount.Value),
        LBL_OFFSET(LblMiniHeader_typ, DetectorEraseCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DetectorEraseCount.Value)},

    {"INSTRUMENT_MODE_ID",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeId.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeId.Value)},

    {"FILTER_NUMBER",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, FilterNumber.Value),
        LBL_OFFSET(LblMiniHeader_typ, FilterNumber.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FilterNumber.Value)},

    {"EXPOSURE_DURATION_COUNT",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ExposureDurationCount.Value),
        LBL_OFFSET(LblMiniHeader_typ, ExposureDurationCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureDurationCount.Value)},

    {"FIRST_LINE_SAMPLE",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, FirstLineSample.Value),
        LBL_OFFSET(LblMiniHeader_typ, FirstLineSample.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FirstLineSample.Value)},

    {"FIRST_LINE",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, FirstLine.Value),
        LBL_OFFSET(LblMiniHeader_typ, FirstLine.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FirstLine.Value)},

    {"LINE_SAMPLES",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, LineSamples.Value),
        LBL_OFFSET(LblMiniHeader_typ, LineSamples.Valid),
        LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

    {"LINES",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, Lines.Value),
        LBL_OFFSET(LblMiniHeader_typ, Lines.Valid),
        LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

    {"INSTRUMENT_FOCUS_MODE",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusMode.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusMode.Value)},

    {"INSTRUMENT_FOCUS_POSITION",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusPosition.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusPosition.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusPosition.Value)},

    {"INSTRUMENT_FOCUS_STEP_SIZE",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusStepSize.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusStepSize.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusStepSize.Value)},

    {"INSTRUMENT_FOCUS_STEPS",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusSteps.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentFocusSteps.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusSteps.Value)},

    {"EXPOSURE_TYPE",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ExposureType.Value),
        LBL_OFFSET(LblMiniHeader_typ, ExposureType.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureType.Value)},

    {"AUTO_EXPOSURE_DATA_CUT",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, AutoExposureDataCut.Value),
        LBL_OFFSET(LblMiniHeader_typ, AutoExposureDataCut.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureDataCut.Value)},

    {"AUTO_EXPOSURE_PIXEL_FRACTION",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, AutoExposurePixelFraction.Value),
        LBL_OFFSET(LblMiniHeader_typ, AutoExposurePixelFraction.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposurePixelFraction.Value)},

    {"AUTO_EXPOSURE_PERCENT",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, AutoExposurePercent.Value),
        LBL_OFFSET(LblMiniHeader_typ, AutoExposurePercent.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposurePercent.Value)},

    {"MAX_AUTO_EXPOS_ITERATION_COUNT",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, MaxAutoExposIterationCount.Value),
        LBL_OFFSET(LblMiniHeader_typ, MaxAutoExposIterationCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(MaxAutoExposIterationCount.Value)},

    {"INST_CMPRS_MODE",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstCmprsMode.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstCmprsMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstCmprsMode.Value)},

    {"INST_CMPRS_QUALITY",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstCmprsQuality.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstCmprsQuality.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstCmprsQuality.Value)},

    {"SAMPLE_BIT_MODE_ID",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, SampleBitModeId.Value),
        LBL_OFFSET(LblMiniHeader_typ, SampleBitModeId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SampleBitModeId.Value)},

    {"START_IMAGE_ID",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, StartImageId.Value),
        LBL_OFFSET(LblMiniHeader_typ, StartImageId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(StartImageId.Value)},

    {"EXPOSURE_COUNT",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ExposureCount.Value),
        LBL_OFFSET(LblMiniHeader_typ, ExposureCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureCount.Value)},

    {"IMAGE_BLENDING_FLAG",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ImageBlendingFlag.Value),
        LBL_OFFSET(LblMiniHeader_typ, ImageBlendingFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageBlendingFlag.Value)},

    {"IMAGE_REGISTRATION_FLAG",        "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ImageRegistrationFlag.Value),
        LBL_OFFSET(LblMiniHeader_typ, ImageRegistrationFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageRegistrationFlag.Value)},

    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[0].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[1].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[2].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[2].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[3].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[3].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[4].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[4].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  6,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[5].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[5].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[6].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[6].Value)},
        
    {"INSTRUMENT_STATE",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[7].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentState[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentState[7].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[0].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[1].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[2].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[2].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[3].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[3].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[4].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[4].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  6,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[5].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[5].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[6].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[6].Value)},
        
    {"INSTRUMENT_STATE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[7].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentStateName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentStateName[7].Value)},

    {"INSTRUMENT_SERIAL_NUMBER",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentSerialNumber.Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentSerialNumber.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentSerialNumber.Value)},

    {"ARTICULATION_DEV_POSITION",                "INT",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPosition[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPosition[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDevPosition[0].Value)},

    {"ARTICULATION_DEV_POSITION",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPosition[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPosition[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDevPosition[1].Value)},

    {"ARTICULATION_DEV_POSITION_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPositionName[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPositionName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDevPositionName[0].Value)},

    {"ARTICULATION_DEV_POSITION_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPositionName[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, ArticulationDevPositionName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDevPositionName[1].Value)},

    {"OFFSET_MODE_ID",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, OffsetModeId.Value),
        LBL_OFFSET(LblMiniHeader_typ, OffsetModeId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OffsetModeId.Value)},

    {"INITIAL_SIZE",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InitialSize.Value),
        LBL_OFFSET(LblMiniHeader_typ, InitialSize.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InitialSize.Value)},

    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[0].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[1].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[2].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[2].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[3].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[3].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[4].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[4].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  6,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[5].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[5].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[6].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[6].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[7].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[7].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[8].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[8].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[9].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[9].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  11,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[10].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[10].Value)},
        
    {"INSTRUMENT_MODE",                "INT",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  12,  LBL_NULL, 
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[11].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentMode[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentMode[11].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[0].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[0].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[1].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[1].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[2].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[2].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[3].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[3].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[4].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[4].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[5].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[5].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[6].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[6].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[7].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[7].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[8].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[8].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[9].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[9].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[10].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[10].Value)},
        
    {"INSTRUMENT_MODE_NAME",                "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[11].Value),
        LBL_OFFSET(LblMiniHeader_typ, InstrumentModeName[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentModeName[11].Value)},
        
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/*static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"MINI_HEADER_PARMS",	LBL_NULL };
*/
static LblApiProcess_typ    Label = {
    LabelTbl,   "PROPERTY", "PROPERTY", "MINI_HEADER",
    LBL_NULL };

/******************************************************************************
 *				_LBL_MINI_HEADER
 *
 *****************************************************************************/
/*int     LblMiniHeader(
  int   Unit,
  int   Obtain,
  LblMiniHeader_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblMiniHeader_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}*/
int     LblMiniHeader(
  int   Unit,
  int   Obtain,
  LblMiniHeader_typ      *LabelItems,
  int   Instance,
  const char* propertyName)
{ int   RtnStatus;

 if (propertyName!=NULL)
   Label.NameValue = propertyName;

  LblApiCntrl_typ   Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblMiniHeader_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}
/******************************************************************************
 *				LBL_PRINT_MINI_HEADER
 *
 *****************************************************************************/
void	LblPrintMiniHeader(
  LblMiniHeader_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}
/******************************************************************************
 *				LBL_TEST_MINI_HEADER
 *
 *****************************************************************************/
void	LblTestMiniHeader(
  LblMiniHeader_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
