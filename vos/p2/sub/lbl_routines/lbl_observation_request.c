/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_observation_request.h"

/******************************************************************************
 *				_LBL_OBSERVATION_REQUEST
 *
 *	This module contains routines to help create, read/write and print
 *  a ObservationRequest property label.  It is part of the MIPL label API
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
 *  LblObservationRequest.  This routine requires exactly 4 parameters.
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
 *****************************************************************************
 * History
 *========
 * Date		Who		Description
 * ============ =============== =============================================
 * 2015-09-24   C. Cheng        Added SHUTTER_CORRECTION_MODE
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblObsRequest_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, SourceId.Value),
		LBL_OFFSET(LblObsRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"COMMAND_INSTRUMENT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, CommandInstrumentId.Value),
		LBL_OFFSET(LblObsRequest_typ, CommandInstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandInstrumentId.Value)},

	{"AUTO_EXPOSURE_DATA_CUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, AutoExposureDataCut.Value),
                LBL_OFFSET(LblObsRequest_typ, AutoExposureDataCut.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposureDataCut.Value)},

	{"AUTO_EXPOSURE_PERCENT",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, AutoExposurePercent.Value),
                LBL_OFFSET(LblObsRequest_typ, AutoExposurePercent.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposurePercent.Value)},

	{"AUTO_EXPOSURE_PIXEL_FRACTION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, AutoExposurePixelFraction.Value),
                LBL_OFFSET(LblObsRequest_typ, AutoExposurePixelFraction.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposurePixelFraction.Value)},

	{"BAD_PIXEL_REPLACEMENT_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, BadPixelReplacementFlag.Value),
		LBL_OFFSET(LblObsRequest_typ, BadPixelReplacementFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementFlag.Value)},

	{"DETECTOR_ERASE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, DetectorEraseCount.Value),
                LBL_OFFSET(LblObsRequest_typ, DetectorEraseCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DetectorEraseCount.Value)},

	{"EARLY_PIXEL_SCALE_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, EarlyImageReturnFlag.Value),
		LBL_OFFSET(LblObsRequest_typ, EarlyImageReturnFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarlyImageReturnFlag.Value)},

	{"EARLY_IMAGE_RETURN_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, EarlyPixelScaleFlag.Value),
		LBL_OFFSET(LblObsRequest_typ, EarlyPixelScaleFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarlyPixelScaleFlag.Value)},

	{"EXPOSURE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ExposureType.Value),
		LBL_OFFSET(LblObsRequest_typ, ExposureType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureType.Value)},

	{"EXPOSURE_SCALE_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ExposureScaleFactor.Value),
                LBL_OFFSET(LblObsRequest_typ, ExposureScaleFactor.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ExposureScaleFactor.Value)},

	{"EXPOSURE_DURATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ExposureDurationCount.Value),
                LBL_OFFSET(LblObsRequest_typ, ExposureDurationCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ExposureDurationCount.Value)},

	{"EXPOSURE_TABLE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ExposureTableId.Value),
		LBL_OFFSET(LblObsRequest_typ, ExposureTableId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureTableId.Value)},

	{"EXPOSURE_TBL_UPDATE_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ExposureTblUpdateFlag.Value),
		LBL_OFFSET(LblObsRequest_typ, ExposureTblUpdateFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureTblUpdateFlag.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(LblObsRequest_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, FilterName.Value),
		LBL_OFFSET(LblObsRequest_typ, FilterName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName.Value)},

	{"FILTER_NUMBER",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, FilterNumber.Value),
                LBL_OFFSET(LblObsRequest_typ, FilterNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FilterNumber.Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[0].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[0].Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[1].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[1].Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[2].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinate[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[2].Value)},

    {"INSTRUMENT_COORDINATE__UNIT",     "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[0].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[0].Value)},

    {"INSTRUMENT_COORDINATE__UNIT",     "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[1].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[1].Value)},

    {"INSTRUMENT_COORDINATE__UNIT",     "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[2].Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateUnit[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[2].Value)},
    
	{"INSTRUMENT_COORDINATE_NAME",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[0].Value),
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateName[0].Value)},

	{"INSTRUMENT_COORDINATE_NAME",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[1].Value),
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateName[1].Value)},

	{"INSTRUMENT_COORDINATE_NAME",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[2].Value),
        LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateName[2].Value)},

	{"INSTRUMENT_COORD_FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordFrameId.Value),
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordFrameId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoordFrameId.Value)},

	{"INSTRUMENT_COORD_FRAME_INDEX",				"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordFrameIndex.Value),
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordFrameIndex.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoordFrameIndex.Value)},

	{"INSTRUMENT_COORDINATE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateId.Value),
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoordinateId.Value)},

	{"INSTRUMENT_BORESIGHT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentBoresiteId.Value),
		LBL_OFFSET(LblObsRequest_typ, InstrumentBoresiteId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentBoresiteId.Value)},

	{"INSTRUMENT_IDLE_TIMEOUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentIdleTimeout.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentIdleTimeout.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentIdleTimeout.Value)},

	{"INSTRUMENT_IDLE_TIMEOUT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentIdleTimeoutUnit.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentIdleTimeoutUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentIdleTimeoutUnit.Value)},

	{"MAX_AUTO_EXPOS_ITERATION_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, MaxAutoExposIterationCount.Value),
                LBL_OFFSET(LblObsRequest_typ, MaxAutoExposIterationCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(MaxAutoExposIterationCount.Value)},

	{"SHUTTER_CORRECTION_MODE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ShutterCorrectionMode.Value),
		LBL_OFFSET(LblObsRequest_typ, ShutterCorrectionMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterCorrectionMode.Value)},

	{"SHUTTER_CORRECTION_MODE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ShutterCorrectionModeId.Value),
		LBL_OFFSET(LblObsRequest_typ, ShutterCorrectionModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterCorrectionModeId.Value)},

	{"SHUTTER_CORRECT_THRESH_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, ShutterCorrectThreshCount.Value),
                LBL_OFFSET(LblObsRequest_typ, ShutterCorrectThreshCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ShutterCorrectThreshCount.Value)},

	{"INSTRUMENT_COORDINATE_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateType.Value),
		LBL_OFFSET(LblObsRequest_typ, InstrumentCoordinateType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoordinateType.Value)},

    {"SOURCE_ID",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, SourceId.Value),
        LBL_OFFSET(LblObsRequest_typ, SourceId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SourceId.Value)},

    {"DARK_SPECTRA_MODE",        "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, DarkSpectraMode.Value),
        LBL_OFFSET(LblObsRequest_typ, DarkSpectraMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DarkSpectraMode.Value)},

    {"GROUP_APPLICABILITY_FLAG",        "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, GroupApplicabilityFlag.Value),
        LBL_OFFSET(LblObsRequest_typ, GroupApplicabilityFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GroupApplicabilityFlag.Value)},

    {"GAIN_NUMBER",     "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, GainNumber.Value),
                LBL_OFFSET(LblObsRequest_typ, GainNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(GainNumber.Value)},

    {"INSTRUMENT_FOCUS_INIT_FLAG",      "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentFocusInitFlag.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentFocusInitFlag.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusInitFlag.Value)},

    {"INSTRUMENT_FOCUS_MODE",       "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentFocusMode.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentFocusMode.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusMode.Value)},

    {"INSTRUMENT_FOCUS_DISTANCE",       "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentFocusDistance.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentFocusDistance.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusDistance.Value)},

    {"INSTRUMENT_FOCUS_DISTANCE__UNIT", "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentFocusDistanceUnit.Value),
        LBL_OFFSET(LblObsRequest_typ, InstrumentFocusDistanceUnit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusDistanceUnit.Value)},

    {"INSTRUMENT_MODE_ID",       "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, InstrumentModeId.Value),
                LBL_OFFSET(LblObsRequest_typ, InstrumentModeId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentModeId.Value)},

    {"LASER_MODE",       "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, LaserMode.Value),
                LBL_OFFSET(LblObsRequest_typ, LaserMode.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LaserMode.Value)},

    {"OFFSET_NUMBER",       "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, OffsetNumber.Value),
                LBL_OFFSET(LblObsRequest_typ, OffsetNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(OffsetNumber.Value)},

    {"SOH_PRIORITY",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, SohPriority.Value),
                LBL_OFFSET(LblObsRequest_typ, SohPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SohPriority.Value)},
    {"START_IMAGE_ID",      "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, StartImageId.Value),
                LBL_OFFSET(LblObsRequest_typ, StartImageId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(StartImageId.Value)},

    {"VALID_MINIMUM_PIXEL",     "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, ValidMinimum.Value),
                LBL_OFFSET(LblObsRequest_typ, ValidMinimum.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ValidMinimum.Value)},

    {"VALID_MAXIMUM_PIXEL",     "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblObsRequest_typ, ValidMaximum.Value),
                LBL_OFFSET(LblObsRequest_typ, ValidMaximum.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ValidMaximum.Value)},


	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"OBSERVATION_REQUEST_PARMS",	LBL_NULL };

/******************************************************************************
 *				_LBL_OBSERVATION_REQUEST
 *
 *****************************************************************************/
int     LblObservationRequest(
  int   Unit,
  int   Obtain,
  LblObsRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblObsRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_OBSERVATION_REQUEST
 *
 *****************************************************************************/
void	LblPrintObservationRequest(
  LblObsRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_OBSERVATION_REQUEST
 *
 *****************************************************************************/
void	LblTestObservationRequest(
  LblObsRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
