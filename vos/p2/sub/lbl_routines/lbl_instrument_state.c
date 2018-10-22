/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_instrument_state.h"

/******************************************************************************
 *				LBL_INSTRUMENT_STATE
 *
 *	This module contains routines to help create, read/write and print an
 *  Instrument State property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_instrument_state.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblInstrumentState.  This routine requires exactly 4 parameters.
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
 * Date         Who             Description
 * ============ =============== =============================================
 * 2017-11-21	B. Deen		Added CFA_TYPE and _VENUE, BAYER_METHOD
 * 2015-09-24   C. Cheng        Added SHUTTER_CORRECTION_MODE
 * 2003-05-12   H. Lee          Changed SUN_FIND_PARM to REAL
 * 2003-04-18   H. Lee          Changed DETECTOR_TO_IMAGE_ROTATION to REAL
 * 2003-01-13   P. Zamani       Changed BadPixelReplaceFlag to
 *                                BadPixelReplacementId
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblInstrumentState_typ*)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"AZIMUTH_FOV",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFov.Value),
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFov.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AzimuthFov.Value)},

	{"AZIMUTH_FOV__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFovUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFovUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AzimuthFovUnit.Value)},

	{"ELEVATION_FOV",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ElevationFov.Value),
		LBL_OFFSET(LblInstrumentState_typ, ElevationFov.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ElevationFov.Value)},

	{"ELEVATION_FOV__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ElevationFovUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, ElevationFovUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ElevationFovUnit.Value)},

	{"BAD_PIXEL_REPLACEMENT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, BadPixelReplacementId.Value),
		LBL_OFFSET(LblInstrumentState_typ, BadPixelReplacementId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementId.Value)},

	{"BAYER_METHOD",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, BayerMethod.Value),
		LBL_OFFSET(LblInstrumentState_typ, BayerMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BayerMethod.Value)},

	{"BAYER_MODE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, BayerMode.Value),
		LBL_OFFSET(LblInstrumentState_typ, BayerMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BayerMode.Value)},

	{"CFA_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, CfaType.Value),
		LBL_OFFSET(LblInstrumentState_typ, CfaType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CfaType.Value)},

	{"CFA_VENUE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, CfaVenue.Value),
		LBL_OFFSET(LblInstrumentState_typ, CfaVenue.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CfaVenue.Value)},

	{"DETECTOR_FIRST_LINE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorFirstLine.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorFirstLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorFirstLine.Value)},

	{"DETECTOR_LINES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorLines.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorLines.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorLines.Value)},

	{"DETECTOR_TO_IMAGE_ROTATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorToImageRotation.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorToImageRotation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorToImageRotation.Value)},

	{"DOWNSAMPLE_METHOD",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DownsampleMethod.Value),
		LBL_OFFSET(LblInstrumentState_typ, DownsampleMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DownsampleMethod.Value)},

	{"EXPOSURE_COUNT",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureCount.Value)},

	{"EXPOSURE_DURATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDuration.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDuration.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDuration.Value)},

	{"EXPOSURE_DURATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDurationUnit.Value)},

	{"EXPOSURE_DURATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDurationCount.Value)},

	{"EXPOSURE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureType.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureType.Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[0].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[1].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[2].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[3].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[4].Value)},

	{"FILTER_NUMBER",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterNumber.Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterNumber.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[0].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[1].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[2].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[3].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[4].Value)},

	{"GAIN_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, GainModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, GainModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GainModeId.Value)},

    {"IMAGE_BIAS",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, ImageBias.Value),
        LBL_OFFSET(LblInstrumentState_typ, ImageBias.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageBias.Value)},

	{"INST_AZ_ROTATION_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstAzRotationDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstAzRotationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstAzRotationDirection.Value)},

	{"INST_EL_ROTATION_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstElRotationDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstElRotationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstElRotationDirection.Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[0].Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[1].Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[2].Value)},

	{"INSTRUMENT_AZIMUTH_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentAzimuthCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentAzimuthCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentAzimuthCount.Value)},

	{"INSTRUMENT_COVER_STATE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentCoverStateId.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentCoverStateId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoverStateId.Value)},

	{"INSTRUMENT_DATA_RATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDataRate.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDataRate.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentDataRate.Value)},

	{"INSTRUMENT_DEPLOYMENT_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDeploymentState.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDeploymentState.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentDeploymentState.Value)},

	{"INSTRUMENT_ELEVATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentElevationCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentElevationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentElevationCount.Value)},

	{"INSTRUMENT_FOCAL_LENGTH_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentFocalLengthCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentFocalLengthCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentFocalLengthCount.Value)},

    {"INSTRUMENT_FOCUS_POSITION",        "INT",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, InstrumentFocusPosition.Value),
        LBL_OFFSET(LblInstrumentState_typ, InstrumentFocusPosition.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusPosition.Value)},

	{"INSTRUMENT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentModeId.Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[0].Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[1].Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[2].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[0].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[1].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[2].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[3].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[4].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[5].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[6].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[7].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[8].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[9].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[10].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[11].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[12].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[13].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[14].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[15].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[16].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[17].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[18].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[19].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[20].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[20].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[21].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[21].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[22].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[22].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[23].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[23].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[0].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[1].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[2].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[3].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[4].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[5].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[6].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[7].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[8].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[9].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[10].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[11].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[12].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[13].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[14].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[15].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[16].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[17].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[18].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[19].Value)},

    {"INSTRUMENT_TEMPERATURE__UNIT",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  21, LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[20].Value),
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[20].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentTemperatureUnit[20].Value)},

    {"INSTRUMENT_TEMPERATURE__UNIT",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  22, LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[21].Value),
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[21].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentTemperatureUnit[21].Value)},

    {"INSTRUMENT_TEMPERATURE__UNIT",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  23, LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[22].Value),
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[22].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentTemperatureUnit[22].Value)},

    {"INSTRUMENT_TEMPERATURE__UNIT",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  24, LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[23].Value),
        LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[23].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentTemperatureUnit[23].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[0].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[1].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[2].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[3].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[4].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[5].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[6].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[7].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[8].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[9].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[10].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[11].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[12].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[13].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[14].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[15].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[16].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[17].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[18].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[19].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[0].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[1].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[2].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[3].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[4].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[5].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[6].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[7].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[8].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[9].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[10].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[11].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[12].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[13].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[14].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[15].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[16].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[17].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[18].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[19].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[0].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[1].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[2].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[3].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[4].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[5].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[6].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[7].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[8].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[9].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[10].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[11].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[12].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[13].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[14].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[15].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[16].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[17].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[18].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[19].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[20].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[20].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[21].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[21].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[22].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[22].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[23].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[23].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[0].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[1].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[2].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[3].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[4].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[5].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[6].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[7].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[8].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[9].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[10].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[11].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[12].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[13].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[14].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[15].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[16].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[17].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[18].Value)},

	{"INSTRUMENT_TEMPERATURE_STATUS",	"INT",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureStatus[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureStatus[19].Value)},

	{"INSTRUMENT_VOLTAGE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentVoltageCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentVoltageCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentVoltageCount.Value)},

	{"LED_BITMASK",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, LedBitmask.Value),
		LBL_OFFSET(LblInstrumentState_typ, LedBitmask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LedBitmask.Value)},

	{"OFFSET_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OffsetModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, OffsetModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OffsetModeId.Value)},

	{"OFFSET_NUMBER",			"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OffsetNumber.Value),
		LBL_OFFSET(LblInstrumentState_typ, OffsetNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OffsetNumber.Value)},

	{"ONBOARD_IMAGE_BIAS",			"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OnboardImageBias.Value),
		LBL_OFFSET(LblInstrumentState_typ, OnboardImageBias.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OnboardImageBias.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingHeight.Value),
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingHeight.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingWidth.Value),
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingWidth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingWidth.Value)},

	{"SAMPLE_BIT_METHOD",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SampleBitMethod.Value),
		LBL_OFFSET(LblInstrumentState_typ, SampleBitMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitMethod.Value)},

	{"SAMPLE_BIT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SampleBitModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, SampleBitModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitModeId.Value)},

	{"SHUTTER_CORRECTION_MODE",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ShutterCorrectionMode.Value),
		LBL_OFFSET(LblInstrumentState_typ, ShutterCorrectionMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterCorrectionMode.Value)},

	{"SHUTTER_EFFECT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ShutterEffectCorrectionFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, ShutterEffectCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterEffectCorrectionFlag.Value)},

	{"SHUTTER_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ShutterModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, ShutterModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterModeId.Value)},

	{"SUN_FIND_FLAG",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindFlag.Value)},

    {"SUN_FIND",           "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, SunFind.Value),
        LBL_OFFSET(LblInstrumentState_typ, SunFind.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFind.Value)},

    {"SUN_FIND_ACTIVE_FLAG",           "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, SunFindActiveFlag.Value),
        LBL_OFFSET(LblInstrumentState_typ, SunFindActiveFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFindActiveFlag.Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[0].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[1].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[2].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[3].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[4].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[0].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[1].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[2].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[3].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[4].Value)},

	{"SUN_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunLine.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunLine.Value)},

	{"SUN_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunLineSample.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunLineSample.Value)},

	{"SUN_VIEW_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunViewPosition.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunViewPosition.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunViewPosition.Value)},

	{"SUN_VIEW_DIRECTION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunViewDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunViewDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunViewDirection.Value)},

    {"ONBOARD_RESPONSIVITY",      "REAL",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[0].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardResponsivity[0].Value)},

    {"ONBOARD_RESPONSIVITY",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[1].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardResponsivity[1].Value)},

    {"ONBOARD_RESPONSIVITY",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[2].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardResponsivity[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardResponsivity[2].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[0].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[0].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[1].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[1].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[2].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[2].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[3].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[3].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[4].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[4].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[5].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[5].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[6].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[6].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[7].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[7].Value)},

    {"ONBOARD_COLOR_MATRIX",      "REAL",      LBL_OPTIONAL,
        LBL_CONTINUE,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[8].Value),
        LBL_OFFSET(LblInstrumentState_typ, OnboardColorMatrix[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(OnboardColorMatrix[8].Value)},


	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_INSTRUMENT_STATE
 *
 *****************************************************************************/
void     LblSetInstrumentState(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE
 *
 *****************************************************************************/
int     LblInstrumentState(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{
  LblSetInstrumentState("INSTRUMENT_STATE");
  return (LblInstrumentStateApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE_PARMS
 *
 *****************************************************************************/
int     LblInstrumentStateParms(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{
  LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
  return (LblInstrumentStateApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE_API
 *
 *****************************************************************************/
int     LblInstrumentStateApi(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblInstrumentState_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	LblPrintInstrumentState(
  LblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	LblTestInstrumentState(
  LblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
