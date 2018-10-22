#ifndef MIPS_LBL_INSTRUMENT_STATE_INCLUDED
#define MIPS_LBL_INSTRUMENT_STATE_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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
 * Date		Who		Description
 * ============ =============== =============================================
 * 2018-01-16   G. Hollins	Added OnboardResponsivity and OnboardColorMatrix
 * 2017-10-21   B. Deeen	Added CfaType and Venue, BayerMethod
 * 2015-09-24   C. Cheng        Added SHUTTER_CORRECTION_MODE
 * 2003-05-12   H. Lee          Changed SunFindParm to LblApiRealItem_typ
 * 2003-04-18   H. Lee          Changed DetectorToImageRotation to LblApiRealItem_typ
 * 2003-01-13	P. Zamani	Changed BadPixelReplaceFlag to
 *				  BadPixelReplacementId and it's type
 * 2003-01-10	P. Zamani	Changed FilterNumber to LblApiIntItem
 *****************************************************************************/

#define  LBL_INST_STATE_TEMPS		24
#define  LBL_INST_STATE_PARMS		5

typedef struct
	{
	LblApiRealItem_typ		AzimuthFov;
	LblApiTypeItem_typ		AzimuthFovUnit;
	LblApiRealItem_typ		ElevationFov;
	LblApiTypeItem_typ		ElevationFovUnit;
	LblApiIdItem_typ		BadPixelReplacementId;
	LblApiIntItem_typ		DetectorFirstLine;
	LblApiIntItem_typ		DetectorLines;
	LblApiRealItem_typ		DetectorToImageRotation;
	LblApiNameItem_typ		DownsampleMethod;
	LblApiIntItem_typ		ExposureCount;
	LblApiRealItem_typ		ExposureDuration;
	LblApiIntItem_typ		ExposureDurationCount;
	LblApiTypeItem_typ		ExposureDurationUnit;
	LblApiTypeItem_typ		ExposureType;
	LblApiNameItem_typ		FilterName[LBL_MAX_ITEMS];
	LblApiIntItem_typ		FilterNumber;
	LblApiFlagItem_typ		FlatFieldCorrectionFlag;
	LblApiRealItem_typ		FlatFieldCorrectionParm[LBL_INST_STATE_PARMS];
	LblApiRealItem_typ		FrameDuration;
	LblApiIdItem_typ		GainModeId;
	LblApiIntItem_typ		ImageBias;
	LblApiIdItem_typ		InstAzRotationDirection;
	LblApiIdItem_typ		InstElRotationDirection;
	LblApiRealItem_typ		InstHostPosition[LBL_POSITION_ARRAY];
	LblApiIntItem_typ		InstrumentAzimuthCount;
	LblApiIdItem_typ		InstrumentCoverStateId;
	LblApiIdItem_typ		InstrumentDataRate;
	LblApiIdItem_typ		InstrumentDeploymentState;
	LblApiIntItem_typ		InstrumentElevationCount;
	LblApiIntItem_typ		InstrumentFocalLengthCount;
	LblApiIdItem_typ		InstrumentModeId;
	LblApiRealItem_typ		InstrumentPosition[LBL_POSITION_ARRAY];
	LblApiRealItem_typ		InstrumentTemperature[LBL_INST_STATE_TEMPS];
	LblApiIntItem_typ		InstrumentTemperatureCount[LBL_INST_STATE_TEMPS];
	LblApiNameItem_typ		InstrumentTemperatureName[LBL_INST_STATE_TEMPS];
	LblApiTypeItem_typ		InstrumentTemperatureUnit[LBL_INST_STATE_TEMPS];
	LblApiTypeItem_typ		InstrumentTemperatureCountUnit[LBL_INST_STATE_TEMPS];
	LblApiIntItem_typ		InstrumentVoltageCount;
	LblApiNameItem_typ		LedBitmask;
	LblApiIdItem_typ		OffsetModeId;
	LblApiIntItem_typ		OffsetNumber;
	LblApiIntItem_typ		OnboardImageBias;
	LblApiIntItem_typ		PixelAveragingHeight;
	LblApiIntItem_typ		PixelAveragingWidth;
	LblApiNameItem_typ		SampleBitMethod;
	LblApiIdItem_typ		SampleBitModeId;
	LblApiStringItem_typ	ShutterCorrectionMode;
	LblApiFlagItem_typ		ShutterEffectCorrectionFlag;
	LblApiIdItem_typ		ShutterModeId;
	LblApiFlagItem_typ		SunFindFlag;
	LblApiFlagItem_typ		SunFind;
	LblApiFlagItem_typ		SunFindActiveFlag;
	LblApiRealItem_typ		SunFindParm[LBL_INST_STATE_PARMS];
	LblApiNameItem_typ		SunFindParmName[LBL_INST_STATE_PARMS];
	LblApiIntItem_typ		SunLine;
	LblApiIntItem_typ		SunLineSample;
	LblApiRealVectorItem_typ	SunViewPosition;
	LblApiRealVectorItem_typ	SunViewDirection;
	LblApiIntItem_typ		InstrumentTemperatureStatus[LBL_INST_STATE_TEMPS];
	LblApiIntItem_typ		InstrumentFocusPosition;
	LblApiNameItem_typ		BayerMode;	// deprecated...
	LblApiNameItem_typ		CfaType;
	LblApiNameItem_typ		CfaVenue;
	LblApiNameItem_typ		BayerMethod;
	LblApiRealItem_typ		OnboardResponsivity[3];
	LblApiRealItem_typ		OnboardColorMatrix[9];
	} LblInstrumentState_typ;

/***  Function prototypes  ***/
int	LblInstrumentState( int, int, LblInstrumentState_typ *, int );
int	LblInstrumentStateParms( int, int, LblInstrumentState_typ *, int );
	/***  For development & internal use  ***/
int	LblInstrumentStateApi( int, int, LblInstrumentState_typ *, int );
void	LblSetInstrumentState( const char * );
void	LblTestInstrumentState( LblInstrumentState_typ *);
void	LblPrintInstrumentState( LblInstrumentState_typ *);

#ifdef __cplusplus
}
#endif

#endif

