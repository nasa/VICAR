/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_derived_image.h"

/******************************************************************************
 *				LBL_DERIVED_IMAGE
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Image property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_data.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblDerivedImage.  This routine requires exactly 4 parameters.
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
 * 27-Mar-2018  Steven Lu       Added EncodedDisplayGamma
 * 02-Feb-2018  Steven Lu       Added ColorSpace, ColorComponent, and Illuminant
 * 21-Nov-2017	Bob Deen	Added TARGET_INSTRUMENT
 * 09-Nov-2017  Steven Lu       Added STEREO_BASELINE, STEREO_PRODUCT_ID,
 *                              LINEARIZATION_PRODUCT_ID, and LINEARIZATION_MODE
 * 08-Jun-2012  Oleg Pariser    Added ERROR_MODEL definition keywords
 * 10-Jun-2011  Oleg Pariser    Added POINTING_CORRECTION_FILE_NAME and
                                SOLUTION_ID  definition keywords
 * 15-Feb-2010  Oleg Pariser    Added INPUT_PRODUCT_ID definition keywords
 * 15-Nov-2010  Oleg Pariser    Added Pointing Model definition keywords
 * 2003-11-12   H. Lee          Added RADIANCE_OFFSET__UNIT
 * 2003-11-10   H. Lee          Added RADIANCE_SCALING_FACTOR__UNIT
 * 2003-06-17   H. Lee          Added CONFIGURATION_BAND_ID & INSTRUMENT_BAND_ID
 * 2003-05-20   H. Lee          Changed RADIANCE_SCALE_FACTOR to
 *                              RADIANCE_SCALING_FACTOR
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblDerivedImage_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"BRIGHTNESS_CORRECTION_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, BrightnessCorrectionType.Value),
		LBL_OFFSET(LblDerivedImage_typ, BrightnessCorrectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BrightnessCorrectionType.Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[0].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[1].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[2].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[3].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[4].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[5].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[6].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[7].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[8].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[9].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     11,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[10].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[10].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[10].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     12,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[11].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[11].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[11].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     13,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[12].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[12].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[12].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     14,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[13].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[13].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[13].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     15,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[14].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[14].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[14].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     16,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[15].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[15].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[15].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[0].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[1].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[2].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[3].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[4].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[5].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[6].Value)},

	{"CONFIGURATION_BIT_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBitId[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBitId[7].Value)},

	{"DERIVED_IMAGE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, DerivedImageType.Value),
		LBL_OFFSET(LblDerivedImage_typ, DerivedImageType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedImageType.Value)},

   	{"INPUT_PRODUCT_ID",                    "STRING",   	LBL_OPTIONAL,
        	LBL_NO_CONT,    1,  1,  LBL_NULL,
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[0].Value),
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[0].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(InputProductId[0].Value)},

    	{"INPUT_PRODUCT_ID",              	"STRING",   	LBL_OPTIONAL,
        	LBL_NO_CONT,    1,  2,  LBL_NULL,
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[1].Value),
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[1].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(InputProductId[1].Value)},

    	{"INPUT_PRODUCT_ID",              	"STRING",   	LBL_OPTIONAL,
        	LBL_NO_CONT,    1,  3,  LBL_NULL,
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[2].Value),
        	LBL_OFFSET(LblDerivedImage_typ, InputProductId[2].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(InputProductId[2].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[0].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[1].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[2].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[3].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[4].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[5].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[6].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[7].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[8].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[9].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     11,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[10].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[10].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[10].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     12,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[11].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[11].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[11].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     13,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[12].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[12].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[12].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     14,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[13].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[13].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[13].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     15,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[14].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[14].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[14].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     16,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[15].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[15].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[15].Value)},   

	{"INVERSE_LUT_FILE_NAME",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,     1,       LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InverseLutFileName.Value),
                LBL_OFFSET(LblDerivedImage_typ, InverseLutFileName.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InverseLutFileName.Value)},   

        {"POINTING_MODEL_NAME",                 "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelName.Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelName.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelName.Value)},

        {"POINTING_CORRECTION_FILE_NAME",       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingCorrectionFileName.Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingCorrectionFileName.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingCorrectionFileName.Value)},

        {"SOLUTION_ID",                         "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, SolutionId.Value),
                LBL_OFFSET(LblDerivedImage_typ, SolutionId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SolutionId.Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[0].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[1].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[2].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[3].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[4].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[5].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[6].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[7].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[8].Value)},

        {"POINTING_MODEL_PARAMS",               "REAL",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParams[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParams[9].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[0].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[1].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[2].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[3].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[4].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[5].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[6].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[7].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[8].Value)},

        {"POINTING_MODEL_PARAMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,    1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, PointingModelParamsName[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(PointingModelParamsName[9].Value)},

        {"ERROR_MODEL_NAME",                 "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelName.Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelName.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelName.Value)},

        {"ERROR_MODEL_DESC__PTR",                     "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelDesc.Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelDesc.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelDesc.Value)},

        {"ERROR_MODEL_PARMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParms[0].Value)},

        {"ERROR_MODEL_PARMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParms[1].Value)},

        {"ERROR_MODEL_PARMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParms[2].Value)},

        {"ERROR_MODEL_PARMS",               "REAL",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParms[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParms[3].Value)},

        {"ERROR_MODEL_PARMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParmsName[0].Value)},

        {"ERROR_MODEL_PARMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParmsName[1].Value)},

        {"ERROR_MODEL_PARMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParmsName[2].Value)},

        {"ERROR_MODEL_PARMS_NAME",          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,     1,     4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, ErrorModelParmsName[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ErrorModelParmsName[3].Value)},

	{"RADIANCE_OFFSET",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadianceOffset.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadianceOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceOffset.Value)},

        {"RADIANCE_OFFSET__UNIT",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, RadianceOffsetUnit.Value),
                LBL_OFFSET(LblDerivedImage_typ, RadianceOffsetUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceOffsetUnit.Value)},

	{"RADIANCE_SCALING_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactor.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactor.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceScaleFactor.Value)},

        {"RADIANCE_SCALING_FACTOR__UNIT",       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactorUnit.Value),
                LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactorUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceScaleFactorUnit.Value)},

	{"RADIOMETRIC_CORRECTION_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadiometricCorrectionType.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadiometricCorrectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadiometricCorrectionType.Value)},

	{"RANGE_ORIGIN_VECTOR",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RangeOriginVector.Value),
		LBL_OFFSET(LblDerivedImage_typ, RangeOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RangeOriginVector.Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[6].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[7].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[8].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[9].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SOLAR_ENERGY_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, SolarEnergyElevation.Value),
		LBL_OFFSET(LblDerivedImage_typ, SolarEnergyElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarEnergyElevation.Value)},

        {"SOLAR_ENERGY_ELEVATION__UNIT",        "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, SolarEnergyElevationUnit.Value),
                LBL_OFFSET(LblDerivedImage_typ, SolarEnergyElevationUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SolarEnergyElevationUnit.Value)},

        {"MASK_DESC_FILE_NAME",		       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, MaskDescFileName[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, MaskDescFileName[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MaskDescFileName[0].Value)},

        {"MASK_DESC_FILE_NAME",		       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, MaskDescFileName[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, MaskDescFileName[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MaskDescFileName[1].Value)},

	{"HORIZON_MASK_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, HorizonMaskElevation.Value),
		LBL_OFFSET(LblDerivedImage_typ, HorizonMaskElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(HorizonMaskElevation.Value)},

        {"STEREO_BASELINE",                     "REAL",         LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, StereoBaseline.Value),
                LBL_OFFSET(LblDerivedImage_typ, StereoBaseline.Valid),
                LBL_NO_RETURN, LBL_SIZE(StereoBaseline.Value)},

        {"STEREO_PRODUCT_ID",                   "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, StereoProductId.Value),
                LBL_OFFSET(LblDerivedImage_typ, StereoProductId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(StereoProductId.Value)},

        {"LINEARIZATION_MODE",                  "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,  1,  LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, LinearizationMode[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, LinearizationMode[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(LinearizationMode[0].Value)},

        {"LINEARIZATION_MODE",                  "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,  2,  LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, LinearizationMode[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, LinearizationMode[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(LinearizationMode[1].Value)},

        {"LINEARIZATION_PRODUCT_ID",            "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, LinearizationProductId.Value),
                LBL_OFFSET(LblDerivedImage_typ, LinearizationProductId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LinearizationProductId.Value)},

        {"TARGET_INSTRUMENT",			"STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, TargetInstrument.Value),
                LBL_OFFSET(LblDerivedImage_typ, TargetInstrument.Valid),
                LBL_NO_RETURN,  LBL_SIZE(TargetInstrument.Value)},

        {"COLOR_SPACE",                         "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ColorSpace.Value),
                LBL_OFFSET(LblDerivedImage_typ, ColorSpace.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ColorSpace.Value)},

        {"COLOR_COMPONENT",                     "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ColorComponent.Value),
                LBL_OFFSET(LblDerivedImage_typ, ColorComponent.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ColorComponent.Value)},

        {"ILLUMINANT",                          "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, Illuminant.Value),
                LBL_OFFSET(LblDerivedImage_typ, Illuminant.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Illuminant.Value)},

        {"ENCODED_DISPLAY_GAMMA",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, EncodedDisplayGamma.Value),
                LBL_OFFSET(LblDerivedImage_typ, EncodedDisplayGamma.Valid),
                LBL_NO_RETURN,  LBL_SIZE(EncodedDisplayGamma.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblSetDerivedImage(
  const char 	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE
 *
 *****************************************************************************/
int     LblDerivedImage(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{
  LblSetDerivedImage("DERIVED_IMAGE");
  return (LblDerivedImageApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE_PARMS
 *
 *****************************************************************************/
int     LblDerivedImageParms(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{
  LblSetDerivedImage("DERIVED_IMAGE_PARMS");
  return (LblDerivedImageApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE_API
 *
 *****************************************************************************/
int     LblDerivedImageApi(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblDerivedImage_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblPrintDerivedImage(
  LblDerivedImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblTestDerivedImage(
  LblDerivedImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
