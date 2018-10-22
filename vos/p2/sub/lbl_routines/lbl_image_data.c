/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_image_data.h"

/******************************************************************************
 *				LBL_IMAGE_DATA
 *
 *	This module contains routines to help create, read/write and print
 *  an Image Data property label.  It is part of the MIPL label API package,
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
 *  LblImageData.  This routine requires exactly 4 parameters.
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
 * 2003-11-12   H. Lee          Added RADIANCE_OFFSET__UNIT & 
 *                              RADIANCE_SCALING_FACTOR__UNIT
 * 2003-07-08   H. Lee          Changed MAXIMUM & MINIMUM to REAL from STRING
 *                              Changed CHECKSUM to REAL from INT
 * 2003-07-01   H. Lee          Changed MISSING_CONSTANT as an array
 * 2003-05-20   H. Lee          Changed RADIANCE_SCALE_FACTOR to
 *                              RADIANCE_SCALING_FACTOR
 * 2003-03-03   H. Lee          Changed INVALID_CONSTANT & MISSING_CONSTANT
 *                              to REAL from INT
 * 2003-02-26   H. Lee          Changed INVALID_CONSTANT & MISSING_CONSTANT
 *                              to INT from STRING
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblImageData_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"BANDS",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Bands.Value),
		LBL_OFFSET(LblImageData_typ, Bands.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Bands.Value)},

	{"BAND_STORAGE_TYPE",		   "STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, BandStorageType.Value),
		LBL_OFFSET(LblImageData_typ, BandStorageType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BandStorageType.Value)},

	{"CHECKSUM",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Checksum.Value),
		LBL_OFFSET(LblImageData_typ, Checksum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Checksum.Value)},

	{"FIRST_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, FirstLine.Value),
		LBL_OFFSET(LblImageData_typ, FirstLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FirstLine.Value)},

	{"FIRST_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, FirstLineSample.Value),
		LBL_OFFSET(LblImageData_typ, FirstLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FirstLineSample.Value)},

	{"INTERCHANGE_FORMAT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InterchangeFormat.Value),
		LBL_OFFSET(LblImageData_typ, InterchangeFormat.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterchangeFormat.Value)},

	{"INVALID_CONSTANT",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[0].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[0].Value)},

	{"INVALID_CONSTANT",			"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[1].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[1].Value)},

	{"INVALID_CONSTANT",			"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[2].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[2].Value)},

	{"LINE_PREFIX_BYTES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LinePrefixBytes.Value),
		LBL_OFFSET(LblImageData_typ, LinePrefixBytes.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LinePrefixBytes.Value)},

	{"LINE_PREFIX_MEAN",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LinePrefixMean.Value),
		LBL_OFFSET(LblImageData_typ, LinePrefixMean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LinePrefixMean.Value)},

	{"LINE_SUFFIX_BYTES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSuffixBytes.Value),
		LBL_OFFSET(LblImageData_typ, LineSuffixBytes.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSuffixBytes.Value)},

	{"LINE_SUFFIX_MEAN",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSuffixMean.Value),
		LBL_OFFSET(LblImageData_typ, LineSuffixMean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSuffixMean.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSamples.Value),
		LBL_OFFSET(LblImageData_typ, LineSamples.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSamples.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Lines.Value),
		LBL_OFFSET(LblImageData_typ, Lines.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Lines.Value)},

	{"MAXIMUM",				"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Maximum.Value),
		LBL_OFFSET(LblImageData_typ, Maximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Maximum.Value)},

	{"MEAN",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Mean.Value),
		LBL_OFFSET(LblImageData_typ, Mean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Mean.Value)},

	{"MEDIAN",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Median.Value),
		LBL_OFFSET(LblImageData_typ, Median.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Median.Value)},

	{"MINIMUM",				"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Minimum.Value),
		LBL_OFFSET(LblImageData_typ, Minimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Minimum.Value)},

	{"MISSING_CONSTANT",			"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, MissingConstant[0].Value),
		LBL_OFFSET(LblImageData_typ, MissingConstant[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissingConstant[0].Value)},

        {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, MissingConstant[1].Value),
                LBL_OFFSET(LblImageData_typ, MissingConstant[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MissingConstant[1].Value)},

        {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, MissingConstant[2].Value),
                LBL_OFFSET(LblImageData_typ, MissingConstant[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MissingConstant[2].Value)},

	{"RADIANCE_OFFSET",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, RadianceOffset.Value),
		LBL_OFFSET(LblImageData_typ, RadianceOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceOffset.Value)},

        {"RADIANCE_OFFSET__UNIT",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, RadianceOffsetUnit.Value),
                LBL_OFFSET(LblImageData_typ, RadianceOffsetUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceOffsetUnit.Value)},

	{"RADIANCE_SCALING_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, RadianceScaleFactor.Value),
		LBL_OFFSET(LblImageData_typ, RadianceScaleFactor.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceScaleFactor.Value)},

        {"RADIANCE_SCALING_FACTOR__UNIT",       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, RadianceScaleFactorUnit.Value),
                LBL_OFFSET(LblImageData_typ, RadianceScaleFactorUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceScaleFactorUnit.Value)},

	{"SAMPLE_BIT_MASK",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleBitMask.Value),
		LBL_OFFSET(LblImageData_typ, SampleBitMask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitMask.Value)},

	{"SAMPLE_BITS",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleBits.Value),
		LBL_OFFSET(LblImageData_typ, SampleBits.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBits.Value)},

	{"SAMPLE_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleType.Value),
		LBL_OFFSET(LblImageData_typ, SampleType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleType.Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[0].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[0].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[1].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[1].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[2].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[2].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[3].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[3].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[4].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[4].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[5].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[5].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[6].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[6].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[7].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[7].Value)},

	{"STANDARD_DEVIATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, StandardDeviation.Value),
		LBL_OFFSET(LblImageData_typ, StandardDeviation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StandardDeviation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "IMAGE_DATA",
	LBL_NULL };

/******************************************************************************
 *				LBL_IMAGE_DATA
 *
 *****************************************************************************/
int     LblImageData(
  int   Unit,
  int   Obtain,
  LblImageData_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblImageData_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_DATA
 *
 *****************************************************************************/
void	LblPrintImageData(
  LblImageData_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_DATA
 *
 *****************************************************************************/
void	LblTestImageData(
  LblImageData_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
