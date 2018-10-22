/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_product_request.h"

/******************************************************************************
 *				LBL_PRODUCT_REQUEST
 *
 *	This module contains routines to help create, read/write and print
 *  a Request property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The different available <product> Request types are:
 *		Image
 *		RefPixel	(Reference Pixel)
 *		Thumbnail
 *		Subframe
 *		RowSum
 *		ColSum		(Column Sum)
 *		SunFind
 *		Histogram
 *
 *	The primary routine used by a typical application program is
 *  Lbl<product>Request.  This routine requires exactly 4 parameters.
 *  All label API routines  have the same four parameters:
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
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblProdRequest_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(LblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(LblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(LblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{"PIXEL_DOWNSAMPLE_OPTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, PixelDownsampleOption.Value),
		LBL_OFFSET(LblProdRequest_typ, PixelDownsampleOption.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelDownsampleOption.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, PixelAveragingHeight.Value),
                LBL_OFFSET(LblProdRequest_typ, PixelAveragingHeight.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, PixelAveragingWidth.Value),
                LBL_OFFSET(LblProdRequest_typ, PixelAveragingWidth.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PixelAveragingWidth.Value)},

	{"SUBFRAME_TYPE"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, SubframeType.Value),
		LBL_OFFSET(LblProdRequest_typ, SubframeType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubframeType.Value)},

	{"FIRST_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, FirstLine.Value),
                LBL_OFFSET(LblProdRequest_typ, FirstLine.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLine.Value)},

	{"FIRST_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, FirstLineSample.Value),
                LBL_OFFSET(LblProdRequest_typ, FirstLineSample.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLineSample.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, Lines.Value),
                LBL_OFFSET(LblProdRequest_typ, Lines.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, LineSamples.Value),
                LBL_OFFSET(LblProdRequest_typ, LineSamples.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

	{"SAMPLE_BIT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, SampleBitModeId.Value),
		LBL_OFFSET(LblProdRequest_typ, SampleBitModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitModeId.Value)},

	{"INST_CMPRS_MODE"	,		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstCmprsMode.Value),
		LBL_OFFSET(LblProdRequest_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstCmprsRate.Value),
                LBL_OFFSET(LblProdRequest_typ, InstCmprsRate.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstCmprsQuality.Value),
                LBL_OFFSET(LblProdRequest_typ, InstCmprsQuality.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_FILTER"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstCmprsFilter.Value),
		LBL_OFFSET(LblProdRequest_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstCmprsSegments.Value),
                LBL_OFFSET(LblProdRequest_typ, InstCmprsSegments.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsSegments.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, InstDecompStages.Value),
                LBL_OFFSET(LblProdRequest_typ, InstDecompStages.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstDecompStages.Value)},

	{"REFERENCE_PIXEL_DP_FLAG",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, ReferencePixelDpFlag.Value),
                LBL_OFFSET(LblProdRequest_typ, ReferencePixelDpFlag.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ReferencePixelDpFlag.Value)},

	{"REFERENCE_PIXEL_PRIORITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, ReferencePixelPriority.Value),
                LBL_OFFSET(LblProdRequest_typ, ReferencePixelPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ReferencePixelPriority.Value)},
	{"THUMBNAIL_PRIORITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, ThumbnailPriority.Value),
                LBL_OFFSET(LblProdRequest_typ, ThumbnailPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ThumbnailPriority.Value)},
	{"THUMBNAIL_SIZE",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblProdRequest_typ, ThumbnailSize.Value),
                LBL_OFFSET(LblProdRequest_typ, ThumbnailSize.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ThumbnailSize.Value)},

    {"IMAGE_ID",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblProdRequest_typ, ImageId.Value),
        LBL_OFFSET(LblProdRequest_typ, ImageId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageId.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/***  Table Definitions  ***/

static LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",
	LBL_PDS_STRING_NULL,	LBL_NULL };
/*
 *  IMAGE REQUEST
 */
/******************************************************************************
 *				_LBL_IMAGE_REQUEST
 *
 *****************************************************************************/
int     LblProductRequest(
  int   Unit,
  int   Obtain,
  LblProdRequest_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;

 if (propertyName!=NULL) 
   Label.NameValue = propertyName;

  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_REQUEST
 *
 *****************************************************************************/
void	LblPrintProductRequest(
  LblProdRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_REQUEST
 *
 *****************************************************************************/
void	LblTestProductRequest(
  LblProdRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}


