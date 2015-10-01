#ifndef MIPS_LBL_PRODUCT_REQUEST_INCLUDED
#define MIPS_LBL_PRODUCT_REQUEST_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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
 * History of modifications:
 * ------------------------
 * Date         Who             Description
 * -----------  --------------- ---------------------------------------------
 * 2003-05-23   H. Lee          Changed SanpleBitModeId to SampleBitModeId.
 * 2003-01-15   H. Lee          Change type of InstCmprsMode to LblApiIntItem_typ
 *****************************************************************************/
#define IMAGE_REQUEST_PARMS_PROPERTY_NAME             "IMAGE_REQUEST_PARMS"
#define THUMBNAIL_REQUEST_PARMS_PROPERTY_NAME         "THUMBNAIL_REQUEST_PARMS"
#define COLUMN_SUM_REQUEST_PARMS_PROPERTY_NAME        "COLUMN_SUM_REQUEST_PARMS"
#define ROW_SUM_REQUEST_PARMS_PROPERTY_NAME           "ROW_SUM_REQUEST_PARMS"
#define HISTOGRAM_REQUEST_PARMS_PROPERTY_NAME         "HISTOGRAM_REQUEST_PARMS"
#define REFERENCE_PIXEL_REQUEST_PARMS_PROPERTY_NAME   "REFERENCE_PIXEL_REQUEST_PARMS"
#define SUBFRAME_REQUEST_PARMS_PROPERTY_NAME          "SUBFRAME_REQUEST_PARMS"

typedef struct
	{
	LblApiIdItem_typ		SourceId;
	LblApiFlagItem_typ		GroupApplicabilityFlag;
	LblApiIntItem_typ		DownloadPriority;
	LblApiTypeItem_typ		SubframeType;
	LblApiIntItem_typ		FirstLine;
	LblApiIntItem_typ		FirstLineSample;
	LblApiIntItem_typ		Lines;
	LblApiIntItem_typ		LineSamples;
	LblApiIntItem_typ		InstCmprsMode;
	LblApiRealItem_typ		InstCmprsRate;
	LblApiIntItem_typ		InstCmprsQuality;
	LblApiIdItem_typ		InstCmprsFilter;
	LblApiIntItem_typ		InstCmprsSegments;
	LblApiIntItem_typ		InstDecompStages;
	LblApiIdItem_typ		SampleBitModeId;
	LblApiTypeItem_typ		PixelDownsampleOption;
	LblApiIntItem_typ		PixelAveragingHeight;
	LblApiIntItem_typ		PixelAveragingWidth;
	LblApiIntItem_typ       ReferencePixelDpFlag;
	LblApiIntItem_typ       ReferencePixelPriority;
	LblApiIntItem_typ       ThumbnailPriority;
	LblApiStringItem_typ    ThumbnailSize;
	LblApiStringItem_typ    ImageId;
	} LblProdRequest_typ;

int	LblProductRequest( int, int, LblProdRequest_typ *, int,const char* );
void	LblTestProductRequest( LblProdRequest_typ *);
void	LblPrintProductRequest( LblProdRequest_typ *);

#ifdef __cplusplus
}
#endif

#endif
