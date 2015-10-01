#ifndef MIPS_LBL_IMAGE_DATA_INCLUDED
#define MIPS_LBL_IMAGE_DATA_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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
 * 2003-11-12   H. Lee          Added RadianceOffsetUnit & RadianceScaleFactorUnit
 * 2003-07-08   H. Lee          Changed Maximum & Minimum to LblApiRealItem_typ.
 *                              Changed Checksum to LblApiRealItem_typ.
 * 2003-07-01   H. Lee          Changed MissingConstant as an array.
 * 2003-03-03   H. Lee          Changed InvalidConstant & MissingConstant
 *                              to LblApiRealItem_typ from LblApiIntItem_typ
 * 2003-02-26   H. Lee          Changed InvalidConstant & MissingConstant
 *                              to LblApiIntItem_typ from LblApiTypeItem_typ
 *****************************************************************************/

typedef struct
	{
	LblApiIntItem_typ		Bands;
	  LblApiStringItem_typ          BandStorageType;
	LblApiRealItem_typ		Checksum;
	LblApiIntItem_typ		FirstLine;
	LblApiIntItem_typ		FirstLineSample;
	LblApiIdItem_typ		InterchangeFormat;
	LblApiRealItem_typ		InvalidConstant[LBL_RGB_ITEMS];
	LblApiIntItem_typ		LinePrefixBytes;
	LblApiRealItem_typ		LinePrefixMean;
	LblApiIntItem_typ		Lines;
	LblApiIntItem_typ		LineSamples;
	LblApiIntItem_typ		LineSuffixBytes;
	LblApiRealItem_typ		LineSuffixMean;
	LblApiRealItem_typ		Maximum;
	LblApiRealItem_typ		Mean;
	LblApiRealItem_typ		Median;
	LblApiRealItem_typ		Minimum;
	LblApiRealItem_typ		MissingConstant[LBL_RGB_ITEMS];
	LblApiRealItem_typ		RadianceOffset;
        LblApiTypeItem_typ              RadianceOffsetUnit;
	LblApiRealItem_typ		RadianceScaleFactor;
        LblApiTypeItem_typ              RadianceScaleFactorUnit;
	LblApiLongNameItem_typ		SampleBitMask;
	LblApiIntItem_typ		SampleBits;
	LblApiTypeItem_typ		SampleType;
	LblApiDescItem_typ		SpiceFileName[8];
	LblApiRealItem_typ		StandardDeviation;
	} LblImageData_typ;

int	LblImageData( int, int, LblImageData_typ *, int );
void	LblTestImageData( LblImageData_typ *);
void	LblPrintImageData( LblImageData_typ *);

#ifdef __cplusplus
}
#endif

#endif
