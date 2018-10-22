#ifndef MIPS_LBL_COMPRESSION_INCLUDED
#define MIPS_LBL_COMPRESSION_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_COMPRESSION
 *
 *	This module contains routines to help create, read/write and print
 *  a Compression property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The primary routine used by a typical application program is
 *  LblCompression.  This routine requires exactly 4 parameters.
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
 * Date         who         Description
 * -----------  --------------- ----------------------------------------------
 * 25-Mar-2003  Hyun Lee        Added InstCmprsSegMissingPixels and
 *                              InstCmprsSegQuality
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/
#define  LBL_MAX_SEGS	32

typedef struct
	{
	LblApiIntItem_typ		ErrorPixels;
	LblApiIntItem_typ		InstCmprsBlkSize;
	LblApiIntItem_typ		InstCmprsBlocks;
	LblApiDescItem_typ		InstCmprsDesc;
	LblApiRealItem_typ		InstCmprsEntropy;
	LblApiIdItem_typ		InstCmprsFilter;
	LblApiIntItem_typ		InstCmprsMode;
	LblApiLongNameItem_typ  InstCmprsName;
	LblApiIntItem_typ		InstCmprsParam;
	LblApiIntItem_typ		InstCmprsQuality;
	LblApiIdItem_typ		InstCmprsQuantzTblId;
	LblApiDescItem_typ		InstCmprsQuantzType;
	LblApiRealItem_typ		InstCmprsRate;
	LblApiRealItem_typ		InstCmprsRatio;
	LblApiIntItem_typ		InstCmprsSegments;
	LblApiRealItem_typ		InstCmprsSegmentQuality[LBL_MAX_SEGS];
	LblApiTypeItem_typ		InstCmprsSegmentStatus[LBL_MAX_SEGS];
	LblApiIntItem_typ		InstCmprsSegFirstLine[LBL_MAX_SEGS];
	LblApiIntItem_typ		InstCmprsSegFirstLineSamp[LBL_MAX_SEGS];
	LblApiIntItem_typ		InstCmprsSegLines[LBL_MAX_SEGS];
	LblApiIntItem_typ       InstCmprsSegMissingPixels[LBL_MAX_SEGS];
	LblApiRealItem_typ      InstCmprsSegQuality[LBL_MAX_SEGS];
	LblApiIntItem_typ		InstCmprsSegSamples[LBL_MAX_SEGS];
	LblApiIntItem_typ		InstCmprsSyncBlks;
	LblApiIntItem_typ		InstDecompStages;
    LblApiFlagItem_typ      InstCmprsDeferredFlag;
    LblApiStringItem_typ    InstCmprsColorMode;
	LblApiIntItem_typ		PixelAveragingHeight;
	LblApiIntItem_typ		PixelAveragingWidth;
	LblApiIntItem_typ		RiceOptionValue;
	LblApiIntItem_typ		RiceStartOption;
	LblApiIntItem_typ		SqrtMaximumPixel;
	LblApiIntItem_typ		SqrtMinimumPixel;
	} LblCompression_typ;

/***  Function prototypes  ***/
int	LblCompression( int, int, LblCompression_typ *, int );
int	LblCompressionParms( int, int, LblCompression_typ *, int );
	/***  For development & internal use  ***/
int	LblCompressionApi( int, int, LblCompression_typ *, int );
void	LblSetCompression( const char * );
void	LblTestCompression( LblCompression_typ *);
void	LblPrintCompression( LblCompression_typ *);

#ifdef __cplusplus
}
#endif

#endif
