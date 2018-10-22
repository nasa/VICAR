/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_compression.h"

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
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_compression.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCompression.  This routine requires exactly 4 parameters.
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
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who         Description
 * -----------  --------------- ----------------------------------------------
 * 18-Apr-2003  Hyun Lee        Changed INST_CMPRS_SEG_QUALITY to 
 *                              INST_CMPRS_SEGMENT_QUALITY
 * 25-Mar-2003  Hyun Lee        Added INST_CMPRS_SEG_MISSING_PIXELS, and 
 *                              INST_CMPRS_SEG_QUALITY 
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCompression_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"ERROR_PIXELS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, ErrorPixels.Value),
		LBL_OFFSET(LblCompression_typ, ErrorPixels.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ErrorPixels.Value)},

	{"INST_CMPRS_BLK_SIZE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsBlkSize.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsBlkSize.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsBlkSize.Value)},

	{"INST_CMPRS_BLOCKS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsBlocks.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsBlocks.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsBlocks.Value)},

	{"INST_CMPRS_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsDesc.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsDesc.Value)},

	{"INST_CMPRS_FILTER",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsFilter.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_CMPRS_ENTROPY",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsEntropy.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsEntropy.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsEntropy.Value)},

	{"INST_CMPRS_MODE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsMode.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsName.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsName.Value)},

	{"INST_CMPRS_PARAM",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsParam.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsParam.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsParam.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuality.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuality.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_QUANTZ_TBL_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzTblId.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzTblId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuantzTblId.Value)},

	{"INST_CMPRS_QUANTZ_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzType.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuantzType.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsRate.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsRate.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_RATIO",			"REAL",		LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsRatio.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsRatio.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRatio.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegments.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegments.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegments.Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[0].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[1].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[2].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[3].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[4].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[5].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[6].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[7].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[8].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[9].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[10].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[11].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[12].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[13].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[14].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[15].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[16].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[17].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[18].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[19].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[20].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[21].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[22].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[23].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[24].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[25].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[26].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[27].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[28].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[29].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[30].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentQuality[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentQuality[31].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[0].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[1].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[2].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[3].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[4].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[5].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[6].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[7].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[8].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[9].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[10].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[11].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[12].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[13].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[14].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[15].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[16].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[17].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[18].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[19].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[20].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[21].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[22].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[23].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[24].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[25].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[26].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[27].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[28].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[29].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[30].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[31].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[0].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[1].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[2].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[3].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[4].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[5].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[6].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[7].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[8].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[9].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[10].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[11].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[12].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[13].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[14].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[15].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[16].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[17].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[18].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[19].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[20].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[21].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[22].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[23].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[24].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[25].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[26].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[27].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[28].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[29].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[30].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[31].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[0].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[1].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[2].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[3].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[4].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[5].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[6].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[7].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[8].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[9].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[10].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[11].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[12].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[13].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[14].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[15].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[16].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[17].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[18].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[19].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[20].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[21].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[22].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[23].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[24].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[25].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[26].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[27].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[28].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[29].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[30].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[31].Value)},

    {"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[0].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[1].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[2].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[3].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[4].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[5].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[6].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[7].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[8].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[9].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[10].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[11].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[12].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[13].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[14].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[15].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[16].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[17].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[18].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[19].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[20].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[21].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[22].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[23].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[24].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[25].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[26].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[27].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[28].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[29].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[30].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[31].Value)},

	{"INST_CMPRS_SYNC_BLKS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSyncBlks.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSyncBlks.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSyncBlks.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstDecompStages.Value),
		LBL_OFFSET(LblCompression_typ, InstDecompStages.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstDecompStages.Value)},

    {"INST_CMPRS_DEFERRED_FLAG",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblCompression_typ, InstCmprsDeferredFlag.Value),
        LBL_OFFSET(LblCompression_typ, InstCmprsDeferredFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstCmprsDeferredFlag.Value)},

    {"INST_CMPRS_COLOR_MODE",        "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblCompression_typ, InstCmprsColorMode.Value),
        LBL_OFFSET(LblCompression_typ, InstCmprsColorMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstCmprsColorMode.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, PixelAveragingHeight.Value),
		LBL_OFFSET(LblCompression_typ, PixelAveragingHeight.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, PixelAveragingWidth.Value),
		LBL_OFFSET(LblCompression_typ, PixelAveragingWidth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingWidth.Value)},

	{"RICE_OPTION_VALUE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, RiceOptionValue.Value),
		LBL_OFFSET(LblCompression_typ, RiceOptionValue.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RiceOptionValue.Value)},

	{"RICE_START_OPTION",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, RiceStartOption.Value),
		LBL_OFFSET(LblCompression_typ, RiceStartOption.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RiceStartOption.Value)},

	{"SQRT_MAXIMUM_PIXEL",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, SqrtMaximumPixel.Value),
		LBL_OFFSET(LblCompression_typ, SqrtMaximumPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtMaximumPixel.Value)},

	{"SQRT_MINIMUM_PIXEL",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, SqrtMinimumPixel.Value),
		LBL_OFFSET(LblCompression_typ, SqrtMinimumPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtMinimumPixel.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_COMPRESSION
 *
 *****************************************************************************/
void     LblSetCompression(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_COMPRESSION
 *
 *****************************************************************************/
int     LblCompression(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{
  LblSetCompression("COMPRESSION");
  return (LblCompressionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_COMPRESSION_PARMS
 *
 *****************************************************************************/
int     LblCompressionParms(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{
  LblSetCompression("COMPRESSION_PARMS");
  return (LblCompressionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_COMPRESSION_API
 *
 *****************************************************************************/
int     LblCompressionApi(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCompression_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COMPRESSION
 *
 *****************************************************************************/
void	LblPrintCompression(
  LblCompression_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COMPRESSION
 *
 *****************************************************************************/
void	LblTestCompression(
  LblCompression_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
