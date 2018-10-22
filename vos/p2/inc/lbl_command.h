#ifndef MIPS_LBL_COMMAND_INCLUDED
#define MIPS_LBL_COMMAND_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_COMMAND
 *
 *	This module contains routines to help create, read/write and print
 *  a Command property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The primary routine used by a typical application program is
 *  LblCommand.  This routine requires exactly 4 parameters.
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
 *****************************************************************************/

typedef struct
	{
	LblApiIntItem_typ		AutoExposureDataCut;
	LblApiRealItem_typ		AutoExposurePixelFraction;
	LblApiFlagItem_typ		BadPixelReplacementFlag;
	LblApiDescItem_typ		CommandDesc;
	LblApiNameItem_typ		CommandName;
	LblApiFlagItem_typ		DarkCurrentCorrectionFlag;
	LblApiTypeItem_typ		DownloadType;
	LblApiFlagItem_typ		FlatFieldCorrectionFlag;
	LblApiIdItem_typ		InstrumentModeId;
	LblApiIntItem_typ		MaxAutoExposIterationCount;
	LblApiFlagItem_typ		ShutterEffectCorrectionFlag;
	LblApiFlagItem_typ		SqrtCompressionFlag;
	} LblCommand_typ;

int	LblCommand( int, int, LblCommand_typ *, int );
void	LblTestCommand( LblCommand_typ *);
void	LblPrintCommand( LblCommand_typ *);

#ifdef __cplusplus
}
#endif

#endif
