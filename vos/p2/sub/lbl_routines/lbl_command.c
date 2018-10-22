/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_command.h"

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
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCommand.  This routine requires exactly 4 parameters.
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
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCommand_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"AUTO_EXPOSURE_DATA_CUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, AutoExposureDataCut.Value),
		LBL_OFFSET(LblCommand_typ, AutoExposureDataCut.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AutoExposureDataCut.Value)},

	{"AUTO_EXPOSURE_PIXEL_FRACTION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, AutoExposurePixelFraction.Value),
		LBL_OFFSET(LblCommand_typ, AutoExposurePixelFraction.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AutoExposurePixelFraction.Value)},

	{"BAD_PIXEL_REPLACEMENT_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, BadPixelReplacementFlag.Value),
		LBL_OFFSET(LblCommand_typ, BadPixelReplacementFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementFlag.Value)},

	{"COMMAND_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, CommandDesc.Value),
		LBL_OFFSET(LblCommand_typ, CommandDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandDesc.Value)},

	{"COMMAND_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, CommandName.Value),
		LBL_OFFSET(LblCommand_typ, CommandName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandName.Value)},

	{"DARK_CURRENT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, DarkCurrentCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, DarkCurrentCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DarkCurrentCorrectionFlag.Value)},

	{"DOWNLOAD_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, DownloadType.Value),
		LBL_OFFSET(LblCommand_typ, DownloadType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DownloadType.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"INSTRUMENT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, InstrumentModeId.Value),
		LBL_OFFSET(LblCommand_typ, InstrumentModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentModeId.Value)},

	{"MAX_AUTO_EXPOS_ITERATION_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, MaxAutoExposIterationCount.Value),
		LBL_OFFSET(LblCommand_typ, MaxAutoExposIterationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaxAutoExposIterationCount.Value)},

	{"SHUTTER_EFFECT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, ShutterEffectCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, ShutterEffectCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterEffectCorrectionFlag.Value)},

	{"SQRT_COMPRESSION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, SqrtCompressionFlag.Value),
		LBL_OFFSET(LblCommand_typ, SqrtCompressionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtCompressionFlag.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "COMMAND",
	LBL_NULL };

/******************************************************************************
 *				LBL_COMMAND
 *
 *****************************************************************************/
int     LblCommand(
  int   Unit,
  int   Obtain,
  LblCommand_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCommand_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COMMAND
 *
 *****************************************************************************/
void	LblPrintCommand(
  LblCommand_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COMMAND
 *
 *****************************************************************************/
void	LblTestCommand(
  LblCommand_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
