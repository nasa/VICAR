/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_chem_request.h"

/******************************************************************************
 *				_LBL_CHEM_REQUEST
 *
 *	This module contains routines to help create, read/write and print
 *  a ChemRequest property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblChemRequest.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblChemRequest_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"SOURCE_ID",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, SourceId.Value),
        LBL_OFFSET(LblChemRequest_typ, SourceId.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SourceId.Value)},
    
    {"DARK_SPECTRA_MODE",        "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, DarkSpectraMode.Value),
        LBL_OFFSET(LblChemRequest_typ, DarkSpectraMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DarkSpectraMode.Value)},

    {"GROUP_APPLICABILITY_FLAG",        "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, GroupApplicabilityFlag.Value),
        LBL_OFFSET(LblChemRequest_typ, GroupApplicabilityFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GroupApplicabilityFlag.Value)},
    
	{"GAIN_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, GainNumber.Value),
                LBL_OFFSET(LblChemRequest_typ, GainNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(GainNumber.Value)},

	{"INSTRUMENT_FOCUS_INIT_FLAG",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, InstrumentFocusInitFlag.Value),
                LBL_OFFSET(LblChemRequest_typ, InstrumentFocusInitFlag.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusInitFlag.Value)},

	{"INSTRUMENT_FOCUS_MODE",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, InstrumentFocusMode.Value),
                LBL_OFFSET(LblChemRequest_typ, InstrumentFocusMode.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusMode.Value)},

	{"INSTRUMENT_FOCUS_DISTANCE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, InstrumentFocusDistance.Value),
                LBL_OFFSET(LblChemRequest_typ, InstrumentFocusDistance.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusDistance.Value)},

    {"INSTRUMENT_FOCUS_DISTANCE__UNIT", "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, InstrumentFocusDistanceUnit.Value),
        LBL_OFFSET(LblChemRequest_typ, InstrumentFocusDistanceUnit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(InstrumentFocusDistanceUnit.Value)},

    {"INSTRUMENT_MODE_ID",       "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, InstrumentModeId.Value),
                LBL_OFFSET(LblChemRequest_typ, InstrumentModeId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentModeId.Value)},

    {"LASER_MODE",       "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblChemRequest_typ, LaserMode.Value),
                LBL_OFFSET(LblChemRequest_typ, LaserMode.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LaserMode.Value)},

	{"OFFSET_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, OffsetNumber.Value),
                LBL_OFFSET(LblChemRequest_typ, OffsetNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(OffsetNumber.Value)},

	{"SOH_PRIORITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, SohPriority.Value),
                LBL_OFFSET(LblChemRequest_typ, SohPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SohPriority.Value)},
	{"START_IMAGE_ID",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, StartImageId.Value),
                LBL_OFFSET(LblChemRequest_typ, StartImageId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(StartImageId.Value)},

	{"VALID_MINIMUM_PIXEL",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, ValidMinimum.Value),
                LBL_OFFSET(LblChemRequest_typ, ValidMinimum.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ValidMinimum.Value)},

	{"VALID_MAXIMUM_PIXEL",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, ValidMaximum.Value),
                LBL_OFFSET(LblChemRequest_typ, ValidMaximum.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ValidMaximum.Value)},

	/*
	{"",		"",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, .Value),
                LBL_OFFSET(LblChemRequest_typ, .Valid),
                LBL_NO_RETURN,  LBL_SIZE(.Value)},
	*/

	/*
	{"INSTRUMENT_FOCUS_MODE",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemRequest_typ, .Value),
                LBL_OFFSET(LblChemRequest_typ, .Valid),
                LBL_NO_RETURN,  LBL_SIZE(.Value)},

	*/
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"CHEM_REQUEST_PARMS",	LBL_NULL };

/******************************************************************************
 *				_LBL_CHEM_REQUEST
 *
 *****************************************************************************/
int     LblChemRequest(
  int   Unit,
  int   Obtain,
  LblChemRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblChemRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_CHEM_REQUEST
 *
 *****************************************************************************/
void	LblPrintChemRequest(
  LblChemRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_CHEM_REQUEST
 *
 *****************************************************************************/
void	LblTestChemRequest(
  LblChemRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
