/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_ground_support.h"

/******************************************************************************
 *				LBL_GROUND_SUPPORT
 *
 *	This module contains routines to help create, read/write and print
 *  a Ground Support property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_ground_support.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblGroundSupport.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblGroundSupport_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CAMERA_LOCATION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, CameraLocationId.Value),
		LBL_OFFSET(LblGroundSupport_typ, CameraLocationId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CameraLocationId.Value)},

	{"FACILITY_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, FacilityName.Value),
		LBL_OFFSET(LblGroundSupport_typ, FacilityName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FacilityName.Value)},

	{"LIGHT_SOURCE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceName.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceName.Value)},

	{"LIGHT_SOURCE_DISTANCE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistance.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceDistance.Value)},

	{"LIGHT_SOURCE_DISTANCE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistanceUnit.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistanceUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceDistanceUnit.Value)},

	{"LIGHT_SOURCE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceType.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceType.Value)},

	{"PRESSURE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, Pressure.Value),
		LBL_OFFSET(LblGroundSupport_typ, Pressure.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Pressure.Value)},

	{"PRODUCER_FULL_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, ProducerFullName.Value),
		LBL_OFFSET(LblGroundSupport_typ, ProducerFullName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProducerFullName.Value)},

	{"TARGET_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetDistance.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetDistance.Value)},

	{"TARGET_DISTANCE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetDistanceUnit.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetDistanceUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetDistanceUnit.Value)},

	{"TARGET_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetName.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetName.Value)},

	{"TEST_PHASE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TestPhaseName.Value),
		LBL_OFFSET(LblGroundSupport_typ, TestPhaseName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TestPhaseName.Value)},

	{"NOTE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, Note.Value),
		LBL_OFFSET(LblGroundSupport_typ, Note.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Note.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"GROUND_SUPPORT_EQUIPMENT",	LBL_NULL };

/******************************************************************************
 *				LBL_GROUND_SUPPORT
 *
 *****************************************************************************/
int     LblGroundSupport(
  int   Unit,
  int   Obtain,
  LblGroundSupport_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblGroundSupport_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_GROUND_SUPPORT
 *
 *****************************************************************************/
void	LblPrintGroundSupport(
  LblGroundSupport_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_GROUND_SUPPORT
 *
 *****************************************************************************/
void	LblTestGroundSupport(
  LblGroundSupport_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
