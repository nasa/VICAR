/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_initstate.h"

/******************************************************************************
 *				LBL_INITSTATE
 *
 *	This module contains routines to help create, read/write and print
 *  a InitState property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_articulation.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblInitState.  This routine requires exactly 4 parameters.
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
 *      History of Modifications
 *
 * Date         who             Description
 * ----------   --------------- ---------------------------------------------
 * 2003-01-07   P. Zamani       Changed SOLUTION_ID to be LBL_OPTIONAL
 * ?            A. Runkle       Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblInitState_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[0].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[0].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[1].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[1].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[2].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[2].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[3].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[3].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[4].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[4].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[5].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounter[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[5].Value)},

    {"ROVER_MOTION_COUNTER",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[6].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[6].Value)},

    {"ROVER_MOTION_COUNTER",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[7].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[7].Value)},

    {"ROVER_MOTION_COUNTER",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[8].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[8].Value)},

    {"ROVER_MOTION_COUNTER",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[9].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounter[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[9].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[0].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[0].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[1].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[1].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[2].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[2].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[3].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[3].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[4].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[4].Value)},

	{"ROVER_MOTION_COUNTER_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[5].Value),
		LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[5].Value)},

    {"ROVER_MOTION_COUNTER_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[6].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[6].Value)},

    {"ROVER_MOTION_COUNTER_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[7].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[7].Value)},

    {"ROVER_MOTION_COUNTER_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[8].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[8].Value)},

    {"ROVER_MOTION_COUNTER_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[9].Value),
        LBL_OFFSET(LblInitState_typ, RoverMotionCounterName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[9].Value)},

	{"ORIGIN_OFFSET_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, OriginOffsetVector.Value),
		LBL_OFFSET(LblInitState_typ, OriginOffsetVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginOffsetVector.Value)},

	{"ORIGIN_ROTATION_QUATERNION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(LblInitState_typ, OriginRotationQuaternion.Value),
		LBL_OFFSET(LblInitState_typ, OriginRotationQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginRotationQuaternion.Value)},

    {"QUATERNION_MEASUREMENT_METHOD",   "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInitState_typ, QuaternionMeasurementMethod.Value),
        LBL_OFFSET(LblInitState_typ, QuaternionMeasurementMethod.Valid),
        LBL_NO_RETURN,  LBL_SIZE(QuaternionMeasurementMethod.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_INITSTATE
 *
 *****************************************************************************/
void     LblSetInitState(
  const char	*Name )
{
  if (Name!=NULL)
    Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_INITSTATE
 *
 *****************************************************************************/
int     LblInitState(
  int   Unit,
  int   Obtain,
  LblInitState_typ      *LabelItems,
  int	Instance)
{
  LblSetInitState( "INITIAL_STATE_PARMS" );
  return (LblInitStateApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_INITSTATE_API
 *
 *****************************************************************************/
int     LblInitStateApi(
  int   Unit,
  int   Obtain,
  LblInitState_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;
  LblSetInitState(propertyName);
  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblInitState_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_INITSTATE
 *
 *****************************************************************************/
void	LblPrintInitState(
  LblInitState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_INITSTATE
 *
 *****************************************************************************/
void	LblTestInitState(
  LblInitState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
