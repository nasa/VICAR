/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_articulation.h"

/******************************************************************************
 *				LBL_ARTICULATION
 *
 *	This module contains routines to help create, read/write and print
 *  a Articulation property label.  It is part of the MIPL label API package,
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
 *  LblArticulation.  This routine requires exactly 4 parameters.
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
 * 2017-10-30   G. Hollins      Changed ArticulationDeviceTempCount to INT
 * 2015-08-19   C. Cheng        Added ArticulationDeviceTempCount
 * 2015-07-01   C. Cheng        Added GravityAcceleration
 * 2015-07-01   C. Cheng        Added ArticulationDevicePhase
 * 2003-01-13   P. Zamani       Added ArticulationDevInstrumentId
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblArticulation_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, SolutionId.Value),
		LBL_OFFSET(LblArticulation_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"ARTICULATION_DEVICE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceId.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceId.Value)},

	{"ARTICULATION_DEVICE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceName.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceName.Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[9].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[9].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[9].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[0].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[1].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[2].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[3].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[4].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[5].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[6].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[7].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[8].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[9].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[0].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[1].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[2].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[3].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[4].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[5].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[6].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[7].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[8].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[9].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[0].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[1].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[2].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[3].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[4].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[5].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[6].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[7].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[8].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[9].Value)},

/**/

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[0].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[1].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[2].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[3].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[4].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[5].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[6].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[7].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[8].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[9].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[0].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[1].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[2].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[3].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[4].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[5].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[6].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[7].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[8].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[9].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[0].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[1].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[2].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[3].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[4].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[5].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[6].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[7].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[8].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[9].Value)},

/**/

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[0].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[1].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[2].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[3].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[4].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[5].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[6].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[7].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[8].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[9].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[0].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[1].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[2].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[3].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[4].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[5].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[6].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[7].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[8].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[9].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[0].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[1].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[2].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[3].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[4].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[5].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[6].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[7].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[8].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[9].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[0].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[1].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[2].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[0].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[1].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[2].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[0].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[1].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[2].Value)},

	{"ARTICULATION_DEVICE_MODE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceMode.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceMode.Value)},

	{"ARTICULATION_DEVICE_TEMP_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempCount[0].Value)},

	{"ARTICULATION_DEVICE_TEMP_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempCount[1].Value)},

    {"ARTICULATION_DEVICE_TEMP_COUNT",        "INT",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[2].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempCount[2].Value)},

    {"ARTICULATION_DEVICE_TEMP_COUNT",        "INT",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[3].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempCount[3].Value)},

    {"ARTICULATION_DEVICE_TEMP_COUNT",        "INT",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[4].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempCount[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempCount[4].Value)},

	{"ARTICULATION_DEVICE_TEMP",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTemp[0].Value)},

	{"ARTICULATION_DEVICE_TEMP",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTemp[1].Value)},

    {"ARTICULATION_DEVICE_TEMP",        "REAL",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[2].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTemp[2].Value)},

    {"ARTICULATION_DEVICE_TEMP",        "REAL",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[3].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTemp[3].Value)},

    {"ARTICULATION_DEVICE_TEMP",        "REAL",     LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[4].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTemp[4].Value)},

	{"ARTICULATION_DEVICE_TEMP__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempUnit[0].Value)},

	{"ARTICULATION_DEVICE_TEMP__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempUnit[1].Value)},

    {"ARTICULATION_DEVICE_TEMP__UNIT",  "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[2].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempUnit[2].Value)},

    {"ARTICULATION_DEVICE_TEMP__UNIT",  "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[3].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempUnit[3].Value)},

    {"ARTICULATION_DEVICE_TEMP__UNIT",  "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[4].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempUnit[4].Value)},

	{"ARTICULATION_DEVICE_TEMP_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempName[0].Value)},

	{"ARTICULATION_DEVICE_TEMP_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempName[1].Value)},

    {"ARTICULATION_DEVICE_TEMP_NAME",   "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[2].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempName[2].Value)},

    {"ARTICULATION_DEVICE_TEMP_NAME",   "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  4,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[3].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempName[3].Value)},

    {"ARTICULATION_DEVICE_TEMP_NAME",   "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  5,  LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[4].Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceTempName[4].Value)},

	{"ARTICULATION_DEV_INSTRUMENT_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevInstrumentId.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevInstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevInstrumentId.Value)},

	{"ARTICULATION_DEV_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVector.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevVector.Value)},

	{"ARTICULATION_DEV_VECTOR_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVectorName.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVectorName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevVectorName.Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[0].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[0].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[1].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[1].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[2].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[2].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[3].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[3].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[4].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[4].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[5].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[5].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[6].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[6].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[7].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[7].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[0].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[1].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[2].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[3].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[4].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[5].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[6].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[7].Value)},

	{"ARTICULATION_DEV_INSTRUMENT_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, InstrumentId.Value),
		LBL_OFFSET(LblArticulation_typ, InstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId.Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[0].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[1].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[2].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[3].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[4].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[5].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[6].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[7].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[8].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[9].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[10].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[10].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[11].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[11].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[0].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[1].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[2].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[3].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[4].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[5].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[6].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[7].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[8].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[9].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[10].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[10].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[11].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[11].Value)},

	{"GRAVITY_ACCELERATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, GravityAcceleration.Value),
		LBL_OFFSET(LblArticulation_typ, GravityAcceleration.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GravityAcceleration.Value)},

	{"ARTICULATION_DEVICE_PHASE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
        LBL_OFFSET(LblArticulation_typ, ArticulationDevicePhase.Value),
        LBL_OFFSET(LblArticulation_typ, ArticulationDevicePhase.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevicePhase.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_ARTICULATION
 *
 *****************************************************************************/
void     LblSetArticulation(
  const char	*Name )
{
  if (Name!=NULL)
    Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_ARTICULATION
 *
 *****************************************************************************/
int     LblArticulation(
  int   Unit,
  int   Obtain,
  LblArticulation_typ      *LabelItems,
  int	Instance)
{
  LblSetArticulation("ARTICULATION_STATE");
  return (LblArticulationApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_ARTICULATION_API
 *
 *****************************************************************************/
int     LblArticulationApi(
  int   Unit,
  int   Obtain,
  LblArticulation_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;
  LblSetArticulation(propertyName);
  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblArticulation_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_ARTICULATION
 *
 *****************************************************************************/
void	LblPrintArticulation(
  LblArticulation_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_ARTICULATION
 *
 *****************************************************************************/
void	LblTestArticulation(
  LblArticulation_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
