/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_camera_model.h"

/******************************************************************************
 *				LBL_MODEL
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Camera Model label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_camera_model.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCameraModel.  This routine requires exactly 4 parameters.
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
 *	History of Modifications
 *
 * Date		who		Description
 * ----------   --------------- ---------------------------------------------
 * 2015-08-19   Cecilia Cheng   Added MODEL_COMPONENT__UNIT (should delete MODEL_COMPONENT_UNIT 
 *                              after confirming that that didn't break anything)
 * 2015-07-12   Cecilia Cheng   Added CAMERA_SERIAL_NUMBER
 * 2003-05-14   Hyun Lee        Added REFERENCE_COORD_SYSTEM_SOLN_ID
 * 2003-02-11	Payam Zamani	Added FILTER_NAME
 * 2003-01-10	p. Zamani	Changed GEOMETRY_SOURCE_ID,
 *				DERIVED_GEOMETRY_NOTE, and
 *				DERIVED_GEOMETRY_TYPE to LBL_OPTIONAL
 * 2003-01-07	P. Zamani	Changed MODEL_NAME to be LBL_OPTIONAL
 * ?		A. Runkle	Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCameraModel_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, SolutionId.Value),
		LBL_OFFSET(LblCameraModel_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"CALIBRATION_SOURCE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, CalibrationSourceId.Value),
		LBL_OFFSET(LblCameraModel_typ, CalibrationSourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CalibrationSourceId.Value)},

    {"CAMERA_SERIAL_NUMBER",        "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblCameraModel_typ, CameraSerialNumber.Value),
     LBL_OFFSET(LblCameraModel_typ, CameraSerialNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(CameraSerialNumber.Value)},

	{"MODEL_DESC__PTR",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelDesc.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelDesc.Value)},

	{"MODEL_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelName.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelName.Value)},

	{"MODEL_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelType.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelType.Value)},

	{"GEOMETRY_SOURCE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, GeometrySourceId.Value),
		LBL_OFFSET(LblCameraModel_typ, GeometrySourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GeometrySourceId.Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[0].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[1].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[2].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[3].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[4].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[5].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[6].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[7].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[8].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[0].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[1].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[2].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[3].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[4].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[5].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[6].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[7].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[8].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[0].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[1].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[2].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[3].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[4].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[5].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[6].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[7].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[8].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[0].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[1].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[2].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[3].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[4].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[5].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[6].Value)},

	{"MODEL_COMPONENT__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[7].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnitB[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnitB[8].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[0].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[2].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[2].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[0].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[2].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[2].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[0].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[2].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[2].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[0].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[2].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[2].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[0].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[2].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[2].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[0].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[2].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[2].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[0].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[2].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[2].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[0].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[2].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[2].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[0].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[2].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[2].Value)},

	{"MODEL_TRANSFORM_VECTOR",        "REAL",     LBL_OPTIONAL,
        	LBL_NO_CONT,    3,  1,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ModelTransformVector.Value),
        	LBL_OFFSET(LblCameraModel_typ, ModelTransformVector.Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ModelTransformVector.Value)},

	{"MODEL_TRANSFORM_QUATERNION",      "REAL",     LBL_OPTIONAL,
        	LBL_NO_CONT,    4,  1,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ModelTransformQuaternion.Value),
        	LBL_OFFSET(LblCameraModel_typ, ModelTransformQuaternion.Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ModelTransformQuaternion.Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, FilterName.Value),
		LBL_OFFSET(LblCameraModel_typ, FilterName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName.Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        	LBL_CONTINUE,   1,  7,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[6].Value),
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[6].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        	LBL_CONTINUE,   1,  8,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[7].Value),
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[7].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        	LBL_CONTINUE,   1,  9,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[8].Value),
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[8].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        	LBL_CONTINUE,   1,  10,  LBL_NULL,
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[9].Value),
        	LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[9].Valid),
        	LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

        {"REFERENCE_COORD_SYSTEM_SOLN_ID",      "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemSolnId.Value),
                LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemSolnId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"INTERPOLATION_METHOD",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, InterpolationMethod.Value),
		LBL_OFFSET(LblCameraModel_typ, InterpolationMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterpolationMethod.Value)},

	{"INTERPOLATION_VALUE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, InterpolationValue.Value),
		LBL_OFFSET(LblCameraModel_typ, InterpolationValue.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterpolationValue.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static	LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblSetCameraModel(
  const	char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_CAMERA_MODEL
 *
 *****************************************************************************/
int     LblCameraModel(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{
  LblSetCameraModel("CAMERA_MODEL");
  return (LblCameraModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_GEOMETRIC_CAMERA_MODEL
 *
 *****************************************************************************/
int     LblGeometricCameraModel(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{
  LblSetCameraModel("GEOMETRIC_CAMERA_MODEL");
  return (LblCameraModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_CAMERA_MODEL_API
 *
 *****************************************************************************/
int     LblCameraModelApi(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCameraModel_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblPrintCameraModel(
  LblCameraModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblTestCameraModel(
  LblCameraModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
