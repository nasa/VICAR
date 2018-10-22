/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_coordinate.h"

/******************************************************************************
 *				LBL_COORDINATE
 *
 *	This module contains routines to help create, read/write and print
 *  a Coordinate property label.  It is part of the MIPL label API package,
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
 *  LblCoordinate.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblCoordinate_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, SolutionId.Value),
		LBL_OFFSET(LblCoordinate_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"COORDINATE_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemName.Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemName.Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[0].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[0].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[1].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[1].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[2].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[2].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[3].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[3].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[4].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[4].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[5].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[5].Value)},

    {"COORDINATE_SYSTEM_INDEX",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[6].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndex[6].Value)},

    {"COORDINATE_SYSTEM_INDEX",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[7].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndex[7].Value)},

    {"COORDINATE_SYSTEM_INDEX",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[8].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndex[8].Value)},

    {"COORDINATE_SYSTEM_INDEX",     "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[9].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndex[9].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[0].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[0].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[1].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[1].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[2].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[2].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[3].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[3].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[4].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[4].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[5].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[5].Value)},

    {"COORDINATE_SYSTEM_INDEX_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[6].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndexName[6].Value)},

    {"COORDINATE_SYSTEM_INDEX_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[7].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndexName[7].Value)},

    {"COORDINATE_SYSTEM_INDEX_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[8].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndexName[8].Value)},

    {"COORDINATE_SYSTEM_INDEX_NAME",    "STRING",   LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[9].Value),
        LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemIndexName[9].Value)},

	{"ORIGIN_ROTATION_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, OriginRotationAngle.Value),
		LBL_OFFSET(LblCoordinate_typ, OriginRotationAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginRotationAngle.Value)},

	{"ORIGIN_OFFSET_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, OriginOffsetVector.Value),
		LBL_OFFSET(LblCoordinate_typ, OriginOffsetVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginOffsetVector.Value)},

	{"ORIGIN_ROTATION_QUATERNION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, OriginRotationQuaternion.Value),
		LBL_OFFSET(LblCoordinate_typ, OriginRotationQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginRotationQuaternion.Value)},

	{"POSITIVE_AZIMUTH_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, PositiveAzimuthDirection.Value),
		LBL_OFFSET(LblCoordinate_typ, PositiveAzimuthDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveAzimuthDirection.Value)},

	{"POSITIVE_ELEVATION_DIRECTION",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, PositiveElevationDirection.Value),
		LBL_OFFSET(LblCoordinate_typ, PositiveElevationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveElevationDirection.Value)},

	{"QUATERNION_MEASUREMENT_METHOD",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, QuaternionMeasurementMethod.Value),
		LBL_OFFSET(LblCoordinate_typ, QuaternionMeasurementMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(QuaternionMeasurementMethod.Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[6].Value),
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[7].Value),
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[8].Value),
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[9].Value),
        LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"RVR_SAVED_I",	"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, RvrSavedI.Value),
		LBL_OFFSET(LblCoordinate_typ, RvrSavedI.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RvrSavedI.Value)},

	{"RVR_SAVED_P",	"REAL",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
	 LBL_OFFSET(LblCoordinate_typ, RvrSavedP.Value),
		LBL_OFFSET(LblCoordinate_typ, RvrSavedP.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RvrSavedP.Value)},

	{"RVR_SAVED_Q",	"REAL",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, RvrSavedQ.Value),
		LBL_OFFSET(LblCoordinate_typ, RvrSavedQ.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RvrSavedQ.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_COORDINATE
 *
 *****************************************************************************/
void     LblSetCoordinate(
  const char	*Name )
{
  if (Name!=NULL)
    Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_COORDINATE
 *
 *****************************************************************************/
int     LblCoordinate(
  int   Unit,
  int   Obtain,
  LblCoordinate_typ      *LabelItems,
  int	Instance)
{
  LblSetCoordinate( "COORDINATE_SYSTEM" );
  return (LblCoordinateApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_COORDINATE_API
 *
 *****************************************************************************/
int     LblCoordinateApi(
  int   Unit,
  int   Obtain,
  LblCoordinate_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;
  LblSetCoordinate(propertyName);
  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCoordinate_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COORDINATE
 *
 *****************************************************************************/
void	LblPrintCoordinate(
  LblCoordinate_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COORDINATE
 *
 *****************************************************************************/
void	LblTestCoordinate(
  LblCoordinate_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
