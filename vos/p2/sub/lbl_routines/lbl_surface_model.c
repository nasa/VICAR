/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_surface_model.h"

/******************************************************************************
 *				LBL_SURFACE_MODEL
 *
 *	This module contains routines to help create, read/write and print
 *  a Surface Model property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblSurfaceModel.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblSurfaceModel_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[6].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[7].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[8].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[9].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SURFACE_MODEL_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelDesc.Value),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceModelDesc.Value)},

	{"SURFACE_MODEL_TYPE",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelType.Value),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceModelType.Value)},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[0]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[0])},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[1]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[1])},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[2]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[2])},


	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[0]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[0])},

	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[1]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[1])},

	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[2]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[2])},



	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SURFACE_MODEL
 *
 *****************************************************************************/
int     LblSurfaceModel(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{
  LblSetSurfaceModel("SURFACE_MODEL");
  return (LblSurfaceModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_MODEL_PARMS
 *
 *****************************************************************************/
int     LblSurfaceModelParms(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{
  LblSetSurfaceModel("SURFACE_MODEL_PARMS");
  return (LblSurfaceModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SET_SURFACE_MODEL
 *
 *****************************************************************************/
void     LblSetSurfaceModel(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_SURFACE_MODEL_API
 *
 *****************************************************************************/
int     LblSurfaceModelApi(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblSurfaceModel_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SURFACE_MODEL
 *
 *****************************************************************************/
void	LblPrintSurfaceModel(
  LblSurfaceModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SURFACE_MODEL
 *
 *****************************************************************************/
void	LblTestSurfaceModel(
  LblSurfaceModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
