/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_derived_geometry.h"

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Geometry label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_derived_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblDerivedGeometry.  This routine requires exactly 4 parameters.
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
 * 2015-08-19   C. Cheng        Added SiteInstrumentAzimuth and SiteInstrumentElevation
 * 2003-01-07   P. Zamani       Changed DERIVED_GEOMETRY_NAME to be
 *				  LBL_OPTIONAL
 * ?            A. Runkle       Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblDerivedGeometry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolutionId.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolutionId.Valid),
		LBL_NO_RETURN,  LBL_SIZE(SolutionId.Value)},

	{"COORDINATE_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, CoordinateSystemName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, CoordinateSystemName.Valid),
		LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemName.Value)},

	{"DERIVED_GEOMETRY_DESC",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryDesc.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryDesc.Value)},

	{"DERIVED_GEOMETRY_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryName.Value)},

	{"DERIVED_GEOMETRY_NOTE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryNote.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryNote.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryNote.Value)},

	{"DERIVED_GEOMETRY_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryType.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryType.Value)},

	{"INSTRUMENT_AZIMUTH__UNIT",
		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentAzimuthUnit.Value)},

	{"INSTRUMENT_ELEVATION__UNIT",
		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentElevationUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentElevationUnit.Value)},

	{"LANDER_INSTRUMENT_AZIMUTH, INSTRUMENT_AZIMUTH",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderInstrumentAzimuth.Value)},

	{"LANDER_INSTRUMENT_ELEVATION, INSTRUMENT_ELEVATION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderInstrumentElevation.Value)},

	{"LANDER_LOCAL_LEVEL_QUATERNION, INST_HOST_TO_FIXED_QUATERNION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderLocalLevelQuaternion.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderLocalLevelQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderLocalLevelQuaternion.Value)},

	{"LCL_LVL_SRFC_FXD_VECTOR, LOCAL_TO_FIXED_OFFSET_VECTOR",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LclLvlSrfcFxdVector.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LclLvlSrfcFxdVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LclLvlSrfcFxdVector.Value)},

	{"LOCAL_LEVEL_INST_AZIMUTH, LOCAL_INSTRUMENT_AZIMUTH",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalLevelInstAzimuth.Value)},

	{"LOCAL_LEVEL_INST_ELEVATION, LOCAL_INSTRUMENT_ELEVATION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalLevelInstElevation.Value)},

	{"NORTH_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, NorthAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, NorthAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NorthAzimuth.Value)},

	{"POSITIVE_AZIMUTH_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveAzimuthDirection.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveAzimuthDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveAzimuthDirection.Value)},

	{"POSITIVE_ELEVATION_DIRECTION",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveElevationDirection.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveElevationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveElevationDirection.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  7,  LBL_NULL,
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[6].Value),
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  8,  LBL_NULL,
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[7].Value),
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  9,  LBL_NULL,
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[8].Value),
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

    {"REFERENCE_COORD_SYSTEM_INDEX",    "INT",      LBL_OPTIONAL,
        LBL_CONTINUE,   1,  10,  LBL_NULL,
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[9].Value),
        LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SITE_INSTRUMENT_AZIMUTH, INSTRUMENT_AZIMUTH",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SiteInstrumentAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SiteInstrumentAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SiteInstrumentAzimuth.Value)},

	{"SITE_INSTRUMENT_ELEVATION, INSTRUMENT_ELEVATION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SiteInstrumentElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SiteInstrumentElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SiteInstrumentElevation.Value)},

	{"SLANT_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SlantDistance.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SlantDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SlantDistance.Value)},

	{"SMEAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SmearAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SmearAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SmearAzimuth.Value)},

	{"SMEAR_MAGNITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SmearMagnitude.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SmearMagnitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SmearMagnitude.Value)},

	{"SOLAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuth.Value)},

	{"SOLAR_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuthUnit.Value)},

	{"SOLAR_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevation.Value)},

	{"SOLAR_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevationUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevationUnit.Value)},

    {"SUN_VIEW_DIRECTION",        "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    3,  1,  LBL_NULL,
        LBL_OFFSET(LblDerivedGeometry_typ, SunViewDirection.Value),
        LBL_OFFSET(LblDerivedGeometry_typ, SunViewDirection.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunViewDirection.Value)},

	{"SRFC_FXD_LCL_LVL_VECTOR,",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SrfcFxdLclLvlVector.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SrfcFxdLclLvlVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SrfcFxdLclLvlVector.Value)},

	{"START_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, StartAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, StartAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuth.Value)},

	{"START_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, StartAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, StartAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuthUnit.Value)},

	{"STOP_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, StopAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, StopAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuth.Value)},

	{"STOP_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, StopAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, StopAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuthUnit.Value)},

	{"SURFACE_FIXED_INST_AZIMUTH, FIXED_INSTRUMENT_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedInstAzimuth.Value)},

	{"SURFACE_FIXED_INST_ELEVATION, FIXED_INSTRUMENT_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedInstElevation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "DERIVED_GEOMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void     LblSetDerivedGeometry(
  const char    *Name )
{
  if (Name!=NULL)
    Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY
 *
 *****************************************************************************/
int     LblDerivedGeometry(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{
  LblSetDerivedGeometry("DERIVED_GEOMETRY");
  return (LblDerivedGeometryApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY_PARMS
 *
 *****************************************************************************/
int     LblDerivedGeometryParms(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{
  LblSetDerivedGeometry("DERIVED_GEOMETRY_PARMS");
  return (LblDerivedGeometryApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY_API
 *
 *****************************************************************************/
int     LblDerivedGeometryApi(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  LblSetDerivedGeometry(propertyName);

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblDerivedGeometry_typ);


  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	LblPrintDerivedGeometry(
  LblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	LblTestDerivedGeometry(
  LblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
