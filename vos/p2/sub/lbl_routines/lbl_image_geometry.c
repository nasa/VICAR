/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_image_geometry.h"

/******************************************************************************
 *				LBL_IMAGE_GEOMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  an Image Geometry property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
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
 *  LblImageGeometry.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblImageGeometry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CENTRAL_BODY_DISTANCE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, CentralBodyDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, CentralBodyDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CentralBodyDistance.Value)},

	{"EMISSION_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, EmissionAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, EmissionAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EmissionAngle.Value)},

	{"INCIDENCE_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, IncidenceAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, IncidenceAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(IncidenceAngle.Value)},

	{"INTERCEPT_POINT_LATITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLatitude.Value)},

	{"INTERCEPT_POINT_LINE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLine.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLine.Value)},

	{"INTERCEPT_POINT_LINE_SAMPLE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLineSample.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLineSample.Value)},

	{"INTERCEPT_POINT_LONGITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLongitude.Value)},

	{"LOCAL_HOUR_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalHourAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalHourAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalHourAngle.Value)},

	{"LOCAL_MEAN_SOLAR_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalMeanSolarTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalMeanSolarTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalMeanSolarTime.Value)},

	{"LOCAL_TIME",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalTime.Value)},

	{"LOCAL_TRUE_SOLAR_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalTrueSolarTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalTrueSolarTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalTrueSolarTime.Value)},

	{"NTV_SAT_TIME_FROM_CLOSEST_APRH",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, NtvSatTimeFromClosestAprh.Value),
		LBL_OFFSET(LblImageGeometry_typ, NtvSatTimeFromClosestAprh.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NtvSatTimeFromClosestAprh.Value)},

	{"NTV_TIME_FROM_CLOSEST_APPROACH",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, NtvTimeFromClosestApproach.Value),
		LBL_OFFSET(LblImageGeometry_typ, NtvTimeFromClosestApproach.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NtvTimeFromClosestApproach.Value)},

	{"PHASE_ANGLE",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, PhaseAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, PhaseAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PhaseAngle.Value)},

	{"SOLAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuth.Value)},

	{"SOLAR_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarDistance.Value)},

	{"SOLAR_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarElevation.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevation.Value)},

	{"SOLAR_LATITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLatitude.Value)},

	{"SOLAR_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLine.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLine.Value)},

	{"SOLAR_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLineSample.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLine.Value)},

	{"SOLAR_LONGITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLongitude.Value)},

	{"SPACECRAFT_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SpacecraftDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, SpacecraftDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpacecraftDistance.Value)},

	{"SUB_SPACECRAFT_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftAzimuth.Value)},

	{"SUB_SPACECRAFT_LATITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftLatitude.Value)},

	{"SUB_SPACECRAFT_LONGITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftLongitude.Value)},

	{"SURCAFE_FIXED_SOLAR_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedSolarAzimuth.Value)},

	{"SURFACE_FIXED_SOLAR_ELEVATION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarElevation.Value),
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedSolarElevation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "IMAGE_GEOMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_IMAGE_GEOMETRY
 *
 *****************************************************************************/
int     LblImageGeometry(
  int   Unit,
  int   Obtain,
  LblImageGeometry_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblImageGeometry_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_GEOMETRY
 *
 *****************************************************************************/
void	LblPrintImageGeometry(
  LblImageGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_GEOMETRY
 *
 *****************************************************************************/
void	LblTestImageGeometry(
  LblImageGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
