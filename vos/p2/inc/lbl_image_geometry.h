#ifndef MIPS_LBL_IMAGE_GEOMETRY_INCLUDED
#define MIPS_LBL_IMAGE_GEOMETRY_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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

typedef struct
	{
	LblApiRealItem_typ		CentralBodyDistance;
	LblApiRealItem_typ		EmissionAngle;
	LblApiRealItem_typ		IncidenceAngle;
	LblApiRealItem_typ		InterceptPointLatitude;
	LblApiRealItem_typ		InterceptPointLine;
	LblApiRealItem_typ		InterceptPointLineSample;
	LblApiRealItem_typ		InterceptPointLongitude;
	LblApiRealItem_typ		LocalHourAngle;
	LblApiTimeItem_typ		LocalMeanSolarTime;
	LblApiRealItem_typ		LocalTime;
	LblApiTimeItem_typ		LocalTrueSolarTime;
	LblApiTimeItem_typ		NtvSatTimeFromClosestAprh;
	LblApiTimeItem_typ		NtvTimeFromClosestApproach;
	LblApiRealItem_typ		PhaseAngle;
	LblApiRealItem_typ		SolarAzimuth;
	LblApiRealItem_typ		SolarDistance;
	LblApiRealItem_typ		SolarElevation;
	LblApiRealItem_typ		SolarLatitude;
	LblApiIntItem_typ		SolarLine;
	LblApiIntItem_typ		SolarLineSample;
	LblApiRealItem_typ		SolarLongitude;
	LblApiRealItem_typ		SpacecraftDistance;
	LblApiRealItem_typ		SubSpacecraftAzimuth;
	LblApiRealItem_typ		SubSpacecraftLatitude;
	LblApiRealItem_typ		SubSpacecraftLongitude;
	LblApiRealItem_typ		SurfaceFixedSolarAzimuth;
	LblApiRealItem_typ		SurfaceFixedSolarElevation;
	} LblImageGeometry_typ;

int	LblImageGeometry( int, int, LblImageGeometry_typ *, int );
void	LblTestImageGeometry( LblImageGeometry_typ *);
void	LblPrintImageGeometry( LblImageGeometry_typ *);

#ifdef __cplusplus
}
#endif

#endif
