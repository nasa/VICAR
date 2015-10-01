#ifndef MIPS_LBL_DERIVED_GEOMETRY_INCLUDED
#define MIPS_LBL_DERIVED_GEOMETRY_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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
 *	The primary routine used by a typical application program is
 *  LblDerivedGeometry.  This routine requires exactly 4 parameters.
 *  All label API routines  have the same four parameters:
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
 *	History of modifications:
 *
 * Date		Who		Description
 * -----------  --------------- ---------------------------------------------
 * 2003-01-08	P. Zamani	Changed type of ReferenceCoordSystemIndex to
 *                              LblApiIntItem_typ
 *****************************************************************************/

typedef struct
	{
	LblApiIdItem_typ		    SolutionId;
	LblApiNameItem_typ		    CoordinateSystemName;
	LblApiDescItem_typ		    DerivedGeometryDesc;
	LblApiNameItem_typ		    DerivedGeometryName;
	LblApiDescItem_typ		    DerivedGeometryNote;
	LblApiTypeItem_typ		    DerivedGeometryType;
	LblApiTypeItem_typ		    InstrumentAzimuthUnit;
	LblApiTypeItem_typ		    InstrumentElevationUnit;
	LblApiRealItem_typ		    LanderInstrumentAzimuth;
	LblApiRealItem_typ		    LanderInstrumentElevation;
	LblApiQuaternionItem_typ	LanderLocalLevelQuaternion;
	LblApiRealVectorItem_typ	LclLvlSrfcFxdVector;
	LblApiRealItem_typ		    LocalLevelInstAzimuth;
	LblApiRealItem_typ		    LocalLevelInstElevation;
	LblApiRealItem_typ		    NorthAzimuth;
	LblApiIdItem_typ		    PositiveAzimuthDirection;
	LblApiIdItem_typ		    PositiveElevationDirection;
	LblApiIntItem_typ		    ReferenceCoordSystemIndex[LBL_COORD_SYS_INDEX];
	LblApiNameItem_typ		    ReferenceCoordSystemName;
	LblApiIdItem_typ		    ReferenceCoordSystemSolnId;
	LblApiRealItem_typ		    SlantDistance;
	LblApiRealItem_typ		    SmearAzimuth;
	LblApiRealItem_typ		    SmearMagnitude;
	LblApiRealItem_typ		    SolarAzimuth;
	LblApiTypeItem_typ		    SolarAzimuthUnit;
	LblApiRealItem_typ		    SolarElevation;
	LblApiTypeItem_typ		    SolarElevationUnit;
	LblApiRealVectorItem_typ    SunViewDirection;
	LblApiRealVectorItem_typ	SrfcFxdLclLvlVector;
	LblApiRealItem_typ		    StartAzimuth;
	LblApiTypeItem_typ		    StartAzimuthUnit;
	LblApiRealItem_typ		    StopAzimuth;
	LblApiTypeItem_typ		    StopAzimuthUnit;
	LblApiRealItem_typ		    SurfaceFixedInstAzimuth;
	LblApiRealItem_typ		    SurfaceFixedInstElevation;
	} LblDerivedGeometry_typ;

/***  Function Prototypes  ***/
int	LblDerivedGeometry( int, int, LblDerivedGeometry_typ *, int );
int	LblDerivedGeometryParms( int, int, LblDerivedGeometry_typ *, int );
	/***  For development & internal use  ***/
int	LblDerivedGeometryApi( int, int, LblDerivedGeometry_typ *, int ,const char*);
void	LblSetDerivedGeometry( const char * );
void	LblTestDerivedGeometry( LblDerivedGeometry_typ *);
void	LblPrintDerivedGeometry( LblDerivedGeometry_typ *);

#ifdef __cplusplus
}
#endif

#endif
