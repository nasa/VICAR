/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_surface_projection.h"

/******************************************************************************
 *				LBL_SURFACE_PROJECTION
 *
 *	This module contains routines to help create, read/write and print
 *  a Surface Projection property label.  It is part of the MIPL label API
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
 *  LblSurfaceProjection.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblSurfaceProjection_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CAMERA_ROTATION_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CameraRotationAxisVector.Value[0])},

	{"CAMERA_ROTATION_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CameraRotationAxisVector.Value[1])},

	{"CAMERA_ROTATION_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, CameraRotationAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CameraRotationAxisVector.Value[2])},

	{"LINE_CAMERA_MODEL_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineCameraModelOffset.Value)},

	{"LINE_CAMERA_MODEL_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineCameraModelOffsetUnit.Value)},

	{"LINE_PROJECTION_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineProjectionOffset.Value)},

	{"LINE_PROJECTION_OFFSET__UNIT",	"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineProjectionOffsetUnit.Value)},

	{"MAP_PROJECTION_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionDesc.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionDesc.Value)},

	{"MAP_PROJECTION_NOTE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionNote.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionNote.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionNote.Value)},

	{"MAP_PROJECTION_TYPE",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionType.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionType.Value)},

	{"MAP_RESOLUTION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	2,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolution.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolution.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolution.Value)},

	{"MAP_RESOLUTION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolutionUnit[0].Value)},

	{"MAP_RESOLUTION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolutionUnit[1].Value)},

	{"MAP_SCALE",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScale[0].Value)},

	{"MAP_SCALE",				"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScale[1].Value)},

	{"MAP_SCALE__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScaleUnit[0].Value)},

	{"MAP_SCALE__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScaleUnit[1].Value)},

	{"MAXIMUM_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaximumElevation.Value)},

	{"MAXIMUM_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaximumElevationUnit.Value)},

	{"MINIMUM_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MinimumElevation.Value)},

	{"MINIMUM_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MinimumElevationUnit.Value)},

	{"PROJECTION_AXIS_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAxisOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAxisOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAxisOffset.Value)},

	{"PROJECTION_AXIS_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAxisOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAxisOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAxisOffsetUnit.Value)},

	{"PROJECTION_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAzimuth.Value)},

	{"PROJECTION_AZIMUTH__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAzimuthUnit.Value)},

	{"PROJECTION_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevation.Value)},

	{"PROJECTION_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationUnit.Value)},

	{"PROJECTION_ELEVATION_LINE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLine.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationLine.Value)},

	{"PROJECTION_ELEVATION_LINE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLineUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLineUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationLineUnit.Value)},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[0])},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[1])},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[2])},

	{"PROJECTION_ORIGIN_VECTOR__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVectorUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVectorUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVectorUnit.Value)},

	{"PROJECTION_X_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionXAxisVector.Value[0])},

	{"PROJECTION_X_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionXAxisVector.Value[1])},

	{"PROJECTION_X_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionXAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionXAxisVector.Value[2])},

	{"PROJECTION_Y_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionYAxisVector.Value[0])},

	{"PROJECTION_Y_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionYAxisVector.Value[1])},

	{"PROJECTION_Y_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionYAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionYAxisVector.Value[2])},

	{"PROJECTION_Z_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionZAxisVector.Value[0])},

	{"PROJECTION_Z_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionZAxisVector.Value[1])},

	{"PROJECTION_Z_AXIS_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionZAxisVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionZAxisVector.Value[2])},

	{"REFERENCE_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceAzimuth.Value)},

	{"REFERENCE_AZIMUTH__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceAzimuthUnit.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[6].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[6].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[7].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[7].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[8].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[8].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[9].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[9].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SAMPLE_CAMERA_MODEL_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleCameraModelOffset.Value)},

	{"SAMPLE_CAMERA_MODEL_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleCameraModelOffsetUnit.Value)},

	{"SAMPLE_PROJECTION_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleProjectionOffset.Value)},

	{"SAMPLE_PROJECTION_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleProjectionOffsetUnit.Value)},

	{"START_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuth.Value)},

	{"START_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuthUnit.Value)},

	{"STOP_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuth.Value)},

	{"STOP_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuthUnit.Value)},

	{"SURFACE_GEOMETRY_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SurfaceGeometryId.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SurfaceGeometryId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGeometryId.Value)},

	{"X_AXIS_MAXIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMaximum.Value)},

	{"X_AXIS_MAXIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMaximumUnit.Value)},

	{"X_AXIS_MINIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMinimum.Value)},

	{"X_AXIS_MINIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMinimumUnit.Value)},

	{"Y_AXIS_MAXIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMaximum.Value)},

	{"Y_AXIS_MAXIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMaximumUnit.Value)},

	{"Y_AXIS_MINIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMinimum.Value)},

	{"Y_AXIS_MINIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMinimumUnit.Value)},

	{"ZERO_ELEVATION_LINE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLine.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ZeroElevationLine.Value)},

	{"ZERO_ELEVATION_LINE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLineUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLineUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ZeroElevationLineUnit.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_SURFACE_PROJECTION
 *
 *****************************************************************************/
void     LblSetSurfaceProjection(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION
 *
 *****************************************************************************/
int     LblSurfaceProjection(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{
  LblSetSurfaceProjection("SURFACE_PROJECTION");
  return (LblSurfaceProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION_PARMS
 *
 *****************************************************************************/
int     LblSurfaceProjectionParms(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{
  LblSetSurfaceProjection("SURFACE_PROJECTION_PARMS");
  return (LblSurfaceProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION_API
 *
 *****************************************************************************/
int     LblSurfaceProjectionApi(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblSurfaceProjection_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SURFACE_PROJECTION
 *
 *****************************************************************************/
void	LblPrintSurfaceProjection(
  LblSurfaceProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SURFACE_PROJECTION
 *
 *****************************************************************************/
void	LblTestSurfaceProjection(
  LblSurfaceProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
