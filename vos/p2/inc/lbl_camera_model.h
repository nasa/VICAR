#ifndef MIPS_LBL_CAMERA_MODEL_INCLUDED
#define MIPS_LBL_CAMERA_MODEL_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_CAMERA_MODEL
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Camera Model label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The primary routine used by a typical application program is
 *  LblCameraModel.  This routine requires exactly 4 parameters.
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
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * 14-May-2003  Hyun Lee        Added ReferenceCoordSystemSolnId
 * 11-Feb-2003  Payam Zamani    Added FilterName
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_CAM_MODEL_ELEMENTS		9
typedef struct
	{
	LblApiIdItem_typ		CalibrationSourceId;
	LblApiIdItem_typ		GeometrySourceId;
	LblApiIdItem_typ		FilterName;
	LblApiDescItem_typ		ModelDesc;
	LblApiNameItem_typ		ModelName;
	LblApiTypeItem_typ		ModelType;
	LblApiFlagItem_typ		ModelComponentId[LBL_CAM_MODEL_ELEMENTS];
	LblApiNameItem_typ		ModelComponentName[LBL_CAM_MODEL_ELEMENTS];
	LblApiTypeItem_typ		ModelComponentUnit[LBL_CAM_MODEL_ELEMENTS];
	LblApiRealItem_typ		ModelComponent1[3];
	LblApiRealItem_typ		ModelComponent2[3];
	LblApiRealItem_typ		ModelComponent3[3];
	LblApiRealItem_typ		ModelComponent4[3];
	LblApiRealItem_typ		ModelComponent5[3];
	LblApiRealItem_typ		ModelComponent6[3];
	LblApiRealItem_typ		ModelComponent7[3];
	LblApiRealItem_typ		ModelComponent8[3];
	LblApiRealItem_typ		ModelComponent9[3];
	LblApiRealVectorItem_typ	ModelTransformVector;
	LblApiQuaternionItem_typ	ModelTransformQuaternion;
	LblApiIntItem_typ		ReferenceCoordSystemIndex[LBL_COORD_SYS_INDEX];
	LblApiNameItem_typ		ReferenceCoordSystemName;
	LblApiIdItem_typ		ReferenceCoordSystemSolnId;
	LblApiIdItem_typ		SolutionId;
	LblApiTypeItem_typ		InterpolationMethod;
	LblApiRealItem_typ		InterpolationValue;
	} LblCameraModel_typ;

/***  Function prototypes  ***/
int	LblCameraModel( int, int, LblCameraModel_typ *, int );
int	LblGeometricCameraModel( int, int, LblCameraModel_typ *, int );
	/***  For development & internal use  ***/
int	LblCameraModelApi( int, int, LblCameraModel_typ *, int );
void	LblSetCameraModel( const char *);
void	LblTestCameraModel( LblCameraModel_typ *);
void	LblPrintCameraModel( LblCameraModel_typ *);

#ifdef __cplusplus
}
#endif

#endif
