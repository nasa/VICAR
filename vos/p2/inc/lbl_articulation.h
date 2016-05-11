#ifndef MIPS_LBL_ARTICULATION_INCLUDED
#define MIPS_LBL_ARTICULATION_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

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
 * 2015-08-19   C. Cheng        Added ArticulationDeviceTempCount
 * 2015-07-01   C. Cheng        Added GravityAcceleration
 * 2015-07-01   C. Cheng        Added ArticulationDevicePhase
 * 2003-01-13   P. Zamani       Added ArticulationDevInstrumentId
 *****************************************************************************/

#define  LBL_ART_DEV_ANGLES		10
#define  LBL_ART_DEV_COUNTS		10
#define  LBL_ART_DEV_LOCATION	10
#define  LBL_ART_DEV_ORIENT	10
typedef struct
	{
	LblApiIdItem_typ		ArticulationDeviceId;
	LblApiNameItem_typ		ArticulationDeviceName;
	LblApiRealItem_typ		ArticulationDeviceAngle[LBL_ART_DEV_ANGLES];
	LblApiNameItem_typ		ArticulationDeviceAngleName[LBL_ART_DEV_ANGLES];
	LblApiTypeItem_typ		ArticulationDeviceAngleUnit[LBL_ART_DEV_ANGLES];
	LblApiIntItem_typ		ArticulationDeviceCount[LBL_ART_DEV_COUNTS];
	LblApiNameItem_typ		ArticulationDeviceCountName[LBL_ART_DEV_COUNTS];
	LblApiTypeItem_typ		ArticulationDeviceCountUnit[LBL_ART_DEV_COUNTS];
	LblApiRealItem_typ		ArticulationDevLocation[LBL_ART_DEV_LOCATION];
	LblApiNameItem_typ		ArticulationDevLocationName[LBL_ART_DEV_LOCATION];
	LblApiTypeItem_typ		ArticulationDevLocationUnit[LBL_ART_DEV_LOCATION];
	LblApiRealItem_typ		ArticulationDevOrient[LBL_ART_DEV_ORIENT];
	LblApiNameItem_typ		ArticulationDevOrientName[LBL_ART_DEV_ORIENT];
	LblApiTypeItem_typ		ArticulationDevOrientUnit[LBL_ART_DEV_ORIENT];
	LblApiIntItem_typ		ArticulationDevPosition[LBL_POSITION_ARRAY];
	LblApiIdItem_typ		ArticulationDevPosition_Id[LBL_POSITION_ARRAY];
	LblApiNameItem_typ		ArticulationDevPositionName[LBL_POSITION_ARRAY];
	LblApiIdItem_typ		ArticulationDeviceMode;
	LblApiRealItem_typ		ArticulationDeviceTempCount[5];
	LblApiRealItem_typ		ArticulationDeviceTemp[5];
	LblApiNameItem_typ		ArticulationDeviceTempName[5];
	LblApiTypeItem_typ		ArticulationDeviceTempUnit[5];
	LblApiIdItem_typ		ArticulationDevInstrumentId;
	LblApiRealVectorItem_typ	ArticulationDevVector;
	LblApiNameItem_typ		ArticulationDevVectorName;
	LblApiIdItem_typ		ContactSensorState[8];
	LblApiNameItem_typ		ContactSensorStateName[8];
	LblApiIdItem_typ		ArticulationDeviceState[12];
	LblApiNameItem_typ		ArticulationDeviceStateName[12];
	LblApiIdItem_typ		InstrumentId;
	LblApiIdItem_typ		SolutionId;
    LblApiRealItem_typ      GravityAcceleration;
    LblApiStringItem_typ	ArticulationDevicePhase;
	} LblArticulation_typ;

/***  Function prototypes  ***/
int	LblArticulation( int, int, LblArticulation_typ *, int );
	/***  For development & internal use  ***/
int	LblArticulationApi( int, int, LblArticulation_typ *, int ,const char*);
void	LblSetArticulation( const char * );
void	LblTestArticulation( LblArticulation_typ *);
void	LblPrintArticulation( LblArticulation_typ *);

#ifdef __cplusplus
}
#endif

#endif
