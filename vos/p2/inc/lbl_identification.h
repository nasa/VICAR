#ifndef MIPS_LBL_IDENTIFIER_INCLUDED
#define MIPS_LBL_IDENTIFIER_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

  /**  Copyright (c) 1995, California Institute of Technology             **/
  /**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

#define  LBL_RMC_ITEMS	10
#define  LBL_SOURCE_PROD_ID_ITEMS	3

  /******************************************************************************
   *				LBL_IDENTIFICATION
   *
   *	This module contains routines to help create, read/write and print
   *  an Identification property label.  It is part of the MIPL label API package,
   *  using a lower-level label processor to do the real work.  This package
   *  basically defines a table that the lower-level routines use.  The table
   *  is the bridge between how the application access the label elements, and
   *  how the label processor specifies the label components to the VICAR label
   *  Run Time Library (RTL).
   *
   *	The primary routine used by a typical application program is
   *  LblIdentification.  This routine requires exactly 4 parameters.
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
   *
   * History of modifications:
   *
   * Date         who             Description
   * -----------  --------------- ----------------------------------------------
   * 7-July-2008  Hyun Lee        Added RequestId
   * ?            Alice Stanboli  Added OpsToken.....
   * 23-May-2003  Hyun Lee        Changed InstrumentHostId & InstrumentId as 
   *                              array values.
   * 14-Feb-2003	Payam Zamani	Changed PlanetDayNumber to integer
   * 12-Feb-2003	Payam Zamani	Added ReleaseId
   * 11-Feb-2003  Payam Zamani    Moved GeometryProjectionType from
   *		                lbl_identification.h
   * ?            Allan Runkle    Original development and release
   *****************************************************************************/

  typedef struct
  {
    LblApiIntItem_typ		CommandSequenceNumber;
    LblApiIdItem_typ		DataSetId;
    LblApiLongNameItem_typ	DataSetName;
    LblApiNameItem_typ		FeatureName;
    LblApiTypeItem_typ		FeatureType;
    LblApiIdItem_typ		FrameId[LBL_MAX_ITEMS];
    LblApiTypeItem_typ		FrameType;
    LblApiTypeItem_typ		GeometryProjectionType;
    LblApiIdItem_typ		ImageId;
    LblApiTimeItem_typ		ImageTime;
    LblApiTypeItem_typ		ImageType;
    LblApiIdItem_typ		InstrumentHostId[LBL_MAX_ITEMS];
    LblApiNameItem_typ		InstrumentHostName[LBL_MAX_ITEMS];
    LblApiIdItem_typ		InstrumentId[LBL_MAX_ITEMS];
    LblApiNameItem_typ		InstrumentName[LBL_MAX_ITEMS];
    LblApiIntItem_typ   	InstrumentSerialNumber;
    LblApiTypeItem_typ		InstrumentType[LBL_MAX_ITEMS];
    LblApiIdItem_typ		InstrumentVersionId;
    LblApiTimeItem_typ      LocalMeanSolarTime;
    LblApiTimeItem_typ		LocalTrueSolarTime;
    LblApiIdItem_typ		MagnetId;
    LblApiIdItem_typ		MeasurementId;
    LblApiTimeItem_typ		MeasurementTime;
    LblApiTypeItem_typ		MeasurementType;
    LblApiNameItem_typ		MissionName[LBL_MAX_ITEMS];
    LblApiNameItem_typ		MissionPhaseName;
    LblApiIdItem_typ		ObservationId;
    LblApiNameItem_typ		ObservationName;
    LblApiTimeItem_typ		ObservationTime;
    LblApiTypeItem_typ		ObservationType;
    LblApiRealItem_typ		OrbitNumber;
    LblApiIntItem_typ		PlanetDayNumber;
    LblApiIntItem_typ		LocalTrueSolarTimeSol;
    LblApiStringItem_typ    ProcessingHistoryText;
    LblApiNameItem_typ		ProducerFullName;
    LblApiIdItem_typ		ProducerId;
    LblApiLongNameItem_typ	ProducerInstitutionName;
    LblApiTimeItem_typ		ProductCreationTime;
    LblApiIdItem_typ		ProductId;
    LblApiIdItem_typ		ProductVersionId;
    LblApiIdItem_typ		ReleaseId;
    LblApiIdItem_typ        RequestId;
    LblApiIntItem_typ		RoverMotionCounter[LBL_RMC_ITEMS];
    LblApiNameItem_typ		RoverMotionCounterName[LBL_RMC_ITEMS];
    LblApiIdItem_typ		SequenceId;
    LblApiNameItem_typ		SequenceName;
    LblApiIdItem_typ		SequenceVersionId;
    LblApiRealItem_typ		SolarLongitude;
    LblApiIdItem_typ		SourceProductId[LBL_SOURCE_PROD_ID_ITEMS];
    LblApiIntItem_typ		SpacecraftClockCntPartition;
    LblApiTimeItem_typ		SpacecraftClockStartCount;
    LblApiTimeItem_typ		SpacecraftClockStopCount;
    LblApiTimeItem_typ		StartTime;
    LblApiTimeItem_typ		StopTime;
    LblApiNameItem_typ		TargetName;
    LblApiTypeItem_typ		TargetType;
    LblApiNameItem_typ      OpsToken;
    LblApiNameItem_typ      OpsTokenActivity;
    LblApiNameItem_typ      OpsTokenCommand;
    LblApiNameItem_typ      OpsTokenPayload;
    LblApiStringItem_typ    ImageAcquireMode;
    LblApiStringItem_typ    ActiveFlightStringId;
    LblApiTimeItem_typ      SourceSCClockStartCnt;
    LblApiTimeItem_typ      SourceSCClockStopCnt;
    LblApiTimeItem_typ      SourceStartTime;
  } LblIdentification_typ;


  int	LblIdentification( int, int, LblIdentification_typ *, int );
  void	LblPrintIdentification( LblIdentification_typ *);
  void	LblTestIdentification( LblIdentification_typ *);

#ifdef __cplusplus
}
#endif


#endif

