#ifndef MIPS_LBL_MINI_HEADER_INCLUDED
#define MIPS_LBL_MINI_HEADER_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

  /**  Copyright (c) 2002, California Institute of Technology             **/
  /**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

  /******************************************************************************
   *				LBL_MINI_HEADER
   *
   *	This module contains routines to help create, read/write and print
   *  a MiniHeader property label.  It is part of the MIPL label API
   *  package, using a lower-level label processor to do the real work.  This
   *  package basically defines a table that the lower-level routines use.
   *  The table is the bridge between how the application access the label
   *  elements, and how the label processor specifies the label components
   *  to the VICAR label Run Time Library (RTL).
   *
   *	The primary routine used by a typical application program is
   *  LblMiniHeader.  This routine requires exactly 4 parameters.
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
   * History of modifications:
   * ------------------------ 
   * Date         Who             Description
   * -----------  --------------- ---------------------------------------------
   *****************************************************************************/
#define MINI_HEADER_PARMS_PROPERTY_NAME   "MINI_HEADER"

  typedef struct
  {
    LblApiIntItem_typ           ImageId;
    LblApiStringItem_typ	MagicNumbers[2];
    LblApiIntItem_typ           SpacecraftClockStartCount;
    LblApiIntItem_typ           DetectorEraseCount;
    LblApiStringItem_typ	InstrumentModeId;
    LblApiIntItem_typ           FilterNumber;
    LblApiIntItem_typ           ExposureDurationCount;
    LblApiIntItem_typ           FirstLineSample;
    LblApiIntItem_typ           FirstLine;
    LblApiIntItem_typ           LineSamples;
    LblApiIntItem_typ           Lines;
    LblApiStringItem_typ	InstrumentFocusMode;
    LblApiIntItem_typ           InstrumentFocusPosition;
    LblApiIntItem_typ           InstrumentFocusStepSize;
    LblApiIntItem_typ           InstrumentFocusSteps;
    LblApiStringItem_typ        ExposureType;
    LblApiIntItem_typ           AutoExposureDataCut;
    LblApiIntItem_typ           AutoExposurePixelFraction;
    LblApiIntItem_typ           AutoExposurePercent;
    LblApiIntItem_typ           MaxAutoExposIterationCount;
    LblApiIntItem_typ		InstCmprsMode;
    LblApiIntItem_typ           InstCmprsQuality;
    LblApiStringItem_typ	SampleBitModeId;
    LblApiIntItem_typ           StartImageId;
    LblApiIntItem_typ           ExposureCount;
    LblApiStringItem_typ	ImageBlendingFlag;
    LblApiStringItem_typ	ImageRegistrationFlag;
    LblApiStringItem_typ	InstrumentState[8];
    LblApiStringItem_typ	InstrumentStateName[8];
    LblApiIntItem_typ           InstrumentSerialNumber;
    LblApiIntItem_typ           ArticulationDevPosition[2];
    LblApiStringItem_typ	ArticulationDevPositionName[8];
    LblApiIntItem_typ           OffsetModeId;
    LblApiIntItem_typ           InitialSize;
    LblApiIntItem_typ		InstrumentMode[12];	// new for MSAM
    LblApiStringItem_typ	InstrumentModeName[12];	// new for MSAM
  } LblMiniHeader_typ;

  int	LblMiniHeader( int, int, LblMiniHeader_typ *, int,const char*  );
  void	LblTestMiniHeader( LblMiniHeader_typ *);
  void	LblPrintMiniHeader( LblMiniHeader_typ *);

#ifdef __cplusplus
}
#endif

#endif
