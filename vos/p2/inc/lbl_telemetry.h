#ifndef LBL_TELEMETRY_INCLUDED
#define LBL_TELEMETRY_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

  /**  Copyright (c) 1995, California Institute of Technology             **/
  /**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"
  //#include "lbl_telemetry.h"

  /******************************************************************************
   *				LBL_TELEMETRY
   *
   *	This module contains routines to help create, read/write and print
   *  a Telemetry property label.  It is part of the MIPL label API package,
   *  using a lower-level label processor to do the real work.  This package
   *  basically defines a table that the lower-level routines use.  The table
   *  is the bridge between how the application access the label elements, and
   *  how the label processor specifies the label components to the VICAR label
   *  Run Time Library (RTL).
   *
   *	The primary routine used by a typical application program is
   *  LblTelemetry.  This routine requires exactly 4 parameters.
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
   * 2015-07-02   C. Cheng        Added ERROR_PIXEL_LINE and ERROR_PIXEL_SAMPLE
   * ============ =============== =============================================
   *****************************************************************************/

  typedef struct
  {
    LblApiIntItem_typ	   ApplicationPacketId;
    LblApiLongNameItem_typ  ApplicationPacketName;
    LblApiIntItem_typ		   ApplicationProcessId;
    LblApiLongNameItem_typ             ApplicationProcessName;
    LblApiIntItem_typ   ApplicationProcessSubtypeId;
    LblApiTimeItem_typ		   EarthReceivedStartTime;
    LblApiTimeItem_typ		   EarthReceivedStopTime;
    LblApiIntItem_typ		   ExpectedPackets;
    LblApiStringItem_typ	   PacketMapMask;
    LblApiIntItem_typ		   ReceivedPackets;
    LblApiIdItem_typ	   SpiceFileId[5];
    LblApiStringItem_typ	   SpiceFileName[5];
    LblApiIdItem_typ	   TelemetryProviderId;
    LblApiNameItem_typ	   TelemetrySourceName;
    LblApiTypeItem_typ	   TelemetryProviderType;
    LblApiStringItem_typ	   TelemetrySourceType;
    LblApiIntItem_typ              ChannelId;
    LblApiIdItem_typ              CommunicationSessionId;
    LblApiIntItem_typ       DownloadPriority;
    LblApiStringItem_typ           ExpectedTransmissionPath;
    LblApiStringItem_typ              FlightSoftwareMode;
    LblApiIdItem_typ              FlightSoftwareVersionId;
    LblApiTimeItem_typ	   PacketCreationSclk;
    LblApiIntItem_typ       PacketSequenceNumber;
    LblApiStringItem_typ           ProductCompletionStatus;
    LblApiStringItem_typ           ProductTag;
    LblApiIntItem_typ              SequenceExecutionCnt;
    LblApiStringItem_typ           RoverComputeElement;
    LblApiIntItem_typ              TelemetrySourceSize;
    LblApiIntItem_typ              SourceProductBytes;
    LblApiIntItem_typ              TelemetrySourceProductCheckSum;
    LblApiIntItem_typ              SourceProductCheckSum;
    LblApiTimeItem_typ             TelemetrySourceSCClockStartCnt;
    LblApiTimeItem_typ             SourceSCClockStartCnt;
    LblApiTimeItem_typ             SourceSCClockStopCnt;
    LblApiTimeItem_typ             SourceStartTime;
    LblApiTimeItem_typ             TelemetrySourceStartTime;
    LblApiNameItem_typ	   SoftwareName;
    LblApiTypeItem_typ	   SoftwareVersionId;
    LblApiFlagItem_typ		   TlmCmdDiscrepancyFlag;
    LblApiIntItem_typ              TotalParts;
    LblApiIntItem_typ              TransmissionIndicator;
    LblApiStringItem_typ              AutoDeleteFlag;
    LblApiStringItem_typ           TransmissionPath;
    LblApiIntItem_typ              Stripes;
    LblApiIntItem_typ              StripingCount;
    LblApiStringItem_typ           TelemetrySourceHostName;
    LblApiStringItem_typ              VirtualChannelId;
    LblApiIntItem_typ              ImageDataSize;
    LblApiIntItem_typ              ErrorPixelLine;
    LblApiIntItem_typ              ErrorPixelSample;
    //LblApiIntItem_typ
  } LblTelemetry_typ;

  int	LblTelemetry( int, int, LblTelemetry_typ *, int );
  void	LblTestTelemetry( LblTelemetry_typ *);
  void	LblPrintTelemetry( LblTelemetry_typ *);

#ifdef __cplusplus
}
#endif

#endif
