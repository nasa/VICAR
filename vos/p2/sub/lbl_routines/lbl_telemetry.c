/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_telemetry.h"

/******************************************************************************
 *              LBL_TELEMETRY
 *
 *  This module contains routines to help create, read/write and print
 *  a Telemetry property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *  The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_telemetry.h".  The
 *  implementation supporting the interface is this module.
 *
 *  The primary routine used by a typical application program is
 *  LblTelemetry.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *      INT VICAR RTL unit number of an opened image file.
 *          This is the file where the label will be read or
 *          written.  It must be open with the appropriate
 *          I/O mode
 *      INT Read/Write flag.  If the value of this parameter is
 *          non-zero, the label will be read from the file.  If
 *          the value of the parameter is zero, a new label will
 *          be written to the file.
 *      VOID*   The structure that an application program will use
 *          to set or retreive the label element values.  Okay
 *          this really isn't a VOID*, but it is a pointer to
 *          the label specific structure.
 *      INT The instance of this label type.  They typical value
 *          of this parameter should be '1'.
 *
 *  The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *  All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2015-07-02   C. Cheng        Added ERROR_PIXEL_LINE and EROR_PIXEL_SAMPLE
 * 2008-07-07   H. Lee
 *****************************************************************************/

#define  LBL_SIZE(x)    sizeof(((LblTelemetry_typ *)0)->x)

static LblApiElement_typ    LabelTbl[] = {

    {"APPLICATION_PACKET_ID",       "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Value),
     LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ApplicationPacketId.Value)},

    {"APPLICATION_PACKET_NAME",     "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Value),
     LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Valid),
     LBL_NO_RETURN, LBL_SIZE(ApplicationPacketName.Value)},

    {"APPLICATION_PROCESS_ID",      "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Value),
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ApplicationProcessId.Value)},

    {"APPLICATION_PROCESS_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Value),
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Valid),
     LBL_NO_RETURN, LBL_SIZE(ApplicationProcessName.Value)},

    {"APPLICATION_PROCESS_SUBTYPE_ID",  "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Value),
     LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ApplicationProcessSubtypeId.Value)},

    {"EARTH_RECEIVED_START_TIME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Value),
     LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(EarthReceivedStartTime.Value)},

    {"EARTH_RECEIVED_STOP_TIME",        "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Value),
     LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(EarthReceivedStopTime.Value)},

    {"EXPECTED_PACKETS",            "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Value),
     LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Valid),
     LBL_NO_RETURN, LBL_SIZE(ExpectedPackets.Value)},

    {"PACKET_MAP_MASK",         "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Value),
     LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Valid),
     LBL_NO_RETURN, LBL_SIZE(PacketMapMask.Value)},

    {"RECEIVED_PACKETS",            "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Value),
     LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Valid),
     LBL_NO_RETURN, LBL_SIZE(ReceivedPackets.Value)},

    {"SPICE_FILE_ID",           "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileId[0].Value)},

    {"SPICE_FILE_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileId[1].Value)},

    {"SPICE_FILE_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileId[2].Value)},

    {"SPICE_FILE_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileId[3].Value)},

    {"SPICE_FILE_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileId[4].Value)},

    {"SPICE_FILE_NAME",         "STRING",   LBL_REQUIRED,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileName[0].Value)},

    {"SPICE_FILE_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileName[1].Value)},

    {"SPICE_FILE_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileName[2].Value)},

    {"SPICE_FILE_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileName[3].Value)},

    {"SPICE_FILE_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Value),
     LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(SpiceFileName[4].Value)},

    {"TELEMETRY_PROVIDER_ID",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetryProviderId.Value)},

    {"TELEMETRY_SOURCE_HOST_NAME",         "STRING",       LBL_OPTIONAL,
     LBL_NO_CONT, 1,      1,      LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceHostName.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceHostName.Valid),
     LBL_NO_RETURN,       LBL_SIZE(TelemetrySourceHostName.Value)},

    {"TELEMETRY_SOURCE_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetrySourceName.Value)},

    {"TELEMETRY_PROVIDER_TYPE",     "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetryProviderType.Value)},

    {"TELEMETRY_SOURCE_TYPE",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetrySourceType.Value)},

    {"CHANNEL_ID",  "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ChannelId.Value),
     LBL_OFFSET(LblTelemetry_typ, ChannelId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ChannelId.Value)},

    {"COMMUNICATION_SESSION_ID",  "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, CommunicationSessionId.Value),
     LBL_OFFSET(LblTelemetry_typ, CommunicationSessionId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(CommunicationSessionId.Value)},

    {"DOWNLOAD_PRIORITY",  "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, DownloadPriority.Value),
     LBL_OFFSET(LblTelemetry_typ, DownloadPriority.Valid),
     LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

    {"EXPECTED_TRANSMISSION_PATH", "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ExpectedTransmissionPath.Value),
     LBL_OFFSET(LblTelemetry_typ, ExpectedTransmissionPath.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ExpectedTransmissionPath.Value)},

    {"FLIGHT_SOFTWARE_MODE",    "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, FlightSoftwareMode.Value),
     LBL_OFFSET(LblTelemetry_typ, FlightSoftwareMode.Valid),
     LBL_NO_RETURN,  LBL_SIZE(FlightSoftwareMode.Value)},

    {"FLIGHT_SOFTWARE_VERSION_ID",  "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, FlightSoftwareVersionId.Value),
     LBL_OFFSET(LblTelemetry_typ, FlightSoftwareVersionId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(FlightSoftwareVersionId.Value)},

    {"PACKET_CREATION_SCLK",        "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Value),
     LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Valid),
     LBL_NO_RETURN, LBL_SIZE(PacketCreationSclk.Value)},

    {"PACKET_SEQUENCE_NUMBER",      "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Value),
     LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(PacketSequenceNumber.Value)},

    {"PRODUCT_COMPLETION_STATUS",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ProductCompletionStatus.Value),
     LBL_OFFSET(LblTelemetry_typ, ProductCompletionStatus.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ProductCompletionStatus.Value)},

    {"PRODUCT_TAG",             "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ProductTag.Value),
     LBL_OFFSET(LblTelemetry_typ, ProductTag.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ProductTag.Value)},

    {"SEQUENCE_EXECUTION_COUNT", "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SequenceExecutionCnt.Value),
     LBL_OFFSET(LblTelemetry_typ, SequenceExecutionCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SequenceExecutionCnt.Value)},

    {"ROVER_COMPUTE_ELEMENT",    "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, RoverComputeElement.Value),
     LBL_OFFSET(LblTelemetry_typ, RoverComputeElement.Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverComputeElement.Value)},

    {"TELEMETRY_SOURCE_SIZE",  "INT",  LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceSize.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceSize.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetrySourceSize.Value)},

    {"SOURCE_PRODUCT_BYTES",  "INT",  LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SourceProductBytes.Value),
     LBL_OFFSET(LblTelemetry_typ, SourceProductBytes.Valid),
     LBL_NO_RETURN, LBL_SIZE(SourceProductBytes.Value)},

    {"TELEMETRY_SOURCE_CHECKSUM",  "INT",  LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceProductCheckSum.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceProductCheckSum.Valid),
     LBL_NO_RETURN, LBL_SIZE(TelemetrySourceProductCheckSum.Value)},

    {"SOURCE_PRODUCT_CHECKSUM",  "INT",  LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SourceProductCheckSum.Value),
     LBL_OFFSET(LblTelemetry_typ, SourceProductCheckSum.Valid),
     LBL_NO_RETURN, LBL_SIZE(SourceProductCheckSum.Value)},

    {"SOURCE_START_TIME",       "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SourceStartTime.Value),
     LBL_OFFSET(LblTelemetry_typ, SourceStartTime.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceStartTime.Value)},

    {"TELEMETRY_SOURCE_START_TIME",       "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceStartTime.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceStartTime.Valid),
     LBL_NO_RETURN,  LBL_SIZE(TelemetrySourceStartTime.Value)},

    {"TELEMETRY_SOURCE_SCLK_START", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceSCClockStartCnt.Value),
     LBL_OFFSET(LblTelemetry_typ, TelemetrySourceSCClockStartCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(TelemetrySourceSCClockStartCnt.Value)},

    {"SOURCE_SC_CLOCK_START_CNT", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SourceSCClockStartCnt.Value),
     LBL_OFFSET(LblTelemetry_typ, SourceSCClockStartCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceSCClockStartCnt.Value)},

    {"SOURCE_SC_CLOCK_STOP_CNT", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SourceSCClockStopCnt.Value),
     LBL_OFFSET(LblTelemetry_typ, SourceSCClockStopCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceSCClockStopCnt.Value)},

    {"SOFTWARE_NAME",           "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SoftwareName.Value),
     LBL_OFFSET(LblTelemetry_typ, SoftwareName.Valid),
     LBL_NO_RETURN, LBL_SIZE(SoftwareName.Value)},

    {"SOFTWARE_VERSION_ID",         "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Value),
     LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Valid),
     LBL_NO_RETURN, LBL_SIZE(SoftwareVersionId.Value)},

    {"STRIPING_COUNT",      "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, StripingCount.Value),
     LBL_OFFSET(LblTelemetry_typ, StripingCount.Valid),
     LBL_NO_RETURN,  LBL_SIZE(Stripes.Value)},

    {"STRIPING_OVERLAP_ROWS",      "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, Stripes.Value),
     LBL_OFFSET(LblTelemetry_typ, Stripes.Valid),
     LBL_NO_RETURN,  LBL_SIZE(Stripes.Value)},

    {"TLM_CMD_DISCREPANCY_FLAG",        "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Value),
     LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Valid),
     LBL_NO_RETURN, LBL_SIZE(TlmCmdDiscrepancyFlag.Value)},

    {"TOTAL_PARTS",            "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TotalParts.Value),
     LBL_OFFSET(LblTelemetry_typ, TotalParts.Valid),
     LBL_NO_RETURN,  LBL_SIZE(TotalParts.Value)},

    {"AUTO_DELETE_FLAG", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, AutoDeleteFlag.Value),
     LBL_OFFSET(LblTelemetry_typ, AutoDeleteFlag.Valid),
     LBL_NO_RETURN,  LBL_SIZE(AutoDeleteFlag.Value)},

    {"TRANSMISSION_INDICATOR", "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TransmissionIndicator.Value),
     LBL_OFFSET(LblTelemetry_typ, TransmissionIndicator.Valid),
     LBL_NO_RETURN,  LBL_SIZE(TransmissionIndicator.Value)},

    {"TRANSMISSION_PATH",      "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, TransmissionPath.Value),
     LBL_OFFSET(LblTelemetry_typ, TransmissionPath.Valid),
     LBL_NO_RETURN,  LBL_SIZE(TransmissionPath.Value)},

    {"VIRTUAL_CHANNEL_ID",  "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, VirtualChannelId.Value),
     LBL_OFFSET(LblTelemetry_typ, VirtualChannelId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(VirtualChannelId.Value)},

    {"IMAGE_DATA_SIZE",            "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblTelemetry_typ, ImageDataSize.Value),
     LBL_OFFSET(LblTelemetry_typ, ImageDataSize.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ImageDataSize.Value)},

    {"ERROR_PIXEL_LINE",		"INT",		LBL_OPTIONAL,
    LBL_NO_CONT,	1,	1,	LBL_NULL,
    LBL_OFFSET(LblTelemetry_typ, ErrorPixelLine.Value),
    LBL_OFFSET(LblTelemetry_typ, ErrorPixelLine.Valid),
    LBL_NO_RETURN,	LBL_SIZE(ErrorPixelLine.Value)},

    {"ERROR_PIXEL_SAMPLE",		"INT",		LBL_OPTIONAL,
    LBL_NO_CONT,	1,	1,	LBL_NULL,
    LBL_OFFSET(LblTelemetry_typ, ErrorPixelSample.Value),
    LBL_OFFSET(LblTelemetry_typ, ErrorPixelSample.Valid),
    LBL_NO_RETURN,	LBL_SIZE(ErrorPixelSample.Value)},

    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ    Label = {
    LabelTbl,       "PROPERTY",     "PROPERTY",     "TELEMETRY",
    LBL_NULL };

/******************************************************************************
 *              LBL_TELEMETRY
 *
 *****************************************************************************/
int     LblTelemetry(
                     int   Unit,
                     int   Obtain,
                     LblTelemetry_typ      *LabelItems,
                     int    Instance)
{ int   RtnStatus;
    LblApiCntrl_typ Cntrl;

    Label.Buffer = (void *)LabelItems;
    Label.BufferSize = sizeof(LblTelemetry_typ);

    memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
    Cntrl.Instance = Instance;
    Cntrl.FileUnit = Unit;
    Cntrl.Obtain = Obtain;
    Cntrl.ProceedOnError = LBL_TRUE;

    RtnStatus = LblProcessor(&Cntrl, &Label);

    return (RtnStatus);
}

/******************************************************************************
 *              LBL_PRINT_TELEMETRY
 *
 *****************************************************************************/
void    LblPrintTelemetry(
                          LblTelemetry_typ  *LabelItems)
{
    Label.Buffer = (void *)LabelItems;

    PrintLabelElements( &Label );

    return;
}

/******************************************************************************
 *              LBL_TEST_TELEMETRY
 *
 *****************************************************************************/
void    LblTestTelemetry(
                         LblTelemetry_typ   *LabelItems)
{
    Label.Buffer = (void *)LabelItems;

    TestLoadLabelElements( &Label );

    return;
}
