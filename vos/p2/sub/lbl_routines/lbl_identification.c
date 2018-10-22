/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_identification.h"

/******************************************************************************
 *              LBL_IDENTIFICATION
 *
 *  This module contains routines to help create, read/write and print
 *  an Identification property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *  The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_identification.h".  The
 *  implementation supporting the interface is this module.
 *
 *  The primary routine used by a typical application program is
 *  LblIdentification.  This routine requires exactly 4 parameters.
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
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * 07-Oct-2015  C. Cheng        Removed SEQUENCE_ID. It's already defined!
 * 02-Jul-2015  C. Cheng        Added MESH_ID, MOSAIC_ID, SEQUENCE_ID and
 *                              STEREO_MATCH_ID
 * 23-May-2003  Hyun Lee        Changed INSTRUMENT_HOST_ID & INSTRUMENT_ID as
 *                              array values
 * 14-Feb-2003  P. Zamani       Changed PLANET_DAY_NUMBER to INT
 * 12-Feb-2003  Payam Zamani    Added RELEASE_ID, change SOLAR_LONGITUE to REAL
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)    sizeof(((LblIdentification_typ *)0)->x)

static LblApiElement_typ    LabelTbl[] = {
    {"ACTIVE_FLIGHT_STRING_ID",    "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ActiveFlightStringId.Value),
     LBL_OFFSET(LblIdentification_typ, ActiveFlightStringId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ActiveFlightStringId.Value)},

    {"DATA_SET_ID",             "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, DataSetId.Value),
     LBL_OFFSET(LblIdentification_typ, DataSetId.Valid),
     LBL_NO_RETURN, LBL_SIZE(DataSetId.Value)},

    {"DATA_SET_NAME",           "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, DataSetName.Value),
     LBL_OFFSET(LblIdentification_typ, DataSetName.Valid),
     LBL_NO_RETURN, LBL_SIZE(DataSetName.Value)},

    {"COMMAND_SEQUENCE_NUMBER",     "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, CommandSequenceNumber.Value),
     LBL_OFFSET(LblIdentification_typ, CommandSequenceNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(CommandSequenceNumber.Value)},

    {"FEATURE_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FeatureName.Value),
     LBL_OFFSET(LblIdentification_typ, FeatureName.Valid),
     LBL_NO_RETURN, LBL_SIZE(FeatureName.Value)},

    {"FEATURE_TYPE",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FeatureType.Value),
     LBL_OFFSET(LblIdentification_typ, FeatureType.Valid),
     LBL_NO_RETURN, LBL_SIZE(FeatureType.Value)},

    {"FRAME_ID",                "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameId[0].Value),
     LBL_OFFSET(LblIdentification_typ, FrameId[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameId[0].Value)},

    {"FRAME_ID",                "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameId[1].Value),
     LBL_OFFSET(LblIdentification_typ, FrameId[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameId[1].Value)},

    {"FRAME_ID",                "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameId[2].Value),
     LBL_OFFSET(LblIdentification_typ, FrameId[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameId[2].Value)},

    {"FRAME_ID",                "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameId[3].Value),
     LBL_OFFSET(LblIdentification_typ, FrameId[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameId[3].Value)},

    {"FRAME_ID",                "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameId[4].Value),
     LBL_OFFSET(LblIdentification_typ, FrameId[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameId[4].Value)},

    {"FRAME_TYPE",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, FrameType.Value),
     LBL_OFFSET(LblIdentification_typ, FrameType.Valid),
     LBL_NO_RETURN, LBL_SIZE(FrameType.Value)},

    {"GEOMETRY_PROJECTION_TYPE",            "STRING",       LBL_OPTIONAL,
     LBL_NO_CONT,    1,      1,      LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, GeometryProjectionType.Value),
     LBL_OFFSET(LblIdentification_typ, GeometryProjectionType.Valid),
     LBL_NO_RETURN,  LBL_SIZE(GeometryProjectionType.Value)},

    {"IMAGE_ID",                "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ImageId.Value),
     LBL_OFFSET(LblIdentification_typ, ImageId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ImageId.Value)},

    {"IMAGE_TIME",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ImageTime.Value),
     LBL_OFFSET(LblIdentification_typ, ImageTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(ImageTime.Value)},

    {"IMAGE_TYPE",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ImageType.Value),
     LBL_OFFSET(LblIdentification_typ, ImageType.Valid),
     LBL_NO_RETURN, LBL_SIZE(ImageType.Value)},

    {"IMAGE_ACQUIRE_MODE",      "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ImageAcquireMode.Value),
     LBL_OFFSET(LblIdentification_typ, ImageAcquireMode.Valid),
     LBL_NO_RETURN,  LBL_SIZE(ImageAcquireMode.Value)},

    {"INSTRUMENT_HOST_ID",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[0].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostId[0].Value)},

    {"INSTRUMENT_HOST_ID",          "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[1].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostId[1].Value)},

    {"INSTRUMENT_HOST_ID",          "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[2].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostId[2].Value)},

    {"INSTRUMENT_HOST_ID",          "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[3].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostId[3].Value)},

    {"INSTRUMENT_HOST_ID",          "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[4].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostId[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostId[4].Value)},

    {"INSTRUMENT_HOST_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[0].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostName[0].Value)},

    {"INSTRUMENT_HOST_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[1].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostName[1].Value)},

    {"INSTRUMENT_HOST_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[2].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostName[2].Value)},

    {"INSTRUMENT_HOST_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[3].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostName[3].Value)},

    {"INSTRUMENT_HOST_NAME",        "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[4].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentHostName[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentHostName[4].Value)},

    {"INSTRUMENT_ID",           "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentId[0].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentId[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentId[0].Value)},

    {"INSTRUMENT_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentId[1].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentId[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentId[1].Value)},

    {"INSTRUMENT_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentId[2].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentId[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentId[2].Value)},

    {"INSTRUMENT_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentId[3].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentId[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentId[3].Value)},

    {"INSTRUMENT_ID",           "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentId[4].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentId[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentId[4].Value)},

    {"INSTRUMENT_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentName[0].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentName[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentName[0].Value)},

    {"INSTRUMENT_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentName[1].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentName[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentName[1].Value)},

    {"INSTRUMENT_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentName[2].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentName[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentName[2].Value)},

    {"INSTRUMENT_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentName[3].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentName[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentName[3].Value)},

    {"INSTRUMENT_NAME",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentName[4].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentName[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentName[4].Value)},

    {"INSTRUMENT_SERIAL_NUMBER",        "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentSerialNumber.Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentSerialNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentSerialNumber.Value)},

    {"INSTRUMENT_TYPE",         "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentType[0].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentType[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentType[0].Value)},

    {"INSTRUMENT_TYPE",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentType[1].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentType[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentType[1].Value)},

    {"INSTRUMENT_TYPE",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentType[2].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentType[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentType[2].Value)},

    {"INSTRUMENT_TYPE",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentType[3].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentType[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentType[3].Value)},

    {"INSTRUMENT_TYPE",         "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentType[4].Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentType[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentType[4].Value)},

    {"INSTRUMENT_VERSION_ID",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, InstrumentVersionId.Value),
     LBL_OFFSET(LblIdentification_typ, InstrumentVersionId.Valid),
     LBL_NO_RETURN, LBL_SIZE(InstrumentVersionId.Value)},

    {"LOCAL_MEAN_SOLAR_TIME",               "STRING",       LBL_OPTIONAL,
     LBL_NO_CONT,    1,      1,      LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, LocalMeanSolarTime.Value),
     LBL_OFFSET(LblIdentification_typ, LocalMeanSolarTime.Valid),
     LBL_NO_RETURN,  LBL_SIZE(LocalMeanSolarTime.Value)},

    {"LOCAL_TRUE_SOLAR_TIME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTime.Value),
     LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(LocalTrueSolarTime.Value)},

    {"MAGNET_ID",               "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MagnetId.Value),
     LBL_OFFSET(LblIdentification_typ, MagnetId.Valid),
     LBL_NO_RETURN, LBL_SIZE(MagnetId.Value)},

    {"MEASUREMENT_ID",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MeasurementId.Value),
     LBL_OFFSET(LblIdentification_typ, MeasurementId.Valid),
     LBL_NO_RETURN, LBL_SIZE(MeasurementId.Value)},

    {"MEASUREMENT_TIME",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MeasurementTime.Value),
     LBL_OFFSET(LblIdentification_typ, MeasurementTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(MeasurementTime.Value)},

    {"MEASUREMENT_TYPE",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MeasurementType.Value),
     LBL_OFFSET(LblIdentification_typ, MeasurementType.Valid),
     LBL_NO_RETURN, LBL_SIZE(MeasurementType.Value)},

    {"MISSION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionName[0].Value),
     LBL_OFFSET(LblIdentification_typ, MissionName[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionName[0].Value)},

    {"MISSION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionName[1].Value),
     LBL_OFFSET(LblIdentification_typ, MissionName[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionName[1].Value)},

    {"MISSION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionName[2].Value),
     LBL_OFFSET(LblIdentification_typ, MissionName[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionName[2].Value)},

    {"MISSION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionName[3].Value),
     LBL_OFFSET(LblIdentification_typ, MissionName[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionName[3].Value)},

    {"MISSION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionName[4].Value),
     LBL_OFFSET(LblIdentification_typ, MissionName[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionName[4].Value)},

    {"MISSION_PHASE_NAME",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MissionPhaseName.Value),
     LBL_OFFSET(LblIdentification_typ, MissionPhaseName.Valid),
     LBL_NO_RETURN, LBL_SIZE(MissionPhaseName.Value)},

    {"OBSERVATION_ID",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ObservationId.Value),
     LBL_OFFSET(LblIdentification_typ, ObservationId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ObservationId.Value)},

    {"OBSERVATION_NAME",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ObservationName.Value),
     LBL_OFFSET(LblIdentification_typ, ObservationName.Valid),
     LBL_NO_RETURN, LBL_SIZE(ObservationName.Value)},

    {"OBSERVATION_TIME",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ObservationTime.Value),
     LBL_OFFSET(LblIdentification_typ, ObservationTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(ObservationTime.Value)},

    {"OBSERVATION_TYPE",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ObservationType.Value),
     LBL_OFFSET(LblIdentification_typ, ObservationType.Valid),
     LBL_NO_RETURN, LBL_SIZE(ObservationType.Value)},

    {"ORBIT_NUMBER",            "REAL",     LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, OrbitNumber.Value),
     LBL_OFFSET(LblIdentification_typ, OrbitNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(OrbitNumber.Value)},

    {"PLANET_DAY_NUMBER",           "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, PlanetDayNumber.Value),
     LBL_OFFSET(LblIdentification_typ, PlanetDayNumber.Valid),
     LBL_NO_RETURN, LBL_SIZE(PlanetDayNumber.Value)},

    {"LOCAL_TRUE_SOLAR_TIME_SOL",           "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTimeSol.Value),
     LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTimeSol.Valid),
     LBL_NO_RETURN,  LBL_SIZE(LocalTrueSolarTimeSol.Value)},

    {"PROCESSING_HISTORY_TEXT",     "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProcessingHistoryText.Value),
     LBL_OFFSET(LblIdentification_typ, ProcessingHistoryText.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProcessingHistoryText.Value)},

    {"PRODUCER_FULL_NAME",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProducerFullName.Value),
     LBL_OFFSET(LblIdentification_typ, ProducerFullName.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProducerFullName.Value)},

    {"PRODUCER_ID",             "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProducerId.Value),
     LBL_OFFSET(LblIdentification_typ, ProducerId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProducerId.Value)},

    {"PRODUCER_INSTITUTION_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProducerInstitutionName.Value),
     LBL_OFFSET(LblIdentification_typ, ProducerInstitutionName.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProducerInstitutionName.Value)},

    {"PRODUCT_CREATION_TIME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProductCreationTime.Value),
     LBL_OFFSET(LblIdentification_typ, ProductCreationTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProductCreationTime.Value)},

    {"PRODUCT_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProductId.Value),
     LBL_OFFSET(LblIdentification_typ, ProductId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProductId.Value)},

    {"PRODUCT_VERSION_ID",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ProductVersionId.Value),
     LBL_OFFSET(LblIdentification_typ, ProductVersionId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ProductVersionId.Value)},
    {"RELEASE_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, ReleaseId.Value),
     LBL_OFFSET(LblIdentification_typ, ReleaseId.Valid),
     LBL_NO_RETURN, LBL_SIZE(ReleaseId.Value)},

    {"REQUEST_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RequestId.Value),
     LBL_OFFSET(LblIdentification_typ, RequestId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(RequestId.Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[0].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounter[0].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[1].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounter[1].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[2].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounter[2].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[3].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounter[3].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[4].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounter[4].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,   1,  6,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[5].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[5].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[5].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,   1,  7,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[6].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[6].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[6].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,   1,  8,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[7].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[7].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[7].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,   1,  9,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[8].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[8].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[8].Value)},

    {"ROVER_MOTION_COUNTER",        "INT",      LBL_OPTIONAL,
     LBL_CONTINUE,   1,  10,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[9].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[9].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounter[9].Value)},


    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[0].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[0].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounterName[0].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[1].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[1].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounterName[1].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[2].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[2].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounterName[2].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  4,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[3].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[3].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounterName[3].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,  1,  5,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[4].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[4].Valid),
     LBL_NO_RETURN, LBL_SIZE(RoverMotionCounterName[4].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,   1,  6,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[5].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[5].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[5].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,   1,  7,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[6].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[6].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[6].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,   1,  8,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[7].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[7].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[7].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,   1,  9,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[8].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[8].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[8].Value)},

    {"ROVER_MOTION_COUNTER_NAME",       "STRING",   LBL_OPTIONAL,
     LBL_CONTINUE,   1,  10,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[9].Value),
     LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[9].Valid),
     LBL_NO_RETURN,  LBL_SIZE(RoverMotionCounterName[9].Value)},

    {"SEQ_ID,SEQUENCE_ID",          "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SequenceId.Value),
     LBL_OFFSET(LblIdentification_typ, SequenceId.Valid),
     LBL_NO_RETURN, LBL_SIZE(SequenceId.Value)},

    {"SEQUENCE_NAME, SEQUENCE_TITLE",   "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SequenceName.Value),
     LBL_OFFSET(LblIdentification_typ, SequenceName.Valid),
     LBL_NO_RETURN, LBL_SIZE(SequenceName.Value)},

    {"SEQUENCE_VERSION_ID",         "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SequenceVersionId.Value),
     LBL_OFFSET(LblIdentification_typ, SequenceVersionId.Valid),
     LBL_NO_RETURN, LBL_SIZE(SequenceVersionId.Value)},

    {"SOLAR_LONGITUDE",         "REAL",     LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SolarLongitude.Value),
     LBL_OFFSET(LblIdentification_typ, SolarLongitude.Valid),
     LBL_NO_RETURN, LBL_SIZE(SolarLongitude.Value)},

    {"SOURCE_PRODUCT_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceProductId[0].Value),
     LBL_OFFSET(LblIdentification_typ, SourceProductId[0].Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceProductId[0].Value)},

    {"SOURCE_PRODUCT_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  2,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceProductId[1].Value),
     LBL_OFFSET(LblIdentification_typ, SourceProductId[1].Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceProductId[1].Value)},

    {"SOURCE_PRODUCT_ID",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  3,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceProductId[2].Value),
     LBL_OFFSET(LblIdentification_typ, SourceProductId[2].Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceProductId[2].Value)},

    {"SPACECRAFT_CLOCK_CNT_PARTITION",  "INT",      LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockCntPartition.Value),
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockCntPartition.Valid),
     LBL_NO_RETURN, LBL_SIZE(SpacecraftClockCntPartition.Value)},

    {"SPACECRAFT_CLOCK_START_COUNT",    "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockStartCount.Value),
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockStartCount.Valid),
     LBL_NO_RETURN, LBL_SIZE(SpacecraftClockStartCount.Value)},

    {"SPACECRAFT_CLOCK_STOP_COUNT",     "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockStopCount.Value),
     LBL_OFFSET(LblIdentification_typ, SpacecraftClockStopCount.Valid),
     LBL_NO_RETURN, LBL_SIZE(SpacecraftClockStopCount.Value)},

    {"START_TIME",              "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, StartTime.Value),
     LBL_OFFSET(LblIdentification_typ, StartTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(StartTime.Value)},

    {"STOP_TIME",               "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, StopTime.Value),
     LBL_OFFSET(LblIdentification_typ, StopTime.Valid),
     LBL_NO_RETURN, LBL_SIZE(StopTime.Value)},

    {"TARGET_NAME",             "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, TargetName.Value),
     LBL_OFFSET(LblIdentification_typ, TargetName.Valid),
     LBL_NO_RETURN, LBL_SIZE(TargetName.Value)},

    {"TARGET_TYPE",             "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, TargetType.Value),
     LBL_OFFSET(LblIdentification_typ, TargetType.Valid),
     LBL_NO_RETURN, LBL_SIZE(TargetType.Value)},

    {"OPS_TOKEN",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, OpsToken.Value),
     LBL_OFFSET(LblIdentification_typ, OpsToken.Valid),
     LBL_NO_RETURN,  LBL_SIZE(OpsToken.Value)},

    {"OPS_TOKEN_ACTIVITY",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, OpsTokenActivity.Value),
     LBL_OFFSET(LblIdentification_typ, OpsTokenActivity.Valid),
     LBL_NO_RETURN,  LBL_SIZE(OpsTokenActivity.Value)},

    {"OPS_TOKEN_COMMAND",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, OpsTokenCommand.Value),
     LBL_OFFSET(LblIdentification_typ, OpsTokenCommand.Valid),
     LBL_NO_RETURN,  LBL_SIZE(OpsTokenCommand.Value)},

    {"OPS_TOKEN_PAYLOAD",            "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, OpsTokenPayload.Value),
     LBL_OFFSET(LblIdentification_typ, OpsTokenPayload.Valid),
     LBL_NO_RETURN,  LBL_SIZE(OpsTokenPayload.Value)},

    /*
      {"RCE_SEQUENCE_EXECUTION_CNT", "INT",      LBL_OPTIONAL,
      LBL_NO_CONT,    1,  1,  LBL_NULL,
      LBL_OFFSET(LblIdentification_typ, RecSequenceExecutionCnt.Value),
      LBL_OFFSET(LblIdentification_typ, RecSequenceExecutionCnt.Valid),
      LBL_NO_RETURN,  LBL_SIZE(RecSequenceExecutionCnt.Value)},
    */

    {"SOURCE_SC_CLOCK_START_CNT", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceSCClockStartCnt.Value),
     LBL_OFFSET(LblIdentification_typ, SourceSCClockStartCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceSCClockStartCnt.Value)},

    {"SOURCE_SC_CLOCK_STOP_CNT", "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceSCClockStopCnt.Value),
     LBL_OFFSET(LblIdentification_typ, SourceSCClockStopCnt.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceSCClockStopCnt.Value)},

    {"SOURCE_START_TIME",       "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, SourceStartTime.Value),
     LBL_OFFSET(LblIdentification_typ, SourceStartTime.Valid),
     LBL_NO_RETURN,  LBL_SIZE(SourceStartTime.Value)},

    {"MESH_ID",       "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MeshId.Value),
     LBL_OFFSET(LblIdentification_typ, MeshId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(MeshId.Value)},

    {"MOSAIC_ID",     "STRING",   LBL_OPTIONAL,
     LBL_NO_CONT,   1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, MosaicId.Value),
     LBL_OFFSET(LblIdentification_typ, MosaicId.Valid),
     LBL_NO_RETURN, LBL_SIZE(MosaicId.Value)},

    {"STEREO_MATCH_ID",       "STRING",      LBL_OPTIONAL,
     LBL_NO_CONT,    1,  1,  LBL_NULL,
     LBL_OFFSET(LblIdentification_typ, StereoMatchId.Value),
     LBL_OFFSET(LblIdentification_typ, StereoMatchId.Valid),
     LBL_NO_RETURN,  LBL_SIZE(StereoMatchId.Value)},

    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ    Label = {
    LabelTbl,   "PROPERTY", "PROPERTY", "IDENTIFICATION",
    LBL_NULL };

/******************************************************************************
 *              LBL_IDENTIFIER
 *
 *****************************************************************************/
int     LblIdentification(
                          int   Unit,
                          int   Obtain,
                          LblIdentification_typ      *LabelItems,
                          int   Instance)
{ int   RtnStatus;
    LblApiCntrl_typ Cntrl;

    Label.Buffer = (void *)LabelItems;
    Label.BufferSize = sizeof(LblIdentification_typ);

    memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
    Cntrl.Instance = Instance;
    Cntrl.FileUnit = Unit;
    Cntrl.Obtain = Obtain;
    Cntrl.ProceedOnError = LBL_TRUE;

    RtnStatus = LblProcessor(&Cntrl, &Label);

    return (RtnStatus);
}

/******************************************************************************
 *              LBL_PRINT_IDENTIFIER
 *
 *****************************************************************************/
void     LblPrintIdentification(
                                LblIdentification_typ   *LabelItems)
{
    Label.Buffer = (void *)LabelItems;

    PrintLabelElements( &Label );

    return;
}

/******************************************************************************
 *              LBL_TEST_IDENTIFIER
 *
 *****************************************************************************/
void     LblTestIdentification(
                               LblIdentification_typ    *LabelItems)
{
    Label.Buffer = (void *)LabelItems;

    TestLoadLabelElements( &Label );

    return;
}
