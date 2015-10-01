/*  
 NAME OF MODULE
     cas_isslab.h ( CASsini ISS LABel Header)

 PURPOSE
     This is the C structure used for ABLE97.  ABLE97 extracts data from the
    Cassini ISS flight and ground calibration labels. 
 ENVIRONMENT
     VMS or UNIX  with VICAR EXECUTIVE       C
    
 REVISION HISTORY
   1-97  SP   Added "CAS_" to front of module name. Note character data is
              not null-terminated.
   2-97  SP   changed short int to in for all members so xable97 does not need
              to use I*2.
  10-99  TLT  Increased all character lengths by 1 to accomodate null-terminator
              Renamed prepdur to prepcycle and readoutdur to readoutcycle
		Added shuttertype
   01-2000 tlt Changed expos from int to float.
   08-2000 ays Updated to accomodate new vicar property labels
   04-2003 vrh Updated to accomodate tour vicar labels, consolidated values
*/

typedef struct
    {      
      int  sclk;          /* spacecraft clock, IMAGE_NUMBER */
      int scety;          /* SCET: IMAGE_TIME (same as STOP_TIME), year */
      int scetd;          /* day of year */
      int sceth;          /* hour of day */
      int scetm;          /* minute of hour */
      int scets;          /* second of minute */
      int scetms;         /* milisecond */
      int scety2;         /* SCET: EARTH_RECEIVED_START_TIME */
      int scetd2;
      int sceth2;
      int scetm2;
      int scets2;
      int scetms2;
      int scety3;         /* SCET: EARTH_RECEIVED_STOP_TIME */
      int scetd3;
      int sceth3;
      int scetm3;
      int scets3;
      int scetms3;
      int scety4;         /* SCET: START_TIME */
      int scetd4;
      int sceth4;
      int scetm4;
      int scets4;
      int scetms4;
      int scety5;         /* SCET: IMAGE_MID_TIME */
      int scetd5;
      int sceth5;
      int scetm5;
      int scets5;
      int scetms5;
      int sclkstart;      /* SPACECRAFT_CLOCK_START_COUNT (seconds) */
      int sclkstartsub;   /* ticks (1/256 of second) */
      int sclkstop;       /* SPACECRAFT_CLOCK_STOP_COUNT (seconds) */
      int sclkstopsub;    /* ticks (1/256 of second) */
      int sccp;           /* SPACECRAFT_CLOCK_CNT_PARTITION */
      float cld;          /* CALIBRAION_LAMP_DURATION */
      float expos;        /* EXPOSURE_DURATION */
      float ccdtemp;      /* DETECTOR_TEMPERATURE */
      float opttemp;      /* Forward Optics Temp, 1st of OPTICS_TEMPERATURE */
      float flttemp;      /* FILTER_TEMPERATURE */
      int blocks;         /* Group of Blocks, 3rd of INST_CMPRS_PARAM */
      int algorithm;      /* Malgo, 1st of INST_CMPRS_PARAM */
      int btype;          /* Block type (TB), 2nd of INST_CMPRS_PARAM */
      int qfactor;        /* Quantization (B) factor, 4th of INST_CMPRS_PARAM */
      float comprat;      /* COMPRESSION_RATIO */
      int   missing;      /* MISSING_LINES */
      float radiance;
      int   prepcycle;    /* PREPARE_CYCLE_INDEX */
      int   readoutcycle; /* READOUT_CYCLE_INDEX */
      float bias;         /* Overclock pixel, OFFSET/BIAS or BIAS_STRIP_MEAN */
      float extpixval;    /* Extended Pixel Value or DARK_STRIP_MEAN */
      float sensortemp;   /* SENSOR_HEAD_ELEC_TEMPERATURE */
      float datarate;     /* INSTRUMENT_DATA_RATE */
      int   compparam;    /* COMPRESSION_PARAMETER_VALUE (LOSSY parm combo) */
      int comdseqnum;     /* COMMAND_SEQUENCE_NUMBER */
      int elbias;         /* ELECTRONICS_BIAS (video offset) */
      int pcvi;           /* PARALLEL_CLOCK_VOLTAGE_INDEX */
      int exppackets;     /* EXPECTED_PACKETS */
      int ordernum;       /* ORDER_NUMBER */
      int recpackets;     /* RECEIVED_PACKETS */
      int seqnum;         /* SEQUENCE_NUMBER */
      int validmax;       /* valid maximum (a/d), 1st of VALID_MAXIMUM */
      int validmaxfw;     /* valid maximum (full well), 2nd of VALID_MAXIMUM */
      float expmax;       /* % of a/d dn, 1st of EXPECTED_MAXIMUM */
      float expmaxfw;     /* % of full well, 2nd of EXPECTED_MAXIMUM */
      float compratepred; /* Bits per pix, predicted, 1st of INST_CMPRS_RATE */
      float comprateact;  /* Bits per pix, actual, 2nd of INST_CMPRS_RATE */
      float rearopttemp;  /* Rear Optics Temp, 2nd of OPTICS_TEMPERAURE */
      char labtype[8];    /* label type = ISSGRND, ISSFLTn */
      char phase[31];     /* mission phase = BENCH, THERMAL/VAC, */
                          /*                  SYSTEM TEST, FLIGHT, mission */
      char camera[6];     /* camera id = ISSNA or ISSWA */
      char swvers[21];    /* SOFTWARE_VERSION_ID (ground) */
      char mode[5];       /* INSTRUMENT_MODE_ID: FULL, SUM2, SUM4 */
      char gain[21];      /* GAIN_MODE_ID: Calibration - 40K, 100K, 400K, 1400K
                             Early Flight - 12, 29, 95, 215 e/DN
                             Later flight - 12, 29, 95, 215 ELECTRONS PER DN */
      char comprsn[9];    /* Compression: NOTCOMP, LOSSLESS, LOSSY */
      char convrsn[7];    /* Conversion: 12BIT, TABLE, 8LSB */
      char ltfld[4];      /* LIGHT_FLOOD_STATE_FLAG: ON or OFF */
      char antiblm[4];    /* ANTIBLOOMING_STATE_FLAG: ON or OFF */
      char callamp[4];    /* CALIBRATION_LAMP_STATE_FLAG: ON or OFF */
      char target[31];    /* TARGET_NAME */
      char observid[31];  /* OBSERVATION_ID */
      char source[19];    /* ILLUMINANT (ground calibration) */
      char shuttermode[9];/* Shutter: NACONLY, WACONLY, BOTSIM */
      char dlyreadout[4]; /* DELAYED_READOUT_FLAG: YES or NO */
      char imagetime[22]; /* IMAGE_TIME */
      char filter1[6];    /* Filter 1 position, 1st of FILTER_NAME */
      char filter2[6];    /* Filter 2 position, 2nd of FILTER_NAME */
      char mispacflg[10]; /* MISSING_PACKET_FLAG: YES or NO */
      char shutid[11];    /* Shutter: ENABLED, DISABLED */
      char ertstart[22];  /* EARTH_RECEIVED_START_TIME */
      char ertstop[22];   /* EARTH_RECEIVED_STOP_TIME */
      char fltsoftversid[31];  /* FLIGHT_SOFTWARE_VERSION_ID e.g. 1.2 or 1.3 */
      char starttime[22]; /* START_TIME */
      char dsid[41];      /* DATA_SET_ID */
      char ihn[16];       /* INSTRUMENT_HOST_NAME */
      char in[39];        /* INSTRUMENT_NAME */
      char note[101];
      char pctime[22];    /* PRODUCT_CREATION_TIME */
      char prid[41];      /* PRODUCT_ID */
      char pvt[12];       /* PRODUCT_VERSION_TYPE, e.g. PRELIMINARY, FINAL */
      char sid[31];       /* SEQUENCE_ID */
      char cmdname[121];       /* COMMAND_FILE_NAME */
      char imgobstype[5][16];  /* IMAGE_OBSERVATION_TYPE */ 
      char seqtitle[61];       /* SEQUENCE_TITLE */
      char description[251];
      char midtime[22];        /* IMAGE_MID_TIME */
      char methoddesc[101];    /* METHOD_DESC */
      char targetdesc[76];     /* TARGET_DESC */
      char targetlist[10][31]; /* TARGET_LIST */
      char telemid[11];        /* TELEMETRY_FORMAT_ID */
    } able97_typ;
