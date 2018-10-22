#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"

void FTN_NAME(tzable97)(unit)
int *unit;
{
      int ind;
      int i;
      able97_typ lab;
/*  ==================================================================  */

      zable97(&ind,*unit,&lab);

      zprintstr(7,lab.labtype,"LABEL TYPE = ");
      zprintstr(30,lab.phase,"MISSION PHASE = ");
      zprintstr(5,lab.camera,"CAMERA ID = ");
      zprnt(4,1,&lab.sclk,"SCLK = ");
      zprnt(4,1,&lab.scety,"EVENT TIME YEAR = ");
      zprnt(4,1,&lab.scetd,"EVENT TIME DAY = ");
      zprnt(4,1,&lab.sceth,"EVENT TIME HOUR = ");
      zprnt(4,1,&lab.scetm,"EVENT TIME MINUTE = ");
      zprnt(4,1,&lab.scets,"EVENT TIME SECOND = ");
      zprnt(4,1,&lab.scetms,"EVENT TIME MSEC = ");
      zprnt(4,1,&lab.scety2,"EARTH RECEIVED START TIME YEAR = ");
      zprnt(4,1,&lab.scetd2,"EARTH RECEIVED START TIME DAY = ");
      zprnt(4,1,&lab.sceth2,"EARTH RECEIVED START TIME HOUR = ");
      zprnt(4,1,&lab.scetm2,"EARTH RECEIVED START TIME MINUTE = ");
      zprnt(4,1,&lab.scets2,"EARTH RECEIVED START TIME SECOND = ");
      zprnt(4,1,&lab.scetms2,"EARTH RECEIVED START TIME MSEC = ");
      zprnt(4,1,&lab.scety3,"EARTH RECEIVED STOP TIME YEAR = ");
      zprnt(4,1,&lab.scetd3,"EARTH RECEIVED STOP TIME DAY = ");
      zprnt(4,1,&lab.sceth3,"EARTH RECEIVED STOP TIME HOUR = ");
      zprnt(4,1,&lab.scetm3,"EARTH RECEIVED STOP TIME MINUTE = ");
      zprnt(4,1,&lab.scets3,"EARTH RECEIVED STOP TIME SECOND = ");
      zprnt(4,1,&lab.scetms3,"EARTH RECEIVED STOP TIME MSEC = ");
      zprnt(4,1,&lab.scety4,"START TIME YEAR = ");
      zprnt(4,1,&lab.scetd4,"START TIME DAY = ");
      zprnt(4,1,&lab.sceth4,"START TIME HOUR = ");
      zprnt(4,1,&lab.scetm4,"START TIME MINUTE = ");
      zprnt(4,1,&lab.scets4,"START TIME SECOND = ");
      zprnt(4,1,&lab.scetms4,"START TIME MSEC = ");
      zprnt(4,1,&lab.scety5,"MID TIME YEAR = ");
      zprnt(4,1,&lab.scetd5,"MID TIME DAY = ");
      zprnt(4,1,&lab.sceth5,"MID TIME HOUR = ");
      zprnt(4,1,&lab.scetm5,"MID TIME MINUTE = ");
      zprnt(4,1,&lab.scets5,"MID TIME SECOND = ");
      zprnt(4,1,&lab.scetms5,"MID TIME MSEC = ");
      zprnt(4,1,&lab.sclkstart,"SCLKSTART = ");
      zprnt(4,1,&lab.sclkstartsub, "SCLKSTARTSUB = ");
      zprnt(4,1,&lab.sclkstop, "SCLKSTOP = ");
      zprnt(4,1,&lab.sclkstopsub, "SCLKSTOPSUB = ");
      zprintstr(20,lab.swvers,"IMAGE BUILDER S/W VERSION = ");
      zprintstr(4,lab.mode,"INSTRUMENT MODE = ");
      zprintstr(20,lab.gain,"GAIN STATE/_MODE_ID = ");
      zprnt(7,1,&lab.expos,"EXPOSURE TIME =");
      zprintstr(8,lab.comprsn,"COMPRESSION = ");
      zprintstr(6,lab.convrsn,"ENCODING/DATA_CONVERSION_TYPE = ");
      zprnt(7,1,&lab.ccdtemp,"CDD TEMP = ");
      zprnt(7,1,&lab.opttemp,"OPTICS TEMP = ");
      zprnt(7,1,&lab.rearopttemp,"REAR OPTICS TEMP = ");
      zprnt(7,1,&lab.flttemp,"FILTER TEMP = ");
      zprintstr(3,lab.ltfld,"LIGHT FLOOD STATE = ");
      zprintstr(3,lab.antiblm,"ANTI-BLOOMING STATE = ");
      zprintstr(3,lab.callamp,"CALIBRATION LAMP STATE = ");
      zprnt(4,1,&lab.blocks,"BLOCKS/GROUP = ");
      zprnt(4,1,&lab.algorithm,"ALGORITHM = ");
      zprnt(4,1,&lab.btype,"BLOCK TYPE = ");
      zprnt(4,1,&lab.qfactor,"Q FACTOR INDEX = ");
      zprnt(7,1,&lab.comprat,"CMPRSN RATIO = ");
      zprnt(4,1,&lab.missing,"MISSING LINES = ");
      zprintstr(30,lab.target,"TARGET_NAME = ");
      zprintstr(30,lab.observid,"OBSERVATION ID = ");
      zprintstr(18,lab.source,"ILLUMINANT = ");
      zprnt(7,1,&lab.radiance,"RADIANCE = ");
      zprnt(4,1,&lab.prepcycle,"PREPARE CYCLE INDEX = ");
      zprnt(4,1,&lab.readoutcycle,"READOUT CYCLE INDEX = ");
      zprnt(7,1,&lab.bias,"OFFSET/BIAS/BIAS_STRIP_MEAN = ");
      zprnt(7,1,&lab.extpixval,"DARK/EXTENDED PIXEL VALUE = ");
      zprnt(7,1,&lab.sensortemp,"SENSOR HEAD ELEC TEMP = ");
      zprnt(7,1,&lab.datarate,"INSTRUMENT DATA RATE = ");
      zprintstr(8,lab.shuttermode,"SHUTTER MODE (NACONLY/WACONLY/BOTSIM) = ");
      zprintstr(3,lab.dlyreadout,"DELAYED READOUT FLAG = ");
      zprintstr(21,lab.imagetime,"IMAGETIME = ");
      zprnt(4,1,&lab.compparam,"COMPPARAM = ");
      zprnt(4,1,&lab.sccp,"SPACECRAFT CLOCK CNT PARTITION = ");
      zprnt(4,1,&lab.comdseqnum,"COMMAND SEQUENCE NUMBER = ");
      zprintstr(5,lab.filter1,"FILTER_NAME1 = ");
      zprintstr(5,lab.filter2,"FILTER_NAME2 = ");
      zprintstr(9,lab.mispacflg,"MISSING_PACKET_FLAG = ");
      zprintstr(10,lab.shutid,"SHUTTER STATE (ENABLED/DISABLED) = ");
      zprintstr(21,lab.ertstart,"EARTH_RECEIVED_START_TIME = ");
      zprintstr(21,lab.ertstop,"EARTH_RECEIVED_STOP_TIME = ");
      zprintstr(30,lab.fltsoftversid,"FLIGHT_SOFTWARE_VERSION_ID = ");
      zprintstr(21,lab.starttime,"START_TIME = ");
      zprnt(7,1,&lab.cld,"CALIBRATION_LAMP_DURATION = ");
      zprnt(4,1,&lab.elbias,"ELECTRONIC_BIAS = ");
      zprnt(4,1,&lab.pcvi,"PARALLEL_CLOCK_VOLTAGE_INDEX = ");
      zprnt(4,1,&lab.exppackets,"EXPECTED_PACKETS = ");
      zprnt(4,1,&lab.recpackets,"RECEIVED_PACKETS = ");
      zprnt(4,1,&lab.ordernum,"ORDER_NUMBER = ");
      zprnt(4,1,&lab.seqnum,"SEQUENCE_NUMBER = ");
      zprnt(4,1,&lab.validmax,"VALID_MAXIMUM = ");
      zprnt(4,1,&lab.validmaxfw,"VALID_MAXIMUM (FULL WELL) = ");
      zprnt(7,1,&lab.expmax,"EXPECTED_MAXIMUM (% DN) = ");
      zprnt(7,1,&lab.expmaxfw,"EXPECTED_MAXIMUM (% FULL WELL) = ");
      zprnt(7,1,&lab.compratepred,"INST_CMPRS_RATE (PREDICTED) = ");
      zprnt(7,1,&lab.comprateact,"INST_CMPRS_RATE (ACTUAL) = ");
      zprintstr(40,lab.dsid,"DATA_SET_ID = ");
      zprintstr(15,lab.ihn,"INSTRUMENT_HOST_NAME = ");
      zprintstr(38,lab.in,"INSTRUMENT_NAME = ");
      zprintstr(100,lab.note,"NOTE = ");
      zprintstr(21,lab.pctime,"PRODUCT_CREATION_TIME = ");
      zprintstr(40,lab.prid,"PRODUCT_ID = ");
      zprintstr(11,lab.pvt,"PRODUCT_VERSION_TYPE = ");
      zprintstr(30,lab.sid,"SEQUENCE_ID = ");
      zprintstr(120,lab.cmdname, "COMMAND_FILE_NAME = ");
      zprintstr(15,lab.imgobstype[0], "IMAGE_OBSERVATION_TYPE = ");
      for (i=1; i<5; i++) {
          zprintstr(15,lab.imgobstype[i], "                       = ");
      }
      zprintstr(60,lab.seqtitle, "SEQUENCE_TITLE = ");
      zprintstr(21,lab.midtime, "IMAGE_MID_TIME = ");
      zprintstr(250,lab.description, "DESCRIPTION = ");
      zprintstr(100,lab.methoddesc, "METHOD_DESC = ");
      zprintstr(75,lab.targetdesc, "TARGET_DESC = ");
      zprintstr(30,lab.targetlist[0], "TARGET_LIST = ");
      for (i=1; i<10; i++) {
          zprintstr(30,lab.targetlist[i], "            = ");
      }
      zprintstr(10,lab.telemid, "TELEMETRY_FORMAT_ID = ");
      zprnt(4,1,&ind,"IND = ");
}

zprintstr(n, buf, title)  /*  print title and then string of n characters.  */

int n;          /* number of characters to print  */
char *title;    /* string to be printed in front of data */
char *buf;        /* string to be printed*/
{
       char pbuf[251];
       int ns;
/*  ==================================================================  */

       if (strlen(title) + strlen(buf) > 250) {
         zvmessage(title,"");
         zvmessage(buf,"");
       }
       else {
         strcpy(pbuf,title);
         ns = n;
         if(n > 250 - strlen(title)) ns = 250 - strlen(title);
         strncat(pbuf,buf,ns);
         zvmessage(pbuf,"");
       }
}




