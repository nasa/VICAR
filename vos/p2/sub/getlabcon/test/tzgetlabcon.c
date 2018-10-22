#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  bridge to C callable version of TGETLABCON.F */
/************************************************************************/

void FTN_NAME(tzgetlabcon)(unit)
  int *unit;
{

  int data[100],cam,fds,ind;
  char project[6];

      zgetproj(*unit,project,&cam,&fds,&ind);

      zvmessage("ZGETLABCON:","");
      zgetlabcon(*unit,project,data,&ind);      
      if(ind == 1) zprnt(4,1,&ind,"warning indicator=.");
      if(ind > 1) zprnt(4,1,&ind,"fatal indicator=.");

      if(data[0] == 0)
         zvmessage("invalid label type","");
      else if(data[0] == 1)
         zvmessage("ground calibration label","");
      else if(data[0] == 2)
         zvmessage("flight label","");
      else
         zprnt(4,1,&data[0],"data[0]=.");

      zprnt(4,1,&data[1], "frame number          .");
      zprnt(7,1,&data[2], "exposure time  msec  .");
      zprnt(4,1,&data[3], "filter position       .");
      zprnt(4,1,&data[4], "frame or scan rate    .");
      zprnt(4,1,&data[5], "camera serial number  .");
      zprnt(4,1,&data[6], "gain state            .");
      zprnt(4,1,&data[7], "S/C event time year   .");
      zprnt(4,1,&data[8], "S/C event time day    .");
      zprnt(4,1,&data[9],"S/C event time hour   .");
      zprnt(4,1,&data[10],"S/C event time minute .");
      zprnt(4,1,&data[11],"S/C event time second .");
      zprnt(4,1,&data[12],"S/C event time milsec .");
      zprnt(4,1,&data[13],"S/C ID                .");
      zprnt(4,1,&data[14],"Camera Flood State    .");
      zprnt(4,1,&data[15],"DC Offset State       .");
      zprnt(4,1,&data[16],"FIBE                  .");
      zprnt(4,1,&data[17],"Boom flag             .");
      zprnt(4,1,&data[18],"Image Scale  m/pixel  .");
      zprnt(4,1,&data[19],"FOV Height            .");
      zprnt(4,1,&data[20],"FOV Width             .");
      zprnt(4,1,&data[21],"Range                 .");
      zprnt(4,1,&data[22],"clock                 .");
      zprnt(4,1,&data[23],"Frame Start Count     .");
      zprnt(99,12,&data[24],"target body in label is: ");
      zprnt(7,1,&data[27],"DN to reflectance IOVF.");
      zprnt(7,1,&data[28],"DN to radiance    CONV.");
      zprnt(7,1,&data[29],"Range target to sun   .");
      zprnt(99,6,&data[59],"input tape name ");
      zprnt(4,1,&data[61],"Input  file #         .");
      zprnt(99,6,&data[62],"output tape name ");
      zprnt(4,1,&data[64],"Output file #         .");
      zprnt(99,10,&data[30],"picno ");
      zprnt(99,10,&data[39],"dark calibration file ");
      zprnt(99,10,&data[44],"radiance cal file ");
      zprnt(99,10,&data[49],"blemish correction file ");
      zprnt(99,14,&data[54],"shutter offset file ");
      zprnt(4,1,&data[65],"Earth rcvd time year  .");
      zprnt(4,1,&data[66],"Earth rcvd time day   .");
      zprnt(4,1,&data[67],"Earth rcvd time hour  .");
      zprnt(4,1,&data[68],"Earth rcvd time minute.");
      zprnt(4,1,&data[69],"Earth rcvd time second.");
      zprnt(4,1,&data[70],"Uneven bit weighting  .");
      zprnt(4,1,&data[71],"Camera ID (1 or 2)    .");
      zprnt(4,1,&data[72],"Partition(#RIM cycles).");
      zvmessage("  ","");
}







