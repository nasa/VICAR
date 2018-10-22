/*****************************************************************************

  Subroutine GETLABCON:

    Returns constants contained in image labels for all flight projects.

  Arguments:

    unit   - (INPUT, int) unit number of file to read.
    project- (INPUT, char) spacecraft identity.
                     valid:
                           MAR-9, MAR10, VIKOR, VGR-1, VGR-2, GLL,
                           and WFPC1 for space telescope (old optics),
                           and WFPC2 for space telescope (first optics upgrade)
    data   - (OUTPUT, mixed type) buffer for returned label constants.
                     Should be at least 80 fullwords long.
    ind    - (OUTPUT, int) status on return.
                 0 =normal return
                 1 =some data returned could not be determined.
                 2 =problem encountered with the label itself.
                    Probably the label belongs to another project.
                 3 =invalid project code input
******************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "cas_isslab.h"

void zgetlabcon ();
void zficorsub ();
void zmidpoint ();

/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/
void FTN_NAME2(getlabcon, GETLABCON) (int *unit, char *project, void *data,
				int *ind, ZFORSTR_PARAM)
 {
   ZFORSTR_BLOCK
   char proj[6];
   int length;
/*  ==================================================================  */

   length = 5;

/* 4 args for GETLABCON, project is 2nd arg and 1st string   */


   zsfor2c(proj, length, project, &unit, 4, 2, 1, ind);

   zgetlabcon(*unit,proj,data,ind);

 }
/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/

      void zgetlabcon(unit,project,data,ind)

      int unit,*ind;
      int data[80];
      char *project;
{
      int buf[50],bufsize,status;
      int nmiss, ngmt;
      float rmiss,temp;
      char lbuf[7200], *ist;
      void *ptr;
      able97_typ casbuf;

      *ind=0;

/* initialize the data array */
	nmiss = -999;
        rmiss = -999.0;

	zmve(4,80,&nmiss,data,0,1);
	zmve(4,1,&rmiss,&data[2],0,1);
	zmve(4,3,&rmiss,&data[27],0,1);
	memset(&data[24],32,4*3);
	memset(&data[30],32,4*31);
	memset(&data[62],32,4*2);



/* space telescopes WFPC */

      if(strcmp(project,"WFPC1") == 0 || strcmp(project,"WFPC2") == 0)
	{
         status = zlget(unit,"HISTORY","UT_DATE_YEAR",&data[7],
                    "FORMAT","INT","HIST","PERSLAB",NULL);
         if(status != 1) *ind=2;
         status = zlget(unit,"HISTORY","UT_DATE_DAY",&data[8],
                    "FORMAT","INT","HIST","PERSLAB",NULL);
         if(status != 1) *ind=2;
         status = zlget(unit,"HISTORY","UT_DATE_HOUR",&data[9],
                    "FORMAT","INT","HIST","PERSLAB",NULL);
         if(status != 1) *ind=2;
         status = zlget(unit,"HISTORY","UT_DATE_MINUTE",&data[10],
                    "FORMAT","INT","HIST","PERSLAB",NULL);
         if(status != 1) *ind=2;
         status = zlget(unit,"HISTORY","UT_DATE_SECOND",&data[11],
                    "FORMAT","INT","HIST","PERSLAB",NULL);
         if(status != 1) *ind=2;
         data[12]=0;
        }

/* Cassini */

      else if (strncmp(project,"CASSI", 5) == 0)
        {
         zable97(ind,unit,&casbuf);
         if(*ind < 0) *ind=1;      /* some label items missing */

         /* label type */
         if(strncmp(casbuf.labtype,"ISSGRND", 7) == 0) data[0]=1;
         if(strncmp(casbuf.labtype,"ISSFLT", 6) == 0) data[0]=2;
         if(data[0] == -999) *ind=2;

         data[1]=casbuf.sclk;     /* frame number: image_number */
	 zmve(4,1,&casbuf.expos,&data[2],1,1);

         /* camera serial number combines camera number and summation mode */
         if(strncmp(casbuf.camera,"ISSNA",5) == 0) data[5]=1;
         if(strncmp(casbuf.camera,"ISSWA",5) == 0) data[5]=2;
         if(strncmp(casbuf.mode,"SUM2",4) == 0)    data[5]+=20;
         if(strncmp(casbuf.mode,"SUM4",4) == 0)    data[5]+=40;

         /* filter position: This is a bit complicated since there
            are two filter wheels on Cassini.  Filter number here
            is encoded as 1000*Filter1_index + Filter2_index */

         if(strncmp(casbuf.camera,"ISSNA",5) == 0)
           {
            if(strncmp(casbuf.filter1,"CL1",3) == 0) data[3]=1000;
            if(strncmp(casbuf.filter1,"RED",3) == 0) data[3]=2000;
            if(strncmp(casbuf.filter1,"BL1",3) == 0) data[3]=3000;
            if(strncmp(casbuf.filter1,"UV2",3) == 0) data[3]=4000;
            if(strncmp(casbuf.filter1,"UV1",3) == 0) data[3]=5000;
            if(strncmp(casbuf.filter1,"IRP0",4) == 0) data[3]=6000;
            if(strncmp(casbuf.filter1,"P120",4) == 0) data[3]=7000;
            if(strncmp(casbuf.filter1,"P60",3) == 0) data[3]=8000;
            if(strncmp(casbuf.filter1,"P0",2) == 0) data[3]=9000;
            if(strncmp(casbuf.filter1,"HAL",3) == 0) data[3]=10000;
            if(strncmp(casbuf.filter1,"IR4",3) == 0) data[3]=11000;
            if(strncmp(casbuf.filter1,"IR2",3) == 0) data[3]=12000;
            if(strncmp(casbuf.filter2,"CL2",3) == 0) data[3]+=1;
            if(strncmp(casbuf.filter2,"GRN",3) == 0) data[3]+=2;
            if(strncmp(casbuf.filter2,"UV3",3) == 0) data[3]+=3;
            if(strncmp(casbuf.filter2,"BL2",3) == 0) data[3]+=4;
            if(strncmp(casbuf.filter2,"MT2",3) == 0) data[3]+=5;
            if(strncmp(casbuf.filter2,"CB2",3) == 0) data[3]+=6;
            if(strncmp(casbuf.filter2,"MT3",3) == 0) data[3]+=7;
            if(strncmp(casbuf.filter2,"CB3",3) == 0) data[3]+=8;
            if(strncmp(casbuf.filter2,"MT1",3) == 0) data[3]+=9;
            if(strncmp(casbuf.filter2,"CB1",3) == 0) data[3]+=10;
            if(strncmp(casbuf.filter2,"IR3",3) == 0) data[3]+=11;
            if(strncmp(casbuf.filter2,"IR1",3) == 0) data[3]+=12;
           }
         else if(strncmp(casbuf.camera,"ISSWA",5) == 0)
           {
            if(strncmp(casbuf.filter1,"CL1",3) == 0) data[3]=1000;
            if(strncmp(casbuf.filter1,"IR3",3) == 0) data[3]=2000;
            if(strncmp(casbuf.filter1,"IR4",3) == 0) data[3]=3000;
            if(strncmp(casbuf.filter1,"IR5",3) == 0) data[3]=4000;
            if(strncmp(casbuf.filter1,"CB3",3) == 0) data[3]=5000;
            if(strncmp(casbuf.filter1,"MT3",3) == 0) data[3]=6000;
            if(strncmp(casbuf.filter1,"CB2",3) == 0) data[3]=7000;
            if(strncmp(casbuf.filter1,"MT2",3) == 0) data[3]=8000;
            if(strncmp(casbuf.filter1,"IR2",3) == 0) data[3]=9000;
            if(strncmp(casbuf.filter2,"CL2",3) == 0) data[3]+=1;
            if(strncmp(casbuf.filter2,"RED",3) == 0) data[3]+=2;
            if(strncmp(casbuf.filter2,"GRN",3) == 0) data[3]+=3;
            if(strncmp(casbuf.filter2,"BL1",3) == 0) data[3]+=4;
            if(strncmp(casbuf.filter2,"VIO",3) == 0) data[3]+=5;
            if(strncmp(casbuf.filter2,"HAL",3) == 0) data[3]+=6;
            if(strncmp(casbuf.filter2,"IRP90",5) == 0) data[3]+=7;
            if(strncmp(casbuf.filter2,"IRP0",4) == 0) data[3]+=8;
            if(strncmp(casbuf.filter2,"IR1",3) == 0) data[3]+=9;
           }
      
         /* scan rate: CDS pickup rate in packets per second */
         if(abs((int)(casbuf.datarate - 60.9)) == 0) data[4]= 8;
         if(abs((int)(casbuf.datarate - 121.9)) == 0) data[4]=16;
         if(abs((int)(casbuf.datarate - 182.8)) == 0) data[4]=24;
         if(abs((int)(casbuf.datarate - 243.7)) == 0) data[4]=32;
         if(abs((int)(casbuf.datarate - 365.6)) == 0) data[4]=48;

         /* gain state */
         if(strncmp(casbuf.gain,"40K",3) == 0) data[6]=3;    /* ground */
         if(strncmp(casbuf.gain,"100K",4) == 0) data[6]=2;
         if(strncmp(casbuf.gain,"400K",4) == 0) data[6]=1;
         if(strncmp(casbuf.gain,"1400K",5) == 0) data[6]=0;
         if(strncmp(casbuf.gain,"12 ",3) == 0) data[6]=3;     /* flight */
         if(strncmp(casbuf.gain,"29 ",3) == 0) data[6]=2;
         if(strncmp(casbuf.gain,"95 ",3) == 0) data[6]=1;
         if(strncmp(casbuf.gain,"215 ",4) == 0) data[6]=0;

         /* SCET */
         data[7]=casbuf.scety;
         data[8]=casbuf.scetd;
         data[9]=casbuf.sceth;
         data[10]=casbuf.scetm;
         data[11]=casbuf.scets;
         data[12]=casbuf.scetms;

         /* Correct time to midpoint of exposure */
         zmidpoint(&data[7],&data[8],&data[9],&data[10],&data[11],&data[12],
                   &casbuf.expos);

         if(strncmp(casbuf.ltfld,"OFF",2) == 0) data[14]=0; /* light flood */
         if(strncmp(casbuf.ltfld,"ON",2) == 0) data[14]=1;  /* light flood */
         data[15]=casbuf.elbias;  /* dc offset state: electronic bias */
         data[22]=casbuf.sclk;    /* spacecraft clock*/

         /* first 12 characters of target name */
         if(strlen(casbuf.target) == 0)
            memset(&data[24],32,12);
         else if(strlen(casbuf.target) < 12) 
            zmve(1,strlen(casbuf.target),casbuf.target,&data[24],1,1);
         else 
            zmve(1,12,casbuf.target,&data[24],1,1);

         /* ERT */
         data[65]=casbuf.scety2;
         data[66]=casbuf.scetd2;
         data[67]=casbuf.sceth2;
         data[68]=casbuf.scetm2;
         data[69]=casbuf.scets2;

         /* camera ID */
         if(strncmp(casbuf.camera,"ISSNA",5) == 0) data[71]=1;
         if(strncmp(casbuf.camera,"ISSWA",5) == 0) data[71]=2;

         data[72]=casbuf.sccp;  /* SCLK partition */
        }

/* galileo flight label */
 
      else if (strncmp(project,"GLL", 3) == 0)
        {
	 buf[0]=48;
	 zable86(&(*ind),unit,buf);
         if(*ind == -1) *ind=1;        /* one or more items missing */
         if(buf[0] == 0)
	   {
            *ind=2;
            zvmessage("GETLABCON: able86 detected invalid label","");
            return;
	   }
         zmve(4,5,&buf[0],&data[0],1,1);
         /* FR89117, make SSI Summation to have Camera SN of 2 */
         if (buf[4]==1 || buf[4]==5)
           data[5]=2;
         else
           data[5]=1;

         data[16]=buf[5];
         data[17]=buf[6];
         data[6]=buf[7];
         data[22]=buf[8];
         zmve(4,6,&buf[9],&data[7],1,1);

         /* blank out data[30]~data[38] because this particular data field
          * is bigger than buf */
         memset(&data[30],' ', 36);
         zmve(1,7,&buf[46],&data[30],1,1);    /* PICNO */

         zmve(4,3,&buf[16],&data[24],1,1);    /* TARGET */
         data[29]=buf[21];
         zmve(4,5,&buf[22],&data[39],1,1);    /* DC */
         zmve(4,5,&buf[27],&data[44],1,1);    /* Radio Metric */
         zmve(4,5,&buf[32],&data[49],1,1);    /* Blemish  */
         zmve(4,5,&buf[37],&data[54],1,1);    /* Shutter Offset */
         zmve(4,2,&buf[42],&data[59],1,1);    /* EDR */
         data[61]=buf[44];
         data[70]=buf[45];                    /* UBWC OAFLAG */
         zmve(7,1,&buf[19],&temp,1,1);
         temp = temp * 1.0e-4;
	  zmve(4,1,&temp,&data[27],1,1);      /* IOF */
         zmve(7,1,&buf[20],&temp,1,1);
         temp = temp * 1.0e-12;
	  zmve(4,1,&temp,&data[28],1,1);      /*  CONV  */
         data[72]=buf[15];                    /* partition */
        }

/* voyager */

      else if(strcmp(project,"VGR-1") == 0 || strcmp(project,"VGR-2") == 0)
        {
         buf[0]=39;
         zable77v2(&(*ind),unit,buf);
         if(*ind == -1) *ind=1;
         if(buf[0] != 3)
	   {
            *ind=2;
            zvmessage("GETLABCON: able77v2 detected invalid label","");
            return;
	   }
         zmve(4,6,&buf[0],&data[0],1,1);
         if(data[0] == 3)
           data[0]=2;
         else
           data[0]=0;
         data[71]=buf[6];
         data[6]=buf[7];
         zmve(4,5,&buf[9],&data[7],1,1);
         data[13]=buf[18];
         zmve(4,5,&buf[19],&data[30],1,1);
         zmve(4,2,&buf[28],&data[59],1,1);
         zmve(4,2,&buf[30],&data[62],1,1);
         data[61]=buf[32];
         zmve(4,6,&buf[33],&data[64],1,1);
         bufsize=7200;
         status = zlgetlabel(unit,lbuf,&bufsize);
         if(status != 1)
	   {
            *ind=2;
            zvmessage("GETLABCON: xlgetlabel error","");
	   }
         zficorsub(lbuf,&data[27],&data[28]);

         /* txh::verify and/or update scet date */
         zscet_update (project, data);

         /* Correct time to midpoint of exposure */
         ptr = &data[2];
         zmidpoint(&data[7],&data[8],&data[9],&data[10],&data[11],&data[12],
                   ptr);

        }

/* viking orbiter */

      else if(strcmp(project,"VIKOR") == 0)
	{
         zvolabv2(&(*ind),unit,buf);
         if(*ind == 20)
	   {
            *ind=2;
            zvmessage("GETLABCON: volabv2 detected invalid label","");
            return;
	   }
         if(*ind != 0)
	   {
            *ind=2;
            zvmessage("GETLABCON: volabv2 troubles with label","");
            return;
	   }
         data[0]=buf[0];
         data[5]=buf[1];			/* camera sn */
         zmve(4,6,&buf[18],&data[7],1,1);           /* OET (SCET?) */

         data[13]=buf[2];			/* sc # */
         data[71]=buf[3];			/* camera id */
         data[1]=buf[6];    			/* FSC */
         data[23]=buf[6];
         data[3]=buf[7];				/* filter */
         temp=buf[8];
         zmve(7,1,&temp,&data[2],1,1);		/* exposure */
         data[14]=buf[9];			/* flood state */
         data[6]=buf[10];			/* gain state */
         data[15]=buf[11];			/* dc offset */
         zmve(4,4,&buf[12],&data[18],1,1);     /* scale,fovh,fovw,range */
         ptr = &buf[16];
         sscanf(ptr,"%s",&data[30]);           /* picno */
         bufsize=7200;
         status = zlgetlabel(unit,lbuf,&bufsize);
         if(status != 1)
	   {
            *ind=2;
            zvmessage("GETLABCON: xlgetlabel error","");
	   }
         zficorsub(lbuf,&data[27],&data[28]);
        }

/* mariner 10 */

      else if(strcmp(project,"MAR10") == 0)
        {
         bufsize=7200;
         status = zlgetlabel(unit,lbuf,&bufsize);
         if(status != 1)
	   {
            *ind=2;
            zvmessage("GETLABCON: xlgetlabel error","");
	   }
         zficorsub(lbuf,&data[27],&data[28]);

         ist = strstr(lbuf,"MVM73");
         data[0] = 2;				/* flight label */
         data[5] = 1;				/* camera number */
         if(strncmp(ist+89,"B",1) == 0) data[5] = 2;

         ist = strstr(lbuf,"FDS=");
         sscanf(ist+4,"%d",&data[1]);              /* fds */
         ist = strstr(lbuf,"ERT  YR=");
         sscanf(ist+8,"%d",&data[65]);		    /* ert year */
         ist = strstr(lbuf,"DAY=");
         sscanf(ist+4,"%d",&data[66]);		    /* ert day */
         ist = strstr(lbuf,"GMT=");
         sscanf(ist+4,"%d",&data[67]);             /* ert hour */
	 sscanf(ist+7,"%d",&data[68]);	            /* ert minute */
	 sscanf(ist+10,"%d",&data[69]);	            /* ert second */
         ist = strstr(lbuf,"NOMINAL EXPOSURE=");
	 sscanf(ist+17,"%f",&buf[2]);             /* exposure */
         zmve(4,1,&buf[2],&data[2],1,1);
         ist = strstr(lbuf,"FILTER=");
         sscanf(ist+7,"%d",&data[3]);              /* filter */
        }

/* mariner 9 */

      else if(strcmp(project,"MAR-9") == 0)
        {
         bufsize=7200;
         status = zlgetlabel(unit,lbuf,&bufsize);
         if(status != 1)
	   {
            *ind=2;
            zvmessage("GETLABCON: xlgetlabel error","");
	   }
         zficorsub(lbuf,&data[27],&data[28]);

         ist = strstr(lbuf,"MARINER 9");
         data[0] = 2;				/* flight label */
         data[5] = 1;				/* camera number */
         if(strncmp(ist+14,"B",1) == 0) data[5] = 2;
         sscanf(ist+58,"%d",&data[1]);		/* fds */

         ist = strstr(lbuf,"PICTURE NUMBER ");
         sscanf(ist+15,"%s",&data[30]);  	/* picno */
         ist = strstr(lbuf,"YR");
         sscanf(ist+2,"%d",&data[65]);		/* ert year */
         ist = strstr(lbuf,"DAY");
         sscanf(ist+3,"%d",&data[66]);		/* ert day */
         ist = strstr(lbuf,"GMT");
	 sscanf(ist+3,"%d",&ngmt);
	 data[67] = ngmt/10000;			/* ert hour */
	 ngmt = ngmt - data[67]*10000;
	 data[68] = ngmt/100;			/* ert minute */
	 data[69] = ngmt - data[68]*100;	/* ert second */
         ist = strstr(lbuf,"EXP TIME");
	 sscanf(ist+8,"%f",&data[2]); 	/* exposure */

         ist = strstr(lbuf,"FILTER POS ");
	 if (strncmp(ist+11,"*",1) == 0)
           {
		data[3] = -1;			/* missing filter */
		*ind = 1;
	   }
	 else
	        sscanf(ist+11,"%d",&data[3]);
		
        }
	
/* unrecognizable project */
	
      else
        {
         zvmessage("GETLABCON: illegal project input","");
         *ind=3;
        }

}
/*****************************************************************************
      SUBROUTINE ZFICORSUB(BUF3,iovf,conv)
  RETURNS CONV VALUE FROM FICOR LABEL ASSUMING LABEL IS IN UNITS OF
  NANOWATTS/(CM**2,ST,NM,DN)  RETURNS  WATTS/(CM**2,ST,NM,DN)
  returns I/F value too.
******************************************************************************/
      void zficorsub(buf3,iovf,conv)

      char buf3[7200];
      float *iovf,*conv;

{   
      float temp;
      char *iptr;
/* retrieve ficor conv value */

        iptr=strstr(buf3,"NM MULTIPLY DN VALUE BY");
        if(iptr == NULL)
          *conv=0.0;
        else
          {
          sscanf(iptr+23,"%f",&temp);
          *conv=temp*1.0e-12;
          }

/* retrieve ficor i/f value */

        iptr=strstr(buf3,"., MULTIPLY DN VALUE BY");
        if(iptr == NULL)
          *iovf=0.0;
        else
          {
          sscanf(iptr+23,"%f",&temp);
          *iovf=temp*1.0e-4;
          }

}
/*****************************************************************************
      SUBROUTINE ZMIDPOINT(year,date,hour,min,sec,msec,expos)
   Routine zmidpoint adjusts time components to change exposure time to 
   midpoint for missions where shutter time is shutter close, e.g. VGR, Cassini.
******************************************************************************/
      void zmidpoint(year,date,hour,min,sec,msec,expos)

      int *year, *date, *hour, *min, *sec, *msec;
      float *expos;
{
      int leap, milli;
      long days, isec;
      double seconds;

      if(*msec != -999) milli = *msec;
      else milli = 0;                    /* in case msec not set */

     /* Calculate seconds past Jan 0, 1901 - this code doesn't 
        account for SPICE added leap seconds but this only affects 
        exposures which cross a leap second boundry */

      leap = (*year-1901)/4 - (*year-2000)/100 + (*year-2000)/400;
      seconds = *sec + 60.*(*min) + 3600.*(*hour) + 86400.*(*date) +
                31536000.*(*year-1901) + 86400.*leap + milli/1000.;

     /* Calculate midpoint correcting for exposure time in millisec*/

      seconds -= *expos/2000.;

     /* Convert back to year, date... */

     if(*msec != -999) *msec = 1000.*fmod(seconds+0.0001,1.);
     days = seconds/86400.;
     isec = (seconds - days*86400.);
     *hour = isec/3600;
     *min = (isec%3600)/60;
     *sec = isec%60;

     *year = 1901 + days/365;
     leap = (*year-1901)/4 - (*year-2000)/100 + (*year-2000)/400;
     *date = days - (*year-1901)*365 - leap; 
     if (*date < 1) {
       (*year)--;
       leap = (*year-1901)/4 - (*year-2000)/100 + (*year-2000)/400; 
       *date = days - (*year-1901)*365 - leap; 
     }

}
