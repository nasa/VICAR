$!****************************************************************************
$!
$! Build proc for MIPL module getlabcon
$! VPACK Version 1.9, Monday, December 07, 2009, 16:20:24
$!
$! Execute by entering:		$ @getlabcon
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module getlabcon ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to getlabcon.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("getlabcon.imake") .nes. ""
$   then
$      vimake getlabcon
$      purge getlabcon.bld
$   else
$      if F$SEARCH("getlabcon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getlabcon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getlabcon.bld "STD"
$   else
$      @getlabcon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getlabcon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getlabcon.com -mixed -
	-s getlabcon.c -
	-i getlabcon.imake -
	-t tgetlabcon.f tzgetlabcon.c tgetlabcon.imake tgetlabcon.pdf -
	   tstgetlabcon.pdf -
	-o getlabcon.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getlabcon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getlabcon.imake
/* Imake file for VICAR subroutine GETLABCON */

#define SUBROUTINE getlabcon

#define MODULE_LIST getlabcon.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C

/*#define LIB_LOCAL    /* Remove upon delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tgetlabcon.f
C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    GETLABCON
C
C********************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44

      implicit integer*4 (a-z)
      integer*4 data(100)
      character*5 project

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call xvmessage('**********fortran callable***********',' ')
      call getproj(unit,project,cam,fds,ind)
      call xvmessage('GETLABCON:',' ')
      call getlabcon(unit,project,data,ind)      
      if(ind.eq.1) call prnt(4,1,ind,'warning indicator=.')
      if(ind.gt.1) call prnt(4,1,ind,'fatal indicator=.')
      if(data(1).eq.0) then
         call xvmessage('invalid label type',' ')
      else if(data(1).eq.1) then
         call xvmessage('ground calibration label',' ')
      else if(data(1).eq.2) then
         call xvmessage('flight label',' ')
      else
         call prnt(4,1,data(1),'data(1)=.')
      endif         
      call prnt(4,1,data(2), 'frame number          .')
      call prnt(7,1,data(3), 'exposure time  msec  .')
      call prnt(4,1,data(4), 'filter position       .')
      call prnt(4,1,data(5), 'frame or scan rate    .')
      call prnt(4,1,data(6), 'camera serial number  .')
      call prnt(4,1,data(7), 'gain state            .')
      call prnt(4,1,data(8), 'S/C event time year   .')
      call prnt(4,1,data(9), 'S/C event time day    .')
      call prnt(4,1,data(10),'S/C event time hour   .')
      call prnt(4,1,data(11),'S/C event time minute .')
      call prnt(4,1,data(12),'S/C event time second .')
      call prnt(4,1,data(13),'S/C event time milsec .')
      call prnt(4,1,data(14),'S/C ID                .')
      call prnt(4,1,data(15),'Camera Flood State    .')
      call prnt(4,1,data(16),'DC Offset State       .')
      call prnt(4,1,data(17),'FIBE                  .')
      call prnt(4,1,data(18),'Boom flag             .')
      call prnt(4,1,data(19),'Image Scale  m/pixel  .')
      call prnt(4,1,data(20),'FOV Height            .')
      call prnt(4,1,data(21),'FOV Width             .')
      call prnt(4,1,data(22),'Range                 .')
      call prnt(4,1,data(23),'clock                 .')
      call prnt(4,1,data(24),'Frame Start Count     .')
      call prnt(99,12,data(25),'target body in label is: ')
      call prnt(7,1,data(28),'DN to reflectance IOVF.')
      call prnt(7,1,data(29),'DN to radiance    CONV.')
      call prnt(7,1,data(30),'Range target to sun   .')
      call prnt(99,6,data(60),'input tape name ')
      call prnt(4,1,data(62),'Input  file #         .')
      call prnt(99,6,data(63),'output tape name ')
      call prnt(4,1,data(65),'Output file #         .')
      call prnt(99,10,data(31),'picno ')
      call prnt(99,10,data(40),'dark calibration file ')
      call prnt(99,10,data(45),'radiance cal file ')
      call prnt(99,10,data(50),'blemish correction file ')
      call prnt(99,14,data(55),'shutter offset file ')
      call prnt(4,1,data(66),'Earth rcvd time year  .')
      call prnt(4,1,data(67),'Earth rcvd time day   .')
      call prnt(4,1,data(68),'Earth rcvd time hour  .')
      call prnt(4,1,data(69),'Earth rcvd time minute.')
      call prnt(4,1,data(70),'Earth rcvd time second.')
      call prnt(4,1,data(71),'Uneven bit weighting  .')
      call prnt(4,1,data(72),'Camera ID (1 or 2)    .')
      call prnt(4,1,data(73),'Partition(#RIM cycles).')
      call xvmessage('  ',' ')
      call xvmessage('**********c callable***********',' ')
      call tzgetlabcon(unit)
      return
      end
$!-----------------------------------------------------------------------------
$ create tzgetlabcon.c
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







$!-----------------------------------------------------------------------------
$ create tgetlabcon.imake
/* Imake file for Test of VICAR subroutine getlabcon */

#define PROGRAM tgetlabcon

#define MODULE_LIST tgetlabcon.f tzgetlabcon.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define LIB_LOCAL        /* Remove upon delivery */
$!-----------------------------------------------------------------------------
$ create tgetlabcon.pdf
!*****************************************************************************
! TGETLABCON.PDF - pdf for test program TGETLABCON.F for GETLABCON
!*****************************************************************************
process 
PARM INP          COUNT=1     TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgetlabcon.pdf
!****************************************************************************
! TSTGETLABCON.PDF, unit test procedure for subroutine GETLABCON.F
!
! This PDF is written for both VMS and Unix
! The testings of projects WFPC1, WFPC2, VIS, MVM73 and MARINER 9 have been
!   removed due to the unavailability of these images.  Please add these test 
!   cases if images are available
!
! NOTE: Since this routine heavily depends on GETPROJ, the test PDF files are 
!       almost identical
!****************************************************************************
procedure help=*
  RefGbl $Echo
  RefGbl $SysChar
body
  local GllFlightFull     type=string
  local GllFlightSum      type=string
  local GllCalibFull      type=string
  local GllCalibSum       type=string
  local GllSum            type=string
  local VGR1              type=string
  local VGR2              type=string
  local CssCalib          type=string
  local CssFlight         type=string
  local CssS2             type=string
  local CssTour           type=string


  let $echo="no"
  if ($syschar(1) = "VAX_VMS")
    let GLLFlightFull = "wms_test_work:[testdata.mipl.gll]venus.img"
    let GLLFlightSum  = "wms_test_work:[testdata.mipl.gll]4600.byt"
    let GllCalibFull  = "wms_test_work:[testdata.mipl.gll]445.rad"
    let GLLCalibSum   = "wms_test_work:[testdata.mipl.gll]gllsumdrkcrnt.tst"
    let VGR1          = "wms_test_work:[testdata.mipl.vgr]f1636832.fic"
    let VGR2          = "wms_test_work:[testdata.mipl.vgr]uvh.img"
    let CssCalib      = "wms_test_work:[testdata.cassini.iss]labtest.img"
    let CssFlight    = "wms_test_work:[testdata.cassini.cas$i$ss]n1356781097.2"
    let CssS2= "wms_test_work:[testdata.cassini.cas$i$ss]n1308947518.182-142523"
    let CssTour   = "wms_test_work:[testdata.cassini.iss]$N1358285193_7.IMG"
  else ! Unix
    let GLLFlightFull = "/project/test_work/testdata/mipl/gll/venus.img"
    let GLLFlightSum  = "/project/test_work/testdata/mipl/gll/4600.byt"
    let GllCalibFull  = "/project/test_work/testdata/mipl/gll/445.rad"
    let GLLCalibSum   = "/project/test_work/testdata/mipl/gll/gllsumdrkcrnt.tst"
    let VGR1          = "/project/test_work/testdata/mipl/vgr/f1636832.fic"
    let VGR2          = "/project/test_work/testdata/mipl/vgr/uvh.img"
    let CssCalib      = "/project/test_work/testdata/cassini/iss/labtest.img"
    let CssFlight     = +
         "/project/test_work/testdata/cassini/casIss/n1356781097.2"
    let CssS2    = +
        "/project/test_work/testdata/cassini/casIss/n1308947518.182-142523"
    let CssTour    = +
        "/project/test_work/testdata/cassini/iss/N1358285193_7.IMG"
  end-if

  let $echo="yes"
 TGETLABCON INP=@GllFlightFull
 TGETLABCON INP=@GllFlightSum
 TGETLABCON INP=@GllCalibFull
 TGETLABCON INP=@GllCalibSum
 TGETLABCON INP=@VGR1
 TGETLABCON INP=@VGR2
 TGETLABCON INP=@CssCalib
 TGETLABCON INP=@CssFlight
 TGETLABCON INP=@CssS2
 TGETLABCON INP=@CssTour
  let $echo="no"

end-proc

$ Return
$!#############################################################################
$Other_File:
$ create getlabcon.hlp
1 GETLABCON

  Purpose: VICAR2 subroutine GETLABCON will return constants 
           contained in image labels for all flight projects.

  Fortran calling sequence:

	INTEGER*4   unit,ind,data(100)
	CHARACTER*5 project

        call getlabcon(unit,project,data,ind)

  C calling sequence:

	int unit,ind,data[100];
	char project[6];

	zgetlabcon(unit,project,data,&ind);

2 ARGUMENTS

  Input:

    unit   - unit number of file to read                   int
    project- spacecraft identity.                          char       
             valid;
	           MAR-9, MAR10, VIKOR, VGR-1, VGR-2, GLL, CASSI
                   and WFPC1 for space telescope (old optics),
                   and WFPC2 for space telescope (first optics upgrade)
  Output:

    data   - buffer for returned label constants.          mixed type      
             Should be at least 80 fullwords long.
    ind    - status on return.                             int
             0 =normal return
             1 =some data returned could not be determined.
             2 =problem encountered with the label itself.
                Probably the label belongs to another project.
             3 =invalid project code input

  Returned data:
-----------------------------------------------------------
| data(1)    - label type.                          integer
|              0=invalid label type detected.
|              1=ground calibration
|              2=flight label
| data(2)    - frame number                         integer
| data(3)    - exposure time.                       real
| data(4)    - filter position.                     integer
| data(5)    - frame or scan rate                   integer
| data(6)    - camera serial number.                integer
| data(7)    - gain state                           integer
| data(8)    - spacecraft event time year           integer
| data(9)    - spacecraft event time day            integer
| data(10)   - spacecraft event time hour           integer
| data(11)   - spacecraft event time minute         integer
| data(12)   - spacecraft event time second         integer
| data(13)   - spacecraft event time milsec         integer
| data(14)   - spacecraft id or serial #            integer
| data(15)   - camera flood state                   integer
| data(16)   - camera dc offset state               integer
| data(17)   - FIBE                                 integer
| data(18)   - boom flag                            integer
| data(19)   - image scale                          integer
| data(20)   - field of view height                 integer
| data(21)   - field of view width                  integer
| data(22)   - range to target body                 integer
| data(23)   - clock                                integer
| data(24)   - frame start count (FSC)              integer
| data(25-27)- target name                          ascii
| data(28)   - conversion factor DN to reflectance  real
|              Ficor IOVF value times 10**-4   
| data(29)   - conversion factor DN to radiance     real
|              Ficor CONV value times 10**-9
| data(30)   - range target body to sun (km)        real
| data(31-39)- picture number (PICNO)               ascii
| data(40-44)- dark current file name               ascii
| data(45-49)- radiometric calibration file name.   ascii
| data(50-54)- blemish file name                    ascii
| data(55-59)- shutter offset file name             ascii
| data(60-61)- input or EDR tape name               ascii
| data(62)   - input file number                    integer
| data(63-64)- output tape name                     ascii
| data(65)   - output file number                   integer
| data(66)   - earth received time year             integer
| data(67)   - earth received time day              integer
| data(68)   - earth received time hour             integer
| data(69)   - earth received time minute           integer
| data(70)   - earth received time second           integer
| data(71)   - uneven bit weighting correction      integer
| data(72)   - camera ID, NA/WA etc                 integer
| data(73)   - partition, #times RIM cntr reset     integer
-----------------------------------------------------------
  

  NOTES on data formats:
-----------------------------------------------------------
| index   MAR-9   MAR10   VIKOR   VGR     GLL	WFPC    CASSI	
|  1
|  2                              fds     sclk          sclk
|  3       msec    msec    msec   msec    msec          msec
|  4                              0-7     0-7           1000*filt1 + filt2
|  5                                      1=2 1/3, 2=8 2/3 3=30 1/2
|                                         4=60 2/3
|                                                       CDS pickup, packet/s
|  6        1       1    4=vo1B 4=vgr2 wa 1=Full Frame  1=nac 21=SUM2 41=SUM4
|           2       2    6=vo2B 5=vgr2 na 2=Summation   2=wac 22=SUM2 42=SUM4
|                        7=vo1A 6=vgr1 wa
|                        8=vo2A 7=vgr1 na     
|  7                              1=hi,0=lo 1=400k,2=100k,3=40k,4=10k
|                                                       3=12e/DN (40K)
|                                                       2=29e/DN (100K) 
|                                                       1=95e/DN (400K)
|                                                       0=215e/DN (1400K)
|  8
|  9
| 10
| 11
| 12
| 13
| 14
| 15
| 16
| 17                                      flood,invrt,blemprotect,extenexpo
| 18                                      2=boomobscured,1=maybe,0=not
| 19                      m/pixel
| 20
| 21
| 22
| 23                                      10*mod10+mod8 sclk
| 24
| 25-27                                   12 char       12 char
| 28
| 29
| 30
| 31-39                    7 char 10 char 7 char
| 40-44                                   20 char
| 45-49                                   20 char
| 50-54                                   20 char
| 55-59
| 60-61                           6 char  8 char
| 62
| 63-64                           6 char
| 65
| 66
| 67
| 68
| 69
| 70        
| 71                                      1=on,0=off
| 72                      1=a,2=b 1=na,2=wa             1=nac 2=wac
| 73
--------------------------------------------------------------


* indicates a valid field for this project:
(For testing purposes, see comments in tstgetlabcon.pdf.)
--------------------------------------------------------------
| index   MAR-9   MAR10   VIKOR   VGR     GLL	WFPC   CASSI
|  1                        *      *       *             *
|  2        *       *       *      *       *             *
|  3        *       *       *      *       *             *
|  4        *       *       *      *       *             *
|  5                               *       *             *
|  6        *       *       *      *       *             *
|  7                        *      *       *             *
|  8                        *      *       *	  *      *
|  9                        *      *       *	  *      *
| 10                        *      *       *	  *      *
| 11                        *      *       *	  *      *
| 12                        *      *       *	  *      *
| 13                        *      *       *	  *      *
| 14                        *      * 
| 15                        *                            *
| 16                        *                            *
| 17                                       *
| 18                                       *
| 19                        *
| 20                        *
| 21                        *
| 22                        *
| 23                                       *             *
| 24                        *
| 25-27                                    *             *
| 28        *       *       *      *       *   
| 29        *       *       *      *       *   
| 30                                       *
| 31-39                     *      *       *
| 40-44                                    *
| 45-49                                    *
| 50-54                                    *
| 55-59                                    *
| 60-61                            *       *
| 62                               *       *
| 63-64                            *
| 65                               *
| 66                               *                     *
| 67                               *                     *
| 68                               *                     *
| 69                               *                     *
| 70                               *                     *
| 71                                       *      
| 72                        *      *                     *
| 73                                       *             *
--------------------------------------------------------------

2 HISTORY

Written By: Jean Lorre        10/1/89
Cognizant Programmer: J Lorre
Source Language: C
Revision: CCA      26 Sep 90   FIXED FOR MAR10, ADDED MAR10 FILTER
                               LOAD DATA(2) WITH FSC FOR GETSPICE
          CCA       6 OCT 90   FIXED FOR MAR-9: DAS, EXP, CAM, FILT
          C Avis   26 OCT 90   FIXED FOR MAR10, ADDED MAR10 FILTER
                                   LOAD DATA(2) WITH FSC FOR GETSPICE
                                   FIXED FOR MAR-9: DAS, EXP, CAM, FILT 
          CCA       1 Mar 91   FOR VIKOR: ADDED SCET
          J Lorre  15 Aug 93   Added WFPC project
	  T Truong 19 AUG 93   PORTED TO UNIX
          SM Chang 23 Jul 96   Set Camera Serial Number=2 for Galileo SSI
                                   Summation mode (FR89116)
                               Blank out data[30]~data[38] (C term)
                                   before assignning
          T Huang  25 Aug 1998 Added call to SCET_UPDATE for project VGR.
          VRH      30 Jun 2001 Added Cassini
          VRH      23 Jan 2002 Corrected SCET to exposure midpoint for 
                                   VGR and Cassini for SPICE calcs

$ Return
$!#############################################################################
