$!****************************************************************************
$!
$! Build proc for MIPL module getproj
$! VPACK Version 1.9, Thursday, August 28, 2014, 13:10:31
$!
$! Execute by entering:		$ @getproj
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
$ write sys$output "*** module getproj ***"
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
$ write sys$output "Invalid argument given to getproj.com file -- ", primary
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
$   if F$SEARCH("getproj.imake") .nes. ""
$   then
$      vimake getproj
$      purge getproj.bld
$   else
$      if F$SEARCH("getproj.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getproj
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getproj.bld "STD"
$   else
$      @getproj.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getproj.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getproj.com -mixed -
	-s getproj.c -
	-i getproj.imake -
	-t tgetproj.f tzgetproj.c tgetproj.imake tgetproj.pdf tstgetproj.pdf -
	-o getproj.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getproj.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**********************************************************************

    vicar2 subroutine GETPROJ

    Returns the project id from an image with unit number.

    unit   - unit number of file containing picture (input)  integer*4
    project- spacecraft identity                    (output) character*5
             valid are: GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9
                        WFPC1 (original)
                        WFPC2 (after first optics upgrade)
	                CASSI    (Cassini)
			MPF   (Mars Pathfinder)
    camera - camera serial number                   (output) integer*4
             For WFPC, 1=PC 2=WF cameras
             For GLL, 1=SSI 2=SSI summation
             For CASSI, 1=ISSNA, 2=ISSWA, 21=ISSNA SUM2, 22=ISSWA SUM2,
                       41=ISSNA SUM4, 42=ISSWA SUM4
	     For MPF, 0 always (add if needed, better via MPF label routines)
    fds    - image number                           (output) integer*4
             For VGR is the fds count
             For GLL is the sclk count
	     For WFPC, 0=fds not applicable
             For CASSI, is the sclk count
	     For MPF, 0 always (add if needed, better via MPF label routines)
    ind    - 0=normal   1=error                     (output) integer*4

   Revision History
    SP   2-97  Changed to use the C calling sequence for zable97.  Added "!"
               before strcmp for Cassini camera determination; then changed
               to use strncmp, because character data in structure is not null-
               terminated.
   VRH   4-01  Changed camera to reflect summation mode
   VRH   4-03  Cassini Tour projects will have 'CASSINI-HUYGENS' as MISSION.
**********************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"
#include "strcasecmp.h"
#include "getproj.h"
#include "zvproto.h"
#include "prnt.h"
#include <string.h>
#include <stdio.h>

/*
* declarations of functions used by getproj
*/
void zable86(int *ind, int unit, int *buf);
void zable77v2(int *ind, int unit, void *arra);
void zvolabv2(int *ind, int unit, void *lbuff);
void zable97(int *ind, int unit, void *lab);

/*---------------------------------------------------------------------------*/
/* fortran-callable version                                                  */
/*---------------------------------------------------------------------------*/
void FTN_NAME2(getproj, GETPROJ) (int *unit, char *project, int *camera,
			int *fds, int *ind, ZFORSTR_PARAM)
#if 0
  int     *unit; /* the unit of the file to be read (input)  */
  char *project; /* spacecraft identity             (output) */
  int   *camera; /* camera serial number            (output) */
  int      *fds; /* image number                    (output) */
  int      *ind; /* returned status                 (output) */
#endif
{
  ZFORSTR_BLOCK
  char proj[6];
  int length;

  length = 5;

/* 5 args for getproj, project is 2nd arg and 1st string */

   zgetproj(*unit,proj,camera,fds,ind);

   zsc2for(proj, length, project, &unit, 5, 2, 1, ind);
   return;
}
/*---------------------------------------------------------------------------*/
/* c-callable version                                                        */
/*---------------------------------------------------------------------------*/
  void    zgetproj(unit,project,camera,fds,ind)

      char *project;
      int  *ind, unit, *camera, *fds;

{
      char labi[3600],*ist;
      int bufsize,stat,iind;
      int buf[160],vgrarr[10];
      able97_typ casbuf;
      int status;

      bufsize = 3600;
      stat = zlgetlabel(unit,labi,&bufsize);
/* check status */
      if(stat != 0 && stat != 1)
	{
	  zvmessage("ERROR IN ZLGETLABEL, STAT =","");
	  zprnt(4,1,&stat,".");
	  return;
	}  

      *ind = 0;

/* galileo flight label */

      if(strstr(labi,"MISSION='GALILEO'") != NULL)
	{

          strcpy(project,"GLL  ");
          buf[0] = 5;

          zable86(ind,unit,buf);
    
           if(*ind != 0)
	     {
              zvmessage("GETPROJ: ABLE86 label error","");  
              *ind = 1;
	     }

          *fds = buf[1];
          if (buf[4]==1 || buf[4]==5)
            *camera = 2;
          else
            *camera = 1;
	  return;
	}     

/* galileo calibration label*/

       if(strstr(labi,"LAB01='GLL/SSI") != NULL)
	 {
           strcpy(project, "GLL  ");
           buf[0] = 5;
           zable86(ind,unit,buf);
           if(*ind != 0)
	   {
              zvmessage("GETPROJ: ABLE86 label error","");  
              *ind = 1;
           }

          *fds = buf[1];

          if (buf[4]==1 || buf[4]==5)
            *camera=2;
          else
            *camera=1;
	  return;
	 } 

/* voyager 2 */

       if(strstr(labi,"VGR-2") != NULL)
	 {
           strcpy(project, "VGR-2");
           vgrarr[0] = 10;
           zable77v2(ind,unit,vgrarr);
           if(*ind != 0)
	   {
              zvmessage("GETPROJ: ABLE77V2 label error","");
             *ind = 1;
           }
          *camera = vgrarr[5];
          *fds = vgrarr[1];
	  return;
	 }
/* voyager 1 */

       if(strstr(labi,"VGR-1") != NULL)
	 {
           strcpy(project, "VGR-1");
           vgrarr[0] = 10;
           zable77v2(ind,unit,vgrarr);
           if(*ind != 0)
	   {
             zvmessage("GETPROJ: ABLE77V2 label error","");
            *ind = 1;
           }
           *camera = vgrarr[5];
           *fds = vgrarr[1];
	   return;
	 }
/* Space Telescope WFPC1 */

       if(strstr(labi,"INSTRUME=  WFPC") != NULL)
	 {
            strcpy(project,"WFPC1");
            if(strstr(labi,"CAMERA  =  PC") != NULL) 
	       *camera = 1;
            else if(strstr(labi,"CAMERA  =  WF") != NULL) 
	       *camera = 2;
            else
	     {
               zvmessage("GETPROJ: WFPC1 cannot find camera id","");
               *ind = 1;
	     }
	    *fds = 0;
	    return;
          }

/* Space Telescope WFPC2
   The actual key is unknown at this time (WFPC2) */

       if(strstr(labi,"INSTRUME=  WFPC2") != NULL)
	 {
            strcpy(project,"WFPC2");
            if(strstr(labi,"CAMERA  =  PC") != NULL)
               *camera = 1;
            else if(strstr(labi,"CAMERA  =  WF") != NULL)
               *camera = 2;
            else
	     {
               zvmessage("GETPROJ: WFPC2 cannot find camera id","");
               *ind = 1;
	     }
	    *fds = 0;
	    return;
          }

/* viking orbiter */

       if((strstr(labi,"VIS ") != NULL) ||
               (strstr(labi,"VO75 ") != NULL))
	{
            strcpy(project,"VIKOR");
            zvolabv2(ind,unit,buf);
            if(*ind != 0 || buf[0] == 0) 
	    {
              zvmessage("GETPROJ: VOLABV2 label error","");
             *ind = 1;
            }
           *camera = buf[1];
           *fds = buf[6];
	   return;
	 }	   
/* mariner 10 */

       if(strstr(labi,"MVM73") != NULL)
	 {
            ist = strstr(labi,"MVM73");
            strcpy(project, "MAR10");
           *camera = 1;

            if(strncmp(ist+89,"B",1) == 0)
	        *camera = 2;
	    iind = sscanf(ist+13,"%d",fds);
            *ind = 1;                       /*error */
	    if(iind  >  0) 
	      *ind=0;	                   /* no error */
	    return;
	 }
/* mariner 9 */

       if(strstr(labi,"MARINER 9") != NULL)
	 {
	    ist = strstr(labi,"MARINER 9");
            strcpy(project, "MAR-9");
           *camera = 1;
            if(strncmp(ist+14,"B",1) == 0) 
	       *camera = 2;
            iind = sscanf(ist + 58,"%d",fds);
	   *ind = 1;		   /* error */
	    if(iind  >  0)
	       *ind = 0;          /* no error */
	    return;
	 }

/* Cassini Orbiter */

      if((strstr(labi,"MISSION_NAME='CASSINI'") != NULL) ||
         (strstr(labi,"MISSION_NAME='CASSINI-HUYGENS'") != NULL))
	{
          strcpy(project,"CASSI");
          zable97(ind,unit,&casbuf);
    
          if(*ind != 0)
	     {
              zvmessage("GETPROJ: ABLE97 label error","");  
              *ind = 1;
	     }
          *fds = casbuf.sclk;
          if(!strncmp(casbuf.camera,"ISSNA",5))
             {
              *camera = 1;
              if(!strncmp(casbuf.mode,"SUM2",4))  *camera = 21;
              if(!strncmp(casbuf.mode,"SUM4",4))  *camera = 41;
             }
          else if(!strncmp(casbuf.camera,"ISSWA",5))
             {
              *camera = 2;
              if(!strncmp(casbuf.mode,"SUM2",4))  *camera = 22;
              if(!strncmp(casbuf.mode,"SUM4",4))  *camera = 42;
             }
          else
            {
              zvmessage("GETPROJ: label error, must be ISSNA or ISSWA ","");  
              *ind = 1;
            }
	  return;
	}     

/* Use "correct" label access for the more modern missions... */
/* Don't use "labi" below this point, it is re-used for a temporary buffer */

	status = zlget(unit, "PROPERTY", "MISSION_NAME", labi,
			"property", "telemproc",
			"format", "string", "err_act", "", NULL);
	if (status == 1)
	  {

/* Mars Pathfinder */

	    if (strcasecmp(labi, "MARS PATHFINDER") == 0)
	      {
		strcpy(project, "MPF");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
	  }

/* Of course, the "multimission" mision name labels keep changing... */

	status = zlget(unit, "PROPERTY", "MISSION_NAME", labi,
			"property", "identification",
			"format", "string", "err_act", "", NULL);

	if (status == 1)
	  {

/* Mars 98 Lander */

	    if (strcasecmp(labi, "MARS SURVEYOR 98") == 0)
	      {
		strcpy(project, "M98");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars 01 Lander Testbed */

	    if (strcasecmp(labi, "M01" ) == 0)
	      {
		strcpy(project, "M01");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars FIDO Testbed */

	    if (strcasecmp(labi, "FIDO-TESTBED" ) == 0)
	      {
		strcpy(project, "FIDO");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
/* Mars MER mission*/

	    if (strcasecmp(labi, "MARS EXPLORATION ROVER" ) == 0)
	      {
		strcpy(project, "MER");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars Phoenix mission*/

	    if ((strcasecmp(labi, "PHOENIX LANDER" ) == 0) ||
		(strcasecmp(labi, "PHOENIX") == 0))
	      {
		strcpy(project, "PHX");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars MSL mission*/

	    if (strcasecmp(labi, "MARS SCIENCE LABORATORY" ) == 0)
	      {
		strcpy(project, "MSL");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars InSight mission*/

	    if ((strcasecmp(labi, "INSIGHT LANDER" ) == 0) ||
		(strcasecmp(labi, "INSIGHT") == 0))
	      {
		strcpy(project, "NSYT");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
	}


/* unrecognizable project */

        zvmessage("GETPROJ: unrecognizable project","");
        *ind = 1;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getproj.imake
/* Imake file for VICAR subroutine GETPROJ */

#define SUBROUTINE getproj

#define MODULE_LIST getproj.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
$Test_File:
$ create tgetproj.f
C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                       GETPROJ
C
C********************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44

      implicit integer*4 (a-z)
      character*5 project

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')

      call xvmessage('******fortran callable*******  ',' ')

      call xvmessage('GETPROJ:',' ')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call xvmessage('project=     '//project,' ')
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')

      call xvmessage('*********c callable**********  ',' ')

      call xvmessage('ZGETPROJ:',' ')
      call tzgetproj(unit)

      return
      end
$!-----------------------------------------------------------------------------
$ create tzgetproj.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  bridge to C callable version of TGETPROJ.F */
/************************************************************************/

void FTN_NAME(tzgetproj)(unit)
  int *unit;
{
  int   fds,ind,camera,status;
  char project[6];
  char msg[132];

      zgetproj(*unit,project,&camera,&fds,&ind);
      if(ind != 0) zprnt(4,1,&ind,"fatal indicator=.");
      sprintf(msg,"project=     %s",project);
      zvmessage(msg,"");
      zprnt(4,1,&camera,"camera serial number=.");
      zprnt(4,1,&fds,   "frame number=        .");
}
$!-----------------------------------------------------------------------------
$ create tgetproj.imake
/* Imake file for Test of VICAR subroutine getproj */

#define PROGRAM tgetproj

#define MODULE_LIST tgetproj.f tzgetproj.c 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL    /* REMOVE BEFORE DELIVERY */

$!-----------------------------------------------------------------------------
$ create tgetproj.pdf
!*****************************************************************************
! TGETPROJ.PDF - pdf for test program TGETPROJ.F for the subroutine GETPROJ
!*****************************************************************************
process 
PARM INP          TYPE=STRING       COUNT=1
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgetproj.pdf
!****************************************************************************
! TSTGETPROJ.PDF, unit test procedure for subroutine GETPROJ.F
!
! This PDF is written for both VMS and Unix
! The testings of projects WFPC1, WFPC2, VIS, MVM73 and MARINER 9 have been
!   removed due to the unavailability of these images.  Please add these test 
!   cases if images are available
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
  local Mpf               type=string


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
    let CssTour       = "wms_test_work:[testdata.cassini.iss]$N1358285193_7.IMG"
    let Mpf          = "wms_test_work:[testdata.mpf]i1246768187r.img_0013060021"
  else ! Unix
    let GLLFlightFull = "/project/test_work/testdata/mipl/gll/venus.img"
    let GLLFlightSum  = "/project/test_work/testdata/mipl/gll/4600.byt"
    let GllCalibFull  = "/project/test_work/testdata/mipl/gll/445.rad"
    let GLLCalibSum   = "/project/test_work/testdata/mipl/gll/gllsumdrkcrnt.tst"
    let VGR1          = "/project/test_work/testdata/mipl/vgr/f1636832.fic"
    let VGR2          = "/project/test_work/testdata/mipl/vgr/uvh.img"
    let CssCalib    = "/project/test_work/testdata/cassini/iss/labtest.img"
    let CssFlight   = "/project/test_work/testdata/cassini/casIss/n1356781097.2"
    let CssS2  = +
        "/project/test_work/testdata/cassini/casIss/n1308947518.182-142523"
    let CssTour     = "/project/test_work/testdata/cassini/iss/N1358285193_7.IMG"
    let Mpf      = "/project/test_work/testdata/mpf/i1246768187r.img_0013060021"
  end-if

  let $echo="yes"
 tgetproj INP=@GllFlightFull
 tgetproj INP=@GllFlightSum
 tgetproj INP=@GllCalibFull
 tgetproj INP=@GllCalibSum
 tgetproj INP=@VGR1
 tgetproj INP=@VGR2
 tgetproj INP=@CssCalib
 tgetproj INP=@CssFlight
 tgetproj INP=@CssS2
 tgetproj INP=@CssTour
 tgetproj INP=@Mpf
  let $echo="no"

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getproj.hlp
1 GETPROJ

  Description:

	VICAR subroutine GETPROJ returns Project ID, Camera serial number and 
          FDS from the VICAR label of the input file unit.

  FORTRAN calling sequence:
	INTEGER      unit,camera,fds,ind
	CHARACTER*5  project

        CALL getproj(unit,project,camera,fds,ind)

  C calling sequence:
	int  unit,camera,fds,ind;
	char project[6];

        zgetproj(unit,project,&camera,&fds,&ind);

2 ARGUMENTS

  Input:
	unit	- unit number of an OPENED file containing picture
	
  Output:
	project - spacecraft identity
                  valid are:
                    GLL, VGR-1, VGR-2, VIKOR, MAR10, MAR-9, 
                    and WFPC1 for space telescope (old optics),
                    and WFPC2 for space telescope (first optics upgrade)
                    and CASSI           (Cassini)
                    and MPF             (Mars Pathfinder)
		    and M98		(Mars 98 Lander)
		    and M01		(Mars 01 Testbed)
	camera  - camera serial number
                    For WFPC 1=PC and 2=WF cameras
                    For CASSI 1=ISSNA 2=ISSWA 21=ISSNA SUM2 22=ISSWA SUM2
                              41=ISSNA SUM4 42=ISSWA SUM4
                    For MPF, M98, M01: 0 always
	fds     - image number
	            for VGR is the fds count
	            for GLL is the sclk count
        	    for WFPC, 0=fds not applicable
                    for CASSI is the sclk count
                    for MPF, M98, M01: 0 always
	ind	- 0=normal   1=error

  Mars Pathfinder/M98/M01 support is partial (mission name only).  If the other
  parameters are needed, they can be added - but access to them is much
  better via the MPF/M98/M01 label routines (which are in the MARS library
  and thus inaccessible to generic p2 programs).

2 HISTORY

  Written By: Jean Lorre        10/1/89
  Cognizant Programmer: J Lorre
  Source Language: C
  Revisions: 
     8 Apr 03    ...vrh...     Cassini tour labels have different mission name
    18 Apr 01    ...vrh...     set camera serial to reflect summation mode
    18 Oct 00    ...rgd...     Added Mars 01
    13 Sep 99    ...gmy...     Include strcasecmp.h to link on VMS
    21 Jul 98    ...rgd...     Added partial Mars Pathfinder, M98 support.
			       Changed logic from else-if to if-return.
    21 Jan 97    ...sp....     Merged Cassini version into MIPS version.
    19 Apr 94    ...jry...     added Cassini
    23 jul 96    ...smc...     set camera serial number=2 for SSI summation
                               mode (FR89117)
    11 apr 94    ...tlt...     added tester note and machine dependence
                               in unit test (FR83074). 
    10 aug 93    ...tlt...     ported to unix
    09 Aug 93    ...jjl...     added space telescope
    26 sep 90    ...cca...     fix for mar10
    10 aug 90    ...cca...     delete xvclose before & after volabv2
                               changed error chk to buf(1)=0 from ne 1

$ Return
$!#############################################################################
