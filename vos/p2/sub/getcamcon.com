$!****************************************************************************
$!
$! Build proc for MIPL module getcamcon
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:35
$!
$! Execute by entering:		$ @getcamcon
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
$ write sys$output "*** module getcamcon ***"
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
$ write sys$output "Invalid argument given to getcamcon.com file -- ", primary
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
$   if F$SEARCH("getcamcon.imake") .nes. ""
$   then
$      vimake getcamcon
$      purge getcamcon.bld
$   else
$      if F$SEARCH("getcamcon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getcamcon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getcamcon.bld "STD"
$   else
$      @getcamcon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getcamcon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getcamcon.com -mixed -
	-s getcamcon.c -
	-i getcamcon.imake -
	-t tgetcamcon.f tzgetcamcon.c tgetcamcon.imake tgetcamcon.pdf -
	   tstgetcamcon.pdf -
	-o getcamcon.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getcamcon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include "zvproto.h"
/**************************************************************************

 GETCAMCON returns camera constants given project and camera from getproj.

 arguments:
   project  = spacecraft id. input.                        character *
              valid=MAR-9 MAR10 VIKOR VGR-1 VGR-2 GLL
                    WFPC1(old optics)  WFPC2(first optics upgrade)
   camera   = camera serial number. input                  int
   focal    = focal length in mm. returned                 float
   oal      = optical axis line object space. returned.    float
   oas      = optical axis sample object space. returned.  float
   scale    = object space scale. pixels/mm. returned.     float
   ind      = 0=normal   1=error                           int

**************************************************************************/
void zgetcamcon();
/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/
void FTN_NAME2(getcamcon, GETCAMCON) (char *project, int *camera, float *focal,
	float *oal, float *oas, float *scale, int *ind, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length;
/*  ==================================================================  */

   length = 5;

/* 7 args for GETCAMCON, project is 1st arg and 1st string   */


   zsfor2c(proj, length, project, &project, 7, 1 , 1, ind);

   zgetcamcon(proj,*camera,focal,oal,oas,scale,ind);

}
/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/
      void zgetcamcon(project,camera,focal,oal,oas,scale,ind)
 
      char *project;
      int camera,*ind;
      float *focal, *oal, *oas, *scale;
{
      int icam;
      static int viksn[] = {0, 0, 0, 2, 0, 4, 1, 3};

/*-----------------------------------
 cassi
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc8[2][4] =
      {  {2000.00, 512.0, 512.0, 83.333333},
         {200.736, 512.0, 512.0, 83.333333}};

/*-------------------------------------
 GLL
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc5[1][4] = { 1501.039, 400.0, 400.0, 65.6167979 };

/*-----------------------------------
 vgr-1 and vgr-2
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc4[4][4] =
      {  {1500.19, 500.0, 500.0, 84.821428},    
         {200.465, 500.0, 500.0, 84.821428},    
         {1503.49, 500.0, 500.0, 84.821428},    
         {200.770, 500.0, 500.0, 84.821428} }; 

/*-------------------------------------
 vikor
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc3[4][4] =
     { {474.398, 575.0, 625.0, 85.0},     
       {474.448, 575.0, 625.0, 85.0},     
       {474.610, 575.0, 625.0, 85.0},    
       {474.101, 575.0, 625.0, 85. }};  

/*-------------------------------------
 mar10
=====================================
  1=focal length [mm.]
  2=optical axisline [o.s. pixels]
  3=optical axis sample [o.s. pixels]
  4=scale [pixels/mm.]
-------------------------------------*/

      static float foc2[4][4] = 
     { {1495.66, 400.0, 475.0, 74.78},
       {1503.69, 400.0, 475.0, 74.78},
       {62.02, 400.0, 475.0, 74.78},  
       {62.68, 400.0, 475.0, 74.78} };

/*-------------------------------------
 mar-9
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc1[2][4] = 
     { {52.267, 400.0, 475.0, 75.0},     
       {500.636, 400.0, 475.0, 75.0} };  

/*-------------------------------------
 wfpc1
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc6[2][4] = 
     { {67991., 400.0, 400.0, 66.66667},     
       {31168., 400.0, 400.0, 66.66667} };  

/*-------------------------------------
 wfpc2
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc7[2][4] = 
     { {67991., 400.0, 400.0, 66.66667},     
       {31168., 400.0, 400.0, 66.66667} };  

      *ind = 0;
      if (strncmp(project,"CASSI",5) == 0)
      {
          if (camera == 1 || camera == 2)         /* FULL summation mode */
          {
            *focal = foc8[camera-1][0];
            *oal = foc8[camera-1][1];
            *oas = foc8[camera-1][2];
            *scale = foc8[camera-1][3];
          }
          else if (camera == 21 || camera == 22)  /* SUM2 summation mode */
          {
            *focal = foc8[camera-21][0];
            *oal = foc8[camera-21][1]/2;
            *oas = foc8[camera-21][2]/2;
            *scale = foc8[camera-21][3]/2;
          }
          else if (camera == 41 || camera == 42)  /* SUM4 summation mode */
          {
            *focal = foc8[camera-41][0];
            *oal = foc8[camera-41][1]/4;
            *oas = foc8[camera-41][2]/4;
            *scale = foc8[camera-41][3]/4;
          }
          else
          {
             zvmessage("GETCAMCON: CASSI ILLEGAL CAMERA #","");
             *ind = 1;
          }

      }
      else if (strncmp(project,"GLL", 3) == 0)
      {
         *focal = foc5[0][0];

         if (camera==2)   /* SSI Summation Mode */
         {
           *oal = foc5[0][1]/2;
           *oas = foc5[0][2]/2;
           *scale = foc5[0][3]/2;
         }
         else             /* SSI Full Frame */
         {
           *oal = foc5[0][1];
           *oas = foc5[0][2];
           *scale = foc5[0][3];
         }
      }
      else if (strncmp(project,"VGR-1",5) == 0 || 
	       strncmp(project,"VGR-2",5) == 0)
      {

              if (camera >=  4 && camera <=  7) 
              {
		 icam = 7 - camera;
                 *focal = foc4[icam][0]; 
                 *oal = foc4[icam][1];
                 *oas = foc4[icam][2];
                 *scale = foc4[icam][3];
              }
              else 
              {
                 zvmessage("GETCAMCON: VGR ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"VIKOR",5) == 0)
      {
              if(camera >=  4 && camera <=  8) 
              {
		 icam = camera - 1;
                 *focal = foc3[viksn[icam]-1][0];
                 *oal = foc3[viksn[icam]-1][1];
                 *oas = foc3[viksn[icam]-1][2];
                 *scale = foc3[viksn[icam]-1][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: VIKOR ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"WFPC1",5) == 0)
      {
              if(camera >=  1 && camera <=  2) 
              {
		 icam = camera-1;
                 *focal = foc6[icam][0];
                 *oal = foc6[icam][1];
                 *oas = foc6[icam][2];
                 *scale = foc6[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: WFPC1 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }
      else if (strncmp(project,"WFPC2",5) == 0)
      {
              if(camera >=  1 && camera <=  2) 
              {
		 icam = camera-1;
                 *focal = foc7[icam][0];
                 *oal = foc7[icam][1];
                 *oas = foc7[icam][2];
                 *scale = foc7[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: WFPC2 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"MAR10",5) == 0)
	{
              if(camera >=  1 && camera <=  4) 
              {
		 icam = camera - 1;
                 *focal = foc2[icam][0];
                 *oal = foc2[icam][1];
                 *oas = foc2[icam][2];
                 *scale = foc2[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: MAR10 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }
      else if (strncmp(project,"MAR-9",5) == 0)
      {
              if (camera >=  1 && camera <=  2) 
              {
		 icam = camera - 1;
                 *focal = foc1[icam][0];
                 *oal = foc1[icam][1];
                 *oas = foc1[icam][2];
                 *scale = foc1[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: MAR-9 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
       } 
       else
       {
         zvmessage("GETCAMCON: UNRECOGNIZABLE PROJECT ID",""); 
	 *ind = 1;
       }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getcamcon.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY getcamcon

   To Create the build file give the command:

        $ vimake getcamcon                     (VMS)
   or
        % vimake getcamcon                     (Unix)


*************************************************************************/

#define SUBROUTINE getcamcon

#define MODULE_LIST getcamcon.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tgetcamcon.f
c*************************************************************
c
c program to test FORTRAN and C callable versions of GETCAMCON
c
c*************************************************************
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)

      REAL*4      FOCAL,OAL,OAS,SCALE
      CHARACTER*5 Project
      INTEGER     Camera, FileUnit, Ind, Fds

      CALL xvunit(FileUnit, 'INP', 1, Ind,' ')
      CALL xvopen(FileUnit, Ind,'OPEN_ACT','SA',' ')

      call xvmessage('************FORTRAN CALLABLE************',' ')
      CALL getproj(FileUnit, Project, Camera, Fds, Ind)

      call xvmessage(project,' ')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call xvmessage('  ',' ')

      call xvmessage('************C- CALLABLE************',' ')
      call tzgetcamcon(FileUnit)

      CALL XVCLOSE(FileUnit, Ind, ' ')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzgetcamcon.c
/*************************************************************

 bridge to C callable version of GETCAMCON

*************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzgetcamcon)(FileUnit)
  int *FileUnit;
{
      int Camera,count,def,ind, FDS;
      float focal,oal,oas,scale;
      char Project[5];

      zgetproj (*FileUnit, Project, &Camera, &FDS, &ind);
      zvmessage(Project,"");
      zgetcamcon(Project,Camera,&focal,&oal,&oas,&scale,&ind);
      if(ind != 0)  zprnt(4,1,&ind,"fatal indicator=.");
      zprnt(7,1,&focal,"focal length=       .");
      zprnt(7,1,&oal,  "optical axis line=  .");
      zprnt(7,1,&oas,  "optical axis sample=.");
      zprnt(7,1,&scale,"image scale=        .");
}
$!-----------------------------------------------------------------------------
$ create tgetcamcon.imake
/* Imake file for Test of VICAR subroutine getcamcon */

#define PROGRAM tgetcamcon

#define MODULE_LIST tgetcamcon.f tzgetcamcon.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define FTN_STRING

/* #define LIB_LOCAL */
/* #define DEBUG    /* Remove upon delivery */
$!-----------------------------------------------------------------------------
$ create tgetcamcon.pdf
process
  PARM INP TYPE=STRING  COUNT=1
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgetcamcon.pdf
!****************************************************************************
! TSTGETCAMCON.PDF, unit test procedure for subroutine GETCAMCON.F
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
  local CssS4             type=string


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
    let CssS4= "wms_test_work:[testdata.cassini.cas$i$ss]n1294639770.011-091050"
  else ! Unix
    let GLLFlightFull = "/project/test_work/testdata/mipl/gll/venus.img"
    let GLLFlightSum  = "/project/test_work/testdata/mipl/gll/4600.byt"
    let GllCalibFull  = "/project/test_work/testdata/mipl/gll/445.rad"
    let GLLCalibSum   = "/project/test_work/testdata/mipl/gll/gllsumdrkcrnt.tst"
    let VGR1          = "/project/test_work/testdata/mipl/vgr/f1636832.fic"
    let VGR2          = "/project/test_work/testdata/mipl/vgr/uvh.img"
    let CssCalib    = "/project/test_work/testdata/cassini/iss/labtest.img"
    let CssFlight   = "/project/test_work/testdata/cassini/casIss/n1356781097.2"
    let CssS2    = +
        "/project/test_work/testdata/cassini/casIss/n1308947518.182-142523"
    let CssS4    = +
        "/project/test_work/testdata/cassini/casIss/n1294639770.011-091050" 
  end-if

  let $echo="yes"
 TGETCAMCON INP=@GllFlightFull
 TGETCAMCON INP=@GllFlightSum
 TGETCAMCON INP=@GllCalibFull
 TGETCAMCON INP=@GllCalibSum
 TGETCAMCON INP=@VGR1
 TGETCAMCON INP=@VGR2
 TGETCAMCON INP=@CssCalib
 TGETCAMCON INP=@CssFlight
 TGETCAMCON INP=@CssS2
 TGETCAMCON INP=@CssS4
  let $echo="no"

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getcamcon.hlp
1 GETCAMCON

  Description:

	VICAR2 subroutine GETCAMCON returns camera constants given project 
  	and camera from GETPROJ.

  FORTRAN calling sequence:

	integer camera,ind
	real*4 focal,oal,oas,scale
	character*5 project

	call getcamcon(project,camera,focal,oal,oas,scale,ind)

  C calling sequence:

	int ind,camera;
	float focal,oal,oas,scale;
	char *project;

	zgetcamcon(project,camera,&focal,&oal,&oas,&scale,&ind);
	
2 ARGUMENTS
     
  Input:
	project	- spacecraft id.      
           	  valid=MAR-9 MAR10 VIKOR VGR-1 VGR-2 GLL
                  WFPC1 (old system)
                  WFPC2 (after first optics upgrade)
                  CASSI
	camera	- camera serial number

  Output:	
	focal	- focal length in mm.
	oal	- optical axis line object space.
	oas	- optical axis sample object space.
	scale	- object space scale. pixels/mm.
	ind	- 0=normal   1=error

2 HISTORY

  Written By: Jean Lorre        10/1/89
  Cognizant Programmer: J Lorre
  Revision : 01 MAR 91 ...CCA...  ADDED TEST PROGRAM, ADDED TEST
                                    FOR VO, MVM, MAR-9
             29 JUL 93 T. Truong  Ported to UNIX
              9 aug 93 jjl        Added WFPC projects
             24 Jul 96 SM Chang   Adjust oal, oas, scale for SSI Summation
                                    Mode (FR 89116)
             18 Apr 01 vrh        Added Cassini support, includes summation
$ Return
$!#############################################################################
