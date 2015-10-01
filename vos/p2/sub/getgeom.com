$!****************************************************************************
$!
$! Build proc for MIPL module getgeom
$! VPACK Version 1.9, Wednesday, August 21, 2002, 17:45:18
$!
$! Execute by entering:		$ @getgeom
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
$ write sys$output "*** module getgeom ***"
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
$ write sys$output "Invalid argument given to getgeom.com file -- ", primary
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
$   if F$SEARCH("getgeom.imake") .nes. ""
$   then
$      vimake getgeom
$      purge getgeom.bld
$   else
$      if F$SEARCH("getgeom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getgeom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getgeom.bld "STD"
$   else
$      @getgeom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getgeom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getgeom.com -mixed -
	-s getgeom.f -
	-i getgeom.imake -
	-t tgetgeom.f tgetgeom.imake tgetgeom.pdf tstgetgeom.pdf -
	-o getgeom.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getgeom.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Returns in restab the GEOMA parameters required to geometrically correct
C an image.
C
      SUBROUTINE GETGEOM(UNIT,PROJECT,CAMERA,GEOM,restab,mestab,NAH,NAV,
     + ind)
      IMPLICIT NONE
      INTEGER*4 UNIT		!Unit number of input image
      CHARACTER*5 PROJECT	!GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9
      INTEGER*4 CAMERA		!Camera serial number
      INTEGER*4 GEOM		!0=return nominals, 1=read from unit number
      REAL*4 RESTAB(2720)	!Returned GEOMA parameters
      Real*4 MESTAB(2720)	!Same buffer as RESTAB
      INTEGER*4 NAH,NAV		!Number of horizontal and vertical areas
      INTEGER*4 IND		!Return status: 0=normal, 1=error

      INTEGER*4 ICOL,MAXPTS
      REAL*4 rloc(404)
c===================================================================
      ind=0

C For CCD cameras, geometric paramters are not used.  Therefore, for these
C cameras, the project ID is returned in words 1-2 of Restab:
      If (Project.eq.'GLL  ' .or. Project.eq.'CASSI') then
          Call MVCL(Project, Restab, 5) 
          CALL MVE(4,1,CAMERA,restab(3),1,1)
         Return
      Endif

      Call Zia (restab,2720)
      If (GEOM.EQ.0) Go To 100
c
C  case 1: READ DISTORTION CORRECTION FILE
c
      ICOL = 4
      MAXPTS=680
      CALL IREAD_TIEPOINTS(unit,nah,nav, MAXPTS, restab(9),ICOL)

      If ( (nah+1)*(nav+1) .gt. MAXPTS) Then
         Call Xvmessage('GETGEOM: Geoma file too large',' ')
         ind = 1
         Return
      EndIf

      call mvcl('NAH     ', restab(1), 8)
      call mvcl('NAV     ', restab(4), 8)
      call mvcl('TIEPOINT', restab(7), 8)
      Call MVE(4,1,nah,Restab(3), 1,1)
      Call MVE(4,1,nav,Restab(6), 1,1)
      Return
c
C  *** case 2: NO GEOMA FILE, USE NOMINALS ***
100   Continue
c
c   Voyager nominals
c
      If (Project .eq. 'VGR-1' .or. Project.eq.'VGR-2') then
           If (camera .ge.4 .and. camera.le.7) then
              Call getres(rloc,camera)
              Call geomav(restab,camera,rloc)
           Else
              Call Xvmessage('GETGEOM: Improper VGR camera #',' ')
              ind=1
              Return
           Endif
c
c Mariner 9 nominals
c
      Else If (Project .eq. 'MAR-9') then
           If (camera.eq.1) then
              Call  MM71A(RESTAB)
           Else If(camera.eq.2) then
              Call  MM71B(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper MAR-9 camera #', ' ')
              ind=1
              Return
           Endif
c
c Mariner 10 nominals
c
      Else If (project.eq.'MAR10') then
           If (camera.eq.1) then
              Call MVM73A(RESTAB)
           Else If (camera.eq.2) then
              Call MVM73B(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper MAR10 camera #',' ')
              ind=1
              Return
           Endif
c
c Viking nominals
c
      Else  If (project.eq.'VIKOR') then
           If (camera.eq.7) then
              Call VOSN7(RESTAB)
           Else If (camera.eq.4) then
              Call VOSN4(RESTAB)
           Else If (camera.eq.8) then
              Call VOSN8(RESTAB)
           Else If (camera.eq.6) then
              Call VOSN6(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper Viking camera #',' ')
              ind=1
              Return
           Endif
c
      Else
         Call Xvmessage('GETGEOM: Unknown flight project', ' ')
         ind=1
         Return
      Endif

      Call MVE(4,1,Restab(3), nah,1,1)
      Call MVE(4,1,Restab(6), nav,1,1)

      Return  
      End 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getgeom.imake
/* Imake file for VICAR subroutine  GETGEOM  */

#define SUBROUTINE  getgeom
#define MODULE_LIST  getgeom.f  
#define P2_SUBLIB
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tgetgeom.f
      Include 'VICMAIN_FOR' 
c
      Subroutine main44
      Implicit integer*4 (a-z)
      Character*5  Project 
      Real*4 BUF(2720)
      Real*4 MUF(2720)

      CALL Xvparm ('PROJECT', Project, ICNT, IDEF, 0)
      CALL Xvparm ('CAMERA', CAMERA, ICNT, IDEF, 0)
      Call Xvmessage(Project, ' ')
c 
      Call Prnt (4, 1, camera, 'camera serial number=.')
      Call Xvmessage(' ', ' ')
      Call Xvpcnt('INP', Count)
      If  (Count .EQ. 1) then
         Call Xvunit(unit2,'INP',1,status,' ')
         geomsor=1
       Call Xvmessage('obtaining geom parameters from input file', ' ')
      Else
         geomsor=0
        Call Xvmessage('obtaining geom parameters from nominals', ' ')
      Endif
      Call getgeom(unit2,project,camera,geomsor,BUF,MUF,
     +             nah,nav,ind)
      If(ind.ne.0) Call Xvmessage('GETGEOM: error, ind=1', ' ')

      IF (PROJECT.EQ.'GLL') THEN
         Call Prnt(99, 8, Buf(1), ' Elements 1 & 2 in BUF .')
         CALL PRNT(4,1,BUF(3),' Camera S/N=.')
         RETURN
      ENDIF

      Call Xvmessage(' First 8 Elements in BUF:  ', ' ')
      Call Prnt(4, 1, Buf(3), ' NAH = .')
      Call Prnt(99, 8, Buf(4), ' Elements 4 & 5 in BUF .')
      Call Prnt(4, 1, Buf(6), ' NAV = .')
      Call Prnt(99, 8, Buf(7), ' Elements 7 & 8 in BUF .')

      Call  Xvmessage(' First PART OF GEOMA PARAMETERS:', ' ')
      Call  Prnt (7, 32, BUF(9), '.')
      Call Xvmessage(' ', ' ')
c
      Return
      End
$!-----------------------------------------------------------------------------
$ create tgetgeom.imake
/* IMAKE file for Test of VICAR subroutine  GETGEOM  */

#define PROGRAM  tgetgeom

#define MODULE_LIST tgetgeom.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
#define   LIB_P2SUB         
#define   LIB_MATH77  

$!-----------------------------------------------------------------------------
$ create tgetgeom.pdf
Process
Parm  INP      STRING  COUNT=0:1 default=--
Parm  PROJECT  STRING  VALID=("GLL  ","VGR-1","VGR-2","VIKOR","MAR10","MAR-9")
Parm  CAMERA   INTEGER VALID=(0:8)
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgetgeom.pdf
procedure
refgbl $echo
refgbl $syschar
LOCAL DIR    TYPE=STRING 
local  vgr2_4         string
local  vgr2_5         string
local  vgr1_6         string
local  vgr1_7         string
local  vik_4          string
local  vik_6          string
local  vik_7          string
local  vik_8          string
local  m10_1          string
local  m10_2          string
local  m9_1           string
local  m9_2           string

body
let _onfail="continue"
let $echo="yes"
! test each camera without reading input file
!
TGETGEOM PROJ="GLL  " CAM=1
TGETGEOM PROJ="VGR-2" CAM=4
TGETGEOM PROJ="VGR-2" CAM=5
TGETGEOM PROJ="VGR-1" CAM=6
TGETGEOM PROJ="VGR-1" CAM=7
TGETGEOM PROJ="VIKOR" CAM=4
TGETGEOM PROJ="VIKOR" CAM=6
TGETGEOM PROJ="VIKOR" CAM=7
TGETGEOM PROJ="VIKOR" CAM=8
TGETGEOM PROJ="MAR10" CAM=1
TGETGEOM PROJ="MAR10" CAM=2
TGETGEOM PROJ="MAR-9" CAM=1
TGETGEOM PROJ="MAR-9" CAM=2
!TEST WITH READING INPUT FILE 
!PRINT OUT SHOULD BE IDENTICAL WITH CORRESPONDING ONE ABOVE
Write  " "  
!
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/sitod1/test_data/geoma/"
else 
   LET DIR  ="WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GEOMA]"
end-if

   let vgr2_4   = "&DIR"//"vgr2.cam4"
   let vgr2_5   = "&DIR"//"vgr2.cam5"
   let vgr1_6   = "&DIR"//"vgr1.cam6"
   let vgr1_7   = "&DIR"//"vgr1.cam7"
   let vik_4    = "&DIR"//"vikor.cam4"
   let vik_6    = "&DIR"//"vikor.cam6"
   let vik_7    = "&DIR"//"vikor.cam7"
   let vik_8    = "&DIR"//"vikor.cam8"
   let m10_1    = "&DIR"//"mar10.cam1"
   let m10_2    = "&DIR"//"mar10.cam2"
   let m9_1     = "&DIR"//"mar9.cam1"
   let m9_2     = "&DIR"//"mar9.cam2"
!
TGETGEOM  inp=@vgr2_4  PROJ="VGR-2"  CAM=4
TGETGEOM  inp=@vgr2_5  PROJ="VGR-2"  CAM=5
TGETGEOM  inp=@vgr1_6  PROJ="VGR-1"  CAM=6
TGETGEOM  inp=@vgr1_7  PROJ="VGR-1"  CAM=7
TGETGEOM  inp=@vik_4   PROJ="VIKOR"  CAM=4
TGETGEOM  inp=@vik_6   PROJ="VIKOR"  CAM=6
TGETGEOM  inp=@vik_7   PROJ="VIKOR"  CAM=7
TGETGEOM  inp=@vik_8   PROJ="VIKOR"  CAM=8
TGETGEOM  inp=@m10_1   PROJ="MAR10"  CAM=1
TGETGEOM  inp=@m10_2   PROJ="MAR10"  CAM=2
TGETGEOM  inp=@m9_1    PROJ="MAR-9"  CAM=1
TGETGEOM  inp=@m9_2    PROJ="MAR-9"  CAM=2
!
Let  $echo="no"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create getgeom.hlp
1 GETGEOM: Returns the geoma parameters required to geometrically correct
  an image.

  FORTRAN CALLING SEQUENCE:

      IMPLICIT NONE
      INTEGER*4 UNIT		!Unit number of input GEOMA parameters
      CHARACTER*5 PROJECT	!GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9 CASSI
      INTEGER*4 CAMERA		!Camera serial number
      INTEGER*4 MODE		!0=return nominals   1=read from unit number
      REAL*4 PARBUF(2720)	!Returned GEOMA parameters
      Real*4 QARBUF(2720)	!Same buffer as PARBUF
      INTEGER*4 NAH,NAV		!Number of horizontal and vertical areas
      INTEGER*4 IND		!Return status: 0=normal, 1=error

      CALL GETGEOM(UNIT,PROJECT,CAMERA,MODE,parbuf,qarbuf,NAH,NAV,ind)

  C CALLING SEQUENCE:

	< There is currently no C-bridge for this routine >

2 OPERATION

GETGEOM returns GEOMA parameters which can be used by routine CONVISOS to
correct for the geometric distortions induced by vidicon camera systems.  All
missions flown by JPL since Mariner 9 (Mariner Venus Mercury) are supported.
The parameters are returned in PARBUF.

For CCD camera systems (Galileo SSI), an optical distortion model is used and
the buffer PARBUF does not apply.  However, the string 'GLL  ' is stored in
the first two words of PARBUF, and CAMERA is stored in the third word of
PARBUF.  This is a cludge to provide CONVEV/CONVISOS with these arguments.

If MODE=0, nominal GEOMA parameters are returned.  The nominal geometric
distortion parameters for a given camera are based on nominal locations for 
the reseaux.

If MODE=1, then the GEOMA parameters are read from the GEOMA parameter file
pointed to be UNIT.  The GEOMA parameter file must be an IBIS tiepoint file in
the format output by programs RESLOC and FARENC.  Note that GETGEOM will both
open and close the file.

2 HISTORY

Written By: Jean Lorre        10/1/89
Cognizant Programmer:         J Lorre

Ported for UNIX Conversion:   W. Lee,  Jan-12-1993
Upgraded to use IBIS tiepoint files like GEOMA.   SP 7-96
18 Sep 96  GMY  Store CAMERA in third word of PARBUF for GLL (FR 89818).
28 Nov 01  GMY  Add Cassini to list of recognized projects.
21 Aug 02  GMY  Blank fill 'NAH ' and 'NAV ' in output parameters.
$ Return
$!#############################################################################
