$!****************************************************************************
$!
$! Build proc for MIPL module momat
$! VPACK Version 1.5, Monday, January 25, 1993, 08:31:15
$!
$! Execute by entering:		$ @momat
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
$ write sys$output "*** module momat ***"
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
$   if F$SEARCH("momat.imake") .nes. ""
$   then
$      vimake momat
$      purge momat.bld
$   else
$      if F$SEARCH("momat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momat.bld "STD"
$   else
$      @momat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momat.com -
	-s momat.f -
	-i momat.imake -
	-t tmomat.f tmomat.imake tmomat.pdf tstmomat.pdf -
	-o momat.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create momat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MOMAT(OAL,OAS,SSL,SSS,SCALE,FL,SCLO,SCLA,ANGN,RANGE,
     &A,RS,SEDRNA)
c
c       Oct 92   ...WPL...   Ported for UNIX Conversion 
C     8 OCT 80   ...JJL...   INITIAL RELEASE
c
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(3,3),RS(3)
      INTEGER*4 SEDRNA
c
C THIS ROUTINE COMPUTES THE PLANET TO CAMERA ROTATION MATRIX (A) AND THE
C VECTOR FROM PLANET CENTER TO SPACECRAFT (RS), EXPRESSED IN THE PLANET
C SYSTEM. A VECTOR V IN THE PLANET SYSTEM IS THEN RELATED TO THE SAME
C VECTOR VP IN THE CAMERA SYSTEM BY
c
C          VP = A*V + RS
c
C Required NAVIGATION Information ......
C   OAL,OAS = LINE,SAMPLE LOCATION OF OPTIC AXIS
C   SSL,SSS = LINE,SAMPLE LOCATION OF PLANET CENTER
C   SCLA = PLANETOCENTRIC LATITUDE OF S/C (DEG)
C   SCLO = WEST LONGITUDE OF S/C
C   ANGN = NORTH ANGLE AT OPTIC AXIS INTERCEPT POINT
C   RANGE = RANGE FROM S/C TO PLANET CENTER
C   SCALE = SCALE IN PIXELS/MM
C   FL = FOCAL LENGTH IN MM
C   SEDRNA  is an INTEGER WHICH IS 0 IF THE NORTH ANGLE IS DEFINED AS
C   DESIRED BY MAP2  OR IS > 0 IF THE NORTH ANGLE IS DEFINED THE
C   SAME AS THE SEDR.
C
      If (SEDRNA.NE.0) GO TO 10
c
C  USE THIS ROUTINE IF THE NORTH ANGLE IS MEASURED AS IF IT WERE
C  THE ANGLE FROM UP OF THE PLANET SPIN AXIS PROJECTED ON THE
C  IMAGE.
c
      Call MomatV( OAL,OAS,SSL,SSS,SCALE,FL,SCLO,SCLA,ANGN,RANGE,
     &A,RS)
      Return
c
C  USE THIS ROUTINE IF THE NORTH ANGLE IS MEASURED AS IF IT WERE
C  THE ANGLE FROM UP OF THE PLANET SPIN AXIS PROJECTED ON THE
C  IMAGE AFTER THE CAMERAS HAVE BEEN SLEWED TO PLACE THE OPTICAL
C  AXIS COINCIDENT WITH THE PLANET CENTER.
c
10    Call MomatI( OAL,OAS,SSL,SSS,SCALE,FL,SCLO,SCLA,ANGN,RANGE,
     &A,RS)
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momat.imake
/* Imake file for VICAR subroutine MOMAT  */

#define SUBROUTINE  momat

#define MODULE_LIST  momat.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tmomat.f
        Include  'VICMAIN_FOR'
c
C-----THIS VICAR Module TESTS Subroutine MOMAT
c
        Subroutine Main44
	Implicit real*8 (a-z)
	common/c/fl,oal,oas,scl,slat,slon,lss,sss,na,rng
	real*8 om(9),rs(3)
        character*80   header
c       Data Header /'           fl   oal   oas   scl   slat   slon   lss   sss
c    1  na   rng'/
	Header = '           fl   oal   oas   scl   slat   slon   ' //
     1		'lss   sss  na   rng'
C
C-----SET UP THE TEST INPUT BUFFER
c
	fl = 1500.1904
	oal = 500.
	oas = 500.
	scl = 84.821431
	slat = 3.4825339
	slon = 116.72441
	lss = -121.600
	sss = 1662.700
	na = 160.8923
 	rng = 14967727.
	SEDRNA = 1
c
c-----Print the input parameters
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss
c     1  na   rng',67)
        Call Xvmessage(Header, ' ')
	Call prnt(8,10,fl,'.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	CALL Momat(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs,SEDRNA)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	Call  Prnt (8,9,oM,' OM MATRIX.')
	Call  Prnt (8,3,rs,' RS VECTOR.')
        Call  Exit(1)
c
C-----SET UP THE TEST INPUT BUFFER
c
	fl = 1500.1904
	oal = 500.
	oas = 500.
	scl = 84.821431
	slat = 3.4825339
	slon = 116.72441
	lss = -121.600
	sss = 1662.700
	na = 160.8923
 	rng = 14967727.
	SEDRNA = O
c
c-----Print the input parameters
c
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss
c     1  na   rng',67)
        Call Xvmessage( Header, ' ')
	Call Prnt(8,10,fl, '.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	Call Momat(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs,SEDRNA)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	Call Prnt(8,9,oM,' OM MATRIX.')
	Call Prnt(8,3,rs,' RS VECTOR.')

c	CALL EXIT

        Return
	END
$!-----------------------------------------------------------------------------
$ create tmomat.imake
/* IMAKE file for Test of VICAR subroutine  MOMAT  */

#define PROGRAM  tmomat

/* #define MODULE_LIST tmomat.f momatv.f  momati.f  */
#define MODULE_LIST tmomat.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/* #define   LIB_LOCAL  */    /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tmomat.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstmomat.pdf
Procedure
Refgbl $echo
Body
Let _onfail="continue"
Let $echo="yes"
!
TMOMAT
!
!THIS IS A TEST OF MODULE MOMAT
!MOMAT EVENTUALLY CALLS EITHER MOMATV OR MOMATI ACCORDING TO 
!THE VALUE OF SEDRNA.
!WHEN MOMATV IS CALLED,
!THE TEST PROGRAM WILL SET UP A SET OF ARGUEMENTS TO INPUT
!TO MOMATV.  THEY ARE PRINTED OUT INITIALLY.  THEN THE OUTPUT
!DATA IS PRINTED : A 9 ELEMENT OM-MATRIX AND A 3 ELEMENT
! RS-VECTOR.
!THE VALUES FROM AN IBM RUN ARE:
! OM -.83108 .315447 .458028 .450869 -.100005 .886969958
!    .325597 .9436588 -.0591128 
! RS  -6718550.879  -13344185.05  909203.4494
!
!WHEN MOMATI IS CALLED,
!THE TEST PROGRAM SETS UP A DATA BUFFER AND CALLS FORMOM.
!FORMOM REFORMATS THE BUFFER AND CALLS MOMATI.
!ELEMENTS 25-40 OF THE BUFFER ARE INPUT VALUES.
!ELEMENTS 1-24 ARE THE OUTPUT VALUES.
!USING THE SAME INPUT BUFFER THE OUTPUT VALUES ON THE IBM ARE:
! OM -.83183 .31319 .458217 .448535 -.106932 .8873447
!    .32691 .943649 -.05153 
! RS  -6721453.83  -13349977.3  795527.9343
!
TMOMAT
!
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create momat.hlp
1  MOMAT

2  PURPOSE

    To compute the OM matrix and the RS vector in the MAPS FARENC mode.

2  CALLING SEQUENCE:

    CALL MOMAT(LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,VABS,MATRIX,VECTOR,SEDRNA)

2  ARGUMENTS: 

    LO       Optical axis line       object space pixels                 R*8
    SO       Optical axis sample     object space pixels                 R*8
    LSSP     Sub spacecraft point    object space line                   R*8
    SSSP     Sub spacecraft point    object space sample                 R*8
    PIXPMM   SCALE                   object space pixels/mm.             R*8
    FIC      Focal length            mm.                                 R*8
    BL       Sub spacecraft point    longitude, degrees west             R*8
    PHI      Sub spacecraft point    latitude   degrees                  R*8
    THT      North angle, degrees clockwise from up of spin axis         R*8
    VABS     Distance planet center to spacecraft,  km.                  R*8
    MATRIX   OM matrix               9 elements                          R*8
    VECTOR   RS Vector               3 elements                          R*8
    SEDRNA   Flag                    0 or 1                              I*4

    SEDRNA   is a flag which defines the manner in which the north angle
             (THT) is computed.

             SEDRNA>0 specifies that THT is measured clockwise from up 
             to the northward projection of the planet spin axis onto
             the image plane.

             SEDRNA=0 specifies that THT is measured clockwise from up
             to the northward projection of the planet spin axis onto 
             the image plane after the cameras have been slewed to place
             the optical axis coincident with the planet center, i.e.,
             as in the SEDR.

             SEDRNA=0 is correct for a hand measurement of THT directly
             from an image of a planet in a picture.  In this mode
             MOMAT calls the subroutine MOMATV.

             SEDRNA>0 is correct for all THT's obtained from the SEDR.
             In this mode MOMAT calls the subroutine MOMATI.

2  HISTORY               

      Jan-25-1993 .. W.P. Lee .. Fixed bug in TMOMAT.IMAKE file (FR #79169)
                           
      Ported for UNIX Conversion :   W.P. Lee   October, 1992  
      Original Programmer:J. J. Lorre, 1 September 1980
      Current Cognizant Programmer: J. J. Lorre
      Source Language: Fortran
      Revision: New

$ Return
$!#############################################################################
