$!****************************************************************************
$!
$! Build proc for MIPL module mwatna
$! VPACK Version 1.7, Thursday, June 09, 1994, 20:02:50
$!
$! Execute by entering:		$ @mwatna
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
$ write sys$output "*** module mwatna ***"
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
$ write sys$output "Invalid argument given to mwatna.com file -- ", primary
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
$   if F$SEARCH("mwatna.imake") .nes. ""
$   then
$      vimake mwatna
$      purge mwatna.bld
$   else
$      if F$SEARCH("mwatna.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mwatna
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mwatna.bld "STD"
$   else
$      @mwatna.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mwatna.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mwatna.com -
	-s mwatna.f -
	-i mwatna.imake -
	-t tmwatna.f tmwatna.imake tmwatna.pdf tstmwatna.pdf -
	-o mwatna.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mwatna.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,*)
C     11 AUG 93   ...DDK...  PORTED TO UNIX
C     2 SEPT 82   ...GMY...  UPDATE VGR2 USING NOV 79 RAND REPORT
C     3 NOV 80   ...GMY...    INITIAL RELEASE
C ROUTINE TO CONVERT WA (LINE,SAMP) COORDINATES TO NA (LINE,SAMP)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 WALINE,WASAMP,NALINE,NASAMP
      REAL*8 C(3,3),R(3)
      REAL*8 VGR1(3,3)/
     * 0.9999950588D0, 0.0031009104D0,-0.0005165310D0,
     *-0.0031011413D0, 0.9999950916D0,-0.0004467931D0,
     * 0.0005151430D0, 0.0004483927D0, 0.9999997668D0/
      REAL*8 VGR2(3,3)/
     * 0.9999966464D0, 0.0025303754D0, 0.0005516696D0,
     *-0.0025304697D0, 0.9999967838D0, 0.0001703842D0,
     *-0.0005512367D0,-0.0001717796D0, 0.9999998333D0/
      INTEGER*2 ICAM
      CHARACTER*50 MSG
C
      ISN = ABS(ICAM)
      IF(ISN.EQ.6.OR.ISN.EQ.7) GOTO 6
      IF(ISN.EQ.4.OR.ISN.EQ.5) GOTO 4
      WRITE(MSG,100) ISN
      CALL XVMESSAGE(MSG,' ')
  100 FORMAT(' **INVALID CAMERA S/N= ',I9)
      RETURN1
C
C          VGR2 WA,NA=4,5
    4 CALL MVE(7,18,VGR2,C,1,1)
      FNA = 1503.49D0
      FWA = 200.77D0
      GOTO 10
C          VGR1 WA,NA=6,7
    6 CALL MVE(7,18,VGR1,C,1,1)
      FNA = 1500.19D0
      FWA = 200.293D0
C
C          CONVERT TO MM (SCALE=PIXELS PER MM)
   10 SCALE = 84.821428D0
      X = (WASAMP-500.D0)/SCALE
      Y = (WALINE-500.D0)/SCALE
      Z = FWA
C          WA TO NA TRANSFORMATION
      DO 20 I=1,3
   20 R(I) = C(I,1)*X + C(I,2)*Y + C(I,3)*Z
C          CONVERT BACK TO PIXELS
      S = SCALE*FNA/R(3)
      NALINE = S*R(2) + 500.D0
      NASAMP = S*R(1) + 500.D0
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mwatna.imake
/* Imake file for VICAR subroutine MWATNA */

#define SUBROUTINE mwatna

#define MODULE_LIST mwatna.f 

#define P2_SUBLIB

#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tmwatna.f

C  THIS PROGRAM TEST THE FORTRAN CALLING SEQUENCE FOR MWATNA
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INTEGER*2 ICAM
      CHARACTER*50 MSG
      REAL*4 WALINE,WASAMP,NALINE,NASAMP
   
C
      DO ICAM=4,8,2
          WRITE(MSG,100)ICAM
          CALL XVMESSAGE(MSG,' ')
  100     FORMAT('ICAM= ',I)
          WRITE(MSG,120)
          CALL XVMESSAGE(MSG,' ')
  120     FORMAT(' WALINE,WASAMP,NALINE,NASAMP')
          WALINE = 500.0	 ! See where the optical axis goes
          WASAMP = 500.0         ! just for intellectual curiosity.
          CALL MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,&999)
          WRITE(MSG,140) WALINE,WASAMP,NALINE,NASAMP
          CALL XVMESSAGE(MSG,' ')
  140     FORMAT(4E12.4)
          WALINE = 1.0           ! Check one of the corners
          WASAMP = 1.0
          CALL MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,&999)
          WRITE(MSG,140) WALINE,WASAMP,NALINE,NASAMP
          CALL XVMESSAGE(MSG,' ')
      ENDDO
      RETURN
  999 WRITE (MSG,160)
      CALL XVMESSAGE(MSG,' ')
  160 FORMAT(' ***Invalid camera serial number')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tmwatna.imake
/* Imake file for Test of VICAR subroutine MWATNA */

#define PROGRAM tmwatna

#define MODULE_LIST tmwatna.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_FORTRAM
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tmwatna.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstmwatna.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tmwatna
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mwatna.hlp
1 MWATNA

  Given a (line,sample) coordinate in a Voyager wide-angle image,
  transforms these coordinates to the corresponding (line,sample)
  coordinates in the narrow-angle camera field of view.  Both
  input and output coordinates are assumed to be in object space
  (i.e. IPL defined geometrically corrected coordinate space).

  Note: Due to the nature of this subroutine's call interface, the
        last parameter, it is not recommended for use with the C
        language.

  FORTRAN Calling Sequence: 
                CALL MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,&nnn)

  Arguments: 

      INTEGER*4 ICAM         Input Voyager camera serial number
      REAL*4 WALINE,WASAMP   Input wide-angle (line,sample)
      REAL*4 NALINE,NASAMP   Output narrow-angle (line,sample)

      &nnn is a statement label to which MWATNA will return if ICAM
  is an invalid Voyager camera serial number.  Valid serial numbers
  are:
		4 = VGR-2 WA
		5 = VGR-2 NA
		6 = VGR-1 WA
		7 = VGR-1 NA
      ICAM (icam) may either specify the wide-angle or narrow-angle camera
  (i.e. ICAM (icam) =4 or 5 will produce the same effect).

2 History

  Original Programmer: Gary Yagi, 1 October 1985
  Current Cognizant Programmer: Damon D.. Knight
  Source Language: FORTRAN
  Ported to UNIX: Damon D. Knight 11 August 1993

3 Operation

  The wide-angle (line,sample) coordinate is converted into a vector
  (x,y,z) from the camera objective to the point in the focal plane
  corresponding to the pixel location.  The vector is then transformed
  into the narrow-angle camera field of view by multiplying the
  vector by a rotation matrix provided by Mert Davies (see Nov 1979
  Rand report), and subsequently converted into narrow-angle
  (line,sample) coordinates.
$ Return
$!#############################################################################
