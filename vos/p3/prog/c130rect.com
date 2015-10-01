$!****************************************************************************
$!
$! Build proc for MIPL module c130rect
$! VPACK Version 1.8, Wednesday, October 08, 2003, 16:01:41
$!
$! Execute by entering:		$ @c130rect
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
$!   PDF         Only the PDF file is created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module c130rect ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to c130rect.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("c130rect.imake") .nes. ""
$   then
$      vimake c130rect
$      purge c130rect.bld
$   else
$      if F$SEARCH("c130rect.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake c130rect
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @c130rect.bld "STD"
$   else
$      @c130rect.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create c130rect.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack c130rect.com -
	-s c130rect.f -
	-i c130rect.imake -
	-p c130rect.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create c130rect.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 24 AUGUST 1983
C     MODIFIED TO VICAR2 BY RICH WALKER, 28 AUGUST 1985
C     FIXED BUG WHEN TILT > DEFL
C     MODIFIED FOR NON-BYTE DATA
C     CONVERTED TO UNIX VICAR  4/18/91 Ron Alley
C     Modified to handle multichannel (BSQ, BIL) datasets  1/24/97  Ron Alley
C**********************************************************************
      SUBROUTINE MAIN44
      LOGICAL XVPTST
      REAL PI/3.1415927/
      REAL RBUF(8000),OBUFR(32000),FRAC(32000)
      INTEGER ADDR(32000)
      CHARACTER*80 PRT
      CHARACTER*4 FORMAT
      CHARACTER *3 ORG
C
      CALL XVMESSAGE(' C130RECT VERSION 2.1',' ')
C							 OPEN THE INPUT DATA SET
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		      'U_FORMAT','REAL',' ')
      CALL XVGET(INP,ISTAT,'FORMAT',FORMAT,' ')
      CALL XVGET(INP,ISTAT,'ORG',ORG,' ')
      CALL XVSIZE(ISL,ISS,NLO,NS,NLI,NSI)
      CALL XVBANDS(ISB,NB,NBIN)
C
      IF (ORG .EQ. 'BIP') THEN
	 CALL XVMESSAGE(' BIP files are not currently supported',' ')
	 CALL ABEND
      ENDIF
C
      IF(ISS.NE.1) THEN
       CALL XVMESSAGE(' Starting sample must be 1',' ')
       CALL XVMESSAGE(' Resetting starting sample to 1',' ')
       ISS = 1
      ENDIF
      IF(NS.NE.0.AND.NS.NE.NSI) THEN
       CALL XVMESSAGE(' Number of samples must be entire input image',
     +			' ')
       CALL XVMESSAGE(' Resetting number of samples to entire input',
     +			' ')
       NS = NSI
      ENDIF
C
      IEL = ISL + NLO - 1
C
      IF(IEL.GT.NLI) THEN
	 CALL XVMESSAGE(
     +	     ' Size field NL specified overruns file dimensions',' ')
	 CALL ABEND
      ENDIF
C
C						        GET THE PARAMETERS
      CALL XVPARM('DEFL',DEFL,ICNT,IDEF,0)
      CALL XVPARM('TILT',TILT,ICNT,IDEF,0)
      CALL XVPARM('SCALE',SCALE,ICNT,IDEF,0)
C						        VERIFY THE PARAMETERS
      IF(DEFL.GT.85.)  DEFL= 85.
      WRITE (PRT,100) DEFL+0.0001
  100 FORMAT('.... DEFL=',F5.2,' DEG.')
      CALL XVMESSAGE(PRT,' ')
      WRITE (PRT,200) SCALE+0.0001
  200 FORMAT('.... SCALE=',F5.2)
      CALL XVMESSAGE(PRT,' ')
      IF (TILT.NE.0.) THEN
          WRITE (PRT,300) TILT+0.0001
  300     FORMAT('.... TILT=',F5.2,' DEG.')
          CALL XVMESSAGE(PRT,' ')
      END IF
      DEFL= DEFL*PI/180.0
      TILT = TILT*PI/180.0
C
      IF (TILT.NE.0.) THEN
          SMAX = NS
          H = (SMAX/(DEFL*2))*SCALE
          HH = H*COS(TILT)
          XMAX1 = HH*TAN(DEFL+TILT)
          XMAX2 = HH*TAN(DEFL-TILT)
          IMAX1 = XMAX1 + 0.5
          THETA = DEFL+TILT
          XMAX = XMAX1 + XMAX2
          IMAX = XMAX + 0.5
          NSO = XMAX+1.5
      ELSE
          R= NS
          SMAX= (R-1.)/2.
          H = (SMAX/DEFL)*SCALE
          XMAX= H*TAN(DEFL)
          NSO= 2.*XMAX+1.5
      END IF
C
      WRITE (PRT,400) NLO,NSO
  400 FORMAT('.... OUTPUT    NL=',I5,'   NS=',I5)
      CALL XVMESSAGE(PRT,' ')
      IF(NSO.GT.32000) THEN
	 CALL XVMESSAGE(' $$$ TOO MANY OUTPUT SAMPLES $$$',' ')
	 CALL ABEND
      ENDIF
C						        OPEN THE OUTPUT DATA SET
      CALL XVUNIT(OUT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA','LAB_ACT',
     +   'SA','U_NL',NLO,'U_NS',NSO,'OP','WRITE','U_FORMAT','REAL',' ')
C					      LOAD THE RESAMPLING ADDRESS BUFFER
      IF (TILT.NE.0) THEN
	  KK = MIN(NSO,IMAX1)
          DO J=1,KK
              R = J - 1
              S = H*ATAN((R+XMAX2-XMAX)/HH)+THETA*H
              IF (S.LT.0) S = 0.
              S = S/SCALE
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
	  END DO
          KK = MAX(IMAX1+1,1)
          DO J = KK,IMAX
              R=J-1
              S=H*ATAN((R-XMAX1)/HH)+THETA*H
              S = S/SCALE
              IF (S.LT.0) S=0.
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
 	  END DO
      ELSE
          DO J=1,NSO
              R= J-1
              X= R-XMAX
              THETA= ATAN(X/H)
              S = THETA*H/SCALE+SMAX
              IF(S.LT.0.)  S= 0.
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
	  END DO
      END IF
C						 DUMP THE RESAMPLING ADDR BUFFER
      IF(XVPTST('DEBUG')) THEN
          CALL XVMESSAGE(' OUTPUT    INPUT',' ')
          DO J=1,NSO
              WRITE (PRT,500) J, ADDR(J)+FRAC(J)
  500         FORMAT(I5,F12.4)
              CALL XVMESSAGE(PRT,' ')
	  END DO
      END IF
C				    RESAMPLE EACH LINE TO CORRECT THE DISTORTION
      IF (ORG .EQ. 'BSQ') THEN
        IF (FORMAT.EQ.'REAL') THEN
          DO IB=1,NB
	    DO ILINE=ISL,IEL
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = (1.0-FRAC(I))*RBUF(ADDR(I)) +
     +                        FRAC(I)*RBUF(ADDR(I)+1)
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
            END DO
	  END DO
        ELSE
          DO IB=1,NB
	    DO ILINE=ISL,IEL
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = NINT( (1.0-FRAC(I))*RBUF(ADDR(I))
     +                            + FRAC(I)*RBUF(ADDR(I)+1))
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
            END DO
          END DO
	END IF
      ELSE
        IF (FORMAT.EQ.'REAL') THEN
	  DO ILINE=ISL,IEL
            DO IB=1,NB
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = (1.0-FRAC(I))*RBUF(ADDR(I)) +
     +                        FRAC(I)*RBUF(ADDR(I)+1)
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
	    END DO
          END DO
        ELSE
	  DO ILINE=ISL,IEL
            DO IB=1,NB
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = NINT( (1.0-FRAC(I))*RBUF(ADDR(I))
     +                            + FRAC(I)*RBUF(ADDR(I)+1))
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
	    END DO
          END DO
        END IF
      END IF
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create c130rect.imake
#define  PROGRAM   c130rect

#define MODULE_LIST c130rect.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create c130rect.pdf
process help=*
PARM INP   TYPE=STRING
PARM OUT   TYPE=STRING
PARM SIZE  TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL    TYPE=INTEGER DEFAULT=1
PARM SS    TYPE=INTEGER DEFAULT=1
PARM NL    TYPE=INTEGER DEFAULT=0
PARM NS    TYPE=INTEGER DEFAULT=0
PARM DEFL  TYPE=REAL    DEFAULT=38.28
PARM TILT  TYPE=REAL    DEFAULT=0.0
PARM SCALE TYPE=REAL    DEFAULT=1.0
PARM DEBUG TYPE=KEYWORD COUNT=(0:1) VALID=DEBUG DEFAULT=--
END-PROC
.TITLE
C130RECT
.HELP
PURPOSE:
C130RECT removes panorama distortion in images acquired by scanners that
sample at equal angular increments. Tilt can be corrected and the aspect 
ratio of the image can be adjusted.
 
EXECUTION:
 
Example 
	C130RECT  INP  OUT  DEFL=30.0  TILT=11.4  SCALE=1.5
 
	In this example, C130RECT will remove the scanning distortion
	in image INP, using a 30 degree deflection from edge to center,
        an expansion factor of 1.5, and a center 11.4 degrees off nadir.
 
 
OPERATION:
The program is essentially divided into three parts.  The first of these
handles simple initialization and computes (using basic trigonometry) 
values for the variables which are dependent on the image tilt and scan
width.  The second part of the program fills a resampling address array
which, for each input pixel position, gives a real-valued index into
the input buffer from which values may be interpolated; this array mapping
may be dumped to the screen by specifying the DEBUG option.  The third part
of the program reads in each line of the image, and using the resampling
address array and c routine resamp, corrects each line for distortion
according to the tilt and scan width specified, finally outputing each 
line to the output image. 
 
 
WRITTEN BY:  John Addington, 7 February 1978
COGNIZANT PROGRAMMER:  Michael Girard
REVISION:  New
 
.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE OUT
Output image file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample - must be 1
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples -
 must be equal to input NS
.VARIABLE DEFL
Scan deflection from center,
in degrees
.VARIABLE TILT
Pointing angle of the central
pixel, in degrees from nadir
.VARIABLE SCALE
Expansion factor
.VARIABLE DEBUG
Prints resampling table
.LEVEL2
.VARIABLE DEFL
DEFL indicates the full deflection from nadir (or from the tilt angle, if
the parameter TILT is being used), in degrees.  (Default is 38.28, the
proper value for TIMS processing. 42.96 is the correct value for MASTER.)
.VARIABLE TILT
TILT is the off-nadir tilt in degrees.  (Default=0.0.)
.VARIABLE SCALE
SCALE specifies the expansion (>1.) or contraction (<1.) factor in the
sample direction for altering the aspect ratio.  (Default=1.0.)
.VARIABLE DEBUG
The keyword parameter DEBUG causes the resampling transformation look-up 
table to be printed. 
.END
$ Return
$!#############################################################################
