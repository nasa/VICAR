$!****************************************************************************
$!
$! Build proc for MIPL module shady2
$! VPACK Version 1.8, Thursday, April 05, 2001, 16:23:33
$!
$! Execute by entering:		$ @shady2
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
$ write sys$output "*** module shady2 ***"
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
$ write sys$output "Invalid argument given to shady2.com file -- ", primary
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
$   if F$SEARCH("shady2.imake") .nes. ""
$   then
$      vimake shady2
$      purge shady2.bld
$   else
$      if F$SEARCH("shady2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake shady2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @shady2.bld "STD"
$   else
$      @shady2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create shady2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack shady2.com -
	-s shady2.f -
	-p shady2.pdf -
	-i shady2.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create shady2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C------------------------------------------------------------------------------
C	SHADY2.F	This program shades a VICAR image as though the
C			objects in the image were illuminated by a source
C			at a given azimuth and elevation.
C
C     MODIFIED FOR VAX CONVERSION BY ASM, 9 AUG 1983
C     JHR: CONVERTED TO VICAR2 1 JULY 1985
C     REA: CONVERTED TO UNIX/VICAR 6 MAY 1991
C     REA: MADE BYTE OUTPUT THE DEFAULT; HALFWORD OUTPUT OPTIONAL
C------------------------------------------------------------------------------
      EXTERNAL ISHADE
      COMMON /QQ/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT,MAXDN,QHALF
C
      REAL*4 LSCALE,ILVEC(3)
      INTEGER*4 OUNIT,STAT,SL,SS
      CHARACTER*8 FORMAT
      LOGICAL XVPTST,QHALF
C------------------------------------------------------------------------------
C PARAMETERS:
C
C AZ,R		THE AZIMUTH OF THE SOURCE, MEASURED FROM UP, PROCEEDING 
C		CLOCKWISE IN DEGREES.
C
C EL,R          SOURCE ELEVATION MEASURED FROM THE IMAGE PLANE WHERE VERTICAL 
C		IS 90. IN DEGREES.
C
C SSCALE,R      THE SCALE IN THE SAMPLE DIRECTION. DEFAULTS TO 208. FT/PIXEL 
C		WHICH IS DMA RESOLUTION.
C
C LSCALE,R      LINE SCALE. DEFAULTS TO 208 FT/PIXEL.
C
C ZSCALE,R      ALTITUDE SCALE. DEFAULTS TO 1 FT/DN, THE DMA RESOLUTION.
C
C SCALE,R       THE DN/COS(THETA) IN THE OUTBUF IMAGE, WHERE THETA IS THE ANGLE
C	 	BETWEEN THE SURFACE NORMAL AND THE ILLUMINATION DIRECTION.
C
C OFORMAT,STR   OUTPUT DATA FORMAT; EITHER BYTE (DEFAULT) OR HALF.
C------------------------------------------------------------------------------
C OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF','IO_ACT','SA',
     +		  'OPEN_ACT','SA',' ')
C
C GET DATA FORMAT AND CHECK
      QHALF = XVPTST('HALF')
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
	 CALL XVMESSAGE(' SHADY2 ACCEPTS BYTE OR HALFWORD DATA ONLY',' ')
	 CALL ABEND
      ENDIF
C
C GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
	 CALL XVMESSAGE(' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',
     +				' ')
	 CALL ABEND
      ENDIF
      IF(SS+NSO-1 .GT. NSI) THEN
	 CALL XVMESSAGE(
     +		      ' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',
     +				' ')
	 CALL ABEND
      ENDIF
C
C OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      IF (QHALF) THEN
          MAXDN = 32767
          CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     +                'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,
     +                'OPEN_ACT','SA','IO_ACT','SA',' ')
      ELSE
          MAXDN = 255
          CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     +                'O_FORMAT','BYTE','U_NL',NLO,'U_NS',NSO,
     +                'OPEN_ACT','SA','IO_ACT','SA',' ')
      END IF
C
C PROCESS PARAMETERS
      CALL PARPRO
C
C DETERMINE CORE SIZE NEEDED
      NBYTES=2*NSO
C
C CALL SUBROUTINE ISHADE VIA STACKA   (ALLOC 4 HALFWORD BUFFERS)
      CALL STACKA(6,ISHADE,4,NBYTES,NBYTES,NBYTES,NBYTES)
C
C CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE PARPRO
C
      COMMON /QQ/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT,MAXDN,QHALF
C
      REAL*4 AZ,EL,LSCALE,SSCALE,ZSCALE,SCALE,ILVEC(3)
      REAL*4 PI/3.141592/
      INTEGER*4 OUNIT,SL,SS
      CHARACTER*8 FORMAT
      LOGICAL QHALF
C
C Read in parameter values from the process:
C
      CALL XVPARM('AZIMUTH',AZ,ICOUNT,IDEF,1)
      CALL XVPARM('ELEV',EL,ICOUNT,IDEF,1)
      CALL XVPARM('SSCALE',SSCALE,ICOUNT,IDEF,1)
      CALL XVPARM('LSCALE',LSCALE,ICOUNT,IDEF,1)
      CALL XVPARM('ZSCALE',ZSCALE,ICOUNT,IDEF,1)
      CALL XVPARM('SCALE',SCALE,ICOUNT,IDEF,1)
C
C Convert degrees to radians
C
      AZ=180.0-AZ
      IF(AZ.LT.0.0) AZ=AZ+360.0
      REL = (EL/360.)*2*PI
      RAZ = (AZ/360.)*2*PI
      CALL SPHREC(ILVEC,1.0,REL,RAZ)
C
C ILVEC IS THE UNIT VECTOR IN THE DIRECTION OF ILLUMINATION
C
      IF(SCALE.EQ.0.0.AND.FORMAT.EQ.'HALF') SCALE=32767.
      IF(SCALE.EQ.0.0.AND.FORMAT.EQ.'BYTE') SCALE=255.
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE ISHADE(INBUF1,NB1,INBUF2,NB2,INBUF3,NB3,OUTBUF,NB4)
C
      COMMON /QQ/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT,MAXDN,QHALF
C
      REAL*4 S(3),L(3),Z(3),R1,R2,ILVEC(3),LSCALE
      INTEGER*4 LINE,SAMP,EL,ES,SL,SS,OUNIT,STAT
      INTEGER*2 INBUF1(*),INBUF2(*),INBUF3(*),OUTBUF(*)
      CHARACTER*8 FORMAT
      LOGICAL QHALF
C
      EL=NLO-1
      ES=NSO-1
      NBYTES=2*NSO
      IREC=SL
      CALL XVREAD(IUNIT,INBUF1,STAT,'LINE',IREC,
     &                              'SAMP',SS,'NSAMPS',NSO,' ')
      CALL XVREAD(IUNIT,INBUF2,STAT,'LINE',IREC+1,
     &                              'SAMP',SS,'NSAMPS',NSO,' ')
      CALL XVREAD(IUNIT,INBUF3,STAT,'LINE',IREC+2,
     &                              'SAMP',SS,'NSAMPS',NSO,' ')
C
C SET CONSTANT VECTOR COMPONENTS
      L(1)=LSCALE
      L(2)=0.0
      S(1)=0.0
      S(2)=SSCALE
      Z(3)=L(1)*S(2)
C COMPUTE INTERMEDIATE RESULTS TO SAVE CPU TIME
      A3=L(1)*S(2)*ILVEC(3)
      A4=Z(3)*Z(3)
C
C        *** FIRST LINE ***
C
C UPPER LEFT CORNER
C
      S(3) = (INBUF1(2)-INBUF1(1))*ZSCALE
      S(1) = 0
      S(2) = SSCALE
      L(2) = 0
      L(3) = (INBUF2(1)-INBUF1(1))*ZSCALE
      L(1) = LSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(1)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
C CENTER SAMPLES
C
      DO 100 SAMP=2,ES
      S(3)=(INBUF1(SAMP+1)-INBUF1(SAMP-1))*ZSCALE
      L(3)=(INBUF2(SAMP)-INBUF1(SAMP))*ZSCALE
      Z(1)=L(3)*S(2)
      Z(2)=L(1)*S(3)
      R1=A3-Z(1)*ILVEC(1)-Z(2)*ILVEC(2)
      R2=SQRT(Z(1)*Z(1)+Z(2)*Z(2)+A4)
      OUTBUF(SAMP)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
  100 CONTINUE
C
C UPPER RIGHT CORNER
C
      S(3)=(INBUF1(NSO)-INBUF1(NSO-1))*ZSCALE
      L(3)=(INBUF2(NSO)-INBUF1(NSO))*ZSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(NSO)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
      CALL XVWRIT(OUNIT,OUTBUF,STAT,'NSAMPS',NSO,' ')
C
C        *** MIDDLE LINES ***
C
      DO 250 LINE=2,EL
C
C LEFT SAMPLE
C
      S(3)=(INBUF2(2)-INBUF2(1))*ZSCALE
      L(3)=(INBUF3(2)-INBUF1(2))*ZSCALE
      Z(1)=L(3)*S(2)
      Z(2)=L(1)*S(3)
      R1=A3-Z(1)*ILVEC(1)-Z(2)*ILVEC(2)
      R2=SQRT(Z(1)*Z(1)+Z(2)*Z(2)+A4)
      OUTBUF(1)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
C CENTER SAMPLES
C
      DO 240 SAMP=2,ES
      S(3)=(INBUF2(SAMP+1)-INBUF2(SAMP-1))*ZSCALE
      L(3)=(INBUF3(SAMP)-INBUF1(SAMP))*ZSCALE
      Z(1)=L(3)*S(2)
      Z(2)=L(1)*S(3)
      R1=A3-Z(1)*ILVEC(1)-Z(2)*ILVEC(2)
      R2=SQRT(Z(1)*Z(1)+Z(2)*Z(2)+A4)
      OUTBUF(SAMP)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
  240 CONTINUE
C
C RIGHT SAMPLE
C
      S(3)=(INBUF2(NSO)-INBUF2(NSO-1))*ZSCALE
      L(3)=(INBUF3(NSO)-INBUF1(NSO))*ZSCALE
      Z(1)=L(3)*S(2)
      Z(2)=L(1)*S(3)
      R1=A3-Z(1)*ILVEC(1)-Z(2)*ILVEC(2)
      R2=SQRT(Z(1)*Z(1)+Z(2)*Z(2)+A4)
      OUTBUF(NSO)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
      CALL XVWRIT(OUNIT,OUTBUF,STAT,'NSAMPS',NSO,' ')
C
C AFTER NEXT TO LAST LINE DON'T READ ANYMORE
C
      IF(LINE.EQ.EL) GO TO 250
      CALL MVL(INBUF2,INBUF1,NBYTES)
      CALL MVL(INBUF3,INBUF2,NBYTES)
      IREC=SL+LINE+1
      CALL XVREAD(IUNIT,INBUF3,STAT,'LINE',IREC,'SAMP',SS,
     +		  'NSAMPS',NSO,' ')
  250 CONTINUE
C
C        *** LAST LINE ***
C
C LOWER LEFT CORNER
C
      S(3)=(INBUF3(2)-INBUF3(1))*ZSCALE
      L(3)=(INBUF3(1)-INBUF2(1))*ZSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(1)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
C CENTER SAMPLES
C
      DO 300 SAMP=2,ES
      S(3)=(INBUF3(SAMP+1)-INBUF3(SAMP-1))*ZSCALE
      L(3)=(INBUF3(SAMP)-INBUF2(SAMP))*ZSCALE
      Z(1)=L(3)*S(2)
      Z(2)=L(1)*S(3)
      R1=A3-Z(1)*ILVEC(1)-Z(2)*ILVEC(2)
      R2=SQRT(Z(1)*Z(1)+Z(2)*Z(2)+A4)
      OUTBUF(SAMP)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
  300 CONTINUE
C
C LOWER RIGHT CORNER
C
      S(3)=(INBUF3(NSO)-INBUF3(NSO-1))*ZSCALE
      L(3)=(INBUF3(NSO)-INBUF2(NSO))*ZSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(NSO)=MIN(MAXDN,MAX1(R1/R2*SCALE,0.0))
C
      CALL XVWRIT(OUNIT,OUTBUF,STAT,'NSAMPS',NSO,' ')
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE VECTOR
C
      REAL R(3),RMAG,LAT,LONG
      REAL A(3),B(3),C(3)
C
      ENTRY SPHREC(R,RMAG,LAT,LONG)
C CONVERT SPHERCAL COORDINATES TO RECTANGULAR VECTOR
      R(1)=RMAG*COS(LAT)*COS(LONG)
      R(2)=RMAG*COS(LAT)*SIN(LONG)
      R(3)=RMAG*SIN(LAT)
      RETURN
C
      ENTRY RECSPH(R,RMAG,LAT,LONG)
C CONVERT RECTANGULAR VECTOR COMPONENTS TO SPHERICAL
      RMAG=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      LAT=ATAN2(R(3),SQRT(R(1)**2+R(2)**2))
      LONG=ATAN2(R(2),R(1) )
      RETURN
C
      ENTRY UNIT(R)
C MAKE R A UNIT VECTOR IN SAME DIRECTION
      X=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      DO 10 J=1,3
10    R(J)=R(J)/X
      RETURN
C
      ENTRY MAG(A,RMAG)
C MAGNITUDE OF VECTOR
      RMAG=SQRT(A(1)*A(1)+A(2)*A(2)+A(3)*A(3))
      RETURN
C
      ENTRY DOT (A,B,PROD)
C VECTOR DOT PRODUCT
      PROD=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
      RETURN
C
      ENTRY CROSS(A,B,C)
C VECTOR CROSS PRODUCT C = A X B
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create shady2.pdf
!------------------------------------------------------------------------------
! process SHADY2.pdf 
!------------------------------------------------------------------------------
process help=*
PARM INP 	TYPE=(STRING,80)		
PARM OUT 	TYPE=(STRING,80)		
PARM SIZE 	TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0) 
PARM SL 	TYPE=INTEGER DEFAULT=1		
PARM SS 	TYPE=INTEGER DEFAULT=1		
PARM NL 	TYPE=INTEGER DEFAULT=0		
PARM NS 	TYPE=INTEGER DEFAULT=0		
PARM OFORMAT    TYPE=KEYWORD DEFAULT=BYTE VALID=(BYTE,HALF) 
PARM AZIMUTH 	TYPE=REAL DEFAULT=0.0		
PARM ELEV 	TYPE=REAL DEFAULT=45.0		
PARM SSCALE 	TYPE=REAL DEFAULT=208.0		
PARM LSCALE 	TYPE=REAL DEFAULT=208.0		
PARM ZSCALE 	TYPE=REAL DEFAULT=1.0		
PARM SCALE 	TYPE=REAL DEFAULT=254.0		
END-PROC
.TITLE
VICAR2 program SHADY2
.HELP
PURPOSE

SHADY2 shades a image as though it were being illuminated by a source
at a given azimuth and elevation.
.page
EXECUTION

Example:
To shade the input image as though it were illuminated by a sun in the 
northeast at an elevation of 10 degrees above the horizon, enter

VICAR> SHADY2 inp=input.img out=output.img azimuth=45.0 elev=10.0
.page
OPERATION

SHADY2 uses the four nearest neighbors around each pixel to compute a
local normal.  The cosine between this vector and the illumination
vector is computed and multiplied by SCALE to obtain the output DN.  The
subroutine STACKA is used to allocate buffers so that no size restrictions
exist.
.page
HISTORY

Written by:  M.A. Girard, 25 July 1978
Cognizant programmer:  M. O'Shaughnessy

Revisions: 
J.H. Reimer, 10 April 1983  
	Original documentation
J.H. Reimer, 1 July 1985
	Converted to VICAR2 format
M. O'Shaughnessy, 9 October 1989
	Extended documentation, upgraded SHADY2 to r2lib, 
	wrote unit test procedure
Ron Alley, 7 February 2000
        Made BYTE the default output data format, with HALF optional.
.LEVEL1
.VARIABLE inp
Name of input image file
.VARIABLE out
Name of output image file
.VARIABLE size
Standard VICAR size field
.VARIABLE sl
Starting line
.VARIABLE ss
Starting sample
.VARIABLE nl
Number of lines
.VARIABLE ns
Number of samples
.VARIABLE oformat
Output image format
(BYTE or HALF)
.VARIABLE azimuth
Azimuth of light source
.VARIABLE elev
Elevation of light source
.VARIABLE sscale
Sample scale (ft/pixel)
.VARIABLE lscale
Line scale (ft/pixel)
.VARIABLE zscale
Vertical scale (ft/dn)
.VARIABLE scale
DN scale factor
.LEVEL2
.VARIABLE oformat
By default, the output pixels are stored as BYTE data.  The user may
optionally specify HALF (16 bit, signed pixels) as the output data
image format.
.VARIABLE azimuth
AZIMUTH is the azimuth of the illumination source (the 'sun') in real 
degrees measured clockwise from 'up' in the image plane.  

examples:
azimuth=0.0 	places the source at the top of the image
azimuth=90.0 	places the source to the right of the image. 
.VARIABLE elev
ELEV is the elevation of the illumination source in real degrees measured
from the image plane. 

examples:
elev=0.0 	puts the 'sun' at sunset
elev=90.0 	puts the 'sun' directly overhead
.VARIABLE sscale
SSCALE is the scale in the sample direction.  Default is 208.0 which is
the DMA resolution in feet/pixel.
.VARIABLE lscale
LSCALE is the scale in the line direction.  Default is 208.0.
.VARIABLE zscale
ZSCALE is the scale in the Z or vertical direction.  Default is 1.0 which
is the DMA resolution in feet/dn.
.VARIABLE scale
For each input pixel, the cosine of the angle between the illumination 
direction and the local normal is calculated.  SCALE is a constant which,
when multiplied by the cosine, gives the output DN.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create shady2.imake
#define  PROGRAM   shady2

#define MODULE_LIST shady2.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
