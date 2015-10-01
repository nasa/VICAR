$!****************************************************************************
$!
$! Build proc for MIPL module shady2
$! VPACK Version 1.7, Wednesday, July 28, 1993, 14:00:30
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
$!   TEST        Only the test files are created.
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
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
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
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
	-i shady2.imake -
	-p shady2.pdf -
	-t tstshady2.pdf
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
C	SHADY2.FOR	This program shades a VICAR image as though the
C			objects in the image were illuminated by a source
C			at a given azimuth and elevation.
C
C     MODIFIED FOR VAX CONVERSION BY ASM, 9 AUG 1983
C     JHR: CONVERTED TO VICAR2 1 JULY 1985
C     GAM: PORTED TO UNIX 28 JULY 1993
C------------------------------------------------------------------------------
      EXTERNAL ISHADE
      COMMON /C1/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT
C
      REAL*4 LSCALE,ILVEC(3)
      INTEGER*4 OUNIT,STAT,SL,SS,IPIXSIZE
      CHARACTER*8 FORMAT
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
C------------------------------------------------------------------------------
C SET ERROR CODE AND PRINT DISCLAIMER
      CALL XVEACTION('SA',' ')
      CALL XVMESSAGE('SHADY2 version  28-JUL-1993',' ')
C OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      STAT = XVPIXSIZEU(IPIXSIZE, FORMAT, IUNIT)
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('SHADY2 ACCEPTS BYTE OR HALFWORD DATA ONLY',
     &   ' ')
         CALL ABEND
      END IF
C
C GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',
     &	 ' ')
         CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE(
     &   'NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
C
C OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,' ')
C
C PROCESS PARAMETERS
      CALL PARPRO
C
C DETERMINE CORE SIZE NEEDED
      NBYTES=2*NSO
      LCHECK=NBYTES
C
C CALL SUBROUTINE ISHADE VIA STACKA   (ALLOC 4 HALFWORD BUFFERS)
      CALL STACKA(8,ISHADE,4,NBYTES,NBYTES,NBYTES,NBYTES,LCHECK,IND)
      IF(IND.EQ.1) GO TO 995
C
C CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
C
C ERROR RETURNS
995   CALL ABEND
      END
C
C**********************************************************************
      SUBROUTINE PARPRO
C
      COMMON /C1/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT
C
      REAL*4 AZ,EL,LSCALE,SSCALE,ZSCALE,SCALE,ILVEC(3)
      REAL*4 PI
      DATA PI/3.141592/
      INTEGER*4 OUNIT,SL,SS
      CHARACTER*8 FORMAT
C
C Set default values
C
      AZ=0.
      EL=45.
      SCALE=0.
      SSCALE=208.0
      LSCALE=208.0
      ZSCALE=1.0
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
100   AZ=180.0-AZ
      IF(AZ.LT.0.0) AZ=AZ+360.0
      REL = (EL/360.)*2*PI
      RAZ = (AZ/360.)*2*PI
      CALL SPHREC(ILVEC,1.0,REL,RAZ)
C
C ILVEC IS THE UNIT VECTOR IN THE DIRECTION OF ILLUMINATION
C
      IF(SCALE.EQ.0.0.AND.FORMAT.EQ.'HALF') SCALE=16383.
      IF(SCALE.EQ.0.0.AND.FORMAT.EQ.'BYTE') SCALE=254.
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE ISHADE(INBUF1,NB1,INBUF2,NB2,INBUF3,NB3,OUTBUF,NB4,
     &                  LCHECK,IND)
C
      COMMON /C1/ SL,SS,NLO,NSO,FORMAT,LSCALE,SSCALE,ZSCALE,SCALE,
     &            ILVEC,IUNIT,OUNIT
C
      REAL*4 S(3),L(3),Z(3),R1,R2,ILVEC(3),LSCALE
      INTEGER*4 LINE,SAMP,EL,ES,SL,SS,OUNIT,STAT
      INTEGER*2 INBUF1(*),INBUF2(*),INBUF3(*),OUTBUF(*)
      CHARACTER*8 FORMAT
C
C CHECK CORE ALLOCATION
      IND=0
      IF(NB4.LT.LCHECK) GO TO 995
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
      OUTBUF(1)=AMAX1(R1/R2*SCALE,0.)
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
      OUTBUF(SAMP)=AMAX1(R1/R2*SCALE,0.)
  100 CONTINUE
C
C UPPER RIGHT CORNER
C
      S(3)=(INBUF1(NSO)-INBUF1(NSO-1))*ZSCALE
      L(3)=(INBUF2(NSO)-INBUF1(NSO))*ZSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(NSO)=AMAX1(R1/R2*SCALE,0.)
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
      OUTBUF(1)=AMAX1(R1/R2*SCALE,0.)
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
      OUTBUF(SAMP)=AMAX1(R1/R2*SCALE,0.)
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
      OUTBUF(NSO)=AMAX1(R1/R2*SCALE,0.)
C
      CALL XVWRIT(OUNIT,OUTBUF,STAT,'NSAMPS',NSO,' ')
C
C AFTER NEXT TO LAST LINE DON'T READ ANYMORE
C
      IF(LINE.EQ.EL) GO TO 250
      CALL MVE(2,NBYTES/2,INBUF2,INBUF1,1,1)
      CALL MVE(2,NBYTES/2,INBUF3,INBUF2,1,1)
      IREC=SL+LINE+1
      CALL XVREAD(IUNIT,INBUF3,STAT,'LINE',IREC,'SAMP',SS,'NSAMPS',NSO,
     &	' ')
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
      OUTBUF(1)=AMAX1(R1/R2*SCALE,0.)
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
      OUTBUF(SAMP)=AMAX1(R1/R2*SCALE,0.)
  300 CONTINUE
C
C LOWER RIGHT CORNER
C
      S(3)=(INBUF3(NSO)-INBUF3(NSO-1))*ZSCALE
      L(3)=(INBUF3(NSO)-INBUF2(NSO))*ZSCALE
      CALL CROSS(L,S,Z)
      CALL DOT(Z,ILVEC,R1)
      CALL MAG(Z,R2)
      OUTBUF(NSO)=AMAX1(R1/R2*SCALE,0.)
C
      CALL XVWRIT(OUNIT,OUTBUF,STAT,'NSAMPS',NSO,' ')
C
      RETURN
C
C INSUFFICIENT CORE RETURN
995   CALL XVMESSAGE('INSUFFICIENT CORE',' ')
      IND=1
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE VECTOR
C
      REAL R(3),RMAG,LAT,LONG
      REAL A(3),B(3),C(3)
      REAL D(3,3),E(3,3),F(3,3)
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
C
      ENTRY MMUL(D,E,F)
C 3 X 3 MATRIX PRODUCT F=D*E
      DO 20 I=1,3
      DO 15 J=1,3
      S=0.
      DO 12 K=1,3
12    S=S+D(I,K)*E(K,J)
      F(I,J)=S
15    CONTINUE
20    CONTINUE
      RETURN
C
      ENTRY VMUL(A,D,R)
C VECTOR (3 X 1) = MATRIX (3 X 3) * VECTOR (3 X 1)  R=D*A
      DO 50 I=1,3
      S=0.
      DO 40 J=1,3
40    S=S+D(I,J)*A(J)
50    R(I)=S
      RETURN
C
      ENTRY TRANS(D,E)
C TRANSPOSE 3 X 3 MATRIX  E=TRANSPOSE(D)
      DO 60 I=1,3
      DO 60 J=1,3
   60 E(J,I)=D(I,J)
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create shady2.imake
#define  PROGRAM   shady2

#define MODULE_LIST shady2.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create shady2.pdf
!------------------------------------------------------------------------------
! process shady2.pdf 
!------------------------------------------------------------------------------
process help=*
PARM INP 	TYPE=STRING			!input image file
PARM OUT 	TYPE=STRING			!output image file
PARM SIZE 	TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0) !VICAR image field size
PARM SL 	TYPE=INTEGER DEFAULT=1		!starting line
PARM SS 	TYPE=INTEGER DEFAULT=1		!starting sample
PARM NL 	TYPE=INTEGER DEFAULT=0		!number of lines
PARM NS 	TYPE=INTEGER DEFAULT=0		!number of samples
PARM AZIMUTH 	TYPE=REAL DEFAULT=0.0		!azimuth of light source
PARM ELEV 	TYPE=REAL DEFAULT=45.0		!elevation of light source
PARM SSCALE 	TYPE=REAL DEFAULT=208.0		!sample scale (ft/px)
PARM LSCALE 	TYPE=REAL DEFAULT=208.0		!line scale ( ft/px)
PARM ZSCALE 	TYPE=REAL DEFAULT=1.0		!vertical scale (ft/px)
PARM SCALE 	TYPE=REAL DEFAULT=254.0		!scale factor
END-PROC
.TITLE
VICAR2 program shady2
.HELP
PURPOSE

shady2 shades a image as though it were being illuminated by a source
at a given azimuth and elevation.
.page
EXECUTION

Example:
To shade the input image as though it were illuminated by a sun in the 
northeast at an elevation of 10 degrees above the horizon, enter

VICAR> shady2 inp=input.img out=output.img azimuth=45.0 elev=10.0
.page
OPERATION

shady2 uses the four nearest neighbors around each pixel to compute a
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
	Extended documentation, upgraded shady2 to r2lib, 
	wrote unit test procedure
G. Madrid,  28-JUL-1993 
	Ported to UNIX
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
.VARIABLE ns
Number of samples
.VARIABLE nl
Number of lines
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

Default values are 254 for byte images and 16383 for halfword images.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstshady2.pdf
procedure help=*
!------------------------------------------------------------------------------
! tstshady2.pdf  Unit test for the program shady2
! written by: M. O'Shaughnessy  10-9-1989
! extensions: G. Madrid		 28-JUL-1993 
!
! If there are problems running fracgen under UNIX, just comment out that
! portion of the pdf and ftp the terrain.img and shaded.img files from your
! VAX account.  The test pdf should then work reasonably well, then.
!
!
!------------------------------------------------------------------------------
refgbl $becho
refgbl $autousage

body
let $autousage = "none"
let $becho="yes"

write "THIS IS A TEST OF MODULE SHADY2"
!------------------------------------------------------------------------------
! create test image
!------------------------------------------------------------------------------
write "FIRST CREATE THE TEST FILE"
fracgen filea nl=256 ns=256 format=byte seed=32161267
write "TESTING PROGRAM SHADY2 WITH DEFAULTS"
!------------------------------------------------------------------------------
! test the program SHADY2
shady2 inp=filea out=fileb 
write "LIST THE RESULT OF THE DEFAULT PARAMETERS"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET AZIMUTH PARAMETER TO 90 DEGREES"
shady2 inp=filea out=fileb az=90.0
write "LIST THE RESULTS WITH AZIMUTH = 90 DEGREES"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET AZIMUTH PARAMETER TO 45 DEGREES"
shady2 inp=filea out=fileb az=45.0
write "LIST THE RESULTS WITH AZIMUTH = 45 DEGREES"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET ELEVATION PARAMETER TO 0 DEGREES"
shady2 inp=filea out=fileb el=0.0
write "LIST THE RESULTS WITH ELEVATION = 0 DEGREES"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET ELEVATION PARAMETER TO 90 DEGREES"
shady2 inp=filea out=fileb el=90.0
write "LIST THE RESULTS WITH ELEVATION = 90 DEGREES"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET SAMPLE SCALE PARAMETER TO 100 FT/PX"
shady2 inp=filea out=fileb sscale=100.0
write "LIST THE RESULTS WITH SAMPLE SCALE = 100 FT/PX"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET LINE SCALE PARAMETER TO 100 FT/PX"
shady2 inp=filea out=fileb lscale=100.0
write "LIST THE RESULTS WITH LINE SCALE = 100 FT/PX"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET VERTICAL SCALE PARAMETER TO 5 FT/PX"
shady2 inp=filea out=fileb zscale=5.0
write "LIST THE RESULTS WITH VERTICAL SCALE = 5 FT/PX"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "SET SCALE PARAMETER TO 128"
shady2 inp=filea out=fileb scale=128
write "LIST THE RESULTS WITH SCALE = 128"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

write "TEST SHADY2 WITH DIFFERENT AZIMUTH AND ELEVATION VALUES TOGETHER"
shady2 inp=filea out=fileb az=45.0 el=15.0
write "LIST THE RESULTS OF COMBINING DIFFERENT PARAMETER VALUES"
list fileb (1,1,5,10)
list fileb (201,201,5,10)

end-proc
.help
To examine the performance of shady2, compare the source image terrain.img
with the output file shaded.img.

To run in batch in VMS type:
	> tstshady2 |run=(batch,fast)| 
To run interactively in VMS type:
	> tstshady2 
where xxxxxx is the VMS username

To run in batch in UNIX type:
	> tstshady2 |run=batch| 
To run interactively in UNIX type:
	> tstshady2 
where xxx is the UNIX username
.end
$ Return
$!#############################################################################
