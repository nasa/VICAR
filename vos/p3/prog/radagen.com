$!****************************************************************************
$!
$! Build proc for MIPL module radagen
$! VPACK Version 1.8, Wednesday, October 11, 2000, 02:34:33
$!
$! Execute by entering:		$ @radagen
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
$ write sys$output "*** module radagen ***"
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
$ write sys$output "Invalid argument given to radagen.com file -- ", primary
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
$   if F$SEARCH("radagen.imake") .nes. ""
$   then
$      vimake radagen
$      purge radagen.bld
$   else
$      if F$SEARCH("radagen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake radagen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @radagen.bld "STD"
$   else
$      @radagen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create radagen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack radagen.com -
	-s radagen.f -
	-i radagen.imake -
	-p radagen.pdf -
	-t tstradagen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create radagen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C##  RADAGEN
C##      INCLUDE TLOCATOR REFERENCES
C##      DELETE TEMP1,TEMP2 IN SUBROUTINE MOVE_OUTPUT
C##      CONVERTED TO VICAR2
C
C##	 INTERNAL RECTIFICATION ADDED 3-6-86
C##	 REFERENCE IMAGE RECTIFICATION ADDED 3-6-86
C##	 MAGELLAN BACKSCATTER MODEL ADDED 3-6-86
C##	 PARAMETER UPDATES IMPLEMENTED 7-1-86
C##	 ANTENNA MISCORRECTION ADDED 7-1-86
c##	 PORTED TO UNIX 05-SEP-94
C##
C       RADAGEN PERFORMS FOUR FUNCTIONS:
C
C          1) IT SHADES A TERRAIN IMAGE ACCORDING TO THE FORMULA
C             COS(THETA)/(SIN(THETA)+.1*COS(THETA))**3 .
C             WHERE THETA IS THE ANGLE BETWEEN THE ILLUMINATION
C             VECTOR AND THE LOCAL SURFACE NORMAL.
C             ILLUMINATION VECTOR IS THE UNIT VECTOR WHOSE SPHERICAL
C             COORDINATES ARE AZ=90 DEGREES (TOWARDS THE RIGHT IMAGE
C             EDGE), EL = VALUE OF INPUT PARAMETER, R=1 ;
C
C          2) IT SHADOWS A TERRAIN IMAGE BY SETTING THE DN OF ANY
C             PIXEL HIDDEN BY TERRAIN FROM THE RADAR BEAM TO A LOW
C             VALUE(1). A SAMPLE IS HIDDEN IF ITS SLOPE IS LESS THAN
C             TAN(THETA).
C
C          3) IT PERFORMS THE SLANT RANGE SIMULATION;
C
C          4) IT CORRECTS THE SLANT RANGE ACCORDING TO OFFSETS
C             INTRODUCED BY THE LOCAL TERRAIN.
C
C          THE LAST TWO STEPS ARE PERFORMED AT THE SAME TIME AS
C          FOLLOWS:
C
C          OUTPUT_SAMPLE = TOTA - (T' - A) + H*SIN(THETA)
C
C          WHERE,
C
C             TOTA = TOT - A
C             TOT             THE SLANT RANGE TO THE LEFT EDGE OF TERRAIN
C                             IMAGE;
C             A               THE SLANT RANGE TO THE RIGHT EDGE OF THE
C                             TERRAIN IMAGE;
C             T'              THE SLANT RANGE TO THE SAMP IN THE TERRAIN
C                             IMAGE THAT CORRESPONDS TO THE SAMPLE IN THE
C                             OUTPUT RADAR IMAGE.
C             H               THE LOCAL TERRAIN ALTITUDE.
C
C          THESE PARAMS CAN BE CALCULATED FROM THE SATELLITE ALTITUDE,
C          SAMP POSITION IN THE TERR. IMAGE, AND THE NADIR OFFSET FROM
C          THE RIGHT EDGE OF THE TERRAIN IMAGE.
C
C          H*SIN(THETA) IS THE LOCAL TERRAIN CORRECTION, AN APPROXIMATION
C          WHICH IS GOOD WHEN SATELLITE ALT >> LOCAL TERRAIN ALTITUDE.
C
C      PARAMETERS:
C
C         EL,R          SOURCE ELEVATION MEASURED FROM THE IMAGE NORMAL
C                       WHERE VERTICAL IS 0. IN DEGREES.  DEFAULT = 20.
C
C         TOP,R         REAL VALUE INDICATING THE VALUE TO BE
C                       SCALED TO.  DEFAULT = 30.
C
C         OFF,R         THE OFFSET OF THE NADIR MEASURED FROM THE RIGHT
C                       PICTURE EDGE IN REAL PIXELS.  COMPUTED.
C
C         HEIGHT,R      HEIGHT IN KM OF SATELLITE.  DEFAULT = 1100.
C
C         OFMT          THE OUTPUT IMAGE IS TO BE HALF WORD OR 
C 			BYTE (THE DEFAULT).
C
C         SSCALE,R      THE SCALE IN THE SAMPLE DIRECTION. DEFAULTS TO
C                       500. M/PIXEL.
C
C         TOFF,N        SPECIFIES HOW MANY PIXELS EVERY POINT IN
C                       THE OUTPUT RADAR IMAGE WILL BE LEFT-SHIFTED.
C                       A POINT IN THE INPUT TERRAIN IMAGE WITH
C                       ELEVATION >0 WILL BE SHIFTED RIGHT IN THE
C                       OUTPUT RADAR IMAGE AS A CONSEQUENCE OF
C                       THE SIMULATION MODELED BY THE EQUATIONS
C                       ABOVE. IF ALL THE POINTS IN THE INPUT ARE
C                       AT SOME SIZABLE ELEVATION. A SUBSTANTIAL
C                       PORTION OF THE OUTPUT DATA COULD BE RIGHT
C                       SHIFTED BEYOND THE LEFT EDGE OF THE OUTPUT
C                       PICTURE. 'TOFF' COUNTERACTS THIS SYSTEMATIC
C                       SHIFT. IF DEFAULTED, RADAGEN WILL COMPUTE
C                       A VALUE THAT WILL RESULT IN A INPUT POINT
C                       OF ELEVATION EQUAL TO THE AVERAGE ELEVATION
C                       OF THE CENTRAL LINE IN THE INPUT BEING PLACED
C                       JUST AT THE RIGHT EDGE OF THE OUTPUT.
C
C         LSCALE,R      LINE SCALE. DEFAULTS TO 500 M/PIXEL.
C
C         ZSCALE,R      ALTITUDE SCALE. DEFAULTS TO 30 M/DN.
C                      
C
C         SCALE,R       THE DN/COS(THETA) IN THE OUTPUT IMAGE, WHERE
C                       THETA IS THE ANGLE BETWEEN THE SURFACE
C                       NORMAL AND THE ILLUMINATION DIRECTION.
C
C
C
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      EXTERNAL WORK
C
      CALL IFMESSAGE('RADAGEN version 5-SEPT-94')
      CALL PROCESS_PARAMETERS 
C
C     FOR SIR-B REVISION ADDED A SECOND OUTPUT OPTION CALLED TLOCATOR
C     WITH SIZE EQUALLING A HALF WORD PER PIXEL IN THE FIRST OUTPUT
C     FILE.
C
      CALL STACKA(7,WORK,5,3*NS*2,NS*2,NS*2,NS*4,NS*4)
      RETURN
      END
C
C
C
C
C
C
      SUBROUTINE WORK(INPUT,I1,OUTPUT,I2,TLOCATOR,I5,A,I3,B,I4)
C
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE,LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      INTEGER   I1, I2, I3, I4, I5, SL, NL_IN, STAT
      INTEGER*2 INPUT(I1/6,3), OUTPUT(I2/2), TLOCATOR(I5/2)
      REAL      A(I3/4), B(I4/4)
C
      IF (I1.LT.3*NS*2 .OR. I2.LT.NS*2 .OR. I5.LT.NS*2 .OR.
     +	  I3.LT.NS*4 .OR. I4.LT.NS*4) THEN
	  CALL XVMESSAGE('STACKA FAILURE',' ')
	  CALL ABEND
      END IF
      CALL RAD_INITIALIZE(A,B,INPUT,OUTPUT,TLOCATOR)
      IF (UPD) THEN
	  NL_IN = NL
	  NL = LINC
	  IF (LINC.LT.3) NL=3
      END IF
C
      DO SL=1,IPASS
	  ISL = (SL - 1) * LINC
    	  IF (ISL.EQ.0) ISL = 1
	  IF ((UPD).AND.(SL.GT.1)) CALL RAD_UPDATE (A,B,
     +	         INPUT,OUTPUT,TLOCATOR,SL)
	  CALL TABLE_THE_IMAGE(A,B,INPUT,OUTPUT,TLOCATOR)
      END DO
      IF (UPD) CALL XVPCLOSE (STAT)
      RETURN
      END
C
C
C
C
       SUBROUTINE PROCESS_PARAMETERS
C
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      INTEGER	  NLI, NSI
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      CHARACTER*90 LBUF(2)
      INTEGER      ICNT, STATUS, MAXPARMS
      LOGICAL      XVPTST
C
C   OPEN THE INPUT
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
      CALL XVGET(INUNIT,STATUS,'FORMAT',IFMT,' ')
      IF ((IFMT.NE.'BYTE').AND.(IFMT.NE.'HALF')) THEN
          CALL XVMESSAGE('TOPO MUST BE BYTE OR HALFWORD',' ')
          CALL ABEND
      ENDIF
C
      CALL XVSIZE(ISL,ISS,NL,NS,NLI,NSI)
C
C   CHECK FOR AN UPDATE FILE 
C
      CALL XVP('UPDATE',LBUF(1),ICNT)
C
      IF (ICNT.GT.0) THEN
    	 UPD = .TRUE.
C
         CALL XVP('SS_OUT',SS_BUF(2),SS_BUF(1))
      	    IF (SS_BUF(1).GT.MAXPARMS) MAXPARMS = SS_BUF(1)
         CALL XVP('NS_OUT',NS_BUF(2),NS_BUF(1))
      	    IF (NS_BUF(1).GT.MAXPARMS) MAXPARMS = NS_BUF(1)
      	 CALL XVP('AZ',AZ_BUF(2),AZ_BUF(1))
	    AZ = AZ_BUF(2)
      	    IF (AZ_BUF(1).GT.MAXPARMS) MAXPARMS = AZ_BUF(1)
	 CALL XVP('EL',EL_BUF(2),EL_BUF(1))
	    EL = EL_BUF(2)
      	    IF (EL_BUF(1).GT.MAXPARMS) MAXPARMS = EL_BUF(1)
	 CALL XVP('HEIGHT',HEIGHT_BUF(2),HEIGHT_BUF(1))
	    HEIGHT = HEIGHT_BUF(2)
      	    IF (HEIGHT_BUF(1).GT.MAXPARMS) MAXPARMS = HEIGHT_BUF(1)
	 CALL XVP('TOP',TOP_BUF(2),TOP_BUF(1))
	    TOP_VALUE = TOP_BUF(2)
      	    IF (TOP_BUF(1).GT.MAXPARMS) MAXPARMS = TOP_BUF(1)
	 CALL XVP('SSCALE',SSCALE_BUF(2),SSCALE_BUF(1))
	    SSCALE = SSCALE_BUF(2)
      	    IF (SSCALE_BUF(1).GT.MAXPARMS) MAXPARMS = SSCALE_BUF(1)
	 CALL XVP('LSCALE',LSCALE_BUF(2),LSCALE_BUF(1))
	    LSCALE = LSCALE_BUF(2)
      	    IF (LSCALE_BUF(1).GT.MAXPARMS) MAXPARMS = LSCALE_BUF(1)
	 CALL XVP('ZSCALE',ZSCALE_BUF(2),ZSCALE_BUF(1))
	    ZSCALE = ZSCALE_BUF(2)
      	    IF (ZSCALE_BUF(1).GT.MAXPARMS) MAXPARMS = ZSCALE_BUF(1)
	 CALL XVP('SCALE',SCALE_BUF(2),SCALE_BUF(1))
	    SCALE = SCALE_BUF(2)
      	    IF (SCALE_BUF(1).GT.MAXPARMS) MAXPARMS = SCALE_BUF(1)
	 CALL XVP('TOFF',TOFF_BUF(2),TOFF_BUF(1))
	    TERRAIN_OFFSET = TOFF_BUF(2)
      	    IF (TOFF_BUF(1).GT.MAXPARMS) MAXPARMS = TOFF_BUF(1)
C
    	 IPASS = MAXPARMS
      	 LINC = NL / IPASS
         IF (NL.GT.(LINC * IPASS)) THEN
	    LINC = LINC + 1
	    IPASS = NL / LINC
	    IF (NL.GT.(LINC*IPASS)) IPASS = IPASS + 1
	 END IF
C
      ELSE
         UPD = .FALSE.
C
	 CALL XVP('AZ',AZ,ICNT)
	 CALL XVP('EL',EL,ICNT)
	 CALL XVP('HEIGHT',HEIGHT,ICNT)
	 CALL XVP('OFMT',OFMT,ICNT)
	 CALL XVP('TOP',TOP_VALUE,ICNT)
	 CALL XVP('SSCALE',SSCALE,ICNT)
	 CALL XVP('LSCALE',LSCALE,ICNT)
	 CALL XVP('ZSCALE',ZSCALE,ICNT)
	 CALL XVP('SCALE',SCALE,ICNT)
	 CALL XVP('TOFF',TERRAIN_OFFSET,ICNT)
    	 IPASS = 1
      END IF
C
      HEIGHT = HEIGHT * 1000
C
      MGN     = XVPTST('MAGELLAN') .OR.  XVPTST('MGN')
      RECTIFY = XVPTST('RECTIFY')
      FILL    = XVPTST('FILL')
C
      CALL XVP('MISCORR',MIS_CORR(1),ICNT)
      CALL XVP('PERIOD',MIS_CORR(2),ICNT)
      CALL XVP('DEGREE',MIS_CORR(3),ICNT)
      IF ((MIS_CORR(1).EQ.0).OR.(MIS_CORR(2).LE.0)) THEN
    	  MIS_CORR(1) = 0
    	  MIS_CORR(2) = 1
      END IF
C
C   LOG DOMAIN OUTPUT?
C
      IF (XVPTST('LOG')) THEN
    	  LOG = .TRUE.
    	  CALL XVP('LOG_SC',LOG_SCALE,ICNT)
      ELSE
    	  LOG = .FALSE.
      END IF
C
C   OPEN THE OUTPUT 
C
      CALL XVUNIT(OUTUNIT(1),'OUT',1,STATUS,' ')
      IF (OFMT.EQ.'BYTE') THEN
         CALL XVOPEN(OUTUNIT(1),STATUS,'OP','WRITE','U_FORMAT',
     +               'HALF','O_FORMAT','BYTE','U_NL',NL,
     +               'U_NS',NS,'OPEN_ACT','SA','IO_ACT','SA',' ')
      ELSE
         CALL XVOPEN(OUTUNIT(1),STATUS,'OP','WRITE','U_FORMAT',
     +               'HALF','O_FORMAT','HALF','U_NL',NL,
     +               'U_NS',NS,'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDIF
C
C   OPEN THE TLOCATOR OUTPUT IF ONE IS SPECIFIED
C
      CALL XVP('OUT',LBUF(1),N_OUTPUTS)
      IF (N_OUTPUTS.EQ.2) THEN
         CALL XVUNIT(OUTUNIT(2),'OUT',2,STATUS,' ')
         CALL XVOPEN(OUTUNIT(2),STATUS,'U_FORMAT','HALF',
     +               'O_FORMAT','HALF','OP','WRITE','U_NL',NL,
     +               'U_NS',NS,'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDIF
C
C   OPEN THE REFERENCE FILE IF ONE IS SPECIFIED
C
      CALL XVP('REF',LBUF(1),ICNT)
      IF (ICNT.GT.0) THEN
         CALL XVUNIT(REFUNIT,'REF',1,STATUS,' ')
         CALL XVOPEN(REFUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +		     'U_FORMAT','HALF',' ')
         REF=.TRUE.
      ELSE
      	 REF=.FALSE.
      ENDIF
C
      RETURN 
      END
C
C
C
C
C
      SUBROUTINE RAD_INITIALIZE(A,B,INPUT,OUTPUT,TLOCATOR)
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      REAL      A(NS), B(NS)
      INTEGER*2 INPUT(NS,3), OUTPUT(NS), TLOCATOR(NS)
C
      REAL      PI/3.1415927/
      REAL      DEG_PER_RAD/57.29578/
      REAL      AUGMENTED_HEIGHT, R
      INTEGER*2 I
      CHARACTER*7200   WORK
C
      IF (NL.LT.3)
     +   CALL XVMESSAGE(
     +    ' RADAGEN INPUT MUST CONTAIN AT LEAST 3 LINES.',' ')
      TAN_THETA = TAN((90.-EL)/DEG_PER_RAD)*(SSCALE/ZSCALE) 
      NEXT_SAMP = NS - 1 
C
      REL = (EL/360.)*2*PI         ! CONVERT TO RADIANS. 
      RAZ = (AZ/360.)*2*PI 
      CALL RADIUS(ILLUM_VECTOR,REL,RAZ,1.0) 
C
C                                       ! ILLUM_VEC IS THE UNIT VECTOR
C                                       ! IN THE DIRECTION OF
C                                       ! ILLUMINATION. 
C
      NADIR_OFFSET = HEIGHT / (ILLUM_VECTOR(3)/ILLUM_VECTOR(1))
     *                 - (NS/2)*SSCALE
      AUGMENTED_HEIGHT = SQRT(NADIR_OFFSET**2. + HEIGHT**2.) 
C
      TOTAL_PIXELS_OUT=(SQRT((NADIR_OFFSET+NS*SSCALE)**2. +
     +    HEIGHT**2.)-AUGMENTED_HEIGHT)/SSCALE 
      IF (SCALE.EQ.0.) THEN
    	IF (LOG) THEN
          IF (OFMT.EQ.'HALF') THEN 
              SCALE = 16383./ALOG10(TOP_VALUE) 
          ELSE
              SCALE = 254./ALOG10(TOP_VALUE)
          END IF
    	ELSE
          IF (OFMT.EQ.'HALF') THEN 
              SCALE = 16383./TOP_VALUE 
          ELSE
              SCALE = 254./TOP_VALUE 
          END IF
    	END IF
      END IF
C
      MAXIM=16383
      IF (OFMT.EQ.'BYTE') MAXIM=254
C
      OUT_SCALE = FLOAT(NS)/FLOAT(TOTAL_PIXELS_OUT) 
C
      CALL XVMESSAGE(' ',' ')
      WORK = ' OUTPUT RESOLUTION IN METERS/PIXEL '
      WRITE(WORK(43:50),'(F8.2)') SSCALE/OUT_SCALE
      CALL XVMESSAGE(WORK,' ')
      DO I = 1,NS
          R = SQRT(((NS-I)*SSCALE+NADIR_OFFSET)**2 +
     +        HEIGHT**2) 
          A(I) = TOTAL_PIXELS_OUT*SSCALE - (R-AUGMENTED_HEIGHT) 
          B(I) =(HEIGHT/R) 
      END DO
      RETURN 
      END
C
C
C
C
C
      SUBROUTINE RAD_UPDATE(A,B,INPUT,OUTPUT,TLOCATOR,PASS)
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      REAL      A(NS), B(NS)
      INTEGER*2 INPUT(NS,3), OUTPUT(NS), TLOCATOR(NS)
C
      REAL      PI/3.1415927/
      REAL      DEG_PER_RAD/57.29578/
      REAL      AUGMENTED_HEIGHT, R
      INTEGER*2 I
      CHARACTER*7200   WORK
      INTEGER	PASS
C
C  UPDATE THE PARAMETERS PER PASS
C
      IF (SS_BUF(1).GE.PASS) ISS = SS_BUF(PASS)
      IF (SS_BUF(1).LT.PASS) ISS = SS_BUF(SS_BUF(1))
C
      IF (NS_BUF(1).GE.PASS) NS = NS_BUF(PASS)
      IF (NS_BUF(1).LT.PASS) NS = NS_BUF(NS_BUF(1))
C
      IF (AZ_BUF(1).GE.PASS) AZ = AZ_BUF(PASS)
      IF (AZ_BUF(1).LT.PASS) AZ = AZ_BUF(AZ_BUF(1))
C
      IF (EL_BUF(1).GE.PASS) EL = EL_BUF(PASS)
      IF (EL_BUF(1).LT.PASS) EL = EL_BUF(EL_BUF(1))
C
      IF (HEIGHT_BUF(1).GE.PASS) HEIGHT = 1000 * HEIGHT_BUF(PASS)
      IF (HEIGHT_BUF(1).LT.PASS)HEIGHT=1000*HEIGHT_BUF(HEIGHT_BUF(1))
C
      IF (TOP_BUF(1).GE.PASS) TOP_VALUE = TOP_BUF(PASS)
      IF (TOP_BUF(1).LT.PASS) TOP_VALUE = TOP_BUF(TOP_BUF(1))
C
      IF (SSCALE_BUF(1).GE.PASS) SSCALE = SSCALE_BUF(PASS)
      IF (SSCALE_BUF(1).LT.PASS) SSCALE = SSCALE_BUF(SSCALE_BUF(1))
C
      IF (LSCALE_BUF(1).GE.PASS) LSCALE = LSCALE_BUF(PASS)
      IF (LSCALE_BUF(1).LT.PASS) LSCALE = LSCALE_BUF(LSCALE_BUF(1))
C
      IF (ZSCALE_BUF(1).GE.PASS) ZSCALE = ZSCALE_BUF(PASS)
      IF (ZSCALE_BUF(1).LT.PASS) ZSCALE = ZSCALE_BUF(ZSCALE_BUF(1))
C
      IF (SCALE_BUF(1).GE.PASS) SCALE = SCALE_BUF(PASS)
      IF (SCALE_BUF(1).LT.PASS) SCALE = SCALE_BUF(SCALE_BUF(1))
C
      IF (TOFF_BUF(1).GE.PASS) TERRAIN_OFFSET = TOFF_BUF(PASS)
      IF (TOFF_BUF(1).LT.PASS) TERRAIN_OFFSET = TOFF_BUF(TOFF_BUF(1))
C
      IF (NL.LT.3)
     +   CALL XVMESSAGE(
     +    ' RADAGEN INPUT MUST CONTAIN AT LEAST 3 LINES.',' ')
      TAN_THETA = TAN((90.-EL)/DEG_PER_RAD)*(SSCALE/ZSCALE) 
      NEXT_SAMP = NS - 1 
C
      REL = (EL/360.)*2*PI         ! CONVERT TO RADIANS. 
      RAZ = (AZ/360.)*2*PI 
      CALL RADIUS(ILLUM_VECTOR,REL,RAZ,1.0) 
C
C                                       ! ILLUM_VEC IS THE UNIT VECTOR
C                                       ! IN THE DIRECTION OF
C                                       ! ILLUMINATION. 
C
      NADIR_OFFSET = HEIGHT / (ILLUM_VECTOR(3)/ILLUM_VECTOR(1))
     *                 - (NS/2)*SSCALE
      AUGMENTED_HEIGHT = SQRT(NADIR_OFFSET**2. + HEIGHT**2.) 
C
      TOTAL_PIXELS_OUT=(SQRT((NADIR_OFFSET+NS*SSCALE)**2. +
     +    HEIGHT**2.)-AUGMENTED_HEIGHT)/SSCALE 
      IF (SCALE.EQ.0.) THEN
    	IF (LOG) THEN
          IF (OFMT.EQ.'HALF') THEN 
              SCALE = 16383./ALOG10(TOP_VALUE) 
          ELSE
              SCALE = 254./ALOG10(TOP_VALUE)
          END IF
    	ELSE
          IF (OFMT.EQ.'HALF') THEN 
              SCALE = 16383./TOP_VALUE 
          ELSE
              SCALE = 254./TOP_VALUE 
          END IF
    	END IF
      END IF
C
      MAXIM=16383
      IF (OFMT.EQ.'BYTE') MAXIM=254
C
      OUT_SCALE = FLOAT(NS)/FLOAT(TOTAL_PIXELS_OUT) 
C
      CALL XVMESSAGE(' ',' ')
      WORK = ' OUTPUT RESOLUTION IN METERS/PIXEL '
      WRITE(WORK(43:50),'(F8.2)') SSCALE/OUT_SCALE
      CALL XVMESSAGE(WORK,' ')
      DO I = 1,NS
          R = SQRT(((NS-I)*SSCALE+NADIR_OFFSET)**2 +
     +        HEIGHT**2) 
          A(I) = TOTAL_PIXELS_OUT*SSCALE - (R-AUGMENTED_HEIGHT) 
          B(I) =(HEIGHT/R) 
      END DO
      RETURN 
      END
C
C
C
C
C
      SUBROUTINE TABLE_THE_IMAGE(A,B,INPUT,OUTPUT,TLOCATOR)
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      COMMON /MOVE/ OFF1, OFF2
      INTEGER	  OFF1, OFF2
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      REAL      A(NS), B(NS)
      INTEGER*2 INPUT(NS,3), OUTPUT(NS), TLOCATOR(NS)
C
      REAL      S(3), L(3), Z(3), Y(3), R3, R1, R2, TERRAIN_AVG
      INTEGER*2 H/0/, SHADOW_HOLD(2)
      INTEGER*2 SHADOW_COUNT, SHADOW_LIMIT/2/
      INTEGER*2 SSS/1/, LL/2/, ALT/3/, TEMP
      INTEGER   I, II, LINE, SAMP, TOP, MIDDLE, BOTTOM, STATUS
C      LOGICAL        IN_RANGE
      CHARACTER*80   STRNG
C
C     FOR SIR-B REVISION TLOCATOR IS FORWARDED TO THE MOVE_OUTPUT ROUTINE
C     WHEREVER IT IS CALLED.  AND EVERYWHERE WRITE IS USED TO WRITE AN
C     OUTPUT LINE TO THE FIRST OUTPUT, A BLOCK OF CODE HAS BEEN ADDED TO
C     WRITE TLOCATOR LINE TO THE SECOND OUTPUT IF A SECOND OUTPUT HAS BEEN
C     SPECIFIED.  FINALLY THE SECOND OUTPUT FILE IS CLOSED BY THIS ROUTINE
C     IF IT HAS BEEN SPECIFIED.
C
      IF (TERRAIN_OFFSET.EQ.-1) THEN  ! NO PARAMETER ENTERED. 
          SAMP = ISL + NL/2 
          CALL XVREAD(INUNIT,INPUT(1,1),STATUS,'SAMP',ISS,
     +                'NSAMPS',NS,'LINE',SAMP,' ')
          TERRAIN_AVG = 0. 
          DO SAMP = 1,NS 
              TERRAIN_AVG = INPUT(SAMP,1)+TERRAIN_AVG 
          END DO
          TERRAIN_AVG = (TERRAIN_AVG/FLOAT(NS))*ZSCALE 
          TERRAIN_OFFSET=((TERRAIN_AVG*B(1))/SSCALE)*OUT_SCALE
      END IF
      STRNG=' AVG HEIGHT USED:             LEFT BORDER OFFSET:'
      WRITE(STRNG(19:25),'(F7.2)') TERRAIN_AVG
      WRITE(STRNG(51:54),'(I4)') TERRAIN_OFFSET
      CALL XVMESSAGE(STRNG,' ')
C
      DO II=1,NS
          OUTPUT(II) = 0 
      END DO
C
C WATCH OUT FOR READING INTO (I,J) ARRAYS
C
      CALL XVREAD(INUNIT,INPUT(1,1),STATUS,'SAMP',ISS,
     +           'NSAMPS',NS,'LINE',ISL,' ')
      CALL XVREAD(INUNIT,INPUT(1,2),STATUS,'SAMP',ISS,
     +           'NSAMPS',NS,' ')
      CALL XVREAD(INUNIT,INPUT(1,3),STATUS,'SAMP',ISS,
     +           'NSAMPS',NS,' ')
C
C                                       /* UPPER LEFT CORNER. */
C
      S(ALT) = (INPUT(2,1)-INPUT(1,1))*ZSCALE 
      S(LL) = 0 
      S(SSS) = SSCALE*2. 
      L(SSS) = 0 
      L(ALT) = (INPUT(1,2)-INPUT(1,1))*ZSCALE 
      L(LL) = LSCALE*2 
      CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +			1,1,MIS_CORR)
      CALL MOVE_OUTPUT(1,1,A,B,INPUT,OUTPUT,TLOCATOR,R3,1)
      NEXT_SAMP = NS - 1 
      DO SAMP = NS-1,2,-1    ! UPPER LINE.
          IF (INPUT(SAMP,1).GE.TAN_THETA*(SAMP-NEXT_SAMP)) THEN
              S(ALT) = (INPUT(SAMP+1,1)-INPUT(SAMP-1,1))*ZSCALE 
              L(ALT) = (INPUT(SAMP,2)-INPUT(SAMP,1))*ZSCALE 
              CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				1,SAMP,MIS_CORR,R3)
              CALL MOVE_OUTPUT(1,SAMP,A,B,INPUT,OUTPUT,TLOCATOR,R3,1)
              NEXT_SAMP = SAMP - INPUT(SAMP,1)*ZSCALE/TAN_THETA 
          ELSE 
              OUTPUT(SAMP) = 1 
          END IF
          NEXT_SAMP = SAMP - INPUT(SAMP,1)/TAN_THETA 
      END DO
C
C                                       /* UPPER RIGHT CORNER. */
C
      S(ALT) = (INPUT(NS,1)-INPUT(NS-1,1))*ZSCALE 
      L(ALT) = (INPUT(NS,2)-INPUT(NS,1))*ZSCALE 
      CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				1,NS,MIS_CORR)
      CALL MOVE_OUTPUT(1,NS,A,B,INPUT,OUTPUT,TLOCATOR,R3,1)
      IF (RECTIFY) CALL RECTIFY_IMAGE(OUTPUT,NS,TLOCATOR)
      CALL RESAMP0(OUTPUT,NS,TLOCATOR,FILL) ! AVG. OVER ZEROES. 
      CALL XVWRIT(OUTUNIT(1),OUTPUT(1),STATUS,'NSAMPS',NS,' ')
      IF (N_OUTPUTS.GT.1) THEN
        CALL XVWRIT(OUTUNIT(2),TLOCATOR(1),STATUS,'NSAMPS',NS,' ')
      ENDIF
      TOP = 1
      BOTTOM = 3
      MIDDLE = 2
C
      LINE = ISL+1
      DO WHILE (LINE.LE.NL+ISL-2)
	DO II=1,NS
            OUTPUT(II) = 0 
	END DO
    	II = LINE - ISL
        SHADOW_COUNT = 0 
        NEXT_SAMP = NS - 1 
        S(ALT) = (INPUT(2,MIDDLE)-INPUT(1,MIDDLE))*ZSCALE 
        L(ALT) = (INPUT(2,BOTTOM)-INPUT(2,TOP))*ZSCALE 
        CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				II,SAMP,MIS_CORR)
        CALL MOVE_OUTPUT(MIDDLE,SAMP,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
        DO SAMP = NS-1,2,-1 
          S(ALT)=(INPUT(SAMP+1,MIDDLE)-INPUT(SAMP-1,MIDDLE))
     +       * ZSCALE 
          L(ALT)=(INPUT(SAMP,BOTTOM)-INPUT(SAMP,TOP))*ZSCALE 
          CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				II,SAMP,MIS_CORR)
          CALL MOVE_OUTPUT(MIDDLE,SAMP,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
          IF (INPUT(SAMP,MIDDLE).LT.TAN_THETA*(SAMP-NEXT_SAMP)) THEN
             SHADOW_COUNT = SHADOW_COUNT + 1
             IF (SHADOW_COUNT.LT.SHADOW_LIMIT) 
     +           SHADOW_HOLD(SHADOW_COUNT) = OUT_SAMP 
             IF (SHADOW_COUNT.EQ.SHADOW_LIMIT) THEN
                DO I = 1,SHADOW_LIMIT-1
                   OUTPUT(SHADOW_HOLD(I)) = 1 
                END DO
                OUTPUT(OUT_SAMP) = 1 
             END IF
             IF (SHADOW_COUNT.GT.SHADOW_LIMIT)
     +          OUTPUT(OUT_SAMP) = 1 
             ELSE 
                SHADOW_COUNT = 0 
                NEXT_SAMP = SAMP - INPUT(SAMP,MIDDLE)/TAN_THETA 
             END IF
          END DO
          S(ALT) = (INPUT(NS,MIDDLE)-INPUT(NS-1,MIDDLE))*ZSCALE 
          L(ALT)=(INPUT(NS,BOTTOM)-INPUT(NS,TOP))*ZSCALE
          CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +					II,NS,MIS_CORR)
          CALL MOVE_OUTPUT(MIDDLE,NS,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
          IF (RECTIFY) CALL RECTIFY_IMAGE(OUTPUT,NS,TLOCATOR)
          CALL RESAMP0(OUTPUT,NS,TLOCATOR,FILL) ! AVG. OVER ZEROES. 
          CALL XVWRIT(OUTUNIT(1),OUTPUT,STATUS,'NSAMPS',NS,' ')
          IF (N_OUTPUTS.GT.1) THEN
           CALL XVWRIT(OUTUNIT(2),TLOCATOR(1),STATUS,'NSAMPS',NS,' ')
          ENDIF
          IF (LINE.NE.NL+ISL-2) THEN
             CALL XVREAD(INUNIT,INPUT(1,TOP),STATUS,'SAMP',ISS,
     +                   'NSAMPS',NS,' ')
              TEMP = TOP 
              TOP = MIDDLE 
              MIDDLE = BOTTOM 
              BOTTOM = TEMP 
          END IF
          LINE=LINE+1
      END DO
C
C                                       /* LAST LINE. */
C
      DO II=1,NS
          OUTPUT(II) = 0 
      END DO
      II = ISL + NL - 1
      S(ALT) = (INPUT(2,BOTTOM)-INPUT(1,BOTTOM))*ZSCALE 
      L(ALT) = (INPUT(1,BOTTOM)- INPUT(1,MIDDLE)) * ZSCALE 
C
      CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				II,1,MIS_CORR)
      CALL MOVE_OUTPUT(BOTTOM,1,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
      NEXT_SAMP = NS - 1 
      DO SAMP = NS-1,2,-1 
C                                       /* MID LINE. */
        IF (INPUT(SAMP,BOTTOM).GE.TAN_THETA*(SAMP-NEXT_SAMP)) THEN
          S(ALT) = (INPUT(SAMP+1,BOTTOM)-INPUT(SAMP-1,BOTTOM))
     +        * ZSCALE 
          L(ALT) = (INPUT(SAMP,BOTTOM)-INPUT(SAMP,MIDDLE))*ZSCALE 
          CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +					II,SAMP,MIS_CORR)
          CALL MOVE_OUTPUT(BOTTOM,SAMP,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
          NEXT_SAMP = SAMP - INPUT(SAMP,BOTTOM)*ZSCALE/TAN_THETA 
        ELSE
          OUTPUT(SAMP) = 1 
        END IF
      END DO
C                                       /* RIGHT CORNER. */
C
      S(ALT) = (INPUT(NS,BOTTOM)-INPUT(NS,BOTTOM))
     +    * ZSCALE 
      L(ALT)=(INPUT(NS,BOTTOM)-INPUT(NS,MIDDLE))*ZSCALE
      CALL COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +				II,NS,MIS_CORR)
      CALL MOVE_OUTPUT(BOTTOM,NS,A,B,INPUT,OUTPUT,TLOCATOR,R3,II)
      IF (RECTIFY) CALL RECTIFY_IMAGE(OUTPUT,NS,TLOCATOR)
      CALL RESAMP0(OUTPUT,NS,TLOCATOR,FILL) ! AVG. OVER ZEROES. 
      CALL XVWRIT(OUTUNIT(1),OUTPUT,STATUS,'NSAMPS',NS,' ')
      IF (N_OUTPUTS.GT.1) THEN
        CALL XVWRIT(OUTUNIT(2),TLOCATOR(1),STATUS,'NSAMPS',NS,' ')
      ENDIF
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT(1),STATUS,' ')
      IF (N_OUTPUTS.GT.1) CALL XVCLOSE(OUTUNIT(2),STATUS,' ')
      RETURN
      END
C
C
C
C
      SUBROUTINE CROSS(CROS,WITH,INTO)
      REAL CROS(3),WITH(3),INTO(3)
      INTO(1) = CROS(2)*WITH(3)-CROS(3)*WITH(2)
      INTO(2) = CROS(3)*WITH(1)-CROS(1)*WITH(3)
      INTO(3) = CROS(1)*WITH(2)-CROS(2)*WITH(1)
      RETURN
      END
C
C
C
C
      SUBROUTINE RADIUS(IS_CARTESIAN,EL,AZ,RAD)
      REAL IS_CARTESIAN(3),EL,AZ,RAD
      IS_CARTESIAN(1) = SIN(EL)*COS(AZ)*RAD
      IS_CARTESIAN(2) = SIN(EL)*SIN(AZ)*RAD
      IS_CARTESIAN(3) = COS(EL)*RAD
      RETURN
      END
C
C
C
C
C
      SUBROUTINE COMPUTE_FUNCTION(L,S,Z,ILLUM_VECTOR,Y,R1,R2,R3,SCALE,
     +			LINE,SAMP,MIS_CORR)
C
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
      REAL L(3), S(3), Z(3), ILLUM_VECTOR(3), Y(3), R1, R2, R3, SCALE
      INTEGER  LINE, SAMP, REFUNIT
      REAL DOTPROD, MIS_CORR(3)
      CALL CROSS(S,L,Z) 
      CALL CROSS(Z,ILLUM_VECTOR,Y)
      R3 = 0.
      R1 = DOTPROD(Z,ILLUM_VECTOR)
      IF (R1.GE.0.) THEN 
	 R2 = SQRT(Z(1)**2.+Z(2)**2.+Z(3)**2.)
	 R3 = SQRT(Y(1)**2.+Y(2)**2.+Y(3)**2.)
	 R1 = R1/R2     ! COS(THETA)
	 R3 = R3/R2     ! SIN(THETA)
         IF (.NOT.MGN) THEN
 	    R3 = R1/(R3+.1*R1)**3.0
   	 ELSE
C           SPECIAL BACKSCATTER MODELING FOR MAGELLAN PROJECT
   	    R3 = (0.0188*R1)/(R3+0.111*R1)**3.0
C	    COMPENSATION IN SCALE FOR MAGELLAN BACKSCATTER
    	    R3 = R3 * 40
   	 END IF
C
         CALL MISCORRECT (LINE, SAMP, MIS_CORR, R3)
    	 IF (LOG) THEN
    	    R3 = MAX(R3, 1.)
    	 ELSE 
 	    R3 = MAX(SCALE*R3, 1.)
    	 END IF
      END IF
      RETURN
      END
C
C
C
C
C
      FUNCTION DOTPROD(DOT,WITH)
      REAL DOT(3),WITH(3)
      DOTPROD = DOT(1)*WITH(1)+DOT(2)*WITH(2)+DOT(3)*WITH(3)
      RETURN
      END
C
C
C
C
      SUBROUTINE MOVE_OUTPUT(FROM_LINE,FROM_SAMP,A,B,INPUT,OUTPUT,
     +                       TLOCATOR,R3,LINE)
C
C*******************************************************************
C
      IMPLICIT NONE
C
      COMMON /GLOBAL/ AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE,
     +       ILLUM_VECTOR, TAN_THETA, REL, TOP_VALUE, MAXIM,
     +       HEIGHT, OUT_SCALE, TERRAIN_OFFSET, NADIR_OFFSET,
     +       LOG_SCALE, ALTI, RAZ, MIS_CORR, LINC, IPASS,
     +	     SS_BUF,NS_BUF,AZ_BUF,EL_BUF,HEIGHT_BUF,TOP_BUF,
     +       SSCALE_BUF,LSCALE_BUF,ZSCALE_BUF,SCALE_BUF,TOFF_BUF,
     +       OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      COMMON /PARMS/ ISL, ISS, NL, NS, N_OUTPUTS, INUNIT, OUTUNIT, 
     +       REFUNIT, REF, UPD, MGN, RECTIFY, LOG, FILL, IFMT, OFMT
      COMMON /MOVE/ OFF1, OFF2
      INTEGER	  OFF1, OFF2
      REAL        REL, ILLUM_VECTOR(3), TAN_THETA, NADIR_OFFSET
      REAL        ALTI, RAZ, TOP_VALUE, HEIGHT, OUT_SCALE, LOG_SCALE
      REAL        AZ, EL, SCALE, SSCALE, LSCALE, ZSCALE, MIS_CORR(3)
      REAL	  AZ_BUF(512),EL_BUF(512),SCALE_BUF(512)
      REAL	  SSCALE_BUF(512),LSCALE_BUF(512),ZSCALE_BUF(512)
      REAL	  HEIGHT_BUF(512),TOP_BUF(512)
      INTEGER     SS_BUF(512),NS_BUF(512),TOFF_BUF(512)
      INTEGER*2   OUT_SAMP, TOTAL_PIXELS_OUT, NEXT_SAMP
      INTEGER     MAXIM, TERRAIN_OFFSET, REFUNIT, LINC, IPASS
      INTEGER     ISL, ISS, NL, NS, INUNIT, OUTUNIT(2), N_OUTPUTS
      LOGICAL	  REF, UPD, MGN, RECTIFY, LOG, FILL
      CHARACTER*5 IFMT, OFMT
C********************************************************************
C
C
C     FOR SIR-B REVISION CODE HAS BEEN ADDED TO ASSOCIATE THE OUTPUT
C     PIXEL(S) WITH THE FROM_SAMP PIXEL IN TLOCATOR LINE.
C
C     FOR MAGELLAN CODE HAS BEEN ADDED TO ALLOW THE TLOCATOR VALUES
C     TO BE BASED ON AN IMAGE THAT IS NOT THE IMAGE BEING RADAGENed.
C
C     IF RECTIFICATION IS TO BE PERFORMED OR A TLOCATOR FILE IS BEING
C     WRITTEN OUT, THE LEFT SIDE OF THE IMAGE IS ALIGNED WITH SAMPLE 
C     1 TO INCLUDE THE ENTIRE IMAGE.
C
      REAL      A(NS), B(NS), R3
      INTEGER*2 LOCBUF(16384),INPUT(NS,3), OUTPUT(NS), TLOCATOR(NS)
      INTEGER   FROM_LINE, FROM_SAMP, LINE, STAT
      INTEGER   L, C
C
      L = FROM_LINE 
      C = FROM_SAMP 
      ALTI = INPUT(C,L)
      OUT_SAMP=-TERRAIN_OFFSET+OUT_SCALE*(A(C)+ALTI*ZSCALE*B(C))/SSCALE
      IF (C.EQ.1) OFF1 = 1 - OUT_SAMP
      IF (RECTIFY.OR.N_OUTPUTS.GT.1) OUT_SAMP = OUT_SAMP + OFF1
      IF (OUT_SAMP.GT.NS) OUT_SAMP=NS
      IF (OUT_SAMP.LT.1) OUT_SAMP=1
C
C   PRESERVE LOG RELATIONSHIP IF IN USE
C
      IF (LOG) THEN
    	  OUTPUT(OUT_SAMP) = SCALE * ALOG10(R3 +
     +      				10.**(OUTPUT(OUT_SAMP)/SCALE))
      ELSE
          OUTPUT(OUT_SAMP) = MIN(NINT(R3),MAXIM) + OUTPUT(OUT_SAMP)
      END IF
C
      IF (OUTPUT(OUT_SAMP).GT.MAXIM) OUTPUT(OUT_SAMP) = MAXIM
C
C   COMPUTE LOCATION SHIFT BASED ON REFERENCE IMAGE
C
      IF (REF) THEN
	 CALL XVREAD(REFUNIT,LOCBUF,STAT,'LINE',LINE,' ')
	 OUT_SAMP=-TERRAIN_OFFSET+OUT_SCALE*(A(C)+LOCBUF(C)*ZSCALE
     +            *B(C))/SSCALE
	 IF (C.EQ.1) OFF2 = 1 - OUT_SAMP
	 IF (RECTIFY.OR.N_OUTPUTS.GT.1) OUT_SAMP = OUT_SAMP + OFF2
	 IF (OUT_SAMP.LT.1) OUT_SAMP=1
	 IF (OUT_SAMP.GT.NS) OUT_SAMP = NS
      END IF
C
      IF (RECTIFY.OR.N_OUTPUTS.GT.1) TLOCATOR(C) = OUT_SAMP
      RETURN
      END
C
C
C
C
      SUBROUTINE MISCORRECT(LINE,SAMP,MIS_CORR,VALUE)
C
      IMPLICIT NONE
      INTEGER  	 LINE, SAMP
      REAL	 MIS_CORR(3), VALUE, VAL, PI/3.1415926536/
C
      VAL = 1. + MIS_CORR(3)/2 * (SIN(2.*PI*LINE/MIS_CORR(2)) - 1)
      VALUE = VALUE + SAMP * MIS_CORR(1) * VAL
C
      RETURN
      END
C
C
C
C
      SUBROUTINE RESAMP0(BUF,N,TBUF,FILL)
C
      IMPLICIT INTEGER(A-Z)
C
      INTEGER*2 BUF(N),TBUF(N)
      LOGICAL    FILL
C
      RCNT = N
      IF (BUF(RCNT).NE.0) GOTO 20
10    IF (BUF(RCNT).NE.0) GOTO 15
      RCNT = RCNT - 1
      IF (RCNT.NE.0) GOTO 10
      RETURN
15    IF (.NOT.FILL) GOTO 20
      RSTART = N
      REND = RCNT
      BUF(RSTART) = BUF(REND)
      TBUF(RSTART) = TBUF(REND)
      GO TO 50
20    IF (BUF(RCNT).EQ.0) GOTO 30
      RCNT = RCNT - 1
      IF (RCNT.GT.0) GOTO 20
      RETURN
30    RSTART = RCNT + 1
40    IF (BUF(RCNT).NE.0) GOTO 50
      RCNT = RCNT - 1
      REND = RCNT
      IF (REND.GT.0) GOTO 40
      REND=1	! NOT IN ORIG BAL
      BUF(REND) = BUF(RSTART)
      TBUF(REND) = TBUF(RSTART)
50    RVAL1 = BUF(RSTART)
      RVAL2 = BUF(REND)
      RDIFF = RSTART-REND
      RDVAL = RVAL1-RVAL2
      R = RSTART - 1
60    BUF(R) = (R-REND)*RDVAL/RDIFF+RVAL2
      IF ((RSTART-R).LE.(RDIFF/2)) THEN
           TBUF(R) = TBUF(RSTART)
      ELSE IF (REND.GT.1) THEN
           TBUF(R) = TBUF(REND)
      ELSE IF (BUF(1).NE.0) THEN
           TBUF(R) = TBUF(REND)
      ELSE 
           TBUF(R) = TBUF(RSTART)
      ENDIF
      R = R - 1
      IF (R.GT.REND) GOTO 60
      IF (REND.EQ.1) RETURN
      GOTO 20
      END
C
C
C
C
C
      SUBROUTINE RECTIFY_IMAGE(OUTPUT,NS,TLOCATOR)
C
C     THIS SUBROUTINE WILL MOVE THE COMPUTED INTENSITIES TO THEIR
C     ORIGINAL LOCATION AS DEFINED IN TLOCATOR TO GEOMETRICALLY 
C     RECTIFY THE IMAGE.
C
      IMPLICIT NONE
      INTEGER NS,SAMP
      INTEGER*2 TLOCATOR(NS),OUTPUT(NS),OUT(32768)
C
      DO SAMP = 1,NS
	  OUT(SAMP) = 0
	  IF (TLOCATOR(SAMP).GT.0) OUT(SAMP)=OUTPUT(TLOCATOR(SAMP))
      ENDDO
C
      DO SAMP = 1,NS
	  OUTPUT(SAMP) = OUT(SAMP)
      ENDDO
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create radagen.imake
#define  PROGRAM   radagen

#define MODULE_LIST radagen.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create radagen.pdf
process help=*
PARM INP 	TYPE=STRING
PARM OUT 	TYPE=STRING COUNT=1:2
PARM REF 	TYPE=STRING COUNT=0:1 DEFAULT=--
PARM UPDATE	TYPE=STRING COUNT=0:1 DEFAULT=--
PARM-PAGE RECTIFY TYPE=KEYWORD COUNT=0:1 VALID=(NORECTIFY,RECTIFY) DEFAULT=--
PARM PROJECT	TYPE=KEYWORD COUNT=0:1 VALID=(MAGELLAN,MGN)	 DEFAULT=--
PARM DOMAIN	TYPE=KEYWORD VALID=(LINEAR,LOG)	DEFAULT=LINEAR
PARM FILL	TYPE=KEYWORD VALID=(FILL,NOFILL) DEFAULT=NOFILL
PARM-PAGE SIZE 	TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL 	TYPE=INTEGER DEFAULT=1
PARM SS 	TYPE=INTEGER DEFAULT=1
PARM NL 	TYPE=INTEGER DEFAULT=0
PARM NS 	TYPE=INTEGER DEFAULT=0
PARM-PAGE EL 	TYPE=REAL VALID=(0.00001:90.0) DEFAULT=20.
PARM HEIGHT 	TYPE=REAL DEFAULT=1100.
PARM-PAGE SSCALE 	TYPE=REAL DEFAULT=500.
PARM LSCALE 	TYPE=REAL DEFAULT=500.
PARM ZSCALE 	TYPE=REAL DEFAULT=30.
PARM-PAGE MISCORR	TYPE=REAL DEFAULT=0.
PARM PERIOD	TYPE=REAL DEFAULT=50.0
PARM DEGREE     TYPE=REAL DEFAULT=0.25 VALID=(0.0:1.0)
PARM-PAGE TOP 	TYPE=REAL DEFAULT=30.
PARM SCALE 	TYPE=REAL DEFAULT=0.
PARM TOFF 	TYPE=INTEGER DEFAULT=-1
PARM LOG_SC	TYPE=REAL DEFAULT=0.2
PARM OFMT 	TYPE=KEYWORD VALID=(HALF,BYTE) DEFAULT=BYTE
END-PROC
.TITLE
VICAR Program RADAGEN
.HELP
PURPOSE:
RADAGEN transforms a terrain image, in which DN corresponds to altitude, to a
radar image, that is, an image which appears as if imaged by a radar device
over the area represented by the terrain image.

It 1) shades the terrain,
   2) shadows areas hidden from the radar beam,
   3) adds slant range effect,
   4) corrects the slant range by offsets caused by the local terrain.

EXECUTION:

Example

	RADAGEN INP=TERRAIN OUT=RADAR HEIGHT=229. SSCALE=98.4252 +
	    LSCALE=98.4252 TOFF=0 ZSCALE=3.280840 EL=15.

	This is a typical call to RADAGEN.  HEIGHT is the altitude of 
        the satellite in km; the default is 1100.  SSCALE and LSCALE 
        are the scales in meters/pixel of the terrain image in the 
        sample and line directions, respectively; the default
        for each is 500 meters/pixel.  TOFF specifies the terrain offset;
        if unspecified, RADAGEN will calculate this for you.  ZSCALE
	specifies the altitude scale in the terrain image in meters/dn;
	the default is 30.  EL is the elevation of the radar source
	measured from the vertical perpendicular to the image plane;
	that is, if the satellite is directly overhead, EL equals
	zero degrees; the default for this parameter is 20.

OPERATION

The value of an output (radar) dn is equal to 

	SCALE*COS(THETA)/(SIN(THETA) + .1*COS(THETA))**3.

where

	COS(THETA) = R1/R2
	SIN(THETA) = R3/R2
	
	R2=MAGNITUDE(LxS)
	R1=The dot product of (LxS) and ILLUM_VECTOR
	R=MAGNITUDE((LxS)xILLUM_VECTOR)

	and

	L = (LSCALE*2.,0.,(DN(L+1)-DN(L-1))*ZSCALE)
	S = (0.,SSCALE*2.,(DN(S+1)-DN(S-1))*ZSCALE)

	ILLUM_VECTOR = unit vector pointing to the satellite from the
		terrain image (assumed the same from all pixels of
		the terrain image.)

If the pixel being processed is at L,S (line, sample), then

	DN(L+1) is the pixel dn at L+1,S
	DN(S+1) is the pixel dn at L,S+1

The location of the output pixel whose input is L,S is

	L'=L
	S'=-OUT_SCALE*A(S)+DN(L,S)*ZSCALE*B(S)

	where

	B(i)=SATELLITE_HEIGHT/R(i)
	A(i)=TOTAL_PIXELS_OUT*SSCALE-(R(i)-AUGMENTED_HT)
	R(i)=SQRT(((N_INPUT_SAMPLES-i) *
	    SSCALE+NADIR_OFFSET)**2.+SATELLITE_HEIGHT**2.)
	AUGMENTED_HT = SQRT(NADIR_OFFSET**2. +
	    SATELLITE_HEIGHT**2.)
	TOTAL_PIXELS_OUT = (SQRT((NADIR_OFFSET+N_INPUT_SAMPLES *
	    SSCALE)**2.+SATTELITE_HEIGHT**2.) -
	    AUGMENTED_HT)/SSCALE

or in summary:

	output position = TOTA-(T'-A)+H*SIN(THETA)

	where

	TOTA=TOT-A
	TOT	the slant range to the terrain image left edge;
	A	the slant range to the terrain image right edge;
	T'	the slant range to the terrain sample;
	H	the terrain altitude at that sample;
	THETA	the angle between the illumination vector and
		the local terrain normal.

RESTRICTIONS:

H*SIN(THETA), above, is the local terrain correction, an approximation
which is good when the satellite altitude >> local terrain altitude.


TOPO LOCATOR MAP DESCRIPTION:

RADAGEN can also create a second output, a topo locator map to the simulated 
radar image.  This topo locator map contains the sample location in the 
simulated radar image of the corresponding pixel in the original digital 
terrain image.

You may have
          topo            simulated radar
        1 pixel     --->    1 pixel
        many pixels --->    1 pixel

   Example:

	      topo	      simulated radar	    locator map
        -----------------   ------------------   ------------------
        | s1  s2        |   |      s8    s12 |   |      s8    s12 |
        | __  __        |   |      __    __  |   |      __    __  |
        ||  ||  |       |   |     |  |  |  | |   |     |s1|  |s2| |
        | --  --        |   |      --  ^ --  |   |      --  ^ --  |
        | p1  p2        |   |      p1  | p2  |   |      p1  | p2  |
        -----------------   -----------+-------  -----------+------
                                       |                    |
			         interpolates      chooses s1 or s2
 
   s1 - sample 1    p1 - pixel 1
   s2 - sample 2    p2 - pixel 2

   Take two pixels from the topo data, p1 and p2, which are located at
   samples s1 and s2.  p1 has moved from sample location s1 to sample
   location s8 in the simulated radar image.  p2 has moved from sample 
   location s2 to sample location s12.  Pixels change only in the sample 
   direction, not in the line direction.  The maximum DN value of these 
   two pixels in the simulated radar is 254 for byte data and 16383 for
   halfword data.  The topo locator map shows that the pixel in sample
   location s8 came from sample s1 in the topo data, and the pixel in 
   sample location s12 came from sample s2 in the topo data.
   
   This example only shows what may happen to two pixels.  Actually many
   topo samples may contribute to the same pixel in the simulated radar.
   Therefore, one pixel in the simulated radar may come from the addition
   of all the radar DN's for several topo samples.  Since the locator map
   cannot keep track of all the topo samples which may contribute to one 
   pixel, it only saves the last sample which contributes to the one pixel.

   After all the topo image pixels have been assigned, there may be "holes"
   in the simulated radar image and topo locator map.  (as shown in the above
   diagram)  The subroutine RESAMP0 fills in all the zero DN's (the holes).
   It fills in zeros in the simulated radar by interpolating.  The locator 
   map is filled in by choosing one of the the nearby non-zero values.  The 
   zeros on the ends of the image and locator map are filled in with the 
   closest non-zero DN value.
 


WRITTEN BY:  Michael Girard, 27 January 1981
COGNIZANT PROGRAMMER:  Michael Girard
REVISION:  1.1		unknown date
CONVERTED TO VICAR2:	January 1986;  Jan Heyada
REVISION:  2.1		July 1986; Kurt Andersen
REVISION                5-SEPT-94 Randy Schenk (CRI)

.LEVEL1
!
.VARIABLE INP
STRING - Input image file
.VARIABLE OUT
STRING - Output image file
and optional topo locator map
.VARIABLE REF
STRING - Reference image file
.VARIABLE UPDATE
STRING - Update file
!
.VARIABLE PROJECT
KEYWORD - Project specific 
          processing to be used
 (MAGELLAN only current option)
.VARIABLE RECTIFY
KEYWORD - Correct the output
      image to projection of 
      input image
.VARIABLE DOMAIN
KEYWORD - LINEAR or LOG output
.VARIABLE FILL
KEYWORD - Fill the end of lines
          with the last value
!
.VARIABLE SIZE
INTEGER - Standard VICAR size 
field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of samples
.VARIABLE NL
INTEGER - Number of lines
!
.VARIABLE EL
REAL - Elevation of radar source
   from vertical 
   (overhead = 0 deg)
.VARIABLE TOP
REAL - Value to be scaled to 
        maximum output dn -- 
   overridden by SCALE parameter
.VARIABLE HEIGHT
REAL - Satellite altitude in km
.VARIABLE SSCALE
REAL - Real scale in m/pixel in
       sample direction
.VARIABLE LSCALE
REAL - Real scale in m/pixel in
       line direction
.VARIABLE ZSCALE
REAL - Altitude scale in terrain
       image in m/dn
.VARIABLE SCALE
REAL - Dn scale in radar image
.VARIABLE TOFF
REAL - Terrain offset
!
.VARIABLE MISCORR
REAL - Ration of miscorrection
  to sample number across line 
.VARIABLE PERIOD
INTEGER - Oscillation period for
          miscorrection in lines
.VARIABLE DEGREE
REAL - Fraction of miscorrection
       affected by oscillation
       Valid: 0.0 to 1.0
.VARIABLE LOG_SC
REAL - Scaling into log domain 
       (dB/dn)
.VARIABLE OFMT 
KEYWORD - The data format of the
     output image, BYTE or HALF.
.LEVEL2
.VARIABLE OUT
The first output is the simulated radar image in slant range created
by the topo data.  The second optional output is a topo locator map
to the simulated radar image.
.VARIABLE REF
This specifies a reference image that must be the same size as the input
image.  Typically this image will be a low resolution version of the
input image.  (Implemented to facilitate MAGELLAN processing modeling)
.VARIABLE UPDATE
This specifies a file in VICAR parameter file format that specifies
updates to the command line parameters that were entered.  If there are 
less than two (2) lines the presence of the file is ignored.

This can be used to model a SAR orbiting spacecraft when the orbit is
significantly different from circular or if the elevation angle (incidence
angle (ILLUM_VECTOR)) changes during the course of the image being 
processed for simulation.

This file may contain the following parameters:

    SS, NS, AZ, EL, HEIGHT, TOP, SSCALE, LSCALE, ZSCALE, SCALE, TOFF

Any or all of these may be specified.  The total NL will be equally partioned
between each specified set of parameters.

.VARIABLE EL
EL specifies the real elevation, in degrees, of the radar source
measured from the vertical perpendicular to the image plane.  (If the
satellite were directly overhead, EL would equal 0.0 degrees.) 
.VARIABLE TOP
TOP is used to compute the scale if the SCALE parameter is not entered.
When used, the scale is 16383.0/TOP for halfword output and 254.0/TOP
for byte output.  The default is 30.
.VARIABLE HEIGHT
HEIGHT is the altitude of the satellite in real kilometres.  
.VARIABLE SSCALE
SSCALE is the (real) scale in meters/pixel of the terrain image in the sample
direction.
.VARIABLE LSCALE
LSCALE is the (real) line scale in meters/ pixel in the terrain image.
.VARIABLE ZSCALE
ZSCALE is the altitude scale in the terrain image in real meters/dn.
.VARIABLE SCALE
The dn scale (real) in the radar image.  If SCALE is not entered, the value 
specified with the TOP parameter (or its default) is used.
.VARIABLE TOFF
The (real) terrain offset; if not specified, the program will calculate this 
for you.
.VARIABLE RECTIFY
This keyword controls whether RADAGEN will remap the output pixels into the 
projection provided in the input image. Default is NORECTIFY.

See also HELP REF.
.VARIABLE PROJECT
Currently this project specific processing selection flag is only valid for
MAGELLAN.
.page
If MAGELLAN is specified, the backscatter model computed for Venus and cited
in PS202-      :
will be used in place of the default model described in the general HELP for 
RADAGEN.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstradagen.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="no"
let $autousage="none"
write " "
write " GENERATE TEST IMAGE "
fracgen out=terrain.img nl=10 ns=10 format=byte seed=32161267
write " "
write "-----------------------------------"
write "!! !! !! TEST ALL DEFAULTS !! !! !!"
write "-----------------------------------"
write " "
write "radagen inp=terrain.img out=rada.img"
radagen inp=terrain.img out=rada.img
list rada.img
write " "
write "------------------------------------------------------------"
write "!1 !! !! TEST MAGELLAN BACKSCATTER MODEL"
write "                    AND "
write "         INTERNAL RECTIFICATION ADDITIONS !! !! !! "
write "------------------------------------------------------------"
write " "
write "radagen inp=terrain.img out=radb.img 'mgn 'rectify 'log 'fill 'half"
radagen inp=terrain.img out=radb.img 'mgn 'rectify 'log 'fill 'half
list radb.img
write " "
write "---------------------------------------"
write "!! !! !! TEST SAMPLE EXECUTION !! !! !!"
write "---------------------------------------"
write " "
write "radagen inp=terrain.img out=radd.img height=229. sscale=98.4252 +"
write "            lscale=98.4252 toff=0 zscale=3.280840 el=15."
radagen inp=terrain.img out=radd.img height=229. sscale=98.4252 +
            lscale=98.4252 toff=0 zscale=3.280840 el=15.
write "list radd.img"
list radd.img
end-proc
$ Return
$!#############################################################################
