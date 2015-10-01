$!****************************************************************************
$!
$! Build proc for MIPL module zcircle
$! VPACK Version 1.8, Wednesday, February 07, 1996, 17:39:59
$!
$! Execute by entering:		$ @zcircle
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
$ write sys$output "*** module zcircle ***"
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
$ write sys$output "Invalid argument given to zcircle.com file -- ", primary
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
$   if F$SEARCH("zcircle.imake") .nes. ""
$   then
$      vimake zcircle
$      purge zcircle.bld
$   else
$      if F$SEARCH("zcircle.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zcircle
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zcircle.bld "STD"
$   else
$      @zcircle.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zcircle.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zcircle.com -
	-s zcircle.f -
	-i zcircle.imake -
	-p zcircle.pdf -
	-t tstzcircle.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zcircle.f
$ DECK/DOLLARS="$ VOKAGLEVE"

C************************************************************************
C*									*
C*      ZCIRCLE								*
C*									*
C*      ZCIRCLE	is a program which removes data in a circular or	*
C*	elliptical pattern from an image.  Data may be removed 		*
C*	either inside or outside of the specified pattern, and is	*
C*	either replaced with zero or, optionally, with a specified	*
C*	DN value.							*
C*      ZCIRCLE reads the input dataset line by line, modifying each	*
C*	if neccesary, and write each line directly to the outout.	*
C*      Parameter processing is done in MAIN44 and a line buffer is     *
C*      allocated by calling STACKA and then all the i/o and actual     *
C*      operations of replacing DN values are done in a subroutine	*
C*      ZMASK.								*
C*									*
C*      HISTORY:							*
C*      Written by W. D. Benton, 30 March 1978				*
C* 	Converted to VICAR2 by M. Martin, 6 December, 1985		*
C*      MSTP S/W CONVERSION (VICAR PORTING) by A. Scop (CRI) 1 July, 94 *
C*									*
C************************************************************************

        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44

	IMPLICIT NONE

	INTEGER*4 SL,SS,NL,NS,DN,DCODE,IN_UNIT,OUT_UNIT,PIX_SIZ
	INTEGER*4 STATUS,NUMBER_OF_VALUES,PDF_DEFAULTED,NBYTES
	INTEGER*4 IMG_NL,IMG_NS,XCEN,YCEN,NLI,NSI
	REAL*4 Q,R,REQ,RPOL,X0,Y0,ANGLE,SINA,COSA,TANA
        REAL*4 ECC,DEGRAD,DNMAX(4),DNMIN(4),RDN
        LOGICAL*1 INSIDE,XFLAG,YFLAG,RFLAG,RPFLAG,REFLAG,EFLAG,AFLAG
	LOGICAL XVPTST
	CHARACTER*32 FMT
        CHARACTER*80 MSG1,MSG2,MSG3,MSG4
	COMMON /C1/ Q,R,REQ,RPOL,X0,Y0,SINA,COSA,TANA,ANGLE,RDN,
     &		    SL,SS,NL,NS,DN,DCODE,IN_UNIT,OUT_UNIT,
     &		    PIX_SIZ,INSIDE
        EXTERNAL ZMASK
	DATA DNMAX /255., 32767., 214748637., 1.7E38/
	DATA DNMIN /0., -32768., -214748638., -1.7E38/
C
C Open input file and get info off its label
C
        CALL IFMESSAGE('ZCIRCLE version 1-JULY-94')
        CALL XVEACTION('SA',' ')
	CALL XVUNIT(IN_UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN(IN_UNIT,STATUS,'OP','READ',' ')
	CALL XVGET(IN_UNIT,STATUS,'NL',IMG_NL,'NS',IMG_NS,'PIX_SIZE',
     &  PIX_SIZ,'FORMAT',FMT,' ')
	IF ((FMT.NE.'BYTE').AND.(FMT.NE.'HALF').AND.(FMT.NE.'FULL').AND.
     &  (FMT.NE.'REAL')) THEN
	   CALL XVMESSAGE('Data set must be in one of :',' ')
	   CALL XVMESSAGE('(BYTE HALF FULL REAL*4)',' ')
	   CALL ABEND
	ENDIF
	IF (FMT.EQ.'BYTE') DCODE=-5
	IF (FMT.EQ.'HALF') DCODE=-6
	IF (FMT.EQ.'FULL') DCODE=4
	IF (FMT.EQ.'REAL') DCODE=7
C
C Initialize flags
C
        DEGRAD=0.01745329252
        INSIDE=.FALSE.
        XFLAG=.FALSE.
        YFLAG=.FALSE.
        RFLAG=.FALSE.
        RPFLAG=.FALSE.
        REFLAG=.FALSE.
        EFLAG=.FALSE.
        AFLAG=.FALSE.
C
C Process parameters
C
	CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
	IF ((SL.LT.1).OR.(SL.GT.IMG_NL)) THEN
	   CALL XVMESSAGE('Invalid SL value',' ')
	   CALL ABEND
	ENDIF
	IF ((NL.LT.1).OR.(NL.GT.IMG_NL)) THEN
	   CALL XVMESSAGE('Invalid NL value',' ')
	   CALL ABEND
	ENDIF
	IF ((SL+NL-1).GT.IMG_NL) THEN
	   CALL XVMESSAGE('SL+NL-1 must be at most NL in image',' ')
	   CALL ABEND
	ENDIF
	IF ((SS.LT.1).OR.(SS.GT.IMG_NS)) THEN
	   CALL XVMESSAGE('Invalid SS value',' ')
	   CALL ABEND
	ENDIF
	IF ((NS.LT.1).OR.(NS.GT.IMG_NS)) THEN
	   CALL XVMESSAGE('Invalid NS value',' ')
	   CALL ABEND
	ENDIF
	IF ((SS+NS-1).GT.IMG_NS) THEN
	   CALL XVMESSAGE('SS+NS-1 must be at most NS in image',' ')
	   CALL ABEND
	ENDIF
	CALL XVPARM('XCEN',XCEN,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) XFLAG=.TRUE.
	CALL XVPARM('YCEN',YCEN,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
        IF (PDF_DEFAULTED.EQ.0) YFLAG=.TRUE.
	CALL XVPARM('R',R,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
        IF (PDF_DEFAULTED.EQ.0) THEN
	   RFLAG=.TRUE.
	   IF (R.LE.0) THEN
	      CALL XVMESSAGE('Invalid radius value, reset to the ' //
     & 'computed default value',' ')
	      RFLAG=.FALSE.
	   ENDIF
	ENDIF
	CALL XVPARM('DN',DN,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
           IF (FMT.EQ.'BYTE') THEN
	      IF (DN.GT.DNMAX(1)) DN=NINT(DNMAX(1))
              IF (DN.LT.DNMIN(1)) DN=NINT(DNMIN(1))
	   ELSE IF (FMT.EQ.'HALF') THEN
	      IF (DN.GT.DNMAX(2)) DN=NINT(DNMAX(2))
              IF (DN.LT.DNMIN(2)) DN=NINT(DNMIN(2))
	   ELSE IF (FMT.EQ.'FULL') THEN
	      IF (DN.GT.DNMAX(3)) DN=NINT(DNMAX(3))
              IF (DN.LT.DNMIN(3)) DN=NINT(DNMIN(3))
	   ELSE
	      CALL XVMESSAGE
     &             ('DN for BYTE, HALF and FULL data format only',' ')
	      CALL ABEND
	   ENDIF
	ELSE
           IF (FMT.EQ.'BYTE') DN=0
	   IF (FMT.EQ.'HALF') DN=NINT(DNMIN(2))
	   IF (FMT.EQ.'FULL') DN=NINT(DNMIN(3))
	ENDIF
	CALL XVPARM('RDN',RDN,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
	   IF (FMT.EQ.'REAL') THEN
	      IF (RDN.GT.DNMAX(4)) RDN=DNMAX(4)
              IF (RDN.LT.DNMIN(4)) RDN=DNMIN(4)
	   ELSE
	      CALL XVMESSAGE('RDN for REAL data format only',' ')
	      CALL ABEND
	   ENDIF
	ELSE
	   RDN=DNMIN(4)
	ENDIF
	INSIDE= XVPTST('IN')
	CALL XVPARM('RPOL',RPOL,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
	   RPFLAG=.TRUE.
           IF (RPOL.LE.0) THEN
	      CALL XVMESSAGE
     &          ('Invalid RPOL value, reset to the default value',' ')
	      RPFLAG=.FALSE.
	   ENDIF
	ENDIF
 	CALL XVPARM('REQ',REQ,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
	   REFLAG=.TRUE.
           IF (REQ.LE.0) THEN
	      CALL XVMESSAGE
     &          ('Invalid REQ value, reset to the default value',' ')
	      REFLAG=.FALSE.
	   ENDIF
	ENDIF
	CALL XVPARM('ECC',ECC,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
	   EFLAG=.TRUE.
           IF (ABS(ECC).GE.1.) THEN
	      CALL XVMESSAGE
     &          ('Invalid ECC value, reset to the default value',' ')
	      EFLAG=.FALSE.
	   ENDIF
	ENDIF
	CALL XVPARM('ANG',ANGLE,NUMBER_OF_VALUES,PDF_DEFAULTED,1)
	IF (PDF_DEFAULTED.EQ.0) THEN
	   AFLAG=.TRUE.
           IF (ABS(ANGLE).GE.90.) THEN
	      CALL XVMESSAGE
     &          ('Invalid ANG value, reset to the default value',' ')
	      AFLAG=.FALSE.
	   ENDIF
	ENDIF
C
C Open output file
C
  	CALL XVUNIT(OUT_UNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN(OUT_UNIT,STATUS,'O_FORMAT',FMT,'U_FORMAT',FMT,
     &  'OP','WRITE','U_NL',NL,'U_NS',NS,' ')
C
C Compute defaulted prameters
C
        IF (.NOT.XFLAG) XCEN=(NS+1)/2
        IF (.NOT.YFLAG) YCEN=(NL+1)/2
        IF (.NOT.AFLAG) ANGLE=0.
        IF (.NOT.EFLAG) ECC=0.
        IF (.NOT.RFLAG) R=(NL+NS)/4.
	IF (.NOT.REFLAG) REQ=0.
	IF (.NOT.RPFLAG) RPOL=0.
        IF (REFLAG.AND.RPFLAG.AND..NOT.EFLAG)
     &     ECC=SQRT(ABS(RPOL*RPOL-REQ*REQ))/(AMAX1(RPOL,REQ))
	IF (REFLAG.AND.RPFLAG.AND.EFLAG) THEN
	   CALL XVMESSAGE
     &            ('ECC will be recalculated from REQ and RPOL',' ')
           ECC=SQRT(ABS(RPOL*RPOL-REQ*REQ))/(AMAX1(RPOL,REQ))
	ENDIF
        IF (REFLAG.AND.RPFLAG) GO TO 28
        IF (REFLAG) GO TO 22
        IF (RPFLAG) GO TO 23
        IF (ECC.GT.0.) GO TO 21
        IF (ECC.EQ.0.) THEN
	   REQ=R
           RPOL=REQ
	ENDIF
        IF (ECC.EQ.0.) GO TO 28
C
C  Determine RPOL & REQ from R and ECC<0
C
        REQ=2.*R*(ECC*ECC-1.+SQRT(1.-ECC*ECC))/(ECC*ECC)
        RPOL=2.*R-REQ
        GO TO 28
C
C  Determine RPOL & REQ from R & ECC>0
C
   21   RPOL=2.*R*(ECC*ECC-1.+SQRT(1.-ECC*ECC))/(ECC*ECC)
        REQ=2.*R-RPOL
        GO TO 28
C
C  Determine RPOL from REQ & ECC
C
   22   IF (ECC.EQ.0.) RPOL=REQ
        IF (ECC.GT.0.) RPOL=REQ*SQRT(1.-ECC*ECC)
        IF (ECC.LT.0.) RPOL=REQ/SQRT(1.-ECC*ECC)
        GO TO 28
C
C  Determine REQ from RPOL & ECC
C
   23   IF (ECC.EQ.0.) REQ=RPOL
        IF (ECC.GT.0.) REQ=RPOL/SQRT(1.-ECC*ECC)
        IF (ECC.LT.0.) REQ=RPOL*SQRT(1.-ECC*ECC)
C
C  Print input parameters
C
   28   WRITE(MSG1,100)XCEN,YCEN
  100	FORMAT('  CENTER (X,Y) =',I6,I6)
        CALL XVMESSAGE(MSG1,' ')
        WRITE(MSG2,102)REQ,RPOL
  102	FORMAT('  RADII (EQU,POL) =',E12.4,E12.4)
        CALL XVMESSAGE(MSG2,' ')
        WRITE(MSG3,104)ANGLE
  104	FORMAT('  AZIMUTH (NORTH) ANGLE =',E10.4)
        CALL XVMESSAGE(MSG3,' ')
        WRITE(MSG4,106)ECC
  106   FORMAT('  ECCENTRICITY =',E10.4)
        CALL XVMESSAGE(MSG4,' ')

 	REQ=REQ*REQ
        RPOL=RPOL*RPOL
        ANGLE=ANGLE*DEGRAD
        TANA=TAN(ANGLE)
        COSA=COS(ANGLE)
        SINA=SIN(ANGLE)
        X0=FLOAT(XCEN)*COSA+FLOAT(YCEN)*SINA
        Y0=FLOAT(-XCEN)*SINA+FLOAT(YCEN)*COSA
        Q=RPOL+REQ*TANA**2
C
C Allocate line buffer and do i/o operations on it by calling ZMASK
C        
	NBYTES=NS*PIX_SIZ
C	CALL STACKA(ZMASK,1,NBYTES,NBYTES,&900)
	CALL STACKA(3,ZMASK,1,NBYTES)
C
C Close files
C
	CALL XVCLOSE(IN_UNIT,STATUS,' ')
	CALL XVCLOSE(OUT_UNIT,STATUS,' ')
	RETURN

C 900    CALL XVMESSAGE('Insufficient memory for STACKA',' ')
C	CALL XVMESSAGE('Consult a programmer',' ')
C	CALL ABEND
	END


C	SUBROUTINE ZMASK(LINE_BUF,N1,NBYTES)
	SUBROUTINE ZMASK(LINE_BUF,N1)

	IMPLICIT NONE
	BYTE LINE_BUF(*)
        LOGICAL*1 INSIDE
	INTEGER*4 SL,SS,NL,NS,DN,DCODE,IN_UNIT,OUT_UNIT,PIX_SIZ
	INTEGER*4 I,STATUS,I1,I2,J,OFFSET,N1
	REAL*4 Q,R,REQ,RPOL,X0,Y0,ANGLE,SINA,COSA,TANA,RDN
	REAL*4 A,B,S,X1,Y1,X2,Y2

	COMMON /C1/ Q,R,REQ,RPOL,X0,Y0,SINA,COSA,TANA,ANGLE,RDN,
     &		    SL,SS,NL,NS,DN,DCODE,IN_UNIT,OUT_UNIT,
     &		    PIX_SIZ,INSIDE
	
C	IF (N1.LT.NBYTES) THEN
C 	   CALL XVMESSAGE('Insufficient memory for STACKA',' ')
C	   CALL XVMESSAGE('Consult a programmer',' ')
C	   CALL ABEND
C	ENDIF
        DO 30 I=1,NL
	   J=1
           A=FLOAT(I)/COSA
           R=2.*(REQ*TANA*(Y0-A)-RPOL*X0)
           S=REQ*(A*A-2.*Y0*A+Y0*Y0)+RPOL*(X0*X0-REQ)
           B=R*R-4.*Q*S
           IF (B.GE.0.) GO TO 31
C
C Image line does not intercept the circle or the ellipse
C
           IF (.NOT.INSIDE) THEN
	      IF (DCODE.EQ.7) THEN
      		 CALL MVE(DCODE,NS,RDN,LINE_BUF,0,1)
	      ELSE
      		 CALL MVE(DCODE,NS,DN,LINE_BUF,0,1)
	      ENDIF
	   ENDIF
           IF (.NOT.INSIDE) GO TO 32
           CALL XVREAD(IN_UNIT,LINE_BUF,STATUS,'LINE',I+SL-1,'SAMP',SS,
     &                 'NSAMPS',NS,' ')
           GO TO 32
C
C Image line makes interception(s) with the circle or the ellipse
C
31         CALL XVREAD(IN_UNIT,LINE_BUF,STATUS,'LINE',I+SL-1,'SAMP',SS,
     &       	       'NSAMPS',NS,' ')
           X1=(-R-SQRT(B))/(2.*Q)
           X2=(-R+SQRT(B))/(2.*Q)
           Y1=A-TANA*X1
           Y2=A-TANA*X2
           X1=X1*COSA-Y1*SINA
           X2=X2*COSA-Y2*SINA
           IF (X1.LE.X2) GO TO 33
           B=X1
           X1=X2
           X2=B
   33      I1=NINT(X1)
           I2=NINT(X2)
C
C Circle or ellipse is outside to the right of the specified area
C
           IF ((I1.GT.NS).AND.(.NOT.INSIDE)) THEN
	      IF (DCODE.EQ.7) THEN
      		 CALL MVE(DCODE,NS,RDN,LINE_BUF,0,1)
	      ELSE
      		 CALL MVE(DCODE,NS,DN,LINE_BUF,0,1)
	      ENDIF
	   ENDIF
           IF (I1.GT.NS) GO TO 32
C
C Circle or ellipse is outside to the left of the specified area
C
           IF ((I2.LT.1).AND.(.NOT.INSIDE)) THEN
	      IF (DCODE.EQ.7) THEN
      		 CALL MVE(DCODE,NS,RDN,LINE_BUF,0,1)
	      ELSE
      		 CALL MVE(DCODE,NS,DN,LINE_BUF,0,1)     
	      ENDIF
	   ENDIF
           IF (I2.LT.1) GO TO 32
           IF (INSIDE) GO TO 34
C
C Replace pixles before I1 (outside)
C
           IF (I1.GE.1) THEN
	      IF (DCODE.EQ.7) THEN
      		 CALL MVE(DCODE,I1,RDN,LINE_BUF,0,1)
	      ELSE
      		 CALL MVE(DCODE,I1,DN,LINE_BUF,0,1)
	      ENDIF
	   ENDIF
C
C Replace pixles between I2 and NS (outside)
C
	   IF (I2.LE.NS)  THEN
	      OFFSET=(I2-1)*PIX_SIZ+1
	      IF (DCODE.EQ.7) THEN
      		 CALL MVE(DCODE,NS-I2+1,RDN,LINE_BUF(OFFSET),0,1)
	      ELSE
   		 CALL MVE(DCODE,NS-I2+1,DN,LINE_BUF(OFFSET),0,1)
   	      ENDIF
	   ENDIF
           GO TO 32
C
C Replace pixels between I1 and I2
C
   34      IF (I1.LT.1) I1=1
           IF (I2.GT.NS) I2=NS
	   OFFSET=(I1-1)*PIX_SIZ+1
           IF (DCODE.EQ.7) THEN
      	      CALL MVE(DCODE,I2-I1+1,RDN,LINE_BUF(OFFSET),0,1)
	   ELSE
      	      CALL MVE(DCODE,I2-I1+1,DN,LINE_BUF(OFFSET),0,1)
	   ENDIF
   32      CALL XVWRIT(OUT_UNIT,LINE_BUF,STATUS,' ')
   30   ENDDO
        RETURN
        END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zcircle.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM zcircle

   To Create the build file give the command:

		$ vimake zcircle			(VMS)
   or
		% vimake zcircle			(Unix)


************************************************************************/


#define PROGRAM	zcircle
#define R2LIB

#define MODULE_LIST zcircle.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create zcircle.pdf
process help=*
PARM INP  TYPE=STRING  COUNT=1
PARM OUT  TYPE=STRING  COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4   DEFAULT=(1,1,0,0)
PARM SL   TYPE=INTEGER DEFAULT=1
PARM SS   TYPE=INTEGER DEFAULT=1
PARM NL   TYPE=INTEGER DEFAULT=0
PARM NS   TYPE=INTEGER DEFAULT=0
PARM XCEN TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM YCEN TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM R    TYPE=REAL    COUNT=0:1 DEFAULT=--
PARM RPOL TYPE=REAL    COUNT=0:1 DEFAULT=--
PARM REQ  TYPE=REAL    COUNT=0:1 DEFAULT=--
PARM ECC  TYPE=REAL    COUNT=0:1 DEFAULT=0.
PARM ANG  TYPE=REAL    COUNT=0:1 DEFAULT=0.
PARM DN   TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM RDN  TYPE=REAL    COUNT=0:1 DEFAULT=--
PARM IN   TYPE=KEYWORD COUNT=0:1 DEFAULT=-- VALID=(IN)
end-proc
.TITLE
"zcircle"
.HELP
PURPOSE:

	"zcircle" is a VICAR2 applications program which removes
	data in a circular or elliptical pattern from an image.  
	Data may be removed either inside or outside of the
	specified pattern, and is either replaced with lowest
	DN value (i.e., 0 for BYTE data, -32678 for HALF, 
	-214748638 for FULL and -1.7E38 for FULL data) or,
	optionally, with a specified DN value.
.PAGE

OPERATION:
	
	"zcircle" reads the input dataset line by line, modifying
	each if necessary, and writes each line directly to the
	output.  The circle or ellipse defined by the parameters
	may intercept any given line at most 2 times.  If the
	default mode of removing data outside the curve is in
	effect, all data outside of these 2 points is replaced
	with the desired DN value.  If the IN (for inside) option
	is used, all data between the two points is replaced.
	The actual curve is replaced in both modes.  If an ellipse
	is desired, it is possible to generate the axis lengths
	by various combinations of two parameters:
.PAGE

	1) Known average radius and eccentricity, specify R and
	   ECC. If polar radius is larger that equatorial radius,
	   make ECC negative.
	2) Known semimajor and semiminor axis lengths, specify
	   RPOL and REQ as desired.  ECC will be calculated from
	   these two given values.
	3) Known eccentricity and one axis length, specify ECC and
	   RPOL or REQ (as known). If the polar radius is to
	   be larger than the equatorial, make ECC negative.

	The average radius of an ellipse is assumed to be
	(NL+NS)/4.
.PAGE
EXECUTION:

	EXAMPLES	
	   zcircle A B (1 1 500 500)
 	   --This replaces all pixels with zero which are outside
	     of a circle centered at line 250.5, sample 250.5, with
	     radius 250.

	   zcircle A B (1 1 500 500) 'IN
	   --This is similar to the above except that all pixels 
	     inside the circle are replaces with zero.

	   zcircle A B DN=100 XCEN=-10 YCEN=200 R=50 'in
	   --This replaces all pixels with 100 DN which are iside
	     the circle centerd (off the picture) at line 200 and
	     sample -10 with radius 50 pixels.
.PAGE

	   zcircle A B (1 1 500 500) ECC=0.5
	   --This replaces all pixels with zero which are outside of
	     an ellipse centered at (250.5 250.5) and average semi-
	     axis length of 500.  The major axis will be aligned
	     along the sample direction (horizontal).

	   zcircle A B (1 1 500 500) ECC=-0.5
	   --This is similar to the above, except that the major
	     axis is aligned along the line direction (vertical).

	   zcircle A B RPOL=25.3 ECC=0.9 ANG=35.7
	   --This replaces all pixels outside of the ellipse with
	     zero DN defines by an eccentricity of 0.9 and a polar
	     axis length of 50.6 pixels (2*RPOL), with the polar
	     axis pointing to an azimuthal angle of 35.7 degrees
	     clockwise from up.  The major axis length (REQ) is
	     determined by RPOL and ECC to be 116.1 pixels.
.PAGE
PROGRAM HISTORY:
Made portable for UNIX by A. Scop (CRI) 1-JULY-1994
.LEVEL1
.VARI INP
An input image
.VARI OUT
An output image
.VARI SIZE
VIACR size field (SL,SS,NL,NS)
.VARI SL
Integer, starting line number for output
.VARI SN
Integer, starting sample number for output
.VARI NL
Integer, number of lines in the output image
.VARI NS
Integer, number of samples in the output image
.VARI XCEN
Integer, center sample coordinate in pixels
.VARI YCEN
Integer, center line coordinate in pixels
.VARI R
Real, average radius of a circle or ellipse
.VARI RPOL
Real, "polar" radius of an ellipse and overrides the R value
.VARI REQ
Real, "equatorial" radius of a ellipse and overrides the R value
.VARI ECC
Real, eccentricity of the ellipse and may be -1<ECC<+1
.VARI ANG
Real, amizuth angle of rotation of the polar axis and may be -90<ANG<+90
.VARI DN
Integer, DN value to replace removed data for BYTE, HALF and FULL format data
.VARI RDN
Real, DN value to replace removed data for REAL*4 format data
.VARI IN
Keyword, specifies that all the pixels inside the limits of the defined
circle or ellipse are to be removed
.LEVEL2
.VARI INP
An input image
.VARI OUT
An output image
.VARI SIZE
VICAR size field (SL,SS,NL,NS)

	where
    	   SL is the starting line for output
   	   SS is the starting sample for output
	   NL is the number of lines in the output image
	   NS is the number of samples in the output image
.VARI SL
is the starting line for output.
Default is 1.
.VARI SS
is the starting sample for output.
Default is 1.
.VARI NL
is the number of lines in the output image.
Default is the number of lines in the input dataset.
.VARI NS
is the number of samples i the output image.
Default is the number of samples in the input dataset.
.VARI XCEN
is the center sample coordinate in pixels.
Default is (NS+1)/2.
.VARI YCEN
is the center line coordinate in pixels.
Default is (NL+1)/2.
.VARI R
is the average radius of the circle or ellipse.
Default is (NL+NS)/4.
.VARI RPOL
is the "polar" radius of the ellipse and overrides the R value.
Default is determined from the R, REQ, and ECC values.
.VARI REQ
is the "equatorial" radius of the ellipse and overrides the R value
Default is determined from the R, RPOL, and ECC values.
.VARI ECC
is the eccentricity of the ellipse and may be -1<ECC<+1.  When ECC is
negative, the polar axis becomes the major axis.
Default is zero (circle).
.VARI ANG
is an azimuthal angle of rotation of the polar axis, measured
clockwise and may be -90<ANG<+90.
Default is 0.0.
.VARI DN
is the DN value to replace removed data for BYTE, HALF and FULL format data.
Default is 0 or -32768 or -214748638 for BYTE or HALF or FULL.
.VARI RDN
is the DN value to replace removed data for REAL*4 format data.
Default is -1.7E38.
.VARI IN
is a keyword to specify that all pixels inside the limits of
the defined circle or ellipse are to be removed.
Default is that pixels outside of the circle or ellipse
are removed.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstzcircle.pdf
procedure
refgbl $autousage
refgbl $echo
body
!let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen A 100 100 IVAL=100. SINC=0. LINC=0.
zcircle A B DN=10 RPOL=5. REQ=8. 'IN
list B (43 41 15 20)
zcircle A B XCEN=-1 YCEN=20 RPOL=10 ECC=-0.2 'IN
list B (1 1 30 10)
zcircle A B ECC=-0.4 ANG=-45.0 RPOL=5. DN=50 'IN
list B (43 40 20 20)
zcircle A B R=4.
list B (40 40 20 20)
end-Proc
$ Return
$!#############################################################################
