$!****************************************************************************
$!
$! Build proc for MIPL module sos
$! VPACK Version 1.9, Friday, May 11, 2001, 15:20:07
$!
$! Execute by entering:		$ @sos
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
$ write sys$output "*** module sos ***"
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
$ write sys$output "Invalid argument given to sos.com file -- ", primary
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
$   if F$SEARCH("sos.imake") .nes. ""
$   then
$      vimake sos
$      purge sos.bld
$   else
$      if F$SEARCH("sos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sos.bld "STD"
$   else
$      @sos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sos.com -mixed -
	-s sos.f -
	-i sos.imake -
	-p sos.pdf -
	-t tstsos.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C*   08 MAY 95   ...CRI...    MSTP S/W CONVERSION (VICAR PORTING)
C*   10 JUL 91   ...CCA...    Modified to use new VOLABV2
C*   19 AUG 88   ...HBM...    Fixed intercept for LO gain images
C*   10 JUN 84   ...CCA...    CONVERT TO VAX
C*   10 APR 81   ...GMY...    FIX ASTRTCH OPTION FOR LOW GAIN
C*   19 OCT 79    ...GMY...    INITIAL RELEASE
	INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C         SOS (A,COEF) (B,HIS)
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/SCALE,SKALE,LPER,HPER,LEXC,HEXC
      COMMON/C1/CAMERA,FILTER,GAIN,OFFSET,NOISE,IHIST,PRINT,DCODE,I,DBUG
      COMMON/C1/MODE,ISCALE,K,Y1,Y2,LINC,SINC,NLRC,REC,VIKING
      COMMON/C1/SLO,SSO,NLO,NSO,NI,NO
      COMMON/C1/BUF(1820),OBUF(1820),LINE,COEF(3641),HIS(4096)
      CHARACTER*72 LBL
      INTEGER*2 BUF,OBUF,COEF,LINE
      REAL*4 SCALE,SKALE,LPER,HPER,S,INTER
      INTEGER LMASK,RMASK
C
C  THE GENERAL FORM OF THE TRANSFORMATION IS
C
C          OUT =  S * ( M*(IN-K) + 256*(B-Y1) )
C
C        WHERE S  = SCALE/4096,    IF MODE=1 (SCALE OPTION)
C              Y1 = 0
C
C              S  = 1/(Y2-Y1)      IF MODE=2 (ASTRETCH OPTION)
C
C              M,B ARE THE SOSGEN COEFFICIENTS (4096*SLOPE,16*OFFSET)
C              K IS THE DC OFFSET
C              Y1,Y2 ARE 16*UPPER AND 16*LOWER BOUNDS FOR ASTRETCH
C
      DATA LMASK/35/,RMASK/1170/

      CALL IFMESSAGE('SOS version 08-MAY-95')
      CALL XVUNIT(IU,'INP',1,IST,' ')
      CALL XVOPEN(IU,IST,'U_FORMAT','HALF','IO_ACT','SA',
     +                  'OPEN_ACT','SA',' ')
      CALL XVGET(IU,IST,'PIX_SIZE',ICODE,'NL',NLO,'NS',NSO,' ')
      IF(ICODE .NE. 1) THEN
	CALL XVMESSAGE('SOS WORKS ON BYTE DATA ONLY',' ')
	CALL XVCLOSE(IU,IST,' ')
	CALL ABEND
      END IF
C
      OCODE = 1
      NLEV = 256
      CALL SOSPAR(IU,CU,*990)

      IF(DBUG.EQ.1) CALL PRNT(7,4,SCALE,'SCALE.')
      IF(DBUG.EQ.1) CALL PRNT(4,26,CAMERA,'CAMERA.')
      NS4 = 2 * NSO + 1
C-------OPEN OUTPUT FILE
      CALL XVUNIT(OU,'OUT',1,IST,' ')
      CALL XVOPEN(OU,IST,'U_NL',NLO,'U_NS',NSO,'OP','WRITE',
     +          'IO_ACT','SA','OPEN_ACT','SA',
     +          'O_FORMAT','BYTE','U_FORMAT','HALF',' ')
C
      LBL='SOS - SCALE='
      IF (DBUG .EQ. 1) CALL PRNT(2,30,COEF,'COEF.')
      IF (MODE .EQ. 2) GOTO 70
C
C          NOSTRETCH
      ISCALE = ISCALE * SCALE
      IF (SKALE .NE. 0.) THEN
        SCALE = 1024. * SKALE / ISCALE
        ISCALE = 1024. * SKALE
      END IF
      S = SCALE / 4096.
      GOTO 90
C
C          ASTRETCH
   70 IF (Y1 .NE. 0 .OR. Y2 .NE. 0) GOTO 88
      HSS = 1
      HNS = NSO
      IF (VIKING .EQ. 0) GOTO 72
      HSS = LMASK 
      HNS = RMASK - LMASK + 1
C
C------------------------------------------------------------------
C-----COMPUTE HISTOGRAM
C-----IT WILL BE 16 TIMES NORMAL LENGTH NLEV=4096
C
72    LL = 0
      DO 82 L=LINC,NLO,LINC
        LL = LL + LINC
	CALL XVREAD(IU,BUF,IST,'LINE',LL,'NSAMPS',NSO,'SAMP',HSS,' ')
	CALL XVREAD(CU,LINE,IST,'LINE',REC+LL,'NSAMPS',NS4,' ')
        IF(LINE.NE.LL) GOTO 910
        IF(GAIN.EQ.0)then
	  hns1 = hns/(2*sinc)	! DIV takes number of operations
	  CALL DIVV(-6,HNS1,2,COEF(2*HSS),0,2*SINC)
	endif
   82 CALL HTRAN(BUF,COEF(HSS),HIS,K,SINC,HNS)
C
C-----CALC UPPER AND LOWER LIMITS (16 TIMES NORMAL VALUES)
   84 CALL ASTRCH(HIS,Y1,Y2,LPER,HPER,16*NLEV)
      IF(IHIST.EQ.0) GOTO 88
      II = 0
C
C-----SQUEEZE HISTOGRAM TO NORMAL SIZE
      DO 86 I=1,4096,16
        II = II + 1
   86 CALL SUMV(4,16,HIS(I),HIS(II),1,1)
C
C-----PRINT CORRRECTED BUT UNSTRETCHED HISTOGRAM
      CALL SUMV(4,NLEV,HIS,NFREQ,1,1)
      CALL PHIST(HIS,NFREQ,0,NLEV-1,0,0)
C
   88 S = 1. / (Y2-Y1)
      LBL(19:33)='STRETCH ***-***'
      WRITE (LBL(27:29),'(I3)') Y1/16
      WRITE (LBL(31:33),'(I3)') Y2/16
C
C-----ADD HISTORY LABEL
   90 WRITE (LBL(13:17),'(F5.2)') ISCALE/1024.
      CALL XLADD(OU,'HISTORY','COMMENT',LBL,IST,'ULEN',72,
     1           'FORMAT','STRING',' ')
C
C-----------------------------------------------------------------
C-----PERFORM CORRECTION AND COLLECT OUTPUT HISTOGRAM
      IF(NO .EQ. 2 .OR. PRINT .EQ. 1) CALL ZIA(HIS,NLEV)
      LL = 0
      IF(GAIN .EQ. 0) Y1 = 2 * Y1
C
      CALL XVMESSAGE('OUT =  S * ( M*(IN-K) + INTERCEPT*(B-Y1) )',' ')
      INTER = 256.
      IF (GAIN .EQ. 0) INTER = 128.
      call prnt(7,1,S,'S = .')
      call prnt(4,1,K,'K = .')
      call prnt(7,1,INTER,'INTERCEPT = .')
      call prnt(4,1,Y1,'Y1 = .')
      call prnt(4,1,NOISE,'NOISE = .')

      DO L = 1, NLO
        LL = LL + 1
	CALL XVREAD(IU,BUF,IST,'LINE',LL,'NSAMPS',NSO,' ')
	CALL XVREAD(CU,LINE,IST,'LINE',REC+LL,'NSAMPS',NS4,' ')
        IF(LINE.NE.LL) GOTO 910
        CALL STRAN(BUF,COEF,OBUF,NSO,Y1,K,S,NOISE,INTER)
        IF(NO.EQ.2.OR.PRINT.EQ.1) CALL HSUB(OCODE,NSO,OBUF,HIS,0,255)
        CALL XVWRIT(OU,OBUF,IST,'NSAMPS',NSO,' ')
      END DO
C-------------------------------------------------------------------
C-----EXCLUDE PARTS OF HISTOGRAM
      N = HEXC - LEXC + 1
      IF(LEXC.GE.0 .AND. N.GT.0) CALL ZIA(HIS(LEXC+1),N)
      CALL SUMV(4,NLEV,HIS,NFREQ,1,1)
      IF (PRINT .EQ. 1) CALL PHIST(HIS,NFREQ,0,NLEV-1,0,0)
      IF (NO .EQ. 2) THEN
		CALL XVUNIT(HU,'OUT',2,IST,' ')
		CALL XVOPEN(HU,IST,'U_NL',1,'U_NS',256,'OP','WRITE',
     +			    'O_FORMAT','FULL','U_FORMAT','FULL',' ')
		CALL XVWRIT(HU,HIS,IST,'NSAMPS',256,' ')
		CALL XVCLOSE(HU,IST,' ')
      END IF
      CALL XVMESSAGE('SOS TASK COMPLETED',' ')
      CALL XVCLOSE(IU,IST,' ')
      CALL XVCLOSE(CU,IST,' ')
      CALL XVCLOSE(OU,IST,' ')
      RETURN
C
  910 CALL XVMESSAGE('**LINE SEQUENCING ERR--COEF',' ')
      CALL PRNT(2,NS4,LINE,'LINE.')
  990 CALL PRNT(4,26,CAMERA,'CAMERA.')
      CALL XVMESSAGE('**SOS TASK CANCELLED',' ')
      CALL XVCLOSE(IU,IST,' ')
      CALL XVCLOSE(CU,IST,' ')
      CALL XVCLOSE(OU,IST,' ')
      CALL ABEND
      END
C************************************************************************
      SUBROUTINE SOSPAR(IU,CU,*)
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/SCALE,SKALE,LPER,HPER,LEXC,HEXC
      COMMON/C1/CAMERA,FILTER,GAIN,OFFSET,NOISE,IHIST,PRINT,DCODE,I,DBUG
      COMMON/C1/MODE,ISCALE,K,Y1,Y2,LINC,SINC,NLRC,REC,VIKING
      COMMON/C1/SLO,SSO,NLO,NSO,NI,NO
      COMMON/C1/BUF(1820),OBUF(1820),LINE,COEF(3641),HIS(4096)
      CHARACTER*20 DUM(2)
      INTEGER*2 BUF,OBUF,COEF,LINE,FILTAB(7),CAMTAB(5),DCTAB(5)
      INTEGER PAR(40),Y(2),SECS
      REAL*4 SCALE,SKALE,LPER,HPER,R,LOW(5),HIGH(5)
C      EQUIVALENCE (PAR,OBUF)
C
C          DEFAULT PARAMETERS
      DATA FILTAB/0,0,4,0,0,0,0/,CAMTAB/0,0,1,2,3/
      DATA DCTAB/35,0,42,52,57/
      DATA LOW/2.,2.,2.,2.,1./
      DATA HIGH/2.,2.,1.,2.,1./
      LOGICAL XVPTST

      CALL ZIA(CAMERA,20)
      NOISE = 0
      DCFLG = 0
      LPER = -1.
      HPER = -1.
      SCALE = 1.
      SKALE = 0.
      SLO = 1
      SSO = 1

C-------GET NUMBER OF INPUTS AND OUTPUTS
      CALL XVPARM('INP',DUM,NI,IDEF,2)
      CALL XVPARM('OUT',DUM,NO,IDEF,2)

C-----CHECK FOR VIKING LABEL
      CALL VOLABV2(IND,IU,PAR)			! Get info from label.
      IF (IND .EQ. 0) THEN
         IF(PAR(2).LT.4.OR.PAR(2).GT.8.OR.PAR(2).EQ.5) GOTO 1
         VIKING = 1
         CAMERA = PAR(2)
         FILTER = PAR(8)
         GAIN = PAR(11)
         OFFSET = PAR(12)
      END IF
C
1     continue
      IF(XVPTST('ASTRETCH')) MODE = 2		! ASTRETCH mode. Default mode.
      IF(XVPTST('NOSTRETC')) MODE = 1		! NOSTRETCH mode.
      DBUG  = XVPTST('DBUG')			! DEBUG mode.
      PRINT = XVPTST('PHIST')			! Print Histogram
      IHIST = XVPTST('IHIST')			!
C
      IF(XVPTST('NOISE')) THEN		!MAKE RAN SEED
        CALL XVPARM('TNOISE',I,ICNT,IDEF,1)	! get test noise value if
        IF(IDEF .EQ. 0) THEN			! present
          NOISE = I
        ELSE
	   CALL GET_SECONDS(SECS)		!GET SECONDS SINCE 12
	   NOISE = 120135. + FLOAT(SECS)	!ADD TO LARGE NUMBER
	   NOISE = 2 * NOISE + 1		!MAKE SURE IS ODD
        END IF
      END IF
C
      CALL XVPARM('CAMERA',I,ICNT,IDEF,1)	! Override CAMERA S/N from
      IF(IDEF .EQ. 0) THEN			! label
        CAMERA = I
	VIKING = 1
      END IF
C
      CALL XVPARM('FILTER',I,ICNT,IDEF,1)	! Override FILTER from label
      IF(IDEF .EQ. 0) FILTER = I
C
      CALL XVPARM('GAIN',I,ICNT,IDEF,1)		! Override GAIN from label
      IF(IDEF .EQ. 0) GAIN = I
C
      CALL XVPARM('OFFSET',I,ICNT,IDEF,1)
      IF(IDEF .EQ. 0) OFFSET = I
C
      CALL XVPARM('DC',I,ICNT,IDEF,1)		! DC value to subtract if
      IF(IDEF .EQ. 0) THEN			! DC offset was off
	K = I
	DCFLG = 1
      END IF
      CALL XVPARM('LPERC',R,ICNT,IDEF,1)	! Stretch lower percent of DN's
      IF(IDEF .EQ. 0) LPER = R
C
      CALL XVPARM('HPERC',R,ICNT,IDEF,1)	! Stretch upper percent of DN's
      IF(IDEF .EQ. 0) HPER = R
C
      CALL XVPARM('STRETCH',Y,ICNT,IDEF,2)
      IF(IDEF .EQ. 0) THEN
	Y1 = 16*Y(1)
	Y2 = 16*Y(2)
	MODE = 2 
      END IF

      CALL XVPARM('LINC',LINC,ICNT,IDEF,1)
      CALL XVPARM('SINC',SINC,ICNT,IDEF,1)
      CALL XVPARM('EXCLUDE',LEXC,ICNT,IDEF,2)
C
      CALL XVPARM('SCALE',R,ICNT,IDEF,1)
      IF(IDEF .EQ. 0) THEN
	SCALE = R
	MODE = 1
      END IF
C
      CALL XVPARM('SKALE',R,ICNT,IDEF,1)
      IF(IDEF .EQ. 0) THEN
	SKALE = R
	MODE = 1
      END IF
C
C          CHECK VIKING CALIBRATION FILE
      CALL XVUNIT(CU,'INP',2,IST,' ')
      CALL XVOPEN(CU,IST,'IO_ACT','SA','OPEN_ACT','SA',' ')
      CALL XVREAD(CU,COEF,IST,'LINE',1,'NSAMPS',30,' ')
      REC = 1
      IF (VIKING .NE. 0) THEN
        IF (CAMERA.LT.4 .OR. CAMERA.GT.8) GOTO 900
        IF (LPER .LT. 0.) LPER = LOW(CAMERA-3)
        IF (HPER .LT. 0.) HPER = HIGH(CAMERA-3)		! If HPER not given
        IF (OFFSET .EQ. 1) K = 0			! If offset off
        IF (OFFSET .EQ.0 .AND. DCFLG.EQ.0) K = DCTAB(CAMERA-3)*(GAIN+1)
        ICAM = CAMTAB(CAMERA-3)
        IF (COEF(1).NE.4 .OR. COEF(2).NE.4) GOTO 40
        REC = 1057*(FILTAB(FILTER)+ICAM) + 1
        CALL XVREAD(CU,COEF,IST,'LINE',REC,'NSAMPS',30,' ')
      END IF
C
   40 ISCALE = COEF(15)
      IF (LPER .LT. 0.) LPER = 3.
      IF (HPER .LT. 0.) HPER = 3.
      IF (DCODE .EQ. 0) DCODE = COEF(17)
      RETURN
C
  900 CALL XVMESSAGE('**INVALID CAMERA S/N',' ')
      RETURN1
      END
************************************************************************
	SUBROUTINE STRAN(BUF,COEF,OBUF,NSO,Y1,K,S,SEED,INTER)
C-------THIS ROUTINE IS THE PIXEL LOOP WHICH COMPUTES THE
C-------OUTPUT PIXEL VALUES FROM THE COEF, INPUT, SCALE ETC
C-------IF THE SEED IS NOT ZERO, A FRACTION OF A DN OF RANDOM
C-------NOISE WILL BE ADDED TO THE OFFSET.  IF GAIN=0 USE AN
C-------INTERCEPT OF 128.
	INTEGER*2 COEF(3641),BUF(1820),OBUF(1820)
	INTEGER*4 K,Y1,SEED
	REAL*4 M,INTER,RANNUM
C
	DO I=1,NSO
	   J = 2*I
	   M = float(COEF(J-1))
	   B = float(COEF(J)-Y1)
	   IF(SEED .NE. 0) THEN
              CALL RANGEN(SEED,RANNUM)
              B = B + RANNUM
           END IF
	   X = float(BUF(I) - K)
	   E = S * (M*X + INTER*B) + 0.5
	   IF(E .GT. 255.) E = 255.
	   IF(E .LT. 0.)   E = 0.0
	   OBUF(I) = E
	END DO
       	RETURN
	END
	SUBROUTINE HTRAN(BUF,COEF,HIS,K,SINC,NS)
C-------THIS ROUTINE WILL COMPUTE A HISTOGRAM OF A SUBSET OF
C-------THE CORRECTED PIXEL VALUES.  THE VALUES ARE CALCULATED
C-------AS 16 TIMES THEIR NORMAL VALUES, SO THE HISTOGRAM IS 
C-------16 TIMES THE NORMAL SIZE AND UPPER AND LOWER LIMITS
C-------DERIVED FROM IT ARE 16 TIMES NORMAL.
	INTEGER*2 COEF(3641),BUF(1820)
	INTEGER*4 HIS(4096),SINC
	REAL*4 M
C
	DO I=SINC,NS,SINC
		J = 2*I
		M = float(COEF(J-1))
		B = float(COEF(J))
		X = BUF(I) - K
		N = nint(M * X / 256. + B + 0.5)
		IF(N .LT. 0) N = 0
		IF(N .GT. 4095) N = 4095
	 	HIS(N+1) = HIS(N+1) + 1
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sos.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM sos

   To Create the build file give the command:

		$ vimake sos			(VMS)
   or
		% vimake sos			(Unix)


************************************************************************/


#define PROGRAM	sos
#define R2LIB

#define MODULE_LIST sos.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create sos.pdf
PROCESS HELP=*
PARM	INP 	TYPE=STRING	COUNT=2
PARM	OUT	TYPE=STRING	COUNT=(1:2)
PARM	DBUG	TYPE=KEYWORD	VALID=DBUG	COUNT=0:1   DEFAULT=--
PARM	PHIST	TYPE=KEYWORD	VALID=PHIST	COUNT=0:1   DEFAULT=--
PARM	NOISE	TYPE=KEYWORD	VALID=NOISE	COUNT=0:1   DEFAULT=--
PARM	MODE	TYPE=KEYWORD	                COUNT=0:1   DEFAULT=ASTRETCH +
				VALID=(ASTRETCH,NOSTRETC)
PARM	CAMERA	TYPE=INTEGER	VALID=(4,6,7,8)	COUNT=0:1   DEFAULT=4
PARM	FILTER	TYPE=INTEGER			COUNT=0:1   DEFAULT=0
PARM	GAIN	TYPE=INTEGER	VALID=(0,1)	COUNT=0:1   DEFAULT=0
PARM	OFFSET	TYPE=INTEGER			COUNT=0:1   DEFAULT=0
PARM	DC	TYPE=INTEGER			COUNT=0:1   DEFAULT=0
PARM	LPERC	TYPE=REAL			COUNT=0:1   DEFAULT=2.
PARM	HPERC	TYPE=REAL			COUNT=0:1   DEFAULT=2.
PARM	LINC	TYPE=INTEGER			COUNT=0:1   DEFAULT=9
PARM	SINC	TYPE=INTEGER			COUNT=0:1   DEFAULT=3
PARM	IHIST	TYPE=KEYWORD	VALID=IHIST	COUNT=0:1   DEFAULT=--
PARM	EXCLUDE	TYPE=INTEGER			COUNT=2	    DEFAULT=(-1,0)
PARM	STRETCH	TYPE=INTEGER			COUNT=2	    DEFAULT=(0,255)
PARM	SCALE	TYPE=REAL			COUNT=0:1   DEFAULT=1.
PARM	SKALE	TYPE=REAL			COUNT=0:1   DEFAULT=0.
PARM	TNOISE	TYPE=INTEGER			COUNT=0:1   DEFAULT=--
END-PROC
.TITLE
VICAR program "sos" -- Removes camera shading from VO images 
.HELP
PURPOSE:

	Removes camera shading from images by assuming a linear transfer
 curve, with coefficients for each pixel in image space. Shading correction
 coefficients for each pixel are generated by "sosgen". These coefficients
 are applied by "sos" to remove camera shading from individual frames. These
 programs are intended for use with the Viking Orbiter VIS system.

	There have been modifications made to "sos". Two of them are directed
 at controlling the contrast of the output picture by introducing scale and
 auto-stretch parameters. The rest of the changes apply only to Viking Orbiter
 frames and involved parameters to override information in the VO label and a
 modification to subtract the dark current from frames for which the D.C.
 offset has been turned off. All parameters are optional and may be specified
 in any order.

.PAGE
OPERATION:

	Shading correction coefficients are generated by "sosgen" using
 calibration data consisting of a sequence of flat-field frames taken at
 increasing levels of illumination. Only frames taken at exposures within
 the linear portion of the light-transfer curve should be used (see IOM
 324-IPL/71-843 by J. B. Seidman and A. A. Schwartz, "First Order Photometric
 Correction," 9 June 1971).

	A modification has been made to subract the dark current from all
 Viking Orbiter frames taken with the D.C. offset turned off before applying
 the shading correction transformation (reference c). This is accomplished
 by subtracting a constant K from all samples in the input frame. The values
 for K are given in Table 1. (The parameter DC is specified.)

		_______________________________________________
		|	|	     |	OFFSET VALUE(K)       |
		|  S/C	| CAMERA S/N |------------------------|
		|  	|	     | 	HIGH GAIN | LOW GAIN  |
		|-------|------------|------------|-----------|
		|	|	     |		  |	      |
		| VO-1A |      7     |     90	  |	45    |
		|_______|____________|____________|___________|
		|	|	     |		  |	      |
		| VO-1B |      4     |     50	  |	25    |
		|_______|____________|____________|___________|
		|	|	     |		  |	      |
		| VO-2A |      8     |     62	  |	31    |
		|_______|____________|____________|___________|
		|	|	     |		  |	      |
		| VO-2B |      6     |     62	  |	31    |
		|_______|____________|____________|___________|

	The basic reason for adjusting the picture contrast within the program 
 rather than during a subsequent job step is to reduce the contouring which
 occurs when an underexposed frame is severely stretched. This contouring is
 primarily caused by the bit truncation error which results when brightness
 information is recorded over a narrow DN range. The scale and ASTRETCH options
 are designed to broaden the DN range of the output picture. What follows is
 a brief description of the shading correction algorithm (see referenc a) and
 how it is modified by the SCALE and ASTRETCH options.

 	Camera induced shading is removed from a picture by first determining
 the light transfer funciton (assumed to be linear) relating image DN
 (x sub ij) to ovject scene luminance (l sub ij). The inverse light tranfer
 function may be approximated as follows:

		l   = m* x  + b*
		 ij    ij ij   ij

	Each pixel in the image is restored to its original object scene
 luminance, and the result scaled as follows:

		y   = s (m* x   + b* )				(1)
		 ij    o  ij ij    ij
 where
		y   = corrected output DN
		 ij

		     mean object scene luminance
		s  = ---------------------------
		 o         mean image DN

	The shading coefficients are generated by the VICAR program "sos"GEN
 and stored in a calibration file as ordered pairs (m' sub ij,b' sub ij), where
 
	m'  = 4096m* s		and	b'  = 16m* s
	 ij        ij o			 ij      ij o

	The shading correction actually performed by "sos" thus involves the 
 following steps:
			y   = ((m'  * x  )/intercept + b' ) / 16
			 ij      ij    ij         ij

 where intercept is 256 for HI gain images and 128 from LO gain images.

	Dropping the subscripts:
			y = ((m'*x)/intercept + b')/16		(2)

	Truncation errors are introduced at the two division steps (all
 calculations are performed using integer variables). When the SCALE parameter
 S is used, equation 2 is modified as follows:

		y  = S' * (m*x + intercept *b)
		 s
	where
		S' = S/4096

	When the ASTRETCH option is used, equation 2 is modified as follows:
 First, a histogram is generated by performing the foillowing transformation
 over a sub-sampling o the input picture:

		y' = (m' * x) / intercept + b'

.PAGE
RESTRICTIONS:

	"sos" is currently restricted to byte pictures of smaple size no larger
 than 1204. Because of the format of the generated coefficients, the allowable
 range of values are |m  | < 8 and |b  | < 2048.
		     | ij|	   | ij|

HISTORY:

  WRITTEN BY:  G.M.Yagi,  19 Oct. 1979

  CONVERTED TO VAX BY: C.C.Avis,  10 June 84

08 MAY 95   ...AMS...    (CRI) Made portable for UNIX
10 JUL 91   ...CCA...    Modified to use new VOLABV2
19 AUG 88   ...HBM...    Fixed intercept for LO gain images
10 JUN 84   ...CCA...    CONVERT TO VAX
10 APR 81   ...GMY...    FIX ASTRTCH OPTION FOR LOW GAIN
19 OCT 79   ...GMY...    INITIAL RELEASE

.LEVEL1
.VARI INP
STRING-Input file name.
.VARI OUT
STRING-Output file name.
.VARI DBUG
KEYWORD-Debugging statements
for programmers use.
.VARI PHIST
KEYWORD-Prints histogram.
.VARI NOISE
KEYWORD-
.VARI MODE
KEYWORD-
.VARI CAMERA
INTEGER-Camera serial number.
.VARI FILTER
INTEGER-Filter position.
.VARI GAIN
INTEGER-Gain state.
.VARI OFFSET
INTEGER-Dark current offset on or
off.
.VARI DC
INTEGER-Dark current offset.
.VARI LPERC
REAL-Percentage of output samples
to be saturated to 0 DN.
.VARI HPERC
REAL-Percentage of output samples
to be saturated to 255 DN.
.VARI LINC
INTEGER-Line increment.
.VARI SINC
INTEGER-Sample increment.
.VARI IHIST
KEYWORD-
.VARI EXCLUDE
INTEGER-
.VARI STRETCH
INTEGER-Causes a linear stretch to
be performed.
.VARI SCALE
REAL-Each element in the output
picture will be multiplied by the
constant term S.
.VARI SKALE
REAL-
.VARI TNOISE
INTEGER-test noise value
.LEVEL2
.VARI INP
Input files:  the first is the image to be processed, the second is
the Viking calibration file.
.VARI OUT
 STRING - Output file name.
.VARI DBUG
 KEYWORD - Valid:('DBUG) Debugging statements for programmers use.
.VARI PHIST
 KEYWORD - Valid:('PHIST) Prints histogram.
.VARI NOISE
 KEYWORD - Valid:('NOISE)
.VARI MODE
 KEYWORD - Valid:('MODE)
.VARI CAMERA
 INTEGER - Camera serial number.
.VARI FILTER
 INTEGER - Filter position.
.VARI GAIN
 INTEGER - Gain state.
.VARI OFFSET
 INTEGER - Dark current offset on or off.
.VARI DC
 INTEGER - Dark current offset.
.VARI LPERC
 REAL - Percentage of output samples to be saturated to 0 DN.
.VARI HPERC
 REAL - Percentage of output samples to be saturated to 255 DN.
.VARI LINC
 INTEGER - Line increment.
.VARI SINC
 INTEGER - Sample increment.
.VARI IHIST
 KEYWORD - 
.VARI EXCLUDE
 INTEGER - 
.VARI STRETCH
 INTEGER - Causes a linear stretch to be performed.
.VARI SCALE
 REAL - Each element in the output picture will be multiplied by the
  constant term S.
.VARI SKALE
 REAL - 
.VARI TNOISE
 INTEGER-test noise value to
  get none random value
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsos.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar

LOCAL DIR TYPE=STRING
LOCAL CAL  TYPE=STRING
LOCAL IMAG TYPE=STRING
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

if ($syschar(1) = "UNIX")
   LET DIR   ="/project/test_work/testdata/sitod1/test_data/images/viking/"
else 
   LET DIR   ="wms_test_work:[testdata.sitod1.test_data.images.viking]"
end-if
let CAL= "&DIR"//"voshad.cal"
let IMAG= "&DIR"//"vo.raw"

write "THIS IS A TEST OF MODULE SOS"
!
write "LIST AREA OF THE INPUT FILE"
!
list &IMAG (100,100,5,15)
write "RUN WITH HISTOGRAM OUTPUT FILE"
sos (&IMAG,&CAL) (X.SOSIMG,HIS) +
    'PHIST 'NOISE 'NOSTR TNOISE=324613
write "LIST AREA OF RESULT"
list X.SOSIMG (100,100,5,15)
write "LIST PART OF HISTOGRAM"
list HIS (1,1,1,40)
write "NOW USE AUTO STRETCH"
sos (&IMAG,&CAL) Y.SOSIMG 'IHIS 'ASTR LPERC=1 HPERC=2 +
    LINC=50 SINC=50 EXCL=(0,10)
label-list Y.SOSIMG
list Y.SOSIMG (100,100,5,15)
write "USE INPUT STRETCH LIMITS"
sos (&IMAG,&CAL) Z.SOSIMG STRETCH=(100,200) 'DBUG +
    CAM=7 FILT=1 GAIN=0 DC=10
list Z.SOSIMG (100,100,5,15)
write "USE SCALE PARAMETERS"
sos (&IMAG,&CAL) W.SOSIMG SCALE=2 SKALE=5
label-list W.SOSIMG
list W.SOSIMG (100,100,5,15)
write "THE OUTPUTS ARE LEFT ON DISK SO THEY CAN BE DISPLAYED"
end-proc
$ Return
$!#############################################################################
