$!****************************************************************************
$!
$! Build proc for MIPL module fft22
$! VPACK Version 1.9, Friday, April 17, 1998, 12:19:12
$!
$! Execute by entering:		$ @fft22
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
$ write sys$output "*** module fft22 ***"
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
$ write sys$output "Invalid argument given to fft22.com file -- ", primary
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
$   if F$SEARCH("fft22.imake") .nes. ""
$   then
$      vimake fft22
$      purge fft22.bld
$   else
$      if F$SEARCH("fft22.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fft22
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fft22.bld "STD"
$   else
$      @fft22.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fft22.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fft22.com -
	-s fft22.f -
	-i fft22.imake -
	-p fft22.pdf -
	-t tstfft22.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fft22.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM FFT22
C  1 JUL 94 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  CALLS 'WORK' WITH USER-SPECIFIABLE MEMORY SIZE
      INTEGER BUFSIZ, POW
      EXTERNAL WORK

      CALL IFMESSAGE('FFT22 version 1-JUL-94')
      CALL XVEACTION('SA',' ')
      CALL XVPARM( 'BUFPOW', POW, I, J, 1)
      BUFSIZ = 2**POW
      CALL STACKA(3, WORK, 1, BUFSIZ)
      RETURN
      END

      SUBROUTINE WORK( BUF, BUFSIZ)
      INTEGER TOTCOR, BUFSIZ
      REAL*4 BUF(BUFSIZ/4)
C
      INTEGER SLO,SSO,UNIT1,UNIT2,UNIT3,STAT
      INTEGER SGN1, SGN2, INFM, IOS
      CHARACTER*4 FMT
      CHARACTER*4 OUTFMT(5)
      CHARACTER*8 OTFMT, MODE,UNAME
      CHARACTER*132 SCR
      LOGICAL INVFLG
      DATA SGN1/-2/, SGN2/-2/, INFM/1/
      DATA OUTFMT/'BYTE','HALF','FULL','REAL','COMP'/
      DATA INVFLG/.FALSE./

C        DEFINE IEXP FUNCTION
      IEXP(N) = 2**N

      TOTCOR = BUFSIZ/8
      ISRINC = 0

C  OPEN INPUT DATA SET
      CALL XVUNIT(UNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(UNIT1, STAT,' ')

C  'SIZE'
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF ((SLO + NLO - 1).GT.NLI) NLO = NLI - SLO + 1
      IF ((SSO + NSO - 1).GT.NSI) NSO = NSI - SSO + 1
      IF (NSO.LE.0 .OR. NLO.LE.0) THEN
	CALL XVMESSAGE('** INVALID INPUT AREA REQUESTED **',' ')
	CALL ABEND
      ENDIF

C  GET FORMAT OF INPUT FILE
      CALL XVGET(UNIT1,STAT,'FORMAT',FMT,' ')
      IF (FMT.EQ.'BYTE') INFM=1
      IF (FMT.EQ.'HALF') INFM=2
      IF (FMT.EQ.'FULL') INFM=3
      IF (FMT.EQ.'REAL') INFM=4
      IF (FMT.EQ.'COMP'.OR.FMT.EQ.'COMPLEX') INFM=5

C  'MODE'
      CALL XVPARM( 'MODE', MODE, ICNT, IDEF, 1)
      IF ((ICNT.EQ.0.AND.INFM.EQ.5) .OR. MODE.EQ.'INVERSE')
     & INVFLG = .TRUE.

      IF (INVFLG) THEN
	SGN1 = 2
	SGN2 = 2
	IF (INFM.NE.5) CALL MABEND(
     &	 '** INPUT TO INVERSE TRANSFORM MUST BE COMPLEX **')
	CALL XVMESSAGE('INVERSE TRANSFORM',' ')
      ELSE
	CALL XVMESSAGE('FORWARD TRANSFORM',' ')
      ENDIF

C  'FORMAT' = FORMAT OF IMAGE
      IF (INVFLG) THEN		! ONLY VALID FOR INVERSE TRANSFORM
	CALL XVPARM('FORMAT',FMT,ICOUNT,IDEF,1)
	IF (IDEF.EQ.0) THEN
	  IF(FMT.EQ.'BYTE') IFMT=1
	  IF(FMT.EQ.'HALF') IFMT=2
	  IF(FMT.EQ.'FULL') IFMT=3
	  IF(FMT.EQ.'REAL') IFMT=4
	  IF(FMT.EQ.'COMP'.OR.FMT.EQ.'COMPLEX') IFMT=5
	ELSE
	  IFMT = 1		! BYTE DEFAULT OUTPUT FOR INVERSE
	ENDIF
      ELSE			! FORWARD: USE LABEL FORMAT
	IFMT = INFM
      ENDIF

      IF (.NOT.INVFLG .AND. IFMT.NE.5) THEN
	CALL XVCLOSE( UNIT1, STAT,' ')
	CALL XVOPEN( UNIT1, STAT, 'U_FORMAT', 'COMP',' ')
      ENDIF

C  SET DATA LIMITS FOR INTEGER OUTPUT:
      IF (INVFLG) THEN
	IF (IFMT.EQ.1) THEN
	  VLO = 0.0
	  VHI = 255.0
	ELSEIF (IFMT.EQ.2) THEN
	  VLO = -32768.0
	  VHI = 32767.0
	ELSEIF (IFMT.EQ.3) THEN
	  VLO = -2.14748E9
	  VHI = 2.14748E9
	ENDIF
      ENDIF

      DO I = 1,2*TOTCOR		! ZERO THE BUFFER
	BUF(I) = 0.0
      ENDDO

C  FIND THE POWER OF 2 THAT CONTAINS MAX(NLO,NSO):
      NA = 1
      IDIMEN = 2
      DO WHILE (IDIMEN.LT.NLO .OR. IDIMEN.LT.NSO)
	NA = NA+1
	IDIMEN = 2*IDIMEN
      ENDDO

      NSPG = TOTCOR/IDIMEN
      JA = MIN0(LOG2(NSPG),NA)
      IF (JA.LE.0) THEN
	CALL XVMESSAGE('** BUFFER MUST HOLD AT LEAST 2 LINES **',' ')
	CALL ABEND
      ENDIF

      NSPG = IEXP(JA)
      KK = (NA - 1)/JA		! NUMBER OF PASSES REQUIRED
      MSROW = 1
      ITEMP2 = IEXP(JA * (KK + 1) - NA)
      MAXC = NSPG * IDIMEN
      LINE = SLO

      NSAMP = NSO  ! (THESE DEFINITIONS LEFT OVER FROM OLD ALGORITHM)
      NLINE = NLO
      NL = NLO
      NS = NSO
      RESCAL = NLINE*NSAMP

C  ASSIGN UNIT NO. TO OUTPUT
      CALL XVUNIT( UNIT2,'OUT',1,STAT,' ')

C  DETERMINE IF SCRATCH FILE NEEDED
      UNIT3 = UNIT2			! BY DEFAULT, USE OUTPUT FILE
      IF (KK.GT.0 .AND. ((IFMT.NE.5 .AND. INVFLG) .OR.
     & NLINE.LT.IDIMEN .OR. NSAMP.LT.IDIMEN)) THEN
	CALL XVPARM( 'SCRATCH', SCR, I, IDEF,1)
        IF (IDEF.EQ.1) THEN
              CALL TESTOS(IOS)
              IF (IOS.EQ.0) SCR='V2$SCRATCH:FFT2SCRX'
              IF (IOS.EQ.1) THEN
                 CALL GTPRCS(UNAME)
                 SCR='/tmp/fft2scrx.'
                 SCR(15:17)=UNAME
              END IF
        ENDIF
	CALL XVUNIT( UNIT3, 'DUMMY', 1, STAT, 'U_NAME', SCR,' ')
	CALL XVMESSAGE('USING SCRATCH FILE: '//SCR,' ')
      ENDIF
C
C  OPEN OUTPUT DATA SET
      IF (.NOT.INVFLG) THEN
	OTFMT = 'COMP'
      ELSE
	OTFMT = OUTFMT(IFMT)
	IF (IFMT.EQ.5) OTFMT = 'COMP'
      ENDIF
      CALL XVOPEN( UNIT2, STAT, 'OP', 'WRITE', 'U_NL', NSAMP,
     & 'U_NS', NLINE, 'U_FORMAT', 'COMP', 'O_FORMAT', OTFMT,' ')

C  AND SCRATCH, IF NEEDED
      IF (UNIT3.NE.UNIT2)
     & CALL XVOPEN( UNIT3, STAT, 'OP', 'WRITE', 'U_NL', IDIMEN,
     &  'U_NS', IDIMEN, 'U_FORMAT', 'COMP', 'O_FORMAT', 'COMP',' ')

C  BEGIN PASS 1:  PROCESS A BLOCK OF 'NSPG' LINES, WHICH WILL
C  FILL THE BUFFER.

      LM = 1
100   DO 150 I = 1, MAXC, IDIMEN	! 'NSPG' TIMES

      IF (LM.GT.NL) THEN

	DO J = 1,2*NSAMP  ! FILL NON-SQUARE PART (IN LINE DIR.) WITH 0'S
          BUF(2*I+J-2) = 0.0
	ENDDO

      ELSE

	IF (LINE.EQ.SLO) THEN
	  CALL XVREAD(UNIT1,BUF(2*I-1),STAT,'LINE',LINE,
     &     'SAMP',SSO,'NSAMPS',NS,' ')
	  LINE = 0
	ELSEIF (SSO.NE.1 .OR. NS.NE.NSI) THEN
	  CALL XVREAD(UNIT1,BUF(2*I-1),STAT,'SAMP',SSO,'NSAMPS',NS,' ')
	ELSE
	  CALL XVREAD(UNIT1,BUF(2*I-1),STAT,' ')
	ENDIF

	CALL DFFT( BUF(2*I-1), BUF(2*I), NSAMP, NSAMP, NSAMP,
     .   -SGN1, *998, *999)

      ENDIF
      LM = LM + 1
150   CONTINUE

C  NOW TRANSPOSE THE MATRIX IN BLOCKS OF NSPG*NSPG ELEMENTS

      INDEX1 = 2
      INDEX2 = IDIMEN + 1
      DO 155 IP = 2,NSPG
      IPQ = INDEX1
      IQP = INDEX2
      IPM1 = IP - 1
      DO 154 IQ = 1,IPM1
      IPQI = IPQ
      IQPI = IQP
      DO 152 I = 1,IDIMEN,NSPG
      CTEMP1 = BUF(2*IPQI-1)
      CTEMP2 = BUF(2*IPQI)
      BUF(2*IPQI-1) = BUF(2*IQPI-1)
      BUF(2*IPQI) = BUF(2*IQPI)
      BUF(2*IQPI-1) = CTEMP1
      BUF(2*IQPI) = CTEMP2
      IPQI = IPQI + NSPG
      IQPI = IQPI + NSPG
152   CONTINUE
      IPQ = IPQ + IDIMEN
      IQP = IQP + 1
154   CONTINUE
      INDEX2 = INDEX2 + IDIMEN
      INDEX1 = INDEX1 + 1
155   CONTINUE

      IF (KK.EQ.0) GO TO 350
C  (IF TRANSFORMATION CAN BE DONE ENTIRELY IN CORE, BYPASS ALL
C   INTERMEDIATE PROCESSING AND GO TO FINAL STEP)

C  WRITE THE 'NSPG' LINES TO SCRATCH FILE

      DO 160 I = 1, MAXC, IDIMEN
      CALL XVWRIT( UNIT3, BUF(2*I-1), STAT,' ')
  160 CONTINUE
      IF(LM.LT.IDIMEN) GO TO 100	! GO BACK & DO NEXT BLOCK
C
C  END OF PASS 1

C  RE-OPEN OUTPUT/SCRATCH DATA SET FOR UPDATE TO ALLOW RANDOM ACCESS
C  (EVEN IF OUTPUT HAS NOT YET BEEN WRITTEN TO)

      CALL XVCLOSE( UNIT2, STAT,' ')
      CALL XVOPEN(UNIT2, STAT, 'OP', 'UPDATE',
     &            'U_FORMAT', 'COMP', 'O_FORMAT', OTFMT,' ')
      IF (UNIT2.NE.UNIT3) THEN
	CALL XVCLOSE( UNIT3, STAT,' ')
	CALL XVOPEN(UNIT3, STAT, 'OP', 'UPDATE',
     &	            'U_FORMAT', 'COMP', 'O_FORMAT', 'COMP',' ')
      ENDIF

      IF (KK .LT. 2) GO TO 300
C  (IF ONLY 1 PASS NEEDED, GO DIRECTLY TO LAST PASS)

C  INTERMEDIATE PASSES:

      KKM1 = KK - 1
      DO K = 1, KKM1
	IRINC = IEXP(JA * K)
	MSROW1 = 1
	MSROW2 = IRINC
	ISROW2 = IDIMEN
	ISRINC = IRINC * NSPG
	DO MSROW = MSROW1, MSROW2
	  DO ISROW = MSROW, ISROW2, ISRINC
	    I = 1
	    ITEMP = ISROW + ISRINC - 1
	    DO IROW = ISROW, ITEMP, IRINC
	      CALL XVREAD( UNIT3, BUF(2*I-1), STAT, 'LINE', IROW,' ')
              I = I+IDIMEN
	    ENDDO
            INDEX1 = IDIMEN+1
            INDEX2 = IRINC + 1
            DO IP = 2, NSPG
              IPQ = INDEX1
              IQP = INDEX2
              IPM1 = IP - 1
              DO IQ = 1, IPM1
                IPQI = IPQ
                IQPI = IQP
                DO I = 1, IDIMEN, ISRINC
                  DO IT = 1, IRINC
		    CTEMP1 = BUF(2*(IPQI-1+IT)-1)
		    CTEMP2 = BUF(2*(IPQI-1+IT))
		    BUF(2*(IPQI-1+IT)-1) = BUF(2*(IQPI-1+IT)-1)
		    BUF(2*(IPQI-1+IT)) = BUF(2*(IQPI-1+IT))
		    BUF(2*(IQPI-1+IT)-1) = CTEMP1
		    BUF(2*(IQPI-1+IT)) = CTEMP2
		  ENDDO
                  IPQI = IPQI + ISRINC
                  IQPI = IQPI + ISRINC
		ENDDO
                IPQ = IPQ + IRINC
                IQP = IQP + IDIMEN
	      ENDDO
              INDEX1 = INDEX1 + IDIMEN
              INDEX2 = INDEX2 + IRINC
	    ENDDO
            I = 1
            DO IROW = ISROW, ITEMP, IRINC
              CALL XVWRIT( UNIT3, BUF(2*I-1), STAT, 'LINE', IROW,' ')
	      I = I + IDIMEN
	    ENDDO

	  ENDDO		!  END ISROW LOOP

	ENDDO		!  END MSROW LOOP

      ENDDO		!  END K LOOP
C
C  BEGIN LAST PASS

300   ISRINC = IEXP(JA * KK)
      INDINC = IEXP(NA - JA*KK)
      MSROW = 1

C  LOOP OVER MSROW:  READ IN 'NSPG' LINES AT A TIME, 'ITEMP2' LINES 
C  FROM EACH 2**(KK-1)-TH BLOCK:

301   I = 1
      ISROW = MSROW
305   ITEMP = ISROW + ITEMP2 - 1

      DO IROW = ISROW,ITEMP		! READ 'ITEMP2' LINES
	CALL XVREAD( UNIT3, BUF(2*I-1), STAT, 'LINE', IROW,' ')
	I = I+IDIMEN
      ENDDO

      ISROW = ISROW + ISRINC
      IF (I.LT.MAXC) GO TO 305	! LOOP UNTIL 'NSPG' LINES READ
C
C  TRANSPOSE ELEMENTS BETWEEN THE NSPG*NSPG-ELEMENT BLOCKS
C
      INDEX1 = 2
      INDEX2 = NSPG + 1
      DO 340 IR  =  1, ITEMP2
      IPR = INDEX1
      IRP  =  INDEX2
      DO 330 IP  =  2, INDINC
      IPRQ  =  IPR
      IQRP  =  IRP
      IPM1  =  IP  -  1
      DO 320 IQ  =  1, IPM1
      DO 315 IT = 1, ISRINC
      CTEMP1 = BUF(2*((IPRQ-1)*ISRINC+IT)-1)
      CTEMP2 = BUF(2*((IPRQ-1)*ISRINC+IT))
      BUF(2*((IPRQ-1)*ISRINC+IT)-1) = BUF(2*((IQRP-1)*ISRINC+IT)-1)
      BUF(2*((IPRQ-1)*ISRINC+IT)) = BUF(2*((IQRP-1)*ISRINC+IT))
      BUF(2*((IQRP-1)*ISRINC+IT)-1) = CTEMP1
      BUF(2*((IQRP-1)*ISRINC+IT)) = CTEMP2
315   CONTINUE
      IPRQ = IPRQ + NSPG
      IQRP = IQRP + 1
320   CONTINUE
      IPR = IPR + 1
      IRP = IRP + NSPG
330   CONTINUE
      INDEX1 = INDEX1 + INDINC
      INDEX2 = INDEX2 + INDINC
340   CONTINUE
350   CONTINUE

C  PERFORM THE SECOND FFT & WRITE TO THE OUTPUT FILE

      I = 0
      ISROW = MSROW
400   ITEMP = ISROW + ITEMP2 - 1
      DO IROW = ISROW,ITEMP

	IF (IROW.LE. NSAMP) THEN ! EXCLUDE DUMMY LINES FOR NON-POWER OF 2

	  CALL DFFT( BUF(2*I+1), BUF(2*I+2), NLINE, NLINE, NLINE,
     .     -SGN2, *998, *999)

	  IF (.NOT.INVFLG .OR. IFMT.EQ.5) THEN	! IF COMPLEX OUTPUT

	    CALL XVWRIT( UNIT2, BUF(2*I+1), STAT, 'LINE', IROW,' ')

	  ELSE			! NON-COMPLEX OUTPUT

	    IF (IFMT.LE.3) THEN	! INTEGER OUTPUT
	      DO K = 1,NLINE
	        V = BUF(2*(I+K)-1)/RESCAL
	        IF (V.LT.VLO) V = VLO
	        IF (V.GT.VHI) V = VHI
	        BUF(2*K-1) = V + 0.5
	      ENDDO
	    ELSEIF (IFMT.EQ.4) THEN	! FLOATING POINT OUTPUT
	      DO K = 1,NLINE	
	        BUF(2*K-1) = BUF(2*(I+K)-1)/RESCAL
	      ENDDO
	    ENDIF

	    CALL XVWRIT( UNIT2, BUF, STAT, 'LINE', IROW,' ')

	  ENDIF

	ENDIF
	I = I + IDIMEN

      ENDDO
C
C     Sequential write to output file
      IF (ISRINC .EQ. 0) THEN
          ISROW = ISROW + 1
      ELSE
C     Random write to output file
          ISROW = ISROW + ISRINC
      END IF
      IF (I.LT.MAXC) GO TO 400
      MSROW = MSROW + ITEMP2
      IF (MSROW.LE.ISRINC) GO TO 301
C  END LOOP OVER MSROW

C  FIX OUTPUT LABEL IF NL < IDIMEN
      IF (NSAMP.LT.NLINE) THEN
	CALL XLDEL( UNIT2, 'SYSTEM', 'NL', STAT, 'ERR_ACT', 'SA',' ')
	CALL XLADD( UNIT2, 'SYSTEM', 'NL', NSAMP, STAT, 'FORMAT',
     &	 'INT', 'ERR_ACT', 'SA',' ')
      ENDIF
C
C  CLOSE DATA SETS
      CALL XVCLOSE( UNIT1, STAT,' ')
      CALL XVCLOSE( UNIT2, STAT,' ')
      IF (UNIT2.NE.UNIT3) CALL XVCLOSE( UNIT3, STAT, 'CLOS_ACT',
     & 'DELETE',' ')

      CALL XVMESSAGE('TRANSFORM COMPLETED',' ')
C
      RETURN
C
998   CALL XVMESSAGE('*** A PRIME FACTOR OF N EXCEEDS 23 ***',' ')
      CALL ABEND
999   CALL XVMESSAGE
     &  ('*** TOO MANY SQUARE-FREE OR PRIME FACTORS IN N ***',' ')
      CALL ABEND
      END
C
      FUNCTION LOG2(N)
      I = N
      LOG2 = 0
10    I = I/2
      IF (I.EQ.0) RETURN
      LOG2 = LOG2 + 1
      GO TO 10
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fft22.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fft22

   To Create the build file give the command:

		$ vimake fft22			(VMS)
   or
		% vimake fft22			(Unix)


************************************************************************/


#define PROGRAM	fft22
#define R2LIB

#define MODULE_LIST fft22.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fft22.pdf
process help=*
PARM INP TYPE=(STRING,132) COUNT=1
PARM OUT TYPE=(STRING,132) COUNT=1
PARM SIZE TYPE=INTEGER COUNT=(0,4) DEFAULT=--
PARM SL TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM SS TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM NL TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM NS TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM MODE TYPE=KEYWORD VALID=(FORWARD,INVERSE) COUNT=0:1 DEFAULT=--
PARM SCRATCH TYPE=(STRING,32) COUNT=0:1 DEFAULT=" "
PARM FORMAT KEYWORD VALID=(BYTE,HALF,FULL,REAL,COMP) DEFAULT=BYTE
PARM BUFPOW INTEGER DEFAULT=18
END-PROC
.TITLE
VICAR2 Program "fft22"  --  performs 2-dimensional fourier transforms
.HELP
 "fft22" performs direct and inverse two dimensional fast fourier 
 transforms.  The program performs the same function, without the 
 array processor, as FFT2AP (which uses the array processor);  but
 "fft22" is more flexible in that the  image dimensions need not be
 powers of 2 (see RESTRICTIONS).  (Program FT2, with program FTPACK,
 does the same thing as FFT2AP with some restrictions and may be
 faster for large images.  See help for programs FFT2AP and FT2.) 
.page
EXECUTION

"fft22" can be invoked by entering the command:

	fft22 IN OUT [SIZE] PARAMS
where 

IN	is an input picture (forward mode) or an FFT (transform mode)

OUT	is an FFT (forward mode) or an output picture 

SIZE    specifies the area of the input dataset to use

PARAMS  includes parameters allowing:

	- mode selection ('FORWARD, 'INVERSE)

	- image format selection ('BYTE, 'HALF, 'FULL, 'REAL, 'COMP);
	  this applies only to the input in forward mode or to the output
	  in inverse mode.

	- scratch file specification. (This is generally defaulted and
          not always needed.)

        - memory buffer size specification (not generally specified by
          the user).

	See Tutor for full description.
.page 
OPERATION
 The program performs the following operation:

               M-1  N-1                   k*m   l*n
 O(k,l) =  K * SUM( SUM( I(m,n) exp( P * [--- + --- ]) )),
               m=0  n=0                    M     N

   for k = 0,1,...,M-1  and  l = 0,1,...,N-1,

 where:  I(m,n) is line m+1, sample n+1, of the input file,
         O(k,l) is line l+1, sample k+1, of the output file,
         K = 1 for a forward transform, and
             1/(M*N) for an inverse transform,
         P = 2*PI*i for a forward transform, and
             -2*PI*i for an inverse transform.
             [PI = 3.1416...,  i = SQRT(-1)]

 "fft22" uses subroutine DFFT to compute the one-dimensionsal fourier
 transform.  This is done twice, with a transposition step in between.
 For further computational details, see Help (in DCL) for that routine. 
 It uses an algorithm by Richard C. Singleton of Stanford Research
 Institute.
 
 As the above equation implies, the output of the program is stored
 in transposed format, in the matrix sense, compared to the input
 data.  This practice is responsible for faster program execution,
 because it allows one transposition to suffice (after the first
 1-D FFT), and does not bother with the second which would bring
 the output back into standard format.  When a forward and inverse
 transform are performed in sequence, the two transpositions cancel
 out and the original format is recovered.  However, the user must
 be aware of the transposition when working with the transformed data. 
 For example, when filtering by multiplication with a transfer
 function, the user must first transpose the transfer function (unless
 it is transpose-invariant).

 The transposition step requires a buffer of size 2*N*N real*4 words,
 where N is the larger of NL and NS.  The default buffer size is 65536
 real*4 words, so the maximum image dimension that can be accomodated
 is 128. (32768 = 2*128*128); for a larger NL or NS, the transposition
 cannot be performed in memory.  In this case, the operation is performed
 in several passes, with intermediate results being saved on disk.

 If disk storage of intermediate results is required, then the output
 file is used if it is of sufficient size, which is the case if the
 output format is complex (always true for forward mode) and NL is equal
 to NS; otherwise, a scratch file is used.  This file has a default value
 on a standard system scratch directory, but may be redefined using the
 SCRATCH parameter.
.page
RESTRICTIONS:

1. For an inverse transform, the input format must be complex.

2. Either dimension of the FFT (i.e., the number of lines or samples of
  the output in forward mode) may be any number, subject to the following
  constraints:

  a. It may not contain a prime factor greater than 23.

  b. The number of prime factors may not exceed 208.

  c. The square-free portion may not exceed 210.  (A factor P of a number
    N is square-free if it cannot be paired with another identical factor
    of N; i.e., each prime occurring an odd number of times in N is a
    square-free factor of N.  The square-free portion of N is the product
    of its square-free factors.)

  E.g., 221 (=13*17) fails because the square-free part exceeds 210, and
  202 (=2*101) fails because a prime factor exceeds 23, but 210 (=2*3*5*7)
  and 216 (= 2**3 * 3**3) are acceptable.  These restrictions are due to
  subroutine DFFT and may be modified by changing array dimensions and
  recompiling that subroutine.

  The program checks if the dimensions of the input image meet these criteria
  and abends if they do not.  You can check these criteria for a given 
  number N by running this program on a test image with one line and N
  samples and seeing if the program abends.

3. The RMS file header does not support a record length exceeding 32767,
  hence a complex (8-byte per sample) image may not have NS equal to or
  above 4096.  Since "fft22" requires a square intermediate or output file 
  with a complex data format whose dimension is the power 
  of 2 greater than or equal to the maximum of NL
  and NS, this implies that the largest value of NL or NS allowed is 2048.
  Thus, for the input file, neither NL nor NS may be greater than 2048.
  (Program FT2 can handle input images up to 4096 square because it uses
  a packed format.)
.page
TIMING

 The following CPU times were measured for "fft22" on a lightly loaded
 VAX-11/780:  (in seconds)

            image size:      256x256     512x512    1024x1024

       Forward transform:       26         104         422

       Inverse transform:       28         114         451

 Processing time for a 1-dimensional FFT on N samples scales roughly as
 N * log(N).  However, the code of subroutine DFFT strongly favours values
 of N that contain few and low prime factors: powers of 2 are particularly
 efficient.
.PAGE

ORIGINAL PROGRAMMERS:  T. C. Rindfleisch and J. B. Seidman,  30 Oct. 1978

CONVERTED TO VAX BY:  J. H. Reimer and L. W. Kamp,  10 Feb. 1985

CURRENT COGNIZANT PROGRAMMER:  L. W. Kamp

HISTORY:

   1jul94 -- Made portable for UNIX  A. Scop (CRI)
  30aug90 -- changed COMPLEX to COMP in F.T. labels.

.page
 Programming note:  the original matrix transposition algorithm was keyed
 to dimensions that were powers of 2.  When this requirement was relaxed
 by use of subroutine DFFT, the transposition algorithm was not changed.
 (The matrix is treated as if it were part of a larger square matrix whose
 dimension is a power of 2.)  Hence the buffer and scratch file requirements
 can be quite wasteful, e.g., a 2*130 image requires the same scratch file
 and buffer that a 256*256 image would, i.e. 0.5 MB.  If anyone would care
 to rewrite the program to eliminate this feature, they are invited to do so.
.LEVEL1
.VAR MODE
Keyword: Transform mode.
Valid: FORWARD, INVERSE.
.VARI FORMAT
Ouput image data format.
Valid: BYTE, HALF, FULL,
REAL, COMP
.VAR INP
Input dataset.
.VAR OUT
Output dataset.
.VAR SIZE
VICAR size field.
.var sl
Starting line
.var ss
Starting sample
.var nl
Number of lines
.var ns
Number of samples
.var SCRATCH
Intermediate dataset.
(Temporary, not always
needed.)
.VARI BUFPOW
2**BUFPOW = Memory buffer
size
.LEVEL2
.VARI MODE
This parameter specifies the type of transform that is to be performed:

FORWARD specifies that a forward transform is desired, from image space
to transform space.  This is the default if the input format is not 
complex.

INVERSE specifies that an inverse transform is desired, from transform
space to image space.  This is the default if the input format is complex.

Note that it is permissible to specify FORWARD with complex input, but
not to specify INVERSE with non-complex input:  the FFT must always be
in complex format.
.VARI FORMAT
Keyword parameter, valid: BYTE, HALF, FULL, REAL, or COMP.

This parameter specifies the format of the data in the output image file
in the case of an inverse transform.  It is ignored in forward transform
mode, because Vicar2 discourages overriding the input label.

Default = BYTE.

Note that the format of the FFT may never be specified, as it is always
COMP (complex).
.VARI INP
INP specifies the input picture for a forward transform, or the 
transform dataset for an inverse transform.
.VARI OUT
OUT specifies the output transform dataset for a forward transform, 
or the output image for an inverse transform.

The output FFT of an image with NL lines and NS samples will be
of COMPLEX format, with NS lines and NL samples.  I.e., it is 
transposed with respect to the input.  This is done to speed up 
processing.
.VARI SIZE
SIZE is a standard VICAR size field:

 (starting line, starting sample, number of lines, number of samples)

This specifies the subimage of the input image to be processed.
.vari ss
Starting sample.  See Help SIZE.
.vari sl
Starting line.  See Help SIZE.
.vari ns
Number of samples.  See Help SIZE.
.vari nl
Number of lines.  See Help SIZE.
.vari scratch
This specifies a disk file that is used by the program to store
data during intermediate steps of the processing.  It is only
required when both of the following conditions are met:
 
   (a) the image buffer required by the program is larger than
      the buffer size specified by the BUFPOW parameter. The image
      buffer size required is N*N complex numbers, where N is the
      smallest power of 2 that equals or exceeds both NL and NS,
      i.e., 8*N*N bytes; and,

   (b) the output file is not large enough to be used for scratch
      storage.  This will be true if its format is not complex, and
      also if the number of lines of the output is less than the
      number of samples.

The default BUFPOW (18) is sufficient to contain a 128*128 image.
However, note that a 64*256 image, say, requires a scratch file,
since it is the larger of NL and NS that determines the buffer size.

The scratch file will be deleted at the end of the execution of "fft22"
by the program.

The default for this parameter is a file in the standard MIPL scratch
directory.  The user may specify a different location for any reason
(e.g., if that disk is full, or not available).  If this parameter is
specified for a case in which it is not required, it is ignored.
.vari BUFPOW
This determines the size of the buffer that the program uses to 
transpose the FFT.  Since this buffer size (in bytes) is always a power
of 2, this parameter specifies the power of 2 which equals the buffer
size:  BUF_SIZE = 2 ** BUFPOW.

Since the FFT will be transposed, this buffer must be able to hold
at least two lines of N complex numbers, where N is the larger of NS
and NL, hence:
                BUF_SIZE >= 2 * 8 * N,
 or:
                               2
                BUFPOW >= 4 + log(N).
The above conditions must be met for the program to run at all.

Furthermore, the entire operation can be done in memory if
BUF_SIZE >= 8*N*N, or BUFPOW >= 3+logN.  This will speed processing,
assuming that the memory is available and that paging is not a
problem.
.page
The user will not in general need to specify this parameter, but it
is provided for the following contingencies:

 (a) to allow improved performance, since increasing the buffer size
    will cut down on the amount of I/O done in the program, but can
    increase paging if a large working set is not available;
 
 (b) to obviate the need for a scratch file (see parameter SCRATCH);
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfft22.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
local PREF  type=string init="ush /bin/rm"
local SUFF  type=string init=""
local LOUTL type=string init="abcdefghijklmnopqrstuvwxyz1234567890.out"
local LINL  type=string init="1234567890abcdefghijklmnopqrstuvwxyz.in"
if ($syschar(1)="VAX_VMS")
  let PREF = "dcl del"
  let SUFF = ";*"
end-if
!
! BASIC TEST: DO FORWARD + INVERSE, MUST RECOVER ORIGINAL
! USE IMAGE SIZE LARGE ENOUGH TO REQUIRE SCRATCH FILE
gen A.MAT 256 256
list A.MAT (1,1,10,10)
fft22 A.MAT B.MAT
fft22 B.MAT C.MAT 
list C.MAT (1,1,10,10)
!
! FOR OTHER TESTS, USE SMALLER SIZE (NO SCRATCH FILE)
! TEST 'FORMAT'
fft22 B.MAT C.MAT 'HALF
list C.MAT (1,1,10,10)
!
fft22 B.MAT C.MAT 'FULL
list C.MAT (1,1,10,10)
!
fft22 B.MAT C.MAT 'REAL
list C.MAT (1,1,10,10)
!
! TEST UNEQUAL DIMENSION & NOT A POWER OF 2:
gen A.MAT 100 120
list A.MAT (91,111,10,10)
fft22 A.MAT B.MAT
label-list B.MAT
fft22 B.MAT C.MAT 
label-list C.MAT
list C.MAT  (91,111,10,10)
!
! TEST 'SCRATCH' PARAMETER
gen A.MAT 256 256
fft22 A.MAT B.MAT
fft22 B.MAT C.MAT SCRATCH="fftscr.dat"
!  SEE IF IT'S THERE -- SHOULDN'T BE:
!if ($syschar(1) = "VAX_VMS")
!   dcl dir fftscr.dat
!end-if
!
gen A.MAT 10 10
fft22 A.MAT &"LINL"
list &"LINL"
fft22 &"LINL" &"LOUTL"
list &"LOUTL"

&PREF A.MAT&"SUFF"
&PREF B.MAT&"SUFF"
&PREF C.MAT&"SUFF"


end-proc
$ Return
$!#############################################################################
