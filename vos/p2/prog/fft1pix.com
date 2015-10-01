$!****************************************************************************
$!
$! Build proc for MIPL module fft1pix
$! VPACK Version 1.7, Friday, July 22, 1994, 14:08:14
$!
$! Execute by entering:		$ @fft1pix
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
$ write sys$output "*** module fft1pix ***"
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
$ write sys$output "Invalid argument given to fft1pix.com file -- ", primary
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
$   if F$SEARCH("fft1pix.imake") .nes. ""
$   then
$      vimake fft1pix
$      purge fft1pix.bld
$   else
$      if F$SEARCH("fft1pix.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fft1pix
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fft1pix.bld "STD"
$   else
$      @fft1pix.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fft1pix.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fft1pix.com -
	-s fft1pix.f -
	-i fft1pix.imake -
	-p fft1pix.pdf -
	-t tstfft1pix.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fft1pix.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  VICAR Program FFT1PIX
C	
C	FFT1PIX  is  a  VICAR application program which works in conjunction
C	with  FFT1  to  allow  the user to display and/or modify the Fourier
C	transform  using  existing VICAR programs.  The procedure is similar
C	to  that  used  in  FFT2,  FFTPIC,  and  TPIKMASK  but  without  the
C	restriction  that  terms  of  the transform be either set to zero or
C	left  unchanged.   In  the forward mode, the input to fft1pix is the
C	complex Fourier transform from FFT1.  The output can be an amplitude
C	picture  and/or a phase picture of the transform.  Those can then be
C	modified to the users needs and re-inputed to fft1pix in the reverse
C	mode.   The  Fourier transform is then modified in accordance to the
C	changes made to the amplitude and/or phase pictures.
C
C  WRITTEN BY:	 		JOHN ADDINGTON
C  COGNIZANT PROGRAMMER: 	FLORANCE MOSS
C  REVISION: 			04 JAN 1993
C  CONVERTED TO VAX BY:  	F. F. Moss,    20 JAN 1984
C  CONVERTED TO VICAR2 BY:  	D. F. Stanfill,   JUN 1987
C  PORTED BY:			J. F. McNeill, 04 JAN 1993
C
C  5  JAN 77   	...JDA...   INITIAL RELEASE
C  28 SEP 87   	...FFM...   DELETED AN ADDITIONAL XVOPEN CALL
C               	    ADDED XVCLOSE CALLS
C                           MODIFIED CODE TO HANDLE NUMBER OF SAMPLES CORRECTLY
C                           CHANGED PARAM "TYPE" DEFAULT TO AMPL
C                           MODIFIED THE COUNT NUMBER FOR PARAM "PLOT"
C                           UPDATED TEST PDF  
C  04 SEP 90 	...LWK...   fixed so 'PLOT' does not disable amplitude output, &
C		            XVOPEN(OUT..) is not called if no output specified.
C  04 JAN 93	...JFM...   Ported to UNIX and removed OUTCON and BINBCD calls.
C  08 FEB 93	...JFM...   Compilation warnings avoided by revision of COMMON
C			    block declarations in subroutines SAVAGE, KEEPIT,
C			    and AVGIT.
C
      IMPLICIT INTEGER (A-Z)

      BYTE BUF(2048),PBUF(2048)
      INTEGER IPLOT(20), CNT

      REAL DC,GAIN,A,SCALE,AREF,TOP,MAX,DIF,ERR,AR,CON
      REAL RC,AIC,THETA,FREQNM,MIN,PSCALE
      REAL AA(2048),AAVG(512),KEPBUF(512,10),P(512,2)

      LOGICAL REVERS,LOG,CONJ,NORMAL,PLOT,FREQ,PHASE,AMPL,NOSIGN,ZERO
      LOGICAL XVPTST
      COMPLEX C(2048),CC(2048),CZERO

      CHARACTER*4  LWD
      CHARACTER*8  XHD,XHD1,XHD2,YHD
      CHARACTER*32 PR1
      CHARACTER*69 AMPBUF,PHSBUF
      CHARACTER*80 HD

      EQUIVALENCE (BUF(1),PBUF(1))
      EQUIVALENCE (C(1),CC(1))
      COMMON /C1/ AA,KEPBUF,AAVG,P

      INCLUDE 'fortport'
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     ** PARAMETERS **
C
C        FORWARD MODE  (DEFAULT)
C        AMPLITUDE PICTURE  (DEFAULT)
C     'LOG'         OUTPUT IS THE NATR'L LOGRITHM OF AMPLITUDE PICTURE.
C     'SPIKE',I     EACH LINE NORMALIZED TO I'TH PEAK (DEFAULT=DC SPIKE)
C     'NORMAL'      OUTPUT NORMALIZED TO FIRST LINE.
C     'PLOT',N,M... PAGE PLOT AMPLITUDES OF LINES N,M,... (MAX=20)
C     'FREQ'        FREQUENCY GIVEN AS X-AXIS OF PAGE PLOT.
C                                      (DEFAULT=TERM NUMBER)
C     'AVG',I       PAGE PLOT OF LINE IS THE AVERAGE OF THE PRECEEDING
C                                      I LINES.  (MAX=10)
C     'PSPIKE',I    PAGE PLOT NORMALIZED TO I'TH PEAK.
C     'CONJ'        COMPLEX CONJUGATE ALSO OUTPUTED.
C
C        PHASE PICTURE
C     'PHASE'       PHASE PICTURE OUTPUTED.
C     'NOSIGN'      SIGN OF PHASE ANGLE DISREGUARDED IN PHASE PICTURE.
C
C
C        REVERSE MODE
C     'REVERSE'     REVERSE MODE
C     'LOG'         OPTION USED IN FORWARD MODE
C     'SPIKE',I     OPTION USED IN FORWARD MODE
C     'NORMAL'      OPTION USED IN FORWARD MODE
C     'PLOT',N,M... PAGE PLOTS OF AMPLITUDES AFTER MODIFICATION.
C     'AVG',I       SAME AS IN FORWARD MODE.
C     'PSPIKE',I    SAME AS IN FORWARD MODE.
C     'FREQ'        SAME AS IN FORWARD MODE.
C     'CONJ'        NOT NORMALLY USED IN REVERSE MODE.  IF USED IT
C                       DENOTES THAT CONJUGATE IS MODIFIED DIFFERENTLY.
C     'DC',R        (DECIMAL)  DC LEVEL INCREASED BY R.
C     'GAIN',R      (DECIMAL)  GAIN INCREASED BY A FACTOR OF R.
C     'ZERO'        ZERO IN REFERENCE CAUSES COMPLEX TERM TO BE ZEROED
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C        DEFAULT PARAMETERS
      ZERO= .FALSE.
      CZERO= CMPLX(0.0,0.0)

C        OPEN INPUT DATA SET
      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
      CALL XVOPEN( IUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
      NS=NS*8
      CALL XVPCNT('INP', NI)
      CALL XVPCNT('OUT', NO)
C        GET PARAMETERS
      CALL XVP('DC', DC, CNT)
      CALL XVP('GAIN', GAIN, CNT)
      REVERS = XVPTST('REVERSE')
      LOG = XVPTST('LOG')
      CONJ = XVPTST('CONJ')
      CALL XVP('SPIKE',SPIKE,CNT)
      NORMAL = XVPTST('NORMAL')
      CALL XVP('PLOT', IPLOT, NPLOT)
      PLOT = NPLOT .NE. 0
      CALL XVP('AVG', NAVE, CNT)
      FREQ = XVPTST('FREQ')
      CALL XVP('PSPIKE', PSPIKE, CNT)
      PHASE = XVPTST('PHASE') .AND. NO.GT.0
      NOSIGN = XVPTST('NOSIGN')
      AMPL = XVPTST('AMPL') .AND. NO.GT.0
      ZERO = XVPTST('ZERO')

C     INITIALIZE STRINGS
      
      AMPBUF	= ' '
      PHSBUF	= ' '
      HD 	= ' '
      LWD	= ' '
      XHD1 	= '    TERM'
      XHD2 	= '    FREQ'
      YHD 	= '  AMP   '

      IF(NAVE.GT.10)  NAVE= 10
      DC= DC*NS
      XCODE= 0
      XHD = XHD1
      IF(.NOT.FREQ)  GO TO 110
      XCODE= 3  
      XHD = XHD2
110   CONTINUE
      NSF= NS/8
      FREQNM= 1./NSF
C
      IF(NI.GT.1 .AND. REVERS)   GO TO 600
C
C     ***** FORWARD MODE *****
C
      IF (PHASE .AND. NO.LE.1)  AMPL= .FALSE.
      NSA= NS
      NB= NSA/16+1
      IF (CONJ) NB = NSA/8
      NLO= NL
C
C        SET UP LABEL MODIFICATION BUFFERS
      PTR= 1
      IF(.NOT.AMPL)  GO TO 135
      IF(.NOT.LOG)  GO TO 120
      AMPBUF(PTR:PTR+3) = 'LOG '
      PTR=PTR+4
120   AMPBUF(PTR:PTR+9) = 'AMPLITUDE '
      PTR=PTR+10
      IF(.NOT.NORMAL)  GO TO 125
      AMPBUF(PTR:PTR+6) = 'NORMAL '
      PTR= PTR+7
125   IF(SPIKE.LE.0)  GO TO 135
      AMPBUF(PTR:PTR+5) = 'SPIKE '
      PTR= PTR+6
      WRITE (LWD,130) SPIKE
130   FORMAT (I4)
      AMPBUF(PTR:PTR+1) = LWD(3:4)
      PTR = PTR+2
135   IF(.NOT.PHASE)  GO TO 145
      PTR= 1
      PHSBUF(PTR:PTR+5) = 'PHASE '
      PTR= PTR+6
      IF(.NOT.NOSIGN)  GO TO 145
      PHSBUF(PTR:PTR+6) = 'NOSIGN '
      PTR = PTR+7
145   CONTINUE
C
C
C   RELABEL OUTPUT 
      PDS = 1
      IF (AMPL) THEN
	CALL XVUNIT( OUN, 'OUT', 1, ISTAT, ' ')
	CALL XVOPEN( OUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .	 'OP', 'WRITE', 'O_FORMAT', 'BYTE', 'U_FORMAT', 'BYTE',
     .	 'U_NL', NLO, 'U_NS', NB, ' ')
	CALL XLADD( OUN, 'HISTORY', 'PGM_LAB', AMPBUF, ISTAT, 'ULEN',
     .	 PTR, 'FORMAT', 'STRING', ' ')
	PDS = 2
      ENDIF
      IF (PHASE) THEN
	CALL XVUNIT( PUN, 'OUT', PDS, ISTAT, ' ')
	CALL XVOPEN( PUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .	 'OP', 'WRITE', 'O_FORMAT', 'BYTE', 'U_FORMAT', 'BYTE',
     .	 'U_NL', NLO, 'U_NS', NB, ' ')
	CALL XLADD( PUN, 'HISTORY', 'PGM_LAB', PHSBUF, ISTAT, 'ULEN',
     .	 PTR, 'FORMAT', 'STRING', ' ')
      ENDIF
      IF (AMPL .OR. PHASE) THEN
        WRITE (PR1,160) NLO, NB
160     FORMAT ('** OUTPUT  NL=',I4,'   NS=',I4,' **')
	CALL XVMESSAGE(PR1,' ')
	CALL XVMESSAGE(' ',' ')
      ENDIF
      LINE2= SL
      SLN= SL-1
      SLN2= SL
      SLN3= SL+1
      NPPTS= NB
      IF(NPPTS.GT.512)  NPPTS= 512
      IF(PLOT) CALL SAVAGE(N,NAVE,NSR,NPPTS)
      KPLOT= 1
      N= 0
C
C
C        MAIN LOOP (FORWARD)
C
      DO 500 II=1,NLO
      CALL XVREAD( IUN, C, ISTAT, ' ')
      LINE2= 0
C
C        COMPUTE AMPLITUDES
C
      IF(.NOT.AMPL .AND. .NOT.PLOT)  GO TO 1200
      DO 210 J=1,NB
210   AA(J)= CABS(C(J))
C
C        MAKE PAGE PLOT (IF DESIRED)
C
      IF(.NOT.PLOT)  GO TO 250
      CALL KEEPIT(N,NAVE,NSR,NHOLD)
      IF(IPLOT(KPLOT).NE.II+SL-1)  GO TO 250
      CALL AVGIT(NHOLD,NPPTS)
      DO 215 J=1,NPPTS
      P(J,1)= J
      IF(FREQ)  P(J,1)= (J-1)*FREQNM
215   P(J,2)= AAVG(J)
      NUMBER = IPLOT(KPLOT)
      WRITE (HD, 225) NUMBER, NAVE
225   FORMAT ('1-D FOURIER TRANSFORM    LINE ',I4,'    AVERAGE',I2)
      CALL PPLOT(P,512, NPPTS,PSPIKE,HD,XHD,YHD,XCODE,-6)
      KPLOT= KPLOT+1
      IF(KPLOT.GT.NPLOT)  PLOT= .FALSE.
C
250   IF(NO.EQ.0)  GO TO 500
      IF(NORMAL .AND. II.NE.1)  GO TO 290
      TOP= AA(1)
      IF(SPIKE.EQ.0)  GO TO 280
C
C        FIND DESIRED SPIKE
C
      TOP= 9.9E30
      DO 270 K=1,SPIKE
        MAX= 0.0
        DO 260 J=1,NB
          IF(AA(J).LE.MAX .OR. AA(J).GE.TOP)  GO TO 260
          MAX= AA(J)
260     CONTINUE
270   TOP= MAX
C
280   IF(LOG .AND. TOP.GT.0.)  TOP= ALOG(TOP)
      IF(TOP.LE.0.)  TOP= 1.
      SCALE= 255./TOP
C
C        SCALE AMPLITUDES
C
290   DO 300 J=1,NB
      A= AA(J)
      IF(LOG .AND. A.GT.0.)  A= ALOG(A)
      IA= A*SCALE + 0.5
      IF(IA.GT.255)  IA= 255
      IF(IA.LT.0)  IA= 0
300   BUF(J) = INT2BYTE(IA)
C
C
      CALL XVWRIT( OUN, BUF, ISTAT, ' ')
C
C        COMPUTE PHASE ANGLES
C
1200  IF(.NOT.PHASE)  GO TO 500
      DO 1210 J=1,NB
      CALL PHASER(C(J),RC,AIC,AA(J))
      IF(NOSIGN)  AA(J)= ABS(AA(J))
1210  CONTINUE
C
C        COMPUTE SCALE FACTOR
C
      MAX= -9.9E30
      MIN= 9.9E30
      DO 1260 J=1,NB
      A= AA(J)
      IF(A.GT.MAX)  MAX= A
      IF(A.LT.MIN)  MIN= A
1260  CONTINUE
      IF(MAX.NE.MIN)  GO TO 1261
      PSCALE=0.
      GO TO 1262
1261  PSCALE= 255./(MAX-MIN)
1262  CONTINUE
C
C        SCALE PHASE ANGLES
C
      DO 1300 J=1,NB
      IA= PSCALE*(AA(J)-MIN) + 0.5
1300  PBUF(J) = INT2BYTE(IA)
C
      CALL XVWRIT( PUN, PBUF, ISTAT, ' ')
C
C
500   CONTINUE
      CALL XVCLOSE (IUN,ISTAT,' ')
      IF (AMPL)  CALL XVCLOSE (OUN,ISTAT,' ')
      IF (PHASE) CALL XVCLOSE (PUN,ISTAT,' ')
C
      RETURN
C
C
C     ***** REVERSE MODE *****
C
600   CONTINUE
C
      NSA= NS
      NLI= NL
      NB= NSA/8
      NBR= NSA/16+1
      IF(CONJ)  NBR= NB
      NBT= NBR*2
C
C        OPEN DATA SETS
C
      PDS= 3
      IF (NI.GT.1)  AMPL= .TRUE.
      IF (PHASE .AND. NI.LE.2) THEN
	AMPL= .FALSE.
	PDS= 2
      ENDIF
      IF (AMPL) THEN
	CALL XVUNIT( AUN, 'INP', 2, ISTAT, ' ')
	CALL XVOPEN( AUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')
      ENDIF
      IF (PHASE) THEN
	CALL XVUNIT( PUN, 'INP', PDS, ISTAT, ' ')
	CALL XVOPEN( PUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')
      ENDIF
      CALL XVUNIT( OUN, 'OUT', 1, ISTAT, ' ')
      CALL XVOPEN( OUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     . 'OP', 'WRITE', 'U_FORMAT','COMP','O_FORMAT','COMP',' ')
      LINE2= SL
      LINE3= SL
      LINEP= SL
      SLN= SL-1
      NPPTS= NBR
      IF(NPPTS.GT.512)  NPPTS= 512
      KPLOT= 1
      N= 0
C
C
C        MAIN LOOP (REVERSE)
C
      DO 880 II=1,NLI
      CALL XVREAD( IUN, C, ISTAT, ' ')
      IF (AMPL) CALL XVREAD( AUN, BUF ,ISTAT, ' ')
      LINE2= 0
      LINE3= 0
C
      IF(.NOT.AMPL)  GO TO 1600
C
C        COMPUTE AMPLITUDES
C
615   DO 620 J=1,NB
620   AA(J)= CABS(C(J))
C
C        FIND DESIRED SPIKE
C
      IF(NORMAL .AND. II.NE.1)  GO TO 690
      TOP= AA(1)
      IF(SPIKE.EQ.0)  GO TO 680
      TOP= 9.9E30
      DO 640 K=1,SPIKE
      MAX= 0.0
      DO 630 J=1,NBR
      IF(AA(J).LE.MAX .OR. AA(J).GE.TOP)  GO TO 630
      MAX= AA(J)
630   CONTINUE
640   TOP= MAX
C
C        COMPUTE ERROR EXPECTED,  SET DC LEVEL
C
680   IF(LOG .AND. TOP.GT.0.)  TOP= ALOG(TOP)
      SCALE= TOP/255.
      ERR= 0.5*SCALE
690   CC(1)= C(1)
      IF(DC.EQ.0.0)  GO TO 695
      CALL PHASER(C(1),RC,AIC,THETA)
      AR= AA(1)+DC
      RC= AR*COS(THETA)
      AIC= AR*SIN(THETA)
      CC(1)= CMPLX(RC,AIC)
695   CONTINUE
C
C        MODIFY AMPLITUDES OF COMPLEX PICTURE
C
      DO 800 J=2,NBR
      CC(J)= C(J)*GAIN
      IF(.NOT.CONJ)  CC(NBT-J)= C(NBT-J)*GAIN
      IA = BYTE2INT(BUF(J))
      IF(IA.GE.255)  GO TO 800
      IF(.NOT.ZERO .OR. IA.GT.0)  GO TO 700
      CC(J)= CZERO
      IF(.NOT.CONJ .AND. J.NE.NBR)  CC(NBT-J)= CZERO
      GO TO 800
700   AREF= IA*SCALE
      A= AA(J)
      IF(LOG .AND. A.GT.0.)  A= ALOG(A)
      DIF= ABS(A-AREF)
      IF(DIF.LE.ERR)  GO TO 800
      IF(A .LT. 1.E-25)  GO TO 800
      AREF= AREF/A
750   CC(J)= CC(J)*AREF
      IF(CONJ .OR. J.EQ.NBR)  GO TO 800
      CC(NBT-J)= CC(NBT-J)*AREF
800   CONTINUE
C
C        MAKE PAGE PLOT (IF DESIRED)
C
      IF(.NOT.PLOT)  GO TO 850
      DO 810 J=1,NPPTS
810   AA(J)= CABS(CC(J))
      CALL KEEPIT(N,NAVE,NSR,NHOLD)
      IF(IPLOT(KPLOT).NE.II+SL-1)  GO TO 875
      CALL AVGIT(NHOLD,NPPTS)
      DO 815 J=1,NPPTS
      P(J,1)= J
      IF(FREQ)  P(J,1)= (J-1)*FREQNM
815   P(J,2)= AAVG(J)
      NUMBER = IPLOT(KPLOT)
      WRITE (HD,825) NUMBER, NAVE
825   FORMAT ('1-D FOURIER TRANSFORM    LINE ',I4,'    AVERAGE',I2)
      CALL PPLOT(P,512, NPPTS,PSPIKE,HD,XHD,YHD,XCODE,-6)
      KPLOT= KPLOT+1
      IF(KPLOT.GT.NPLOT)  PLOT= .FALSE.
850   CONTINUE
C
C        COMPUTE PHASE ANGLES
C
1600  IF(.NOT.PHASE)  GO TO 875
      CALL XVREAD( PUN, PBUF, ISTAT, ' ')
      LINEP= 0
      DO 1610 J=1,NB
      CALL PHASER(C(J),RC,AIC,AA(J))
      IF(NOSIGN)  AA(J)= ABS(AA(J))
1610  CONTINUE
C
C        COMPUTE SCALE FACTOR
C
      MAX= -9.9E30
      MIN= 9.9E30
      DO 1660 J=1,NB
      A= AA(J)
      IF(A.GT.MAX)  MAX= A
      IF(A.LT.MIN)  MIN= A
1660  CONTINUE
      PSCALE= (MAX-MIN)/255.
      ERR= 0.5*PSCALE
C
C        MODIFY PHASE ANGLES OF COMPLEX PICTURE
C
      DO 1700 J=2,NBR
      IA = BYTE2INT(PBUF(J))
      AREF= IA*PSCALE+MIN
      A= AA(J)
      DIF= ABS(A-AREF)
      IF(DIF.LT.ERR)  GO TO 1700
      CON= TAN(AREF)
      CON= CON*CON
      A= CABS(CC(J))
      RC= A/SQRT(1.+CON)
      CON= 1./CON
      AIC= A/SQRT(1.+CON)
      CC(J)= CMPLX(RC,AIC)
      IF(CONJ .OR. J.EQ.NBR)  GO TO 1700
      CC(NBT-J)= CONJG(CC(J))
1700  CONTINUE
C
C
875   CALL XVWRIT( OUN, CC, ISTAT, ' ')
880   CONTINUE
      CALL XVCLOSE (IUN,ISTAT,' ')
      CALL XVCLOSE (OUN,ISTAT,' ')
      IF (AMPL)  CALL XVCLOSE(AUN,ISTAT,' ')
      IF (PHASE) CALL XVCLOSE(PUN,ISTAT,' ')
C
      RETURN
      END

      SUBROUTINE PHASER(C,RE,AIMG,THETA)
      COMPLEX C
C
      RE= REAL(C)
      AIMG= AIMAG(C)
      IF(RE.EQ.0.0)  GO TO 10
      THETA= ATAN2(AIMG,RE)
      GO TO 30
10    IF(AIMG.EQ.0.0)  GO TO 20
      THETA= SIGN(1.570796,AIMG)
      GO TO 30
20    THETA= 0.0
30    RETURN
      END

      SUBROUTINE PPLOT(P,N,NPTS,SAT,TITLE,XAXIS,YAXIS,XCODE,YCODE)
C
      IMPLICIT INTEGER (A-Z)

      CHARACTER*1 	ONE,ZERO,BLANK
      CHARACTER*4	NUM
      CHARACTER*8 	XAXIS,YAXIS
      CHARACTER*32 	HMAX 
      CHARACTER*80   	TITLE
      CHARACTER*101     RASTER
      CHARACTER*132 	LINE

      REAL P(1),RNUM,MAX,ZMAX,PCENT,LARGE/9.9E 15/
C
C        CODE .LT. 0    FLOATING POINT  9-CODE FRACTION DIGITS
C        CODE .EQ. 0    INTEGER
C        CODE .GT. 0    FIXED POINT  CODE FRACTION DIGITS

C     INITIALIZE CHARACTER STRINGS

      ONE	= '1'
      ZERO 	= '0'
      BLANK 	= ' '
      RASTER	= ' '    
      LINE 	= ' '
      HMAX	= ' '

C     PAGE ADVANCE
      CALL QPRINT(ONE,1)	

      CALL XVMESSAGE(TITLE,' ')
      CALL XVMESSAGE(' ',' ')

      ZMAX= LARGE
      DO 110 K=1,SAT
      MAX= 0.0
      DO 100 J=1,NPTS
      IF(P(N+J).LE.MAX .OR. P(N+J).GE.ZMAX)  GO TO 100
      MAX= P(N+J)
100   CONTINUE
      ZMAX= MAX
110   CONTINUE
      IF(MAX.LT.1.0E-20)  MAX= 1.
C
C
      IF(YCODE.LT.0) THEN
	WRITE (HMAX, 115) MAX
115	FORMAT (9X,E8.3,' = 100 PERCENT')
      ENDIF
 
      IF(YCODE.EQ.0) THEN
	WRITE (HMAX, 120) MAX
120	FORMAT (9X,I8,' = 100 PERCENT')
      ENDIF
 
      IF(YCODE.GT.0) THEN
	WRITE (HMAX, 125) MAX
125	FORMAT (9X,F8.3,' = 100 PERCENT')
      ENDIF
      CALL XVMESSAGE(HMAX,' ')
      CALL XVMESSAGE(' ',' ')

      LINE(1:8)  	= XAXIS
      LINE(11:18) 	= YAXIS 
      LINE(22:28)	= 'PERCENT'
      LINE(31:31)	= ZERO
      PTR = 38
      DO 200 J=10,100,10

      WRITE (NUM, 150) J
150   FORMAT (I4) 
      LINE(PTR:PTR+3) = NUM
200   PTR= PTR+10

      CALL XVMESSAGE(LINE,' ')
C
C        PLOT
C

C     Reinitialize string

      MAX = 1./MAX

      DO 500 J = 1,NPTS

      LINE = ' '

      PCENT = 100.*P(N+J)*MAX +0.0005
      QCENT = PCENT+0.5
      IF(QCENT.GT.100) 	QCENT=100
      IF(QCENT.LT.0) 	QCENT=0

      IF(XCODE.LT.0)  THEN
	WRITE (LINE(1:8), 210) P(J)
210	FORMAT (E8.3)
      ENDIF

      IF(XCODE.EQ.0)  THEN
	PNUM = P(J)
	WRITE (LINE(1:8), 215) PNUM
215	FORMAT (I8)
      ENDIF

      IF(XCODE.GT.0)  THEN
	WRITE (LINE(1:8), 220) P(J)
220	FORMAT (F8.3)
      ENDIF

      IF(YCODE.LT.0)  THEN
	RNUM = P(N+J)
	WRITE (LINE(11:20), 230) RNUM
230	FORMAT (E8.3)
      ENDIF

      IF(YCODE.EQ.0)  THEN
	PNUM = P(N+J)
	WRITE (LINE(11:20), 235) PNUM
235	FORMAT (I9) 
      ENDIF

      IF(YCODE.GT.0)  THEN
	WRITE (LINE(11:20), 240) P(N+J)
240	FORMAT (F8.3)
      ENDIF

      WRITE (LINE(21:29), 250) PCENT
250   FORMAT (F8.3)

      PTR = 31
      DO 400  K=1,11
      LINE(PTR:PTR) = '+'
      PTR=PTR+10
400   CONTINUE
	
      PTR = 31
      DO 450  K=1,QCENT+1
      LINE(PTR:PTR) = '*'
450   PTR=PTR+1

      CALL XVMESSAGE(LINE,' ')

500   CONTINUE
C
      RETURN
      END

      SUBROUTINE SAVAGE(N,NAVE,NSR,NSI)
      IMPLICIT INTEGER (A-Z)
          
      N = 0
      NSR = NSI * 4
      RETURN
      END

      SUBROUTINE KEEPIT(N,NAVE,NSR,NHOLD)
      IMPLICIT INTEGER (A-Z)
      REAL AA(2048),AAVG(512),KEPBUF(512,10),P(512,2)
      COMMON /C1/ AA,KEPBUF,AAVG,P

      N= N+1
      NV= MOD(N,NAVE)
      IF(NV.EQ.0)  NV= NAVE
      NHOLD= NV
      IF(N.GE.NAVE)  NHOLD= NAVE
      DO 50 K=1,NSR
50    KEPBUF(K,NV) = AA(K) 
      RETURN
      END
C
      SUBROUTINE AVGIT(NHOLD,NSI)
      IMPLICIT INTEGER (A-Z)
      REAL AA(2048),AAVG(512),KEPBUF(512,10),P(512,2),RHOLD,TOT
      COMMON /C1/ AA,KEPBUF,AAVG,P

      RHOLD= NHOLD
      RHOLD= 1./RHOLD
      DO 175 J=1,NSI
      TOT= 0.0
      DO 150 I=1,NHOLD
150   TOT= TOT + KEPBUF(J,I)
      AAVG(J)= TOT*RHOLD
175   CONTINUE
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fft1pix.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fft1pix

   To Create the build file give the command:

		$ vimake fft1pix			(VMS)
   or
		% vimake fft1pix			(Unix)


************************************************************************/


#define PROGRAM	fft1pix
#define R2LIB

#define MODULE_LIST fft1pix.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fft1pix.pdf
process help=*
PARM INP 	TYPE=STRING   			COUNT=(1:3)	
PARM OUT	TYPE=STRING     		COUNT=(0:2)  	DEFAULT=--
PARM TYPE   	TYPE=KEYWORD VALID=(AMPL,PHASE) COUNT=(0:1) 	DEFAULT=AMPL
PARM LOG    	TYPE=KEYWORD VALID="LOG" 	COUNT=(0:1) 	DEFAULT=--
PARM SPIKE      TYPE=INTEGER                    		DEFAULT=0
PARM NORMAL 	TYPE=KEYWORD VALID="NORMAL" 	COUNT=(0:1)  	DEFAULT=--
PARM NOSIGN  	TYPE=KEYWORD VALID="NOSIGN" 	COUNT=(0:1)  	DEFAULT=--
PARM CONJ  	TYPE=KEYWORD VALID="CONJ" 	COUNT=(0:1)  	DEFAULT=--
PARM PLOT       TYPE=INTEGER    		COUNT=(0:20)    DEFAULT=--
PARM AVG        TYPE=INTEGER                    		DEFAULT=1
PARM PSPIKE     TYPE=INTEGER                    		DEFAULT=2
PARM FREQ  	TYPE=KEYWORD VALID="FREQ" 	COUNT=(0:1)  	DEFAULT=--
PARM REVERSE  	TYPE=KEYWORD VALID="REVERSE" 	COUNT=(0:1)  	DEFAULT=--
PARM DC         TYPE=REAL                       		DEFAULT=0.0
PARM GAIN       TYPE=REAL                       		DEFAULT=1.0
END-PROC
.TITLE
 FFT1PIX
.HELP
 FFT1PIX  is  a  VICAR application program which works in conjunction
 with  FFT1  to  allow  the user to display and/or modify the Fourier
 transform  using  existing VICAR programs.  The procedure is similar
 to  that  used  in  FFT2,  FFTPIC,  and  TPIKMASK  but  without  the
 restriction  that  terms  of  the transform be either set to zero or
 left  unchanged.   In  the forward mode, the input to fft1pix is the
 complex Fourier transform from FFT1.  The output can be an amplitude
 picture  and/or a phase picture of the transform.  Those can then be
 modified to the users needs and re-inputed to fft1pix in the reverse
 mode.   The  Fourier transform is then modified in accordance to the
 changes made to the amplitude and/or phase pictures.
.page
 Let

 z(k) = a(k) + ib(k) 
 
 be  the  k'th  term  of  the  Fourier  series  of a given line.  The
 amplitude   of   this   term   is   |z|   and  the  phase  angle  is
 atan(b(k)/a(k)).   Then  the  DN of the k'th sample in the amplitude
 picture equals is p*|z|, where p is a scaling factor.

 If  the log option is used, DN = p*LOG(|z|).  p is determined by the
 normalization  options  used.  Similarly the DN of the phase picture
 is given by:

 DN = q * (phase angle) + s

 where q and s are scaling factors.  If NOSIGN is requested,

 DN = q * |phase angle| + s.

 If  requested, then the amplitude and phase pictures are written out
 on  the primary and secondary output data sets.  In addition, a page
 plot of the amplitudes may be printed on the line printer, providing
 a  means  of accurately locating noise spikes in the transform.  The
 user  may  now  modify the amplitude or phase picture using existing
 VICAR programs.  For example, Noise spikes may be QSAR'ed out of the
 amplitude picture.

 The  modified  amplitude  and  phase  pictures are now re-input into
 fft1pix  in  the  reverse  mode.   The  complex*8 transform data set
 remains  as  the  primary  input, the modified amplitude picture the
 secondary input, and the modified phase picture the third input.  If
 the amplitude or phase picture was not modified, then it need not be
 re-input  to  fft1pix.   Each  term  of  the  original  transform is
 compared  to  the  corresponding  term of the modified amplitude and
 phase  pictures; if the amplitude differs, the term is recomputed to
 match  that of the amplitude picture with phase angle preserved.  If
 the  phase  angle  differs  from  the  phase  picture,  the  term is
 recomputed  to match the new phase angle, amplitude preserved.  Thus
 the  user  has complete control over each term of the transform.  If
 CONJ  is not specified in the reverse mode, the complex conjugate of
 a  term  is  automatically  modified  in the identical manner to its
 conjugate  term.   The output is a complex*8 data set containing the
 Fourier  transform  after  modifications  have  been made to it.  If
 requested,  a page plot is made of the transform after modification.

 The modified transform is now ready to be input into FFT1 in inverse
 mode to produce the resulting image.

 WRITTEN BY : 			JOHN ADDINGTON
 COGNIZANT PROGRAMMER : 	FLORANCE MOSS
 REVISION : 			04 JAN 1993
 CONVERTED TO VAX    BY:  	F. F. Moss,    20 JAN 1984
 CONVERTED TO VICAR2 BY:  	D. F. Stanfill,   JUN 1987
 PORTED BY:			J. F. McNeill, 04 JAN 1993

				J. F. McNeill, 22 JUL 1994
				Corrected printing of integer
				within PPLOT subroutine.
				(FR 85096)

 TIMING : Average CPU time used on VAX 8650 = 00:00:02.41.
	  Computed from TSTFFT1PIX.PDF for ported version 04 JAN 1993. 

.LEVEL1
.VARIABLE INP
1-3 input data set(s)
.VARIABLE OUT
0-2 Output data set(s)
.VARIABLE TYPE
Type of output picture
(AMPL or PHASE)
.VARIABLE LOG
Output is a logarithm
of amplitude picture
.VARIABLE SPIKE
Scale the amplitude picture to 
this spike
.VARIABLE NORMAL
Normalize the amplitude picture 
to the spike
.VARIABLE NOSIGN
Suppress the sign of the output 
phase picture
.VARIABLE CONJ
Outputs include the complex 
conjugate
.VARIABLE PLOT
Produce an amplitude page plot
.VARIABLE AVG
Page plot lines are averaged 
.VARIABLE PSPIKE
Normalize the page plot to the spike
.VARIABLE FREQ
Use 'FREQUENCY' as page plot x-axis 
.VARIABLE REVERSE
Reverse mode
.VARIABLE DC
Increase DC term of each line 
.VARIABLE GAIN
REAL -  a gain factor is applied to each line
.LEVEL2
.VARIABLE INP
 In the forward mode, only one input is allowed.

 In the reverse mode, up to three input files are allowed.  The first
 input  is  the  picture on which to operate, the second input is the
 modified  amplitude picture, and the third input is (optionally) the
 modified phase picture.
.VARIABLE OUT
 Up to two outputs are allowed.  No output is needed if only the page
 plot is desired.

 If  only one output is provided, the type of output is controlled by
 the TYPE parameter.
.VARIABLE TYPE
 If  two  output  files  are given, TYPE is ignored, and an amplitude
 picture  is  written to the first output, and a phase picture to the
 second. 

 If  only  one  output  is  specified, then TYPE controls the type of
 output;  AMPL  specifies  an amplitude output, and PHASE specifies a
 phase output.
.VARIABLE LOG
 If  LOG  is  specified,  then the logarithm of the amplitude picture
 output.   This  is  recommended  if  the  amplitude picture is to be
 MASK'ed or FOTO'ed.
.VARIABLE SPIKE

 If  SPIKE  is  given,  then  each  line  of the amplitude picture is
 normalized  to  the  SPIKE'th peak for that line.  If NORMAL is also
 specified,  then each line is normalized to the SPIKE'th peak of the
 first line.  (default = dc term)

.VARIABLE NORMAL
 AMPLITUDE PICTURE NORMALIZED TO SPECIFIED SPIKE OF FIRST LINE.
.VARIABLE NOSIGN
 THE SIGN OF THE PHASE ANGLE IS IGNORED IN THE OUTPUT PHASE 
 PICTURE.
.VARIABLE CONJ
 THE COMPLEX CONJUGATE OF THE TRANSFORM IS INCLUDED IN BOTH
 THE AMPLITUDE AND THE PHASE PICTURES.
.VARIABLE PLOT
 PLOT,L1,L2, ------ AN AMPLITUDE PLOT IS PRODUCED OF LINES L1,
 L2,... ETC. (MAX OF 20)  THIS IS SIMILAR TO THE OUTPUT 
 OF POWER, ONLY LINE BY LINE.
.VARIABLE AVG
 AVG,I ----- THE I LINES PRECEDING LINE L ARE AVERAGED IN THE
 PAGE PLOT OF LINE L. 
.VARIABLE PSPIKE
 PSPIKE,I ----- PAGE PLOT NORMALIZED TO I'TH PEAK (DEFAULLT = 2)
.VARIABLE FREQ
 FREQUENCY GIVEN AS X - AXIS OF PAGE PLOT. (DEFAULT = TERM 
 NUMBER OF FOURIER COEF.)
.VARIABLE REVERSE
 REVERSE MODE. THE PRIMARY INPUT IS THE COMPLEX*8 DATA SET FROM
 FFT1. THE SECONDARY AND THIRD INPUT DATA SETS ARE REFERENCE 
 AMPLITUDE AND/OR PHASE PICTURES BY WHICH THE TRANSFORM WILL
 BE MODIFIED AND WRITTEN ON THE OUTPUT DATA SET.
 (IN THE REVERSE MODE, IF ANY OF THE PARMS "LOG", "SPIKE,I", "NORMAL",
  "PLOT,L1,L2", "AVG,I", "PSPIKE,I", "FREQ", IS SPECIFIED, 
  IT DENOTES THAT THE SPECIFIED PARM WAS USED IN THE FORWARD
  MODE. THE PARM "CONJ" IS NOT NORMALLY USED IN THE REVERSE 
  MODE. IF USED, IT DENOTES THAT THE COMPLEX CONJUGATE OF 
  THE TRANSFORM IS TO BE MODIFIED INDEPENDENTLY .  THIS COULD
  PRODUCE UNPREDICTABLE RESULTS.)
.VARIABLE DC
 DC,R ----- THE DC TERM OF EACH LINE WILL BE INCREASED BY AN
 AMOUNT R. THIS WILL CAUSE THE AVERAGE GRAY LEVEL OF THE 
 PICTURE TO BE INCREASED BY R AFTER THE INVERSE TRANSFORM.
.VARIABLE GAIN
 GAIN,R ----- A GAIN FACTOR OF R IS APPLIED TO EACH LINE.
 THIS WILL HAVE THE EFFECT OF A CONTRAST STRETCH AFTER
 THE INVERSE TRANSFORM.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfft1pix.pdf
procedure
refgbl $echo
refgbl $autousage 
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

! THE INPUT OF FFT1PIX IS THE OUTPUT OF FFT1.
! *** PLEASE NOTE THAT FFT1 IS NOT CURRENTLY AVAILABLE ON THE UNIX 
!     SYSTEM AND IS REQUIRED FOR THIS TEST SCRIPT.  ONCE FFT1 IS
!     AVAILABLE, THIS COMMENT WARNING SHOULD BE REMOVED.	   ***
! Jun 2010 / lwk:  replaced FFT1 with FFT11;  this gives sensible output,
! but since I cannot be sure that FFT11 has exactly the same FFT format
! conventions as the old FFT1, and the documentation for FFT1PIX does not
! contain numerical examples, I cannot be positive that the program behaves
! exactly as intended.  Therefore, these notes are retained in the test pdf.
! (Caveat emptor.)

gen a 16 16
fft11 a b 

fft1pix b c 
list c

!
!TEST THE PARM LOG
fft1pix b c 'log
list c

!
!TEST THE PARM SPIKE
fft1pix b c spike=2
list c

!
!TEST THE PARM NORMAL
fft1pix b c 'normal
list c

!
! TEST THE PARM PHASE
fft1pix b c 'phase
list c

!
! TEST THE PARM PLOT
fft1pix b plot=(1,3)

!
! TEST THE PARM AVG
fft1pix b plot=2 avg=2

!
! TEST THE PARM FREQ
fft1pix b plot=5 'freq 

!
! TEST THE REVERSE MODE
fft1pix b c 'log
fft1pix (b,c) d 'reve 'log
list d

!
! TEST THE PARM DC AND GAIN
fft1pix (b,c) d 'reve 'log dc=10. gain=2. 
list d

end-proc
$ Return
$!#############################################################################
