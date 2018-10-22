
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


