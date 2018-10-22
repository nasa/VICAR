      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 23 SEPT 1983
C     REVISION 1 BY ASM, FEBRUARY 7 1984 - SPEED ENHANCEMENTS
C         1) REPLACED CONVERSION OF ALL INPUT DATA TO REAL-TYPE WITH
C	     SEPARATE TABULATION ROUTINES FOR EACH TYPE
C         2) ADDED LOOK-UP TABLE FOR BYTE-IMAGE PROCESSING
c  84-10-9  ...LWK...  converted to Vicar2, check for rounding error in sdev.
c  84-10-11 ...LWK...  for byte data, compute stats from histogram.
c  84-12-13 ...LWK...  revised treatment of BINS, LIMITS.
c  85-4-17  ...REA...  fixed bug in LINC & AREA parameters
c  85-4-17  ...LWK...  revised processing of REAL*4 data
C  86-11-11 ...REA...  modify formatting, hist collection routines, add
C		       output parameters MEAN, SIGMA
C  87-1-12  ...REA...  add EXCLUDE, SCREEN parameters
C  87-2-4   ...REA...  add SPLOT, PPLOT and TITLE parameters
C  87-8-4   ...REA...  fix bug in x-axis scaling algorithm
C  87-10-27 ...REA...  add 3-D file capability
C  89-8-23  ...REA...  add BAR graph parameter
C  90-2-22  ...REA...  add output parameters MIN, MAX
C  90-10-5  ...REA...  add laserprinter capabilities
C  91-4-8   ...REA...  convert to UNIX
C  92-8-6   ...REA...  add WINDOW and WBAR options
C  96-10-25 ...REA...  remove Regis options
C  97-8-20  ...REA...  add BYCHAN option
C  00-7-12  ...REA...  adjust print-out column widths for DN and population
C  01-2-16  ...REA...  fix min/max bug when both regular and pen plots;
C                      consolidate statistics logic.
C
C**********************************************************************
      SUBROUTINE MAIN44

C**********************************************************************
C               DECLARATIONS AND INITIALIZATION
C**********************************************************************

      EXTERNAL WORK
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      INTEGER CNT,DEF,BATCH
      INTEGER SL,SS,SLI,SSI,SINC
      REAL*4 BOUNDS(2)
      COMMON/PAUSE/ QPAUSE
      LOGICAL*4 NOCUM,QPAUSE,XVPTST,QEXCLUDE,QPPLOT,QBYCHAN
      CHARACTER*80 MSG
      CHARACTER*8 FMT, FMT1
      CHARACTER*3 ORG


      call xvmessage( '*** p3/HIST version Sep 9 2015 ***',' ')

C********************************************************************
C			OPEN INPUT FILE
C********************************************************************

      CALL XVUNIT(INUN,'INP',1,ISTAT,0)
      CALL XVOPEN(INUN,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',0)
      CALL XVGET(INUN,ISTAT,'ORG',ORG,0)
      IF (ORG.EQ.'BIP') THEN
	  CALL XVMESSAGE(' HIST does not accept BIP input',' ')
	  CALL ABEND
      ENDIF
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      SL = SL-1
      SS = SS-1
      CALL XVBANDS(ISB,NB,NBI)
      IEB = ISB+NB-1
C**********************************************************************
C                       PROCESS PARAMETERS
C**********************************************************************

      QBYCHAN = XVPTST('BYCHAN')
      CALL XVGET(INUN,ISTAT,'FORMAT',FMT,0)
      CALL XVPARM('FORMAT',FMT1,CNT,DEF,1) 
      IF (DEF.EQ.0) FMT = FMT1

      IF (FMT.EQ.'BYTE') THEN
	 IFORM=1
         CALL XVCLOSE(INUN,ISTAT,0)
         CALL XVOPEN(INUN,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +               'U_FORMAT','HALF',0)
      ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	IFORM=2
      ELSEIF (FMT.EQ.'FULL') THEN
	IFORM=4
      ELSEIF (FMT.EQ.'REAL') THEN
	IFORM=7
      ELSE
	CALL XVMESSAGE(' ** FORMAT '//FMT//' NOT SUPPORTED **',' ')
	CALL ABEND
      ENDIF

C ----- CHECK FOR NEW SINC, LINC, INC, OR SPIKE SPECIFICATION

      CALL XVPARM('LINC',LINC,CNT,DEF,1)
      CALL XVPARM('SINC',SINC,CNT,DEF,1)
      CALL XVPARM('INC',INC,CNT,DEF,1)
      IF (DEF.NE.1) THEN
	LINC = INC
	SINC = INC
      ENDIF
C
C DETERMINE PRINT MODE
C
      MODE = 0
      IF (XVPTST('NOHIST')) MODE=-1
      IF (XVPTST('WIDE'))   MODE=1
      IF (XVPTST('SCREEN')) MODE=2
      IF (XVPTST('WINDOW')) MODE=6
      IF (XVPTST('WBAR'))   MODE=7
      QPPLOT = (XVPTST('PPLOT'))
      IF (QPPLOT) THEN	
	CALL XVPARM('MODE',FMT,ICNT,IDEF,1)
	IF (IDEF.EQ.1) MODE=4
      END IF
      IF (MODE.EQ.1) NOCUM = XVPTST('NOCUM')
      IF (MODE.EQ.2 .AND. IFORM.NE.1) THEN
	 CALL XVMESSAGE(
     +	 ' Screen oriented histogram not supported for non-byte data',
     +   ' ')
	 CALL ABEND
      ENDIF
C
C CHECK FOR SPIKES
C
      CALL XVPARM('SPIKES',ISPIKE,CNT,DEF,1)
      IF (ISPIKE.LT.1) ISPIKE = 1
      IF (ISPIKE.GT.9) ISPIKE = 9
C
C NUMBER OF BINS:
C
      CALL XVPARM('BINS',IBINS,CNT,DEF,1)
      IF (IBINS.LT.2) IBINS=2

C NUMBER OF HISTOGRAMS IN DATA SET:

      CALL XVPARM('AREA',IAREA,CNT,DEF,600)
      IF (DEF.EQ.1) THEN
	IAREA(3) = NL
	IAREA(4) = NS
      ENDIF
      NUMARS = CNT/4

C PIXEL RANGE:

      CALL XVPARM('LIMITS',BOUNDS,CNT,DEF,2)
      IF (DEF.EQ.1 .AND. IFORM.NE.1) THEN
          BOUNDS(1) = -32768.
          BOUNDS(2) = 32767.
      END IF
      BOUNDL = BOUNDS(1)
      BOUNDU = BOUNDS(2)
C
C  COMPUTE BIN WIDTHS.  
C
      BINWID = (BOUNDU-BOUNDL)/(IBINS-1)
      IF (IFORM.NE.7 .AND. BINWID.LT.1.0) BINWID=1.0
      BOUNDL = BOUNDL-2.5*BINWID
      IBINS = IBINS+2
C
C EXCLUDE ZEROES?	INTERACTIVE PAUSES?	PEN PLOT?
C
      QEXCLUDE = XVPTST('EXCLUDE')
      QPAUSE = XVPTST('PAUSE') .AND. BATCH().NE.1
C*********************************************************
C			GO TO WORK
C*********************************************************
      M = 4*MAX(NSI,IBINS)
      N = 4*MAX(256,IBINS)
      IF (MODE.EQ.7) THEN
	  M = 4*MAX(NSI,3*IBINS+2)
	  N = 4*MAX(256,3*IBINS+2)
      END IF
      N2 = N
      IF (QBYCHAN) THEN
          ISTART = ISB
          IEND = IEB
          DO I=ISB,IEB
              CALL XVMESSAGE(' ',' ')
              CALL XVMESSAGE(' ',' ')
              CALL XVMESSAGE(' ',' ')
              WRITE (MSG,100) I
  100         FORMAT('---Channel',I4)
              CALL XVMESSAGE(MSG,' ')
              ISB = I
              IEB = I
              CALL STACKA(5,WORK,3,M,N,N2)
          END DO
      ELSE
          CALL STACKA(5,WORK,3,M,N,N2)
      END IF
      RETURN
      END
C**********************************************************************
      SUBROUTINE WORK(BUF,BUFSIZ,HIST,HSTSIZ,RHIST,RHSTSIZ)
C
      INCLUDE 'pgminc'
      COMMON /PARB/PARB
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
 
      INTEGER RHSTSIZ,HSTSIZ,BUFSIZ,MM(2),HIST(*),PARB(xprdim)
      INTEGER NL,NS,SL,SS,LINC,SINC,LINES
      REAL BUF(*),RMM(2),RHIST(*)
      REAL*8 SUM,SUM2,NSUM2
      LOGICAL*4 NOCUM, QEXCLUDE, QPPLOT
C
C**********************************************************************
C GET AREA SPECIFICATION IF ONE WAS GIVEN AND CHECK FOR LEGALITY
C**********************************************************************
      LINES = 0

      DO JAREA=1,NUMARS
	ISL = IAREA(1+(JAREA-1)*4)+SL
	ISS = IAREA(2+(JAREA-1)*4)+SS
	INL = IAREA(3+(JAREA-1)*4)+ISL-1
	INS = IAREA(4+(JAREA-1)*4)+ISS-1
	IF (INL .GT. NLI) THEN
	  CALL XVMESSAGE(
     *    ' DESIRED AREA EXCEEDS INPUT IMAGE SIZE, LINES TRUNCATED',' ')
	  LINES = LINES+1
	  INL = MIN0(NLI,INL)
	ENDIF
	IF (INS .GT. NSI) THEN
	  CALL XVMESSAGE(
     *  ' DESIRED AREA EXCEEDS INPUT IMAGE SIZE, SAMPLES TRUNCATED',' ')
	  LINES = LINES+1
	  INS = MIN0(NSI,INS)
	ENDIF
C
C			CALL APPROPRIATE TABULATING ROUTINE
C
	IF (IFORM.EQ.1) THEN
	  CALL TAB1(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE IF (IFORM.EQ.2) THEN
	  CALL TAB2(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE IF (IFORM.EQ.4) THEN
	  CALL TAB4(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE
	  CALL TAB7(BUF,SUM2,HIST,RMM,SUM,NZEROES)
	END IF
	IF (IFORM.NE.7) THEN
	  RMM(1) = MM(1)
	  RMM(2) = MM(2)
	END IF
	PIXNUM = (IEB-ISB+1)*(1+(INL-ISL)/LINC)*(1+(INS-ISS)/SINC)
	IF (QEXCLUDE) PIXNUM=PIXNUM-NZEROES
C									    mean
        IF (PIXNUM.NE.0.) THEN
            RMEAN = SUM/PIXNUM
        ELSE
            RMEAN = 0.0
        END IF
C							      standard deviation
        NSUM2 = PIXNUM*SUM2-SUM*SUM
        IF (PIXNUM.NE.0 .AND. NSUM2.GT.0) THEN
          SDEV = SQRT(NSUM2)/PIXNUM
        ELSE
          SDEV = 0.0
        END IF
C
C ----- PRINT HISTOGRAM
C
        CALL XVMESSAGE(' ',' ')
	IF (MODE.LE.1) THEN					! normal, wide,
	  CALL PHIST(HIST,RMEAN,SDEV,MM,RMM,PIXNUM)		! or nohist
	  CALL XVMESSAGE(' ',' ')
	ELSE IF (MODE.EQ.2) THEN				! screen
	  CALL SHIST(HIST,RMEAN,SDEV,MM,PIXNUM)
	END IF
	IF (MODE.GT.2 .OR. QPPLOT) THEN				! pplot
	  CALL PLOTXY(HIST,RHIST,BUF,RMEAN,SDEV,RMM,PIXNUM)
	END IF
      END DO
C								send MEAN, SIGMA
C								values to PDF
      CALL XQINI(PARB,xprdim,xcont,ISTAT)
      CALL XQREAL(PARB,'MIN',1,RMM(1),xadd,ISTAT)      
      CALL XQREAL(PARB,'MAX',1,RMM(2),xadd,ISTAT)      
      CALL XQREAL(PARB,'MEAN',1,RMEAN,xadd,ISTAT)      
      CALL XQREAL(PARB,'SIGMA',1,SDEV,xadd,ISTAT)      
      CALL XVQOUT(PARB,ISTAT)
C
      RETURN
      END
C
C**********************************************************************
C     DATA TABULATION ROUTINES FOR ALL DATA TYPES
C
      SUBROUTINE TAB1(IN,SUM2,HOLD,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      COMMON /BHIST/ HIST(256)
      LOGICAL*4 NOCUM
      INTEGER*2 IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST,MM(2),HOLD(256)
C
      NZEROES = 0
      CALL ZIA(HIST,256)
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
            HIST(IN(I)+1) = HIST(IN(I)+1) + 1
	  END DO
	END DO
      END DO
      NZEROES = HIST(1)
      SUM = 0.0
      SUM2 = 0.0
      MM(1) = -1
      CALL ZIA(HOLD,IBINS)
      DO I=1,256
	X = I-1
	SUM = SUM + X*DBLE(HIST(I))
	SUM2 = SUM2 + X*X*DBLE(HIST(I))
	IF (HIST(I).GT.0) THEN
	  MM(2) = X
	  IF (MM(1).LT.0) MM(1)=X
	END IF
	K = MIN(MAX1(1.0,(X-BOUNDL)/BINWID),IBINS)
	HOLD(K) = HOLD(K)+HIST(I)
      END DO
      RETURN
      END
C****************************************************************************
      SUBROUTINE TAB2(IN,SUM2,HIST,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      INTEGER*2 IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*),MM(2)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      MINV = 99999
      MAXV = -99999
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    ITMP = IN(I)
	    IF (ITMP.LT.MINV) MINV=ITMP
	    IF (ITMP.GT.MAXV) MAXV=ITMP
	    IF (ITMP.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + ITMP
            SUM2 = SUM2 + ITMP*ITMP
	    LOC = MIN(MAX1((ITMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      MM(1) = MINV
      MM(2) = MAXV
      RETURN
      END
C******************************************************************************
      SUBROUTINE TAB4(IN,SUM2,HIST,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      INTEGER IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*),MM(2)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      MINV = 999999999
      MAXV = -999999999
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    ITMP = IN(I)
	    IF (ITMP.LT.MINV) MINV=ITMP
	    IF (ITMP.GT.MAXV) MAXV=ITMP
	    IF (ITMP.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + ITMP
            SUM2 = SUM2 + FLOAT(ITMP)*FLOAT(ITMP)
	    LOC = MIN(MAX1((ITMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      MM(1) = MINV
      MM(2) = MAXV
      RETURN
      END
C****************************************************************************
      SUBROUTINE TAB7(IN,SUM2,HIST,RMM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      REAL RMM(2),IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      XMIN = 1.E30
      XMAX = -1.E30
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    TMP = IN(I)
	    IF (TMP.LT.XMIN) XMIN=TMP
	    IF (TMP.GT.XMAX) XMAX=TMP
	    IF (TMP.EQ.0.0) NZEROES=NZEROES+1
            SUM = SUM + TMP
            SUM2 = SUM2 + TMP*TMP
	    LOC = MIN(MAX1((TMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      RMM(1) = XMIN
      RMM(2) = XMAX
      RETURN
      END
C**********************************************************************
      SUBROUTINE PHIST(HIST,RMEAN,SDEV,MM,RMM,PIXNUM)
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      REAL*4 RMM(2)
      INTEGER HIST(*),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
      INTEGER LINC,SINC,LINES,MM(2)
      LOGICAL*4 QEXCLUDE,NOCUM
      CHARACTER*132 PRT
      CHARACTER*101 PLUS/'+         +         +         +         +     
     *    +         +         +         +         +         +'/
      CHARACTER*101 STAR/'**********************************************
     +*******************************************************'/
      CHARACTER*50 BLANK/'                                              
     +    '/
      CHARACTER*30 PRT2
      CHARACTER*11 COL1
      CHARACTER*11 LOWLIMIT  /'< LOW LIMIT'/
      CHARACTER*11 HIGHLIMIT /'>HIGH LIMIT'/
      CHARACTER*8 PRT3
      CHARACTER*6 COL3,PCTILE/'PERCNT'/,CDF/'   CDF'/
      CHARACTER*2 COL5
C
      IF (MODE.LT.0) GO TO 1000
C		 				FIND SPIKE LOCATIONS
      CALL ZIA(SPIKES,9)
      DO J=1,ISPIKE
          MAX=0
          DO I=1,IBINS
              IF(HIST(I) .GT. MAX) IMAX=I
              IF(HIST(I) .GT. MAX) MAX=HIST(I)
          END DO
          IF(MAX .EQ. 0) GO TO 6
          SPIKES(J)=HIST(IMAX)
          HIST(IMAX)=-J
      END DO
    6 IF (MAX.EQ.0 .AND. J.NE.1) MAX=SPIKES(J-1)
      GRAYLEVEL = BOUNDL + 0.5*BINWID
C					             CREATE/PRINT FORMAT HEADERS
      PT = 0.0
c                                                                   WIDE FORMAT
      IF (MODE.EQ.1) THEN
C							     print header lines
C
          WRITE (PRT,100) ISL,ISS,INL-ISL+1,INS-ISS+1,LINC,SINC
  100	  FORMAT(' FREQUENCY DISTRIBUTION     SL=',I5,'     SS=',I5,
     *           '     NL=',I5,'     NS=',I5,'     LINC=',I3,
     *		 '    SINC=',I3)
          CALL IPRNT(PRT,LINES)
          CALL IPRNT(' ',LINES)
C
	  IF (NOCUM) THEN
	      COL3 = PCTILE
	  ELSE
	      COL3 = CDF
	  END IF
          WRITE (PRT,200)  COL3,10,20,30,40,50,60,70,80,90,100
  200	  FORMAT('        GRAY    FREQ  ',A6,'   ',10I10,' ')
          CALL IPRNT(PRT,LINES)
          PRT = '                              ' // PLUS
	  CALL IPRNT(PRT,LINES)
C								    for each bin
	  DO I=1,IBINS
	      GRAYLEVEL = GRAYLEVEL+BINWID
	      IF (HIST(I).EQ.0) THEN
		  IF(I.NE.1.AND.HIST(I-1).NE.0) CALL IPRNT(' ',LINES)
	      ELSE
		  IF (I.EQ.1) THEN
		      COL1 = LOWLIMIT
		  ELSE IF (I.EQ.IBINS) THEN
		      COL1 = HIGHLIMIT
                  ELSE
                      WRITE (COL1,400) GRAYLEVEL
  400                 FORMAT (G11.5)
		  END IF
C		 		   if one of the n=spike largest, label on graph
        	  IF (HIST(I) .LT. 0) THEN
		      J = -HIST(I)
		      HIST(I) = SPIKES(J)
                      WRITE (COL5,500) J
  500                 FORMAT (I2)
                  ELSE
                      COL5 = '  '
		  ENDIF
C						    update number of pixels seen
		  IF (NOCUM) THEN
                      PT = 100.0*HIST(I)/PIXNUM
		  ELSE
		      PT = PT + 100.0*HIST(I)/PIXNUM
		  END IF
C							       draw bar of chart
		  J = (MIN(HIST(I),MAX) * 100) / MAX
		  IF (J.NE.0) THEN
                      WRITE (PRT2,600) COL1,HIST(I),PT
  600                 FORMAT (A11,I8,F8.3,'  ')
                      PRT = PRT2 // STAR(1:J) // PLUS(J+1:101) // COL5
                  ELSE
                      WRITE (PRT,700) COL1,HIST(I),PT,PLUS
  700                 FORMAT (A11,I8,F8.3,'  ',A101)
                  END IF
                  CALL IPRNT(PRT,LINES)
	      END IF
	  END DO
      ELSE
C                                                                  NARROW FORMAT
C								    for each bin
	  DO I=1,IBINS
	      GRAYLEVEL = GRAYLEVEL+BINWID
	      IF (HIST(I).NE.0) THEN
		  IF (I.EQ.1) THEN
		      COL1 = LOWLIMIT
		  ELSE IF (I.EQ.IBINS) THEN
		      COL1 = HIGHLIMIT
                  ELSE
                      WRITE (COL1,400) GRAYLEVEL
		  END IF
C		 		   if one of the n=spike largest, label on graph
        	  IF (HIST(I) .LT. 0) THEN
		      J = -HIST(I)
		      HIST(I) = SPIKES(J)
                      WRITE (COL5,500) J
                  ELSE
                      COL5 = '  '
		  ENDIF
C						    update number of pixels seen
		  IF (I.GT.2 .AND. HIST(I-1).EQ.0) THEN
                      COL3 = '*     '
                  ELSE
                      COL3 = '      '
                  END IF
                  WRITE (PRT3,800) HIST(I)
  800             FORMAT(I8)
C							       draw bar of chart
		  J = (MIN(HIST(I),MAX)*50)/MAX
		  IF (J.NE.0) THEN
                      PRT = COL1 // PRT3 // COL3 // STAR(1:J) //
     +                      BLANK(J+1:50) // COL5
                  ELSE
                      PRT = COL1 // PRT3 // COL3
                  END IF
		  CALL IPRNT(PRT,LINES)
	      END IF
	  END DO
      END IF
      CALL IPRNT(' ',LINES)
 1000 CONTINUE
C				      print statistics for graph data and return
C									  print 
      IF (QEXCLUDE) CALL IPRNT(' EXCLUDING PIXELS OF DN=0',LINES)
      IF (IFORM.NE.7) THEN
         WRITE (PRT,1100) RMEAN,MM(1)
 1100    FORMAT(' Average Gray Level = ',G14.6,10X,'Minimum = ',I10)
         CALL IPRNT(PRT,LINES)
         WRITE (PRT,1200) SDEV,MM(2)
 1200    FORMAT(' Standard Deviation = ',G14.6,10X,'Maximum = ',I10)
         CALL IPRNT(PRT,LINES)
      ELSE
         WRITE (PRT,1300) RMEAN,RMM(1)
 1300    FORMAT(' Average Gray Level = ',G14.6,10X,'Minimum = ',G14.6)
         CALL IPRNT(PRT,LINES)
         WRITE (PRT,1400) SDEV,RMM(2)
 1400    FORMAT(' Standard Deviation = ',G14.6,10X,'Maximum = ',G14.6)
         CALL IPRNT(PRT,LINES)
      END IF
      WRITE (PRT,1500) INT(PIXNUM)
 1500 FORMAT(' Number of Pixels =',I8)
      CALL IPRNT(PRT,LINES)
      RETURN
      END
C**********************************************************************
	SUBROUTINE PLOTXY(HIST,RHIST,BUF,RMEAN,SDEV,RMM,PIXNUM)
        COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
	REAL RMM(2),BUF(*),RHIST(*)
	INTEGER HIST(*),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
	INTEGER LINC,SINC,LINES
	LOGICAL*4 NOCUM,QPPLOT
        CHARACTER*133 PRT,PRT1,PRT2,PRT3
C
C							SCALE X-AXIS
C
	BOUNDL = BOUNDL+2.5*BINWID
	X1 = AMAX1(BOUNDL,RMM(1))			! start of hist
	X2 = AMIN1(BOUNDU,RMM(2))			! end of hist
	IF (X1.GE.X2) GO TO 300
	CALL AXISPTS(X1,X2,XLO,XHI,NXTIC)
C		 					SCALE Y-AXIS
	NSPIKES = MAX(ISPIKE,2)
	CALL ZIA(SPIKES,NSPIKES)
	DO I=2,IBINS-1
	    J = NSPIKES
	    DO WHILE (HIST(I).GT.SPIKES(J) .AND. J.GT.0)
		J = J-1
	    END DO
	    IF (J.NE.NSPIKES) THEN
		K = NSPIKES
		DO L=J+2,NSPIKES
		    SPIKES(K) = SPIKES(K-1)
		    K = K-1
		END DO
		SPIKES(J+1) = HIST(I)
	    END IF
	END DO
	IF (SPIKES(2).EQ.0) 
     +    CALL XVMESSAGE(' Only 1 bin occupied, no plotting done.',' ')
	IF (SPIKES(2).EQ.0) GO TO 300
	TOP = SPIKES(ISPIKE)
	YHI = 10**INT(1.0+ALOG10(TOP))
	IF (YHI/5.0 .GE. TOP) YHI = YHI/5.0
	IF (YHI/2.0 .GE. TOP) YHI = YHI/2.0
	DIV = YHI/10.0
	NYTIC = 10
	DO WHILE (TOP .LE. YHI-DIV)
	    YHI = YHI-DIV
	    NYTIC = NYTIC-1
	END DO
	DO I=2,IBINS-1					! truncate spikes
	    IF (HIST(I).GT.INT(YHI)) HIST(I)=YHI
	END DO
C
	IF (MODE.EQ.7) THEN                             ! for bar graph
	    X = BOUNDL + BINWID/2.0
	    II = 2
	    DO WHILE (HIST(II).EQ.0 .OR. X.LE.X1)	!     find left edge
		II = II + 1
		X = X + BINWID
	    END DO
	    BUF(1) = X-BINWID
	    X = BOUNDU - BINWID/2.0			!     find right edge
	    III = IBINS
	    DO WHILE(HIST(III).EQ.0 .OR. X.GE.X2)
		III = III - 1
		X = X - BINWID
	    END DO
	    J = 0					!     fill x (BUF) and
	    X = BUF(1)					!     y (RHIST) arrays
	    BUF(1) = MAX(X,XLO)
	    BUF(2) = BUF(1)
	    DO I=II,III
		J = J + 3
		X = X + BINWID
		BUF(J) = X
		BUF(J+1) = X
		BUF(J+2) = X
		RHIST(J-2) = 0.0
		RHIST(J-1) = HIST(I)
		RHIST(J) = HIST(I)
	    END DO
	    IBINS = J + 1
	    RHIST(IBINS) = 0.0
	    BUF(IBINS) = MIN(BUF(IBINS),XHI)
	    BUF(IBINS-1) = BUF(IBINS)
	ELSE
	    X = BOUNDL
	    IBINS = IBINS-2
	    DO I=1,IBINS
		RHIST(I) = HIST(I+1)
		BUF(I) = X
		X = X+BINWID
	    END DO
	END IF
C							PLOT THE DATA
C
C				      print statistics for graph data and return
  300	CONTINUE
	N = PIXNUM
	IF (IFORM.EQ.7) THEN
	    WRITE (PRT,500) N,RMM(1),RMM(2),RMEAN,SDEV
  500       FORMAT('#',I9,'#PIXELS###RANGE#',F14.3,'#TO#',F14.3,
     *             '###MEAN#',F12.3,'###STD#DEV#',F12.3)
            I2 = 101
        ELSE
            WRITE (PRT,510) N,RMM(1),RMM(2),RMEAN,SDEV
  510       FORMAT('#',I9,'#PIXELS###RANGE#',F12.1,'#TO#',F12.1,
     *             '###MEAN#',F12.3,'###STD#DEV#',F12.3)
            I2 = 97
	END IF
	CALL SQUEEZE(PRT,PRT2,I2)
	IF (MODE.GE.3)  CALL XVMESSAGE(PRT2,' ')
C								plot to a window
	IF (MODE.GE.6) THEN
	    OPEN (11,FILE='scrvhist001',STATUS='NEW')
	    WRITE(11,280) XLO,XHI,0.0,YHI
	    WRITE(11,270) NXTIC,NYTIC
  270	    FORMAT(2I10)
	    DO I=1,IBINS
		WRITE (11,280) BUF(I),RHIST(I)
  280		FORMAT(4E14.7)
	    END DO
	    CLOSE(11)
	    ISTAT = SYSTEM('idl vicarhist.inp')
	    OPEN (11,FILE='scrvhist001',STATUS='OLD')
	    CLOSE(11,STATUS='DELETE')
	END IF
C								hardcopy plot?
	IF (MODE.NE.4) THEN
	    CALL XVINTRACT('PLOTR','Print this plot (Yes, No) [NO]? ')
	    CALL XVIPARM('PENPLOT',PRT,ICNT,IDEF,1,0)
	    QPPLOT = PRT(1:1).EQ.'Y' .OR. PRT(1:1).EQ.'y'
	END IF
C								submit to the
C								laser printer
	IF (QPPLOT) THEN
	    CALL XVP('TITLE',PRT1,ICNT)
	    DO I=81,1,-1
		IF (PRT1(I:I) .NE. ' ') THEN
		    PRT1(I+1:I+1) = CHAR(0)
		    I1 = I
		    GO TO 650
		END IF
	    END DO
  650	    CONTINUE
C
	    CALL XVGET(INUN,ISTAT,'NAME',PRT3,0)
            WRITE (PRT,700) PRT3,ISL,ISS,INL-ISL+1,INS-ISS+1,LINC,SINC
  700	    FORMAT(A60,'###(', I5, ',', I5, ',', I5, ',', I5,
     +		   ')####LINC#=#', I4, '####SINC#=#', I4)
	    I3 = 118
	    CALL SQUEEZE(PRT,PRT3,I3)
	    CALL PSPLOT(BUF,RHIST,IBINS,XLO,XHI,0.0,YHI,NXTIC,
     +	                NYTIC,'DN','Frequency',PRT1,PRT2,PRT3,-1)
	END IF
C
	RETURN
	END
C**********************************************************************
	SUBROUTINE SHIST(HIST,RMEAN,SDEV,MM,PIXNUM)
        COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
	INTEGER HIST(256),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
	INTEGER LINC,SINC,LINES,MM(2),NEWBIN(80)
	LOGICAL*4 NOCUM
        CHARACTER*1 SCREEN(81,22)/1782*' '/
        CHARACTER*132 PRT
C
	BOUNDL = BOUNDL+2.5*BINWID
	IF (BOUNDL.EQ.0.0 .AND. BOUNDU.EQ.255.0) THEN
	    LOW = MM(1)
	    IHI = MM(2)
	ELSE
	    LOW = BOUNDL
	    IHI = BOUNDU
	END IF
	IBINWIDTH = 1 + (IHI-LOW)/80
	NBINS = 1 + (IHI-LOW)/IBINWIDTH
C
	CALL ZIA(NEWBIN,80)
	N = LOW
	DO I=1,NBINS
	    DO J=1,IBINWIDTH
		N = N+1
		NEWBIN(I) = NEWBIN(I)+HIST(N)
	    END DO
	END DO
C		 				FIND SPIKE LOCATIONS
	CALL ZIA(SPIKES,9)
	DO J=1,ISPIKE
	    MAX = 0
	    DO I=1,NBINS
		IF (NEWBIN(I) .GT. MAX) THEN
		    IMAX = I
		    MAX = NEWBIN(I)
		END IF
	    END DO
	    IF(MAX .EQ. 0) GO TO 6
	    SPIKES(J) = NEWBIN(IMAX)
	    NEWBIN(IMAX) = -J
	END DO
    6   IF (MAX.EQ.0 .AND. J.NE.1) MAX=SPIKES(J-1)
	DIVISOR = MAX/19.0
	NUM = LOW+IBINWIDTH/2
C								label x-axis
	DO I=1,NBINS
	    IF (MOD(I,5).EQ.1) THEN
                WRITE (PRT,100) NUM
  100           FORMAT(I3)
		DO J=1,3
		    SCREEN(I+1,J+19) = PRT(J:J)
		END DO
		NUM = NUM+5*IBINWIDTH
	    END IF
	    IF (NEWBIN(I).NE.0) THEN
C		 		   if one of the n=spike largest, label on graph
		IF (NEWBIN(I) .LT. 0) THEN
                    WRITE(SCREEN(I+1,1),200) -NEWBIN(I)
  200               FORMAT(I1)
		    N = 2
		ELSE
		    N = 20.5-NEWBIN(I)/DIVISOR
		END IF
		DO K=N,19
		    SCREEN(I+1,K) = '*'
		END DO
	    END IF
	END DO
	DO I=1,22
            WRITE (PRT,300) (SCREEN(J,I),J=1,80)
  300       FORMAT(80A1)
            CALL XVMESSAGE(PRT,' ')
	END DO
C				      print statistics for graph data and return
	N = PIXNUM
        WRITE (PRT,500) N,MM(1),MM(2),RMEAN,SDEV
  500   FORMAT(I10,'PIXELS   RANGE',I4,'-',I3,'     MEAN',F8.3,
     1  '     STD DEV',F8.3)
        CALL XVMESSAGE(PRT,' ')
C
	RETURN
	END
C**********************************************************************
C    PRINT MESSAGE, INCREASE LINE COUNT, AND CHECK FOR FULL SCREEN
C**********************************************************************
      SUBROUTINE IPRNT(PRT,LINES)
      COMMON/PAUSE/ QPAUSE
      LOGICAL QPAUSE
      CHARACTER*(*) PRT
C
      CALL XVMESSAGE(PRT,' ')
      LINES = LINES + 1
      IF (LINES.GE.23 .AND. QPAUSE) THEN
	  CALL XVINTRACT('PAGER','PRESS RETURN')
	  LINES = 0
      END IF
      RETURN
      END
