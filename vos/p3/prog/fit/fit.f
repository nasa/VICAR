      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C APR 91      ...REA...  CONVERT TO UNIX/VICAR
C NOV 87      ...JXH...  MODIFIED TO WORK ON BSQ, BIP, BIL ORGS
C OCT 87      ...FFM...  MODIFIED THE SOURCE TO HANDLE THE CASE WHICH INPUT
C                        IS A HALFWORD, OUTPUT IS A BYTE, LVAL=255, HVAL=0.
C FEB 85      ...LWK...  FULL VICAR2 CONVERSION & BUG FIXES. STACKA ADDED.
C NOV 84      ...LWK...  PARTIALLY CONVERTED TO VICAR2 (I/O ONLY)  
C 13 FEBR 84  ...LWK...  RENAMED FIT
C JUN 83      ...JAM...  CONVERTED TO VAX
C 2 SEPT 82   ...GMY...  INITIAL RELEASE: PROGRAM FIT3
C
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/SL,SS,NL,NS,NLI,NSI,NI,NO,NLRI,NPAR,IUN
      COMMON/CP/DBUG,OCODE,LVAL,HVAL,LPER,HPER,NEXCL,INCL,NLEV
      COMMON/CP/PHIS,PEHIS,NSPIKE,MODE,NLIN,INC,LOG
      COMMON/CP/LSL,LSS,LNL,LNS,LNSI,LEL,LINC,EXCL(2,101)
      COMMON/CP2/SBO,NBO,EBO,BRANGE,ORG
      COMMON/CXBUFS/ HIST,NFREQ
      INTEGER*4 HIST(-32768:32767),BRANGE(2)
      CHARACTER*3 ORG
      EXTERNAL FITHIS, FITPIC
C
      CALL XVMESSAGE(' FIT Version 6.1',' ')
      CALL FITPAR
      CALL STACKA(4, FITHIS, 1, 2*NS*NBO, DUM)
      IF(NO.GT.0) CALL STACKA(4, FITPIC, 1, 2*NS*NBO, DUM)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FITPAR
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/SL,SS,NL,NS,NLI,NSI,NI,NO,NLRI,NPAR,IUN
      COMMON/CP/DBUG,OCODE,LVAL,HVAL,LPER,HPER,NEXCL,INCL,NLEV
      COMMON/CP/PHIS,PEHIS,NSPIKE,MODE,NLIN,INC,LOG
      COMMON/CP/LSL,LSS,LNL,LNS,LNSI,LEL,LINC,EXCL(2,101)
      COMMON/CP2/SBO,NBO,EBO,BRANGE,ORG
      REAL*4 LPER,HPER,RPER
      INTEGER BRANGE(2)
      LOGICAL XVPTST
      CHARACTER*4 FORMAT
      CHARACTER*3 ORG
C
      CALL ZIA(DBUG,30)			! ZERO THE FLAGS
      NLEV = 65536

      CALL XVPARM( 'PERCENT', RPER, I, DEF, 0)
      LPER = RPER/2.
      HPER = RPER/2.
      IF (DEF.EQ.1) THEN
	CALL XVPARM( 'LPERCENT', LPER, I, DEF, 0)
	CALL XVPARM( 'HPERCENT', HPER, I, DEF, 0)
      ENDIF

      CALL XVPARM( 'LVALUE', LVAL, I, J, 0)
      CALL XVPARM( 'HVALUE', HVAL, I, J, 0)

      OCODE = 2
      IF (XVPTST( 'BYTE')) THEN
	OCODE = 1
	IF ( J .EQ. 1) HVAL = 255
      ENDIF

      CALL XVPARM( 'SPEED', LINC, I, J, 0)

      CALL XVPARM( 'EXCLUDE', EXCL, NEXCL, DEF, 0)
      IF (DEF.EQ.1) THEN
	IF (XVPTST( 'INCLUDE')) THEN
	  NEXCL = 0
	ELSE
	  NEXCL = 1			! DEFAULT: ONLY EXCLUDE 0 DN
	ENDIF
      ELSE
	IF ((NEXCL/2).NE.(NEXCL+1)/2) THEN
	    CALL XVMESSAGE(' ** EXCLUDE MUST HAVE PAIRS OF NUMBERS **',
     +						' ')
	    CALL ABEND
	ENDIF
	NEXCL = NEXCL/2			! # OF PAIRS
	IF (.NOT.XVPTST( 'INCLUDE')) THEN
	  NEXCL = NEXCL+1		! EXCLUDE ZERO DN
	  CALL ZIA( EXCL(1,NEXCL), 2)
	ENDIF
      ENDIF

      CALL XVPARM( 'AREA', LSL, I, J, 0)
      LEL = LSL + LNL - 1

      IF (XVPTST( 'PHIST')) PHIS = 1

      IF (XVPTST( 'EHIST')) PEHIS = 1

      CALL XVPARM( 'SPIKES', NSPIKE, I, J, 0)

      CALL XVPARM( 'NLINES', NLIN, I, J, 0)

      IF (XVPTST( 'LOG')) LOG = 1

      CALL XVUNIT(IUN,'INP',1,STAT,' ')
      CALL XVOPEN(IUN,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')

      CALL XVGET(IUN,STAT,'NB',NB,'ORG',ORG,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'HALF') THEN
	 CALL XVMESSAGE(' INPUT FORMAT MUST BE HALFWORD',' ')
	 CALL ABEND
      ENDIF

      CALL XVPARM( 'SB', SBO, I, J, 0)
      CALL XVPARM( 'NB', NBO, I, DEF, 0)
      IF (DEF.EQ.1)  NBO = NB
      EBO = SBO+NBO-1
      IF (EBO.GT.NB) THEN
	 CALL XVMESSAGE(
     &	  ' SB+NB-1 IS GREATER THAN THE NUMBER OF BANDS IN THE IMAGE',
     &    ' ')
	 CALL ABEND
      ENDIF

      CALL XVPARM('BRANGE',BRANGE,I,DEF,0)
      IF (DEF.EQ.1) THEN
         BRANGE(1) = SBO
         BRANGE(2) = NBO
      ELSE
        IF (BRANGE(1)+BRANGE(2)-1.GT.NB) THEN
	    CALL XVMESSAGE(
     &	  ' BRANGE IS GREATER THAN THE NUMBER OF BANDS IN THE IMAGE',' ')
	    CALL ABEND
	ENDIF
      ENDIF

      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
      IF (LSL.EQ.0) THEN
	LSL = SL
	LSS = SS
	LNL = NL
	LNS = NS
	LNSI = NSI
	LEL = LSL + LNL - 1
      ENDIF

      CALL XVPCNT( 'OUT', NO)

      RETURN
      END

C**************************************************************
      SUBROUTINE FITHIS( BUF, NBYT)
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/ SL,SS,NL,NS,NLI,NSI,NI,NO,NLRI,NPAR,IUN
      COMMON/CP/ DBUG,OCODE,LVAL,HVAL,LPER,HPER,NEXCL,INCL,NLEV
      COMMON/CP/ PHIS,PEHIS,NSPIKE,MODE,NLIN,INC,LOG
      COMMON/CP/ LSL,LSS,LNL,LNS,LNSI,LEL,LINC,EXCL(2,101)
      COMMON/CP2/SBO,NBO,EBO,BRANGE,ORG
      COMMON/CXBUFS/ HIST,NFREQ
      COMMON/CLAB/NLAB,NLA,NSA,DCODE
      INTEGER*2 BUF(*)
      INTEGER*4 HIST(-32768:32767),BRANGE(2)
      CHARACTER*80 PRT
      CHARACTER*3 ORG

C GENERATE HISTOGRAM
      IOFSET = 0
      CALL ZIA(HIST,65536)
C
      ENDB = BRANGE(1) + BRANGE(2) - 1
      IF (ORG.EQ.'BSQ') THEN
         DO I=BRANGE(1),ENDB
            DO L=LSL,LEL,LINC
  	      CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',LSS,
     &                    'NSAMPS',LNS,'BAND',I,' ')
	      CALL HSTGENF(LNS,BUF,HIST)
            ENDDO
	 ENDDO
      ELSEIF (ORG.EQ.'BIL') THEN
         DO L=LSL,LEL,LINC
            DO I=BRANGE(1),ENDB
  	      CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',LSS,
     &                    'NSAMPS',LNS,'BAND',I,' ')
	      CALL HSTGENF(LNS,BUF,HIST)
            ENDDO
	 ENDDO
      ELSE       		! ORG .EQ. 'BIP'
         LES = LNS+LSS-1
         DO L=LSL,LEL,LINC
            DO I=LSS,LES
  	      CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',I,
     &                    'BAND',BRANGE(1),'NBANDS',BRANGE(2),' ')
	      CALL HSTGENF(BRANGE(2),BUF,HIST)
            ENDDO
	 ENDDO
      ENDIF

      ITOT=0
      DO L=-32768,32767   !COUNT UP TOTAL NUMBER OF PIXELS
	ITOT=ITOT+HIST(L)
      ENDDO
      WRITE (PRT,100) ITOT
  100 FORMAT(' Total Number of Pixels =',I9)
      CALL XVMESSAGE(PRT,' ')
      CALL XVMESSAGE('      RAW HISTOGRAM STATISTICS...',' ')
      CALL PHIST2(HIST(-32768),NLEV,NLIN,INC,NSPIKE,MODE,LOG,PHIS)
      IF (NEXCL .NE. 0) THEN
	DO K=1,NEXCL      !EXCLUDED HISTOGRAM OPTION
            J1=EXCL(1,K)      
            J2=EXCL(2,K)     
            J1=MAX0(J1,-32768) 
            N = J2 - J1 + 1
            IF (N.GT.0) CALL ZIA( HIST(J1), N)
	ENDDO
        CALL XVMESSAGE(' EXCLUDED HISTOGRAM STATISTICS...',' ')
	CALL PHIST2(HIST(-32768),NLEV,NLIN,INC,NSPIKE,MODE,LOG,PEHIS)
      END IF
      RETURN
      END
C*************************************************************************
      SUBROUTINE PHIST2(HIST,NLEV,NLIN,INC0,NSPIKE,MODE,LOG,IPRNT) 
C ROUTINE TO LIST OUT AN INTEGER*4 HISTOGRAM ARRAY
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 MAXT/2147483647/,HIST(-32768:32767)
      CHARACTER*132 PRT
      CHARACTER*1 CSPIKE
C
      N = 0
      S = 0.D0
      S2 = 0.D0
      DO I=-32768,32767
          IFREQ = HIST(I) 
          IF(IFREQ.NE.0) THEN
              R = I
              RMOM = IFREQ*R
              N = N + IFREQ
              S = S + RMOM
              S2 = S2 + RMOM*R
          END IF
      END DO
      IF(N.EQ.0) THEN
          CALL XVMESSAGE(' **HISTOGRAM CONTAINS ZERO ELEMENTS',' ')
          RETURN
      END IF
      AVG = S/N
      SIGMA = S2/N - AVG*AVG
      SIGMA = DSQRT(SIGMA)
      WRITE (PRT,100) AVG,SIGMA,N
  100 FORMAT(' Average Gray Level =',F10.3,'  Standard Deviation =',
     +       F9.3,I9,' Elements')
      CALL XVMESSAGE(PRT,' ')
      IF(NLIN*IPRNT.EQ.0) RETURN
      INC = INC0             !DETERMINE HISTOGRAM INCREMENT (INC)
      IF(INC.EQ.0) CALL PINC(HIST,INC,NLEV,NLIN)
      MAXS = MAXT

      DO J=1,NSPIKE             !SPIKE OPTION
	MAX = 0
	II=999999
	DO I=-32768,32767,INC
	  CALL SUM(INC,HIST(I),IFREQ)
	  IF (IFREQ.GT.MAX.AND.IFREQ.LT.MAXS) MAX=IFREQ
	ENDDO
	IF (MAX.LE.M) GOTO 200
	MAXS = MAX
      ENDDO
  200 CONTINUE
C
      Z0 = MAX0(MAX,1)
      IF(LOG.EQ.1) Z0=DLOG(Z0)
      R = 100.D0/N
      JINC = INC
      II=999999
      CALL XVMESSAGE(' ',' ')
      IF(LOG.EQ.1) CALL XVMESSAGE(' HORIZONTAL SCALE IS LOGARITHMIC',
     +				  ' ')
      WRITE (PRT,300) (N,N=10,100,10)
  300 FORMAT(' GRAY       FREQ  PERCENT   0',10I10)
      CALL XVMESSAGE(PRT,' ')
      IFLAG = 1
      DO I=-32768,32767,INC          			!PRINT HISTOGRAM
          CALL SUM(INC,HIST(I),IFREQ)
          IF(IFREQ.GT.0) THEN
              IFLAG = 0
              PERCEN = IFREQ*R
              Z = MAX0(IFREQ,1)
              IF(LOG.EQ.1) Z=DLOG(Z)
              IVAL = 100.D0*Z/Z0 + 1
	      IF (IVAL .GE. 101) THEN
		  IVAL = 101
		  CSPIKE = '*'
	      ELSE
		  CSPIKE = ' '
	      END IF
              WRITE (PRT,400) I,IFREQ,PERCEN,('*',M=1,IVAL),
     +			      (' ',N=IVAL+1,102),CSPIKE
  400         FORMAT(I7,I9,F9.3,'   ',103A1)
              DO N=11,101,10
                   IF (N .GT. IVAL) PRT(28+N:28+N) = '+'
              END DO
              CALL XVMESSAGE(PRT,' ')
          ELSE
              IF (IFLAG .EQ. 0) CALL XVMESSAGE(' ',' ')
              IFLAG = 1
          END IF
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE PINC(HIST,INC,NLEV,NLIN)
C ROUTINE TO FIND SUITABLE INCREMENT FOR PRINTING OUT A FULLWORD HISTOGRAM
      INTEGER*4 HIST(-32768:32767)
      N1 = 0
      N2 = 0
      N4 = 0
      N8 = 0
      N16 = 0
      N32 = 0
      N64 = 0
      N128 = 0
      N256 = 0
      I = -32769
C
      DO I512=1,NLEV,256
	M128 = N128
	DO I256=1,2
          M64 = N64
          DO I128=1,2  
            M32 = N32
            DO I64=1,2   
              M16 = N16
              DO I32=1,2  
                M8 = N8
                DO I16=1,2
                  M4 = N4
                  DO I8=1,2 
                    M2 = N2
                    DO I4=1,2  
                      M1 = N1
                      DO I2=1,2 
                        I = I + 1
                        IF(HIST(I).GT.0) N1=N1+1    
                      ENDDO                          
                      IF(M1.LT.N1) N2=N2+1
                    ENDDO                            
                    IF(M2.LT.N2) N4=N4+1
                  ENDDO                              
                  IF(M4.LT.N4) N8=N8+1
                ENDDO                                
                IF(M8.LT.N8) N16=N16+1
              ENDDO                                   
              IF(M16.LT.N16) N32=N32+1
            ENDDO     
            IF(M32.LT.N32) N64=N64+1
          ENDDO         
          IF(M64.LT.N64) N128=N128+1
        ENDDO 
        IF(M128.LT.N128) N256=N256+1
      ENDDO

      INC = 1
      IF(N1.LE.NLIN.AND.N1.GT.N2) RETURN
      INC = 2
      IF(N2.LE.NLIN.AND.N2.GT.N4) RETURN
      INC = 4
      IF(N4.LE.NLIN.AND.N4.GT.N8) RETURN
      INC = 8
      IF(N8.LE.NLIN.AND.N8.GT.N16) RETURN
      INC = 16
      IF(N16.LE.NLIN.AND.N16.GT.N32) RETURN
      INC = 32
      IF(N32.LE.NLIN.AND.N32.GT.N64) RETURN
      INC = 64
      IF(N64.LE.NLIN.AND.N64.GT.N128) RETURN
      INC = 128
      IF(N128.LE.NLIN.AND.N128.GT.N256)RETURN
      INC=256
      IF(N256.GT.NLIN) INC=512
      RETURN
      END
C****************************************************************
      SUBROUTINE EHIST2(HIST,NLEV,I1,I2)
C ROUTINE TO EXCLUDE DN INTERVAL I1 TO I2 FROM FULLWORD HISTOGRAM 
      INTEGER*4  HIST(-32768:32767)
      J1 = I1
      J2 = I2
      N = J2 - J1 + 1
      DO  J=1,N
	HIST(J+J1) = 0
      ENDDO
      RETURN
      END

C*******************************************************************
      SUBROUTINE FITPIC( BUF, NBYT)
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/SL,SS,NL,NS,NLI,NSI,NI,NO,NLRI,NPAR,IUN
      COMMON/CP/DBUG,OCODE,LVAL,HVAL,LPER,HPER,NEXCL,INCL,NLEV
      COMMON/CP/PHIS,PEHIS,NSPIKE,MODE,NLIN,INC,LOG
      COMMON/CP/LSL,LSS,LNL,LNS,LNSI,LEL,LINC,EXCL(2,101)
      COMMON/CP2/SBO,NBO,EBO,BRANGE,ORG
      COMMON/CXBUFS/ HIST,NFREQ
      INTEGER*2 BUF(*)
      CHARACTER*80 PRT
      CHARACTER*4 FMT
      CHARACTER*3 ORG
      INTEGER*4 HIST(-32768:32767),BRANGE(2)
      REAL*4 SLOPE,OFFSET
C
C COMPUTE PERCENTAGE LIMITS
      CALL ASTRC2(HIST,LPER,HPER,MIN,MAX)
      WRITE (PRT,100) MIN,LVAL
  100 FORMAT(' Minimum DN of',I7,'   Scaled to',I6)
      CALL XVMESSAGE(PRT,' ')
      WRITE (PRT,200) MAX,HVAL
  200 FORMAT(' Maximum DN of',I7,'   Scaled to',I6)
      CALL XVMESSAGE(PRT,' ')

C OPEN OUTPUT IMAGE
      CALL XVUNIT(OUN,'OUT',1,STAT,' ')
      IF (OCODE.EQ.2) THEN
	FMT = 'HALF'
      ELSE
	FMT = 'BYTE'
      ENDIF
      CALL XVOPEN(OUN,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &            'OP','WRITE','O_FORMAT',FMT,'U_NL',NL,
     &            'U_NS',NS,'U_ORG',ORG,' ')

C ADD HISTORY RECORD TO LABEL
      WRITE (PRT,300) MIN,LVAL,MAX,HVAL,CHAR(0)
  300 FORMAT('Stretch',I7,' to',I6,' and',I8,' to',I6,A1)
      CALL XLADD(OUN,'HISTORY','LIMITS',PRT,STAT,'FORMAT','STRING',' ')

C CREATE LOOK UP TABLE
      DO I=-32768,MIN              !SET BOTTOM OF LUT TO LVAL
	HIST(I)=LVAL
      ENDDO
      DO I=MAX,32767               !SET TOP OF LUT TO HVAL
	HIST(I)=HVAL
      ENDDO
      IF (MAX.LE.MIN) THEN
	SLOPE = 0.
	OFFSET = 0.
	CALL XVMESSAGE(' INPUT PICTURE HAS ONE DN, OUTPUT AS ZERO',' ')
      ELSE
	SLOPE = (1.*(HVAL-LVAL))/(MAX-MIN)
	OFFSET = HVAL - SLOPE*MAX + .5
      ENDIF

      DO  K=MIN,MAX                !GENERATE LUT
	HIST(K) = SLOPE*K + OFFSET
      ENDDO

C GENERATE OUTPUT IMAGE

      EL = NL+SL-1
      IF (ORG.EQ.'BSQ') THEN
         DO K=SBO,EBO
            DO  L=SL,EL
	        CALL XVREAD(IUN,BUF,STAT,'SAMP',SS,'NSAMPS',
     &                      NS,'LINE',L,'BAND',K,' ')
                DO I=1,NS
	           BUF(I)=HIST(BUF(I))
	        ENDDO
	        CALL XVWRIT(OUN,BUF,STAT,' ')
            ENDDO
         ENDDO
      ELSEIF (ORG.EQ.'BIL') THEN
         DO L=SL,EL
            DO K=SBO,EBO
	        CALL XVREAD(IUN,BUF,STAT,'SAMP',SS,'NSAMPS',
     &                      NS,'LINE',L,'BAND',K,' ')
                DO I=1,NS
	           BUF(I)=HIST(BUF(I))
	        ENDDO
	        CALL XVWRIT(OUN,BUF,STAT,' ')
            ENDDO
         ENDDO
      ELSE			!ORG .EQ. 'BIP'
         ES = NS+SS-1
         DO L = SL,EL
            DO  K=SS,ES
	        CALL XVREAD(IUN,BUF,STAT,'SAMP',K,'LINE',L,
     &                      'BAND',SBO,'NBANDS',NBO,' ')
                DO I=1,NBO
	           BUF(I)=HIST(BUF(I))
	        ENDDO
	        CALL XVWRIT(OUN,BUF,STAT,' ')
            ENDDO
         ENDDO
      ENDIF
      RETURN
      END
C**********************************************************************
	SUBROUTINE ASTRC2(HIST,LPER,HPER,MIN,MAX)
C SUBROUTINE TO COMPUTE AUTO-STRETCH LIMITS FROM A FULLWORD HISTOGRAM
	INTEGER*4 HIST(-32768:32767)
	REAL*4    LPER,HPER

	MIN=-32768
	MAX=32767
	NFREQ=0
	DO I=-32768,32767
		NFREQ=NFREQ+HIST(I)
	ENDDO
	IF (NFREQ.EQ.0) RETURN

	ISUM=0
	KOUNT=NFREQ*.01*LPER+1
	DO WHILE (ISUM.LT.KOUNT)
		ISUM=ISUM+HIST(MIN)
		MIN=MIN+1
	ENDDO
	ISUM=0
	KOUNT=NFREQ*.01*HPER+1
	DO WHILE(ISUM.LT.KOUNT)
		ISUM=ISUM+HIST(MAX)
		MAX=MAX-1
	ENDDO

	IF (MIN.GT.32767) MIN=32767
	IF (MAX.LT.MIN) MAX=MIN

	RETURN
	END
C********************************************************************
	SUBROUTINE HSTGENF(NSAMP,PIXLIN,HIST)
C THIS SUBROUTINE GENERATES A HISTOGRAM ON 16BIT DATA.
C EACH CALL TO THE SUBROUTINE PROCESSES ONE PICTURE LINE.
C ARGUMENT LIST
C NSAMP     NUMBER OF SAMPLES IN A PICTURE LINE
C PIXLIN    ARRAY CONTAINING ONE PICTURE LINE
C HIST      ARRAY CONTAINING RUNNING ACCUMULATION FOR HISTOGRAM 65536 WORDS
C 6/3/83 -JAM- INITIAL CODING
	INTEGER*2 PIXLIN(1)
	INTEGER*4 HIST(-32768:32767),DN
C
	DO I=1,NSAMP
             DN=PIXLIN(I)
             HIST(DN)=HIST(DN)+1
	ENDDO
	RETURN
	END
C******************************************************************************
      SUBROUTINE SUM(NUM,IARR,ISUM)
      INTEGER IARR(ISUM)
C
      ISUM = 0
      DO I=1,NUM
          ISUM = ISUM + IARR(I)
      END DO
      RETURN
      END
