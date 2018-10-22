C   WRITTEN BY:               J. H. Reimer       10/31/85
C   REVISION HISTORY
C
C    5-87  SP   CHANGED TO USE NINT FUNCITION FOR ROUNDING TO THE NEAREST
C               INTEGER TO BE COMPATIBLE WITH THE PREVIOUS VERSION OF STRETCH
C               FOR NEGATIVE DNs.
C    6-87  SP   ADDED PARMS PARAMETER IN PDF TO BE COMPATIBLE WITH ASTRTCH2.
C    1-88  SP   ADDED INCLUDE KEYWORD AND CHANGED EXCLUDE DEFAULTS TO AGREE
C               WITH ASTRTCH2.
C    3-91  REA  REWORKED FROM ASU VERSION
C    2-93  REA  GAMMA OPTION ADDED
C    1-01  REA  REMOVED STREQ CALL
C    3-01  REA  modified knuth & xknuth call to make compatible with current
C               argument lists
C
      INCLUDE 'VICMAIN_FOR'
 
      SUBROUTINE MAIN44
      EXTERNAL STRMAIN
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,QHALF,DNMIN,DNMAX
      INTEGER*4 OUNIT,STAT,SL,SS,DNMIN,DNMAX,LUTSIZ,HISSIZ
      CHARACTER*8 FORMAT
      LOGICAL QHALF
C
      CALL XVMESSAGE('STRETCH Version 3-30-01',' ')
C                                                       OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT, 0)
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','HALF',0)
C                                                    GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,0)
      IF(FORMAT .EQ. 'BYTE') THEN
         QHALF = .FALSE.
         DNMAX = 255
      ELSE IF (FORMAT .EQ. 'HALF') THEN
         QHALF = .TRUE.
         DNMAX = 32767
      ELSE
         CALL MYABORT(' STRETCH ACCEPTS BYTE AND HALFWORD DATA ONLY')
      END IF
C                                               GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI)
     +    CALL MYABORT(' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      IF(SS+NSO-1 .GT. NSI)
     +    CALL MYABORT(
     +       ' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
C                                                        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,0)
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     &            'U_NL',NLO,'U_NS',NSO,'U_FORMAT','HALF',0)
C                                                 'DNMIN' - MINIMUM DN VALUE
      CALL XVPARM('DNMIN',DNMIN,ICOUNT,IDEF,1)
      IF(QHALF) THEN      
	 IF (IDEF.NE.0) DNMIN=-32768
         IF(DNMIN.LT.-32768) THEN
            CALL XVMESSAGE(' *** DNMIN RESET TO HALFWORD MIN OF -32768',
     +                     ' ')
            DNMIN=-32768
         ENDIF
      ELSE               
         IF(DNMIN.LT.0) THEN
            CALL XVMESSAGE(' *** DNMIN RESET TO BYTE MIN OF 0',' ')
            DNMIN=0
         ENDIF
      ENDIF
C                                                  'DNMAX' - MAXIMUM DN VALUE
      CALL XVPARM('DNMAX',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
        DNMAX = IPARM
        IF(QHALF) THEN      
          IF(DNMAX.GT.32767) THEN
            CALL XVMESSAGE(' *** DNMAX RESET TO HALFWORD MAX OF 32767',
     +                     ' ')
            DNMAX=32767
          ENDIF
        ELSE               
           IF(DNMAX.GT.255) THEN
              CALL XVMESSAGE(' *** DNMAX RESET TO BYTE MAX OF 255',' ')
              DNMAX=255
           ENDIF
        ENDIF
      END IF
      IF(DNMIN.GT.DNMAX) 
     +    CALL MYABORT(' *** ERROR - DNMIN EXCEEDS DNMAX ***')
C                                                 DYNAMIC ALLOCATION OF BUFFERS
      NBI = 2*NSI
      NLEV=DNMAX-DNMIN+1
      HISSIZ=4*NLEV
      LUTSIZ=2*NLEV
      CALL STACKA(5,STRMAIN,3,NBI,LUTSIZ,HISSIZ)
C                                                               CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,0)
      CALL XVCLOSE(OUNIT,STAT,0)
      RETURN
      END
C***********************************************************************
      SUBROUTINE STRMAIN(BUF,NBI,LUT,LUTSIZ,HIST,HISSIZ)
      EXTERNAL GAUSS,RAMP,ELLIPSE,POWR
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,QHALF,DNMIN,DNMAX
      COMMON /C2/ GSIGMA,POWER
      REAL*4 RPARM(1000),LPERC,TABBUF(200)
      INTEGER*4 OUNIT,DNMIN,DNMAX,STAT,LUTSIZ,HISSIZ
      INTEGER*4 SLAREA,SSAREA,ELAREA,IPARM(1000)
      INTEGER*4 DNVAL,BCKGND,CONINC,SL,SS,HVAL,ALRBUF(100)
      INTEGER*4 HIST(DNMIN:DNMAX),HIST2(0:255)
      LOGICAL XVPTST,QIHIST,QOHIST,QICDF,QOCDF,QHIST,QHALF
      INTEGER*2 BUF(NBI),LUT(DNMIN:DNMAX)
      CHARACTER*1024 FNCBUF  
      EQUIVALENCE (RPARM,IPARM)
      CHARACTER*80 PRT,PRT2
      CHARACTER*14 PRT3
C
   10 FORMAT(' Percentage Saturation at Low End =',F6.2)
   15 FORMAT(' Percentage Saturation at High End =',F6.2)
   20 FORMAT(' DN Values',I7,' Thru',I7,' Excluded')
   25 FORMAT(' DN Value',I7,' Excluded')
   30 FORMAT(I8,' Pixels Outside Range',I7,' to',I7)
   40 FORMAT(' Mean  =',F11.4)
   50 FORMAT(' Sigma =',F11.4)
   60 FORMAT(' (SL,SS,NL,NS) = (',I4,',',I4,',',I4,',',I4,')')
C                                                                initialize
      ICHK=0
      NCHAR2=0
      NLEV=DNMAX-DNMIN+1
C                                                  histogram parameters
      QIHIST = XVPTST('IHIST')
      QOHIST = XVPTST('OHIST')
      QICDF  = XVPTST('ICDF')
      QOCDF  = XVPTST('OCDF')
      QHIST = QIHIST .OR. QOHIST .OR. QICDF .OR. QOCDF
      IF (QHIST .AND. QHALF) THEN
         CALL XVMESSAGE(' Histograms available only for byte data',' ')
         QIHIST = .FALSE.
         QOHIST = .FALSE.
         QICDF = .FALSE.
         QOCDF = .FALSE.
         QHIST = .FALSE.
      END IF
C              *** PROCESS STRETCH SPECIFICATION PARAMETERS ***
C                                                  'COMP' - COMPLEMENT IMAGE
      IF(XVPTST('COMP')) THEN
         IMODE=1
         ICHK=ICHK+1
      END IF
C                                                  'LINEAR' - LINEAR STRETCH
      CALL XVPARM('LINEAR',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .EQ. 2) THEN
         IMODE=2
         ICHK=ICHK+1
         NMIN = IPARM(1)
         NMAX = IPARM(2)
      END IF
C                                                  'CLIP' - BIT CLIP
      CALL XVPARM('CLIP',NBITS,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=3
         ICHK=ICHK+1
      ENDIF
C                                                  'CONTOUR'
      CALL XVPARM('CONTOUR',CONINC,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=4
         ICHK=ICHK+1
         IF(CONINC .EQ. 0) CALL MYABORT(
     +				' *** INVALID CONTOUR VALUE ***')
      ENDIF
C                                            'ALARM' - ALARM SPECIFIED DN VALUES
      CALL XVPARM('ALARM',ALRBUF,NALARM,IDEF,100)
      IF(NALARM .GE. 1) THEN
         IMODE=5
         ICHK=ICHK+1
      END IF
C                                                  'TABLE' - TABLE STRETCH
      CALL XVPARM('TABLE',TABBUF,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=6
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT(
     +      ' INVALID COUNT FOR PARAMETER "TABLE"')
      ENDIF
C                                      'ITABLE' - INDIVIDUAL DN TABLE STRETCH
      CALL XVPARM('ITABLE',IPARM,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=7
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT( 
     +      ' INVALID COUNT FOR PARAMETER "ITABLE"')
         DO I=1,ICOUNT
            TABBUF(I)=IPARM(I)
         ENDDO
      ENDIF
C                                                  'PSTRETCH' - PERIODIC STRETCH
      IF(XVPTST('PSTRETCH')) THEN
         IMODE=8
         ICHK=ICHK+1
      END IF
C                                      'FUNCTION' - USER SPECIFIED FUNCTION 
      CALL XVP('FUNCTION',FNCBUF,ICOUNT)
      CALL XVSPTR(FNCBUF,1,I,NCFUNC)
      IF (NCFUNC.GT.1) THEN
          IMODE=9
          ICHK=ICHK+1
      END IF
C                                                  'GAMMA' - GAMMA STRETCH
      CALL XVPARM('GAMMA',GAMMA,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=10
         ICHK=ICHK+1
      ENDIF
C                                                  'SMOOTH' - CDF RAMP FUNCTION
      IF(XVPTST('SMOOTH')) THEN
         IMODE=11
         QHIST = .TRUE.
         ICHK=ICHK+1
      END IF
C                                'GAUSS' - STRETCH TO FORM GAUSSIAN HISTOGRAM
      IF(XVPTST('GAUSS')) THEN
         IMODE=12
         QHIST = .TRUE.
         ICHK=ICHK+1
      END IF
C                                            'ELLIPSE' - ELLIPTICAL HISTOGRAM
      IF(XVPTST('ELLIPSE')) THEN
         IMODE=13
         ICHK=ICHK+1
         QHIST = .TRUE.
      END IF
C                                      'POWER' - POWER LAW HISTOGRAM ENVELOPE
      CALL XVPARM('POWER',POWER,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IF (POWER.LT.0.0) 
     +      CALL MYABORT(' Power values must be greater than 0')
         IMODE=14
         ICHK=ICHK+1
         QHIST = .TRUE.
      END IF
C                                'PEAK' - CENTER MAX FREQ AND STRETCH AROUND IT
      IF(XVPTST('PEAK')) THEN
         IMODE=16
         QHIST = .TRUE.
         ICHK=ICHK+1
      END IF
C                                'MEAN' - CENTER MEAN FREQ AND STRETCH AROUND IT
      IF(XVPTST('MEAN')) THEN
         IMODE=17
         QHIST = .TRUE.
         ICHK=ICHK+1
      END IF
C                                'ASTRETCH' - AUTO STRETCH
      IF(XVPTST('ASTRETCH')) THEN
         IMODE=18
         QHIST = .TRUE.
         ICHK=ICHK+1
      END IF
C                                CHECK THAT ONLY ONE STRETCH WAS SPECIFIED
      IF(ICHK.GT.1) THEN
         CALL XVMESSAGE(' *** MULTIPLE STRETCHES SPECIFIED ***',' ')
         CALL MYABORT('  ONLY ONE STRETCH MAY BE SPECIFIED')
      END IF
C                                CHECK FOR DEFAULT STRETCH (ASTRETCH)
      IF(ICHK.EQ.0) THEN
         IMODE=18
         QHIST = .TRUE.
      END IF
C        *** END OF GENERAL PARAMETER PROCESSING ***
C**********************************************************************
C        *** OBTAIN HISTOGRAM IF REQUIRED ***
C
      IF (QHIST) THEN
         DO IDN=DNMIN,DNMAX
             HIST(IDN)=0
         ENDDO
C                                  'SPIKES' - NUMBER OF SPIKES IN HISTOGRAM
         CALL XVPARM('SPIKES',NSPIKE,ICOUNT,IDEF,1)
C             IF A SECOND INPUT IS PROVIDED IT IS ASSUMED TO BE A HISTOGRAM
C             OF THE INPUT IMAGE IN THE FORMAT USED BY THE PROGRAM HISTGEN.
C             IF ONLY ONE INPUT IS PROVIDED THE HISTOGRAM OF THE INPUT
C             IS COMPUTED.
C
         CALL XVPCNT('INP',NI)
         IF (NI.EQ.2) THEN
C                      --- HISTOGRAM IS SUPPLIED ---
C                   READ AND UNPACK THE HISTOGRAM IN HISTGEN FORMAT:
C                   DNMIN MUST BE ZERO
C
            IF(DNMIN.NE.0) CALL MYABORT(
     +         ' DNMIN MUST BE ZERO FOR HISTGEN INPUT')
            CALL XVUNIT(IUNIT2,'INP',2,STAT,0)
            CALL XVOPEN(IUNIT2,STAT,'OPEN_ACT','SA','IO_ACT','SA',0)
            CALL XVGET(IUNIT2,STAT,'NL',NL2,'NS',NS2,0)
            DO I = 1,NL2
               CALL XVREAD(IUNIT2,HIST((I-1)*NS2+1),STAT,0)
            ENDDO
            CALL XVCLOSE(IUNIT2,STAT,0)
            NBINS = HIST(1)
            TMEAN = .001*HIST(2)
            TSIGMA = .001*HIST(3)
            DO I=1,NBINS
               HIST(I-1) = HIST(I+3)
            ENDDO
         ELSE
C                                  --- CALCULATE HISTOGRAM OF INPUT IMAGE ---
C
C                                   'LINC' - SUBSAMPLE TO GENERATE HISTOGRAM
C
            CALL XVPARM('LINC',LINC,ICOUNT,IDEF,1)
            IF(LINC.LT.1.OR.LINC.GT.100) THEN
               CALL XVMESSAGE(' INVALID LINC VALUE - LINC SET TO 10',
     +                        ' ')
               LINC=10
            END IF
C                                   'AREA' - AREA SPECIFICATION (SL,SS,NL,NS)
            CALL XVPARM('AREA',IPARM,ICOUNT,IDEF,400)
	    IF (ICOUNT .EQ. 0) THEN
		IPARM(1) = SL
		IPARM(2) = SS
		IPARM(3) = NLO
		IPARM(4) = NSO
		ICOUNT = 4
		NSETS = 1
	    ELSE
		NSETS = ICOUNT/4
		CALL XVMESSAGE(
     +		    'Histogram computed using a sub-area of input:',' ')
		DO I=1,NSETS
		    WRITE (PRT,60) IPARM(4*I-3),IPARM(4*I-2),
     +				   IPARM(4*I-1),IPARM(4*I)
		    CALL XVMESSAGE(PRT,' ')
		END DO
	    END IF
	    IF (ICOUNT .NE. 4*NSETS) CALL MYABORT(
     +			'Error in the number of AREA parameter values')
	    IBAD = 0
	    DO I=1,NSETS
		SLAREA=IPARM(4*I-3)
		SSAREA=IPARM(4*I-2)
		NLAREA=IPARM(4*I-1)
		NSAREA=IPARM(4*I)
		IF(SLAREA.LT.1) SLAREA=1
		IF(SSAREA.LT.1) SSAREA=1
		IF(SLAREA+NLAREA-1.GT.NLI.OR.SSAREA+NSAREA-1.GT.NSI)THEN
		    CALL XVMESSAGE(
     +			' AREA SPECIFICATION EXCEEDS SIZE OF INPUT',' ')
		    CALL XVMESSAGE(' AREA REDUCED',' ')
		    IF(SLAREA+NLAREA-1.GT.NLI) NLAREA=NLI-SLAREA+1
		    IF(SSAREA+NSAREA-1.GT.NSI) NSAREA=NSI-SSAREA+1
		END IF
		ELAREA=SLAREA+NLAREA-1
		DO LINE=SLAREA,ELAREA,LINC
		    CALL XVREAD(IUNIT,BUF,STAT,'LINE',LINE,
     &                          'SAMP',SSAREA,'NSAMPS',NSAREA,0)
		    CALL HISGET(HIST,DNMIN,DNMAX,BUF,NSAREA,NBAD)
		    IBAD=IBAD+NBAD
		END DO
	    END DO
C                                                     COMPUTE MEAN AND SIGMA
            CALL STATI(IND,HIST,DNMIN,DNMAX,TMEAN,TSIGMA)
            IF(IND.NE.0)
     +         CALL MYABORT(' NO PIXELS WITHIN DNMIN TO DNMAX RANGE')
         END IF
C                                CALCULATE TOTAL NUMBER OF PIXELS IN HISTOGRAM
         NPTS=0
         DO I=DNMIN,DNMAX
            NPTS=NPTS+HIST(I)
         ENDDO
         IF(NPTS.LT.1) CALL MYABORT(' HISTOGRAM IS EMPTY')
C                                            PRINT MEAN AND STANDARD DEVIATION
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(' HISTOGRAM BEFORE EXCLUSION . . .',' ')
         WRITE (PRT,40) TMEAN
         CALL XVMESSAGE(PRT,' ')
         WRITE (PRT,50) TSIGMA
         CALL XVMESSAGE(PRT,' ')
C                                            PRINT INPUT HISTOGRAM
         IF(QIHIST) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(' INPUT HISTOGRAM',' ')
            CALL PHIST(HIST,NPTS,0,DNMAX,NSPIKE)
         END IF
C                                            PRINT INPUT CDF
         IF(QICDF) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(' CUMULATIVE DISTRIBUTION FUNCTION',' ')  
            CALL CDF(HIST,DNMIN,DNMAX,NPTS)
         END IF
C                                EXCLUDE OPTION 
C                                DEFAULT IS TO EXCLUDE 0 AND THE MAXIMUM DN.
         IF ((.NOT. XVPTST('INCLUDE')).AND.(IMODE.EQ.18)) THEN
            IDN = 0
            WRITE (PRT,25) IDN
            CALL XVMESSAGE(PRT,' ')
C                                                  SET EXCLUDED ENTRY TO ZERO
            HIST(IDN)=0
            IDN = 255
            IF (QHALF) IDN=32767
            WRITE (PRT,25) IDN
            CALL XVMESSAGE(PRT,' ')
C                                                  SET EXCLUDED ENTRY TO ZERO
            HIST(IDN)=0
         END IF
C                                                              'REXCLUDE'
         CALL XVPARM('REXCLUDE',IPARM,ICOUNT,IDEF,200)
         IF(ICOUNT.GT.0) THEN
            NPAIR=ICOUNT/2
            IF(2*NPAIR.NE.ICOUNT) CALL MYABORT(
     +         ' INVALID COUNT FOR PARAMETER "REXCLUDE"')
            CALL XVMESSAGE(' ',' ')
            DO I=1,NPAIR
               IXL=IPARM(2*(I-1)+1)
               IXH=IPARM(2*(I-1)+2)
               IF(IXL.LT.DNMIN) IXL=DNMIN
               IF(IXH.GT.DNMAX) IXH=DNMAX
               IF(IXL.GT.IXH) CALL MYABORT(
     +				' **ERROR IN REXCLUDE RANGE**')
               WRITE (PRT,20) IXL,IXH
               CALL XVMESSAGE(PRT,' ')
C                                                  SET EXCLUDED ENTRIES TO ZERO
               DO IDN=IXL,IXH
                  HIST(IDN)=0
               ENDDO
            ENDDO
         END IF
C                                                              'IEXCLUDE'
         CALL XVPARM('IEXCLUDE',IPARM,ICOUNT,IDEF,100)
         IF(ICOUNT.GT.0) THEN
            CALL XVMESSAGE(' ',' ')
            DO I=1,ICOUNT
               IDN=IPARM(I)
               IF(IDN.LT.DNMIN) IDN=DNMIN
               IF(IDN.GT.DNMAX) IDN=DNMAX
               WRITE (PRT,25) IDN
               CALL XVMESSAGE(PRT,' ')
C                                                  SET EXCLUDED ENTRY TO ZERO
               HIST(IDN)=0
            ENDDO
         END IF
C                                               INSURE THAT SOME POINTS REMAIN
         ICNT=0  
         DO I=DNMIN,DNMAX
            ICNT=ICNT+HIST(I)
         ENDDO
         NPTS=ICNT
         IF(NPTS.LT.1) CALL MYABORT(
     +       ' EXCLUDE SPECIFICATIONS EXCLUDE ENTIRE IMAGE')
C                                   CALCULATE NEW MEAN AND STANDARD DEVIATION
         CALL STATI(IND,HIST,DNMIN,DNMAX,TMEAN,TSIGMA)
         IF(IND.NE.0) CALL MYABORT(' NO PIXELS REMAIN AFTER EXCLUSION')
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(' HISTOGRAM AFTER EXCLUSION . . .',' ')
         WRITE (PRT,40) TMEAN
         CALL XVMESSAGE(PRT,' ')
         WRITE (PRT,50) TSIGMA
         CALL XVMESSAGE(PRT,' ')
C                                      HISTOGRAM CUTTING OPTION
C                          'CUT' - CUT OUT DN LEVELS WITH LESS THAN A SPECIFIED
C                                  PERCENT OF THE MAXIMUM DN LEVEL
         CALL XVPARM('CUT',RPARM,ICOUNT,IDEF,1)
         IF(ICOUNT.GT.0) THEN
            CUT=RPARM(1)
            IF(CUT.LT.0.0 .OR. CUT.GT.100.0) THEN
               CALL XVMESSAGE(' INVALID CUT VALUE - RESET TO 5.0',' ')
               CUT=5.0
            END IF
            CALL FCLIP(HIST,DNMIN,DNMAX,CUT)
C                                                    COMPUTE NEW MEAN AND SIGMA
            CALL STATI(IND,HIST,DNMIN,DNMAX,TMEAN,TSIGMA)
            IF(IND.NE.0) CALL MYABORT(' NO PIXELS REMAIN AFTER CUTTING')
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(' HISTOGRAM AFTER CUTTING . . .',' ')
            WRITE (PRT,40) TMEAN
            CALL XVMESSAGE(PRT,' ')
            WRITE (PRT,50) TSIGMA
            CALL XVMESSAGE(PRT,' ')
         END IF
C
         IF(IBAD.NE.0) THEN
            WRITE (PRT,30) IBAD,DNMIN,DNMAX
            CALL XVMESSAGE(PRT,' ')
         END IF
      END IF
C                          *** HISTOGRAM OBTAINED ***
C************************************************************************
C        *** GENERATE LOOKUP TABLE FOR SPECIFIED STRETCH ***
      IF(IMODE.LT.1 .OR. IMODE.GT.18) CALL MYABORT(
     +   ' *** ILLEGAL STRETCH MODE ***')
      GO TO (110,120,130,140,150,160,170,180,190,200,
     &       210,220,230,240,200,260,270,280),IMODE
C                                              +++ NON-HISTOGRAM STRETCHES +++
C                                                  --- COMPLEMENT MODE ---
110   CALL XVMESSAGE(' *** COMPLEMENT MODE ***',' ')
      DO I=DNMIN,DNMAX
         LUT(I) = DNMAX - (I-DNMIN)
      ENDDO
      WRITE (PRT,115) DNMIN,DNMAX,DNMAX,DNMIN
  115 FORMAT (' Complement Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR = 58
      GO TO 800
C                                               --- LINEAR STRETCH MODE ---
120   CALL XVMESSAGE(' *** LINEAR CONTRAST STRETCH MODE ***',' ')
      IF(NMIN.EQ.NMAX) NMAX = NMAX+1
C                                                 COMPUTE STRETCH TABLE
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO I = DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(A*I + B)))
      ENDDO
      WRITE (PRT,125) NMIN,DNMIN,NMAX,DNMAX
  125 FORMAT (' Linear Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR = 54
      GO TO 800
C                                                  --- BIT CLIP MODE ---
130   CALL XVMESSAGE(' *** BIT CLIPPING MODE ***',' ')
      NMULT = 2**ABS(NBITS)
      IF (NBITS.GE.0) THEN
         WRITE (PRT,133) NBITS
  133    FORMAT (' Bit Clip Stretch:',I3,
     +           ' Most  Significant Bits Clipped')
         IF (QHALF) THEN
            MODN = 32768
         ELSE
            MODN = 256
         ENDIF
         DO I=DNMIN,DNMAX
            LUT(I) = MIN(DNMAX,MAX(DNMIN,MOD(NMULT*I,MODN)))
	 ENDDO
      ELSE
         WRITE (PRT,136) -NBITS
  136    FORMAT (' Bit Clip Stretch:',I3,
     +           ' Least Significant Bits Clipped')
         DO I=DNMIN,DNMAX
            LUT(I) = I/NMULT
         ENDDO
      ENDIF
      NCHAR=52
      GO TO 800
C                                                   --- CONTOUR MODE ---
140   CALL XVMESSAGE(' *** CONTOUR MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      CONINC=IABS(CONINC)
      DO I=DNMIN,DNMAX,CONINC
         LUT(I) = DNVAL
      ENDDO
      WRITE (PRT,145) CONINC,DNVAL
  145 FORMAT (' Contour Stretch: Interval =',I5,'  DNvalue =',I7)
      NCHAR=51
      GO TO 800
C                                                    --- ALARM MODE ---
150   CALL XVMESSAGE(' *** ALARM MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      DO I=1,NALARM
         IDN=ALRBUF(I)
         IF (IDN.GE.DNMIN .AND. IDN.LE.DNMAX) LUT(IDN)=DNVAL
      ENDDO
      WRITE (PRT,155) DNVAL
  155 FORMAT (' Alarm Stretch: DNvalue =',I7)
      NCHAR=32
      GO TO 800
C                                                 --- TABLE STRETCH MODE ---
160   CALL XVMESSAGE(' *** TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      NINTRV=NPAIRS-1
      DO J=1,NINTRV
         INDN1  = NINT( TABBUF(2*(J-1)+1) )
         OUTDN1 = TABBUF(2*(J-1)+2)
         INDN2  = NINT( TABBUF(2*J+1) )
         OUTDN2 = TABBUF(2*J+2)
         IF(INDN1.LT.DNMIN.OR.INDN2.LE.INDN1.OR.INDN2.GT.DNMAX)
     +      CALL MYABORT(' *** TABLE STRETCH PARAMETER ERROR')
         A=(OUTDN2-OUTDN1)/(INDN2-INDN1)
         B=OUTDN1-A*INDN1
         DO I = INDN1,INDN2
            LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(A*I + B)))
         ENDDO
      ENDDO
      PRT = ' Table Stretch'
      NCHAR=14
      GO TO 800
C                                                  --- ITABLE MODE ---
170   CALL XVMESSAGE(' *** INDIVIDUAL DN TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      DO I=1,NPAIRS
         INDN  = TABBUF(2*(I-1)+1)
         IF(INDN.LT.DNMIN .OR. INDN.GT.DNMAX) CALL MYABORT(
     +      ' *** ERROR IN ITABLE PARAMETER')
         OUTDN = TABBUF(2*(I-1)+2)
         IF(OUTDN.LT.DNMIN) OUTDN=DNMIN
         IF(OUTDN.GT.DNMAX) OUTDN=DNMAX
         LUT(INDN)=OUTDN
      ENDDO
      PRT = ' Individual Table Stretch'
      NCHAR=25
      GO TO 800
C                                                     --- PSTRETCH MODE ---
180   CALL XVMESSAGE(' *** PERIODIC STRETCH MODE ***',' ')
C                                                    get related parameters 
      CALL XVPARM('FREQ',FREQ,ICOUNT,IDEF,1)
      CALL XVPARM('PHI',PHI,ICOUNT,IDEF,1)
      CALL XVPARM('DC',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DC = RPARM(1)
      ELSE
         DC = (DNMAX+DNMIN)/2.0
      END IF
      CALL XVPARM('AMPL',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         AMPL = RPARM(1)/2.0
      ELSE
         AMPL = (DNMAX-DNMIN)/2.0
      END IF
      W=2.0*3.14159*FREQ/(DNMAX-DNMIN)
      DO I=DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(AMPL*SIN(W*I+PHI) + DC)))
      ENDDO
      WRITE (PRT,185) FREQ,DC,2.0*AMPL,PHI
  185 FORMAT (' Periodic Stretch: FREQ=',F6.2,'  DC=',F8.1,
     +        '  AMPL=',F8.1,'  PHI=',F6.2)
      NCHAR=70
      GO TO 800
C                                                 --- FUNCTION MODE ---
190   CALL XVMESSAGE(' *** USER SPECIFIED FUNCTION MODE ***',' ')
      J = MIN(NCFUNC,50)
      NCHAR=30+J
      PRT = ' Function Stretch: FUNCTION = ' // FNCBUF(1:J)
C                                                 parse function, build LUT
      CALL KNUTH(FNCBUF,RPARM,IER)
      IF(IER.NE.0) THEN
         CALL XVMESSAGE(' *** ERROR IN PARSING FUNCTION WITH KNUTH',' ')
         CALL MYABORT(FNCBUF)
      END IF
      DO I=DNMIN,DNMAX
         RPARM(1)=I
         CALL XKNUTH(RPARM,DN)
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(DN)))
      ENDDO
      GO TO 800
C                                                      --- GAMMA OPTION ---
200   CONTINUE
      WRITE(PRT,205) GAMMA
205   FORMAT(' *** Gamma Stretch, Gamma =',F6.3)
      DNRANGE = DNMAX - DNMIN
      DO I=DNMIN,DNMAX
          LUT(I) = NINT(DNRANGE*((I-DNMIN)/DNRANGE)**(1.0/GAMMA)+DNMIN)
      END DO
      GO TO 800
C                                                 +++ HISTOGRAM STRETCHES +++
C                                                    --- SMOOTH OPTION---
C                                    GENERATE A STRETCH SUCH THAT THE CDF OF THE
C                                      RESULTANT HISTOGRAM IS A CONSTANT RAMP
210   CALL XVMESSAGE(' *** SMOOTH OPTION ***',' ')
      CALL HMOD(HIST,LUT,DNMIN,DNMAX,RAMP,NPTS)
      PRT = ' Ramp CDF Stretch'
      NCHAR = 17
      IF(.NOT.QHALF) CALL PTABLE(LUT,DNMIN,DNMAX)
      GO TO 800
C                                                    --- GAUSS OPTION ---
C                                             GENERATE STRETCH SUCH THAT THE 
C                                           RESULTANT HISTOGRAM IS A GAUSSIAN
220   CALL XVMESSAGE(' *** GAUSS OPTION ***',' ')
      CALL XVPARM('GSIGMA',GSIGMA,ICOUNT,IDEF,1)
      IF(GSIGMA.GE.1000..OR.GSIGMA.LE.0.0005) THEN
         CALL XVMESSAGE(' *** INVALID GSIGMA VALUE - RESET TO 3.0',' ')
         GSIGMA=3.0
      END IF
      CALL HMOD(HIST,LUT,DNMIN,DNMAX,GAUSS,NPTS)
      WRITE (PRT,225) GSIGMA
  225 FORMAT (' Gaussian Stretch: GSIGMA =',F7.2)
      NCHAR = 34
      IF(.NOT.QHALF) CALL PTABLE(LUT,DNMIN,DNMAX) 
      GO TO 800
C                                           --- ELLIPTICAL HISTOGRAM OPTION ---
230   CALL XVMESSAGE(' *** ELLIPSE OPTION ***',' ')
      CALL HMOD(HIST,LUT,DNMIN,DNMAX,ELLIPSE,NPTS)
      PRT = ' Elliptical Stretch'
      NCHAR = 19
      GO TO 800
C                                            --- POWER LAW HISTOGRAM OPTION ---
240   CALL XVMESSAGE(' *** POWER LAW OPTION ***',' ')
      CALL HMOD(HIST,LUT,DNMIN,DNMAX,POWR,NPTS)
      WRITE (PRT,245) POWER
  245 FORMAT (' Power Law Stretch: POWER =',F8.2)
      NCHAR = 35
      IF(.NOT.QHALF) CALL PTABLE(LUT,DNMIN,DNMAX)
      GO TO 800
C                                                     --- PEAK OPTION ---
260   CALL XVMESSAGE(' *** PEAK OPTION ***',' ')
      NPKFQ=0
      NPKDN=0
      DO I=DNMIN+1,DNMAX-1
         IF(NPKFQ.LT.HIST(I)) THEN
            NPKFQ=HIST(I)
            NPKDN=I
         END IF
      ENDDO
      PRT3 = 'Peak Stretch:'
      GO TO 272
C                                                     --- MEAN OPTION ---
270   CALL XVMESSAGE(' *** MEAN OPTION ***',' ')
      AMEAN=0.0
      DO I=DNMIN,DNMAX
         AMEAN=AMEAN+I*HIST(I)
      ENDDO
      AMEAN=AMEAN/FLOAT(NPTS)
      IMEAN= NINT( AMEAN )
      NPKFQ=HIST(IMEAN)
      NPKDN=IMEAN
      PRT3 = ' Mean Stretch:'
C                                                    for both MEAN and PEAK
C                                                        RANGE parameter
272   CALL XVPARM('RANGE',IDELTA,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) GO TO 274
C                                                       FACTOR parameter
      CALL XVPARM('FACTOR',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         IDELTA = NINT(FLOAT(NLEV)/RPARM(1))
         GO TO 274
      END IF
C                                                        PERCENT parameter
      CALL XVPARM('PERCENT',PERC,ICOUNT,IDEF,1)
      LP = NINT(NPTS - 0.01*PERC*NPTS)
      ICNT=HIST(NPKDN)
      ILOW=ICNT
      NDNMAX=MAX((NLEV-NPKDN),(NPKDN-1))
      DO I=1,NDNMAX
          IF (NPKDN-I .GE. DNMIN) ICNT=ICNT+HIST(NPKDN-I)
          IF (NPKDN+I .LE. DNMAX) ICNT=ICNT+HIST(NPKDN+I)
          IF (LP.LE.ICNT) GO TO 273
          ILOW = ICNT
      ENDDO
  273 CONTINUE
      IF ((LP-ILOW) .GT. (ICNT-LP)) THEN
         IDELTA = 2*(I+1)
      ELSE
         IDELTA = 2*I
      END IF
C                                            compute MEAN or PEAK stretch table 
  274 CONTINUE
      NMIN = NPKDN - (IDELTA/2)
      NMAX = NPKDN + (IDELTA/2)
      IF (NMIN.EQ.NMAX) NMAX=NMAX+1
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO I = DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(A*I + B)))
      ENDDO
      WRITE (PRT,275) PRT3,NMIN,DNMIN,NMAX,DNMAX
  275 FORMAT (A14,I7,' to',I7,' and',I7,' to',I7)
      NCHAR=52
      GO TO 800
C                                                  --- AUTO-STRETCH OPTION ---
280   CALL XVMESSAGE(' *** AUTO-STRETCH OPTION ***',' ')
C                                                'PERCENT' - TOTAL CUTOFF GIVEN
      CALL XVPARM('PERCENT',RPARM,ICOUNT,IDEF,1)
      LPERC=RPARM(1)/2.
      HPERC=LPERC
C                                                   'LPERCENT' - LOW END CUTOFF
      CALL XVPARM('LPERCENT',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) LPERC=RPARM(1)
C                                                   'HPERCENT' - HIGH END CUTOFF
      CALL XVPARM('HPERCENT',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) HPERC=RPARM(1)
C                                                   PRINT SATURATION PERCENTAGES
      WRITE (PRT,10) LPERC
      CALL XVMESSAGE(PRT,' ')
      WRITE (PRT,15) HPERC
      CALL XVMESSAGE(PRT,' ')
C                                           CALCULATE LOWDN AND HIGHDN FOR TRANS
      CALL LTRANS(HIST,DNMIN,DNMAX,NPTS,LPERC,HPERC,NMIN,NMAX)
C                                                          COMPUTE STRETCH TABLE
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO I = DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(A*I + B)))
      ENDDO
      WRITE (PRT,285) NMIN,DNMIN,NMAX,DNMAX
  285 FORMAT (' Auto-stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR=52
C                                               *** LOOKUP TABLE COMPLETED ***
C*****************************************************************************
  800 CONTINUE
      CALL XVMESSAGE(PRT,' ')
      CALL XVPARM('POST',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
C                                          *** PERFORM POST-STRETCH ***
C                                   CALCULATE LINEAR TRANSFORMATION SO THAT
C                                   DNMIN GOES TO LVAL AND DNMAX GOES TO HVAL.
C                                   MODIFY LOOKUP TABLE TO INCLUDE THIS STRETCH.
        CALL XVMESSAGE(' ',' ')
        CALL XVMESSAGE(' *** POST-STRETCH OPTION ***',' ')
        LVAL=IPARM(1)
        HVAL=IPARM(2)
        IF(LVAL.LT.DNMIN .OR. HVAL.LT.DNMIN) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER LESS THAN DNMIN',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMIN',' ')
           IF(LVAL.LT.DNMIN) LVAL=DNMIN
           IF(HVAL.LT.DNMIN) HVAL=DNMIN
        END IF
        IF(LVAL.GT.DNMAX .OR. HVAL.GT.DNMAX) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER EXCEEDS DNMAX',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMAX',' ')
           IF(LVAL.GT.DNMAX) LVAL=DNMAX
           IF(HVAL.GT.DNMAX) HVAL=DNMAX
        END IF
        IF(LVAL.EQ.HVAL) CALL MYABORT(
     +     ' *** ERROR - POST VALUES SPECIFIED ARE EQUAL')
C                                           COMPUTE COMPOSITE LOOKUP TABLE
        A = FLOAT(HVAL-LVAL)/FLOAT(DNMAX-DNMIN)
        B = -A*DNMIN+LVAL
        DO I = DNMIN,DNMAX
           LUT(I) = NINT( A*LUT(I) + B )
        ENDDO
C                                            SET UP SECOND ADDED LABEL
        WRITE (PRT2,810) DNMIN,LVAL,DNMAX,HVAL
  810   FORMAT (' Post-stretch:',I7,' to',I7,' and',I7,' to',I7)
        NCHAR2=52
        CALL XVMESSAGE(PRT2,' ')
      END IF
C                                        *** LOOKUP TABLE GENERATED ***
C ***********************************************************************
C                                              *** PERFORM STRETCH ***
C                                                   UPDATE LABEL
      CALL XLADD(OUNIT,'HISTORY','PARMS',PRT,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR,0)
      IF(NCHAR2.GT.0) CALL XLADD(OUNIT,'HISTORY','PARMS2',PRT2,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR2,0)
C                                              APPLY STRETCH TABLE TO THE DATA
      DO L=SL,SL+NLO-1
         CALL XVREAD(IUNIT,BUF,STAT,'LINE',L,'SAMP',SS,'NSAMPS',NSO,0)
         DO I=1,NSO
            BUF(I) = LUT(BUF(I))
         END DO
         CALL XVWRIT(OUNIT,BUF,STAT,0)
      END DO
C                          COMPUTE OUTPUT HISTOGRAM IF REQUIRED (BYTE CASE ONLY)
      IF(QOHIST .OR. QOCDF) THEN
         DO I=0,255
            HIST2(I)=0
         ENDDO
         DO I=DNMIN,DNMAX
            HIST2(LUT(I))=HIST2(LUT(I))+HIST(I)
         ENDDO
      END IF
C                                                       PRINT OUTPUT HISTOGRAM
      IF(QOHIST) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(' OUTPUT HISTOGRAM',' ')
         CALL PHIST(HIST2,NPTS,DNMIN,DNMAX,NSPIKE)
      END IF
C                                                            PRINT OUTPUT CDF
      IF(QOCDF) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(' OUTPUT CUMULATIVE DISTRIBUTION FUNCTION',' ')  
         CALL CDF(HIST2,DNMIN,DNMAX,NPTS)
      END IF
      RETURN
      END
C************************************************************************
      SUBROUTINE LTRANS(HIST,DNMIN,DNMAX,NPTS,LPERC,HPERC,IDNLOW,IDNHI)
C
C        CALCULATE LINEAR STRETCH TRANSFORMATION
C     HIST   - HISTOGRAM ARRAY OF SIZE  (NLEV)
C     NPTS   - TOTAL NUMBER OF PIXELS IN HISTOGRAM
C     LPERC - PERCENT OF NPTS TO SATURATE AT LOW DN LEVEL
C     HPERC - PERCENT OF NPTS TO SATURATE AT HIGH DN LEVEL
C     IDNLOW - DN VALUE TO MAP TO LOW DN IN TRANSFORMATION
C     IDNHI  - DN VALUE TO MAP TO HIGH DN IN TRANSFORMATION
C
      REAL*4 LPERC,HPERC
      INTEGER*4 DNMIN,DNMAX
      INTEGER*4 HIST(DNMIN:DNMAX)
C                                                       FIND LOW DN CUTOFF
      NPTSLO=NINT(0.01*LPERC*FLOAT(NPTS))
      ICNT=0
      ILOW=0
      DO I=DNMIN,DNMAX
         ICNT=ICNT+HIST(I)
         IF (NPTSLO .LE. ICNT) GO TO 200
         ILOW = ICNT
      END DO
  200 CONTINUE
      IF ((NPTSLO-ILOW) .GT. (ICNT-NPTSLO)) THEN
         IDNLOW = I
      ELSE
         IDNLOW = I-1
      END IF
C                                                      FIND HIGH DN CUTOFF
      NPTSHI=NINT(0.01*HPERC*FLOAT(NPTS))
      ICNT=0
      ILOW=0
      DO I=DNMAX,DNMIN,-1
         ICNT = ICNT + HIST(I)
         IF (NPTSHI .LE. ICNT) GO TO 400
         ILOW = ICNT
      END DO
  400 CONTINUE
      IF ((NPTSHI-ILOW) .GT. (ICNT-NPTSHI)) THEN
         IDNHI = I
      ELSE
         IDNHI = I+1
      END IF
      RETURN
      END
C****************************************************************************
	SUBROUTINE HMOD(HIST,LUT,DNMIN,DNMAX,XCDF,NPTS)
C
C		Generate lookup tables (LUT) for the non-linear histogram
C		based stretches
C		HIST    - Histogram array (input)
C		LUT     - Lookup table array (output)
C		DNMIN   - Minimun DN value (input)
C		DNMAX   - Maximum DN value (output)
C		XCDF    - Name of the external function the returns the
C			  appropriate output cdf for the supplied input cdf
C		NPTS    - Total number of pixels in excluded histogram
C
	INTEGER*4 DNMIN,DNMAX
	INTEGER*4 HIST(DNMIN:DNMAX)
	INTEGER*2 LUT(DNMIN:DNMAX)
C
	PTS = NPTS
	RANGE = DNMAX - DNMIN
	SUM = 0.0
C
	DO I=DNMIN,DNMAX
	    SUM = SUM + HIST(I)
	    FRAC = (SUM - HIST(I)/2.0) / PTS
	    LUT(I) = NINT(DNMIN + RANGE*XCDF(FRAC))
	END DO
	RETURN
	END
C***************************************************************************
	FUNCTION ELLIPSE(FRAC)
	REAL PI/3.141593/
C
	XLFRAC = 0.0
        XLOWER = -1.0
	UFRAC = 1.0
	UPPER = 1.0
  100	CONTINUE
C                                        Estimate a trial value (TEST) and
C                                        compute its fractional area (TFRAC)
	TEST = (UPPER-XLOWER)*(FRAC-XLFRAC)/(UFRAC-XLFRAC) + XLOWER
	TFRAC = (TEST*SQRT(1.0-TEST*TEST) + ASIN(TEST))/PI + 0.5
C                                          If trial area is not close enough,
C                                          substitute the trial value for the
C                                          bounding value and try again
	IF (TFRAC-FRAC .GT. 0.000001) THEN
	    UFRAC = TFRAC
	    UPPER = TEST
	    GO TO 100
	ELSE IF (FRAC-TFRAC .GT. 0.000001) THEN
	    XLFRAC = TFRAC
	    XLOWER = TEST
	    GO TO 100
	END IF
C                                          TEST is close enough; rescale 0-1
	ELLIPSE = (TEST+1.0)/2.0
	RETURN
	END
C***************************************************************************
	FUNCTION GAUSS(FRAC)
C
C	Polynomial approximation to the Gaussian (Normal) distribution
C	function, taken from Handbook of Mathematical Functions by 
C	Abramowitz and Stegun, National Bureau of Standards, 1968
C	(via HP-33E Statistics Applications Handbook)
C
	COMMON /C2/ GSIGMA,POWER
	REAL B0/2.515517/
	REAL B1/0.802853/
	REAL B2/0.010328/
	REAL D1/1.432788/
	REAL D2/0.189269/
	REAL D3/0.001308/
C
	IF (FRAC .EQ. 0.0) THEN
	    GAUSS = 0.0
	ELSE IF (FRAC .EQ. 1.0) THEN
	    GAUSS = 1.0
	ELSE IF (FRAC .LE. 0.5) THEN
	    Q = FRAC
	    T = SQRT(ALOG(1.0/(Q*Q)))
	    X = T - (B0 + B1*T +B2*T*T)/(1.0 + D1*T + D2*T*T + D3*T*T*T)
	    GAUSS = MAX((GSIGMA-X)/(2.0*GSIGMA), 0.0)
	ELSE
	    Q = 1.0 - FRAC
	    T = SQRT(ALOG(1.0/(Q*Q)))
	    X = T - (B0 + B1*T +B2*T*T)/(1.0 + D1*T + D2*T*T + D3*T*T*T)
	    GAUSS = MIN((GSIGMA+X)/(2.0*GSIGMA), 1.0)
	END IF
	RETURN
	END
C***************************************************************************
	FUNCTION POWR(FRAC)
	COMMON /C2/ GSIGMA,POWER
C
	P1 = POWER+1
	XLFRAC = 0.0
        XLOWER = -1.0
	UFRAC = 1.0
	UPPER = 1.0
  100	CONTINUE
C                                        Estimate a trial value (TEST) and
C                                        compute its fractional area (TFRAC)
	TEST = (UPPER-XLOWER)*(FRAC-XLFRAC)/(UFRAC-XLFRAC) + XLOWER
	IF (TEST .GE. 0.0) THEN
	    TFRAC = (1.0 + (P1*TEST - (TEST**P1))/POWER) / 2.0
	ELSE
	    TFRAC = (1.0 + (P1*TEST + ((-TEST)**P1))/POWER) / 2.0
	END IF
C                                          If trial area is not close enough,
C                                          substitute the trial value for the
C                                          bounding value and try again
	IF (TFRAC-FRAC .GT. 0.000001) THEN
	    UFRAC = TFRAC
	    UPPER = TEST
	    GO TO 100
	ELSE IF (FRAC-TFRAC .GT. 0.000001) THEN
	    XLFRAC = TFRAC
	    XLOWER = TEST
	    GO TO 100
	END IF
C                                          TEST is close enough; rescale 0-1
	POWR = (TEST+1.0)/2.0
	RETURN
	END
C******************************************************************************
	FUNCTION RAMP(FRAC)
	RAMP = FRAC
	RETURN
	END
C******************************************************************************
      SUBROUTINE FCLIP(HIST,DNMIN,DNMAX,FCLIPC)
C        ELIMINATE LOW DN FREQUENCIES
C     LET MAX BE MAXIMUM FREQUENCY FOR A DN LEVEL
C     FOR HISTOGRAM. THEN IF HIST(I).LT.MAX*FCLIPC/100., HIST(I)=0
C     HIST   - (I*4) HISTOGRAM ARRAY
C     FCLIPC - CLIPPING TOLERANCE
      INTEGER*4 DNMIN,DNMAX
      INTEGER*4 HIST(DNMIN:DNMAX)
C                                                           FIND MAX FREQ LEVEL
      MAX=0
      DO I=DNMIN,DNMAX
         IF(HIST(I).GT.MAX) MAX=HIST(I)
      END DO
C                                                    PERFORM CLIPPING ALGORITHM
      ITOL=(FLOAT(MAX)*(FCLIPC/100.0)+0.05)
      DO I=DNMIN,DNMAX
         IF(HIST(I).LT.ITOL) HIST(I)=0
      END DO
      RETURN
      END
C**************************************************************************
	SUBROUTINE PTABLE(LUT,DNMIN,DNMAX)
C
	INTEGER*4 DNMIN,DNMAX
	INTEGER*2 LUT(DNMIN:DNMAX)
	CHARACTER*80 BUF
C			        print the table stretch transformation coords
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('  T R A N S F O R M A T I O N',' ')
        CALL XVMESSAGE('    IN OUT    IN OUT    IN OUT    IN OUT    IN O 
     +UT    IN OUT    IN OUT    IN OUT',' ')
	N = (DNMAX-DNMIN)/8 + 1
	DO I=1,N
            WRITE (BUF,100) (J,LUT(J), J=DNMIN+I-1,DNMAX,N)
	    CALL XVMESSAGE(BUF,' ')
	END DO
  100   FORMAT (8(I6,I4))
	RETURN
	END
C***************************************************************************
      SUBROUTINE CDF(HIST,DNMIN,DNMAX,NPTS)
C        GENERATE AND PRINT CDF OF A HISTOGRAM
C     HIST    INTEGER*4 HISTOGRAM BUFFER
      INTEGER*4 DNMIN,DNMAX,CDFUN(512)
      INTEGER*4 HIST(DNMIN:DNMAX)
      NLEV=DNMAX-DNMIN+1
      CDFUN(1)=HIST(DNMIN)
      DO I=2,NLEV
         CDFUN(I)=CDFUN(I-1)+HIST(DNMIN+I-1)
      END DO
      CALL PHIST(CDFUN,NPTS,DNMIN,DNMAX,0)
      RETURN
      END
C**************************************************************************
      SUBROUTINE HISGET(HIST,DNMIN,DNMAX,HBUF,NS,NBAD)
C        ACCUMULATE HISTOGRAM
      INTEGER*4 DNMIN,DNMAX
      INTEGER*4 HIST(DNMIN:DNMAX)
      INTEGER*2 HBUF(*)
      NBAD=0
      DO I=1,NS
         IDN=HBUF(I)
         IF(IDN.LT.DNMIN) THEN
            NBAD=NBAD+1
            IDN=DNMIN
         ELSE IF(IDN.GT.DNMAX) THEN
            NBAD=NBAD+1
            IDN=DNMAX
         END IF
         HIST(IDN)=HIST(IDN)+1
      END DO
      RETURN
      END
C************************************************************************
      SUBROUTINE PHIST(FREQ,NUM,LOWDN,HIGHDN,NSPIKE)
C
C  PRINT A VARIABLE WIDTH HISTOGRAM WITH SPIKES OPTION
C  (THIS IS A MODIFICATION OF PHIST BY HJFRIEDEN)
C
C  FREQ   - INTEGER ARRAY CONTAINING HISTOGRAM
C  NUM    - INTEGER SUM OF FREQUENCIES OF ALL DN LEVELS
C  NSPIKE - INTEGER NUMBER OF SPIKES TO SKIP IN NORMALIZATION
C  LOWDN  - INTEGER VALUE OF DN LEVEL TO ASSIGN TO HIST(1)
C  HIGHDN - INTEGER VALUE OF THE UPPER DN LEVEL OF THE HISTOGRAM
C
      INTEGER FREQ(*),HIGHDN,MAXT,SPIKE
      CHARACTER*132 LISTO
      CHARACTER*23 NUMS
      CHARACTER*103 ASTER/'*********************************************
     +******************************************************* *'/
      CHARACTER*105  PLUS/'+         +         +         +         +    
     +     +         +         +         +         +        +    '/
      DATA MAXT/z'7FFFFFFF'/
C
      NLEV=HIGHDN-LOWDN+1
      IF(NLEV.LT.1 .OR. NUM.LT.1) THEN
         CALL XVMESSAGE(' ERROR IN PHIST ARGUMENT LIST',' ')
         CALL XVMESSAGE(' NO HISTOGRAM PRINTED',' ')
         RETURN
      END IF
      SPIKE=NSPIKE+1
      IF(SPIKE.GE.NLEV.OR.SPIKE.LT.1) SPIKE=1
      CALL XVMESSAGE(' GRAY      FREQ  PERCENT   0        10        20   
     +      30        40        50        60        70        80         
     +90       100    ',' ')
      CALL XVMESSAGE(' ',' ')
      MAXS=MAXT
      DO J=1,SPIKE
         MAX=0
         DO I=1,NLEV
            IF(FREQ(I).GT.MAX.AND.FREQ(I).LT.MAXS) MAX=FREQ(I)
         ENDDO
         MAXS=MAX
      ENDDO
      IF(MAX.LT.1) MAX=1
      DO I=1,NLEV
         IFREQ=FREQ(I)
         IF(I.GT.1 .AND. IFREQ.EQ.0) THEN
            IF(FREQ(I-1).NE.0) CALL XVMESSAGE(' ',' ')
         ELSE
            PERCEN=(100.0*IFREQ)/NUM
            MTEMP=((100*IFREQ)/MAX)+1
            IF(MTEMP.GT.101) MTEMP=103
            WRITE (NUMS,500) LOWDN+I-1, IFREQ, PERCEN+0.0005 
  500       FORMAT (I5,I10,F8.3)
            LISTO = NUMS//'    '//ASTER(1:MTEMP)//PLUS(MTEMP+1:105)
            CALL XVMESSAGE(LISTO,' ')
         ENDIF
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE STATI(IND,HIST,DNMIN,DNMAX,MEAN,SIGMA)
C        CALCULATE MEAN AND SIGMA FROM HISTOGRAM
C     IND   - RETURN INDICATOR  (=0 IF NO ERROR)
C     HIST  - INTEGER*4 ARRAY DIMENSIONED AT NLEV
C     NLEV  - NUMBER OF DN LEVELS IN HIST
C     MEAN  - REAL*4 VALUE RETURNED WITH MEAN OF HIST DISTRIBUTION
C     SIGMA - REAL*4 VALUE RETURNED WITH STANDARD DEVIATION OF
C             HIST DISTRIBUTION
      REAL*8 SUM,SUMSQ,DMEAN
      REAL*4 MEAN,SIGMA
      INTEGER*4 DNMIN,DNMAX
      INTEGER*4 HIST(DNMIN:DNMAX)
      IND=0
      SUMSQ=0.0
      SUM=0.0
      NPTS=0
      DO IDN=DNMIN,DNMAX
         NPIX=HIST(IDN)
         SUM=SUM+NPIX*IDN
         SUMSQ=SUMSQ+DBLE(NPIX)*DBLE(IDN)*DBLE(IDN)
         NPTS=NPTS+NPIX
      END DO
      IF(NPTS.EQ.0) THEN
         IND=-1
      ELSE
C                                              CALCULATE MEAN AND STD DEVIATION
         DMEAN=SUM/NPTS
         MEAN=DMEAN
         SIGMA=DSQRT(DABS(SUMSQ/NPTS-DMEAN*DMEAN))
      END IF
      RETURN
      END
C*************************************************************************
	subroutine MYABORT(msg)
	character*(*) msg
	call xvmessage(msg, ' ')
	call abend
	return
	end
