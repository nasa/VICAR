      INCLUDE 'VICMAIN_FOR'
C Radiometric Calibration Program PICSUM -- Adds multiple frames together.
C        PICSUM  (I1,I2,I3,...,In)  OUT  user-parameters...
C Inputs may be byte or halfword.
C
C Cassini data is 12 bits and so the range for the despike routine is 0 - 4095.

      SUBROUTINE MAIN44

         INTEGER*2 LTBL(0:4095),HTBL(0:4095), IN(32768)
         INTEGER OUNI,IUNI(30),IUNIT,XVPTST,ASCALE,MFLAG,PICSCALE,TFLAG
         INTEGER BUF(30), EXPOS, EXPOS1, SCLK, 
     &           SCLKS(30), TNL, TNS

         REAL*4 PAR(2),LSCALE

         CHARACTER*100 FN, FNAME
         CHARACTER*8 FMT, TFMT
         CHARACTER*5 PROJECT
         CHARACTER*5 CAMERA, CAMERA1, FILT1, FILT11, FILT2, FILT21
         CHARACTER*5 GAIN, GAIN1
         CHARACTER*4 MODE, MODE1

         CALL IFMESSAGE ('PICSUM version 14 Feb 2011')

         CALL XVPCNT('INP',NI)	!Number of input datasets

C        Open all inputs and print out picture labels...

         CALL XVP('LIST',FNAME,NJ)		! use a search list or INP

         IF ((NI .NE. 1) .AND. (NJ .NE. 0)) THEN
            CALL XVMESSAGE('Give only one INP if LIST is given',' ')
            CALL ABEND()
         ENDIF

         IF (NI .NE. 1) THEN 
            DO I=1,NI
               CALL XVUNIT(IUNI(I),'INP',I,IND,' ')
               CALL XVOPEN (IUNI(I),IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                      'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF ((CAMERA1 .NE. CAMERA) .OR.
     &                    (GAIN1 .NE. GAIN) .OR. 
     &                    (MODE1 .NE. MODE) .OR. 
     &                    (EXPOS1 .NE. EXPOS) .OR. 
     &                    (FILT11 .NE. FILT1) .OR. 
     &                    (FILT21 .NE. FILT2)) THEN
                     CALL XVMESSAGE 
     & ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
            ENDDO
         ELSEIF (NJ .NE. 0) THEN		!open search list

C Read INP just to get the VICAR label put on the output
            CALL XVUNIT(IUNIT,'INP',1,IND,' ')
            CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
            OPEN(UNIT=99,FILE=FNAME,STATUS='OLD',ERR=950)
            READ(99,FMT=1) FN                     !SKIP FIRST LINE
1	    FORMAT(A)
	
	    DO I=1,31				!open files in list
	       READ(99,FMT=1,END=11) FN
	       IF (I .GE. 31) GO TO 920
	       NI = I
	       CALL XVUNIT(IUNI(I),'NONE',I,IST,'U_NAME',FN,' ')
               CALL XVOPEN(IUNI(I),IST,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF (CAMERA1.NE.CAMERA .OR. GAIN1.NE.GAIN .OR.
     &                    MODE1.NE.MODE .OR. EXPOS1.NE.EXPOS .OR.
     &                    FILT11.NE.FILT1 .OR. FILT21.NE.FILT2) THEN
                     CALL XVMESSAGE
     &  ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
	    ENDDO
11	    CONTINUE
         ELSE
            CALL XVMESSAGE 
     &         ('Specify at least two INPs or one INP with LIST',' ')
            CALL ABEND
         END IF

C        Get size and format of first input image NLxNS....
         CALL XVGET(IUNI(1),IND,'FORMAT',FMT,'NL',NL,'NS',NS,' ')
         IF ((FMT .NE. 'BYTE') .AND. (FMT .NE. 'HALF')) THEN
            CALL XVMESSAGE('***Illegal input format',' ')
            GOTO 999
         ENDIF
         DO I=1,NI-1
            CALL XVGET(IUNI(I),IND,'FORMAT',TFMT,'NL',TNL,'NS',TNS,' ')
            IF (TFMT.NE.FMT .OR. TNL.NE.NL .OR. TNS.NE.NS) THEN
               CALL XVMESSAGE
     &         ('***All inputs must be of the same size and format',' ')
               GOTO 999
            ENDIF
         ENDDO

         ASCALE = XVPTST('ASCALE')		!Auto picture scale flag
         IF (ASCALE.EQ.1) THEN
            XSCALE = 128./NI
            PICSCALE = 128
         ELSE
            PICSCALE = NI
         ENDIF

         TFLAG = 0				!Threshold flag for despiking
         CALL XVPARM('TSCALE',PAR,ICNT,IDEF,2)
         IF (IDEF.EQ.0) THEN
            TFLAG = 1			!Turn flag on
            LSCALE = PAR(1)/SQRT(SGC)
            HSCALE = PAR(2)/SQRT(SGC)
            CALL XVPARM('MINT',ival,icnt,idef,1)
            IF (IDEF.NE.1) MINT=IVAL
            CALL PRNT(4,1,MINT,'MINT=.')

            DO I=0,4095
               DN = SQRT(FLOAT(I))
               LTBL(I) = LSCALE*DN
               HTBL(I) = HSCALE*DN
               IF (LTBL(I) .LT. MINT) LTBL(I)=MINT
               IF (HTBL(I) .LT. MINT) HTBL(I)=MINT
            ENDDO
         ENDIF

         MFLAG = .FALSE.
         IF (TFLAG.EQ.0) MFLAG = XVPTST('MEDIAN')		!Median filter flag

         CALL XVUNIT(OUNI,'OUT',1,IND,' ')
         CALL XVOPEN(OUNI,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &         'OP','WRITE','U_NL',NL,'U_NS',NS,'O_FORMAT','HALF',' ')
         CALL XLADD
     &      (OUNI,'HISTORY','PICSCALE',PICSCALE,IND,'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','NFRAMES',NI,IND,'FORMAT','INT',' ')
         IF (PROJECT .EQ. 'CASSI') THEN
            CALL SORTIN(SCLKS,NI)
            CALL XLADD(OUNI,'HISTORY','SCLKS',SCLKS,IND,'FORMAT','INT',
     &              'NELEMENT',NI,' ')
         ENDIF
         CALL SUMIT(IUNI,OUNI,IN,IN,BUF,NL,NS,NI,TFLAG,
     &      ASCALE,XSCALE,LTBL,HTBL,MFLAG)
         CALL XVCLOSE(OUNI,IND,' ')
         CALL XVMESSAGE('PICSUM task completed',' ')
         RETURN
CCCCCCCCC
C
920      CALL XVMESSAGE('more than 30 filenames in LIST',' ')
         GO TO 999
950      CALL XVMESSAGE('could not open input list file',' ')
         CALL XVMESSAGE(FNAME,' ')
999      CALL XVMESSAGE('***PICSUM task cancelled',' ')
         END



C Perform the pixel summation (or filtering)
C
      SUBROUTINE SUMIT(IUNI,OUNI,IN,OBUF,BUF,NL,NS,NI,
     &	TFLAG,ASCALE,XSCALE,LTBL,HTBL,MFLAG)

         INTEGER IUNI(NI),OUNI,TFLAG,ASCALE,MFLAG
         INTEGER*2 IN(NS,NI),OBUF(NS),LTBL(0:4095),HTBL(0:4095)
         INTEGER*4 BUF(NI)

         NBAD = 0

         DO 100 L=1,NL
            DO I=1,NI
               CALL XVREAD(IUNI(I),IN(1,I),IND,' ')
            ENDDO

            IF (TFLAG .EQ. 1) THEN    !If thresholds are specified,
               DO I=1,NS
                  NP = 0
                  DO J=1,NI
                     IDN = IN(I,J)
                     IF (IDN.GT.0) THEN
                        NP = NP + 1
                        BUF(NP) = IDN
                     ENDIF
                  ENDDO
                  CALL DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)  !despike data.
                  NBAD = NBAD + (NI-N)	   !Number of bad samples
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ELSEIF (MFLAG .EQ. 1) THEN    !If produce a median file.
               DO I=1,NS
                  NP = 0
                  DO J=1,NI
                     IDN = IN(I,J)
C                     IF (IDN.GT.0) THEN
                        NP = NP + 1
                        BUF(NP) = IDN
C                     ENDIF
                  ENDDO
                  CALL GETMED(BUF,NI,NP,N,IDN,LTBL,HTBL)  !return median
C                  NBAD = NBAD + (NI-N)	   !Number of bad samples
                  OBUF(I) = IDN
               ENDDO
            ELSE
               DO I=1,NS	    !Else, just add lines together
                  IDN = IN(I,1)
                  DO J=2,NI
                     IDN = IDN + IN(I,J)
                  ENDDO
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF

            IF (ASCALE .EQ. 1) THEN
               DO I=1,NS
                  IDN = OBUF(I)*XSCALE
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF
  100    CALL XVWRIT(OUNI,OBUF,IND,' ')

         IF (TFLAG.EQ.1) 
     &      CALL PRNT(4,1,NBAD,'Number of bad pixels=.')
         RETURN
      END



C Scan for noise spikes
C
      SUBROUTINE DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)

         INTEGER BUF(NI)

         INTEGER*2 LTBL(0:4095),HTBL(0:4095)
         INTEGER HTHRESH

         IF (NP.EQ.0) THEN		!If all samples are zero
            N = 0
            IDN = 0		!return zero
            RETURN
         ELSEIF (NP.EQ.1) THEN	!If only one non-zero sample
            N = 1
            IDN = BUF(1)*NI	!then no screening necessary
            RETURN
         ENDIF

C     ....Here if at least two non-zero samples are in BUF
         CALL SORTIN(BUF,NP)	!Sort the samples
         MIX = (NP+1)/2		!Index of median
         MEDIAN = BUF(MIX)		!Median DN value
         ISUM = MEDIAN		!Initialize sum
         N = 1			!Number of points in sum

C     ....Add all samples less than median
         IEND = MIX - 1
         IF (IEND.EQ.0) GOTO 20	!Skip if BUF(1) is median
         LTHRESH = LTBL(MEDIAN)	!Lower threshold

         DO I=1,IEND
            IDN = BUF(I)
            IF (MEDIAN-IDN.LE.LTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

C     ....Add all samples greater than median
   20    IBEG = MIX + 1
         HTHRESH = HTBL(MEDIAN)	!Upper threshold

         DO I=IBEG,NP
            IDN = BUF(I)
            IF (IDN-MEDIAN.LE.HTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

         IF (N.LT.NI) THEN
            IDN = (FLOAT(NI)*ISUM)/N + 0.5
         ELSE
            IDN = ISUM
         ENDIF

         RETURN
      END

C Just use the median value in the buffer
C
      SUBROUTINE GETMED(BUF,NI,NP,N,IDN,LTBL,HTBL)

         INTEGER BUF(NI)

         INTEGER*2 LTBL(0:4095),HTBL(0:4095)
         INTEGER HTHRESH

         IF (NP.EQ.0) THEN		!If all samples are zero
            N = 0
            IDN = 0		!return zero
            RETURN
         ELSEIF (NP.EQ.1) THEN	!If only one non-zero sample
            N = 1
            IDN = BUF(1)	!then no screening necessary
            RETURN
         ENDIF

C     ....Here if at least two non-zero samples are in BUF
         CALL SORTIN(BUF,NP)	!Sort the samples
         MIX = (NP+1)/2		!Index of median
         MEDIAN = BUF(MIX)		!Median DN value
         IDN = MEDIAN		!Initialize sum

         RETURN
      END



C Print input label
C
      SUBROUTINE LABPROC(IUNI,PROJECT,ISTATUS,SGC,MINT)

         INTEGER LBUF(80)

         CHARACTER*5 PROJECT
         CHARACTER*132 MSG

C     ...Galileo gain ratios for 400K  100K  40K  10K
C
         REAL*4 GLL_GAIN_RATIO(4)
         DATA GLL_GAIN_RATIO/47.091,9.809,4.799,1.0/

         INTEGER MINTHRESH(4)  !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

         REAL*4 GLL_GAIN_CONSTANT !electrons/DN at 10K
         DATA GLL_GAIN_CONSTANT/42.3/ 
 
         EQUIVALENCE (EXPO,IEXPO)


  101    FORMAT(' CAMERA=',I4,' FRAME=',I9,' FILTER=',I1,' GAIN=',I1,
     &    ' EXP=',F8.1,' RATE=',I3)

         IF (ISTATUS.NE.1) THEN
            CALL GETLABCON(IUNI,PROJECT,lbuf,ind)
            ICAM = LBUF(6)
            IFRAME = LBUF(2)
            IFILT = LBUF(4)
            IGAIN = LBUF(7)
            IEXPO = LBUF(3)
            IRATE = LBUF(5)
            WRITE(MSG,101,ERR=10) ICAM,IFRAME,IFILT,IGAIN,EXPO,IRATE
   10       CALL XVMESSAGE (MSG,' ')
         ENDIF

         IF (PROJECT.EQ.'GLL') THEN
            SGC = GLL_GAIN_CONSTANT*GLL_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
            IF (MOD(LBUF(17),10).EQ.1)
     &         CALL XVMESSAGE ('EXTENDED-EXPOSURE',' ')
         ELSE
            SGC = 1.
            MINT = 3
         ENDIF
         RETURN
      END



C
C Check Cassini input label
C
      SUBROUTINE CASSIPROC(IUNI,ISTATUS,SGC,MINT,CAMERA,GAIN,MODE,
     &  EXPOS,FILT1,FILT2,SCLK)

         INCLUDE 'cas_isslab'
c         INCLUDE 'cas_isslab.fin'  ! remove before delivery

         INTEGER IND, EXPOS, SCLK

         CHARACTER*132 MSG
         CHARACTER*5 CAMERA, FILT1, FILT2, GAIN
         CHARACTER*4 MODE

C     ...Cassini gain ratios for 1400K  400K  100K  40K
C
         INTEGER MINTHRESH(4)     !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

c         REAL*4 CASSI_GAIN_RATIO(4)
c         DATA CASSI_GAIN_RATIO/17.1,7.9,2.4,1.0/

c         REAL*4 CASSI_GAIN_CONSTANT     !electrons/DN at 24K
c         DATA CASSI_GAIN_CONSTANT/12.6/

c     ...Use System Gain Constant as calculated by CISSCAL/DECAL
	 REAL*4 CASSI_SGC_NAC1(4), CASSI_SGC_NAC2(4)
	 REAL*4 CASSI_SGC_WAC1(4), CASSI_SGC_WAC2(4)
         DATA CASSI_SGC_NAC1/233.04, 98.88, 30.27, 12.85/
         DATA CASSI_SGC_NAC2/219.80, 99.32, 29.15, 13.93/
         DATA CASSI_SGC_WAC1/210.566, 85.09, 27.68, 11.85/
         DATA CASSI_SGC_WAC2/194.30, 90.13, 27.66, 11.74/

         IF (ISTATUS.EQ.1) THEN
            CALL XVMESSAGE
     &         ('Must have a correct Cassini Vicar label',' ')
            CALL ABEND()
         ELSE
            CALL ABLE97(IND,IUNI)
            SCLK = LAB_SCLK
            CAMERA = LAB_CAMERA
            GAIN = LAB_GAIN
            MODE=LAB_MODE
            EXPOS = LAB_EXPOS
            FILT1 = LAB_FILTER1
            FILT2 = LAB_FILTER2

            WRITE
     &        (MSG,201,ERR=20)SCLK,CAMERA,GAIN,MODE,EXPOS,FILT1,FILT2

  201       FORMAT('SCLK=',I9,' CAM=',A5,' GAIN=',A5,' MODE=',A4,
     &' EXP=',I7,' FILT1=',A5,' FILT2=',A5)

   20       CALL XVMESSAGE (MSG,' ')
            IF (LAB_GAIN .EQ. '1400K'
     &          .OR. LAB_GAIN(1:3) .EQ. '215') THEN
               IGAIN = 1
            ELSEIF (LAB_GAIN .EQ. '400K'
     &              .OR. LAB_GAIN(1:2) .EQ. '95') THEN
               IGAIN = 2
            ELSEIF (LAB_GAIN .EQ. '100K'
     &              .OR. LAB_GAIN(1:2) .EQ. '29') THEN 
               IGAIN = 3
            ELSE
               IGAIN = 4
            ENDIF
            IF (LAB_CAMERA .EQ. 'ISSNA') THEN
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_NAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_NAC2(IGAIN)
               ENDIF
            ELSE
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_WAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_WAC2(IGAIN)
               ENDIF
            ENDIF
c            SGC = CASSI_GAIN_CONSTANT*CASSI_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
         ENDIF
         RETURN
      END
