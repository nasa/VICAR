      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      COMMON/C1/HIST(-32768:32767),IBUF(32768)
      INTEGER*4 HIST
      INTEGER*2 IBUF

      COMMON/C2/OBUF(10),OHIST(0:255)	!Histogram record data structure
      INTEGER*4 OHIST
      INTEGER*4 BUF(10)
      EQUIVALENCE (BUF,OBUF)

      LOGICAL XVPTST
      INTEGER*4 SL,SS,EL
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
      CHARACTER*80 MSG


  803 FORMAT(' Mean=',F14.8,'   Sigma=',F14.8,
     &   '   Total number of pixels=',I10)

C     ....Get unit number and open input image
      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      IF (NS.GT.32768) GOTO 904
      NPTS = NL*NS
C     ....Compute histogram of input image.  If the image is byte
C     ....the histogram is stored directly in OHIST.  If halfword, 
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.

      EL = SL+NL-1
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.EQ.'BYTE') THEN
         ITYPE = 1
C     ....Generate histogram
         DO LINE=SL,EL
            CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &                 'NSAMPS',NS,' ')
            CALL HSUB(1,NS,IBUF,OHIST)
         ENDDO
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         ITYPE = 2
         DO LINE=SL,EL
            CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &                 'NSAMPS',NS,' ')
            DO J=1,NS
              IDN = IBUF(J)
              HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDDO
      ELSE
         GOTO 990
      ENDIF

      IF (ITYPE.EQ.1) THEN
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
C     ....Print mean, standard deviation, and total number of pixels
c         CALL QPRINT(' ***Test from Fortran')
          CALL XVMESSAGE(' ***Test from Fortran',' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c        CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
         mean=0.0
         sigma=0.0
         maxfreq=0
         CALL tzhistat(OHIST,NPTS,mean,sigma,maxfreq)
c         CALL QPRINT(' ***Test from C')
         CALL Xvmessage(' ***Test from C', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
      ELSE
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C     ....Print mean, standard deviation, and total number of pixels
c         CALL QPRINT(' ***Test from Fortran')
         CALL Xvmessage(' ***Test from Fortran', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
         mean=0.0
         sigma=0.0
         mindn=0
         maxdn=0
         maxfreq=0
         CALL tzhistat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
c         CALL QPRINT(' ***Test from C')
         CALL Xvmessage(' ***Test from C', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
      ENDIF

      RETURN

c  902 CALL QPRINT(' ***Number of lines requested exceeds input size')
  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
c  903 CALL QPRINT(' ***Number of samples requested exceeds input size')
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
c  904 CALL QPRINT(' ***Input image number of samples exceeds limit')
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
c  990 CALL QPRINT(' ***Invalid input data format')
  990 CALL Xvmessage(' ***Invalid input data format',' ')
c      CALL QPRINT(' ***Inputs must be byte or halfword')
      CALL Xvmessage(' ***Inputs must be byte or halfword',' ')
      GOTO 999
c  999 CALL QPRINT(' thistat task cancelled')
  999 CALL Xvmessage(' thistat task cancelled', ' ')
      CALL ABEND
      END
