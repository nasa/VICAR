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
         CALL Xvmessage(' ***Test from Fortran', ' ')
         CALL COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
         CALL Xvmessage(' ***Test from C', ' ')
         CALL tzCOMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         CALL Xvmessage(' ***Test from Fortran', ' ')
         CALL COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
         CALL Xvmessage(' ***Test from C', ' ')
         CALL tzCOMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
      ELSE
         GOTO 990
      ENDIF



      RETURN

  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
  990 CALL Xvmessage(' ***Invalid input data format',' ')
      CALL Xvmessage(' ***Inputs must be byte or halfword',' ')
      GOTO 999
  999 CALL Xvmessage(' thistat task cancelled', ' ')
      CALL ABEND
      END

