      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     INTEGER*4 LABUF(80)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)

      INTEGER*4 OHIST(0:255)

      LOGICAL XVPTST
      INTEGER*4 SL,SS,EL
      REAL*4 ISCALE,SCALE,LSAT,HSAT
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
C     CHARACTER*5 PROJECT
      CHARACTER*80 MSG
  800 FORMAT(' Percent saturation at low end of histogram=',F6.2)
  801 FORMAT(' Percent saturation at high end of histogram=',F6.2)
  803 FORMAT(' Mean=',F14.8,'   Sigma=',F14.8,
     &   '   Total number of pixels=',I10)
  804 FORMAT(' Histogram scale is ',F14.8,' IOF')
  805 FORMAT(' Histogram scale is ',F14.8,
     &   ' nanowatts/cm**2/steradian/nanometer')

C     ....Get unit number and open input image
      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      IF (NS.GT.32768) GOTO 904
      NPTS = NL*NS

      EL = SL+NL-1

      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'HALF' .AND.  FORMAT .NE. 'WORD') GOTO 990

      ITYPE = 2

C     ...Generate histogram,(replace COMPHIST2, until COMPHIST2 is ported)
C     ....Compute histogram of input image
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.

      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &              'NSAMPS',NS,' ')
         DO J=1,NS
            IDN = IBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      IF (XVPTST('IOF')) ITYPE=3
      IF (XVPTST('RADIANCE')) ITYPE=4
C
C  ... The following comments need to be removed once routines are ported
C      IF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
C         CALL GETPROJ(IUNIT,project,icam,ifds,ind)
C         IF (IND.EQ.1) GOTO 999
C         CALL GETLABCON(IUNIT,PROJECT,labuf,ind)
C      ENDIF

       IF (.NOT.XVPTST('INCLUDE')) THEN
           NPTS = NPTS - HIST(-32768)
           HIST(-32768) = 0
       ENDIF
       CALL HISTAT2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C
C  ... The following comments need to be removed once GETSCALE is ported
C        CALL GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
C        IF (IND.EQ.0) GOTO 999
C
C  ... The following values are hard-coded, needs to be removed when GETSCALE
C      is ported

         ISCALE = 9.9999997e-05
         OSCALE = 3.9062500e-03

         MEAN =ISCALE*MEAN
         SIGMA = ISCALE*SIGMA
 	 SCALE = ISCALE/OSCALE

         CALL Xvmessage('Test from FORTRAN',' ')
         CALL HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
C     ....Print number of pixels saturated at low and high ends
      IF (LSAT+HSAT.GT.0) THEN
         WRITE(MSG,800) LSAT
         CALL Xvmessage(MSG,' ')
         WRITE(MSG,801) HSAT
         CALL Xvmessage(MSG,' ')
      ENDIF
      CALL PRNT(4,256,OHIST,'Output histogram=. ')

         CALL Xvmessage('Test from C',' ')
         lsat=0.0
         hsat=0.0
         CALL tzhiscale(HIST,NPTS,SCALE,ohist,lsat,hsat)
C     ....Print number of pixels saturated at low and high ends
      IF (LSAT+HSAT.GT.0) THEN
         WRITE(MSG,800) LSAT
         CALL Xvmessage(MSG,' ')
         WRITE(MSG,801) HSAT
         CALL Xvmessage(MSG,' ')
      ENDIF
      CALL PRNT(4,256,OHIST,'Output histogram=. ')

      MAXFREQ = 0
      DO I=1,254
         NPIXELS = OHIST(I)
         IF (NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
      ENDDO


C     ....Print mean, standard deviation, and total number of pixels
      WRITE (MSG,803) MEAN,SIGMA,NPTS
      CALL Xvmessage(MSG,' ')
C     ....Print histogram scale
      IF (ITYPE.LE.2) THEN
         CALL Xvmessage(' Histogram scale is DN',' ')
      ELSE IF (ITYPE.EQ.3) THEN
         WRITE (MSG,804) OSCALE
         CALL Xvmessage(MSG,' ')
      ELSE
         WRITE (MSG,805) OSCALE
         CALL Xvmessage(MSG,' ')
      ENDIF

      RETURN

  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
  990 CALL Xvmessage(' ***Invalid input data format',' ')
      CALL Xvmessage(' ***Inputs must be halfword',' ')
      GOTO 999
  999 CALL Xvmessage(' THISCALE task cancelled', ' ')
      CALL ABEND
      END
