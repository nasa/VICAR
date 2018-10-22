      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      INTEGER*4 LABUF(80)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)

      INTEGER*4 OHIST(0:255)

      LOGICAL XVPTST
      INTEGER*4 SL,SS
      REAL*4 ISCALE,SCALE,LSAT,HSAT
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
      CHARACTER*5 PROJECT
      
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
C     ....Compute histogram of input image.  If the image is byte
C     ....the histogram is stored directly in OHIST.  If halfword, 
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'HALF' .AND.  FORMAT .NE. 'WORD') GOTO 990
      IF (FORMAT.EQ.'BYTE') THEN
         ITYPE = 1
         CALL COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         ITYPE = 2
         CALL COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
      ELSE
         GOTO 990
      ENDIF
      IF (XVPTST('IOF')) ITYPE=3
      IF (XVPTST('RADIANCE')) ITYPE=4
      IF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
         IF (FORMAT.EQ.'BYTE') GOTO 992
         CALL GETPROJ(IUNIT,project,icam,ifds,ind)
         IF (IND.EQ.1) GOTO 999
         CALL GETLABCON(IUNIT,PROJECT,labuf,ind)
      ENDIF

      IF (ITYPE.NE.1) THEN
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL HISTAT2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)

         CALL Xvmessage('Test from Fortran',' ')
         CALL GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
         IF (IND.EQ.0) GOTO 999
	 SCALE = ISCALE/OSCALE
         CALL PRNT(7,1,iscale,'ISCALE=')
         CALL PRNT(7,1,oscale,'OSCALE=')
         CALL PRNT(7,1,scale,'SCALE=')

         CALL Xvmessage('Test from C',' ')
         iscale = 0.0
         oscale = 0.0
         CALL tzgetscale(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
         IF (IND.EQ.0) GOTO 999
	 SCALE = ISCALE/OSCALE
         CALL PRNT(7,1,iscale,'ISCALE=')
         CALL PRNT(7,1,oscale,'OSCALE=')
         CALL PRNT(7,1,scale,'SCALE=')

         MEAN =ISCALE*MEAN
         SIGMA = ISCALE*SIGMA

         CALL HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
         MAXFREQ = 0
         DO I=1,254
            NPIXELS = OHIST(I)
            IF (NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
         ENDDO
      ENDIF

      CALL PRNT(4,256,ohist,'OHIST=')
      CALL PRNT(7,1,lsat,'LSAT=')
      CALL PRNT(7,1,hsat,'HSAT=')
      CALL PRNT(7,1,mean,'MEAN=')
      CALL PRNT(7,1,sigma,'SIGMA=')
      CALL PRNT(4,1,maxfreq,'MAXFREQ=')

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
  992 CALL Xvmessage(' Keywords IOF and RADIANCE invalid for byte data',
     &               ' ')
  999 CALL Xvmessage(' Tgetscale task cancelled', ' ')
      CALL ABEND
      END
