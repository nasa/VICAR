CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute or read histogram of input image
C
      SUBROUTINE GET_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 BUF(*)		!Image line buffer
      REAL*4 TMEAN,TSIGMA	!Mean and sigma
      INTEGER*4 NPTS		!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 NI

      CALL ZIA(hist,65536)	!Zero out histogram
      CALL XVPCNT('INP',NI)     !Number of input files.
      IF (NI.EQ.2) THEN		!Histogram supplied as second input
         CALL READ_HISTOGRAM(hist,tmean,tsigma,npts)
      ELSE			!Else, calculate histogram from input image
         CALL COMPUTE_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      END IF

      IF (NPTS.LT.1) THEN
         CALL XVMESSAGE('***Histogram is empty',' ')
         CALL ABEND
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read histogram from 2nd input file (must be IBIS format).
C
      SUBROUTINE READ_HISTOGRAM(hist,tmean,tsigma,npts)
      IMPLICIT NONE
      INTEGER*4 HIST(-32768:32767)
      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NPTS			!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,IDN,NLEV
      INTEGER*4 IUNIT2,IBIS,STAT,STATUS,NL2,NS2

      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL IBIS_FILE_OPEN(IUNIT2,IBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_GET(IBIS,'NR',NS2,1,1)
      CALL IBIS_FILE_GET(IBIS,'NC',NL2,1,1)        
      CALL XLGET(IUNIT2,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',
     &        nlev,status,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLGET(IUNIT2,'PROPERTY','MEAN_VALUE',tmean,status,
     &           'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLGET(IUNIT2,'PROPERTY','STANDARD_DEVIATION_VALUE',tsigma,
     &           status,'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL ZIA(HIST(INMIN),INMAX-INMIN+1)
      DO I=1,NL2
         CALL IBIS_COLUMN_READ(IBIS,HIST((I-1)*NS2-3),I,1,NS2,STATUS)
         IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      ENDDO
      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE(IUNIT2,STAT,' ')
      HIST(-3) = 0
      HIST(-2) = 0
      HIST(-1) = 0
      NPTS=0
      DO IDN=0,NLEV-1
         NPTS = NPTS + HIST(IDN)
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute histogram of input image
C
      SUBROUTINE COMPUTE_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      IMPLICIT NONE
      LOGICAL*1 BUF(*)
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NPTS		!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

      INTEGER*4 LINC,ICOUNT,IDEF,STAT
      INTEGER*4 LINE,DCODE
      INTEGER*4 SLA,SSA,NLA,NSA,ELA
      INTEGER*4 IPARM(4)
      INTEGER*4 BAND
      CHARACTER*80 PRT
   60 FORMAT('(SL,SS,NL,NS) = (',I5,',',I5,',',I5,',',I5,')')

      CALL XVPARM('LINC',linc,icount,idef,1)	!Line subsampling
      CALL XVPARM('AREA',iparm,icount,idef,4)	!Image area
      IF (ICOUNT.EQ.4) THEN
         SLA=IPARM(1)
         SSA=IPARM(2)
         NLA=IPARM(3)
         NSA=IPARM(4)
         IF (SLA+NLA-1.GT.NLI .OR. SSA+NSA-1.GT.NSI) THEN
            CALL XVMESSAGE('Specified area exceeds size of input',' ')
            CALL XVMESSAGE('Area reduced',' ')
            IF (SLA+NLA-1.GT.NLI) NLA=NLI-SLA+1
            IF (SSA+NSA-1.GT.NSI) NSA=NSI-SSA+1
         END IF
         CALL XVMESSAGE('Histogram computed from sub-area',' ') 
         WRITE (PRT,60) SLA,SSA,NLA,NSA
         CALL XVMESSAGE(PRT,' ')
      ELSE
         SLA=SL
         SSA=SS
         NLA=NLO
         NSA=NSO
      END IF
      ELA=SLA+NLA-1
      CALL ZIA(hist,65536)	!Zero out the histogram
      DCODE = 1
      IF (FORMAT.EQ.'HALF') DCODE=2

      DO BAND=SB,SB+NBO-1
        DO LINE=SLA,ELA,LINC
         CALL XVREAD(IUNIT,buf,stat,'LINE',LINE,
     &			'SAMP',SSA,'NSAMPS',NSA,'BAND',BAND,' ')
         CALL HSUB(DCODE,NSA,buf,hist(inmin),INMIN,INMAX)
        ENDDO
      ENDDO
      CALL STATI(HIST,tmean,tsigma,npts)
      RETURN
      END
