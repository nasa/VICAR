C VICAR program HISTGEN
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      COMMON/C1/HIST(-32768:32767),IBUF(32768)
      INTEGER*4 BUF(32771),HIST
      INTEGER*4 OUNIT,ILOW,IHIGH
      INTEGER*2 IBUF
      REAL*8 DMEAN,DSIGMA
      REAL*4 RMEAN,RSIGMA
      CHARACTER*8 FORMAT
      CHARACTER*5 IBISFRMT(8)
      INTEGER  IBIS,NLO,NSO,STATUS
      CHARACTER*10 ORGANIZATION
      CHARACTER*32 INSTANCE_NAME
      CHARACTER*20 SUBTYPE_NAME
      CHARACTER*40 MEMBER_NAME
      CHARACTER*10 SCLTYPE
C
      EQUIVALENCE(BUF,HIST(-3))
C
      CALL XVMESSAGE(' HISTGEN version July 26, 1999',' ')
C     ... Initialize ibis constants
      do i=1,8
         IBISFRMT(i)='FULL'
      enddo
      ORGANIZATION='COLUMN'
      MEMBER_NAME='HISTOGRAM'
      INSTANCE_NAME='HISTOGRAM FOR MASKV OR VGRMASK'
      SCLTYPE='0 - MAXD'
      SUBTYPE_NAME='STATISTICS'
C     ... Open input file and ge parameters
      CALL OPENINP(iunit,format,ilow,ihigh,maxd,*999)
C     ....Determine size of output histogram file (NLOxNSO)
      NWORD = MAXD+4	!Total # of words for output histogram
      NSO = NWORD
      NLO = 1
C     ....Open output file
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
C     ..... Open ibis file
      call ibis_file_open(OUNIT,IBIS,'write',NLO,NSO,IBISFRMT,
     +                     ORGANIZATION,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUNIT,STATUS,1)
      CALL IBIS_FILE_SET(IBIS,'type',SUBTYPE_NAME,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C     .... Zero Array
      CALL ZIA(HIST(ILOW),IHIGH-ILOW+1)
C     .... Compute Histogram 
      CALL COMPHIST(IUNIT,FORMAT,hist,ibuf,npts,*999)
C     .... Comput mean & sigma
      CALL COMPSTAT(NPTS,ILOW,IHIGH,MAXD,hist,dmean,dsigma)
C     ....Write histogram
      BUF(1) = MAXD + 1		!Number of DN levels in output histogram
      BUF(2)= 1000.*DMEAN + .5
      BUF(3)= 1000.*DSIGMA + .5

      CALL IBIS_COLUMN_WRITE(IBIS,BUF,1,1,NSO,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C         ... Install column ID into the HISTOGRAM member ...
      call icl_new_statistics(IBIS,1,1,0,0,MEMBER_NAME,
     &                        INSTANCE_NAME,STATUS)
      IF (STATUS.LT.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)

C     ... Install the STATISTICS  property values
      CALL XLADD(OUNIT,'PROPERTY','SCALE_TYPE',SCLTYPE,IND,
     &       'FORMAT','STRING','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',
     &     BUF(1),IND,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      RMEAN = DMEAN
      CALL XLADD(OUNIT,'PROPERTY','MEAN_VALUE',RMEAN,IND,
     &       'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      RSIGMA = DSIGMA
      CALL XLADD(OUNIT,'PROPERTY','STANDARD_DEVIATION_VALUE',
     &     RSIGMA,IND,'FORMAT','REAL','PROPERTY','STATISTICS',' ')

      CALL IBIS_FILE_CLOSE(IBIS,0,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE (OUNIT,STATUS,' ')
      CALL XVMESSAGE(' HISTGEN task completed', ' ')
      RETURN

  999 CALL XVMESSAGE(' ***HISTGEN task cancelled', ' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open the input image, get data format, and determine input and output
C DN limits (ilow to ihigh and 0 to MAXD).
C
      SUBROUTINE OPENINP(iunit,format,ilow,ihigh,maxd,*)
C      COMMON/C2/FORMAT,ILOW,IHIGH
      CHARACTER*8 FORMAT	!Input data format (BYTE or HALF)
      INTEGER*4  ILOW,IHIGH

      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA', ' ')
C     ....Get FORMAT and MAXD
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT, ' ')
      IF (FORMAT.EQ.'BYTE') THEN
         MAXD = 255
         ILOW = 0
         IHIGH = 255
      ELSE IF (FORMAT.EQ.'HALF') THEN
         MAXD = 32767
         ILOW = -32768
         IHIGH = 32767
      ELSE
         CALL XVMESSAGE(' ***Invalid input data format', ' ')
         RETURN1
      ENDIF

      CALL XVPARM('MAXD',IVAL,ICOUNT,IDEF,1)
      IF (ICOUNT.EQ.1) MAXD=IVAL
      RETURN
      END
C Compute the histogram
C
      SUBROUTINE COMPHIST(IUNIT,FORMAT,hist,ibuf,npts,*)
C      COMMON/C2/FORMAT,ILOW,IHIGH
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     1 NS,' ')
         IF (FORMAT.EQ.'BYTE') THEN
            CALL HSUB(1,NS,IBUF,HIST(0),0,255)
         ELSE
            DO J=1,NS
               IDN = IBUF(J)
               HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDIF
      ENDDO

      NPTS = NL*NS
      RETURN

  902 CALL XVMESSAGE(' ***Number of lines requested exceeds input size'
     & ,' ')
      GOTO 999
  903 CALL XVMESSAGE(
     & ' ***Number of samples requested exceeds input size',' ')
  999 RETURN1
      END
C Compute mean and standard deviation.  Modify histogram to set all values
C below 0 DN to 0 DN and all values above MAXD to MAXD.
C
      SUBROUTINE COMPSTAT(NPTS,ILOW,IHIGH,MAXD,hist,dmean,dsigma)
      INTEGER*4 HIST(-32768:32767)
      REAL*8 DMEAN,DSIGMA
      CHARACTER*80 MSG
  800 FORMAT(' Number of DN values below 0=',I7)
  801 FORMAT(' Number of DN values above',I5,' =',I7)

      DMEAN = 0.0D0
      DSIGMA = 0.0D0
      NLOW = 0
      NHIGH = 0

      DO J=ILOW,IHIGH
         NPIXELS = HIST(J)
	 DN = J
         DMEAN = DMEAN + NPIXELS*DN
         DSIGMA = DSIGMA + NPIXELS*DN**2
         IF (J.LT.0) THEN
            NLOW = NLOW + NPIXELS
         ELSE
            IF (J.GT.MAXD) NHIGH=NHIGH+NPIXELS
         ENDIF 
      ENDDO

      DMEAN = DMEAN/NPTS
      DSIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
C     ....Modify histogram
      HIST(0) = HIST(0) + NLOW
      HIST(MAXD) = HIST(MAXD) + NHIGH
C     ....Print number of values outside DN range
      IF (NLOW+NHIGH.GT.0) THEN
         WRITE(MSG,800) NLOW
         CALL XVMESSAGE( MSG, ' ')
         WRITE(MSG,801) MAXD,NHIGH
         CALL XVMESSAGE( MSG, ' ')
      ENDIF

      RETURN
      END
