      include 'VICMAIN_FOR'
C VICAR program HICCUP
C 5-SEPT-1994 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C        HICCUP  INP=PIC  OUT=HIS  user-parameters...
      SUBROUTINE MAIN44
      IMPLICIT NONE
      REAL*4 MEAN(3),SIGMA(3),LSAT(3),HSAT(3),ISCALE(3),OSCALE(3)
      REAL*4 SCALE,MAXOSCALE
      INTEGER*4 MAXFREQ(3),NPTS(3)
      INTEGER*4 HIST(-32768:32767,3)
      INTEGER*4 OHIST(0:255,3)
      INTEGER*4 DEF,NI,NO
      INTEGER*4 I,J,IUNIT(3),IND,ITYPE,NPIXELS
      INTEGER*4 SL,SS,NL,NS
      INTEGER*2 IBUF(32768)
      CHARACTER*10 SCLTYPE
      CHARACTER*8 FORMAT(3)

      CALL IFMESSAGE ('HICCUP version Feb 11, 1998')
      CALL XVPCNT('INP',NI)
      CALL XVPCNT('OUT',NO)
      IF (NI.NE.NO) GOTO 900
      CALL XVP('SCALE',SCLTYPE,DEF)!Now a parameter

      DO I = 1,NI
         CALL XVUNIT(IUNIT(I),'INP',I,IND,' ')
         CALL XVOPEN(IUNIT(I),IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVGET(IUNIT(I),IND,'FORMAT',FORMAT(I),' ')
         IF (FORMAT(I).NE.FORMAT(1)) GOTO 904
      ENDDO

      DO I = 1,NI
         CALL GET_SIZE(IUNIT(I),sl,ss,nl,ns,ind)
         IF (IND.EQ.0) GOTO 998
         NPTS(I) = NL*NS
         CALL COMPUTE_HIST(FORMAT(1),SL,SS,NL,NS,iunit(i),ohist(0,i),
     &                     hist(-32768,i),ibuf,itype)
         CALL COMPUTE_SCALE(IUNIT(I),NPTS(I),ITYPE,OHIST(0,i),
     &           HIST(-32768,i),FORMAT(1),
     &		 SCLTYPE,iscale(i),oscale(i),mean(i),sigma(i),
     &           maxfreq(i),lsat(i),hsat(i))
         CALL XVCLOSE(IUNIT(I),IND,' ')
      ENDDO

      MAXOSCALE = OSCALE(1)
      DO I=1,NI
        IF (OSCALE(I).GT.MAXOSCALE) MAXOSCALE=OSCALE(I)
      ENDDO

      DO I=1,NI
         IF (ITYPE.GT.1) THEN
            MEAN(I) = ISCALE(I)*MEAN(I)
            SIGMA(I) = ISCALE(I)*SIGMA(I)
            SCALE = ISCALE(I)/MAXOSCALE
            CALL HISCALE(HIST(-32768,I),NPTS(I),SCALE,ohist(0,i),
     &                   lsat(i),hsat(i))
            MAXFREQ(I) = 0
            DO J=1,254
               NPIXELS = OHIST(J,I)
               IF (NPIXELS.GT.MAXFREQ(I)) MAXFREQ(I)=NPIXELS
            ENDDO
         ENDIF
         CALL WRITE_HIST(MAXOSCALE,LSAT(I),HSAT(I),MAXFREQ(I),NPTS(I),
     & 		         MEAN(I),SIGMA(I),SCLTYPE,OHIST(0,I),I)

         CALL PRINT_HIST_STATS(MAXOSCALE,LSAT(I),HSAT(I),MAXFREQ(I),
     & 		  NPTS(I),MEAN(I),SIGMA(I),SCLTYPE,ITYPE,OHIST(0,I))
      ENDDO

      CALL XVMESSAGE('HICCUP task completed',' ')
      RETURN
900   CALL XVMESSAGE('***Number of inputs and outputs must equal',' ')
      GOTO 998
904   CALL XVMESSAGE('***Inputs must have the same format',' ')
998   CALL XVMESSAGE('***HICCUP task cancelled',' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to return the location and dimensions of the cutout window
C
      SUBROUTINE GET_SIZE(UNIT,sl,ss,nl,ns,ind)
      INTEGER*4 UNIT		!Logical unit number of input image
      INTEGER*4 SL,SS,NL,NS	!Output size field
      INTEGER*4 IND		!Return status (1=success, 0=failure

      INTEGER*4 SIZE(4),INSTANCE,NHIST/1/
      CHARACTER*32 TASK1
      CHARACTER*80 MSG
      LOGICAL XVPTST

      IND = 0
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF (SL+NL-1 .GT. NLI) THEN
         CALL XVMESSAGE
     &     ('***Number of lines requested exceeds input size',' ')
         RETURN
      ENDIF
      IF (SS+NS-1 .GT. NSI) THEN
         CALL XVMESSAGE
     &     ('***Number of samples requested exceeds input size',' ')
         RETURN
      ENDIF
      IF (NS.GT.32768) THEN
         CALL XVMESSAGE
     &     ('***Input image number of samples exceeds limit',' ')
         RETURN
      ENDIF

      IND = 1
      IF (.NOT.XVPTST('USECOW')) RETURN
      CALL XLHINFO(UNIT,TASK1,INSTANCE,NHIST,IND,' ')
      CALL XLGET(UNIT,'HISTORY','CUT_OUT_WINDOW',size,ind,
     &	'NELEMENT',4,'FORMAT','INT','HIST',TASK1,' ')
      IF (IND.NE.1) RETURN
      SL = SIZE(1)
      SS = SIZE(2)
      NL = SIZE(3)
      NS = SIZE(4)
      WRITE(MSG,101) SL,SS,NL,NS
  101 FORMAT('CUT_OUT_WINDOW=(',I4,',',I4,',',I4,',',I4,')')
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Routine to compute the histogram of the input image
C
      SUBROUTINE COMPUTE_HIST(FORMAT,SL,SS,NL,NS,iunit,ohist,
     &                        hist,ibuf,itype)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 IUNIT,ITYPE
      INTEGER*4 HIST
      INTEGER*4 OHIST(0:255)
      INTEGER*4 SL,SS,NL,NS
      INTEGER*2 IBUF
C     ....Compute histogram of input image.  If the image is byte
C     ....the histogram is stored directly in OHIST.  If halfword, 
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.
      IF (FORMAT.EQ.'BYTE') THEN
         ITYPE = 1
         CALL COMPHIST(IUNIT,SL,SS,NL,NS,OHIST,IBUF)
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         ITYPE = 2
         CALL COMPHIST2(IUNIT,SL,SS,NL,NS,HIST,IBUF)
      ELSE
         GOTO 990
      ENDIF
      RETURN
990   CALL XVMESSAGE('***Invalid input data format',' ')
      CALL XVMESSAGE('***Inputs must be byte or halfword',' ')
      GOTO 999
999   CALL XVMESSAGE('***HICCUP task cancelled',' ')
      CALL ABEND
      RETURN
      END

C
C  Routine to compute the scale of the input image
C
      SUBROUTINE COMPUTE_SCALE(IUNIT,NPTS,ITYPE,OHIST,HIST,FORMAT,
     &		SCLTYPE,iscale,oscale,mean,sigma,maxfreq,lsat,hsat)
      IMPLICIT NONE
      REAL*4 MEAN,SIGMA,ISCALE,OSCALE,LSAT,HSAT
      INTEGER*4 IUNIT,ITYPE
      INTEGER*4 ICAM,IFDS,IND,MINDN,MAXDN
      INTEGER*4 MAXFREQ,NPTS
      INTEGER*4 HIST(-32768:32767),LABUF(80)
      INTEGER*4 OHIST(0:255) 
      CHARACTER*10 SCLTYPE
      CHARACTER*8 FORMAT
      CHARACTER*5 PROJECT
      LOGICAL XVPTST

      IF (SCLTYPE.EQ.'IOF') ITYPE=3
      IF (SCLTYPE.EQ.'RADIANCE') ITYPE=4
      IF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
         IF (FORMAT.EQ.'BYTE') GOTO 992
         CALL GETPROJ(IUNIT,project,icam,ifds,ind)
         IF (IND.EQ.1) GOTO 998
         CALL GETLABCON(IUNIT,PROJECT,labuf,ind)
      ENDIF

      IF (ITYPE.EQ.1) THEN
         CALL HISTAT(OHIST,NPTS,mean,sigma,maxfreq)
         ISCALE = 1.0
         OSCALE = 1.0
         LSAT = (100.0*OHIST(0))/NPTS
         HSAT = (100.0*OHIST(255))/NPTS
      ELSE
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL HISTAT2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
         CALL GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      RETURN
992   CALL XVMESSAGE
     &     ('***Keywords IOF and RADIANCE invalid for byte data',' ')
998   CALL XVMESSAGE('***HICCUP task cancelled',' ')
      CALL ABEND
      RETURN
      END



      SUBROUTINE WRITE_HIST(OSCALE,LSAT,HSAT,MAXFREQ,NPTS,MEAN,SIGMA,
     &			    SCLTYPE,OHIST,I)
      IMPLICIT NONE
      REAL*4 MEAN,SIGMA,LSAT,HSAT,OSCALE
      CHARACTER*10 SCLTYPE
      INTEGER*4 MAXFREQ,NPTS,I
      INTEGER*4 OHIST(0:255) 

      INTEGER*4 COLUMNS,ROWS,IBIS_PTR,STATUS
      INTEGER*4 DEF,IND,OUNIT
      CHARACTER*5  IBISFRMT(1)		!Must be an *array* of strings
      CHARACTER*10 ORGANIZATION
      CHARACTER*32 INSTANCE_NAME	!Must be at least 32
      CHARACTER*20 SUBTYPE_NAME
      CHARACTER*40 MEMBER_NAME

      COLUMNS=1
      ROWS=256
      IBISFRMT(1)='FULL'
      ORGANIZATION='COLUMN'
      MEMBER_NAME='HISTOGRAM'
      SUBTYPE_NAME='STATISTICS'
      CALL XVP('INSTANCE',INSTANCE_NAME,DEF)!Now a parameter
C     ....Get unit number and open output image
      CALL XVSELPI(I)
      CALL XVUNIT(OUNIT,'OUT',I,IND,' ')
      CALL ibis_file_open(OUNIT,IBIS_PTR,'write',COLUMNS,ROWS,
     &                  IBISFRMT,ORGANIZATION,STATUS)
      IF(STATUS.NE.1) CALL ibis_signal_u(OUNIT,STATUS,1)
      CALL ibis_file_set(IBIS_PTR,'type',SUBTYPE_NAME,STATUS)
      IF(STATUS.NE.1) CALL ibis_signal(IBIS_PTR,STATUS,1)

C -- Write the column data
      call ibis_column_write(ibis_ptr,OHIST,COLUMNS,
     +                       1,256,status)
      if (status.ne.1) call ibis_signal(ibis_ptr,status,1)

C -- Install the column ID into the HISTOGRAM member of the
C    C_STATISTICS class. The instance should be a PDF parameter.
C    The parameters entered are the COLUMNS array, followed
C    by the count of the array (1). This member has no INDEX
C    columns so we leave those parameters as 0,0.
      CALL icl_new_statistics(IBIS_PTR,COLUMNS,1,0,0,
     &                    MEMBER_NAME,INSTANCE_NAME,STATUS)
      IF(STATUS.LT.0) CALL ibis_signal(IBIS_PTR,STATUS,1)

C -- Install the STATISTICS property values. Needed to 
C    change FORMAT to 'FORMAT', and add 'PROPERTY','STATISTICS'.
c	
      CALL XLADD(OUNIT,'PROPERTY','SCALE_TYPE',SCLTYPE,IND,
     &           'FORMAT','STRING','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','OUTPUT_HISTOGRAM_SCALE',OSCALE,IND,
     &           'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','PERCENT_SATURATION_AT_LOW_VALUE',
     &           LSAT,IND,'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','PERCENT_SATURATION_AT_HIGH_VALUE',
     &           HSAT,IND,'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','MAXIMUM_FREQUENCY',MAXFREQ,
     &           IND,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','NUMBER_OF_PIXELS_IN_HISTOGRAM',NPTS,
     &           IND,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',256,
     &           IND,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','MEAN_VALUE',MEAN,IND,'FORMAT',
     &           'REAL','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','STANDARD_DEVIATION_VALUE',SIGMA,IND,
     &           'FORMAT','REAL','PROPERTY','STATISTICS',' ')

      CALL IBIS_FILE_CLOSE(IBIS_PTR,0,STATUS)
      IF(STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PTR,STATUS,1)

      RETURN
      END


      SUBROUTINE PRINT_HIST_STATS(OSCALE,LSAT,HSAT,MAXFREQ,NPTS,MEAN,
     & 				  SIGMA,SCLTYPE,ITYPE,OHIST)
      IMPLICIT NONE
      CHARACTER*10 SCLTYPE
      REAL*4 MEAN,SIGMA,LSAT,HSAT,OSCALE
      INTEGER*4 MAXFREQ,NPTS,ITYPE
      INTEGER*4 OHIST(0:255) 
      INTEGER*4 ICNT,ISPIKE,IMODE
      LOGICAL XVPTST
      CHARACTER*80 MSG

      IF (XVPTST('PHIST')) THEN
         CALL XVP('SPIKES',ISPIKE,ICNT)
         IMODE = 0
         IF (XVPTST('ZEROES')) IMODE=1
         IF (ITYPE.EQ.1) CALL PHIST(OHIST,NPTS,0,255,ISPIKE,IMODE)
         IF (ITYPE.GT.1) CALL PHIST2(OHIST,ITYPE,OSCALE,
     &		NPTS,ISPIKE,IMODE)
      ENDIF
C     ....Print mean, standard deviation, and total number of pixels
      WRITE (MSG,803) MEAN,SIGMA,NPTS
      CALL XVMESSAGE(MSG,' ')
C     ....Print histogram scale
      IF (ITYPE.LE.2) THEN
         CALL XVMESSAGE('Histogram scale is DN',' ')
      ELSE IF (ITYPE.EQ.3) THEN
         WRITE (MSG,804) OSCALE
         CALL XVMESSAGE(MSG,' ')
      ELSE
         WRITE (MSG,805) OSCALE
         CALL XVMESSAGE(MSG,' ')
      ENDIF
C     ....Print number of pixels saturated at low and high ends
      IF (LSAT+HSAT.GT.0) THEN
         WRITE(MSG,800) LSAT
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,801) HSAT
         CALL XVMESSAGE(MSG,' ')
      ENDIF
  800 FORMAT('Percent saturation at low end of histogram=',F6.2)
  801 FORMAT('Percent saturation at high end of histogram=',F6.2)
  803 FORMAT('Mean=',F14.8,'   Sigma=',F14.8,
     &   '   Total number of pixels=',I10)
  804 FORMAT('Histogram scale is ',F14.8,' IOF')
  805 FORMAT('Histogram scale is ',F14.8,
     &   ' nanowatts/cm**2/steradian/nanometer')

      RETURN
      END


C
C Routine to print the scaled histogram. 
C
      SUBROUTINE PHIST2(OHIST,ITYPE,SCALE,NPTS,ISPIKE,IMODE)
      INTEGER OHIST(0:255),SPIKE,MAXT,CNT
      CHARACTER*8 TYPE(4)
      CHARACTER*131 LISTO
      CHARACTER*132 FMT1
      CHARACTER*133 MSG
      DATA TYPE/'   DN   ','   DN   ','   IOF  ','RADIANCE'/

      LISTO(1:131) = ' '
      MAXT = '7FFFFFFF'X
C
108   FORMAT(A8,'   FREQ  PERCENT   ',
     &  '0        10        20        30        40        50       60',
     &  '        70        80        90       100')
      WRITE(MSG,108) TYPE(ITYPE)
      CALL XVMESSAGE(MSG,' ')
      LISTO(130:130) = '*'
      SPIKE = ISPIKE + 1
C     ....Search for n+1st highest freq, ignoring lowest and highest levels.
   10 MAXS = MAXT
      DO J=1,SPIKE
         MAX = 0
         DO I=1,254
            IF (OHIST(I).GT.MAX.AND.OHIST(I).LT.MAXS) MAX=OHIST(I)
         ENDDO
         MAXS = MAX
      ENDDO

      IF (MAX.EQ.0) THEN		!If max frequency is zero
         IF (SPIKE.GT.1) THEN           !and spikes.gt.1 then
            SPIKE = SPIKE - 1           !reduce number of spikes
            GOTO 10                     !and try again
         ELSE
            MAX=MAX0(OHIST(0),OHIST(254))   !otherwise, use the ends.
            IF (MAX.EQ.0) GOTO 990     !If all levels zero, print err.
         ENDIF
      ENDIF
C     ....If ITYPE=1 or 2, the scale is an integer
      ISCALE = SCALE + 0.5
C     ....If ITYPE=3 or 4, determine number of fractions to print in
C     ....output DNs.
      NFRACT=0
      TEST = 1000.0
      DO I=1,10
         IF (SCALE.LT.TEST) NFRACT=I
         TEST = TEST/10.0
      ENDDO

      IFLAG = 0
C
      DO 100 IDN=0,255
      IFREQ = OHIST(IDN)
      IF (IFREQ.EQ.0.AND.IMODE.EQ.0) THEN
         IF (IFLAG.EQ.0) CALL XVMESSAGE(' ',' ')
         IFLAG = 1
         GOTO 100
      ENDIF
      IFLAG = 0
      PERCEN = (100.*IFREQ)/NPTS
      IVAL = MAX0(100*IFREQ/MAX+1,1)	!Sample length of histogram line
      NCHAR = 129			!Number of characters to print
      IF (IVAL.GT.101) THEN
         IVAL = 101
         NCHAR = 131			!Add room for spike *
      ENDIF
      IF (ITYPE.LT.3) THEN
         WRITE (LISTO(1:5),'(I5)')IDN*ISCALE
      ELSE
         WRITE (FMT1,'(''(F9.'',I4.4,'')'')') NFRACT
         WRITE (LISTO(1:10),FMT1)IDN*SCALE
      ENDIF
      WRITE (LISTO(9:15),'(I7)') IFREQ
      WRITE (LISTO(17:24),'(F8.3)') PERCEN+.0005
      LISTO(25:129) = ' '
      DO CNT=38,128,10
         LISTO(CNT:CNT) = '+'
      ENDDO
      DO CNT=28,28+IVAL-1
         LISTO(CNT:CNT) = '*'
      ENDDO
      CALL XVMESSAGE(LISTO(1:NCHAR),' ')
  100 CONTINUE
C
      RETURN
C
  990 CALL XVMESSAGE('***Histogram contains all zeroes',' ')
      RETURN
      END
