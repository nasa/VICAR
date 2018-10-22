C If input image is halfword, get scale of output histogram:
C   ITYPE=3 if IOF is specified.
C        =4 if RADIANCE is specified.
C        =2 otherwise.
C   ISCALE=input image scale:
C         =IOF value used in FICOR*10-4 if MAXIOF is specified,
C          or CONV value used in FICOR*10-9 if MAXRAD is specified,
C          or 1.0 if neither MAXIOF or MAXRAD is specified.
C   OSCALE=output histogram scale
C
      SUBROUTINE GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
      REAL*4 ISCALE
      REAL*4 LABUF(80)		!GETLABCON buffer
      REAL*4 MAXIOF,MAXRAD
      REAL*4 MAXS(40)/.00001,.00002,.00004,.00005,
     &		     .0001,.0002,.0004,.0005,
     &		     .001,.002,.004,.005,
     &		     .01,.02,.04,.05,
     &               .1,.2,.4,.5,
     &               1.,2.,4.,5.,
     &               10.,20.,40.,50.,
     &               100.,200.,400.,500.,
     &               1000.,2000.,4000.,5000.,
     &               10000.,20000.,40000.,50000./
      IND = 1
      IF (ITYPE.EQ.3) GOTO 30
      IF (ITYPE.EQ.4) GOTO 40
C     ....Here if ITYPE=2
      ISCALE = 1.0
      IDN = 256
      DO I=1,8
         IF (MAXDN.LE.IDN) GOTO 20
         IDN = 2*IDN
      ENDDO
   20 OSCALE = IDN/256.
      RETURN
C
C     ....Here if ITYPE=3
   30 ISCALE = LABUF(28)
      IF (ISCALE.EQ.0.0) GOTO 995
      MAXIOF = ISCALE*MAXDN		!Compute maximum IOF in histogram
      IF (MAXIOF.GT.1.0) MAXIOF=1.0	!Physical upper limit
      DO I=6,21
         IF (MAXIOF.LE.MAXS(I)) GOTO 31
      ENDDO
   31 IF (I.EQ.8) I=9
      MAXIOF = MAXS(I)
      OSCALE = MAXIOF/256.
      RETURN
C
C     ....Here if ITYPE=4
   40 ISCALE = LABUF(29)*1.E+12		!Get radiance scale of input image
      IF (ISCALE.EQ.0.0) GOTO 996
      MAXRAD = ISCALE*MAXDN		!Compute maximum radiance in histogram
      DO I=1,40
         IF (MAXRAD.LE.MAXS(I)) GOTO 41
      ENDDO
   41 MAXRAD = MAXS(I)
      OSCALE = MAXRAD/256.
      RETURN
C
  995 CALL XVMESSAGE(' ***Invalid use of IOF scale',' ')
      GOTO 998
  996 CALL XVMESSAGE(' ***Invalid use of RADIANCE scale',' ')
  998 CALL XVMESSAGE(' Input image was not radiometrically corrected',
     &' ')
      IND = 0
      RETURN
      END
