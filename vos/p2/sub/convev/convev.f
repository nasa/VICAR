C  CONVERT POINTS FROM LINE,SAMP TO LAT,LON AND THE REVERSE
C  MODE=2 L,S TO LAT,LON     MODE=1 LAT,LON TO L,S
C
      SUBROUTINE CONVEV(IND,DATA,IDATA,LINE,SAMP,LAT,LON,MODE,CONV)
      IMPLICIT NONE
      REAL DATA(40),LINE,SAMP,LAT,LON
      INTEGER IND,MODE,IDATA(40),CONV(*)

      INTEGER*4 PTYPE,ICAM,I,ITIEP,NAH,NAV,IX
      REAL*4 PTS(3),FLAG,OSLINE,OSSAMP,A,B,ELO
      DOUBLE PRECISION EMATRX(9),XC,ZC,TH,TH1,TH2,LAM,F,CAS,RP,RE,PSI,
     .                 OM(3,3),RS(3)
      character*5 project ! used in IS to OS conversions for 'GLL  '
      CHARACTER*4 CONVSTR ! USED IN FINDING ASCII IN CONV ARRAY.

      IND=0
      FLAG=-400.
      PTYPE = IDATA(39)			!Get projection type
C   Do some preliminary shuffling for certain map projections.

      IF(PTYPE.EQ.6)CALL MERCPATCH(DATA)
CCCCCCC      IF(PTYPE.EQ.9)CALL CYLPATCH(DATA)
      IF(PTYPE.EQ.10)CALL RECTPATCH(DATA)

C   Check parameters. exit if invalid.

      IF(MODE.LT.1.OR.MODE.GT.2) GO TO 99
      IF(PTYPE.LT.1.OR.PTYPE.GT.16) GO TO 98


      IF(PTYPE.EQ.7.OR.PTYPE.EQ.8.OR.PTYPE.EQ.16) GO TO 10

C  MAP PROJECTION MODE       Most common case uses TRANV.

      XC=DATA(1)
      ZC=DATA(2)
      TH=DATA(3)
      TH1=DATA(4)
      TH2=DATA(5)
      LAM=DATA(6)
      F=DATA(7)
      CAS=DATA(8)
      RP=DATA(25)
      RE=DATA(26)
      PSI=DATA(9)
      CALL TRANV(IND,PTYPE,MODE,XC,ZC,TH,TH1,TH2,LAM,F,CAS,
     *  LINE,SAMP,LAT,LON,RP,RE,PSI)
      RETURN

C  RAW OR OBJECT SPACE PICTURE     !!!! Other cases are handled according ...

10    CONTINUE

c If image space extract reseau matrix parameters
      IF(PTYPE.EQ.7) then
        CALL MVLC ( CONV, PROJECT, 5 )
        if(project.ne.'GLL  ')then
          DO 12 I=1,20
             CALL MVLC( CONV(I), CONVSTR, 4)
             IF(CONVSTR.EQ. 'TIEP')  ITIEP=I+2
             IF(CONVSTR.EQ. 'NAH ')  NAH=CONV(I+2)
             IF(CONVSTR.EQ. 'NAV ')  NAV=CONV(I+2)
12        CONTINUE
        else
           icam = CONV(3)	!GLL camera serial number
        endif
      ENDIF

      IF(MODE.EQ.1) GO TO 30            !!!! ... to MODE.

C  L,S TO LAT,LON

      DO IX=1,9
         EMATRX(IX) = 0.D0
      ENDDO
      EMATRX(1)=1.D0
      EMATRX(5)=1.D0
      EMATRX(9)=DBLE(DATA(26))/DBLE(DATA(25))

C  CONVERT POINT FROM I.S. TO O.S.
      IF(PTYPE.EQ.7) then
        call convisos(project,icam,line,samp,osline,ossamp,1,
     &		conv(itiep),nah+1,nav+1,ind)
        line=osline
        samp=ossamp      
      endif

      PTS(1)=DATA(26)
      PTS(2)=DATA(27)*DATA(30)
      PTS(3)=DATA(38)
      CALL MVE(4,18,DATA(1), OM, 1,1) !TRICK TO INSURE THIS DATA IS DOUBLE
      CALL MVE(4, 6,DATA(19),RS, 1,1) !PRECISION ALIGNED
      CALL IPPCOV(LAT,LON,LINE,SAMP,PTS,RS, OM,
     *  EMATRX,DATA(28),DATA(29),FLAG)
      IF(LAT.EQ.FLAG) IND=1
      LAT=LAT*180./3.141592654
      LON=360.-LON*180./3.141592654
      RETURN

C  LAT,LON TO L,S

30    CONTINUE
      A=DATA(27)*DATA(30)
      B=DATA(26)-DATA(25)
      ELO=360.-LON
      CALL MVE(4,18,DATA(1), OM, 1,1) !TRICK TO INSURE THIS DATA IS DOUBLE
      CALL MVE(4, 6,DATA(19),RS, 1,1) !PRECISION ALIGNED
      CALL CORCAV(IND,LAT,ELO,OM,RS,A,DATA(26),
     * B,LINE,SAMP,DATA(28),DATA(29),FLAG)

C  CHECK FOR BEHIND THE PLANET, IF TRUE RETURN IND=1
      IF (IND.EQ.99) THEN
	IND=1
	RETURN
      ELSE
	IND=0
      ENDIF
      IF(LINE.EQ.FLAG.AND.SAMP.EQ.FLAG) IND=1

C  CONVERT FROM O.S. TO I.S.
      IF(PTYPE.EQ.7) then
        osline=line
        ossamp=samp
        call convisos(project,icam,line,samp,osline,ossamp,0,
     &		conv(itiep),nah+1,nav+1,ind)
      endif

      RETURN

99    CALL XVMESSAGE('MODE NOT 1 OR 2 - CONVEV',' ')
      IND=1
      RETURN
98    CALL XVMESSAGE('ILLEGAL IMAGE TYPE - CONVEV',' ')
      IND=1
      RETURN
      END
