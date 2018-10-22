      INCLUDE 'VICMAIN_FOR'
C         MAPCOORD  INP
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INCLUDE 'pgminc'              ! FOR XQINI... TAE CONSTANTS & PARAMETERS
      INCLUDE 'mp_for_defs'
      COMMON/C0/DATA,CONV,TIEPOINTS,NPTS,PARBLOCK
      COMMON/C0/OUT_LAT,OUT_LON
      INTEGER*4 IDATA(40),CONV(4000),NPTS,PARBLOCK(500)
      REAL*4 DATA(40),RCONV(40),TIEPOINTS(2,80),OUT_LAT(80),OUT_LON(80)
      EQUIVALENCE (DATA,IDATA),(CONV,RCONV)

      REAL*4 FL,OAL,OAS,SCALE
      REAL*8 MP
      INTEGER*4 MPTYPE
      INTEGER UNIT,ICNT,IDEF,ISTAT
      INTEGER CORDTYPE
      INTEGER*4 CAMERA,FDS,ILAB(80)
      INTEGER NAH,NAV
      INTEGER BUF(200)			!SPICE buffer
      CHARACTER*5 PROJECT
      LOGICAL XVPTST

      CALL XVMESSAGE('MAPCOORD version May 18, 1998',' ')
      IF(XVPTST('LINSAM')) THEN
        CALL XVMESSAGE('GIVEN POINTS ARE TAKEN AS LINE-SAMP.',' ')
        CORDTYPE=2
      ELSE
        CALL XVMESSAGE('GIVEN POINTS ARE TAKEN AS LAT-LON.',' ')
        CORDTYPE=1
      ENDIF

      CALL XVPARM('COORDIN',TIEPOINTS,ICNT,IDEF,2*80)
      IF (IDEF .EQ. 1) THEN
        CALL XVMESSAGE('WARNING ::: NO LINE SAMPLE PROVIDED.',' ')
        CALL XVMESSAGE('            USING DEFAULT VALUES.',' ')
      ENDIF
      NPTS=ICNT/2

      CALL XVUNIT(unit,'INP',1,ISTAT,' ')
      CALL XVOPEN(UNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      IF (ISTAT .NE. 1) GOTO 900

      CALL GETPROJ(UNIT,project,camera,fds,istat)
      IF (ISTAT.NE.0) THEN
         CALL PRNT(4,1,ISTAT,'***GETPROJ error, ISTAT=.')
         PROJECT='     '
      ELSE
         CALL GETLABCON(UNIT,PROJECT,ILAB,ISTAT)
         IF (ISTAT.GT.1) GOTO 902
      ENDIF
      CALL XVMESSAGE(' LABEL SAYS THE PROJECT IS '//PROJECT,' ')

C     ....If image is map projected, get map projection label information
      CALL MP_INIT(MP,ISTAT)
      CALL MP_LABEL_READ(MP,UNIT,ISTAT)
      IF (ISTAT .EQ. MP_SUCCESS) THEN
         CALL MP_MPO2BUF(MP,data,istat)		!Convert data to CONVEV format
         IF (ISTAT .NE. MP_SUCCESS) GOTO 905
         GOTO 90
      ENDIF

C     ....Here if no map projection label.
      CALL GETSPICE4(PROJECT,-1,ILAB,buf,istat)
      IF (ISTAT.NE.1) GOTO 910
      CALL GETCAMCON(PROJECT,CAMERA,fl,oal,oas,scale,istat)
      IF (ISTAT.NE.0) GOTO 912
      CALL SEARC_DISTOR(UNIT,istat)
      MPTYPE = 8			!Object space
      IF (ISTAT.EQ.0) MPTYPE=7		!Image space 
      CALL SPICE2CONVEV(BUF,FL,OAL,OAS,SCALE,MPTYPE,data)
C     ....Obtain geoma parameters
      IF (MPTYPE.EQ.8) GOTO 90
      CALL XVMESSAGE('Creating nominal geom parameters',' ')
      CALL GETGEOM(UNIT,PROJECT,CAMERA,0,CONV,CONV,NAH,NAV,ISTAT)
      IF (ISTAT.NE.0) GOTO 914

   90 IF (PROJECT .EQ. 'GLL') THEN
         CALL MVCL(PROJECT,CONV,5)
         CALL MVE(4,1,CAMERA,CONV(3),1,1)
      ENDIF

      IF(CORDTYPE .EQ. 2) THEN
        CALL LS2LL
      ELSE
        CALL LL2LS
      ENDIF
      CALL XVMESSAGE('MAPCOORD task completed.',' ')
      RETURN

  900 CALL XVMESSAGE('***Error in XVOPEN',' ')
      GOTO 990
  902 CALL PRNT(4,1,ISTAT,'***GETLABCON error, ISTAT=.')
      GOTO 990
  905 CALL XVMESSAGE('***Error occured in MP_MPO2BUF',' ')
      GOTO 990
  910 CALL PRNT(4,1,ISTAT,'***Error in GETSPICE4, status=.')
      GOTO 990
  912 CALL PRNT(4,1,ISTAT,'***GETCAMCON: BAD ISTAT=.')
      GOTO 990
  914 CALL PRNT(4,1,ISTAT,'GETGEOM: BAD ISTAT=.')
  990 CALL XVMESSAGE('MAPCOORD task canceled.',' ')
      CALL ABEND
      END
C------------------------------------------------------------------------------
C Convert line,sample to latitude,longitude
C
      SUBROUTINE LS2LL
      COMMON/C0/DATA,CONV,TIEPOINTS,NPTS,PARBLOCK
      COMMON/C0/OUT_LAT,OUT_LON
      INTEGER*4 IDATA(40),CONV(4000),NPTS,PARBLOCK(500)
      REAL*4 DATA(40),RCONV(40),TIEPOINTS(2,80),OUT_LAT(80),OUT_LON(80)
      EQUIVALENCE (DATA,IDATA),(CONV,RCONV)

      CHARACTER*100 MSG
      CHARACTER*33 ERROR
      CHARACTER*75 MSG1,MSG2,MSG3,MSG4,M,M1,M2,M4

      DATA
     *   MSG1/'|----------------------||---------------------------|'/,
     *   MSG2/'|   OBJECT SPACE       ||      WEST IN DEGREES      |'/,
     *   MSG3/'|---------|------------||--------------|------------|'/,
     *   MSG4/'|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |'/,
     *   M1/' |'/,M2/'   ||'/,M/'|'/,M4/'   |'/

      DATA ERROR/'   ||  OFF TAR     |  OFF TARG  |'/
111   FORMAT(A1,F8.2,A2,F9.2,A5,F11.5,A4,F11.5,A2)
1111  FORMAT(A1,F8.2,A2,F9.2,A33)

      CALL XVMESSAGE(MSG1,0)
      CALL XVMESSAGE(MSG2,0)
      CALL XVMESSAGE(MSG3,0)
      CALL XVMESSAGE(MSG4,0)

      DO I=1,NPTS
         RL=TIEPOINTS(1,I)
         RJ=TIEPOINTS(2,I)
         CALL CONVEV(ISTAT,DATA,DATA,RL,RJ,RLAT,RLON,2,CONV)
         IF (ISTAT.NE.0) THEN
            WRITE(MSG,1111) M,RL,M1,RJ,ERROR 
            CALL XVMESSAGE(MSG3,0)
            CALL XVMESSAGE(MSG,0)
         ELSE
            OUT_LAT(I) = RLAT
            OUT_LON(I) = RLON
            WRITE(MSG,111) M,RL,M1,RJ,M2,RLAT,M4,RLON,M1
            CALL XVMESSAGE(MSG3,0)
            CALL XVMESSAGE(MSG,' ')
         ENDIF
      ENDDO

      CALL XVMESSAGE(MSG1,0)
C
C     ....Write output parameters
      CALL XQINI(PARBLOCK, 500, XABORT)
      CALL XQREAL(PARBLOCK, 'OUT_LAT', NPTS, OUT_LAT, XADD, ISTAT)
      CALL XQREAL(PARBLOCK, 'OUT_LON', NPTS, OUT_LON, XADD, ISTAT)
      CALL XVQOUT(PARBLOCK, ISTAT)
      CALL XVMESSAGE(' ',' ')
      RETURN
      END
C------------------------------------------------------------------------------
C Convert from lat-lon line-sample
C
      SUBROUTINE LL2LS
      COMMON/C0/DATA,CONV,TIEPOINTS,NPTS,PARBLOCK
      COMMON/C0/OUT_LAT,OUT_LON
      INTEGER*4 IDATA(40),CONV(4000),NPTS,PARBLOCK(500)
      REAL*4 DATA(40),RCONV(40),TIEPOINTS(2,80),OUT_LAT(80),OUT_LON(80)
      EQUIVALENCE (DATA,IDATA),(CONV,RCONV)

      CHARACTER*100 MSG
      CHARACTER*33 ERROR
      CHARACTER*75 MSG1,MSG2,MSG3,MSG4,M,M1,M2,M4

      DATA
     *   MSG1/'|---------------------------||----------------------|'/,
     *   MSG2/'|      WEST IN DEGREES      ||   OBJECT SPACE       |'/,
     *   MSG3/'|--------------|------------||---------|------------|'/,
     *   MSG4/'|  LATITUDE    |  LONGITUDE ||  LINE   |   SAMPLE   |'/,
     *   M1/' |'/,M2/' ||'/,M/'|'/,M4/'   |'/

      DATA ERROR/' ||  OFFTRG |   OFF TARG |'/
  112 FORMAT(A1,F11.5,A4,F11.5,A3,F8.2,A2,F9.2,A4)
 1112 FORMAT(A1,F11.5,A4,F11.5,A26)

      CALL XVMESSAGE(MSG1,0)
      CALL XVMESSAGE(MSG2,0)
      CALL XVMESSAGE(MSG3,0)
      CALL XVMESSAGE(MSG4,0)

      DO I=1,NPTS
         RLAT=TIEPOINTS(1,I)
         RLON=TIEPOINTS(2,I)
         CALL CONVEV(ISTAT,DATA,DATA,RL,RJ,RLAT,RLON,1,CONV)
         IF (ISTAT.NE.0) THEN
            WRITE(MSG,1112) M,RLAT,M4,RLON,ERROR
            CALL XVMESSAGE(MSG3,0)
            CALL XVMESSAGE(MSG,0)
         ELSE
            OUT_LAT(I) = RL
            OUT_LON(I) = RJ
            WRITE(MSG,112) M,RLAT,M4,RLON,M2,RL,M1,RJ,M4
            CALL XVMESSAGE(MSG3,0)
            CALL XVMESSAGE(MSG,' ')
         ENDIF
      ENDDO

      CALL XVMESSAGE(MSG1,0)
C
C     ....Write output parameters
      CALL XQINI(PARBLOCK, 500, XABORT)
      CALL XQREAL(PARBLOCK, 'OUT_LAT', NPTS, OUT_LAT, XADD, ISTAT)
      CALL XQREAL(PARBLOCK, 'OUT_LON', NPTS, OUT_LON, XADD, ISTAT)
      CALL XVQOUT(PARBLOCK, ISTAT)
      RETURN
      END
