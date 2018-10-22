      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM GSPICE: Print navigation and lighting data for an image
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER*4 IND,ICNT,IDEF,I
      INTEGER*4 NINP,UNIT,ICAM,FDS,SCET(6),TARGET_ID
      INTEGER*4 MP,MPTYPE,NL,NS
      REAL*8 LINE,SAMP
      REAL*4 FL,OAL,OAS,SCALE

      REAL*8 SCLAT,SCLON,SCRANGE
      REAL*8 SUNLAT,SUNLON,SUNRANGE
      REAL*4 RLINE,RSAMP,IS_LINE,IS_SAMP
      INTEGER*4 NAH,NAV,CONV(2720),NHIST,INSTANCE

      REAL*8 RADIUS	!Radius at P5 point
      REAL*8 LAT,LON,PHASE,INCIDENCE,EMISSION
      REAL*8 RPD		!NAIF function (radians-per-degree)
      DOUBLE PRECISION C_RA,C_DEC,C_TWIST,RA,DEC,TWIST
      DOUBLE PRECISION HALFPI,DPR,C_MATRIX(3,3)

      CHARACTER*5 PROJECT
ccc      CHARACTER*5 CAMERA
      CHARACTER*12 TARGET
      CHARACTER*80 MSG
      CHARACTER*30 OBSID
      CHARACTER*32 TASK1
      CHARACTER*7 MISSION
      CHARACTER*14 PROPERTYNAME

      INTEGER*4 LBUF(80)	!GETLABCON buffer

      REAL*4 DATA(40)		!PICSCALE nav data buffer
      REAL*8 DATA8(20)
      EQUIVALENCE(DATA,DATA8)

      REAL*4 PBUF(30)		!PICSCALE output buffer

      REAL*8 VSC(3)		!Vector from target-center to spacecraft
      REAL*8 VSUN(3)		!Vector from target-center to sun

      DOUBLE PRECISION RBUF(100)		!SPICE/SEDR buffer
      INTEGER*4 BUF(200)
      EQUIVALENCE (BUF,RBUF)

      LOGICAL RING_FLAG, XVPTST

      CALL XVMESSAGE('GSPICE Version Oct 2, 2002',' ')
      RING_FLAG = .FALSE.
      TARGET = '            '

      FDS = 0
      OBSID = ' '

C     ....If input image exists, get image identifiers from label
      CALL XVPCNT('INP',ninp)
      IF (NINP.EQ.0) GOTO 20
      CALL XVUNIT(unit,'INP',1,ind, ' ')
      CALL XVOPEN(UNIT,ind,'OPEN_ACT','SA',' ')
      CALL GETPROJ(UNIT,project,icam,fds,ind)
      CALL GETLABCON(UNIT,PROJECT,lbuf,ind)      
      IF (IND.GT.1) GOTO 982
      IF (LBUF(1).EQ.0) GOTO 983
      CALL MVE(4,6,LBUF(8),scet,1,1)
      CALL SEARC_DISTOR(UNIT,ind)
      MPTYPE = 8
      IF (IND.EQ.0) MPTYPE=7
      IF (XVPTST('RING')) RING_FLAG=.TRUE.
      IF (PROJECT.EQ.'VGR-1' .OR. PROJECT.EQ.'VGR-2') THEN
         CALL XVPARM('TARGET',target,icnt,idef,0)
         IF (ICNT.EQ.0) GOTO 971
         OBSID = 'Voyager Image'
      ELSE
         CALL MVLC(LBUF(25),target,12)
      ENDIF

      IF (PROJECT.EQ.'GLL') THEN 
         NHIST=1
         CALL XLHINFO(UNIT,TASK1,INSTANCE,NHIST,IND,' ')
         CALL XLGET(UNIT,'HISTORY','PA',OBSID,IND,'FORMAT',
     &          'STRING','HIST',TASK1,' ')
         WRITE(MSG,500) OBSID
  500    FORMAT('Observation=',A20)
         CALL XVMESSAGE(MSG,' ')      
         IF (OBSID(3:3) .EQ. 'R') RING_FLAG = .TRUE.
      ENDIF

      IF (PROJECT.EQ.'CASSI') THEN 
         CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,IND,
     &             'PROPERTY','CASSINI-ISS','ERR_ACT',' ',
     &             'FORMAT','STRING','ULEN',7,' ')
         IF (IND.EQ.1) THEN
           PROPERTYNAME = 'CASSINI-ISS   '
         ELSE
           CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,IND,
     &             'PROPERTY','CASSINI-ISS2','ERR_ACT',' ',
     &             'FORMAT','STRING','ULEN',7,' ')
           IF (IND.EQ.1) THEN
             PROPERTYNAME = 'CASSINI-ISS2  '
           ELSE
             CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,IND,
     &             'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     &             'FORMAT','STRING','ULEN',7,' ')
             IF (IND.EQ.1) PROPERTYNAME = 'IDENTIFICATION'
           END IF
         END IF
         CALL XLGET(UNIT,'PROPERTY','OBSERVATION_ID',OBSID,IND,
     &             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     &             'FORMAT','STRING','ULEN',30,' ')
         WRITE(MSG,501) OBSID
  501    FORMAT('Observation=',A30)
         CALL XVMESSAGE(MSG,' ')      
         I = INDEX(OBSID,'_')
C     .... OBSID of the form INST_xxxTT_...  where TT is Target code
         IF (OBSID(I+4:I+4) .EQ. 'R' .AND. OBSID(I+5:I+5) .NE. 'H') 
     &             RING_FLAG = .TRUE.
      ENDIF
      GOTO 50

C     ....If no input image, get image identifiers from user parameters
   20 CALL XVPARM('SPACECRAFT',project,icnt,idef,0)
      IF (ICNT.EQ.0) GOTO 970
      CALL XVPARM('TARGET',target,icnt,idef,0)
      IF (ICNT.EQ.0) GOTO 971
      CALL XVPARM('CAMERA',icam,icnt,idef,0)
      IF (ICNT.EQ.0) GOTO 972
      CALL XVPARM('SCET',scet,icnt,idef,0)
      IF (ICNT.NE.6) GOTO 974
      IF (XVPTST('RING')) RING_FLAG=.TRUE.
      MPTYPE = 7

   50 CALL GETSPICE3(PROJECT,TARGET,ICAM,FDS,SCET,.TRUE.,buf,ind)
      CALL XVMESSAGE(' ',' ')
      IF (BUF(10).EQ.1) THEN
c      IF (BUF(3).GT.1988) THEN
         CALL XVMESSAGE('Coordinate system is: J2000',' ')
      ELSE IF (BUF(10).EQ.2) THEN
         CALL XVMESSAGE('Coordinate system is: B1950',' ')
      ELSE
         CALL XVMESSAGE('Unknown or invalid coordinate system',' ')
      ENDIF

      CALL CMSOURCE(BUF,i)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(
     &'-----------------------------------------------------------',' ')
      WRITE(MSG,95) PROJECT,FDS,(BUF(I),I=3,8)
   95 FORMAT(A5,'  SCLK=',I10,
     &  '  SCET=(',I4,',',I3,',',I2,',',I2,',',I2,',',I3,')')
      CALL XVMESSAGE(MSG,' ')

      IF (IND.NE.1) GOTO 999

ccc      IF (PROJECT.EQ.'VGR-1' .OR. PROJECT.EQ.'VGR-2') THEN
ccc         SC_ID = BUF(1)
ccc         CALL MVLC(BUF(2),camera,5)
ccc         IF (SC_ID.EQ.-31) THEN			!VGR-1
ccc            ICAM = 6				!WA
ccc            IF (CAMERA(1:4).EQ.'ISSN') ICAM=7	!NA
ccc         ELSE					!VGR-2
ccc            ICAM = 4            		!WA
ccc            IF (CAMERA(1:4).EQ.'ISSN') ICAM=5	!NA
ccc         ENDIF
ccc      ENDIF

      CALL GETCAMCON(PROJECT,ICAM,fl,oal,oas,scale,ind)
      IF (IND.NE.0) GOTO 981

      WRITE(MSG,100) ICAM,FL,SCALE
  100 FORMAT('Camera=',I2,'  Focal length=',F8.2,
     &  ' mm  Picture scale=',F7.4,' pixels/mm')
      CALL XVMESSAGE(MSG,' ')

      TARGET_ID = BUF(9)
      CALL PBNAME(TARGET_ID,target,*985)
      WRITE(MSG,106) TARGET,(RBUF(I),I=13,15)
  106 FORMAT('Target=',A12,' Radii=(',F7.1,',',F7.1,',',F7.1,')')
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,112) RBUF(25),RBUF(28),RBUF(29)
  112 FORMAT('Solar range=',F15.0,' (lat,lon)=(',F7.3,',',F7.3,')')
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,113) RBUF(27),RBUF(30),RBUF(31)
  113 FORMAT('Spacecraft range=',F10.0,' (lat,lon)=(',
     &          F7.3,',',F7.3,')')
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,114) RBUF(69),RBUF(70)
  114 FORMAT('Object Space target center (line,samp)=(',
     &          F12.2,',',F12.2,')')
      CALL XVMESSAGE(MSG,' ')

      CALL PRINT_MATRIX(RBUF(41),1)		!C-Matrix
      CALL PRINT_MATRIX(RBUF(50),2)		!ME-Matrix
      CALL PRINT_MATRIX(RBUF(59),3)		!OM-Matrix
    
      C_MATRIX(1,1) = RBUF(41)      ! Matrix needs to be transposed
      C_MATRIX(1,2) = RBUF(42)      ! before the call to M2EUL
      C_MATRIX(1,3) = RBUF(43)      ! MIPS standard is to return the
      C_MATRIX(2,1) = RBUF(44)      ! matrix transposed.
      C_MATRIX(2,2) = RBUF(45)
      C_MATRIX(2,3) = RBUF(46)
      C_MATRIX(3,1) = RBUF(47)
      C_MATRIX(3,2) = RBUF(48)
      C_MATRIX(3,3) = RBUF(49)
C As in SPICE doc file rotation.req
      IF (PROJECT.EQ.'GLL') THEN
         CALL M2EUL(C_MATRIX,3,2,3,C_TWIST,C_DEC,C_RA)
         RA = DMOD(C_RA * DPR(),360.D0)
      ELSE
         CALL M2EUL(C_MATRIX,3,1,3,C_TWIST,C_DEC,C_RA)
         RA = DMOD((C_RA-HALFPI())*DPR(),360.D0)
      ENDIF
      IF (RA .LT. 0.D0) RA = RA + 360.D0
      DEC = DMOD((HALFPI()-C_DEC)*DPR(),360.D0)
      TWIST = C_TWIST*DPR()
      IF (TWIST .LT. 0.D0) TWIST = TWIST+360.D0
     
      CALL XVMESSAGE(' ',' ')      
      WRITE(MSG,115) RBUF(22),RBUF(23),RBUF(24)
  115 FORMAT('RS vector=(',F13.2,',',F13.2,',',F13.2,')')
      CALL XVMESSAGE(MSG,' ')      

C      CALL XVMESSAGE(' ',' ')      ! Used for debugging
C      WRITE(MSG,119) C_RA, C_DEC, C_TWIST
C  119 FORMAT('M2EUL-RA, DEC, TWIST (radians)=(',F13.2,',',F13.2,','
C     +             ,F13.2,')')
C      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,116) RA, DEC, TWIST
  116 FORMAT('RA, DEC, TWIST=(',F8.2,',',F8.2,',',F8.2,')')
      CALL XVMESSAGE(MSG,' ')      
      CALL XVMESSAGE(' ',' ')      

      WRITE(MSG,117) RBUF(68)
  117 FORMAT('North angle=',F6.2,' deg clockwise from right')
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,118) RBUF(26)
  118 FORMAT('Spacecraft distance from planet=',F10.0)
      CALL XVMESSAGE(MSG,' ')      
      CALL XVMESSAGE(' ',' ')      

C     ....Set up call to PICSCALE
      CALL SPICE2CONVEV(RBUF,FL,OAL,OAS,SCALE,MPTYPE,data)
      SCLAT = RBUF(30)
      SCLON = RBUF(31)
      NL = 2*OAL
      NS = 2*OAS
      IF (RING_FLAG) THEN
        CALL XVMESSAGE('Ring Image Observation',' ')
C VRH: This radius seems arbitrary
        RADIUS = 180000.0
        CALL RPICSCALE(RBUF,DATA,MP,MPTYPE,SCLAT,SCLON,RADIUS,NL,NS,
     &		pbuf,line,samp)
      ELSE
        CALL PICSCALE(RBUF,DATA,MP,MPTYPE,SCLAT,SCLON,NL,NS,
     &		pbuf,line,samp)
      ENDIF

C     ....Convert line,samp to image space
      IF (MPTYPE.EQ.7) THEN
         CALL GETGEOM(UNIT,PROJECT,ICAM,0,conv,conv,nah,nav,ind)
         RLINE = LINE
         RSAMP = SAMP
         CALL CONVISOS(PROJECT,ICAM,is_line,is_samp,RLINE,RSAMP,
     &		0,CONV(9),NAH+1,NAV+1,ind)
         LINE = IS_LINE
         SAMP = IS_SAMP
      ENDIF

      WRITE(MSG,120) LINE,SAMP
  120 FORMAT('Computations at (line,samp)=(',F8.2,',',F8.2,')')
      CALL XVMESSAGE(MSG,' ')

      IF (RING_FLAG) THEN
c VRH: Note - This is Planetary LON, not inertial Ring system LON as in NAV
        LON = PBUF(9)

        SUNRANGE = RBUF(25)
        SUNLAT = RBUF(28)*RPD()
        SUNLON = (360.D0 - RBUF(29))*RPD()	!Convert to East longitude
        CALL LATREC(SUNRANGE,SUNLON,SUNLAT,vsun)!Vector from center to Sun

        SCRANGE = RBUF(27)
        SCLAT = RBUF(30)*RPD()
        SCLON = (360.D0 - RBUF(31))*RPD()	!Convert to East longitude
        CALL LATREC(SCRANGE,SCLON,SCLAT,vsc)	!Vector from center to S/C

        CALL RING_LIGHT(VSC,VSUN,RADIUS,LON,incidence,emission,phase)
        WRITE(MSG,131) RADIUS,LON
  131   FORMAT(' (rad,lon)=(',F10.2,',',F8.2,')')
      ELSE
        LAT = PBUF(8)
        LON = PBUF(9)
        CALL LIGHTING(RBUF,LAT,LON,phase,incidence,emission)
        WRITE(MSG,121) LAT,LON
  121   FORMAT(' (lat,lon)=(',F8.2,',',F8.2,')')
      ENDIF
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,122) INCIDENCE,EMISSION,PHASE
  122 FORMAT(' Incidence=',F7.2,'  Emission=',F7.2,
     &  '  Phase=',F7.2)
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,123) PBUF(1)/1000.,PBUF(2)/1000.
  123 FORMAT(' Horizontal scale=',F9.3,'  Vertical scale=',
     &  F9.3,' km/pixel')
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,124) PBUF(6)
  124 FORMAT(' Slant range=',F12.2,' km')
      CALL XVMESSAGE(MSG,' ')      

      WRITE(MSG,125) PBUF(4), PBUF(3), PBUF(5)
  125 FORMAT(' Azimuths: Solar, North, Spacecraft=(',F9.2,',',
     +        F9.2,',',F9.2,')')
      CALL XVMESSAGE(MSG,' ')      
      CALL XVMESSAGE(' ',' ')      
      RETURN

  970 CALL XVMESSAGE('***SPACECRAFT parameter required',' ')
      GOTO 999
  971 CALL XVMESSAGE('***TARGET parameter required',' ')
      GOTO 999
  972 CALL XVMESSAGE('***CAMERA parameter required',' ')
      GOTO 999
  974 CALL XVMESSAGE('***SCET parameter required',' ')
      GOTO 999
  981 CALL PRNT(4,1,IND,'***GETCAMCON err, IND=.')
      GOTO 999
  982 CALL PRNT(4,1,IND,'***GETLABCON fatal err, IND==.')
      GOTO 999
  983 CALL XVMESSAGE('***Invalid label type')
      GOTO 999
  985 CALL PRNT(4,1,target_id,'***Invalid target ID=.')
  999 CALL XVMESSAGE('***GSPICE task cancelled',' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print 3x3 matrix
C
      SUBROUTINE PRINT_MATRIX(BUF,N)
      REAL*8 BUF(9)
      INTEGER*4 I,N
      CHARACTER*80 MSG
  100 FORMAT(10X,3F13.8)
  101 FORMAT(' C-Matrix:',3F13.8)
  102 FORMAT('ME-Matrix:',3F13.8)
  103 FORMAT('OM-Matrix:',3F13.8)

      CALL XVMESSAGE(' ',' ')
      IF (N.EQ.1) WRITE(MSG,101) BUF(1),BUF(4),BUF(7)
      IF (N.EQ.2) WRITE(MSG,102) BUF(1),BUF(4),BUF(7)
      IF (N.EQ.3) WRITE(MSG,103) BUF(1),BUF(4),BUF(7)
      CALL XVMESSAGE(MSG,' ')

      DO I=2,3
         WRITE(MSG,100) BUF(I),BUF(I+3),BUF(I+6)
         CALL XVMESSAGE(MSG,' ')
      ENDDO
      RETURN
      END
