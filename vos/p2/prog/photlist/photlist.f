      INCLUDE 'VICMAIN_FOR'
C VICAR Program PHOTLIST:
C         PHOTLIST  FDS=xxxxxyy  SCET=(1999,13,23,59,59,0)
C This program is Voyager specific

      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER*4 I,IND,COUNT,DEF

      INTEGER*4 FDS		!VGR spacecraft clock
      INTEGER*4 CAMERA		!Camera serial number

      CHARACTER*10 VALTYPE
      CHARACTER*128 ResFile
      INTEGER ResUnit, IBISUnit, nrows, JData(5)
      INTEGER IBIS_File_Get     ! External IBIS Function
      character *6 format(409) /5*'FULL',404*'REAL'/

      BYTE LBUF(400)
      INTEGER*4 IBUF(200)	!SPICE89 buffer
      REAL*8 BUF(100)
      EQUIVALENCE (LBUF,IBUF,BUF)

C     ....PPROJ arguments
      REAL*8 DMAP(20)	!CONVEV geometric data buffer
      REAL*4 MAP(40)
      EQUIVALENCE (DMAP,MAP)

      REAL*4 LINE,SAMP
      INTEGER*4 ILAT	!0=planetocentric latitudes
      REAL*4 RADIUS	!Radius at P5 point
      REAL*4 SRANGE	!Distance from S/C to P5 point

      INTEGER*4 SCET(6)         !SCET (year,day,hour,minute,sec,msec)
      CHARACTER*5 PROJECT	!VGR-1 or VGR-2
      CHARACTER*4 INSTRUMENT	!ISSN or ISSW
      CHARACTER*12 TARGET_NAME
      REAL*8 RA,RB,RC		!Target radii (km)

      REAL*4 SUNRANGE		!Distance from Sun to target center (km)
      REAL*4 SUNLAT,SUNLON	!Sun lat,lon (radians)

      REAL*4 SCRANGE		!Distance from S/C to target center (km)
      REAL*4 SCLAT,SCLON	!Spacecraft lat,lon (radians)
      REAL*4 PCLINE,PCSAMP	!Line-sample at target center
      REAL*4 RES(2,202),CONV(2216),ISLINE,ISSAMP

      INTEGER*4 NPH,NPV

      REAL*4 LAT,LON		!Lat,Lon of retical point
      REAL*4 PHASE,INCIDENCE,EMISSION
      REAL*8 VSC(3),VSUN(3)

      REAL*8 OM(3,3),RS(3),ANGN
      REAL*4 DATA(20)
      REAL*4 FL,OAL,OAS,SCALE
      REAL*8 DFL,DOAL,DOAS,DSCALE
      REAL*8 DPCLINE,DPCSAMP,DSCLON,DSCLAT,DSCRANGE

      REAL*4 IPTS(2), GRID(4)
      REAL*4 PTS(2,6)/
     &  400.,400.,1.,1.,1.,800.,800.,1.,800.,800.,0.,0./

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      ! Points to be reported, we need these arrays so we can report the first
      ! encountered point.  NOTE: we could convert all these from PTS, but
      ! precision error would be introduced
      REAL*4 ImagePts(2,6)   ! Reticle points in image space (LINSAMP format)
      REAL*4 LatLonPts(2,6)  ! Reticle points in (LATLON format)
      LOGICAL PtOffTarget(6) ! if the reticle points are off target

      CHARACTER*2 RETICLE(6)/'C ','UL','UR','LL','LR','PC'/
      CHARACTER*80 MSG

      CALL XVMESSAGE ('PHOTLIST Version 10 JUL 2011',' ')
      PI = 3.141592653589D0
      DTR = PI/180.D0
      RTD = 180.D0/PI
      CALL XVPARM('FDS',fds,count,def,0)	! added Jul.2011 - lwk
      CALL XVPARM('SCET',scet,count,def,0)
      CALL XVPARM('CRAFT',project,count,def,0)
      CALL XVPARM('TARGET',target_name,count,def,0)
      CALL XVPARM('CAMERA',instrument,count,def,0)
      IF (PROJECT.EQ.'VGR-1') THEN
         IF (INSTRUMENT.EQ.'ISSN') THEN
            CAMERA = 7		!VGR1 ISSNA
         ELSE
            CAMERA = 6		!VGR1 ISSWA
         ENDIF
      ELSE
         IF (INSTRUMENT.EQ.'ISSN') THEN
            CAMERA = 5		!VGR2 ISSNA
         ELSE
            CAMERA = 4		!VGR2 ISSWA
         ENDIF
      ENDIF
      CALL GETSPICE3(PROJECT,TARGET_NAME,CAMERA,FDS,SCET,0,buf,ind)
      IF (IND.NE.1) GOTO 992
c  need to call INIT_SPICE, since the new version of PBDATA
c  makes a SPICE call and GETSPICE3 clears the kernel pool ...
c  (July2011, lwk)
      call init_spice
      CALL PBDATA(TARGET_NAME,data,*990)
      RA = DATA(1)	!Long equatorial radius (km)
      RB = DATA(2)	!Short equatorial radius (km)
      RC = DATA(3)	!Polar radius (km)
      CALL MVLC(LBUF(5),instrument,4)

      CALL GETCAMCON(PROJECT,CAMERA,fl,oal,oas,scale,ind)
      IF (IND.NE.0) GOTO 994
C     ....Solar position vector  
      SUNRANGE = BUF(25)
      SUNLAT = BUF(28)
      SUNLON = BUF(29)
      CALL VECTOR(SUNRANGE,SUNLAT,SUNLON,vsun)	!Vector from center to Sun
C     ....Spacecraft position vector
      SCRANGE = BUF(27)
      SCLAT = BUF(30)
      SCLON = BUF(31)
      CALL VECTOR(SCRANGE,SCLAT,SCLON,vsc)	!Vector from center to S/C

      CALL MVE(8,9,BUF(59),dmap,1,1)		!OM matrix
      CALL MVE(8,3,BUF(22),dmap(10),1,1)	!RS vector
      MAP(25) = RC	!Polar radius (km)
      MAP(26) = RA	!Equatorial radius (km)
      MAP(27) = FL	!Focal length (mm)
      MAP(28) = OAL	!Optical axis line
      MAP(29) = OAS	!Optical axis sample
      MAP(30) = SCALE	!Object-space picture scale (pixels/mm)
      MAP(38) = SCRANGE	!Distance from S/C to target center (km)

C     ....Get image space to object space transformation
      CALL XVPARM ('RES', ResFile, count, def, 0)
      IF (def.EQ.0) THEN
        ! IBIS Reseau file given, read it
        CALL XVUNIT(ResUnit, 'N/A',1,ind, 'U_NAME',ResFile, ' ')

        CALL IBIS_File_Open(ResUnit, IBISUnit, 'read',409,99999,
     +       format, 0,ind)
        if (ind.ne.1) call ibis_signal_u(ResUnit, ind, 1)
        count = ibis_file_get(IBISUnit,'nr',nrows,1,1)
        if (nrows.LT.0) call ibis_signal(IBISUnit,count,1)
        JData(1) = fds
        JData(2) = CAMERA
        CALL getlocv2(IBISUnit,nrows,JData,RES,ind)
        IF (ind.EQ.1) THEN
          CALL XVMESSAGE('Frame not located in input Reseau file!',' ')
          CALL ABEND
        END IF
      ELSE
        CALL GETRES(res,CAMERA)		!Get nominal reseau
      END IF
      CALL GEOMAV(conv,CAMERA,RES)	!Use nominals to compute transformation
      NPH = 24
      NPV = 23

C     ...Check if target center is specified
      CALL XVPARM('OSPC',IPTS,Count,def,0)
      IF (Count.GT.0) THEN
         PCLINE = IPTS(1)      !Object space line-sample coordinates
         PCSAMP = IPTS(2)      !of target center
         CALL CONVISOS(PROJECT,CAMERA,isline,issamp,PCLINE,PCSAMP,0,
     &                   CONV(9),NPH, NPV, ind)

         ilat=0
         CALL PPROJ(MAP,PCLINE,PCSAMP,LatLonPts(1,6),LatLonPts(2,6),
     &                2,ilat,radius,srange,ind)

         PtOffTarget(6)=(ind.EQ.0)

         PTS(1,6) = PCLINE	!Object space line-sample coordinates
         PTS(2,6) = PCSAMP	!of target center
         ANGN = BUF(68) + 90.	!North angle
         DOAL = OAL		!Everything needs to be converted
         DOAS = OAS		!to double precision (ho hum...)
         DPCLINE = PCLINE
         DPCSAMP = PCSAMP
         DSCALE = SCALE
         DFL = FL
         DSCLON = SCLON
         DSCLAT = SCLAT
         DSCRANGE = SCRANGE
         CALL MOMATI(DOAL,DOAS,DPCLINE,DPCSAMP,DSCALE,DFL,
     &     DSCLON,DSCLAT,ANGN,DSCRANGE,om,rs)
         CALL MVE(8,9,OM,dmap,1,1)
      ELSE			!Else, compute from current pointing
         LatLonPts(1,6) = SCLAT
         LatLonPts(2,6) = SCLON
         ILAT = 0		!Compute planetocentric latitudes
         CALL PPROJ(MAP,pcline,pcsamp,SCLAT,SCLON,1,ilat,radius,
     &		srange,ind)

         PtOffTarget(6) = (ind.EQ.0)

         PTS(1,6) = PCLINE	!Object space line-sample coordinates
         PTS(2,6) = PCSAMP	!of target center
         CALL CONVISOS(PROJECT,CAMERA,isline,issamp,PCLINE,PCSAMP,0,
     &         CONV(9),NPH,NPV,ind)	!Convert to image space
      ENDIF
      ImagePts(1,6) = ISLINE
      ImagePts(2,6) = ISSAMP

C-----Get the coordinate system which the user wants to  use
      CALL XVPARM ('VALUETYPE', VALTYPE,count,def,0)
C-----Check if user has moved reticle points
      DO I=1,5
         CALL XVPARM(RETICLE(I),IPTS,count,def,0)
         IF (COUNT.GT.0) THEN
            IF (VALTYPE.EQ.'LATLON') THEN
              LatLonPts(1,I) = IPTS(1)
              LatLonPts(2,I) = IPTS(2)
              PtOffTarget(I)=.FALSE.
              CALL PPROJ(MAP,line,samp,IPTS(1),IPTS(2),1,ILAT,
     &                RADIUS,SRANGE,ind)
              CALL CONVISOS(PROJECT,CAMERA,Pts(1,I),Pts(2,I),LINE,SAMP,
     &                      0,CONV(9),NPH,NPV,ind)
            ELSE
              PTS(1,I) = IPTS(1)
              PTS(2,I) = IPTS(2)

              ! Convert to object space
              CALL CONVISOS(PROJECT,CAMERA,IPTS(1),IPTS(2),line,samp,1,
     &                      CONV(9),NPH, NPV,ind)
              ! Conver to LatLon
              CALL PPROJ(MAP,LINE,SAMP,LatLonPts(1,I),LatLonPts(2,I),
     &                   2,ilat,radius,srange,ind)
              PtOffTarget(I) = (ind.EQ.0)
            ENDIF

         ELSE
           CALL CONVISOS(PROJECT,CAMERA,PTS(1,I),PTS(2,I),line,samp,1,
     &                   CONV(9),NPH,NPV,ind)
           CALL PPROJ(MAP,LINE,SAMP,LatLonPts(1,I),LatLonPts(2,I),
     &                2,ilat,radius,srange,ind)
           PtOffTarget(I) = (ind.EQ.0)
         ENDIF
         ImagePts(1,I) = PTS(1,I)
         ImagePts(2,I) = PTS(2,I)
      ENDDO


C     ....Convert reticle points of all but PC (already in object space)
C         from image space to object space
      DO I=1,5
         LINE = PTS(1,I)
         SAMP = PTS(2,I)
         CALL CONVISOS(PROJECT,CAMERA,LINE,SAMP,pts(1,I),pts(2,I),1,
     &		CONV(9),NPH,NPV,ind)
      ENDDO

      CALL XVMESSAGE(
     & '------------------------------------------------------',' ')
      WRITE(MSG,101) FDS
  101 FORMAT('FDS=',I9)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,102) PROJECT,TARGET_NAME,RA,RC
  102 FORMAT('S/C=',A5,'   Target=',A12,'   Radii=(',F7.1,', ',F7.1,')')
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,107) BUF(26)
  107 FORMAT('S/C-Central Body Range=',F12.2)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,103) SCRANGE,SCLAT,SCLON
  103 FORMAT('S/C-Target Body Range=',F12.2, 
     +       '   Lat=',F8.4,'   Lon=',F8.4)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,104) SUNRANGE,SUNLAT,SUNLON
  104 FORMAT('Solar Range=',F12.2,'   Lat=',F8.4,'   Lon=',F8.4)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,105) PCLINE,PCSAMP
  105 FORMAT('Object space coordinates at target center=(',
     &   F11.2,',',F11.2,')')
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,106) ImagePts(1,6),ImagePts(2,6)
  106 FORMAT('Image space coordinates at target center=(',
     &   F11.2,',',F11.2,')')
      CALL XVMESSAGE(MSG,' ')

      IF (INSTRUMENT.EQ.'ISSN') THEN
         CALL XVMESSAGE(                                     
     &'                Narrow Angle Camera                   '//
     &'Image Space',' ')
      ELSE
         CALL XVMESSAGE(
     &'                Wide Angle Camera                     '//
     &'Image Space' ,' ')
      ENDIF
      CALL XVMESSAGE(
     &'    Latitude  Longitude  Phase   Incidence  Emission  '//
     &'Line      Sample',' ')

C-------------------- Calculate and Display Points -----------------------
      DO I=1,6
        ILAT = 0		!Compute planetocentric latitudes
        CALL PPROJ(MAP,PTS(1,I),PTS(2,I),lat,lon,2,ilat,radius,
     &             srange,ind)
        IF (PtOffTarget(I)) THEN
          WRITE(MSG,99) RETICLE(I)
   99     FORMAT(A2,'    ***Point off target***')
          CALL XVMESSAGE(MSG,' ')
        ELSE
          ! Use the more accurate Lat/Lon set
          LAT = LatLonPts(1,I)
          LON = LatLonPts(2,I)

          CALL CAL_PIE(RADIUS,LAT,LON,RC,RA,VSUN,VSC,RTD,
     &                 PHASE, INCIDENCE, EMISSION)
          WRITE(MSG,120) RETICLE(I),LAT,LON,PHASE,INCIDENCE,EMISSION,
     &               ImagePts(1,I), ImagePts(2,I)
  120     FORMAT(A2,7F10.4)
          CALL XVMESSAGE(MSG,' ')
        ENDIF
      END DO

C-----Process additional points if GRID is specified
      CALL XVPARM('GRID',GRID,count,def,0)
      IF (count.GT.0) THEN
        CALL XVMESSAGE ('(User defined points)',' ')
        ISLINE=GRID(1)      ! Start Line
        ISSAMP=GRID(2)      ! Start Sample

        ILAT = 0          ! Compute planetocentric latitudes
        IND  = 1
        DO WHILE (IND.NE.0)
          CALL CONVISOS(PROJECT,CAMERA,ISLINE,ISSAMP,pcline,pcsamp,1,
     &                  CONV(9),NPH, NPV, IND)
          CALL PPROJ(MAP,PCLINE,PCSAMP,lat,lon,2,ILAT,RADIUS,SRANGE,IND)
          IF (IND.EQ.0) THEN
            CALL XVMESSAGE('      ***Point off target***',' ')
            IND = 1
          ELSE
            CALL CAL_PIE(RADIUS,LAT,LON,RC,RA,VSUN,VSC,RTD,
     &                   PHASE, INCIDENCE, EMISSION)
            WRITE(MSG,108) LAT,LON,PHASE,INCIDENCE,EMISSION,
     &                     ISLINE,ISSAMP
  108       FORMAT('  ',7F10.4)
            CALL XVMESSAGE (MSG,' ')
          END IF
          ISSAMP = ISSAMP + GRID(4)
          IF (ISSAMP.GT.800 .OR. ISSAMP.LT.1) THEN
            ISSAMP = GRID(2)           ! Reset Sample to starting value
            ISLINE = ISLINE + GRID(3)  ! increment Line value
            IF (ISLINE.GT.800 .OR. ISLINE.LT.1) THEN
C-------------Going off image space boundary, trigger IND to stop while loop
              IND = 0
            END IF
          END IF
        END DO
      END IF

      CALL CMSOURCE(IBUF,ind)
      RETURN

  990 CALL XVMESSAGE('***Invalid target ID',' ')
      GOTO 999
  992 CALL XVMESSAGE('***SEDR data not available',' ')
      GOTO 999
  994 CALL XVMESSAGE('***Invalid camera serial number',' ')

  999 CALL XVMESSAGE('***PHOTLIST task canceled',' ')
      CALL ABEND
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Routine to CALculate Phase, Incidence, Emission
      SUBROUTINE CAL_PIE(RADIUS,LAT,LON,RC,RA,VSUN,VSC,RTD,
     &                   PHASE, INCIDENCE, EMISSION)
C-----Input Parameters
      REAL*4 RADIUS, LAT, LON
      REAL*8 RC, RA, VSUN(3), VSC(3), RTD

C-----Output Parameters
      REAL*4 PHASE, INCIDENCE, EMISSION

C-----Local Variables
      REAL*8 N(3), S(3), C(3), V(3)

      CALL VECTOR(RADIUS,LAT,LON,v)	       !Vector from center to surface pt
C     ....Compute unit normal N at surface point
      N(1) = V(1)*(RC/RA)**2
      N(2) = V(2)*(RC/RA)**2
      N(3) = V(3)
      CALL UNIT_VECTOR(n)      
C     ....Compute unit vector from surface point to Sun
      S(1) = VSUN(1) - V(1)
      S(2) = VSUN(2) - V(2)
      S(3) = VSUN(3) - V(3)
      CALL UNIT_VECTOR(s)
C     ....Compute unit vector from surface point to spacecraft
      C(1) = VSC(1) - V(1)
      C(2) = VSC(2) - V(2)
      C(3) = VSC(3) - V(3)
      CALL UNIT_VECTOR(c)
C     ....phase angle = ARCCOS(S o C)
      PHASE = DACOS(S(1)*C(1) + S(2)*C(2) + S(3)*C(3))*RTD
C     ....incidence angle = ARCCOS(N o S)
      INCIDENCE = DACOS(N(1)*S(1)+N(2)*S(2)+N(3)*S(3))*RTD
C     ....emission angle = ARCCOS(N o C)
      EMISSION = DACOS(N(1)*C(1)+N(2)*C(2)+N(3)*C(3))*RTD
        
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to convert a point defined by R,LAT,LON into a vector
C
      SUBROUTINE VECTOR(R,LAT,LON,v)
      REAL*4 R,LAT,LON		!West longitude
      REAL*8 V(3)
      REAL*4 RLAT,RLON		!Converted to radians

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      RLAT = LAT*DTR
      RLON = LON*DTR
      V(1) = R*COS(RLAT)*COS(RLON)
      V(2) = -R*COS(RLAT)*SIN(RLON)
      V(3) = R*SIN(RLAT)
      RETURN
      END
CCCCCCCCCCCC
C Routine to scale a vector to a unit vector
C           V = V/|V|
      SUBROUTINE UNIT_VECTOR(V)
      REAL*8 V(3),VMAG

      VMAG = DSQRT(V(1)**2+V(2)**2+V(3)**2)
      V(1) = V(1)/VMAG
      V(2) = V(2)/VMAG
      V(3) = V(3)/VMAG
      RETURN
      END
