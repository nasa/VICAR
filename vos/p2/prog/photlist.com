$!****************************************************************************
$!
$! Build proc for MIPL module photlist
$! VPACK Version 1.9, Sunday, July 10, 2011, 11:46:26
$!
$! Execute by entering:		$ @photlist
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module photlist ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to photlist.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("photlist.imake") .nes. ""
$   then
$      vimake photlist
$      purge photlist.bld
$   else
$      if F$SEARCH("photlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake photlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @photlist.bld "STD"
$   else
$      @photlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create photlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack photlist.com -mixed -
	-s photlist.f -
	-p photlist.pdf -
	-i photlist.imake -
	-t tstphotlist.pdf tstphotlist.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create photlist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create photlist.pdf
process help=*
PARM FDS     TYPE=INTEGER COUNT=1
PARM SCET    TYPE=INTEGER COUNT=6
PARM CRAFT   TYPE=KEYWORD COUNT=1  VALID=(VGR-1,VGR-2)
PARM CAMERA  TYPE=KEYWORD COUNT=1  VALID=(ISSN,ISSW)
PARM TARGET  TYPE=STRING  COUNT=1
PARM OSPC    TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM C       TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM UL      TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM UR      TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM LL      TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM LR      TYPE=REAL    COUNT=(0:2)  DEFAULT=--
PARM GRID    TYPE=REAL    COUNT=(0:4)  DEFAULT=--
PARM VALUETYPE  TYPE=KEYWORD VALID=(LINESAMP,LATLON)	DEFAULT=LINESAMP
PARM SEDRSRC TYPE=KEYWORD VALID=(SEDR,AMOS,NEAR,NAV2,FARENC,NAV,DAVI) +
  		DEFAULT=DAVI
PARM RES     TYPE=STRING  COUNT=(0:1)  DEFAULT=--
PARM DEBUG   TYPE=KEYWORD VALID=(DEBUG, NODEBUG) DEFAULT=NODEBUG
END-PROC

.TITLE
Vicar2 Program PHOTLIST
.HELP
PURPOSE:

PHOTLIST will compute the photometric parameters for specified areas of
an image.  PHOTLIST is restricted to Voyager images.

EXECUTION:

    PHOTLIST FDS=1636832 SCET=(1979,63,19,23,0,0) TARGET=IO +
       CRAFT=VGR-1 CAMERA=ISSN
where

    FDS is the 7-digit Flight Data System counter (FDS) specified as
       xxxxxyy where xxxxx is the Mod16 count and yy is the Mod60 count.
    SCET is the spacecraft event time specified as (year,day,hr,min,sec,msce).
    TARGET is the name of the target body.
    CRAFT is either VGR-1 or VGR-2.
    CAMERA is either ISSN or ISSW for the narrow and wide angle cameras.

.page
OPERATION:

PHOTLIST will extract the geometry data for the image from SPICE kernels
and computes the following lighting parameters for the center
(C), upper left (UL), upper right (UR), lower left (LL), and lower right (LR)
corners of the image, and at the target center (OSPC).  The line-sample 
coordinates at the target center is also reported for both geometrically
corrected (object space) and uncorrected (image space) images.  Object space
images have a 1000x1000 format and image space images have an 800x800 format.

SCLK=xxxxxyy
S/C=VGRx  Target=xxxxxxxxxxxx  Radii=(xxxxx.xx, xxxxx.xx)
S/C-Central Body Range=nnnnnnnnn.nn
S/C-Target Body Range=nnnnnnnnn.nn  Lat=nnn.nn  Lon=nnn.nn
Solar Range=nnnnnnnnn.nn  Lat=nnn.nn  Lon=nnn.nn
Object space coordinates at target center=( nnnnnnn., nnnnnnn.)
Image space coordinates at target center=( nnnnnnn., nnnnnnn.)
                Narrow Angle Camera                   Image Space
    Latitude  Longitude  Phase   Incidence  Emission  Line      Sample
C    nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
UL   nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
UR   nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
LL   nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
LR   nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
PC   nn.nn    nnn.nn     nnn.nn    nnn.nn    nnn.nn   nnn.nnnn  nnn.nnnn
 C-matrix source for frame xxxxxxx is FARENC

All longitudes are reported in System 3 west-longitude.  All latitudes are
planetocentric.

All calculations assume an oblate spheroid target model.  The equatorial and
polar radii are printed beside the target name.

The C matrix source is an indication of how accurately the camera pointing is
known.  If the source is SEDR, then the source of the camera pointing is from
predict data, and will in general be off by 50 or more pixels.  If the source
is NAV or FARENC, then the camera pointing has been determined by fitting the
limb, and is accurate to within 0.5 pixels or better.  If the source is NAV2,
NEAR, or AMOS, then the camera pointing has been determined via tiepoint
navigation (and is of indeterminate accuracy).  If the source is DAVI, then
the camera pointing has been determined by Mert Davies, and is considered to
be the most reliable source.

.page
POINT OFF TARGET:

The standard image space line-sample coordinates for the center and corners
(called reticle points) of the image are:

             UL=(1,1)                     UR=(1,800)

                          C=(400,400)

             LL=(800,1)                   LR=(800,800)

Occasionally, one or more of the reticle points calculated will be determined
to be off the target.  These points can be replaced by supplying different image
space line-sample coordinates as in the following example:

   photlist 2061657 LL=(500,100) LR=(500,800)

NOTE: By default, the supplied coordinates for C, UL, UR, LL, and LR are in 
      line-sample coordinates; however, such can be overridden by the 'LATLON 
      keyword.

.page
CORRECTING THE CAMERA POINTING:

The camera pointing can be corrected by specifying the object space line-sample
coordinates of the target-center via the OSPC parameter:

   photlist fds=2061657 scet=(1996,232,23,2,35,0) craft=vgr-1 +
     target=jupiter OSPC=(216,-14)

NOTE: the two examples will and should create different result.

For images containing a significant portion of the limb, these coordinates may
be derived via program NAV or measured directly off a print of the raw image.

.page
GEOMETRIC DISTORTIONS:

If the IBIS Reseau file is given (with the RES parameter) the program will read
the reseau locations from that file.  If the parameter is not given the nominal
locations of the reseau marks are used to model the geometric distortion in the
image.  


WARNING: The nominal reseau locations lead to an error of one or two pixels 
         near the center of the image.  The error grows progressively larger 
         toward the image margins and corners.

The distortion is removed by converting the line sample coordinates for each
reticle from the raw (image space) image to (distortion free) object space.

.page
A NOTE ON THE VOYAGER FDS:

The FDS is used as an index for accessing SEDR data.  The FDS associated with
the image is the FDS at the start of the camera cycle during which an image is
read out.  This is normally the same camera cycle in which the exposure takes
place.

A source of confusion occurs when simultaneous exposures are taken through the
narrow angle and wide angle cameras.  When this occurs, the image is read out
from the narrow angle camera immediately, while read-out of the wide angle
camera is suppressed until the following camera cycle.  The FDS associated
with the wide angle image will consequently not coincide with the camera cycle
during which the exposure took place, but will trail by one or more Mod60
counts, depending of the frame rate (FDS = FDS + frame-rate).

.page
PROGRAM HISTORY:

Original  Programmer:  Gary Yagi, April 12, 1996
Cognizant Programmer:  Gary Yagi
Revisions:
 Sep 14 99  GMY  Modify to use SPICE kernels
 Dec  7 96  SMC  Initialize ILAT to 0 before use                         (DFR)
 Oct  1 96  SMC  Change inertial coordinate system from J2000 to EME50.
                   (vgrspice.f, SYSTEM=1 => SYSTEM=2)
 Sep 26 96  SMC  > Modified CONVISOS calls.                          (FR89818)
                 > Modified the output so Radii's output don't overlap
 Jun 19 96  SMC  Modify output report labeling on line 3 and 4
 Jun 12 96  SMC  Took out the PC parameter in favor of OSPC
 May 31 96  SMC  Added the RES parameter and functionality
 May 20 96  SMC  Added the GRID parameter and functionality
 May 20 96  SMC  Added the OSPC parameter and functionality
 May 03 96  SMC  Added the VALUETYPE parameter and functionality
 Apr 14 96  GMY  Ported to Unix
 Apr 17 96  GMY  Added calculations at target center <443>
 Apr 25 96  GMY  Fix identification of VGR-1 and VGR-2
 Apr 26 96  GMY  Added C, UL, UR, LL, LR parameters
 Apr 27 96  GMY  Added PC parameter

.LEVEL1
.VARIABLE FDS
INTEGER--REQUIRED
Voyager spacecraft clock (FDS)
.VARI SCET
6 integers (required)
Spacecraft Event Time
SCET=(yr,day,hr,min,sec,msec)
.VARI TARGET
STRING (required)
Name of target body
.VARI CRAFT
STRING (required)
Spacecraft name (VGR-1 or VGR-2)
.VARI CAMERA
STRING (required)
Camera name (ISSN or ISSW)
.VARI OSPC
2 optional floating point values
line-samp in object space at target center
.VARI C
2 optional floating point values
line-samp at center of image
.VARI UL
2 optional floating point values
line-samp at upper-left corner of image
.VARI UR
2 optional floating point values
line-samp at upper-right corner of image
.VARI LL
2 optional floating point values
line-samp at lower-left corner of image
.VARI LR
2 optional floating point values
line-samp at lower-right corner of image
.VARI GRID
4 optional floating point values
(SL,SS,LINC,SINC)
.VARIABLE VALUETYPE
KEYWORD--OPTIONAL
Line/Samp or Latitude/Longtitude
.VARIABLE SEDRSRC
KEYWORD--OPTIONAL
Source for camera pointing
.VARIABLE PLANET
KEYWORD--OPTIONAL
Planet of encounter
.VARIABLE RES
1 optional string
IBIS Format Reseau file
.LEVEL2
.VARIABLE FDS
FDS is the 7-digit Flight Data System counter specified as
xxxxxyy where xxxxx is the Mod16 count and yy is the Mod60 count.  See help
file for details regarding simultaneous exposures.
.VARI OSPC
The camera pointing can be corrected by specifying the object space line-sample
coordinates of the target-center via the OSPC parameter.

For images containing a significant portion of the limb, these coordinates may
be derived via program NAV or measured directly off a print of the raw image.

Note that the effect of the OSPC parameter is completely different from that of
the C, UL, UR, etc. parameters.  The OSPC parameter is used to correct the
camera pointing whereas the latter merely change the location within the image
where calculations are performed.

.VARI C
Changes location in image where calculations for C (normally center of image)
are performed.
  e.g.  photlist 1234567 C=(300,300)
      where the coordinates are specified in image space
.VARI UL
Changes location in image where calculations for UL (normally upper left
corner of image) are performed.
  e.g.  photlist 1234567 UL=(300,300)
      where the coordinates are specified in image space
.VARI UR
Changes location in image where calculations for UR (normally upper right
corner of image) are performed.
  e.g.  photlist 1234567 UR=(300,300)
      where the coordinates are specified in image space
.VARI LL
Changes location in image where calculations for LL (normally lower left
corner of image) are performed.
  e.g.  photlist 1234567 LL=(300,300)
      where the coordinates are specified in image space
.VARI LR
Changes location in image where calculations for LR (normally lower right
corner of image) are performed.
  e.g.  photlist 1234567 LR=(300,300)
      where the coordinates are specified in image space
.VARI GRID
Defines additional points for PHOTLIST to report by specifying starting
point and incrementing values.  The specification is (SL,SS,LINC,SINC) where
  SL   = Starting Line
  SS   = Starting Sample
  LINC = Line increment
  SINC = Sample increment
The incrementing is done UNTIL either the LINE or SAMP exceeds the boundary
points 1..800 or 1..800 respectively.  (The UNTIL is empasized because initial
given point is not checked for the boundary)

  Ex:  GRID=(1,1,100,100)
         -------> reports (1,1),  (1,101),  (1,201)  ...(1,701)
                          (101,1),(101,101),(101,201)...(101,701)
                          (201,1),(201,101),(201,201)...(201,701)
                           .
                           .
                          (701,1),(701,101),(701,201)...(701,701)
                  in a left-right, then top to left order
                     if all are inside the target

       GRID=(701,701,-100,-100)
         -------> reports same as above but in reverse order (right-left,
                    botom-top) from the table presented above
                    if all are inside the target

       GRID=(-50,100,100,200)
         -------> reports (-50,100), (-50,300), (-50,500), (-50,700)
                          (50,100),  (50,300),  (50,500),  (50,700)
                           .
                           .
                          (750,100), (750,300), (750,500), (750,700)
                  if all are inside the target (left-right, top-botom)
                  NOTE: The initial point is reported even if it's outside the
                        boundary.  The boundary is not checked against -50 as
                        long as it is not incremented.

.VARIABLE VALUETYPE
 Specifies the coordinate system for user input of C, UL, UR, LL, LR
 (all except PC)
        LINESAMP - default, line/sample coordinate system of the geometrically
                            uncorrected image.
        LATLON   - latitude/longtitude coordinate system of the geometrically
                   corrected image.
.VARIABLE SEDRSRC
 Specifies source of desired camera pointing:
	DAVI	--Data determined by Mert Davies
	NAV     --Data determined by program NAV
	FARE    --Data determined by program FARENC
	NAV2    --Data determined by program NAV2
	NEAR    --Data determined by program NEARENC
        AMOS    --Data determined by program AMOS
	NAIF    --Data determined by NAIF
	SEDR    --Original SEDR values
 If this keyword is not specified, or if the specified source does not exist,
 the camera pointing will be selected from the available sources, in the
 following order of priority: (1) DAVI, (2) NAV, (3) FARE, (4) NAV2,
 (5) NEAR, (6) AMOS, (7) predict or final SEDR.
.VARIABLE PLANET
Planet of encounter.  The Voyager SEDR data is divided into separate files for
each planet.
.VARIABLE RES
Specifies the IBIS Format Reseau file to be read for obtaining the Reseau
locations of the image frame specifed by SCLK.  The Reseau file can be master
or one created by RESLOC as long as it has the Reseau locations for the image
frame specifed by the SCLK.

If this parameter is not given, the program uses the nominal values for the
Reseau locations which means that the output will not be as accurate.
.End
$ Return
$!#############################################################################
$Imake_File:
$ create photlist.imake
/* Imake file for VICAR program PHOTLIST */
#define PROGRAM photlist
#define MODULE_LIST photlist.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define LIB_P2SUB 
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_FORTRAN
/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstphotlist.pdf
procedure
! The PDF test file is written for both VMS and Unix
  RefGbl $echo
  RefGbl $SYSCHAR
body
  Local IBISReseauFile

  let $echo="NO"
  let _onfail="continue"
  if ($SYSCHAR(1)="VAX_VMS")
    let IBISReseauFile="WMS_VGR:[000000]CResJ.Fil"
  else
    let IBISReseauFile="/project/vgr/cresj.fil"
  end-if
   let $echo="yes"

!!!!!! Test on Voyager 2
!PHOTLIST FDS=2048013 SCET=(1979,184,23,4,47,0) TARGET=JUPITER CRAFT=VGR-2 +
! CAMERA=ISSN

!!!!!! Test on Voyager 1
PHOTLIST FDS=1636832 SCET=(1979,63,19,23,0,0) TARGET=IO CRAFT=VGR-1 +
 CAMERA=ISSN UL=(250,300) UR=(250,600) LL=(500,300) LR=(500,600) C=(400,400)
end-proc
$!-----------------------------------------------------------------------------
$ create tstphotlist.log
tstphotlist
PHOTLIST FDS=1636832 SCET=(1979,63,19,23,0,0) TARGET=IO CRAFT=VGR-1  +
 CAMERA=ISSN UL=(250,300) UR=(250,600) LL=(500,300) LR=(500,600) C=(400,400)
Beginning VICAR task PHOTLIST
PHOTLIST Version 10 JUL 2011
------------------------------------------------------
FDS=  1636832
S/C=VGR-1   Target=IO             Radii=( 1829.4,  1815.7)
S/C-Central Body Range=  1199648.60
S/C-Target Body Range=   806056.44   Lat= -0.0320   Lon=156.4714
Solar Range=790941184.00   Lat=  0.5413   Lon=171.2767
Object space coordinates at target center=(     537.16,     603.32)
Image space coordinates at target center=(     433.73,     489.19)
                Narrow Angle Camera                   Image Space
    Latitude  Longitude  Phase   Incidence  Emission  Line      Sample
C   -12.7636  175.2698   14.8560   14.0610   22.7253  400.0000  400.0000
UL    ***Point off target***
UR   52.8208  193.1440   14.8673   55.7354   61.4529  250.0000  600.0000
LL  -52.1181  176.5587   14.8399   53.2611   55.2355  500.0000  300.0000
LR   12.6784  127.5222   14.7560   45.0693   31.5325  500.0000  600.0000
PC   -0.0320  156.4714   14.8163   14.8163    0.0005  433.7271  489.1902
CKNAME=NAV   SPKID=N005  PROGRAM=NAV     GMY059  NONE  01/12/01
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
