$!****************************************************************************
$!
$! Build proc for MIPL module gspice
$! VPACK Version 1.9, Friday, July 14, 2006, 14:34:07
$!
$! Execute by entering:		$ @gspice
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
$ write sys$output "*** module gspice ***"
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
$ write sys$output "Invalid argument given to gspice.com file -- ", primary
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
$   if F$SEARCH("gspice.imake") .nes. ""
$   then
$      vimake gspice
$      purge gspice.bld
$   else
$      if F$SEARCH("gspice.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gspice
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gspice.bld "STD"
$   else
$      @gspice.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gspice.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gspice.com -mixed -
	-s gspice.f -
	-p gspice.pdf -
	-i gspice.imake -
	-t tstgspice.pdf tstgspice.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gspice.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create gspice.pdf
process help=*
PARM INP        TYPE=STRING   COUNT=0:1				DEFAULT=--
PARM SPACECRAFT TYPE=KEYWORD  COUNT=0:1  VALID=(CASSI,GLL,VGR-1,VGR-2) +
 DEFAULT=GLL
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SCET	TYPE=INTEGER  COUNT=0:6				DEFAULT=--
PARM CAMERA	TYPE=INTEGER  COUNT=0:1    VALID=1:42		DEFAULT=1
PARM RING       TYPE=KEYWORD  COUNT=0:1    VALID=RING           DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
END-PROC

.TITLE
VICAR2 program GSPICE

.HELP
PURPOSE:

GSPICE lists the SPICE data for a specified image.  The program is
currently restricted to Voyager ISS, Galileo SSI, and Cassini ISS.

EXECUTION:

    GSPICE  INP=inputimage  TARGET=IO
 or
    GSPICE  SPACECRAFT=VGR-1  CAMERA=7 	TARGET=IO +
	SCET=(year,day,hour,minute,second,msec)
 or
    GSPICE  SPACECRAFT=GLL  CAMERA=1  TARGET=IO +
	SCET=(year,day,hour,minute,second,msec)
 or
    GSPICE  SPACECRAFT=CASSI  CAMERA=1  TARGET=IO +
	SCET=(year,day,hour,minute,second,msec)
where
    INP is the filename of the image for which SPICE data is to be listed.
    SPACECRAFT is CASSI, GLL, VGR-1, or VGR-2.
    SCET is the shutter-centered SpaceCraft Event Time

If INP is specified and the project is GLL or CASSI, the TARGET parameter
need only be specified if the target name is incorrect in the VICAR label.

If INP is not specified,  SPACECRAFT, CAMERA, TARGET, and SCET must be
specified.

.page
OPERATION:

If INP is specified, the spacecraft ID, camera ID, target name, and
spacecraft event time are retrieved from the VICAR label.  Since the target
name is not present in the Voyager image label, the TARGET parameter is
required for Voyager images.

If INP is not specified, this information must be supplied via the SPACECRAAFT,
CAMERA, TARGET, and SCET parameters.

Planetocentric latitudes and west longitudes are used.  Unless explicitly
stated otherwise, all distances are in kilometers.

DESCRIPTION OF PRINTED SPICE DATA:

Coordinate system is: B1950
    The inertial coordinate system used is Earth Mean Equator 1950 for Voyager
    and J2000 for Galileo and Cassini.

CKNAME=FARE  SPKID=N120 PROGRAM=*NONE* IG.PLT  PLTF 00/00/00
    The C-matrix source is MIPS_FARENC.CK.

GLL    SCLK=420467122  SCET=(1997,309,17,46,25,395)
    The spacecraft, SCLK and SCET times used.

Observation=11JSFEATRK03
    The Observation id from the PA Vicar label keyword for Galileo images.

Camera=2  Focal length= 1501.04 mm  Picture scale=32.8084 pixels/mm
    Camera information

Target=IO           Radii=( 1830.0, 1818.7, 1815.3)
    Target long equatorial radius, short equatorial radius, and polar radius.

Solor range=     790940864. (lat,lon)=(  0.555,169.925)
    The latitude and longitude of the sub-solar point.

Spacecraft range=   806022. (lat,lon)=( -0.096,155.074)
    The latitude and longitude of the sub-spacecraft point.

 C-Matrix:   0.59451884   0.17499144  -0.78480911
             0.56678396   0.60112065   0.56339139
             0.57035369  -0.77976400   0.25819519
    Transformation from camera coordinates to inertial coordinates. 

ME-Matrix:  -0.44974107  -0.89304841  -0.01405499
             0.80900592  -0.40064800  -0.43010536
             0.37847379  -0.20480661   0.90266931
    Transformation from target fixed coordinates to inertial coordinates.

OM-Matrix:   0.40701598  -0.87482715   0.26270795
             0.11248909  -0.23741280  -0.96487373
             0.90646797   0.42227080   0.00177771
    Transformation from target fixed coordinates to camera coordinates.

RS vector=(   -730945.00,   -339690.13,     -1353.79)
    Vector from target center to spacecraft in target coordinates.

RA, DEC, TWIST=(       109.06,       -18.49,       314.53)
    Right Ascension, Declination, Twist Angles of the platform in J2000
    Galileo uses a different definition of Twist than Voyager and Cassini.
    For info see the rotation.req SPICE documentation file.

North angle=285.23 deg clockwise from right
    Orientation of the projected spin axis in the image.

Spacecraft distance from planet=  1199689.
    Distance from planet center to spacecraft.

Computations at (line,samp)=( 505, 514)
 (lat,lon)=(    0.68,  156.20)
 Incidence=  13.73  Emission=   1.37  Phase=  14.87
 Horizontal scale=   47.306  Vertical scale=   47.300 km/pixel
 Slant range=   804192.81 km
 Azimuths: Solar, North, Spacecraft=(      191.60,       92.12,      358.10)

The lighting angles, image resolution, target distance and azimuths are computed
at some convenient point in the image.  For high-resolution images (where the 
target covers the entire field-of-view), the center of the image is chosen.  For
low-resolution images, the target-center is used if it is visible in the image
or if the size of the target is less than the width of the image.  If not, the
computations are performed at the point along the picture margin where the
resolution is highest.

.page
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

CDATE specifies the date and time the camera pointing was created.
REQNUM identifies the request number associated with the camera pointing.
PURPOSE identifies the purpose for creating the camera pointing.
PROGRAM identifies the program which created the camera pointing.
SPKID identifies the SP-kernel used to create the camera pointing.
USERID identifies the user who created the camera pointing.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

Complete lists of CK and SPK IDs are located in the xxx_KDB files listed in
the ASCII file assigned the logical name (or environmental variable) 
SPICE_CONFIG_FILE.  These files are project-specific, e.g., GLL_KDB, 
CAS_KDB, etc.  The value of SPICE_CONFIG_FILE being used is defined
by the VICAR system in use as selected by the "select X" command (X = O
for operational system, D for development, T for test, etc.).  The kernels
for systems other than O are not guaranteed to be correct.  If problems are
encountered with the kernels used by "select O", please contact the MIPS
SPICE cognizant engineer.

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

EXAMPLES:
  gspice inp=a.img
  gspice spacecraft=gll   target=ganymede  camera=1 +
	scet=(1996,121,11,10,9,123)
  gspice spacecraft=vgr-1 target=jupiter  camera=7 +
	scet=(1996,121,11,10,9,123)

.page
PROGRAM HISTORY:
Written By: Gary Yagi, October 1, 1996
Cognizant Programmer: Gary Yagi
REVISIONS: 

14 Jul 2006  lwk  Updated discussion of kernel DB in Help text.
Oct 02, 2002 VRH  Some Cassini fixes, Doc updates.
Sep 01, 2002 GMY  Update VGR and Cassini test cases.  Fix VGR camera s/n.
Dec 07, 2001 GMY  Fix .PDF by adding continuation char to PROJECT keyword.
Nov 28, 2001 GMY  Add Cassini capability.
Jul 23, 1998 TXH  Assigned initial values to parameters FDS and OBSID.
                  Removed unuse variables.
Jun 06, 1998 RRP  Modified call to xvpcnt not to include more then two
                  parameters to make it work on hp platform.
Apr 30, 1988 GMY  Modify to work with VGR SPICE files.
Jan 22, 1998 GMY  Calculate phase, incidence, and emission on ring.
Jan 12, 1998 HBM  Added the printing of the ra, dec, twist,and azimuth values
                       and resolved Voyager OBSID RING issue.
Nov 24, 1997 HBM  Added new picscale routines into GSPICE.  
                       Still need to resolve OBSID for Voyager and port for SGI
Dec 30, 1996 OAM  Moved spice2convev and lighting to picscale.com.

.LEVEL1
.VARI INP
Optional input image
.VARI SPACECRAFT
Optional spacecraft ID
.LEVEL1
.VARI CAMERA
Optional camera serial number
.VARI TARGET
Optional 12-char string
Target name
.VARI SCET
Optional SpaceCraft Event Time
.VARI RING
Keyword to identify
the image as a RING image.
Must be specified if the input
image is a Voyager RING image.
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created

.LEVEL2
.VARI INP
Ex:  INP=/home/gmy/nicepicture.dat

Input image for which SPICE data is to be printed.

If INP is not specified, the SPACECRAFT, CAMERA, TARGET, and SCET parameters
must be specified.

.VARI SPACECRAFT
EX:  SPACECRAFT=GLL

Valid values are VGR-1, VGR-2, GLL and CASSI.  SPACECRAFT is only required if
INP is not specified.

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI CAMERA
Camera serial number.

Valid Voyager camera serial numbers are:
        4 = VGR-2 WA            6 = VGR-1 WA
        5 = VGR-2 NA            7 = VGR-1 NA
 
For Galileo, the camera serial number is 1 for full-frame images, and 2 for
summation mode images.

Valid Cassini camera serial numbers are:
     1=NAC  21=NAC 2x2 summation mode  41=NAC 4x4 summation mode
     2=WAC  22=WAC 2x2 summation mode  42=WAC 4x4 summation mode

.VARI SCET
Shutter centered Spacecraft Event Time of the image for which SPICE data is
to be printed.  SCET is only required if INP is not specified.

.VARI RING
Identifies the image to be a ring image.  The RING keyword is required if the
parameter INP is not supplied for a Galileo or Voyager image.  The RING keyword 
must be supplied if the INP parameter image is a Voyager image and the image is 
also a ring image.  The program uses the PA keyword value from a Galileo image 
label to determine if the image is a ring image, but there is not an equivalent 
for a Voyager image.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create gspice.imake
#define PROGRAM gspice

#define MODULE_LIST gspice.f
                            
#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_FORTRAN

/*
#define LIB_LOCAL
#define DEBUG
*/
$ Return
$!#############################################################################
$Test_File:
$ create tstgspice.pdf
! Test script for the program GSPICE
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
local path1 type=string init="wms_test_work:[testdata.mipl.vgr]"
local path2 type=string init="wms_test_work:[testdata.mipl.gll]"
local path3 type=string init="wms_test_work:[testdata.gll]"
local path4 type=string init="wms_test_work:[testdata.cassini.cas$i$ss]"
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
if ($syschar(1) = "UNIX")
    let path1="/project/test_work/testdata/mipl/vgr/"
    let path2="/project/test_work/testdata/mipl/gll/"
    let path3="/project/test_work/testdata/gll/"
    let path4="/project/test_work/testdata/cassini/casIss/"
end-if

!................................Voyager Test.................................
!....Test local mode
!gspice inp=&"path1"f1636832.fic target=io spicemode=local
!....Test remote mode
gspice spacecraft=vgr-1 camera=7 target=jupiter scet=(79,58,7,42,59,0) +
    spicemode=remote

!..............................Galileo Test.................................
!...Test local SPICE access
!gspice inp=&"path3"s0401863200.1 spicemode=local
!...Test remote SPICE access
gspice inp=&"path2"venus.img  ckname=NAIF
!...Same as before but with no input image.  Results should match.
gspice spacecraft=gll target=venus scet=(1990,44,5,58,16,962) camera=1
!...Test on ring image
gspice inp=&"path3"s0368991900.5 targ=JUPITER 'ring

!
!...........................Cassini Test......................................
if ($syschar(1) = "UNIX")
  let path4="/project/test_work/testdata/cassini/casIss/"
!  gspice inp=&"path4"n1354897340.1 spicemode=local      !local mode on Unix only
end-if
gspice inp=&"path4"n1354897340.1
gspice inp=&"path4"w379.img     !test 2x2 summation mode
gspice inp=&"path4"n013.img     !test 4x4 summation mode
end-proc
$!-----------------------------------------------------------------------------
$ create tstgspice.log_solos
tstgspice
if ($syschar(1) = "UNIX")
    let path1="/project/test_work/testdata/mipl/vgr/"
    let path2="/project/test_work/testdata/mipl/gll/"
    let path3="/project/test_work/testdata/gll/"
    let path4="/project/test_work/testdata/cassini/casIss/"
end-if
gspice spacecraft=vgr-1 camera=7 target=jupiter scet=(79,58,7,42,59,0)  +
    spicemode=remote
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Warning::year number is not 4-digit.
***SPICE error=          0

Coordinate system is: B1950
CKNAME=NAIF  SPKID=

-----------------------------------------------------------
VGR-1  SCLK=         0  SCET=(1979, 58, 7,42,59,  0)
***GSPICE task cancelled
 ** ABEND called **
continue
gspice inp=/project/test_work/testdata/mipl/gll/venus.img  ckname=NAIF
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Observation=VVGLI01

Coordinate system is: J2000
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  

-----------------------------------------------------------
GLL    SCLK=  18494400  SCET=(1990, 44, 5,58,16,962)
Camera= 1  Focal length= 1501.04 mm  Picture scale=65.6168 pixels/mm
Target=VENUS        Radii=( 6137.0, 6137.0, 6137.0)
Solar range=     107564533. (lat,lon)=( -2.579,129.476)
Spacecraft range=  1629972. (lat,lon)=( -2.954,176.320)
Object Space target center (line,samp)=(      143.26,      473.60)

 C-Matrix:  -0.91898043   0.06314746  -0.38921377
            -0.37781762  -0.42345808   0.82336936
            -0.11282203   0.90371215   0.41300791

ME-Matrix:  -0.33169343  -0.94320206   0.01869081
             0.86714712  -0.31263036  -0.38770881
             0.37153106  -0.11239278   0.92159239

OM-Matrix:  -0.06472057   0.99758188   0.02533080
            -0.05238892  -0.02874568   0.99821295
             0.99652729   0.06327786   0.05412267

RS vector=(  -1624449.25,   -104477.87,    -84007.70)
RA, DEC, TWIST=(  115.30,   24.39,   82.88)

North angle= 88.55 deg clockwise from right
Spacecraft distance from planet=  1629972.

Picture scale calculated at target center
Computations at (line,samp)=(  143.14,  473.63)
 (lat,lon)=(   -2.95,  176.32)
 Incidence=  46.79  Emission=   0.00  Phase=  46.79
 Horizontal scale=   16.488  Vertical scale=   16.488 km/pixel
 Slant range=  1623834.88 km
 Azimuths: Solar, North, Spacecraft=(   179.31,    88.55,   105.99)

gspice spacecraft=gll target=venus scet=(1990,44,5,58,16,962) camera=1
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002

Coordinate system is: J2000
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  

-----------------------------------------------------------
GLL    SCLK=         0  SCET=(1990, 44, 5,58,16,962)
Camera= 1  Focal length= 1501.04 mm  Picture scale=65.6168 pixels/mm
Target=VENUS        Radii=( 6137.0, 6137.0, 6137.0)
Solar range=     107564533. (lat,lon)=( -2.579,129.476)
Spacecraft range=  1629972. (lat,lon)=( -2.954,176.320)
Object Space target center (line,samp)=(      143.26,      473.60)

 C-Matrix:  -0.91898043   0.06314746  -0.38921377
            -0.37781762  -0.42345808   0.82336936
            -0.11282203   0.90371215   0.41300791

ME-Matrix:  -0.33169343  -0.94320206   0.01869081
             0.86714712  -0.31263036  -0.38770881
             0.37153106  -0.11239278   0.92159239

OM-Matrix:  -0.06472057   0.99758188   0.02533080
            -0.05238892  -0.02874568   0.99821295
             0.99652729   0.06327786   0.05412267

RS vector=(  -1624449.25,   -104477.87,    -84007.70)
RA, DEC, TWIST=(  115.30,   24.39,   82.88)

North angle= 88.55 deg clockwise from right
Spacecraft distance from planet=  1629972.

Picture scale calculated at target center
Computations at (line,samp)=(  143.14,  473.63)
 (lat,lon)=(   -2.95,  176.32)
 Incidence=  46.79  Emission=   0.00  Phase=  46.79
 Horizontal scale=   16.488  Vertical scale=   16.488 km/pixel
 Slant range=  1623834.88 km
 Azimuths: Solar, North, Spacecraft=(   179.31,    88.55,   105.99)

gspice inp=/project/test_work/testdata/gll/s0368991900.5 targ=JUPITER 'ring
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Observation=C3RSRINGS_02

Coordinate system is: J2000
CKNAME=NAIF  SPKID=N143  PROGRAM=1-07-9  K2.PLT  PLTF  7 /CK/ V

-----------------------------------------------------------
GLL    SCLK= 368991900  SCET=(1996,314, 7,15, 2,606)
Camera= 1  Focal length= 1501.04 mm  Picture scale=65.6168 pixels/mm
Target=JUPITER      Radii=(71492.0,71492.0,66854.0)
Solar range=     771381469. (lat,lon)=( -1.258,342.605)
Spacecraft range=  2331298. (lat,lon)=(  0.478,162.145)
Object Space target center (line,samp)=(       92.77,    -4907.11)

 C-Matrix:  -0.88999907  -0.02704949  -0.45515928
            -0.40065617  -0.43013119   0.80898813
            -0.21766092   0.90236106   0.37197881

ME-Matrix:  -0.10700033  -0.99415101  -0.01465244
             0.89809829  -0.09031837  -0.43042078
             0.42657987  -0.05921449   0.90250942

OM-Matrix:  -0.35744820   0.93386877  -0.01094963
             0.00152328   0.01230718   0.99992310
             0.93393172   0.35740404  -0.00582173

RS vector=(  -2218934.08,   -714775.43,     19439.93)
RA, DEC, TWIST=(  119.36,   21.84,   76.44)

North angle= 90.63 deg clockwise from right
Spacecraft distance from planet=  2331298.

Ring Image Observation
Computations at (line,samp)=(   95.77,   63.35)
 (rad,lon)=( 180000.00,  200.00)
 Incidence=  88.74  Emission=  90.51  Phase= 176.56
 Horizontal scale=   34.096  Vertical scale=   29.625 km/pixel
 Slant range=  2191966.50 km
 Azimuths: Solar, North, Spacecraft=(   175.27,    90.63,   301.26)

if ($syschar(1) = "UNIX")
  let path4="/project/test_work/testdata/cassini/casIss/"
end-if
gspice inp=/project/test_work/testdata/cassini/casIss/n1354897340.1
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Observation=ISS_C23JU_18ATM2X2B181_PRIME

Coordinate system is: J2000
CKNAME=NAIF  SPKID=N009  PROGRAM=Y CASS  

-----------------------------------------------------------
CASSI  SCLK=1354897340  SCET=(2000,342,16,10,56,162)
Camera= 1  Focal length= 2000.00 mm  Picture scale=83.3333 pixels/mm
Target=JUPITER      Radii=(71492.0,71492.0,66854.0)
Solar range=     753807730. (lat,lon)=(  2.943, 40.735)
Spacecraft range= 23592884. (lat,lon)=(  3.573, 47.782)
Object Space target center (line,samp)=(      138.96,      872.57)

 C-Matrix:   0.86661596  -0.01899372   0.49861410
            -0.45502875   0.37997546   0.80533688
            -0.20475746  -0.92480154   0.32065011

ME-Matrix:   0.30465352   0.95235049  -0.01465513
            -0.86161293   0.26900345  -0.43041876
            -0.40596724   0.14375564   0.90251034

OM-Matrix:   0.73920107   0.67348279  -0.00164319
             0.04226085  -0.04881940  -0.99791317
            -0.67215757   0.73758904  -0.06454931

RS vector=(  15822560.27, -17438721.55,   1470284.87)
RA, DEC, TWIST=(   58.24,   18.70,  192.48)

North angle=269.91 deg clockwise from right
Spacecraft distance from planet= 23592884.

Picture scale calculated at target center
Computations at (line,samp)=(  138.96,  872.57)
 (lat,lon)=(    3.57,   47.78)
 Incidence=   7.13  Emission=   0.51  Phase=   7.06
 Horizontal scale=  141.048  Vertical scale=  141.139 km/pixel
 Slant range= 23521412.00 km
 Azimuths: Solar, North, Spacecraft=(     8.91,   269.91,   134.03)

gspice inp=/project/test_work/testdata/cassini/casIss/w379.img
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Observation=VIMS_C23RI_RPHASE75L_PRIME

Coordinate system is: J2000
CKNAME=NAV   SPKID=N009  PROGRAM=NAV     SPE

-----------------------------------------------------------
CASSI  SCLK=1357089379  SCET=(2001,  2, 1, 4,40, 72)
Camera=22  Focal length=  200.74 mm  Picture scale=41.6667 pixels/mm
Target=JUPITER      Radii=(71492.0,71492.0,66854.0)
Solar range=     754922776. (lat,lon)=(  2.899,165.022)
Spacecraft range= 10108159. (lat,lon)=( -0.761, 89.984)
Object Space target center (line,samp)=(      150.83,      254.79)

 C-Matrix:  -0.01828159  -0.57313798  -0.81925493
             0.40923507  -0.75190110   0.51688625
            -0.91224582  -0.32581835   0.24829411

ME-Matrix:   0.58389750  -0.81169510  -0.01465517
             0.72903667   0.53220791  -0.43041872
             0.35716837   0.24063626   0.90251036

OM-Matrix:  -0.03815255   0.01311779  -0.99918582
            -0.99918931  -0.01335813   0.03797732
            -0.01284907   0.99982473   0.01361681

RS vector=(      2824.70, -10107266.37,   -134267.73)
RA, DEC, TWIST=(  147.75,   14.38,  250.35)

North angle=177.82 deg clockwise from right
Spacecraft distance from planet= 10108159.

Ring Image Observation
Computations at (line,samp)=(  150.91,  252.77)
 (rad,lon)=( 180000.00,   90.00)
 Incidence=  87.10  Emission=  90.77  Phase=  75.11
 Horizontal scale=87745.062  Vertical scale= 1186.897 km/pixel
 Slant range=  9928175.00 km
 Azimuths: Solar, North, Spacecraft=(    88.02,   177.82,   135.12)

gspice inp=/project/test_work/testdata/cassini/casIss/n013.img
Beginning VICAR task gspice
GSPICE Version Oct 2, 2002
Observation=ISS_C23RI_RMOV020_PRIME

Coordinate system is: J2000
CKNAME=NAIF  SPKID=N009  PROGRAM=Y CASS  

-----------------------------------------------------------
CASSI  SCLK=1355372013  SCET=(2000,348, 4, 2, 5,505)
Camera=41  Focal length= 2000.00 mm  Picture scale=20.8333 pixels/mm
Target=JUPITER      Radii=(71492.0,71492.0,66854.0)
Solar range=     754046185. (lat,lon)=(  2.933,143.007)
Spacecraft range= 19037360. (lat,lon)=(  3.413,143.632)
Object Space target center (line,samp)=(      127.43,      469.93)

 C-Matrix:   0.92266677  -0.01014659   0.38546475
            -0.35285266   0.38091911   0.85463187
            -0.15550249  -0.92455269   0.34788115

ME-Matrix:   0.86156136  -0.50744187  -0.01465514
             0.45263416   0.78093663  -0.43041875
             0.22985723   0.36419875   0.90251035

OM-Matrix:   0.59947750  -0.80038913  -0.00199001
            -0.04884003  -0.03409844  -0.99822440
             0.79890011   0.59851026  -0.05953228

RS vector=( -15302101.25, -11268641.82,   1133349.68)
RA, DEC, TWIST=(   65.72,   20.36,  189.55)

North angle=269.89 deg clockwise from right
Spacecraft distance from planet= 19037360.

Ring Image Observation
Computations at (line,samp)=(  147.50,  251.55)
 (rad,lon)=( 180000.00,  177.00)
 Incidence=  87.07  Emission=  86.56  Phase=   0.60
 Horizontal scale=  814.343  Vertical scale=  547.474 km/pixel
 Slant range= 18887560.00 km
 Azimuths: Solar, North, Spacecraft=(    87.17,   269.89,    92.74)

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


NOTE:  the test log on linux is identical to this one.
$ Return
$!#############################################################################
