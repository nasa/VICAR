$!****************************************************************************
$!
$! Build proc for MIPL module mapcoord
$! VPACK Version 1.9, Friday, March 12, 2010, 16:58:49
$!
$! Execute by entering:		$ @mapcoord
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
$ write sys$output "*** module mapcoord ***"
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
$ write sys$output "Invalid argument given to mapcoord.com file -- ", primary
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
$   if F$SEARCH("mapcoord.imake") .nes. ""
$   then
$      vimake mapcoord
$      purge mapcoord.bld
$   else
$      if F$SEARCH("mapcoord.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mapcoord
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mapcoord.bld "STD"
$   else
$      @mapcoord.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mapcoord.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mapcoord.com -mixed -
	-s mapcoord.f -
	-p mapcoord.pdf -
	-i mapcoord.imake -
	-t tstmapcoord.pdf tstmapcoord.log_solos tstmapcoord.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mapcoord.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create mapcoord.pdf
PROCESS HELP=*
LOCAL DLAT REAL COUNT=(0:80)
LOCAL DLON REAL COUNT=(0:80)
PARM INP       TYPE=STRING  COUNT=1
PARM COORDTYPE TYPE=KEYWORD COUNT=0:1 VALID=(LINSAM,LATLON) DEFAULT=LINSAM
PARM COORDIN   TYPE=REAL    COUNT=(1:160)  DEFAULT=(1,1)
PARM OUT_LAT  NAME DEFAULT=DLAT
PARM OUT_LON  NAME DEFAULT=DLON
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=REMOTE
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
VICAR PROGRAM MAPCOORD
.HELP
PURPOSE:

MAPCOORD converts from (line,sample) to (latitude,longitude) coordinates, or
vice-versa, for a given VICAR image.

EXECUTION:

    MAPCOORD INP=A COORDTYPE=LINSAM COORDIN=(1,1,45,700,600,600)
    MAPCOORD INP=A COORDTYPE=LATLON COORDIN=(22.7,33.5,90.,0.)
where
    INP is a VICAR image.  INP may be a map-projected or raw (image or object
        space) image.
    COORDTYPE specifies whether the input coordinates are line-sample or
        latitude-longitude.

.page
OPERATION:

If the input image has a map-projection label (created by MAP3 or PERSLAB),
then this label is used to determine the image geometry.

If not, the image is assumed to be an unprojected image (either image-space or
object-space). The program searches the label history for GEOM or FARENC to
determine whether it is image or object space.  If the image is image-space,
nominal geometric distortion parameters are generated.  An attempt is made to
retrieve the SPICE data for the image.  Only if all this is successful will the
program continue with the line-sample to lat-lon calculations.

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

A complete list of CK and SPK IDs are located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

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

.page
HISTORY:

WRITTEN BY      : JEAN LORRE	07/01/93
PORTED TO VICAR : RRP          04/21/97
REVISIONS:

21 MAY 98  GMY  Replace call to GETSPICE with call to GETSPICE4.

.LEVEL1

.VARI INP
 STANDARD VICAR IMAGE
.VARI COORDTYPE
 SPECIFY IF POINT
 IS/ARE IN LATLON
 OR LINE-SAMP
 FORMAT
.VARI COORDIN
LINE,SAMPLE OR
LAT,LON
 VALUES
.VARIABLE SEDRSRC
 KEYWORD--OPTIONAL
 SPECIFIES DESIRED SOURCE
 OF C-MATRIX, ME-MATRIX,
 AND RS-VECTOR.
.VARI OUT_LAT
OUTPUT VALUES.
Can be latitude
or line values
depending on
coordtype.
.VARI OUT_LON
OUTPUT VALUES.
Can be longitudes
or samples
depending upon
coordtype.
.LEVEL1
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
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
 THE NAME OF A STANDARD VICAR IMAGE FILE FROM WHOM COORDINATES WILL BE
MEASURED.

.VARI COORDTYPE
  SPECIFY IF THE GIVEN COORDIN IS LATLON OR LINESAMP.
.VARI COORDIN
(LINE,SAMPLE) VALUES INPUT IN PAIRS. UP TO 80 INPUT PAIRS.
FOR EACH PAIR THE CORRESPONDING LAT,LON WILL BE COMPUTED AND PRINTED.

.VARIABLE SEDRSRC
 KEYWORD--OPTIONAL
 SPECIFIES DESIRED SOURCE OF C-MATRIX, ME-MATRIX, AND TARGET-TO-SPACECRAFT
 VECTOR (RS).  VALID VALUES ARE:
	DAVI	--DATA DETERMINED BY MERT DAVIES
	NAV     --DATA DETERMINED BY PROGRAM NAV
	FARE    --DATA DETERMINED BY PROGRAM FARENC
	NAV2    --DATA DETERMINED BY PROGRAM NAV2
	NEAR    --DATA DETERMINED BY PROGRAM NEARENC
 IF THIS KEYWORD IS NOT SPECIFIED, OR IF THE SPECIFIED SOURCE DOES NOT EXIST,
 THE NAVIGATION DATA WILL BE SELECTED FROM THE AVAILABLE SOURCES, IN THE
 FOLLOWING ORDER OF PRIORITY: (1) DAVI, (2) NAV, (3) FARE, (4) NAV2,
 (5) NEAR, (6) PREDICT OR FINAL SEDR.

.VARIABLE OUT_LAT
AN OUTPUT NAME PARAMETER THAT RETURNS THE LATITUDES or line value FOR 
EACH INPUT POINT.
If COORDTYPE=LINSAM then OUT_LAT is a latitude.
If COORDTYPE=LATLON then OUT_LAT is a line number.
THE VARIABLE GIVEN FOR OUT_LAT SHOULD BE OF TYPE REAL AND SHOULD HAVE A COUNT
BIG ENOUGH TO HOLD ALL THE COORDINATES.  THE MAXIMUM IS 80 COORDINATES.

NOTE THAT THE INPUT COORDINATES ARE IN A SINGLE PARAMETER, COORDIN, AS PAIRS,
WHILE THE OUTPUT COORDINATES ARE IN TWO SEPARATE PARAMETERS, OUT_LAT AND
OUT_LON, WHICH ARE EACH HALF AS BIG.

OUT_LAT IS OPTIONAL; IF NO VARIABLE IS GIVEN THEN THE VALUES ARE NOT RETURNED.

.VARIABLE OUT_LON
AN OUTPUT NAME PARAMETER THAT RETURNS THE LONGITUDES or sample values FOR 
EACH INPUT POINT.
If COORDTYPE=LINSAM then OUT_LON is a longitude.
If COORDTYPE=LATLON then OUT_LON is a sample number.
THE VARIABLE GIVEN FOR OUT_LON SHOULD BE OF TYPE REAL AND SHOULD HAVE A COUNT
BIG ENOUGH TO HOLD ALL THE COORDINATES.  THE MAXIMUM IS 80 COORDINATES.

NOTE THAT THE INPUT COORDINATES ARE IN A SINGLE PARAMETER, COORDIN, AS PAIRS,
WHILE THE OUTPUT COORDINATES ARE IN TWO SEPARATE PARAMETERS, OUT_LAT AND
OUT_LON, WHICH ARE EACH HALF AS BIG.

OUT_LON IS OPTIONAL; IF NO VARIABLE IS GIVEN THEN THE VALUES ARE NOT RETURNED.

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

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
$ create mapcoord.imake
#define PROGRAM mapcoord 
#define R2LIB
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define MODULE_LIST mapcoord.f
#define FTNINC_LIST pgminc mp_for_defs
#define LIB_NETWORK
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
#define LIB_SPICE
#define LIB_MATH77
/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstmapcoord.pdf
PROCEDURE
  REFGBL $ECHO
  REFGBL $SYSCHAR
  PARM INP TYPE=STRING COUNT=(0:1) DEFAULT=--
BODY

  LET _ONFAIL="CONTINUE"
  LET $ECHO="NO"

  LOCAL MGLL TYPE=STRING INIT="/project/test_work/testdata/mipl/gll/"
  LOCAL MVGR TYPE=STRING INIT="/project/test_work/testdata/mipl/vgr/"

  LOCAL DLAT REAL COUNT=(0:80)
  LOCAL DLON REAL COUNT=(0:80)

  LOCAL IDATA1 REAL
  LOCAL IDATA2 REAL
  LOCAL IDATA3 REAL
  LOCAL IDATA4 REAL

  LOCAL ODATA1 REAL
  LOCAL ODATA2 REAL
  LOCAL ODATA3 REAL
  LOCAL ODATA4 REAL
  LOCAL ODATA5 REAL
  LOCAL ODATA6 REAL

  IF ($SYSCHAR(1)="VAX_VMS")
    LET MGLL = "wms_test_work:[testdata.mipl.gll]"
    LET MVGR = "wms_test_work:[testdata.mipl.vgr]"
    DCL SELCAT O
  ELSE
    ush setenv SELCAT o; source /usr/local/share/selcat.csh
  END-IF
  
  WRITE ""
  WRITE ""
  WRITE "-------------------------------------------------------------"
  WRITE "|-----------------------------------------------------------|"
  WRITE "|-----------------------------------------------------------|"
  WRITE "|---------------------TEST CASE 1---------------------------|" 
  WRITE "|-----------------------------------------------------------|"
  WRITE "|-----------------------------------------------------------|"
  WRITE "-------------------------------------------------------------"
  WRITE ""
  WRITE ""
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 1 : INITIALIZE INPUT DATA                             |"
  WRITE "-------------------------------------------------------------"

  LET $ECHO="YES"
  LET IDATA1=400.0
  LET IDATA2=500.0
  LET IDATA3=600.0
  LET $ECHO="NO"

  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 1 : ON THE ORIGINAL IMAGE USING GLLPHOT.              |"
  WRITE "|         RESULTS FOR HERE SHOULD MATCH RESULTS FROM MAPCOORD |"
  WRITE "|         RUN.                                              |"
  WRITE "|Note added 12-Mar-2010 (LWK):  GLLPHOT has been obsoleted, |"
  WRITE "|so its output can no longer be used as a benchmark.  Tests |"
  WRITE "|will have to use logs of previous runs of this proc.       |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
# GLLPHOT INP=&"MGLL"earth1.img COORDIN=(&IDATA1,&IDATA1) 'LINESAMP
# GLLPHOT INP=&"MGLL"earth1.img COORDIN=(&IDATA2,&IDATA2) 'LINESAMP
# GLLPHOT INP=&"MGLL"earth1.img COORDIN=(&IDATA3,&IDATA3) 'LINESAMP
  MAPCOORD INP=&"MGLL"earth1.img +
     COORDIN=(&IDATA1,&IDATA1,&IDATA2,&IDATA2,&IDATA3,&IDATA3) +
     OUT_LAT=DLAT OUT_LON=DLON
  LET $ECHO="NO"
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 1 : NOW ADD MAP PROJECTION LABEL TO THE INPUT.        |"
  WRITE "|         THERE IS NO PROJECTION DONE. ONLY A LABEL WILL BE |"
  WRITE "|         ADDED.                                            |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
  PERSLAB INP=&"MGLL"earth1.img OUT=earth1_map.img
  LET $ECHO="NO"
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 1 : NOW RUN MAPCOORD ON THE OUTPUT IMAGE.               |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
  MAPCOORD INP=earth1_map.img +
     COORDIN=(&IDATA1,&IDATA1,&IDATA2,&IDATA2,&IDATA3,&IDATA3) +
     OUT_LAT=DLAT OUT_LON=DLON
  LET $ECHO="NO"

  LET ODATA1=DLAT(1)
  LET ODATA2=DLON(1)
  LET ODATA3=DLAT(2)
  LET ODATA4=DLON(2)
  LET ODATA5=DLAT(3)
  LET ODATA6=DLON(3)
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 1 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |"
  WRITE "|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
  MAPCOORD INP=earth1_map.img COORDTYPE=LATLON +
     COORDIN=(&ODATA1,&ODATA2,&ODATA3,&ODATA4,&ODATA5,&ODATA6) +
     OUT_LAT=DLAT OUT_LON=DLON
  LET $ECHO="NO"

  LET ODATA1=DLAT(1)
  LET ODATA2=DLON(1)
  LET ODATA3=DLAT(2)
  LET ODATA4=DLON(2)
  LET ODATA5=DLAT(3)
  LET ODATA6=DLON(3)

  IF (ODATA1 <> IDATA1)
    WRITE "MISMATCH &ODATA1 &IDATA1"
  END-IF
  IF (ODATA2 <> IDATA1)
    WRITE "MISMATCH &ODATA2 &IDATA1"
  END-IF
  IF (ODATA3 <> IDATA2)
    WRITE "MISMATCH &ODATA3 &IDATA2"
  END-IF
  IF (ODATA4 <> IDATA2)
    WRITE "MISMATCH &ODATA4 &IDATA2"
  END-IF
  IF (ODATA5 <> IDATA3)
    WRITE "MISMATCH &ODATA5 &IDATA3"
  END-IF
  IF (ODATA6 <> IDATA3)
    WRITE "MISMATCH &ODATA6 &IDATA3"
  END-IF

  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 2 : Voyaager image-space test                         |"
  WRITE "-------------------------------------------------------------"

  MAPCOORD INP=&"MVGR"f1636832.raw TARGET=IO +
     COORDIN=(&IDATA1,&IDATA1,&IDATA2,&IDATA2,&IDATA3,&IDATA3) +
     OUT_LAT=DLAT OUT_LON=DLON
  
  WRITE ""
  WRITE ""
  WRITE "-------------------------------------------------------------"
  WRITE "|-----------------------------------------------------------|"
  WRITE "|-----------------------------------------------------------|"
  WRITE "|---------------------TEST CASE 3---------------------------|" 
  WRITE "|-----------------------------------------------------------|"
  WRITE "|-----------------------------------------------------------|"
  WRITE "-------------------------------------------------------------"
  WRITE ""
  WRITE ""

  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 3 : INITIALIZE INPUT DATA                             |"
  WRITE "-------------------------------------------------------------"

  LET $ECHO="YES"
  LET IDATA1=825.0
  LET IDATA2=867.0
  LET IDATA3=897.0
  LET IDATA4=672.0
  LET $ECHO="NO"

  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 3 : USE A MOSAIC IMAGE AS NEXT TEST.                  |"
  WRITE "|   THE IMAGE 4600.byt AND 4605.byt ARE USED TO CREATE      |"
  WRITE "|   e6global.nmos. I HAVE ARBITRARILY PICKED POINTS OFF     |"
  WRITE "|   e6global.nmos, THAT I THOUGHT ARE SOMEWHAT RECOGNIZABLE |"
  WRITE "|   ON 460 IMAGES AND THAT IS THE REASON WHY THE LONGITUDE  |"
  WRITE "|   IS LITTLE OFF.                                          |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
# GLLPHOT INP=&"MGLL"4600.byt COORDIN=(195,398) 'LINESAMP
# GLLPHOT INP=&"MGLL"4605.byt COORDIN=(230,240) 'LINESAMP
  LET $ECHO="NO"
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 3 : MAPCOORD ON THE POINTS THAT I THINK MATCH WHEN    |"
  WRITE "|         GIVEN A VISUAL TEST.                              |"
  WRITE "-------------------------------------------------------------"
  
  LET $ECHO="YES"
  MAPCOORD INP=&"MGLL"e6global.nmos COORDIN=(&IDATA1,&IDATA2,&IDATA3,&IDATA4) +
     OUT_LAT=DLAT OUT_LON=DLON
  LET $ECHO="NO"

  LET ODATA1=DLAT(1)
  LET ODATA2=DLON(1)
  LET ODATA3=DLAT(2)
  LET ODATA4=DLON(2)
  
  WRITE "-------------------------------------------------------------"
  WRITE "|TEST 3 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |"
  WRITE "|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |"
  WRITE "-------------------------------------------------------------"

  LET $ECHO="YES"
  MAPCOORD INP=&"MGLL"e6global.nmos COORDTYPE=LATLON +
     COORDIN=(&ODATA1,&ODATA2,&ODATA3,&ODATA4) +
     OUT_LAT=DLAT OUT_LON=DLON
  LET $ECHO="NO"

  LET ODATA1=DLAT(1)
  LET ODATA2=DLON(1)
  LET ODATA3=DLAT(2)
  LET ODATA4=DLON(2)

  IF (ODATA1 <> IDATA1)
    WRITE "MISMATCH &ODATA1 &IDATA1"
  END-IF
  IF (ODATA2 <> IDATA2)
    WRITE "MISMATCH &ODATA2 &IDATA2"
  END-IF
  IF (ODATA3 <> IDATA3)
    WRITE "MISMATCH &ODATA3 &IDATA3"
  END-IF
  IF (ODATA4 <> IDATA4)
    WRITE "MISMATCH &ODATA4 &IDATA4"
  END-IF

  WRITE "-------------------------------------------------------------"
  WRITE "|REMOVING IMAGES CREATED INTERMEDIATELY                     |"
  WRITE "-------------------------------------------------------------"
  
  IF ($SYSCHAR(1)="VAX_VMS")
    DCL DEL EARTH1_MAP.IMG;*
  ELSE
    USH rm earth1_map.img
  END-IF
END-PROC
$!-----------------------------------------------------------------------------
$ create tstmapcoord.log_solos
tstmapcoord


-------------------------------------------------------------
|-----------------------------------------------------------|
|-----------------------------------------------------------|
|---------------------TEST CASE 1---------------------------|
|-----------------------------------------------------------|
|-----------------------------------------------------------|
-------------------------------------------------------------


-------------------------------------------------------------
|TEST 1 : INITIALIZE INPUT DATA                             |
-------------------------------------------------------------
  LET IDATA1=400.0
  LET IDATA2=500.0
  LET IDATA3=600.0
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : ON THE ORIGINAL IMAGE USING GLLPHOT.              |
|         RESULTS FOR HERE SHOULD MATCH RESULTS FROM MAPCOORD |
|         RUN.                                              |
|Note added 12-Mar-2010 (LWK):  GLLPHOT has been obsoleted, |
|so its output can no longer be used as a benchmark.  Tests |
|will have to use logs of previous runs of this proc.       |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/+
testdata/mipl/gll/earth1.img       COORDIN=(4.000000000000e+02,4+
.000000000000e+02,5.000000000000e+02,5.000000000000e+02,6.000000000000e+02,6.000000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
Creating nominal geom parameters
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  400.00 |   400.00   ||  -41.57478   |   28.09799 |
|---------|------------||--------------|------------|
|  499.99 |   499.99   ||  -31.99328   |   63.26208 |
|---------|------------||--------------|------------|
|  599.89 |   599.89   ||  -14.87613   |   87.74152 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW ADD MAP PROJECTION LABEL TO THE INPUT.        |
|         THERE IS NO PROJECTION DONE. ONLY A LABEL WILL BE |
|         ADDED.                                            |
-------------------------------------------------------------
  PERSLAB INP=/project/test_work/testdata/mipl/gll/earth1.img OUT=earth1_map.img
Beginning VICAR task PERSLAB
PERSLAB version (August 28, 2002)
Project is GLL
GETLABCON: warning indicator
Correcting the NORTH_ANGLE with 90-degree offset
Target planet body is: EARTH
Planet id number        399
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW RUN MAPCOORD ON THE OUTPUT IMAGE.               |
-------------------------------------------------------------
  MAPCOORD INP=earth1_map.img  +
     COORDIN=(4.000000000000e+02,4.000000000000e+02,5.0000000000+
00e+02,5.000000000000e+02,6.000000000000e+02,6.000000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  400.00 |   400.00   ||  -41.57473   |   28.09827 |
|---------|------------||--------------|------------|
|  500.00 |   500.00   ||  -31.99133   |   63.26593 |
|---------|------------||--------------|------------|
|  600.00 |   600.00   ||  -14.85442   |   87.76544 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |
|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |
-------------------------------------------------------------
  MAPCOORD INP=earth1_map.img COORDTYPE=LATLON  +
     COORDIN=(-4.157473373413e+01,2.809826660156e+01,-3.19913311+
0046e+01,6.326593017578e+01,-1.485441780090e+01,8.776544189453e+01)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LAT-LON.
 LABEL SAYS THE PROJECT IS GLL
|---------------------------||----------------------|
|      WEST IN DEGREES      ||   OBJECT SPACE       |
|--------------|------------||---------|------------|
|  LATITUDE    |  LONGITUDE ||  LINE   |   SAMPLE   |
|--------------|------------||---------|------------|
|  -41.57473   |   28.09827 ||  400.00 |   400.00   |
|--------------|------------||---------|------------|
|  -31.99133   |   63.26593 ||  500.00 |   500.00   |
|--------------|------------||---------|------------|
|  -14.85442   |   87.76544 ||  600.00 |   600.00   |
|---------------------------||----------------------|
MAPCOORD task completed.
  LET $ECHO="NO"
MISMATCH 3.999989929199e+02 4.000000000000e+02
MISMATCH 4.000016784668e+02 4.000000000000e+02
MISMATCH 5.000005493164e+02 5.000000000000e+02
MISMATCH 5.000020751953e+02 5.000000000000e+02
MISMATCH 6.000000610352e+02 6.000000000000e+02
MISMATCH 6.000009765625e+02 6.000000000000e+02
-------------------------------------------------------------
|TEST 2 : Voyaager image-space test                         |
-------------------------------------------------------------
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
 LABEL SAYS THE PROJECT IS VGR-1
Creating nominal geom parameters
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  497.99 |   499.40   ||  -12.75861   |  175.23819 |
|---------|------------||--------------|------------|
|  614.66 |   615.43   ||   -6.70656   |  142.17203 |
|---------|------------||--------------|------------|
|  731.55 |   732.23   ||   -0.61849   |  102.66302 |
|----------------------||---------------------------|

MAPCOORD task completed.


-------------------------------------------------------------
|-----------------------------------------------------------|
|-----------------------------------------------------------|
|---------------------TEST CASE 3---------------------------|
|-----------------------------------------------------------|
|-----------------------------------------------------------|
-------------------------------------------------------------


-------------------------------------------------------------
|TEST 3 : INITIALIZE INPUT DATA                             |
-------------------------------------------------------------
  LET IDATA1=825.0
  LET IDATA2=867.0
  LET IDATA3=897.0
  LET IDATA4=672.0
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : USE A MOSAIC IMAGE AS NEXT TEST.                  |
|   THE IMAGE 4600.byt AND 4605.byt ARE USED TO CREATE      |
|   e6global.nmos. I HAVE ARBITRARILY PICKED POINTS OFF     |
|   e6global.nmos, THAT I THOUGHT ARE SOMEWHAT RECOGNIZABLE |
|   ON 460 IMAGES AND THAT IS THE REASON WHY THE LONGITUDE  |
|   IS LITTLE OFF.                                          |
-------------------------------------------------------------
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : MAPCOORD ON THE POINTS THAT I THINK MATCH WHEN    |
|         GIVEN A VISUAL TEST.                              |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/testdata/mipl/gll/e6global.nmos COORDIN=(8.25+
0000000000e+02,8.670000000000e+02,8.970000000000e+02,6.720000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  825.00 |   867.00   ||  -41.17764   |  195.48232 |
|---------|------------||--------------|------------|
|  897.00 |   672.00   ||  -38.42732   |  214.69397 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |
|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/testdata/mipl/gll/e6+
global.nmos COORDTYPE=LATLON       COORDIN=(-4.1+
17763900757e+01,1.954823150635e+02,-3.842732238770e+01,2.146939697266e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LAT-LON.
 LABEL SAYS THE PROJECT IS GLL
|---------------------------||----------------------|
|      WEST IN DEGREES      ||   OBJECT SPACE       |
|--------------|------------||---------|------------|
|  LATITUDE    |  LONGITUDE ||  LINE   |   SAMPLE   |
|--------------|------------||---------|------------|
|  -41.17764   |  195.48232 ||  825.00 |   867.00   |
|--------------|------------||---------|------------|
|  -38.42732   |  214.69397 ||  897.00 |   672.00   |
|---------------------------||----------------------|
MAPCOORD task completed.
  LET $ECHO="NO"
MISMATCH 8.969999389648e+02 8.970000000000e+02
-------------------------------------------------------------
|REMOVING IMAGES CREATED INTERMEDIATELY                     |
-------------------------------------------------------------
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstmapcoord.log_linux
tstmapcoord


-------------------------------------------------------------
|-----------------------------------------------------------|
|-----------------------------------------------------------|
|---------------------TEST CASE 1---------------------------|
|-----------------------------------------------------------|
|-----------------------------------------------------------|
-------------------------------------------------------------


-------------------------------------------------------------
|TEST 1 : INITIALIZE INPUT DATA                             |
-------------------------------------------------------------
  LET IDATA1=400.0
  LET IDATA2=500.0
  LET IDATA3=600.0
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : ON THE ORIGINAL IMAGE USING GLLPHOT.              |
|         RESULTS FOR HERE SHOULD MATCH RESULTS FROM MAPCOORD |
|         RUN.                                              |
|Note added 12-Mar-2010 (LWK):  GLLPHOT has been obsoleted, |
|so its output can no longer be used as a benchmark.  Tests |
|will have to use logs of previous runs of this proc.       |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/+
testdata/mipl/gll/earth1.img       COORDIN=(4.000000000000e+02,4+
.000000000000e+02,5.000000000000e+02,5.000000000000e+02,6.000000000000e+02,6.000000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
Creating nominal geom parameters
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  400.00 |   400.00   ||  -41.57478   |   28.09798 |
|---------|------------||--------------|------------|
|  499.99 |   499.99   ||  -31.99328   |   63.26208 |
|---------|------------||--------------|------------|
|  599.89 |   599.89   ||  -14.87613   |   87.74151 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW ADD MAP PROJECTION LABEL TO THE INPUT.        |
|         THERE IS NO PROJECTION DONE. ONLY A LABEL WILL BE |
|         ADDED.                                            |
-------------------------------------------------------------
  PERSLAB INP=/project/test_work/testdata/mipl/gll/earth1.img OUT=earth1_map.img
Beginning VICAR task PERSLAB
PERSLAB version (August 28, 2002)
Project is GLL
GETLABCON: warning indicator
Correcting the NORTH_ANGLE with 90-degree offset
Target planet body is: EARTH
Planet id number        399
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW RUN MAPCOORD ON THE OUTPUT IMAGE.               |
-------------------------------------------------------------
  MAPCOORD INP=earth1_map.img  +
     COORDIN=(4.000000000000e+02,4.000000000000e+02,5.0000000000+
00e+02,5.000000000000e+02,6.000000000000e+02,6.000000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  400.00 |   400.00   ||  -41.57473   |   28.09828 |
|---------|------------||--------------|------------|
|  500.00 |   500.00   ||  -31.99133   |   63.26594 |
|---------|------------||--------------|------------|
|  600.00 |   600.00   ||  -14.85442   |   87.76544 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 1 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |
|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |
-------------------------------------------------------------
  MAPCOORD INP=earth1_map.img COORDTYPE=LATLON  +
     COORDIN=(-4.157473373413e+01,2.809827804565e+01,-3.19913311+
0046e+01,6.326593780518e+01,-1.485441780090e+01,8.776544189453e+01)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LAT-LON.
 LABEL SAYS THE PROJECT IS GLL
|---------------------------||----------------------|
|      WEST IN DEGREES      ||   OBJECT SPACE       |
|--------------|------------||---------|------------|
|  LATITUDE    |  LONGITUDE ||  LINE   |   SAMPLE   |
|--------------|------------||---------|------------|
|  -41.57473   |   28.09828 ||  400.00 |   400.00   |
|--------------|------------||---------|------------|
|  -31.99133   |   63.26594 ||  500.00 |   500.00   |
|--------------|------------||---------|------------|
|  -14.85442   |   87.76544 ||  600.00 |   600.00   |
|---------------------------||----------------------|
MAPCOORD task completed.
  LET $ECHO="NO"
MISMATCH 3.999989929199e+02 4.000000000000e+02
MISMATCH 4.000016784668e+02 4.000000000000e+02
MISMATCH 5.000005493164e+02 5.000000000000e+02
MISMATCH 5.000020446777e+02 5.000000000000e+02
MISMATCH 6.000000610352e+02 6.000000000000e+02
MISMATCH 6.000009765625e+02 6.000000000000e+02
-------------------------------------------------------------
|TEST 2 : Voyaager image-space test                         |
-------------------------------------------------------------
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
 LABEL SAYS THE PROJECT IS VGR-1
Creating nominal geom parameters
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  497.99 |   499.40   ||  -12.75862   |  175.23819 |
|---------|------------||--------------|------------|
|  614.66 |   615.43   ||   -6.70657   |  142.17203 |
|---------|------------||--------------|------------|
|  731.55 |   732.23   ||   -0.61850   |  102.66304 |
|----------------------||---------------------------|

MAPCOORD task completed.


-------------------------------------------------------------
|-----------------------------------------------------------|
|-----------------------------------------------------------|
|---------------------TEST CASE 3---------------------------|
|-----------------------------------------------------------|
|-----------------------------------------------------------|
-------------------------------------------------------------


-------------------------------------------------------------
|TEST 3 : INITIALIZE INPUT DATA                             |
-------------------------------------------------------------
  LET IDATA1=825.0
  LET IDATA2=867.0
  LET IDATA3=897.0
  LET IDATA4=672.0
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : USE A MOSAIC IMAGE AS NEXT TEST.                  |
|   THE IMAGE 4600.byt AND 4605.byt ARE USED TO CREATE      |
|   e6global.nmos. I HAVE ARBITRARILY PICKED POINTS OFF     |
|   e6global.nmos, THAT I THOUGHT ARE SOMEWHAT RECOGNIZABLE |
|   ON 460 IMAGES AND THAT IS THE REASON WHY THE LONGITUDE  |
|   IS LITTLE OFF.                                          |
-------------------------------------------------------------
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : MAPCOORD ON THE POINTS THAT I THINK MATCH WHEN    |
|         GIVEN A VISUAL TEST.                              |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/testdata/mipl/gll/e6global.nmos COORDIN=(8.25+
0000000000e+02,8.670000000000e+02,8.970000000000e+02,6.720000000000e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LINE-SAMP.
 LABEL SAYS THE PROJECT IS GLL
|----------------------||---------------------------|
|   OBJECT SPACE       ||      WEST IN DEGREES      |
|---------|------------||--------------|------------|
|  LINE   |   SAMPLE   ||  LATITUDE    |  LONGITUDE |
|---------|------------||--------------|------------|
|  825.00 |   867.00   ||  -41.17764   |  195.48232 |
|---------|------------||--------------|------------|
|  897.00 |   672.00   ||  -38.42732   |  214.69397 |
|----------------------||---------------------------|

MAPCOORD task completed.
  LET $ECHO="NO"
-------------------------------------------------------------
|TEST 3 : NOW CHECK TO SEE IF WE GET SAME LINE SAMPLE WHEN  |
|         GIVEN THE SAME LAT LON FROM THE PREVIOUS INPUT.   |
-------------------------------------------------------------
  MAPCOORD INP=/project/test_work/testdata/mipl/gll/e6+
global.nmos COORDTYPE=LATLON       COORDIN=(-4.1+
17763900757e+01,1.954823150635e+02,-3.842732238770e+01,2.146939697266e+02)       OUT_LAT=DLAT OUT_LON=DLON
Beginning VICAR task MAPCOORD
MAPCOORD version May 18, 1998
GIVEN POINTS ARE TAKEN AS LAT-LON.
 LABEL SAYS THE PROJECT IS GLL
|---------------------------||----------------------|
|      WEST IN DEGREES      ||   OBJECT SPACE       |
|--------------|------------||---------|------------|
|  LATITUDE    |  LONGITUDE ||  LINE   |   SAMPLE   |
|--------------|------------||---------|------------|
|  -41.17764   |  195.48232 ||  825.00 |   867.00   |
|--------------|------------||---------|------------|
|  -38.42732   |  214.69397 ||  897.00 |   672.00   |
|---------------------------||----------------------|
MAPCOORD task completed.
  LET $ECHO="NO"
MISMATCH 8.969999389648e+02 8.970000000000e+02
-------------------------------------------------------------
|REMOVING IMAGES CREATED INTERMEDIATELY                     |
-------------------------------------------------------------
exit
slogoff
$ Return
$!#############################################################################
