$!****************************************************************************
$!
$! Build proc for MIPL module getpc
$! VPACK Version 1.9, Friday, January 31, 2003, 10:41:08
$!
$! Execute by entering:		$ @getpc
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
$ write sys$output "*** module getpc ***"
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
$ write sys$output "Invalid argument given to getpc.com file -- ", primary
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
$   if F$SEARCH("getpc.imake") .nes. ""
$   then
$      vimake getpc
$      purge getpc.bld
$   else
$      if F$SEARCH("getpc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getpc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getpc.bld "STD"
$   else
$      @getpc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getpc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getpc.com -mixed -
	-s getpc.f -
	-p getpc.pdf -
	-i getpc.imake -
	-t tstgetpc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getpc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------------------
C PROGRAM GETPC
C Purpose: Retrieves PC from SPICE kernal and assign to TAE variable.
C      For GLL and CASSI, also assigns SCLAT, SCLON, and SCALE to TAE variable.
C------------------------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      INTEGER IND
      REAL*4 C(2),DATA(40),SCL, CONV(3)
      CHARACTER*4 CKNReturn
      CHARACTER*5 Project
      INTEGER     Camera

      CALL xvmessage ('GETPC Version Jan 30, 2003',' ')
      
      CALL MSEDR(IND,Project,Camera,CKNReturn,DATA,SCL)
      IF (IND .LT. 0) THEN
        CALL ABEND
      END IF

C-----LAT LON to LINE SAMP Conversion
      IF (project.EQ.'GLL  ' .OR. project.EQ.'CASSI') THEN
        CALL mvcl (PROJECT, conv, 5)
        CALL mve (4,1,CAMERA, conv(3),1,1)
      END IF
      CALL CONVEV(IND,DATA,DATA,C(1),C(2),DATA(31),DATA(32),1,CONV)

      CALL PPC(Project,C,CKNReturn,data(31),data(32),SCL)
      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine MSEDR
C Input: none
C Output: ind => integer, 0 -> all successful
C                         1 -> non-fatal error, all fine but CK source unknown
C                        -1 -> fatal error, getspice2 failed
C                        -2 -> fatal error, getcamcon failed
C                        -3 -> fatel error, unable to determine project/camera
C         Project => CHARACTER*5, The type of project of the file specified
C                    by the INP VICAR user parameter (e.g. GLL)
C         Camera => INTEGER, the camera serial number of the file INP
C         CKReturn => CHARACTER*4, The source of C-Kernel buffer used to
C                     process DATA (e.g. DAVI, NAV, NAV2..etc)
C         DATA => 40xREAL*4, MP Buffer
C         SCL => REAL*4, Calculated Scale in KM/Pixel
C------------------------------------------------------------------------------
      	SUBROUTINE MSEDR(IND,Project,Camera,CKNReturn,DATA,SCL)
	INTEGER IND
        CHARACTER*5 Project
        CHARACTER*4 CKNReturn
        REAL*4 DATA(40)
        REAL*4 SCL

      	REAL*8 PI,R2,DTOR,GBUF8(100)
        REAL*4 GBUF4(200)
	CHARACTER*4 CKNRequest, CKID
        INTEGER*4 IntBuf
        INTEGER   Camera

        EQUIVALENCE (GBUF4, GBUF8) ! Spice Buffer, in 4 byte and 8 byte format

        Ind = 0

C-------Open input file for getspice2 and getproj
	CALL XVUNIT(IUN,'INP',1,IST, ' ')
	CALL XVOPEN(IUN,ind, ' ')
        IF (Ind.NE.1) call xvsignal(iunit,ind,1)

C-------Get Project and Camera
        CALL getproj(IUN, Project,Camera,IntBuf,ind) ! IntBuf will not be used
        IF (Ind.NE.0) THEN
          CALL xvmessage ('GETPROJ failed',' ')
          Ind = -3
          RETURN
        END IF

C-------Get Spice buffer
        call getspice2(IUN, .TRUE., GBUF8, ind)
	CALL XVCLOSE(IUN,IST, ' ')
        IF (ind.NE.1) THEN
          CALL xvmessage ('GETSPICE2 Failed', ' ')
          CALL xvmessage ('GETPC cannot continue', ' ')
          ind = -1
          RETURN
        END IF

C-------Check if desired source was found.
        CALL mvlc(GBUF4(11), CKNRequest,4)
        IF (CKNRequest.EQ.'    ') CKNRequest='NAIF'
        CALL MVLC (GBUF4(172), CKID, 4)
        CALL CKID2CKName (CKID, CKNReturn)
        IF (CKNRequest.NE.CKNReturn) THEN
          CALL XVMESSAGE ('Source '//CKNRequest//' not available',' ')
          CALL XVMESSAGE ('Retrieving from '//CKNReturn,' ')
        END IF
       
C-------Process SPICE Buffer
   50   PI = 3.141592653589793D0
	dtor = pi/180.D0
        IntBuf=8       ! need this for MVE
	CALL MVE(4, 1, IntBuf, DATA(39), 1, 1)
	CALL GETCAMCON(Project,Camera,DATA(27),DATA(28),DATA(29),
     +                 DATA(30),IND)
        IF (ind.NE.0) THEN
          CALL xvmessage ('GETCAMCON error', ' ')
          ind = -2
          RETURN
        END IF

C          PLANET RADII
      DATA(25) = GBUF8(15)
	rp2=data(25)*data(25)
      DATA(26) = GBUF8(13)
	re2=data(26)*data(26)
	r2=1.0D0*rp2/re2
C          RANGE
      DATA(38) = GBUF8(27)
C          SUBSPACECRAFT (LAT,LON)
      DATA(31) = GBUF8(30)
      DATA(32) = DMOD(GBUF8(31)+360.D0,360.D0)

C-----NORTH ANGLE
      DATA(35) = DMOD(GBUF8(68)+90.D0,360.D0)
C-----OM MATRIX
       CALL MVE(8,9,GBUF8(59),DATA, 1, 1)
C-----RS VECTOR
      CALL MVE(8,3,GBUF8(22),DATA(19), 1, 1)
C-----SCALE IN KM/PX
      SCL = GBUF8(27)/(DATA(27)*DATA(30))

      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine PPC
C Purpose: Print and output PC; if GLL or CAS also output SCLAT/LON and SCALE
C------------------------------------------------------------------------------
      SUBROUTINE PPC(Project,PC,Source,LAT,LON,SCL)
      CHARACTER*5 Project       ! Input: GLL, CASSI, VGR-1 or VGR-2
      CHARACTER*4 Source        ! Input: DAVI, NAV, NAV2...etc
      REAL*4 PC(2),LAT,LON,SCL  ! Input: SCLAT,SCLON and SCALE

      INTEGER*4 PARB(500)
      CHARACTER*80 MSG
 
      CALL XVMESSAGE(' ', ' ')
      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
 	CALL XVMESSAGE('IMAGE SPACE PLANET CENTER (LINE, SAMP)',' ')
      ELSE
        CALL xvmessage('OBJECT SPACE PLANET CENTER (LINE, SAMP)',' ')
      END IF

      WRITE (msg, '(a4, '' PC = '', f9.2, ''  '', f9.2)') Source,
     +              PC(1), PC(2)
      CALL XVMESSAGE(MSG,' ')

      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
        CALL PRNT(7,1,LAT,' SCLAT =.')
	CALL PRNT(7,1,LON,' SCLON =.')
	CALL PRNT(7,1,SCL,' SCALE (KM/PX) =.')
      END IF

      CALL XQINI(PARB,500,XABORT)
      CALL XQREAL(PARB,'PCL',1,PC(1),XADD,IST)
      CALL XQREAL(PARB,'PCS',1,PC(2),XADD,IST)

      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
        CALL XQREAL(PARB,'SCLAT',1,LAT,XADD,IST)
        CALL XQREAL(PARB,'SCLON',1,LON,XADD,IST)
        CALL XQREAL(PARB,'SCALE',1,SCL,XADD,IST)
      END IF
      CALL XVQOUT(PARB,IST)

      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine CKID2CKName
C Purpose: Converts a C-Kernel ID (CKID) to its common Name format (CKName)
C
C Usage  : This procedure is useful in converting the 172nd element of the 4x200
C          SPICE buffer to common recognized SOURCE name (DAVI, NAV, NAIF, etc)
C
C Input: CKID    => CHARACTER*4, CKID as returned by the 172nd element of the
C                   4x200 SPICE buffer.
C Output: CKName => CHARACTER*4, Common recognized SPICE source name such as:
C                     NAIF, AMOS, NEAR, NAV2, FAR, NAV, DAVI, etc.
C------------------------------------------------------------------------------
      SUBROUTINE CKID2CKName (CKID, CKName)
      IMPLICIT NONE
      CHARACTER*4 CKID
      CHARACTER*4 CKName

      CKNAME='NAIF'     ! default to NAIF
      IF (CKID.EQ.'M901') CKNAME='AMOS'
      IF (CKID.EQ.'M902') CKNAME='NEAR'
      IF (CKID.EQ.'M903') CKNAME='NAV2'
      IF (CKID.EQ.'M904') CKNAME='FARE'
      IF (CKID.EQ.'M905') CKNAME='NAV'
      IF (CKID.EQ.'M906') CKNAME='DAVI'
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create getpc.pdf
PROCESS help=*
LOCAL DUM1 REAL
LOCAL DUM2 REAL
LOCAL DUM3 REAL
LOCAL DUM4 REAL
LOCAL DUM5 REAL

PARM INP    STRING
PARM PCL    NAME     DEFAULT=DUM1 
PARM PCS    NAME     DEFAULT=DUM2
PARM SCLAT  NAME     DEFAULT=DUM3
PARM SCLON  NAME     DEFAULT=DUM4
PARM SCALE  NAME     DEFAULT=DUM5

PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1                        DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                        DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
END-PROC
.TITLE
VICAR Program GETPC
.HELP
PURPOSE

GETPC is a VICAR application program which computes the line-sample
coordinates of the target-center from SPICE kernels and outputs it as a TAE
variable.  In addition, it will return SCLAT, SCLON and SCALE as TAE variables
when processing Galileo or Cassini images.

For Cassini and Galileo, the image-space planet center is returned.  For
Voyager, the object-space planet center is returned.

.page
EXECUTION

     GETPC INP=InpFile PCL=PCL PCS=PCS SCLAT=SCLAT SCLON=SCLON SCALE=SCALE

where InpFile is a Cassini, Galileo, or Voyager image.

.page
HISTORY:
 WHEN       WHO WHAT
 ---------- --- ---------------------------------------------------------------
 01/30/2003 GMY Upgraded to support Cassini.  Ported to Linux.
 12/17/1996 SMC Modified to report the correct SCLAT and SCLON.       (FR89983)
 10/08/1996 SMC Modified to adapt CONVEV and CONVISOS calling sequence so that
                 Summation Mode images are supported.                 (FR89818)
 07/23/1996 SMC Initial release (Combined unported GLLPC and unported VGRPC)

.level1
.vari INP
Required input image file name
.VARI PCL
Optional output variable
.VARI PCS
Optional output variable
.VARI SCLAT
Optional output variable for GLL
.VARI SCLON
Optional output variable for GLL
.VARI SCALE
Optional output variable for GLL
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
The input file of interest.  This should be a geometrically corrected image (in
Galileo's case it can be just a raw image).
.VARI PCL
Planet Center Line pixel
.VARI PCS
Planet Center Sample pixel
.VARI SCLAT
SpaceCraft LATtitude
.VARI SCLON
SpaceCraft LONtitude
.VARI SCALE
The scale of the size of the given image pixel as compared to the actual size
of the object.  In an other word, this shows how big of a ground coverage the
pixel represents.
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

  CKNAME        C KERNEL
  --------      -------------
  DAVI          MIPS_DAVI.CK
  NAV           MIPS_NAV.CK
  FARE          MIPS_FARENC.CK
  NAV2          MIPS_NAV2.CK
  NEAR          MIPS_NEAR.CK
  AMOS          MIPS_AMOS.CK
  NAIF          the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID    CKNAME        C KERNEL
  ----    --------      -------------
  M906    DAVI          MIPS_DAVI.CK
  M905    NAV           MIPS_NAV.CK
  M904    FARE          MIPS_FARENC.CK
  M903    NAV2          MIPS_NAV2.CK
  M902    NEAR          MIPS_NEAR.CK
  M901    AMOS          MIPS_AMOS.CK
  varies  NAIF          there are a large number of these files

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
$ create getpc.imake
/* Imake file for GETPC */

#define PROGRAM getpc 
#define R2LIB

#define MODULE_LIST getpc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_SPICE
#define LIB_TAE
#define LIB_MATH77
#define LIB_RTL
#define LIB_P2SUB
#define LIB_NETWORK

/* #define DEBUG         /* Remove upon delivery */
/* #define LIB_LOCAL     /* Remove upon delivery */

/* End of Imake file */
$ Return
$!#############################################################################
$Test_File:
$ create tstgetpc.pdf
! This TEST PDF file has been written for Alpha and Unix Platform
procedure
  RefGbl $echo
  RefGbl $SysChar
body
  Local GLLFile    TYPE=STRING
  Local GLLPh2Full TYPE=STRING  ! Galileo Phase 2 Full Frame
  Local GLLPh2Summ TYPE=STRING  ! Galileo Phase 2 Summation Mode
  Local VGRFile    TYPE=STRING
  Local CASFile    TYPE=STRING

  Local PCL    TYPE=REAL
  Local PCS    TYPE=REAL
  Local SCLAT  TYPE=REAL
  Local SCLON  TYPE=REAL
  Local SCALE  TYPE=REAL

  Let $echo="no"
  If ($SysChar(1)="VAX_VMS")
    Let GLLFile="wms_test_work:[testdata.mipl.gll]venus.img"
    Let GLLPh2Full="WMS_TEST_WORK:[TESTDATA.GLL]s0349632000.u"
    Let GLLPh2Summ="WMS_TEST_WORK:[TESTDATA.GLL]s0349666200.u"
    Let VGRFile="wms_test_work:[testdata.mipl.vgr]f1636832.fic"
    Let CASFile="wms_test_work:[testdata.cassini.cas$i$ss]n1354897340.1"
  Else
    Let GLLFile="/project/test_work/testdata/mipl/gll/venus.img"
    Let GLLPh2Full="/project/test_work/testdata/gll/s0349632000.u"
    Let GLLPh2Summ="/project/test_work/testdata/gll/s0349666200.u"
    Let VGRFile="/project/test_work/testdata/mipl/vgr/f1636832.fic"
    Let CASFile="/project/test_work/testdata/cassini/casIss/n1354897340.1"
  End-If

  write "===Project GLL test"
  Let $echo="yes"
 GETPC &"GLLFile" PCL=PCL PCS=PCS SCLAT=SCLAT SCLON=SCLON SCALE=SCALE 'REMOTE
  Let $echo="no"
  WRITE "===Returned TAE variables are:"
  WRITE "===  PCL   = "&PCL
  WRITE "===  PCS   = "&PCS
  WRITE "===  SCLAT = "&SCLAT
  WRITE "===  SCLON = "&SCLON
  WRITE "===  SCALE = "&SCALE

  WRITE "=== Testing Phase 2 full frame image"
  Let $echo="yes"
 GETPC &"GLLPh2Full" 'REMOTE
  Let $echo="no"
  WRITE "=== Testing Phase 2 summation mode image"
  Let $echo="yes"
 GETPC &"GLLPh2Summ" 'REMOTE
  Let $echo="no"

  write "===================================================================="
  write "===Project VGR test"
  Let $echo="yes"
 GETPC &"VGRFile" PCL=PCL PCS=PCS target=IO
  Let $echo="no"
  WRITE "===Returned TAE variables are:"
  WRITE "===  PCL   = "&PCL
  WRITE "===  PCS   = "&PCS

  write "===================================================================="
  write "===Project CASSI test"
  Let $echo="yes"
 GETPC &"CASFile" PCL=PCL PCS=PCS SCLAT=SCLAT SCLON=SCLON SCALE=SCALE 'REMOTE
  Let $echo="no"
  WRITE "===Returned TAE variables are:"
  WRITE "===  PCL   = "&PCL
  WRITE "===  PCS   = "&PCS
  WRITE "===  SCLAT = "&SCLAT
  WRITE "===  SCLON = "&SCLON
  WRITE "===  SCALE = "&SCALE
end-proc
$ Return
$!#############################################################################
