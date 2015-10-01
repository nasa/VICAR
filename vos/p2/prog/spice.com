$!****************************************************************************
$!
$! Build proc for MIPL module spice
$! VPACK Version 1.8, Tuesday, October 15, 1996, 11:02:17
$!
$! Execute by entering:		$ @spice
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
$ write sys$output "*** module spice ***"
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
$ write sys$output "Invalid argument given to spice.com file -- ", primary
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
$   if F$SEARCH("spice.imake") .nes. ""
$   then
$      vimake spice
$      purge spice.bld
$   else
$      if F$SEARCH("spice.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spice
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spice.bld "STD"
$   else
$      @spice.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spice.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spice.com -
	-s spice.f -
	-p spice.pdf -
	-i spice.imake -
	-t tstspice.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spice.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'
      subroutine main44
      implicit integer*4 (a-z)
      character*5 project
      character*4 destination
      character*12 planet,labplanet
      character*36 picname
      character*20 dcfile,radfile,blemfile,shfile
      character*8 intape,outtape
      character*4 CKNAME,instrument,csource
      integer*4 data(80),buf(200),count,def
      real*8 rbuf(100)
      real*4 restab(2720),pldata(20),rdata(80)
      real*4 is_line,is_samp,os_line,os_samp
      logical xvptst
      equivalence (buf,rbuf),(instrument,buf(2))
      equivalence (csource,buf(11)),(data,rdata)
      equivalence (labplanet,data(25)),(picname,data(31))
      equivalence (dcfile,data(40)),(radfile,data(45))
      equivalence (blemfile,data(50)),(shfile,data(55))
      equivalence (intape,data(60)),(outtape,data(63))

      call init_spice
      call xvunit(unit,'INP',1,status, ' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call qprint('  ')

      call qprint('GETPROJ:')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call qprint(project)
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')
      call qprint('  ')

      call qprint('GETCAMCON:')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call qprint('  ')
           
      call qprint('GETLABCON:')
      call getlabcon(unit,project,data,ind)      
      if(ind.eq.1) call prnt(4,1,ind,'warning indicator=.')
      if(ind.gt.1) call prnt(4,1,ind,'fatal indicator=.')
      if(data(1).eq.0) then
         call qprint('invalid label type')
      else if(data(1).eq.1) then
         call qprint('ground calibration label')
      else if(data(1).eq.2) then
         call qprint('flight label')
      else
         call prnt(4,1,data(1),'data(1)=.')
      endif         
      call prnt(4,1,data(2), 'frame number          .')
      call prnt(7,1,rdata(3), 'exposure time         .')
      call prnt(4,1,data(4), 'filter position       .')
      call prnt(4,1,data(5), 'frame or scan rate    .')
      call prnt(4,1,data(6), 'camera serial number  .')
      call prnt(4,1,data(7), 'gain state            .')
      call prnt(4,1,data(8), 'S/C event time year   .')
      call prnt(4,1,data(9), 'S/C event time day    .')
      call prnt(4,1,data(10),'S/C event time hour   .')
      call prnt(4,1,data(11),'S/C event time minute .')
      call prnt(4,1,data(12),'S/C event time second .')
      call prnt(4,1,data(13),'S/C event time milsec .')
      call prnt(4,1,data(14),'S/C ID                .')
      call prnt(4,1,data(17),'FIBE                  .')
      call prnt(4,1,data(18),'Boom flag             .')
      call prnt(4,1,data(23),'clock                 .')
      call qprint('planet in label is: '//labplanet)
      call prnt(7,1,data(28),'DN to reflectance IOVF.')
      call prnt(7,1,data(29),'DN to radiance    CONV.')
      call prnt(7,1,data(30),'Range target to sun   .')
      call qprint('input tape name '//intape)
      call prnt(4,1,data(62),'Input  file #         .')
      call qprint('output tape name '//outtape)
      call prnt(4,1,data(65),'Output file #         .')
      if (data(31).eq.-999) data(31)=0
      call qprint('picno '//picname)
      if (data(40).eq.-999)  data(40)=0
      call qprint('dark calibration file '//dcfile)
      if (data(45).eq.-999)  data(45)=0
      call qprint('radiance cal file '//radfile)
      if (data(50).eq.-999)  data(50)=0
      call qprint('blemish correction file '//blemfile)
      if (data(55).eq.-999) data(55)=0
      call qprint('shutter offset file '//shfile)
      call prnt(4,1,data(66),'Earth rcvd time year  .')
      call prnt(4,1,data(67),'Earth rcvd time day   .')
      call prnt(4,1,data(68),'Earth rcvd time hour  .')
      call prnt(4,1,data(69),'Earth rcvd time minute.')
      call prnt(4,1,data(70),'Earth rcvd time second.')
      call prnt(4,1,data(71),'Uneven bit weighting  .')
      call prnt(4,1,data(72),'Camera ID (1 or 2)    .')
      call prnt(4,1,data(73),'Partition(#RIM cycles).')
      call qprint('  ')
     
      call qprint('GETSPICE2:')
      if(project.eq.'GLL  ') then
         call qprint('coordinate system is: J2000')
      else
         call qprint('coordinate system is: B1950')
      endif
c     CKNAME='    '
      call xvparm('CKNAME',CKNAME,count,def,0)
      if(CKNAME.eq.'    ')then
         call qprint('SEDR/SPICE CKNAME is defaulted to blanks')
      else
         call qprint('SEDR/SPICE CKNAME set to '//CKNAME)
      endif
      planet='            '
      call getspice2(unit,.true.,buf,ind)
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         call qprint('Central body= '//planet)
      else
         call qprint('Target body= '//planet)
      endif
      if(ind.ne.1) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(4,1,buf(1),  'spacecraft spice ID #=            .')
      call prnt(4,1,buf(13),  'SEDR update DDDYY                .')
      call qprint('Instrument= '//instrument)
      call prnt(4,1,buf(9),  'target body code                  .')
      call prnt(4,1,buf(3),  'measurement time year             .')
      call prnt(4,1,buf(4),  'measurement time day              .')
      call prnt(4,1,buf(5), 'measurement time hour              .')
      call prnt(4,1,buf(6), 'measurement time minute            .')
      call prnt(4,1,buf(7), 'measurement time second            .')
      call prnt(4,1,buf(8), 'measurement time millisecond       .')
      call prnt(4,1,buf(12), 'FDS or SCLK counter               .')
      call prnt(8,3,rbuf(16), 'xyz of s/c rel central body      .')
      call prnt(8,3,rbuf(19), 'xyz of picture body rel s/c      .')
      call prnt(8,1,rbuf(25), 's/c range from sun               .')
      call prnt(8,1,rbuf(26), 's/c range from central body      .')
      call prnt(8,1,rbuf(27), 's/c range from picture body      .')
      call prnt(8,2,rbuf(28), 'lat & lon of sun rel picture body.')
      call prnt(8,2,rbuf(30), 'lat & lon of s/c rel picture body.')
      call prnt(8,9,rbuf(41), 'C-matrix                         .')
      call prnt(8,2,rbuf(77), 'lat & lon of P5 point            .')
      call prnt(8,1,rbuf(79), 'incidence angle at P5 point      .')
      call prnt(8,1,rbuf(80), 'emission  angle at P5 point      .')
      call prnt(8,1,rbuf(81), 'phase     angle at P5 point      .')
      call prnt(8,2,rbuf(82), 'horiz & vert pixel size at P5    .')
      call prnt(8,9,rbuf(50), 'ME-matrix                        .')
      call prnt(8,1,rbuf(84),'s/c range to P5                   .')
      call prnt(8,1,rbuf(68),'north angle                       .')
      call qprint('SEDR CKNAME= '//csource)
      call prnt(8,1,rbuf(13),'picture body equat radius, long   .')
      call prnt(8,1,rbuf(14),'picture body equat radius, short  .')
      call prnt(8,1,rbuf(15),'picture body polar radius         .')
      call prnt(8,9,rbuf(59),'OM-matrix                         .')
      call prnt(8,3,rbuf(22),'RS-vector                         .')
      call prnt(8,1,rbuf(69),'line   of sub-s/c point           .')
      call prnt(8,1,rbuf(70),'sample of sub-s/c point           .')
      call qprint('  ')

      call qprint('GETPLACON:')
      idnum=buf(9)
      planet='            '
      call getplacon(planet,idnum,pldata,ind)
      if(ind.eq.1) call qprint('unrecognizable planet name')
      if(ind.eq.2) call qprint('unrecognizable planet id#')
      call qprint('Target planet body is:'//planet)
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call qprint('   ')      


      call qprint('GETGEOM:')
      call xvpcnt('INP',count)
      if(count.gt.1)then
         call xvunit(unit2,'INP',2,status, ' ')
         geomsor=1
         call qprint('obtaining geom parameters from input file')
      else
         geomsor=0
         call qprint('obtaining geom parameters from nominals')
      endif
      call getgeom(unit2,project,camera,geomsor,
     +             restab,restab,
     +             nah,nav,ind)
      if(ind.ne.0) call qprint('GETGEOM: error, ind=1')
      call qprint('    ')

      call qprint('CONVISOS:')
      call qprint('assume image space l,s=400.,400.')
      is_line=400.
      is_samp=400.
      nph=nah+1
      npv=nav+1
      mode=1
      call convisos(project,camera,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call qprint('CONVISOS: error, ind=1')
      call prnt(7,1,os_line,'object space line=.')
      call prnt(7,1,os_samp,'object space samp=.')
      mode=0
      call convisos(project,camera,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call qprint('CONVISOS: error, ind=1')
      call prnt(7,1,is_line,'image space line=.')
      call prnt(7,1,is_samp,'image space samp=.')
      call qprint('   ')

      if(xvptst('UPDATE'))then
        call qprint('PUTSPICE2:')
        call xvparm('DESTINAT',destination,count,def,0) ! loads buf(11)
        if(csource.eq.'    ') then
           call qprint('must specify SPICE destination keyword')
           call abend
        endif
        call putspice2(destination,'SPICE',buf,ind)
        if(ind.ne.1) call prnt(4,1,ind,'PUTSPICE: fatal indicator.')
        if(ind.eq.1) call prnt(4,1,ind,'PUTSPICE: successful run.')
      endif

      return
      end


      subroutine qprint (message)
      character *(*) message

      call xvmessage ( message , ' ')
      return
      end













































$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create spice.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=(1:2)
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
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
PARM UPDATE       TYPE=KEYWORD VALID=(UPDATE,NOUPDATE) +
  DEFAULT=NOUPDATE
PARM DESTINAT TYPE=(STRING,5) COUNT=(0:1) VALID=("DAVI ","NAV  ", +
  "FARE ","NAV2 ","NEAR ","SEDR ","NAIF ","AMOS ") DEFAULT=--
END-PROC

.TITLE
VICAR2 program SPICE

.HELP
SPICE lists all the information available on an image from any
flight project. This includes the SPICE/SEDR and the image labels.
The program also acts as a test for the following subroutines.
The output of each will be listed under that routines heading:
GETPLACON
GETCAMCON
GETSPICE
GETPROJ
GETGEOM
GETLABCON
CONVISOS

PARAMETERS:

SPICEMODE specifies whether local or remote SPICE access is to be used.
If defaulted, SPICEMODE is set to the value of the logical name (or
environmental variable) DEFAULTSPICE.


PARAMETERS FOR RETRIEVING THE INITIAL CAMERA POINTING:

Initial camera pointing data is first retrieved from predict C kernels or
from MIPS C kernels.  The following optional parameters permit the user to
specify where this initial pointing is retrieved:

CKNAME and CKID are alternative ways to specify the C kernel from which camera
pointing is to be retrieved.  For example, CKNAME=FARE or CKID=M904 specifies
that the camera pointing should be retrieved from the file MIPS_FARENC.CK.
The CKID is the unique kernel ID assigned to each C kernel.  When CKID is
specified, it overrides the CKNAME parameter.  A complete list
of kernel IDs is located in the ASCII file assigned the logical name (or
environmental variable) KERNELDB.


PARAMETERS FOR STORING THE IMPROVED CAMERA POINTING:

The following optional parameters are used to store provenance information along
with the improved (C-Smithed) camera pointing computed by the program.  This
provenance information is stored in the (C kernel) segment identifier for the
camera pointing data.  In cases where there are several versions of camera
pointing for an image in a given C kernel, this provenance information can
later be used to retrieve a specific instance of camera pointing from the
kernel.

PURPOSE identifies the purpose for creating the camera pointing.
REQNUM identifies the request number associated with the camera pointing.
CDATE specifies the date and time the camera pointing was created.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

See the level 2 help (via the TAE tutor mode) for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.
          Improved camera pointing will later be stored in the local C kernel
          specific to this program.

           'REMOTE INSTITUTE=DLR USERID=TYR retrieves SPICE data remotely
          (from MIPS) via the SPICE server.  When improved camera pointing is
          stored (at MIPS), provenance data regarding the facility (DLR) and
          user (Thomas Roatsch) who created the data is included.
 

EXAMPLE:
      spice inp=a.img
.page
PROGRAM HISTORY:
Written By: J Lorre			1/9/89
Cognizant Programmer: J Lorre
REVISIONS:
  25 Jun GMY  Adde call to init_spice (70964)
  29 Jul OAM  Modified to call getspice2 instead of getspice.
              Included provenance parameters.DFR.
  15 Oct OAM  Included the CAMERA parameter to CONVISOS calls.
              FR 89818. 
.LEVEL1
.VARI INP
1.The image to analyze
2.Geom file

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

.VARI UPDATE
To update the SEDR/SPICE
with subroutine PUTSPICE

.VARI DESTINATION
The name to be associate 
with the SEDR/SPICE


.LEVEL2
.VARI INP
There are up to 2 input files:
1. The image whose label and spice/sedr to list.
2. The geom correction file for the first input.
   The second file is optional. If not included,
   nominals will be generated.

.VARI UPDATE
To update the SEDR or SPICE
Specifies whether subroutine PUTSPICE is to be called.
The default (NOUPDATE) is to NOT call PUTSPICE.
Note that the sedr/spice will be updated with the
source specified by the DESTINATION keyword .

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

.VARI DESTINAT
Destination is the name to be associated with the
SEDR/SPICE as a source when it is updated. Only required if
the UPDATE keyword is specified.

.end

$ Return
$!#############################################################################
$Imake_File:
$ create spice.imake
#define PROGRAM spice

#define MODULE_LIST spice.f  
                            
#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
#define LIB_SPICE
#define LIB_NETWORK

$ Return
$!#############################################################################
$Test_File:
$ create tstspice.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
local path1 type=string init="wms_test_work:[testdata.mipl.vgr]"
local path2 type=string init="wms_test_work:[testdata.mipl.gll]"
local path3 type=string init="wms_gll:[ssi.rad]"
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! TEST SCRIPT FOR THE PROGRAM SPICE
!
!check for a UNIX box
!
if ($syschar(1) = "UNIX")
!
    let path1="/project/it/testdata/mipl/vgr/"
    let path2="/project/it/testdata/mipl/gll/"
    let path3="/project/gll/ssi/rad/"
end-if
!
!-----Just Local processing. Remote mode is not
!-----supported for VGR data yet. 7/29/96 OAM.
!
WRITE "test for VGR without geom correction file"
spice inp=&"path1"f1636832.fic 
!
WRITE "test for VGR with geom correction file"
spice inp=(&"path1"f1636832.fic, + 
           &"path1"f1636832.gpribis) 
!
!
WRITE "test for GLL Venus, saving the sedr as NEAR"
spice inp=&"path2"venus.img ckname=naif destinat=near +
      spicemode=remote 'update 
!
WRITE "test for GLL Venus, retrieving the NEAR sedr"
spice inp=&"path2"venus.img ckname=near spicemode=remote
!
WRITE "test for Summation Mode"
spice inp=&"path3"1s15.dc ckname=near spicemode=remote
!
end-proc



$ Return
$!#############################################################################
