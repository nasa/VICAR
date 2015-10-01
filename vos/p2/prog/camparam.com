$!****************************************************************************
$!
$! Build proc for MIPL module camparam
$! VPACK Version 1.9, Thursday, May 25, 2000, 16:48:34
$!
$! Execute by entering:		$ @camparam
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
$ write sys$output "*** module camparam ***"
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
$ write sys$output "Invalid argument given to camparam.com file -- ", primary
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
$   if F$SEARCH("camparam.imake") .nes. ""
$   then
$      vimake camparam
$      purge camparam.bld
$   else
$      if F$SEARCH("camparam.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake camparam
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @camparam.bld "STD"
$   else
$      @camparam.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create camparam.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack camparam.com -
	-s camparam.f -
	-i camparam.imake -
	-p camparam.pdf -
	-t tstcamparam.pdf tstcamparam_old.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create camparam.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'

	subroutine main44

c       Revision History:
c       02 Jan 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
c------ program CAMPARAM 
    
c------ Program CAMPARAM will fill the LOCAL variables;
c------ "sc", "scan", "camera", "filter", "fds" and "exprng"
c------ and return the variables to the calling proc.  It will
c------ acquire the data via an able77 call on the VGR label.

	character*2 cam(2)
	character*4 irange,ISECCHR
	integer*4   able(19),parb(500)

	character*80 MSG
	character*4  ISECSTR
	character*3  flt(2,8)
        character*1  virgule
        integer      ist, unit, ind, isec, nc
        real         exp

        DATA FLT   / 'CHJ','CLR','BLU','VIO',
     1   	     'CLR','BLU','VIO','ORG',
     2      	     'NAD','CLR','GRN','GRN',
     3    	     'CH4','GRN','ORG','UV '/
C
	data cam     /'NA','WA'/
	data irange  /'    '/
        data isecchr /'    '/
        data isecstr /'    '/
        data virgule /'/'/
        msg = ' '
        call zia (able,19)
        call zia (parb,500)

        call ifmessage ('CAMPARAM version 02-Jan-95')

	call xvunit(unit,'INP',1,ist,' ')
	call xvopen(unit,ist,' ')
	able(1) = 19
	call able77v2(ind,unit,able)
c
        call mve (4,1,able(3),exp,1,1)

c-------calculate the exposure range
	NC = 4

	if (exp .lt. 23000.) then
		irange = 'A'
	else if(exp .lt. 62000.) then
		irange = 'B'
	else
                irange = 'C'
		ISEC = IFIX(EXP/1000.)
		IF (ISEC .LT. 100) then
                   NC = 2
                   write (ISECSTR(1:2),'(I2)') ISEC
                else IF (ISEC .LT. 1000)  then
                   NC = 3
                   write (ISECSTR(1:3),'(I3)') ISEC
                else 
                   write (ISECSTR(1:4),'(I4)') ISEC
		end if
                write (ISECCHR(1:4),'(A4)') ISECSTR(1:4)
      end if
      call xvmessage(' ',' ')
      call xvmessage('From the Voyager Label:',' ')

      call xvmessage
     &('   FDS    SC   CAMERA    FILTER    EXP(SEC)/RANGE  SCAN',' ')
      MSG = ' '
      WRITE (MSG(2:8),  '(I7)') ABLE(2)
      WRITE (MSG(12:12),'(I1)') ABLE(19)
      WRITE (MSG(36:43),'(F8.3)') EXP/1000.
      WRITE (MSG(53:54),'(I2)') ABLE(5)

      write (msg(18:19),'(A2)') cam(able(7)) 
      write (msg(27:29),'(A3)') flt(3-able(7),able(4)+1)
      write (msg(44:44),'(A1)') virgule
      IF (IRANGE .NE. 'C') THEN
                write (msg(45:48),'(A4)') irange
      ELSE
                write (msg(45:48),'(A4)') ISECCHR
      END IF
      call xvmessage (msg,' ')
c
      call xqini(parb,500,xabort)
      call xqintg(parb,'SC',1,able(19),xadd,ist)
      call xqintg(parb,'SCAN',1,able(5),xadd,ist)
      call xqstr(parb,'CAMERA',1,cam(able(7)),xadd,ist)
      call xqstr(parb,'FILTER',1,flt(3-able(7),able(4)+1),xadd,ist)
      call xqintg(parb,'FDS',1,able(2),xadd,ist)
      IF (IRANGE .NE. 'C') THEN
		call xqstr(parb,'EXPRNG',1,irange,xadd,ist)
      ELSE
		call xqstr(parb,'EXPRNG',1,ISECCHR,xadd,ist)
      END IF
      call xvqout(parb,ist)
      call xvclose(unit,ist,' ')
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create camparam.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM camparam
   To Create the build file give the command:
		$ vimake camparam			(VMS)
   or
		% vimake camparam			(Unix)
************************************************************************/
#define PROGRAM	camparam
#define R2LIB
#define MODULE_LIST camparam.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create camparam.pdf
process help=*
LOCAL DUMMY TYPE=INTEGER
LOCAL DMY TYPE=STRING
PARM INP TYPE=STRING
PARM SC TYPE=NAME DEFAULT=DUMMY
PARM SCAN TYPE=NAME DEFAULT=DUMMY
PARM CAMERA TYPE=NAME DEFAULT=DMY
PARM FILTER TYPE=NAME DEFAULT=DMY
PARM FDS TYPE=NAME DEFAULT=DUMMY
PARM EXPRNG TYPE=NAME DEFAULT=DMY
END-PROC
.TITLE
Program CAMPARAM
.help
CAMPARAM is a utility program that reads information from the label of a
Voyager image and returns it to a TAE procedure.  The id values returned for
Camera, Scan rate, Spacecraft, and Filter are designed to be compatible with
the naming convention of the Voyager calibration files (FICOR files).  In 
this way, the complete file name of the appropriate calibration file may be
constructed from the values output from CAMPARAM.
 
Its use is completely defined by the following examples:
 
	PROCEDURE		!getting camera,scan,sc,filter,fds
	BODY
	LOCAL	CAMERA	TYPE=STRING
	LOCAL	SCAN	TYPE=INTEGER
	LOCAL	SC	TYPE=INTEGER
	LOCAL	FILTER	TYPE=STRING
	LOCAL   FDS     TYPE=INTEGER
	LOCAL   EXPRNG  TYPE=STRING
	CAMPARAM inp SC SCAN CAMERA FILTER FDS EXPRNG
 
	 <rest of procedure>
 
	END-PROC


	PROCEDURE		!getting camera,scan,sc,filter
	BODY
	LOCAL	CAMERA	TYPE=STRING
	LOCAL	SCAN	TYPE=INTEGER
	LOCAL	SC	TYPE=INTEGER
	LOCAL	FILTER	TYPE=STRING
	CAMPARAM inp SC SCAN CAMERA FILTER
 
	 <rest of procedure>
 
	END-PROC
 
Note that all the parameters are output parameters.

.page
PROGRAM HISTORY:

Written by: Charles Avis
Cognizant programmer: Charles Avis
Revisions:
    Made portable for UNIX ... J. Turner (CRI) 02-Jan-95
    22 May 2000 AXC     Modified and updated tstcamparam.pdf so test data
                        can be handled by both VMS and UNIX system (AR-104183).

.level1 
.vari INP 
Input image filename.
.vari SC 
Spacecraft number.
.vari SCAN 
Scan rate.
.vari CAMERA
Camera (WA or NA).
.vari FILTER 
Filter name.
.vari FDS
FDS count of image
.vari EXPRNG
Exposure range of image
.level2
.vari INP 
Input image filename.
.vari SC 
Spacecraft number:
   1 = VGR-1     2 = VGR-2
.vari SCAN 
Scan rate of the input image: 1, 2, 3, 5, 10
.vari CAMERA
Camera indentifier: 
   WA = Wide angle   NA = Narrow angle
.vari FILTER 
Filter name of image:
   VIO, CLR, BLU, ORG, NAD, GRN, CH4, CHJ, UV
.vari FDS
FDS count of image, no decimal point.  That is, mod60 + 100*mod16 .
.vari EXPRNG
Exposure range of image.  Valid values are A and B, corresponding to
NORMAL, EXTENDED.  In addition, for LONG exposures,
the exposure time truncated to seconds is output.

	NORMAL     0.000 <  exposure time < 23.000 sec value returned = A
	EXTENDED  23.000 <= exposure time < 62.000 sec value returned = B
	LONG      62.000 <= exposure time              value returned = exptime
$ Return
$!#############################################################################
$Test_File:
$ create tstcamparam.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar

body
let $autousage="none"
let _onfail="continue"

!! parameter values returned from Voyager image label
LOCAL	CAMERA	TYPE=STRING
LOCAL	SCAN	TYPE=INTEGER
LOCAL	SC	TYPE=INTEGER
LOCAL	FILTER	TYPE=STRING
LOCAL   FDS     TYPE=INTEGER
LOCAL   EXPRNG  TYPE=STRING

!! set up test file directory 
LOCAL   PATH    TYPE=STRING  INIT="/project/test_work/testdata/mipl/vgr/"
if ($syschar(1) = "VAX_VMS")
let PATH="wms_test_work:[testdata.mipl.vgr]"
end-if

CAMPARAM &"path"f1636832.geo SC SCAN CAMERA FILTER
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write " "
write "check these values against what is in the image label"
label-l &"path"f1636832.geo
write " "
write "now get the FDS also, which should also correspond with label listing"
CAMPARAM &"path"f1636832.geo SC SCAN CAMERA FILTER FDS
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write "fds count = &fds"
write " "
write " "
write "now get EXPRNG also (should be A for this image)"
CAMPARAM &"path"f1636832.geo SC SCAN CAMERA FILTER FDS EXPRNG
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write "fds count = &fds"
write "exposure range = &exprng"
END-PROC
$!-----------------------------------------------------------------------------
$ create tstcamparam_old.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
LOCAL	CAMERA	TYPE=STRING
LOCAL	SCAN	TYPE=INTEGER
LOCAL	SC	TYPE=INTEGER
LOCAL	FILTER	TYPE=STRING
LOCAL   FDS     TYPE=INTEGER
LOCAL   EXPRNG  TYPE=STRING
let $echo="no"
write " "
write " "
write " The Test Data are handeled for both VMS and UNIX in this PDF."
write " At present (January 1995), in order to run this program, the"
write " following data file MUST be copied to the local directory"
write " where the program resides."
write " "
write " "
write "                OLD          => NEW (VMS or UNIX)"
write " MIPL:[MIPL.VGR]F1636832.GEO    F1636832.GEO"
write " " 
write " " 
let $echo="no"
CAMPARAM F1636832.GEO SC SCAN CAMERA FILTER
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write " "
write "check these values against what is in the image label"
label-l F1636832.GEO
write " "
write "now get the FDS also, which should also correspond with label listing"
CAMPARAM F1636832.GEO SC SCAN CAMERA FILTER FDS
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write "fds count = &fds"
write " "
write " "
write "now get EXPRNG also (should be A for this image)"
CAMPARAM F1636832.GEO SC SCAN CAMERA FILTER FDS EXPRNG
write " "
write "write out the variable values returned"
write "spacecraft number = &sc"
write "scan rate  = &scan"
write "camera (NA or WA)  = &camera"
write "Three character filter name = &filter"
write "fds count = &fds"
write "exposure range = &exprng"
END-PROC
$ Return
$!#############################################################################
