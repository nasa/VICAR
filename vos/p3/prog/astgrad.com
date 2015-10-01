$!****************************************************************************
$!
$! Build proc for MIPL module astgrad
$! VPACK Version 1.8, Thursday, July 06, 2000, 17:16:44
$!
$! Execute by entering:		$ @astgrad
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module astgrad ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to astgrad.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("astgrad.imake") .nes. ""
$   then
$      vimake astgrad
$      purge astgrad.bld
$   else
$      if F$SEARCH("astgrad.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astgrad
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astgrad.bld "STD"
$   else
$      @astgrad.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astgrad.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astgrad.com -
	-p astgrad.pdf -
	-i astgrad.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create astgrad.pdf
procedure help=*
PARM INP     TYPE=(STRING,60)
PARM OUT     TYPE=(STRING,60)
PARM SIZE    TYPE=INTEGER    COUNT=4                      DEFAULT=(1,1,0,0)
PARM SL	     TYPE=INTEGER   				  DEFAULT=1
PARM SS	     TYPE=INTEGER				  DEFAULT=1
PARM NL      TYPE=INTEGER				  DEFAULT=0
PARM NS      TYPE=INTEGER				  DEFAULT=0
PARM TRANS   TYPE=REAL       COUNT=5
PARM PATHRAD TYPE=REAL       COUNT=5
LOCAL TR     TYPE=REAL
LOCAL PATH   TYPE=REAL

BODY

let TR = TRANS(1)
let PATH = PATHRAD(1)
COPY @INP SCRASTGRADX @SIZE @SL @SS @NL @NS SB=1 NB=1
F2 SCRASTGRADX SCRASTGRAD10 FUNC="(IN1-&PATH)/&TR"

let TR = TRANS(2)
let PATH = PATHRAD(2)
COPY @INP SCRASTGRADX @SIZE @SL @SS @NL @NS SB=2 NB=1
F2 SCRASTGRADX SCRASTGRAD11 FUNC="(IN1-&PATH)/&TR"

let TR = TRANS(3)
let PATH = PATHRAD(3)
COPY @INP SCRASTGRADX @SIZE @SL @SS @NL @NS SB=3 NB=1
F2 SCRASTGRADX SCRASTGRAD12 FUNC="(IN1-&PATH)/&TR"

let TR = TRANS(4)
let PATH = PATHRAD(4)
COPY @INP SCRASTGRADX @SIZE @SL @SS @NL @NS SB=4 NB=1
F2 SCRASTGRADX SCRASTGRAD13 FUNC="(IN1-&PATH)/&TR"

let TR = TRANS(5)
let PATH = PATHRAD(5)
COPY @INP SCRASTGRADX @SIZE @SL @SS @NL @NS SB=5 NB=1
F2 SCRASTGRADX SCRASTGRAD14 FUNC="(IN1-&PATH)/&TR"

TRAN (SCRASTGRAD10,SCRASTGRAD11,SCRASTGRAD12,SCRASTGRAD13,SCRASTGRAD14) +
   @OUT 'BSQ

ush rm -f SCRASTGRAD*
END-PROC
.TITLE
	Procedure ASTGRAD
.HELP
PURPOSE:
ASTGRAD takes as input the 5 ASTER TIR channels of Level 1B data, in units of
milliWatts per square meter per steradian per micrometer and performs an
atmospheric correction to yield upwelling radiance at surface, in the same
units.  The user is required to provide the path transmittance and path
radiance for each channel.

OPERATION:
ASTGRAD uses the VICAR programs COPY an F2 to copy out each ASTER TIR channel
and apply the following formula:

     Radiance(ground) = [Radiance(sensor) - Pathrad] / Transmittance

Once all 5 channels have been processed, the results are reassembled into a
single file by the VICAR program TRAN.  The output file will have BSQ
organization.
.LEVEL1
.VARIABLE INP
input data set
.VARIABLE OUT
output data set
.VARIABLE SIZE
The standard Vicar size
 field (sl,ss,nl,ns)
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE TRANS
Path transmittance
.VARIABLE PATHRAD
Path radiance
(mW/m^2/sr/um)
.LEVEL2
.VARIABLE INP
Input dataset containing ASTER 1B TIR radiance at sensor data, in units of
milliWatts per square meter per steradian per micrometer.
.VARIABLE OUT
Output dataset containing ASTER 1B TIR upwelling radiance at surface data,
in units of milliWatts per square meter per steradian per micrometer.
.VARIABLE SIZE
The standard Vicar size field ( starting_line, starting_sample, 
number_of_lines, number_of_samples).
.VARIABLE TRANS
The values of TRANS should be the tranmittance from the surface to top of
atmosphere.  5 values must be provided, one for each ASTER TIR channel.
.VARIABLE PATHRAD
The values of PATHRAD are the amounts of path emitted radiance observed at
the top of atmosphere, when viewing nadir.  The required units are 
milliWatts per square meter per steradian per micrometer.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create astgrad.imake
#define  PROCEDURE astgrad

#define R2LIB 
$ Return
$!#############################################################################
