$!****************************************************************************
$!
$! Build proc for MIPL module astirad
$! VPACK Version 1.8, Monday, January 12, 2004, 19:10:05
$!
$! Execute by entering:		$ @astirad
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
$ write sys$output "*** module astirad ***"
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
$ write sys$output "Invalid argument given to astirad.com file -- ", primary
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
$   if F$SEARCH("astirad.imake") .nes. ""
$   then
$      vimake astirad
$      purge astirad.bld
$   else
$      if F$SEARCH("astirad.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astirad
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astirad.bld "STD"
$   else
$      @astirad.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astirad.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astirad.com -
	-p astirad.pdf -
	-i astirad.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create astirad.pdf
procedure help=*
PARM INP     TYPE=(STRING,60)
PARM OUT     TYPE=(STRING,60)
PARM SIZE    TYPE=INTEGER    COUNT=4                      DEFAULT=(1,1,0,0)
PARM SL	     TYPE=INTEGER   				  DEFAULT=1
PARM SS	     TYPE=INTEGER				  DEFAULT=1
PARM NL      TYPE=INTEGER				  DEFAULT=0
PARM NS      TYPE=INTEGER				  DEFAULT=0
BODY

COPY @INP SCRASTIRADX @SIZE @SL @SS @NL @NS SB=1 NB=1
F2 SCRASTIRADX SCRASTIRAD10 FUNC="MAX(0,6.882*(IN1-1))"

COPY @INP SCRASTIRADX @SIZE @SL @SS @NL @NS SB=2 NB=1
F2 SCRASTIRADX SCRASTIRAD11 FUNC="MAX(0,6.780*(IN1-1))"

COPY @INP SCRASTIRADX @SIZE @SL @SS @NL @NS SB=3 NB=1
F2 SCRASTIRADX SCRASTIRAD12 FUNC="MAX(0,6.590*(IN1-1))"

COPY @INP SCRASTIRADX @SIZE @SL @SS @NL @NS SB=4 NB=1
F2 SCRASTIRADX SCRASTIRAD13 FUNC="MAX(0,5.693*(IN1-1))"

COPY @INP SCRASTIRADX @SIZE @SL @SS @NL @NS SB=5 NB=1
F2 SCRASTIRADX SCRASTIRAD14 FUNC="MAX(0,5.225*(IN1-1))"

TRAN (SCRASTIRAD10,SCRASTIRAD11,SCRASTIRAD12,SCRASTIRAD13,SCRASTIRAD14) +
   @OUT 'BSQ

ush rm -f SCRASTIRAD*
END-PROC
.TITLE
	Procedure ASTIRAD
.HELP
PURPOSE:
ASTIRAD takes as input the 5 ASTER TIR channels Level 1B data (radiance at
sensor) and applies the appropriate gains and offsets to convert to 
radiance in units of milliWatts per square meter per steradian per 
micrometer.

OPERATION:
ASTIRAD uses the VICAR programs COPY and F2 to first copy out each individual
channel, then apply that channel's rescaling values to arrive at the proper
units. Once all channels have been converted, the VICAR program TRAN is used
to recombine the channels into a single BSQ organized dataset. Finally,
all scratch datasets are deleted.

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
.LEVEL2
.VARIABLE INP
Input dataset containing raw ASTER 1B TIR radiance at sensor data.
.VARIABLE OUT
Output dataset containing ASTER 1B TIR radiance at sensor data, in units of
milliWatts per square meter per steradian per micrometer.
.VARIABLE SIZE
The standard Vicar size field ( starting_line, starting_sample, 
number_of_lines, number_of_samples).
.END
$ Return
$!#############################################################################
$Imake_File:
$ create astirad.imake
#define  PROCEDURE astirad

#define R2LIB 
$ Return
$!#############################################################################
