$!****************************************************************************
$!
$! Build proc for MIPL module demlog
$! VPACK Version 1.8, Friday, April 04, 1997, 08:12:46
$!
$! Execute by entering:		$ @demlog
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
$ write sys$output "*** module demlog ***"
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
$ write sys$output "Invalid argument given to demlog.com file -- ", primary
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
$   if F$SEARCH("demlog.imake") .nes. ""
$   then
$      vimake demlog
$      purge demlog.bld
$   else
$      if F$SEARCH("demlog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake demlog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @demlog.bld "STD"
$   else
$      @demlog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create demlog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack demlog.com -
	-p demlog.pdf -
	-i demlog.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create demlog.pdf
procedure help=*
PARM INP     TYPE=STRING     
PARM OUT     TYPE=STRING     
PARM BLOCK   TYPE=INTEGER     DEFAULT=4
BODY

DEMLOGA @INP DEMSCR230 @BLOCK
FLOT DEMSCR230 @OUT 'COUNTER

END-PROC

.TITLE
	Procedure DEMLOG
.HELP
PURPOSE:
     DEMLOG logs into VICAR format DEM (Digital Elevation Model) files obtained
from the USGS. Specifically, this program will log usgs DEM FILES that are in 
the format described in the USGS National Mapping Division Open-file Report 
86-004 "Standards for Digital Elevation Models".
     This format differs from the ones known to VDEMLOG and DTTLOG; it involves
unlabelled tapes with fixed length blocksize of some multiple of 1024 bytes. 
All data are stored as ASCII characters in 1024 byte logical records.

                            ***UPDATE***
     The program has been modified to handle datasets with carriage return
delimited records.  These files may be identified by their having a
dataset length that is a multiple of 1025, rather than 1024.  To process
this type of file, use the parameter BLOCK=0

     The output of this procedure has been rotated to make North be the 
top of the image.

     Numerous options are documented in the document referenced above that
have not been implemented.  If you have a dataset that this program should
log, but does not, please contact Ron Alley x40751.
.PAGE
EXECUTION:

Examples

DEMLOG  TAPE/4 OUTIMAGE

This command will log the 4th file on the specified drive's tape from
DEM-format into VICAR format. No parameter or SIZE field is allowed.  


DEMLOG TAPE/2 TOPO BLOCK=19

This command will log the second file onthe specified drive's tape. BLOCK=19
implies that there are 19 x 1024 = 19,456 bytes per block on the tape.


WRITTEN BY:  Ron Alley, November, 1986
COGNIZANT PROGRAMMER:  Ron Alley
.LEVEL1
.VARIABLE INP
Tape file 
.VARIABLE OUT
Output file
.VARIABLE BLOCK
Blocking factor on tape
(bytes per record/1024)
.LEVEL2
.VARIABLE INP
The input should be a disk or tape file without a VICAR label.
.VARIABLE BLOCK
The input dataset contains logical records of length 1024 bytes.  When the
input is on tape, the records are typically blocked, with up to 31 logical
records per physical record.  When the input is from tape, the value of BLOCK
should be specified as the bytes per physical record on tape, divided by 1024.
The default value of 1 is usually the appropriate value for disk input files.
.END
##########################################
     DEMLOGA is not normally run directly, but is instead called by the 
procedure DEMLOG. Running DEMLOGA outside the procedure DEMLOG yields an image
with west at the top, rather than north.
     Only a single file was available for testing this program, so the 
correctness of this code could not be checked for many of the possible data 
forms. If programming errors exist, they will most likely be in the values
placed in the VICAR label, rather than in the image itself. Examine the VICAR
label carefully before proceeding. Report all problems to Ron Alley, x40751.
.PAGE
EXECUTION:

Example

DEMLOGA  TAPE/4 OUTIMAGE

This command will log the 4th file on the specified drive's tape from
DEM-format into VICAR format. No parameter or SIZE field is allowed.  


DEMLOG TAPE/2 TOPO BLOCK=19

This command will log the second file onthe specified drive's tape. BLOCK=19
implies that there are 19 x 1024 = 19,456 bytes per block on the tape.

WRITTEN BY:  Ron Alley, November, 1986
LATEST REVISION: April, 1997
COGNIZANT PROGRAMMER:  Ron Alley
.LEVEL1
.VARIABLE INP
Input unlabelled file
.VARIABLE OUT
Output file
.VARIABLE BLOCK
Blocking factor on tape
(bytes per record/1024)
For <CR> delimited disk
files, use BLOCK=0
.END
$ Return
$!#############################################################################
$Imake_File:
$ create demlog.imake
#define  PROCEDURE   demlog

#define R3LIB 

$ Return
$!#############################################################################
