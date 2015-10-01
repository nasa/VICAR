$!****************************************************************************
$!
$! Build proc for MIPL module gain_fix
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:07
$!
$! Execute by entering:		$ @gain_fix
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
$ write sys$output "*** module gain_fix ***"
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
$   if F$SEARCH("gain_fix.imake") .nes. ""
$   then
$      vimake gain_fix
$      purge gain_fix.bld
$   else
$      if F$SEARCH("gain_fix.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gain_fix
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gain_fix.bld "STD"
$   else
$      @gain_fix.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gain_fix.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gain_fix.com -
	-p gain_fix.pdf -
	-i gain_fix.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create gain_fix.pdf
procedure      help=*

parm aux_in  (string,15)
parm aux_out (string,15)
parm gain real default=1.0
local format type=keyword
local nl type=integer
local ns type=integer
local nb type=integer

body
form &aux_in format nl ns nb
if (ns <> 16)
    if (ns <> 33) 
       write "!! NOT AN AUX FILE !!"
       goto adios
    end-if
end-if
c &aux_in a.img size=(1 13 &nl 2) so=(&gain 0.0) 'real
insect (&aux_in a.img) &aux_out insect=(1 1 &nl 2 1 13)
ush \rm a.img
adios>continue
end-proc

.title
vicar2 procedure gain_fix

.level1
.variable aux_in
name of input aux file
.varible aux_out
name for output aux file
.variable gain
gain set by TIMS operator
$ Return
$!#############################################################################
$Imake_File:
$ create gain_fix.imake
#define  PROCEDURE gain_fix

#define R2LIB 
$ Return
$!#############################################################################
