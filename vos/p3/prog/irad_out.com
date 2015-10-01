$!****************************************************************************
$!
$! Build proc for MIPL module irad_out
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:07
$!
$! Execute by entering:		$ @irad_out
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
$ write sys$output "*** module irad_out ***"
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
$   if F$SEARCH("irad_out.imake") .nes. ""
$   then
$      vimake irad_out
$      purge irad_out.bld
$   else
$      if F$SEARCH("irad_out.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake irad_out
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @irad_out.bld "STD"
$   else
$      @irad_out.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create irad_out.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack irad_out.com -
	-p irad_out.pdf -
	-i irad_out.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create irad_out.pdf
PROCEDURE      HELP=*

PARM NAME (STRING,30)
PARM DATE    INTEGER DEFAULT=-1
LOCAL INPIX (STRING,30)
LOCAL INAUX (STRING,30)
LOCAL BND1 (STRING,30)
LOCAL BND2 (STRING,30)
LOCAL BND3 (STRING,30)
LOCAL BND4 (STRING,30)
LOCAL BND5 (STRING,30)
LOCAL BND6 (STRING,30)

BODY
LET INPIX = NAME // ".pix"
LET INAUX = NAME // ".aux"
LET BND1 = NAME // ".b1"
LET BND2 = NAME // ".b2"
LET BND3 = NAME // ".b3"
LET BND4 = NAME // ".b4"
LET BND5 = NAME // ".b5"
LET BND6 = NAME // ".b6"

TIMSCAL (&INPIX,&INAUX) IRAD 'IRAD 'WATT DATE=&DATE
TRAN IRAD (&BND1,&BND2,&BND3,&BND4,&BND5,&BND6) +
     BANDS=(1,2,3,4,5,6) 'BSQ
ush \rm IRAD 
END-PROC

.TITLE
VICAR2 Procedure IRAD_OUT

.HELP
     IRAD_OUT is a procedure designed to reduce TIMS DN values to
radiance units, using the internal calibration targets as references. 
The resulting .IRAD file is split into six single-channel files to be
written to tape.

Cognizant Programmer: Vince Realmuto                   9 OCT 1990

.LEVEL1
.VARIABLE NAME
name of input PIX file,
excluding the ".PIX",
i.e. MONO3 for MONO3.PIX
and MONO3.AUX
.VARIABLE DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.LEVEL2
.VARIABLE NAME
The user must provide the base name for the datasets used. For example,
to process "HAWAII32.PIX" and "HAWAII32.AUX" the name "HAWAII32" must be
given.  This procedure will then generate output datasets with the names
"HAWAII32.IRAD", "HAWAII32.DS1", "HAWAII32.DS3", "HAWAII32.DS5", 
"HAWAII32.RED", "HAWAII32.GRN", and/or "HAWAII32.BLU" as needed.
.VARIABLE DATE
TIMSCAL uses the date of data acquisition to determine the proper
calibration coefficients.  If defaulted, the date in the VICAR 
label is used. This parameter is needed only if the VICAR label
is incorrect, or if an abnormal calibration set is to be used.
$ Return
$!#############################################################################
$Imake_File:
$ create irad_out.imake
#define  PROCEDURE irad_out

#define R2LIB 
$ Return
$!#############################################################################
