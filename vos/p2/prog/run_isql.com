$!****************************************************************************
$!
$! Build proc for MIPL module run_isql
$! VPACK Version 1.9, Monday, October 02, 2000, 16:08:03
$!
$! Execute by entering:		$ @run_isql
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module run_isql ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to run_isql.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("run_isql.imake") .nes. ""
$   then
$      vimake run_isql
$      purge run_isql.bld
$   else
$      if F$SEARCH("run_isql.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake run_isql
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @run_isql.bld "STD"
$   else
$      @run_isql.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create run_isql.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack run_isql.com -
	-p run_isql.pdf -
	-i run_isql.imake -
	-t tstrun_isql.pdf tstrun_isql.isql
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create run_isql.pdf
procedure help=*
Refgbl $echo
refgbl $syschar
parm user        (string,30) 
parm pass        (string,30) 
parm server      (string,30)  default="miplDev"
parm sy_filename (string,250)
parm database    (string,30)  default="devCat"
body
let _onfail="continue"
if ($syschar(1) = "UNIX")
! We're using UNIX
ush dbq -u'&user' -s'&server' -p'&pass' -d'&database' -c&sy_filename
else
! We're using VMS
dcl dbq -u"&user" -s"&server" -p"&pass" -d"&database" -c"&sy_filename"
end-if

!# annot function="VICAR Utilities"
!# annot keywords=(script,Sybase,database,ush,dcl,ISQL)
end-proc
.title
Enter or delete data in Sybase catalog
.help
 Purpose:

 The reason for this script is for use in test pdfs of programs that use
 Sybase calls.  This is to allow manipulation of the database.  The
 reason for not using a ush or dcl call directly from the test pdf itself
 is that in UNIX, all ush commands are executed before any of the rest of
 the pdf.  This often results in cleanup portions at the end of the script
 being executed prematurely, erasing all data before it is processed.

Example:
 run_isql user=&user pass=&pass server=&server sy_filename=tstcatproducts.list_oview1

 run-isql use to execute the program isql, now it runs the program dbq.

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 WARNING:  For UNIX only, special characters (i.e. $, etc.) need to be 
           preceeded with a backslash(\) when passed into this
           program.

      	   For example,  an account having username 'norm' and password 'no$rm'
	   would be passed in as follows:

 	   run_isql user=norm pass=no\$rm server=MIPS1 sy_filename=NORM.sql

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

History:
 06-09-00  HBM  Replaced isql call with dbq.

 05-20-96  TLT  Removed db parameter because isql does not have one.
		Added single quotes to string values for the call from unix.
		Added WARNING note about special characters in the help.

.level1
.var USER
Sybase user name (see WARNING in main help)
.var PASS
Sybase password (see WARNING in main help)
.var SERVER
Sybase server
.var SY_FILENAME
Name of Sybase ISQL script file
.var DATABASE
Sybase database.
$ Return
$!#############################################################################
$Imake_File:
$ create run_isql.imake
#define PROCEDURE run_isql
$ Return
$!#############################################################################
$Test_File:
$ create tstrun_isql.pdf
procedure
Refgbl $echo
refgbl $syschar
parm user   (string,30) 
parm pass   (string,30) 
parm server (string,30) default="MIPSDB1"
parm database (string,30) default="devCat"
body
let $echo="yes"
let _onfail="continue"
run_isql user=&user pass=&pass server=&server sy_filename=tstrun_isql.isql +
  database=&database
end-proc
$!-----------------------------------------------------------------------------
$ create tstrun_isql.isql
select count (*) from ssiraw
go
select "This is only a test"
go
$ Return
$!#############################################################################
