$!****************************************************************************
$!
$! Build proc for MIPL module makesrchlist
$! VPACK Version 1.8, Friday, August 01, 1997, 11:35:46
$!
$! Execute by entering:		$ @makesrchlist
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
$ write sys$output "*** module makesrchlist ***"
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
$ write sys$output "Invalid argument given to makesrchlist.com file -- ", primary
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
$   if F$SEARCH("makesrchlist.imake") .nes. ""
$   then
$      vimake makesrchlist
$      purge makesrchlist.bld
$   else
$      if F$SEARCH("makesrchlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake makesrchlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @makesrchlist.bld "STD"
$   else
$      @makesrchlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create makesrchlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack makesrchlist.com -
	-i makesrchlist.imake -
	-p makesrchlist.pdf -
	-t tstmakesrchlist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create makesrchlist.imake
#define PROCEDURE makesrchlist
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create makesrchlist.pdf
procedure help=*
! makesrchlist.pdf - will create a SRCH-format file from a
!                    directory specification
refgbl $syschar
parm DIR	string
parm FILE	string

body
let _onfail="continue"
createfile &FILE
addtofile &FILE "NEXT FILE=00001"
if ($syschar(1) = "UNIX")
  let DIR = "&DIR"//"/"		!inserts trailing '/' after directory
  ush /bin/ls -1 &DIR | sed -e 's/^/`echo &DIR | sed -e 's/\//\\\\\//g'`/' +
	>> &FILE	
  !Inserts directory before filename but first puts a '\' before every '/'
  !in the directory name telling sed to treat '/' as a literal.
else
  dcl dire/nohead/notrail/out=z.zz &DIR
  dcl convert/append/trunc z.zz &FILE	!converts FILE to proper format
  dcl delete z.zz;
end-if
!# annot function="VICAR Procedure Generation"
!# annot keywords=(output,SRCH,NXT,CNT)
end-proc
.title
Outputs a list of all files in a directory in SRCH format

.help
PURPOSE:
	Makesrchlist creates an output file in SRCH format from a
	directory specification.  This output file can then be used 
	with nxt to sequentially access the files in the directory. The
	filenames listed in the output file will include the directory.

	Note:   In UNIX, don't put a trailing '/' after the directory
		because makesrchlist puts it in automatically.

EXECUTION:
	makesrchlist DIR FILE

REVISIONS:
	7-97 ...RRD... Converted to pdf and ported to UNIX.

.level1
.variable DIR
Name and path of directory.
.variable FILE
Output file in SRCH format.

.level2
.variable DIR
The full directory specification that contains the files you wish to access.
All files in the directory will be included in the output file.

Note: In UNIX, don't put a trailing '/' after the directory name.
      Makesrchlist will put that in for you.
.variable FILE
The name of the output file in SRCH format which you wish to create.
This file can then be used as input for nxt to access the files.
.end




$ Return
$!#############################################################################
$Test_File:
$ create tstmakesrchlist.pdf
procedure help=*
refgbl $echo
refgbl $autousage
refgbl $syschar
local (DIR,FILE,FNAM)	string
local (ISTAPE,POS,FNUM)	int

body
let _onfail="continue"
let $autousage="none"
let $echo="no"

if ($syschar(1) = "UNIX")
	let DIR = "/project/test_work/testdata/cassini/iss"
else
	let DIR = "WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]"
end-if
let FILE = "srchlist.tst"
makesrchlist &DIR &FILE
loop
	nxt &FILE FNAM ISTAPE POS FNUM
	if (FNAM = "END_OF_FILE") break
end-loop
createfile srchlist2.tst
if ($syschar(1) = "UNIX")
	ush /bin/ls -1 &DIR >> srchlist2.tst
else
	dcl dire/nohead/notrail/out=srchlist2.tst &DIR
end-if
typetext srchlist2.tst
end-proc

.help
	This is the test for procedure makesrchlist.  It runs
	makesrchlist on a Cassini test directory and then runs
	nxt on the resulting file.  It then lists the contents
	of the directory for comparison.
.end



$ Return
$!#############################################################################
