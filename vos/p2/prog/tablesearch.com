$!****************************************************************************
$!
$! Build proc for MIPL module tablesearch
$! VPACK Version 1.8, Wednesday, September 10, 1997, 11:10:45
$!
$! Execute by entering:		$ @tablesearch
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
$ write sys$output "*** module tablesearch ***"
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
$ write sys$output "Invalid argument given to tablesearch.com file -- ", primary
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
$   if F$SEARCH("tablesearch.imake") .nes. ""
$   then
$      vimake tablesearch
$      purge tablesearch.bld
$   else
$      if F$SEARCH("tablesearch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tablesearch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tablesearch.bld "STD"
$   else
$      @tablesearch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tablesearch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tablesearch.com -
	-i tablesearch.imake -
	-p tablesearch.pdf -
	-t tsttablesearch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tablesearch.imake
#define  PROCEDURE   tablesearch
#define  R2LIB
$ Return
$!#############################################################################
$PDF_File:
$ create tablesearch.pdf
procedure help=*
parm inp        type=string  count=1
parm out        type=string  count=1
parm size	type=integer count=4	default=(0,0,0,0)
parm cl		type=real    count=1	valid=(0.:1.) default=1.
parm cs		type=real    count=1	valid=(0.:1.) default=1.
parm radius	type=real    count=1  	valid=(0.:1.) default=1.

body

local (r2,begl,endl,begs,ends) real
let r2 = radius * radius
let begl = $float(size(1))
let begs = $float(size(2))
let endl = $float(size(1)) + $float(size(3)) - 1.0
let ends = $float(size(2)) + $float(size(4)) - 1.0

!(C5 MOD 1) gets fractional part of CLINBOX
!MIN takes into account wrap-around

if (size(1) <> 0 and size(2) <> 0 and size(3) <> 0 and size(4) <> 0)
   if (radius <> 1.)
      mf inp=&inp function= +
	("C10=MIN(ABS((C5 MOD 1)-&CL),1.0-ABS((C5 MOD 1)-&CL))**2 +
             +MIN(ABS((C6 MOD 1)-&CS),1.0-ABS((C6 MOD 1)-&CS))**2")
      rowop inp=&inp out=&out keycol=(10) range=(0.0, &r2) 'select
      mf inp=&out function=("C10 = C2 + C5")
      rowop inp=&out out=&out keycol=(10) range=(&begl,&endl) 'select
      mf inp=&out function=("C10 = C3 + C6")
      rowop inp=&out out=&out keycol=(10) range=(&begs,&ends) 'select
   else
      mf inp=&inp function=("C10 = C2 + C5")
      rowop inp=&inp out=&out keycol=(10) range=(&begl,&endl) 'select
      mf inp=&out function=("C10 = C3 + C6")
      rowop inp=&out out=&out keycol=(10) range=(&begs,&ends) 'select
   end-if
else-if (radius <> 1.)
      mf inp=&inp function= +
	("C10=MIN(ABS((C5 MOD 1)-&CL),1.0-ABS((C5 MOD 1)-&CL))**2 +
             +MIN(ABS((C6 MOD 1)-&CS),1.0-ABS((C6 MOD 1)-&CS))**2")
      rowop inp=&inp out=&out keycol=(10) range=(0.0, &r2) 'select
else
   ibis-copy inp=&inp out=&out
end-if

end-proc

.TITLE
VICAR procedure tablesearch

.HELP
PURPOSE:
	A procedure generally used in camera calibration processing to
	search and extract records from an input file.  The input file is
	created by prf (Point Response Function) and records are extracted
	from it using either or both of two methods.  If SIZE is specified
	by the user then the sums of the columns SLBOX + CLINBOX and SSBOX +
	CSINBOX must lie in the rectangle defined by SIZE.  If CL, CS, and
	RADIUS are specified by the user then the point represented by the
	fractional parts of the columns CLINBOX and CSINBOX must lie inside
	the circle defined by (x - CS)^2 + (y - CL)^2 = RADIUS^2.  If the
	circle crosses the square defined by (0,0), (0,1), (1,1) and (1,0)
	then wrap-around is taken into account.

	If either SIZE or RADIUS retain their default values then that
	method of selection is ignored.  Consequently if they both retain
	their default values then all the records in the input file are
	selected and copied to the output file.  If they are both specified
	then selected records must satisfy both conditions.

	Note: As a consequence of using program mf, tablesearch changes the
	value of WORK column 10 in the input file.  This column was set up
	to be used by tablesearch.

EXECUTION:
	tablesearch inp=input.file out=out.file size=(257,257,512,512) + 
		    cl=0.5 cs=0.3 radius=0.2

	The above command line uses both methods to select records from the
	input file and copy them to the output file.

.PAGE

METHOD:
	1) Use tablelist on input.file to list the records.
	2) Next use tablesearch to search and extract records based on
	   either or both of the two methods described above.

HISTORY:
	4-1996    ...YKK... Wrote C version for VAX/VMS.
	8-29-1997 ...RRD... Converted to VICAR procedure.

.LEVEL1
.VARI INP
Input file
.VARI OUT
Output file
.VARI SIZE
Size field to contain circular
centroid area.	 
.VARI CL 
Circular centroid area's center
line coordinate.
.VARI CS
Circular centroid area's center
sample coordinate.
.VARI RADIUS
Radius of circular centroid
area.
.LEVEL 2
.VARI INP
Input file of records created by program prf.
.VARI OUT
Output file consisting of records selected based on parameters SIZE, CL, CS
and RADIUS.
.VARI SIZE
If specified, then the sums of the columns SLBOX + CLINBOX and SSBOX +
CSINBOX must lie in the field defined by SIZE in order for the record to be
selected.
.VARI CL
If CL, CS, and RADIUS are specified by the user then the point represented
by the fractional parts of the columns CLINBOX and CSINBOX must lie inside
the circle defined by (x - CL)^2 + (y - CS)^2 = RADIUS^2.
.VARI CS
If CL, CS, and RADIUS are specified by the user then the point represented
by the fractional parts of the columns CLINBOX and CSINBOX must lie inside
the circle defined by (x - CL)^2 + (y - CS)^2 = RADIUS^2.
.VARI RADIUS
If CL, CS, and RADIUS are specified by the user then the point represented
by the fractional parts of the columns CLINBOX and CSINBOX must lie inside
the circle defined by (x - CL)^2 + (y - CS)^2 = RADIUS^2.  If RADIUS is not
specified then this method of selection is ignored.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tsttablesearch.pdf
procedure help=*
!TEST FOR TABLESEARCH
refgbl $echo

body

let _onfail="continue"
let $echo="no"

ibis-gen tabs.tst  nc=10 nr=10 'column +
   form=("FULL","FULL","FULL","FULL","REAL","REAL","FULL","REAL","REAL", +
	"REAL") datacols=(1,2,3,4,5,6,7,8,9) +
   data=(128424,82, 84, 1,4.99,4.73,9,1250.0,30.0, +
	 128424,82, 94, 2,5.12,4.83,9,1250.0,30.0, +
	 128424,82,104, 3,5.25,4.91,9,1250.0,30.0, +
	 128424,82,114, 4,5.33,4.94,9,1250.0,30.0, +
	 128424,82,124, 5,5.47,4.99,9,1250.0,30.0, +
	 128424,83,134, 6,4.63,5.02,9,1250.0,30.0, +
	 128424,92, 84, 7,5.01,4.64,9,1250.0,30.0, +
	 128424,92, 94, 8,5.12,4.67,9,1250.0,30.0, +
	 128424,92,104, 9,5.30,4.81,9,1250.0,30.0, +
	 128424,92,114,10,5.44,4.84,9,1250.0,30.0)	 

ibis-list tabs.tst csize=(8,6,6,5,6,6,5,10,8,6) 'formats

let $echo=("yes","no","no")

tablesearch tabs.tst tabs.out size=(80,80,20,20)
ibis-list tabs.out csize=(8,6,6,5,6,6,5,10,8,8) 'formats

tablesearch tabs.tst tabs.out cl=.5 cs=.5 radius=.5
ibis-list tabs.out csize=(8,6,6,5,6,6,5,10,8,8) 'formats

tablesearch tabs.tst tabs.out size=(80,80,20,20) cl=.5 cs=.5 radius=.5
ibis-list tabs.out csize=(8,6,6,5,6,6,5,10,8,8) 'formats

tablesearch tabs.tst tabs.out cl=.5 cs=0.0 radius=.2
ibis-list tabs.out csize=(8,6,6,5,6,6,5,10,8,8) 'formats

tablesearch tabs.tst tabs.out
ibis-list tabs.out csize=(8,6,6,5,6,6,5,10,8,8) 'formats

end-proc
This is the test procedure for tablesearch.  It creates an IBIS test file
and runs tablesearch on it five times.  The rows that should be selected
are listed below (row numbers are listed in the fourth column of output):

	RUN #	  ROWS SELECTED
	-----  --------------------
	  1       1, 2, 7, 8
	  2    3, 4, 5, 6, 8, 9, 10
	  3	        8
	  4	  4, 5, 6, 10
	  5	       ALL
.help
$ Return
$!#############################################################################
