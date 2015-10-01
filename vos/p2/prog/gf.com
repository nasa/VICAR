$!****************************************************************************
$!
$! Build proc for MIPL module gf
$! VPACK Version 1.7, Wednesday, November 23, 1994, 14:38:04
$!
$! Execute by entering:		$ @gf
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
$ write sys$output "*** module gf ***"
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
$ write sys$output "Invalid argument given to gf.com file -- ", primary
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
$   if F$SEARCH("gf.imake") .nes. ""
$   then
$      vimake gf
$      purge gf.bld
$   else
$      if F$SEARCH("gf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gf.bld "STD"
$   else
$      @gf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gf.com -
	-p gf.pdf -
	-i gf.imake -
	-t tstgf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create gf.pdf
PROCEDURE	   HELP=*
! GF PDF - VICAR/IBIS MOSAIC SOFTWARE
! VICAR2/MIPL VERSION
PARM INP TYPE=STRING
PARM FUNCTION TYPE=(STRING,200),COUNT=(1:50)
PARM DIM TYPE=INTEGER COUNT=1 DEFAULT=2
PARM ZEROES  TYPE=KEYWORD VALID=(SKIP,INCLUDE) DEF=SKIP
BODY
  ! GF Now just calls MF; no big deal. The only difference
  ! is that MF has a different default ZEROES action.
  MF INP=@INP FUNCTION=@FUNCTION GR1DIM=@DIM ZEROES=@ZEROES
END-PROC
.TITLE
VICAR/IBIS Program GF
.HELP
PURPOSE

	GF   allows   the  user  to  create  FORTRAN or C   like 
	expressions to perform general mathematic operations on 
	one  or more graphics-1 file coordinates.  

TAE COMMAND LINE FORMAT

	GF INP=FILE.GRA PARAMS

	where

	FILE.GRA			  is a random access file.  Since it
					is used for both input and  output, 
					no output file is specified.

	PARAMS		    is   a  standard  VICAR   parameter 
					field.
.PAGE
METHOD

	GF now calls MF, and is provided merely for backward
	compatibility. These two programs are now completely
	interchangeable with the advent of the IBIS-2 subroutine
	library. For more information on the operation of the
	underlying code, see the help for MF. 


RESTRICTIONS

	Maximum number of operations in one execution is 50.
.PAGE
EXAMPLE

	GF INP=MIT FUNCTION=("X5 = X2/X3+100+SQRT(X2)")

	In this example,  X2 is divided by X3 and added to  100 
	plus the square root of X2.   The results are placed in 
	X5.  Further examples of allowable functions follow:

			 FUNCTION=("X5 = .NOT.(X3.AND.X2)")

	logical   operations  are  performed  bitwise  on   the 
	operands

			 FUNCTION=("X5 = X3.LE.X2")

	the  logical values T and F are converted to 1.  and 0. 
	for storage in column X5
			 FUNCTION=("X5 = (X3.LE.X2)*100.")

	the  logical  quantities  1.  and 0.  can  be  used  in 
	arithmetic

			FUNCTION=("X3 = POLY")

	This operation places the polygon number into the third
	coordinate (similar to the INDEX function in program MF)

Original Programmer:  N. D. Ritter	7 July,  1988

Cognizant Programmer:  N. D. Ritter	

Revision:  3				23 November 1994

.LEVEL1
.VARIABLE INP
Input IBIS graphics-1 file
.VARIABLE FUNCTION
Specifies function and coords
.VARIABLE DIM
Graphics Dimension
.VARIABLE ZEROES
SKIP or INCLUDE zero-rows?
.LEVEL2
.VARIABLE INP
				    Specifies IBIS graphics-1 file. There
				    is no output file. Results of GF are
				    written in INP.
.VARIABLE FUNCTION
	FUNCTION		  this keyword specifies the function 
					to be applied,  and the coordinates  to 
					which  it applies.   Functions  are 
					delimited  by  double  quotes.    X 
					followed  by a number indicates the 
					coordinates  used as input  or  output.  
					Up  to nineteen coordinates can be used 
					as  input.   The term INDEX can  be 
					used  in the arithmetic  expression 
					to  introduce row number  into  the 
					calculation   (see  examples).    A 
					special notation is used for coordinate 
					statistics   (see  examples).    By 
					using   ","   separator,    several 
					functions can be placed in one call
					(i.e. FUNC=("X1=POLY","X2=X1*X1").
					Maximum number of coordinates in one
					execution is 50. See the help on VICAR
					program MF for more information.
.VARIABLE DIM
Graphics-1 files have no dimension information stored in
the file. This parameter allows specification of the dimension.
For IBIS-1 interface and IBIS-2 format files this parameter is
ignored.
.VARIABLE ZEROES
Graphics files use an all-zero row to indicate "pen-up", and
usually should not be treated as data. By default, GF will
SKIP these rows. To override this feature, use the 'INCLUDE
keyword.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create gf.imake
/* Not much to this */

#define PROCEDURE gf

/* that's all folks */


$ Return
$!#############################################################################
$Test_File:
$ create tstgf.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

write "Since GF is now just a front-end to MF, we"
write "need only make sure that MF is called correctly."

!Generate a set of 4-point polygons to make sure values are skipped
ibis-gen a nc=3 nr=20 'ibis-1 'row
mf inp=a function=("c1 = index %5") gr1dim=3
ibis-list a nr=20 gr1dim=3

gf inp=a function=("X3 = POLY", "X2 = X3*X3", "X1 = (X2 != X3)+1") DIM=3

ibis-list a nr=20 gr1dim=3

let $echo="no"
end-proc
$ Return
$!#############################################################################
