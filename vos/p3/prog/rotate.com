$!****************************************************************************
$!
$! Build proc for MIPL module rotate
$! VPACK Version 1.5, Monday, August 09, 1993, 10:31:44
$!
$! Execute by entering:		$ @rotate
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
$ write sys$output "*** module rotate ***"
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
$   if F$SEARCH("rotate.imake") .nes. ""
$   then
$      vimake rotate
$      purge rotate.bld
$   else
$      if F$SEARCH("rotate.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rotate
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rotate.bld "STD"
$   else
$      @rotate.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rotate.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rotate.com -
	-i rotate.imake -
	-p rotate.pdf -
	-t tstrotate.pdf timerotate.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rotate.imake
#define  PROCEDURE rotate
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create rotate.pdf
PROCEDURE HELP=* 
PARM	INP	TYPE=(STRING,80)
PARM	OUT	TYPE=(STRING,80) COUNT=0:1 	DEFAULT=--
PARM 	SIZE	TYPE=INTEGER	COUNT=0:4 	DEFAULT=--
PARM 	SL	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM 	SS	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	NL	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	NS	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	ANGLE	TYPE=REAL
PARM	LINE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	SAMPLE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	CENTER	TYPE=REAL	COUNT=(0:2)	DEFAULT=--
PARM	NOINTERP   TYPE=KEYWORD	COUNT=(0:1) VALID="NOINTERP" DEFAULT=--
PARM 	IDSNAM  TYPE=STRING     COUNT=1 	DEFAULT=LGIDS
PARM  	IDSNS   TYPE=INTEGER 	COUNT=1 	DEFAULT=1000
BODY
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	+
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	+
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=&INP OUT=&OUT SIZE=@SIZE NL=@NL NS=@NS +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
END-PROC
.title
Vicar procedure ROTATE  --  rotate an image by some angle.
.help
PURPOSE:

ROTATE will rotate a picture any amount about a specified point.
It writes out the necessary parameters and then runs LGEOM
to accomplish the rotation.

EXECUTION:

   The following is the execution statement format for ROTATE:

		ROTATE INP OUT PARAMS

	or, to compute the parameters without GEOM-ing:

		ROTATE INP PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their 
   respective sections.
.page
OPERATION:

ROTATE generates parameters for LGEOM to rotate a picture.
These parameters are passed via parameter I/O routines to file ZZPAR.

The rotation is about an axis normal to the picture and intersecting it at
the specified pixel center of rotation.

The size field should take into account any increase in the number 
of lines and samples due to the rotation.
.page
EXAMPLES:

1) rotate IN OUT SIZE=(1,1,100,160) LINE=15. SAMP=35. ANGL=24.2
----This example will rotate the 100x160 sample file by 24.2 degrees about
    the pixel at line 15 and sample 35.

2) rotate IN OUT SIZE=(1,1,100,160) ANGL=24.2
----This example does the same but about the center of the picture.

3) rotate IN OUT ANGL=-1. CENTER=(50.,30.)
----This example will rotate IN -1. degrees about its center and translate the
    rotated picture so that the center of rotation in the output occupies
    line 50, sample 30.
.page
 TIMING: 
  The following CPU times for ROTATE were obtained on a 
VAX 8650 (MIPL2) in June 1993.
.page

 ORIGINAL PROGRAMMER:    A. R. Gillespie, 25 Jul 1972
 CURRENT COGNIZANT PROGRAMMER:  L.W.Kamp
 PORTED TO UNIX: Steve Pohorsky

 REVISION HISTORY
  93-6-8    SP   Made portable for UNIX.
.level1
.vari inp
The data file to be rotated.
.vari out
The rotated data file.
.vari size
The area to be rotated.
.vari sl
Starting line of input 
.vari ss
Starting sample of input 
.vari nl
Number of lines in output 
.vari ns 
Number of samples in output 
.vari angle
The amount of rotation 
in degrees clockwise.
.vari line
The line number of the 
center of rotation in input.
.vari sample
The sample of the center 
of rotation in input.
.vari center
The location of the center
of rotation in the output. 
.vari nointerp
Indicates no interpolation.
.VARI IDSNAM
Name of Intermediate Data Set
.VARI IDSNS
Bytes per line in Intermediate
Data Set
.level2
.vari inp
A VICAR labelled image to be rotated.
.vari out
A VICAR labelled image file to receive the rotated image.

If OUT is omitted, only the ROTATE2 (tiepoint computation step) is 
performed, and LGEOM is not invoked.  The parameter dataset 
ZZPAR is still created, so the user may run LGEOM with 
PARMS=ZZPAR as a separate step to complete the rotation.  
(See ROTATE.PDF.)
.vari size
The size field indicates which area of the input image is to be 
rotated.  The NL and NS parameters specify the size of the output image.
.vari sl
The starting line of the size field.
.vari ss
The starting sample of the size field.
.vari nl
The number of lines in the size field.  Also, the number of lines in the
output image.
.vari ns
The number of bytes in the size field.  Also, the number of bytes in the 
output image.
.vari angle
This is the only required parameter.  It specifies the number of degrees
clockwise from up to rotate the image.  May be positive or negative.
.vari line
This is the line number of the center of rotation in the input image.
Default = .5 * (sl + nl) ...the center line of the picture.
.vari sample
This is the sample number of the center of rotation in the input image.
Default = .5 * (ss + ns) ...the center sample of the picture.  (Note that the
sample values are expressed in units of pixels, not bytes.)
.vari center
This specifies the center of rotation in the output image.  Default is the 
same as that specified for or defaulted for the input image.  (Note that the
sample values are expressed in units of pixels, not bytes.)
.vari nointerp
This specifies that no interpolation is to be performed during the GEOM.
In this case, the DN value of the point closest to the fractional line and
sample is used.  This method ("nearest neighbor") is somewhat faster, but is
not as accurate as the four point interpolation.  Default = NOINTERP.
.VARI IDSNAM
IDSNAM is an optional parameter which can be used to change the
default name of the Intermediate Data Set.  Primarily it can 
be used to change the directory specification.  

.VARI IDSNS
IDSNS is an optional parameter which may be used to change the number
of bytes per line in the Intermediate Data Set.  The only real purpose
of this parameter is to allow the user to "tune" LGEOM to obtain the
optimum speed.
$ Return
$!#############################################################################
$Test_File:
$ create tstrotate.pdf
PROCEDURE
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
WRITE "THIS IS A TEST OF MODULE ROTATE"
WRITE "WE WILL ROTATE A GEN'D IMAGE BY -45 DEG SUCH THAT"
WRITE "SHADING SHOULD APPEAR IN THE SAMPLE DIRECTION ONLY"
let $echo="yes"
let $autousage="none"
gen A NL=15 NS=15 IVAL=90
list A
rotate A B ANGLE=-45. IDSNAM=IDS.DAT IDSNS=1000
list B
!
! TEST LOGICAL ASSIGNMENT OF IDS
WRITE "SHIFT THE OUTPUT CENTER OF ROTATION AND USE NOINTERP"
rotate A B ANGLE=-45. 'NOIN CENTER=(8,4)
list B
!
WRITE "NOW LET'S ROTATE ABOUT (10,6) [104 DN]"
WRITE " AND MAKE IT END UP AT (3,3)....AND IN HALFWORD"
gen C NL=15 NS=16 IVAL=90 'HALF
list C
rotate C D ANGLE=-45. LINE=10 SAMP=6 CENTER=(3,3)
list D
!
! AND ONE CASE WHERE LGEOM IS CALLED:
rotate C D ANGLE=-85
list D
end-proc
$!-----------------------------------------------------------------------------
$ create timerotate.pdf
procedure        !  PROCEDURE FOR MEASURING rotate PERFORMANCE ON VAX
refgbl $echo     !  R2LIB:rotate WAS THE UNPORTED VERSION; MODIFY ACCORDINGLY
body             !  IF RERUNNING.
let _onfail="continue"
let $echo="yes"
gen A NL=1000 NS=1000 IVAL=0 
!
rotate A B ANGLE=-45. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    17.66s
!
r2lib:rotate A B2 ANGLE=-45. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    15.84s
!
difpic (B B2)
!
rotate A B ANGLE=-90. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    17.66s
!
r2lib:rotate A B2 ANGLE=-90. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    15.84s
!
difpic (B B2)
!
! repeat for halfword
!
gen A NL=1000 NS=1000 IVAL=0 'HALF
!
rotate A B ANGLE=-45. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    17.66s
!
r2lib:rotate A B2 ANGLE=-45. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    15.84s
!
difpic (B B2)
!
rotate A B ANGLE=-90. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    17.66s
!
r2lib:rotate A B2 ANGLE=-90. 
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    15.84s
!
difpic (B B2)
!
!
!
end-proc
$ Return
$!#############################################################################
