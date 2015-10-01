$!****************************************************************************
$!
$! Build proc for MIPL module geom
$! VPACK Version 1.8, Friday, November 03, 1995, 14:40:04
$!
$! Execute by entering:		$ @geom
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
$ write sys$output "*** module geom ***"
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
$ write sys$output "Invalid argument given to geom.com file -- ", primary
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
$   if F$SEARCH("geom.imake") .nes. ""
$   then
$      vimake geom
$      purge geom.bld
$   else
$      if F$SEARCH("geom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geom.bld "STD"
$   else
$      @geom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geom.com -
	-i geom.imake -
	-p geom.pdf -
	-t tstgeom.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geom.imake
#define PROCEDURE geom
#define R2LIB
$ Return
$!#############################################################################
$PDF_File:
$ create geom.pdf
PROCEDURE help=*
 !
 PARM INP     TYPE=STRING  COUNT=1:2
 PARM OUT     TYPE=STRING  COUNT=1:2
 PARM SIZE    TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=1 DEFAULT=0
 !
 PARM NAH     TYPE=INTEGER COUNT=0:1      DEFAULT=--
 PARM NAV     TYPE=INTEGER COUNT=0:1      DEFAULT=--
 PARM TIEPOINT TYPE=REAL   COUNT=0:600   DEFAULT=--
 PARM INTERP  TYPE=KEYWORD COUNT=0:1 VALID=(NOIN,ZNOIN) DEFAULT=--
 PARM HVARY   TYPE=KEYWORD COUNT=0:1 VALID=HVARY DEFAULT=--
 !
 PARM PARMS   TYPE=STRING  COUNT=0:1 DEFAULT=--
 !
 PARM IDSNAM  TYPE=STRING  COUNT=1   DEFAULT=LGIDS
 PARM IDSNS   TYPE=INTEGER COUNT=1   DEFAULT=1000
 !
BODY
WRITE "GEOM start"
!
IF ($COUNT(INP)=2)
   WRITE "Tiepoint file supplied:  LGEOM called"
   GOTO LGEOM
END-IF
!
!MGEOM>
!LET _ONFAIL="GOTO LGEOM"
!WRITE " MGEOM start"
!IF ($COUNT(PARMS)=0)
!   MGEOM  	@INP @OUT SIZE=@SIZE                    +
!        SL=@SL SS=@SS NL=@NL NS=@NS	        +
!	NAH=@NAH NAV=@NAV	+
!	TIEPOINT=@TIEPOINT                      +
!        INTERP=@INTERP HVARY=@HVARY
!ELSE
!   MGEOM  	@INP @OUT SIZE=@SIZE                    +
!        SL=@SL SS=@SS NL=@NL NS=@NS	        +
!        INTERP=@INTERP HVARY=@HVARY +
!        PARMS=@PARMS
!END-IF
!WRITE " MGEOM completed"
!GOTO DONE
!
LGEOM>
LET _ONFAIL="RETURN"
LOCAL INTERPL KEYWORD VALID=NOIN COUNT=0:1 INITIAL=--
IF (INTERP="NOIN") LET INTERPL=INTERP
LOCAL OUT1 STRING INITIAL=""
LET OUT1=OUT(1)
!
WRITE " LGEOM start"
IF ($COUNT(PARMS)=0)
    LGEOM   @INP @OUT1 SIZE=@SIZE			+
	SL=@SL SS=@SS NL=@NL NS=@NS		+
	NAH=@NAH NAV=@NAV	+
	TIEPOINT=@TIEPOINT                      +
	INTERP=@INTERPL				+
        IDSNAM=@IDSNAM IDSNS=@IDSNS 
ELSE
    LGEOM   @INP @OUT1 SIZE=@SIZE			+
	SL=@SL SS=@SS NL=@NL NS=@NS		+
	INTERP=@INTERPL				+
        IDSNAM=@IDSNAM IDSNS=@IDSNS +
        PARMS=@PARMS
END-IF
WRITE " LGEOM completed"
GOTO DONE
!
DONE>
WRITE "GEOM completed"
END-PROC
!
!HELP TEXT FOR GEOM
.TITLE
GEOM - Geometric transformations on images
.HELP
PURPOSE

 GEOM is a general purpose procedure that frees the user from the
 burden of selecting which specific geometric transformation program
 is most appropriate for the picture at hand. It tries to run MGEOM
 first as MGEOM is considerably faster and somewhat more accurate.
 If MGEOM fails to work on the picture because the transformation as
 defined by the tiepoints is too complicated (e.g. large rotation or
 pretzel bend), then LGEOM is attempted which should be able to handle
 any case, albeit slowly.
.page
CALLING SYNTAX:

 GEOM   (PIX [,TIE])   (GEO [, IDS)   PARAMS

 where PIX is the input image, TIE is an optional tiepoint dataset,
 (which can only be used by LGEOM), GEO is the geometrically transformed
 output, and IDS is a scratch file that may be used by MGEOM.  Note that
 the scratch file used by LGEOM is supplied via the parameter IDSNAM.

 PARAMS are user parameters that are described further under Tutor Help
 mode.

 The user is referred to the Help for MGEOM and LGEOM for information on
 how these operate.
.page
WRITTEN BY:  Budak Z. Barkan,  24 April 1985

CURRENT COGNIZANT PROGRAMMER:  L. W. Kamp

Make Portable: F.F.Moss 3 November 1995
               Comment out MGEOM because MGEOM is not working, once 
               MGEOM is fixed, need to remove the comment in this proc.
.LEVEL1
.VARI INP
Input file name and (LGEOM
only) optional tiepoint file.
.VARI OUT
Output file name and (MGEOM
only) optional IDS.
.VARI SIZE
Standard VICAR Size parameter
.VARI NL
Number of lines for output
.VARI NS
Number of samples for output
.VARI IDSNAM
Name of intermediate data set
(LGEOM only)
.VARI IDSNS
Bytes per line in intermediate
data set
.VARI NAH
Number of areas horizontally.
.VARI NAV
Number of areas vertically.
.VARI INTERP
Interpolation method
Valid: NOIN,ZNOIN; 
.VARI TIEPOINT
Specifies mapping of control
points.
.VARI HVARY
Non-uniform horizontal grid
spacing (MGEOM only)
Valid: HVARY
.VARI PARMS
Parameter file for tiepoints
.level2
.vari INP
The first value is the name of the input image.

The optional second value is a tiepoint file, which can only be used 
by LGEOM.  (Tiepoints can be passed to MGEOM by the PARMS parameter.)
If this file is specified, then MGEOM will not be called.
.vari OUT
The first value is the geometrically transformed output image name.

The optional second value is a scratch file that may be used by MGEOM.
Note that LGEOM uses a scratch file supplied via the IDSNAM parameter;
if this file is specified and LGEOM is called, the file will be ignored.
.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 samples.
.VARI NL
The number of lines for output.  (See SIZE.)
.VARI NS
The number of samples per line for output.  (See SIZE.)
.VARI SL
The starting line for output.  (See SIZE.)
.VARI SS
The starting sample for output.  (See SIZE.)
.VARI IDSNAM
(This parameter is used only by LGEOM.)

IDSNAM is an optional parameter which can be used to change the
default name of the intermediate data set used by LGEOM.
.VARI IDSNS
(This parameter is used only by LGEOM.)

IDSNS is an optional parameter which may be used to change the number
of bytes per line in the intermediate data set used by LGEOM, see
parameter IDSNAM.

The purpose of this parameter is to allow the user to "tune" LGEOM to 
obtain the optimum speed.
.VARI INTERP
This parameter has two valid values:  NOIN and ZNOIN.

NOIN means no interpolation.  The default method for computing the
DN values of the output picture is to use a bi-linear interpolation
on the four nearest neighbors in the input picture.  With NOIN, the
value of the nearest point is simply used.

ZNOIN (recognized by MGEOM only) specifies that a bilinear
interpolation is done except when one or more of the points used 
has a value equal to zero. In that case the nearest method is used.
This allows preparation of sharp edges (no interpolation rolloff)
for mosaicking.
.VARI NAH
NAH=N where N is an integer value specifing the number of areas horizontally;
which is also the number of columns of tiepoints less 1.
.VARI NAV
NAV=N where N is an integer value specifing the number of areas vertically;
which is also the number of rows of tiepoints less 1. 
.VARI HVARY
(This parameter is only recognized by MGEOM.)

Valid value: HVARY

This keyword is used if the tiepoint grid is not uniform in the 
horizontal direction. If this keyword is used, then the program 
will abend if more than fifty horizontal grid areas are processed 
in a single swath.  If the grid is small (NAH < 50) then this 
keyword isn't needed).

Vertical spacing is not subject to this limitation or this keyword.
.VARI TIEPOINT
Each tiepoint is four real numbers, the first two are the line-sample
coordinate in the output, the second two are the line-sample coordinate
in the input which is mapped to the point in the output.  There must be
(nah+1)*(nav+1) tiepoint quadruples aligned in a perfectly horizontal
and vertical grid.
.vari PARMS
This optional parameter contains the name of a parameter dataset which
the user wishes to pass to this procedure.  If specified, this dataset 
will be used by MGEOM or LGEOM to read in the tiepoints.

If specified, this file must contain the NAH, NAV, and TIEPOINT parameters
(and no others).
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgeom.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gen a 1000 1000
!
! This case should work with MGEOM:
!geom							+
!inp=a					+
!out=b					+
!nl=900 ns=900						+
!nav=1							+
!nah=1							+
!tiepoint=(						+
!001,001,001,001,					+
!001,900,001,900,					+
!900,001,900,001,					+
!900,900,900,900)
!
! This case should fail in MGEOM and run LGEOM:
geom							+
inp=a					+
out=b					+
nl=900 ns=900						+
nav=9							+
nah=9							+
tiepoint=(						+
001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
001,800,001,800,001,900,001,900,100,001,100,001,+
100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
100,900,100,900,		+
200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
200,800,200,800,200,900,200,900,300,001,300,001,+
300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
300,900,300,900,		+
400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
400,800,400,800,400,900,400,900,500,001,500,001,+
500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
500,900,500,900,		+
600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
600,800,600,800,600,900,600,900,700,001,700,001,+
700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
700,900,700,900,		+
800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
800,800,800,800,800,900,800,900,900,001,900,001,+
900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
900,900,900,900)
!
! run above case again, testing SIZE & INTERP params:
geom							+
inp=a					+
out=b					+
size=(1,1,900,900)					+
interp=noin						+
nav=9							+
nah=9							+
tiepoint=(						+
001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
001,800,001,800,001,900,001,900,100,001,100,001,+
100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
100,900,100,900,		+
200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
200,800,200,800,200,900,200,900,300,001,300,001,+
300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
300,900,300,900,		+
400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
400,800,400,800,400,900,400,900,500,001,500,001,+
500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
500,900,500,900,		+
600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
600,800,600,800,600,900,600,900,700,001,700,001,+
700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
700,900,700,900,		+
800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
800,800,800,800,800,900,800,900,900,001,900,001,+
900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
900,900,900,900)
end-proc
$ Return
$!#############################################################################
