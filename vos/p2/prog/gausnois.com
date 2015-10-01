$!****************************************************************************
$!
$! Build proc for MIPL module gausnois
$! VPACK Version 1.7, Wednesday, July 06, 1994, 14:52:05
$!
$! Execute by entering:		$ @gausnois
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module gausnois ***"
$!
$ Create_Source = ""
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
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gausnois.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("gausnois.imake") .nes. ""
$   then
$      vimake gausnois
$      purge gausnois.bld
$   else
$      if F$SEARCH("gausnois.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gausnois
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gausnois.bld "STD"
$   else
$      @gausnois.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gausnois.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gausnois.com -
	-s gausnois.f -
	-i gausnois.imake -
	-p gausnois.pdf -
	-t tstgausnois.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gausnois.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	real*4	buffer(32768)
	real*4	mean,sigma
	real*4	x,y,u
	real*4	ranx,rany
	integer*4	nl,ns
	integer*4	cnt,status
	integer*4	line,samp
	integer*4	unit,seed
	character*8 format

	call xvp('NL',nl,cnt)
	call xvp('NS',ns,cnt)
	call xvp('FORMAT',format,cnt)

	call xvunit(unit,'OUT',1,status,' ')
	call xvopen(UNIT,STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT',format,
     +		'U_NL',NL, 'U_NS',NS,' ')

	call xvp('MEAN',mean,cnt)
	call xvp('SIGMA',sigma,cnt)
	call xvp('SEED',seed,cnt)
	if (cnt .eq. 0)  then
		call get_seconds( seed)
	end if

	do line = 1,nl
	    do samp = 1,ns
		call rangen(seed,ranx)
		call rangen(seed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		buffer(samp) = sigma*u + mean
	    enddo
	    call xvwrit(unit,buffer,status,' ')
	enddo

	call xvclose(unit,status,' ')

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gausnois.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gausnois

   To Create the build file give the command:

		$ vimake gausnois			(VMS)
   or
		% vimake gausnois			(Unix)


************************************************************************/


#define PROGRAM	gausnois

#define MODULE_LIST gausnois.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create gausnois.pdf
PROCESS HELP=*
PARM OUT TYPE=STRING
PARM NL TYPE=INTEGER DEFAULT=256
PARM NS TYPE=INTEGER DEFAULT=256
PARM MEAN  TYPE=REAL DEFAULT=128
PARM SIGMA TYPE=REAL DEFAULT=16
PARM SEED TYPE=INTEGER DEFAULT=-- COUNT=0:1
PARM FORMAT TYPE=(STRING,8) DEFAULT=BYTE

!# function="Generating Synthetic Images"
!# keywords=(image,pixel,random,number,RANGEN,VAX,VMS,RAN)
END-PROC
.TITLE
Create Gaussian noise image
.HELP
PURPOSE:

GAUSNOIS generates an image with random pixel values where the pixel values
are distributed according to a gaussian (normal or bell shaped) probability
distribution.  This type of a noise image can be useful in tests.


EXECUTION:

GAUSNOIS  NOISE.IMG  NL=200 NS=300  MEAN=100 SIGMA=25 FORMAT=BYTE  SEED=7382382

This is an example using all of the parameters:  it produces a 200x300 byte
image with mean 100 and standard deviation 25, and starts the random number
generator with that particular seed.

The SEED parameter is provided so that the same random images can be generated.
If it is defaulted the seed for the random number generator comes from the
system time.

All of the parameters except the output image can be defaulted.
.PAGE
PERFORMANCE:

	The ported version of GAUSNOIS is approximately half the speed of
the original GAUSNOIS.  The primary reason for this is that the random
number generator, RANGEN, is half the speed of the VAX/VMS routine RAN.
Steve Pohorsky agrees that this is unavoidable without a significant
rewrite to the program, including writing a seperate vectorized version of
the random number generator.

Original Programmer :	Frank Evans	August 1985
Ported to UNIX by :	Steve Hwan	June 1994
Cognizant Programmer:   Frank Evans

.LEVEL1
.VARIABLE OUT
Output image
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE MEAN
The mean of the distribution
.VARIABLE SIGMA
The standard deviation
of the distribution
.VARIABLE SEED
The starting seed for the
random number generator
.VARIABLE FORMAT
Output format (default BYTE)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgausnois.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
!
!
write "gausnois  noise.img  nl=200 ns=300  mean=100 sigma=25 format=byte  seed=7382382"
write "hist noise.img"
gausnois  noise.img  nl=200 ns=300  mean=100 sigma=25 format=byte  seed=7382382
hist noise.img
write " You should have just seen a straightforward usage of noise.img  The"
write " results are tallied by hist and should return something close to a"
write " mean of 100 and a sigma of 25 (or in this case, mean of 99.46647 and"
write " std dev of 24.95622)."
write ""
write ""
!
!
write "gausnois  noise.img  nl=200 ns=300  mean=50 sigma=5 format=byte  seed=4617316"
write "hist noise.img"
gausnois  noise.img  nl=200 ns=300  mean=50 sigma=5 format=byte  seed=4617316
hist noise.img
write " Next, I shifted the gaussian over and make it narrower.  I set the"
write " arguments to ask for a mean of 50 and a std. dev. of 5.  You should"
write " expect to see a mean of 49.51002 and std. dev. of 5.002862."
write ""
write ""
!
!
write "gausnois  noise.img  nl=200 ns=300  mean=0.15 sigma=0.2 format=real seed=74526"
write "hist noise.img"
gausnois  noise.img  nl=200 ns=300  mean=0.15 sigma=0.2 format=real seed=74526
hist noise.img
write " Next, I shifted the gaussian over and make it narrower again.  the"
write " primary reason for this test is to test the format=real.  I set the"
write " arguments to ask for a mean of 0.15 and a std. dev. of 0.2.  You"
write " should expect to see a mean of 0.149699 and std. dev. of 0.200121."
write ""
write ""
!
!
write "gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275"
write "gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275"
write "difpic inp=(noise.img,noise2.img)"
gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275
gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275
difpic inp=(noise.img,noise2.img)
write " Here, I called gaussnois twice with the same seed.  I should get the"
write " same image out (0 differences)"
write ""
write ""
!
!
write "gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275"
write "gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=94151"
write "difpic inp=(noise.img,noise2.img)"
gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=2167275
gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte  seed=94151
difpic inp=(noise.img,noise2.img)
write " Here, I called gaussnois twice with the same seed.  I should get"
write " very different images out. (expect 285 differences)"
write ""
write ""
!
!
write "gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte"
write "list noise.img"
write "gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte"
write "list noise2.img"
write "difpic inp=(noise.img,noise2.img)"
gausnois  noise.img  nl=10 ns=30  mean=50 sigma=5 format=byte
list noise.img
gausnois  noise2.img  nl=10 ns=30  mean=50 sigma=5 format=byte
list noise2.img
difpic inp=(noise.img,noise2.img)
write " In this case, I let the seed parameter default.  It should give me"
write " 2 very different images (>200 differences).  However, this part is"
write " dependent of the values of the system clock when this test is run"
write " so I cannot predict exactly how many differences there will be."
end-proc
$ Return
$!#############################################################################
