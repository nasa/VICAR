$!****************************************************************************
$!
$! Build proc for MIPL module fftadd
$! VPACK Version 1.9, Friday, September 04, 1998, 14:42:48
$!
$! Execute by entering:		$ @fftadd
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
$ write sys$output "*** module fftadd ***"
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
$ write sys$output "Invalid argument given to fftadd.com file -- ", primary
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
$   if F$SEARCH("fftadd.imake") .nes. ""
$   then
$      vimake fftadd
$      purge fftadd.bld
$   else
$      if F$SEARCH("fftadd.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftadd
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftadd.bld "STD"
$   else
$      @fftadd.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftadd.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftadd.com -
	-s fftadd.f -
	-i fftadd.imake -
	-p fftadd.pdf -
	-t tstfftadd.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftadd.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program swap

      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsize=20000)
      complex*8 bufin1(maxsize),bufin2(maxsize),bufout(maxsize)
      integer*4 inunit1,inunit2,outunit,status

c open inputs
      call xvunit(inunit1,'INP',1,status,' ')
      call xvsignal(inunit1,status,1)
      call xvopen(inunit1,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit1,status,1)
      call xvget(inunit1,status,'NL',nl,'NS',ns,' ')
      call xvsignal(inunit1,status,1)
      if(ns.gt.maxsize)then
        call xvmessage('Picture records too long',' ')
        call abend
      endif
      call xvunit(inunit2,'INP',2,status,' ')
      call xvsignal(inunit2,status,1)
      call xvopen(inunit2,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit2,status,1)
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      call xvsignal(inunit2,status,1)
      if((nl.ne.nl2).or.(ns.ne.ns2))then
        call xvmessage('Pictures of unequal size',' ')
        call abend
      endif

c open output
      call xvunit(outunit,'OUT',1,status,' ')
      call xvsignal(outunit,status,1)
      call xvopen(outunit,status,'U_FORMAT','COMP','OP','WRITE',' ')
      call xvsignal(outunit,status,1)
  
c process data
      do line=1,nl
        call xvread(inunit1,bufin1,status,' ')
        call xvsignal(inunit1,status,1)
        call xvread(inunit2,bufin2,status,' ')
        call xvsignal(inunit2,status,1)
        do i=1,ns
          bufout(i)=bufin1(i)+bufin2(i)
        enddo
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftadd.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftadd

   To Create the build file give the command:

		$ vimake fftadd			(VMS)
   or
		% vimake fftadd			(Unix)


************************************************************************/


#define PROGRAM	fftadd
#define R2LIB

#define MODULE_LIST fftadd.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftadd.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=1
END-PROC
.TITLE
VICAR2 Program FFTADD - Adds two fft's.
.HELP

PURPOSE
Adds two fft's.

EXECUTION
fftadd inp=(fft1,fft2) out=fft12

Cognizant Programmer:  JJ Lorre   July 1998

.LEVEL1
.VARIABLE INP
2 input images
.VARIABLE OUT
output image

.LEVEL2
.VARIABLE INP
2 input images
.VARIABLE OUT
output image
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftadd.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
gausnois out=a.img nl=16 ns=16 seed=123456
gausnois out=b.img nl=16 ns=16 seed=123452
fft22 a.img ffta.img
fft22 b.img fftb.img
fftadd (ffta.img,fftb.img) fftab.img
list ffta.img nl=1 ns=8
list fftb.img nl=1 ns=8
list fftab.img nl=1 ns=8
!
end-proc
$ Return
$!#############################################################################
