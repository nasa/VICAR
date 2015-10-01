$!****************************************************************************
$!
$! Build proc for MIPL module swap
$! VPACK Version 1.9, Friday, September 04, 1998, 14:43:14
$!
$! Execute by entering:		$ @swap
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
$ write sys$output "*** module swap ***"
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
$ write sys$output "Invalid argument given to swap.com file -- ", primary
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
$   if F$SEARCH("swap.imake") .nes. ""
$   then
$      vimake swap
$      purge swap.bld
$   else
$      if F$SEARCH("swap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake swap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @swap.bld "STD"
$   else
$      @swap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create swap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack swap.com -
	-s swap.f -
	-i swap.imake -
	-p swap.pdf -
	-t tstswap.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create swap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program swap

      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsize=20000)
      complex*8 bufin(maxsize),bufout(maxsize)
      integer*4 inunit,outunit,status

c open input
      call xvunit(inunit,'INP',1,status,' ')
      call xvsignal(inunit,status,1)
      call xvopen(inunit,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit,status,1)
      call xvget(inunit,status,'NL',nl,'NS',ns,' ')
      call xvsignal(inunit,status,1)
      if(ns.gt.maxsize)then
        call xvmessage('Picture records too long',' ')
        call abend
      endif
      if((ns/2)*2.ne.ns)then
        call xvmessage('NS must be even',' ')
        call abend
      endif
      if((nl/2)*2.ne.nl)then
        call xvmessage('NL must be even',' ')
        call abend
      endif

c open output
      call xvunit(outunit,'OUT',1,status,' ')
      call xvsignal(outunit,status,1)
      call xvopen(outunit,status,'U_FORMAT','COMP','OP','WRITE',' ')
      call xvsignal(outunit,status,1)
  
c process data
c     copy bottom half to top half
      do line=(nl/2)+2,nl
        call xvread(inunit,bufin,status,'LINE',line,' ')
        call xvsignal(inunit,status,1)
        k=0
        do i=(ns/2)+2,ns
          k=k+1
          bufout(k)=bufin(i)
        enddo
        do i=1,(ns/2)+1
          k=k+1
          bufout(k)=bufin(i)
        enddo
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo
c     copy top half to bottom half
      do line=1,(nl/2)+1
        call xvread(inunit,bufin,status,'LINE',line,' ')
        call xvsignal(inunit,status,1)
        k=0
        do i=(ns/2)+2,ns
          k=k+1
          bufout(k)=bufin(i)
        enddo
        do i=1,(ns/2)+1
          k=k+1
          bufout(k)=bufin(i)
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
$ create swap.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM swap

   To Create the build file give the command:

		$ vimake swap			(VMS)
   or
		% vimake swap			(Unix)


************************************************************************/


#define PROGRAM	swap
#define R2LIB

#define MODULE_LIST swap.f

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
$ create swap.pdf
process help=*
PARM INP TYPE=(STRING,32) COUNT=1
PARM OUT TYPE=(STRING,32) COUNT=1
END-PROC
.TITLE
VICAR2 Program SWAP -- swaps the quadrants of an image or fft.
.HELP

PURPOSE

Switches the locations of the quadrants of an image or a complex fft.
Upper left goes to lower right.
Lower right goes to upper left.
Upper right goes to lower left.
Lower left goes to upper right.

The main purpose of SWAP is to move the origin or DC term of the fft located
at (1,1) to the center of the image at (nl/2,ns/2).

EXECUTION

fft22 a b
swap b c 

RESTRICTIONS

Image must have an even number of lines and samples.

Cognizant Programmer:  JJ Lorre   July 1998

.LEVEL1
.VARIABLE INP
input image
.VARIABLE OUT
output image

.LEVEL2
.VARIABLE INP
input image
.VARIABLE OUT
output image
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstswap.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
gen out=a.img nl=8 ns=8
swap a.img b.img
list a.img
list b.img
!
gausnois out=a.img nl=16 ns=16
fft22 a.img b.img
swap b.img c.img
list b.img
list c.img
!
end-proc
$ Return
$!#############################################################################
