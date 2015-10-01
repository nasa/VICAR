$!****************************************************************************
$!
$! Build proc for MIPL module xyzpic
$! VPACK Version 1.8, Wednesday, September 27, 1995, 16:25:25
$!
$! Execute by entering:		$ @xyzpic
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
$ write sys$output "*** module xyzpic ***"
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
$ write sys$output "Invalid argument given to xyzpic.com file -- ", primary
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
$   if F$SEARCH("xyzpic.imake") .nes. ""
$   then
$      vimake xyzpic
$      purge xyzpic.bld
$   else
$      if F$SEARCH("xyzpic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xyzpic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xyzpic.bld "STD"
$   else
$      @xyzpic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xyzpic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xyzpic.com -
	-s xyzpic.f -
	-i xyzpic.imake -
	-p xyzpic.pdf -
	-t tstxyzp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xyzpic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c     program name: xyzpic
c
c     author: Leo M. Bynum
c     date:   June 30, 1985
cc
c
c********************************************************************
c
c     update history:
c
c     revision a - initial release
c
c     revision b - rewritten to take in a graphics 1 format only
c                  lmb 6/85
c
c     revision c - fixed half and real output
c		   kfe 9/85
c
c     revision d - put in calls to graphics I/O subroutines
c		   put in sorting  
c		   kfe    March 1986
c
c     revision e - upgrade to handle 1,000,000 data points
c
c     revision f - upgrade to handle infinite data points
c		   and avoid "page file quota" problems
c		   added test procedure for P2 upgrade
c	           ndr    October 1994
c
c     revision g - MSTP S/W Conversion (VICAR Porting)  CRI  2JAN95
c
c********************************************************************
c
	include 'VICMAIN_FOR'
	subroutine main44
c
	implicit none
c
	integer	wunit, status, nl, ns, count
	integer arraysize
	parameter (arraysize=100000)
	integer	linearray(arraysize)
	integer samparray(arraysize)
	integer pointer(arraysize)
	real	DNvalue(arraysize)
	integer	numpoints
	logical	eof
	character*8 format
c
      call ifmessage('XYZPIC version 02-JAN-95')
c
      call xveaction('SA',' ')
c
c		get the size and format of the output image
	call xvp('NL',nl,count)
	call xvp('NS',ns,count)
	call xvp('FORMAT',format,count)

c		open output image and clear it
	call xvunit(wunit,'OUT',1,status,' ')
	call xvopen (wunit, status, 'OP','WRITE', 'U_NL',nl, 'U_NS',ns,
     *             'U_FORMAT','REAL', 'O_FORMAT',format,' ')
	call clear_image(wunit,nl,ns)
	call xvclose(wunit,status,' ')
	call xvopen (wunit, status, 'OP','UPDATE',
     *             'U_FORMAT','REAL', ' ')

c		open the 3-D graphics-1 file
	call RDGR (1, 1, 3)

c		Process all the points
	eof = .false.
	do while (.not. eof)
	    call getpts(nl,ns,numpoints,eof,arraysize,
     +	    		linearray,samparray,DNvalue,pointer)
	    call writepts(wunit,nl,ns,numpoints,
     +			linearray,samparray,DNvalue,pointer)

	enddo

	call CLGR (1)
	call xvclose(wunit,status,' ')

	return
	end

c--------------------------------------------------------------------
	subroutine clear_image(wunit,nl,ns)
	real*4 buffer(64000)
	integer i,wunit,nl,ns,status

	do i=1,ns
		buffer(i)=0.0
	enddo

	do i=1,nl
		call xvwrit(wunit,buffer,status,'line',i,' ')
		if (status.ne.1) call xvsignal(wunit,status,1)
	enddo

	return
	end

c--------------------------------------------------------------------
c
c  getpts reads in a buffer of graphics points, and sorts the
c   linearray for more efficient file i/o later
c


	subroutine getpts(nl,ns,numpoints,eof,arraysize,
     +	    		linearray,samparray,DNvalue,pointer)
	integer	nl, ns
	integer arraysize
	integer	linearray(arraysize)
	integer samparray(arraysize)
	integer pointer(arraysize)
	real	DNvalue(arraysize)
	integer	line, samp
	integer	point, numpoints
	real	x, y, z
	logical	eof, zero
	

c		read the line, sample, DNvalue triplets into the arrays
c			ignoring points that fall outside of the image
	point = 1
	eof = .false.
	do while (.not. eof .and. point.lt.arraysize)
	    call NEXTGR (1, eof, x, y, z)
	    zero = .false.
	    do while (.not. eof .and. .not. zero
     +	      .and. point.lt.arraysize)
		line = nint(x)
		samp = nint(y)
		if (line .ge. 1 .and. line .le. nl .and.
     +		    samp .ge. 1 .and. samp .le. ns)  then
		    linearray(point) = line
		    samparray(point) = samp
		    pointer(point) = point
		    DNvalue(point) = z
		    point = point + 1
		endif
		if (point.lt.arraysize)
     +		    call GETGR (1, zero, eof, x, y, z)
	    enddo
	enddo
	linearray(point) = nl + 1      ! put in fake point to make image finish
	samparray(point) = 1
	pointer(point) = point
	numpoints = point

	call INDSRTF(linearray, pointer, numpoints)

	return
	end

c--------------------------------------------------------------------
c
c writepts writes out the points int the buffer
c
	subroutine writepts(wunit,nl,ns,numpoints,
     +			linearray,samparray,DNvalue,pointer)
	integer	wunit,status, nl,ns
	integer	linearray(1)
	integer samparray(1)
	integer pointer(1)
	real	DNvalue(1)
	real	buffer(64000)
	integer	line, samp, oldline
	integer	point, numpoints

	if (numpoints.lt.2) return  !No valid points

c	Get first line to update
	call xvread(wunit,buffer,status,'line',1,' ')
	if (status.ne.1) call xvsignal(wunit,status,1)
	oldline = 1

	do point = 1, numpoints

	    line = linearray(pointer(point))

	    if (line .gt. oldline) then
c			write old line to image
		call xvwrit(wunit,buffer,status,'LINE',oldline,' ')
c		read in new line to update
		if (line .le. nl)
     +		   call xvread(wunit,buffer,status,'line',line,' ')
		oldline = line
	    endif

	    samp = samparray(pointer(point))
	    buffer(samp) = DNvalue(pointer(point))

	enddo

	return
	end
	
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xyzpic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM xyzpic

   To Create the build file give the command:

		$ vimake xyzpic			(VMS)
   or
		% vimake xyzpic			(Unix)


************************************************************************/


#define PROGRAM	xyzpic
#define R2LIB

#define MODULE_LIST xyzpic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create xyzpic.pdf
PROCESS help=*
  PARM INP    TYPE=(STRING,40)  COUNT=1
  PARM OUT    TYPE=(STRING,40)  COUNT=1
  PARM NL     TYPE=INTEGER
  PARM NS     TYPE=INTEGER   VALID=(1:64000)
  PARM FORMAT TYPE=KEYWORD   VALID=(BYTE,HALF,FULL,REAL,DOUB,COMP) +
           DEFAULT=BYTE
END-PROC
.TITLE
VICAR/IBIS Program xyzpic
.help
PURPOSE

    xyzpic takes an three dimensional (3-D) IBIS graphics-1 
file of (line, sample, DN value) triplets and creates an image 
of all zero value pixels except for those pixels indicated in 
the graphics file.  Any coordinates off of the image are ignored.
The DN values are clipped to fit within the DN range of the format
of the output pixels.  The points in the graphics file may be
in any order (sorting is performed by the program).  The line, sample
position of the points is rounded to the nearest pixel, while
the DN value is truncated for integer output formats.


EXECUTION

xyzpic INP=POINTS.GRA  OUT=FILE.IMG  NL=3000 NS=5000 FORMAT=HALF





RESTRICTIONS

The maximum output line length is 64,000 samples.



Original Programmer:  Leo Bynum        June 1985

Cognizant Programmer: Niles Ritter
Made portable for UNIX      CRI        02-JAN-95


.LEVEL1
.VARIABLE INP
IBIS graphics file (input).
containing (line,sample,DN value)
triplets
.VARIABLE OUT
VICAR image (output).
.VARIABLE NL
Number of lines 
in the output.
.VARIABLE NS
Number of samples
in the output.
.VARIABLE FORMAT
Type of pixel to be output.

.LEVEL2
.VARIABLE INP
IBIS graphics file (input).
containing (line,sample,DN value)
triplets
.VARIABLE OUT
VICAR image (output).
.VARIABLE NL
Number of lines in the output.
.VARIABLE NS
Number of samples in the output.
.VARIABLE FORMAT
Format of pixel to be output.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstxyzp.pdf
procedure
!refgbl $autousage
body
!let $autousage="none"

!Generate an old Graphics-1 file
! In ported code this should also work for the
! files generated by the ported ibisgr library.
!
ibis-gen graph nc=3 nr=9 'ibis-1 'row datacol=(1,2,3) +
   data=( 7,1,5,   1,8,7,   3,8,20, +
   		  3,1,30,  8,2,8,   8,8,254, +
		  9,9,500, 10,10,1234567,  1000,1000,4)
ibis-list graph gr1dim=3 nr=9

!Test the various formatting options
xyzpic graph image nl=8 ns=8
list image

xyzpic graph image nl=9 ns=9 format=half
list image

xyzpic graph image nl=10 ns=10 format=full
list image

xyzpic graph image nl=10 ns=10 format=real
list image

end-proc
$ Return
$!#############################################################################
