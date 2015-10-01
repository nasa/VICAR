$!****************************************************************************
$!
$! Build proc for MIPL module polyclip
$! VPACK Version 1.8, Friday, December 23, 1994, 13:19:03
$!
$! Execute by entering:		$ @polyclip
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
$ write sys$output "*** module polyclip ***"
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
$ write sys$output "Invalid argument given to polyclip.com file -- ", primary
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
$   if F$SEARCH("polyclip.imake") .nes. ""
$   then
$      vimake polyclip
$      purge polyclip.bld
$   else
$      if F$SEARCH("polyclip.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polyclip
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polyclip.bld "STD"
$   else
$      @polyclip.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polyclip.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polyclip.com -
	-s polyclip.f -
	-i polyclip.imake -
	-p polyclip.pdf -
	-t tstpolyclip.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polyclip.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'

	subroutine main44

c
c	This program takes an IBIS graphics file as input
c	and clips the coordinates into a user specified window
c	and outputs the clipped coordinates to a newly created
c	IBIS graphics file. It should be noted that a coordinate
c	pair of ( 0, 0 ) is interpreted as the end of a line
c	string. The variable names suggest an x y coordinate system
c	but actually, y is the line dimension and x is the sample dimension.
c
c
c	Original Programmer: Leo Bynum		December 1985
c
c	Revision A:	Fixed line string formatting algorithm,
c			Fixed graphics file handling,
c			Change window parmeter names, 
c			Improved help file.
c		Frank Evans	February 1986
c
c	Revision B:	Put in calls to standard graphics 1 subroutines
c		Frank Evans	February 1986
c
c	Revision C:	Put in skip parameter
c		Frank Evans	April 1986
c
c       Revision D:     MSTP S/W CONVERSION (VICAR PORTING)
c               A. Scop (CRI)   January 1995
	
	implicit none

	integer*4 status, rdgr, wrgr, getgr, clgr
	integer*4 whocares
	integer*4 i				! loop counter
	integer*4 skip				! number of extra word to skip
	real*4 xmin, xmax, ymin, ymax		! the window limits
	real*4 x1, y1, x2, y2			! points defining line segments
 	real*4 extra1(40), extra2(40), zeros(40) ! the extra data to be carried
	real*4 prev_x, prev_y			! contains the last pair output
	real*4 org_x, org_y			! origin to subtract if reorigin
	logical eof, eol			! end of file and end of line
	logical	zero				! dummy
	logical inside				! used for "clipping" a point
	logical	reorigin, xvptst		! flag to reorigin


	common / window / xmin, xmax, ymin, ymax
	common / dmp / prev_x, prev_y
        common / dmp2 /  reorigin, org_x, org_y
	common / drw/ extra1, extra2, zeros

        call ifmessage ('POLYCLIP version 2-JAN-95')

	call xvp ('SKIP', skip, whocares)

c		open the input graphics 1 file
	status = rdgr (1, 1, skip+2)
        if (status .ne. 1) call signalgr (1,status,1)

c		open the output graphics 2 file
	status = wrgr (1, 2, skip+2)
        if (status .ne. 1) call signalgr (1,status,1)



c		get the window parameters
	call xvp( 'MINLINE', ymin, whocares )
	call xvp( 'MAXLINE', ymax, whocares )
	call xvp( 'MINSAMP', xmin, whocares )
	call xvp( 'MAXSAMP', xmax, whocares )


	reorigin = xvptst ('REORIGIN')

	if (reorigin) then
	    org_x = xmin - 1
	    org_y = ymin - 1
	endif


c		The double zero line string terminators are output
c		    at the beginning of a linestring (except for the
c		    first one) instead of at the end as is typically done.
c		    This is done because we don't know if we're at the
c		    end of a linestring until we get the next segment.

	prev_x = 0.0		! don't want (0.0,0.0) output first time
	prev_y = 0.0



c	    Main loop through input graphics file


   10   continue

c		scan for the beginning of a line string
	x1 = 0.0
	y1 = 0.0
	do while (x1 .eq. 0.0 .and. y1 .eq. 0.0)
	    status = getgr (1, zero, eof, y1, x1, extra1)
            if (status .ne. 1) call signalgr (1,status,1)
	    if ( eof ) go to 30
	enddo


	status = getgr (1, zero, eof, y2, x2, extra2 )	! get the next pair
        if (status .ne. 1) call signalgr (1,status,1)
	eol = (x2 .eq. 0.0 .and. y2 .eq. 0.0)
	if ( eof )   eol = .true.		! if eof then this is a point

	
	if ( eol ) then				! is this a point or a
							! line segment ?
	    inside = .true.				! find out if the point
	    if ( x1 .lt. xmin ) inside = .false.	! is inside or outside
	    if ( x1 .gt. xmax ) inside = .false.
	    if ( y1 .lt. ymin ) inside = .false.
	    if ( y1 .gt. ymax ) inside = .false.
	    if ( inside ) then				! if inside, output it
		if (prev_x .ne. 0.0 .or. prev_y .ne. 0.0) 
     +				call dump (0.0, 0.0, zeros)
	        call dump (y1, x1, extra1)
	    endif
	    go to 10					! get next element
	endif


c		if we got this far we have a line string and not a point

	do while (.not. eol)
					    ! clipper outputs the line segment
	    call clipper( x1, y1, x2, y2 )
					    !   that remains (if any) after 
					    !   clipping.  It does not change
					    !   (x1,y1, x2,y2).
	    x1 = x2				! old (x2,y2) is new (x1,y1)
	    y1 = y2				! get next pair and go back 
	    do i = 1, skip
		extra1(i) = extra2(i)
	    enddo
	    status = getgr (1, zero, eof, y2, x2, extra2) ! to clip or  
            if (status .ne. 1) call signalgr (1,status,1) ! terminate if
	    eol = (x2 .eq. 0.0 .and. y2 .eq. 0.0)
	    if ( eof ) go to 30			       ! that element is done
	enddo

	go to 10



   30	continue
	call dump (0.0, 0.0, zeros)

	status = clgr (1)
        if (status .ne. 1) call signalgr (1,status,1)
	status = clgr (2)
        if (status .ne. 1) call signalgr (1,status,1)

	return
	end





	subroutine clipper( a1, b1, a2, b2 )
c
c	This routine is a fortran version of the Cohen-Sutherland
c	algorithm for clipping. A complete explanation of the 
c	workings of the algorithm plus the Pascal code can be
c	found in the book "Fundamentals of Interactive Computer
c	Graphics" by J. D. Foley and A. Van Dam ( 1982 ).
c
c
	implicit none
	logical outcode1(4),  outcode2(4), accept, reject, done
	logical reject_check, accept_check, swapped
	real*4  x1, x2, y1, y2
	real*4  a1, a2, b1, b2
	real*4  xmin, xmax, ymin, ymax

	common / window / xmin, xmax, ymin, ymax


	x1 = a1
	x2 = a2
	y1 = b1
	y2 = b2

	accept = .false.
	reject = .false.
	done = .false.
	swapped = .false.

	do while( .not. done )
	  call outcodes( x1, y1, outcode1 )
	  call outcodes( x2, y2, outcode2 )
	  reject = reject_check ( outcode1, outcode2 )
	  if ( reject ) then
	    done = .true.
	  else
	    accept = accept_check( outcode1, outcode2 )
	    if ( accept ) then
	      done = .true.
	    else
	      if (.not.( outcode1(1) .or. outcode1(2) .or. outcode1(3)        
     &		    .or. outcode1(4) ) ) then
		call swap( x1, x2, y1, y2, outcode1, outcode2 )
		swapped = .not. swapped
	      endif

	      if ( outcode1(1) ) then
		x1 = x1 + ( x2 - x1 ) * ( ymax - y1 ) / ( y2 - y1 )
		y1 = ymax
	      else if ( outcode1(2) ) then
		x1 = x1 + ( x2 - x1 ) * ( ymin - y1 ) / ( y2 - y1 )
		y1 = ymin
	      else if ( outcode1(3) ) then
		y1 = y1 + ( y2 - y1 ) * ( xmax - x1 ) / ( x2 - x1 )
		x1 = xmax
	      else if ( outcode1(4) ) then
		y1 = y1 + ( y2 - y1 ) * ( xmin - x1 ) / ( x2 - x1 )
		x1 = xmin
	      endif
	    endif
	  endif
	enddo ! while

	if ( accept ) then
	    if (swapped) then
		call draw (x2, y2, x1, y1)
	    else
		call draw (x1, y1, x2, y2)
	    endif
	endif

	return
	end
 


	subroutine outcodes( x, y, outcode )
	implicit none
	real*4 x, y, xmin, xmax, ymin, ymax
	logical outcode(4)

	common /window/ xmin, xmax, ymin, ymax

	if ( x .lt. xmin ) then 
	  outcode(4) = .true.
	else
	  outcode(4) = .false.
	endif

	if ( x .gt. xmax ) then
	  outcode(3) = .true.
	else
	  outcode(3) = .false.
	endif

	if ( y .lt. ymin ) then 
	  outcode(2) = .true.
	else
	  outcode(2) = .false.
	endif

	if ( y .gt. ymax ) then
	  outcode(1) = .true.
	else
	  outcode(1) = .false.
	endif

	return
	end



	logical function reject_check( outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4)

	reject_check = .false.
	if      ( outcode1(1) .and. outcode2(1) ) then
	  reject_check = .true.
	else if ( outcode1(2) .and. outcode2(2) ) then
	  reject_check = .true.
	else if ( outcode1(3) .and. outcode2(3) ) then
	  reject_check = .true.
	else if ( outcode1(4) .and. outcode2(4) ) then
	  reject_check = .true.
	endif

	return
	end



	logical function accept_check( outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4)
	integer*4 i

	accept_check = .true.
	do i = 1, 4
	  if ( outcode1(i) .or. outcode2(i) ) then
	    accept_check = .false.
	  endif
	enddo

	return
	end



	subroutine swap( x1, x2, y1, y2, outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4), ltemp
	real*4  x1, y1, x2, y2, temp
	integer i

	temp = x1
	x1 = x2
	x2 = temp
	temp = y1
	y1 = y2
	y2 = temp
	do i = 1, 4
	  ltemp = outcode1(i)
	  outcode1(i) = outcode2(i)
	  outcode2(i) = ltemp
	enddo
	return
	end



	subroutine draw( x1, y1, x2, y2)
c	    draw outputs a line segment to the graphics file
c		If this line segment starts in a different place
c		than the last one ended then it puts in a double zero
c		unless the previous coordinate was a double zero.
	implicit none
	real*4 x1, y1, x2, y2, prev_x, prev_y
	real	extra1(40), extra2(40), zeros(40)
	common / dmp / prev_x, prev_y
	common / drw/ extra1, extra2, zeros

	if ( x1 .ne. prev_x .or. y1 .ne. prev_y ) then
	    if (prev_x .ne. 0.0 .or. prev_y .ne. 0.0) then
		call dump (0.0, 0.0, zeros)
	    endif
	    call dump (y1, x1, extra1)
	endif

	call dump (y2, x2, extra2)

	return 
	end




	subroutine dump ( y, x, extra )
c		dump outputs the coordinate pair to the graphics file
c		  reoriginating if necessary
	implicit none
        integer putgr, status
	real*4 x, y,  prev_x, prev_y, org_x, org_y, extra(40)
	logical	reorigin
	common / dmp / prev_x, prev_y
        common / dmp2 / reorigin, org_x, org_y

	prev_x = x
	prev_y = y
	if (.not. reorigin .or. (x .eq. 0.0 .and. y .eq. 0.0) ) then
	    status = putgr (2, y, x, extra)
            if (status .ne. 1) call signalgr (1,status,1)
	else
	    status = putgr (2, y - org_y, x - org_x, extra)
            if (status .ne. 1) call signalgr (1,status,1)
	endif

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polyclip.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polyclip

   To Create the build file give the command:

		$ vimake polyclip			(VMS)
   or
		% vimake polyclip			(Unix)


************************************************************************/


#define PROGRAM	polyclip
#define R2LIB

#define MODULE_LIST polyclip.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polyclip.pdf
process help=*
parm INP	type=string 
parm OUT	type=string 
parm MINLINE	type=real 
parm MAXLINE	type=real 
parm MINSAMP	type=real 
parm MAXSAMP	type=real 
parm REORIGIN   type=keyword count=0:1 valid=(reorigin,--) default=--
parm SKIP	type=integer valid=(0:38) default=0
end-proc
.TITLE
VICAR/IBIS Program "polyclip"
.HELP
PURPOSE

"polyclip" takes an IBIS graphics-1 file as input and clips the graphics
elements with a specified window. The new clipped data is output into
another graphics file.


EXECUTION

polyclip INPUT.GRA OUTPUT.GRA   MINLINE=901.5 MINSAMP=100.0 +
				MAXLINE=1000.0 MAXSAMP=333.3

The IBIS graphics-1 file INPUT.GRA is clipped according to the window
coordinates (MINLINE,MAXLINE,MINSAMP,MAXSAMP) and output to OUTPUT.GRA .


polyclip INPUT.GRA OUTPUT.GRA   MINLINE=901.5 MINSAMP=100.0 +
				MAXLINE=1000.0 MAXSAMP=333.3  'REORIGIN

In this example the output graphics file is reoriginated so that the
output origin is (line,sample)=(1,1) , i.e. (MINLINE-1,MINSAMP-1) is 
subtracted from all of the coordinate.


polyclip INPUT.GRA OUTPUT.GRA   SKIP=3 MINLINE=1 MINSAMP=1 +
				MAXLINE=1056 MAXSAMP=1204

In this example, the input IBIS graphics-1 file has five "columns" of
which the first two are the coordinate values and the last three
are skipped.  The skipped data is just carried along and does not effect
the clipping on the other two coordinates.  This is used for carrying
along identification information so it can be determined which polygons
were clipped.



Original Programmer:  Leo Bynum		December 1985

Cognizant Programmer:  Frank Evans	April 1986

Revisions
Made portable for UNIX   A. Scop (CRI)  January 1995

.LEVEL1
.VARIABLE INP
IBIS graphics-1 file to
be clipped ( input ).
.VARIABLE OUT
IBIS graphics-1 file to
be created.
.VARIABLE MINLINE
Minimum line coordinate.
Defines the top of the window.
.VARIABLE MAXLINE
Maximum line coordinate.
Defines the bottom of the window.
.VARIABLE MINSAMP
Minimum sample coordinate.
Defines the left side of the window.
.VARIABLE MAXSAMP
Maximum sample coordinate.
Defines the right side of the window.
.VARIABLE REORIGIN
Keyword to reorigin the
output data
.VARIABLE SKIP
The number of nominal data
values to skip over.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolyclip.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

! Generate a standard graphics file
ibis-gen in.gra nc=4 nr=12 datacol=(1,2) 'ibis-1 'row +
  data=(1,1,3,3,5,5,100,100, 6,7,8,8,0,0, 6,100,5,5,6,6,0,0)
ibis-list in.gra gr1dim=4 nr=4

!The IBIS graphics-1 file INPUT.GRA is clipped according to the window
!coordinates (MINLINE,MAXLINE,MINSAMP,MAXSAMP) and output to OUTPUT.GRA 
!The 'SKIP' parameter is the graphics dimension minus 2.

polyclip in.gra out.gra   MINLINE=1.5 MINSAMP=1.0 +
				MAXLINE=99.0 MAXSAMP=300 SKIP=2
ibis-list out.gra gr1dim=4 nr=4

!In this example the output graphics file is reoriginated so that the
!output origin is (line,sample)=(1,1) , i.e. (MINLINE-1,MINSAMP-1) is 
!subtracted from all of the coordinate.

polyclip in.gra out.gra   MINLINE=1.5 MINSAMP=1.0 +
			MAXLINE=99.0 MAXSAMP=300 SKIP=2 'reorigin
ibis-list out.gra gr1dim=4 nr=4

end-proc

$ Return
$!#############################################################################
