
	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE

! This is a program to test the IBIS graphics-1 subroutines, with 
! subcommands used to specify the particular test to run.
!
! Available subcommands:  -CONVERT   -PRINT   -BIGCOPY -UPDATE
!

	integer count
	character*80 message
	character*20 subcommand
	character*1 cmdchar

! Get the sub command
	call xvp ('_SUBCMD', subcommand, count)
	write (message, 5) subcommand
 5	format (' Subcommand ',A)
	call xvmessage (message, ' ')

!  Now do it	
	cmdchar = subcommand(1:1)
	if (cmdchar.eq.'C') then
		call convert_file
	else if (cmdchar.eq.'P') then
		call print_file
	else if (cmdchar.eq.'B') then
		call copy_files
	else if (cmdchar.eq.'U') then
		call update_file
	endif
	
 	return
	end  ! tibisgr

c------------------------------------------------------------------------
!
!    1.	-CONVERT: Converts the input IBIS graphics-1 file in (x,y) format into a 
!	(line,sample) format for output.
!
!       REPEAT=5  causes a repeat of the first four coordinate
!	sets in the file. For example, given the following graphics file:
!	    a, b, c, d, e, f
!	where a,...,f are each coordinate sets, test 2 will produce the
!	following output coordinate sets:
!	    a, b, c, d, a, b, c, d, e, f
!	If keyword 'WRITE is supplied, then the SETGR call for the
!	repeat is applied to the write file rather than to the read file;
!	the repeat should have no effect--the output file should match 
!	the input file.
!       RSTART=3  causes the 3rd and 4th coordinate sets 
!	to be repeated. Using the same input file as shown above, test #3
!	will produce:
!	    a, b, c, d, c, d, e, f
!	If keyword 'WRITE is supplied, then the SETGR call for the
!	repeat is applied to the write file rather than to the read file;
!	the repeat should have no effect--the output file should match 
!	the input file.
	
	subroutine convert_file
	implicit none
	real x, y, extra_coords(8)
	logical zero, eof, set_write, xvptst
	integer count,repeat_trigger,repeat_start
	integer num_dimen
	integer maxY
	parameter (maxY = 50)
	
	call xvp('REPEAT',repeat_trigger,count)
	call xvp('RSTART',repeat_start,count)
 	call xvp ('DIMEN', num_dimen, count)

! Open and create the files
  	call rdgr (1, 1, num_dimen)
	call wrgr (1, 2, num_dimen)

	set_write = xvptst ('WRITE')

!     Copy the coordinate sets, repeating some coordinates if the count 
!     equals repeat_trigger
	count = 1
	eof = .false.
	do while (.not. eof)
	    call nextgr (1, eof, x, y,extra_coords) ! get the start of the linestring
	    zero = .false.
	    do while (.not. zero .and. .not. eof) ! read until end of linestring
		call putgr (2, maxY-y, x+1,extra_coords)
		call getgr (1, zero, eof, x, y,extra_coords)

!	  Check for repeat of coordinates
		count = count + 1
		if (count .eq. repeat_trigger) then
		    if (set_write) then
			call setgr (2, repeat_start)
		    else
			call setgr (1, repeat_start)
		    endif
		endif
	    enddo
	    if (zero) call putgr (2, 0, 0,extra_coords)
	enddo

	call clgr (1)
	call clgr (2)
	
	return
	end
	
c------------------------------------------------------------------------
!    2.	-PRINT Prints a coordinate from a graphics file. The coordinate is specified
!	via the COORD parameter. The dimensions of the graphics file are
!	specified via the DIMEN parameter, which defaults to 2. (A maximum of
!	8 coordinates may be in a set.) The EXTRA option is used to 
!	specify how many additional coordinate sets are to be displayed 
!	following the specified coordinate set; it defaults to zero.
!


	subroutine print_file
	implicit none
	character*80 message
	real x, y, extra_coords(8)
	integer count
	integer num_dimen,k,coord_num,extra,i
	logical zero, eof
	
 	call xvp ('DIMEN', num_dimen, count)
	
	call rdgr (1, 1, num_dimen)

	call xvp ('COORD', coord_num, count)
	call setgr (1, coord_num)
	call getgr (1, zero, eof, x, y, extra_coords)

	write (message, 40) coord_num, x, y, 
     +            (extra_coords(k),k=1,num_dimen-2)
 40	format (' Coordinate #',I3,': ',8F7.2)
	call xvmessage (message,' ')

! Do we need to print some trailing coordinate sets?
	call xvp ('EXTRA', extra, count)
	do i = 1, extra
	    call getgr (1, zero, eof, x, y, extra_coords)
	    if (eof) go to 460
	    write (message, 40) coord_num+i, x, y, 
     +		(extra_coords(k),k=1,num_dimen-2)
	    call xvmessage (message,' ')
	enddo

 460 	call clgr (1)
	
	return
	end

c------------------------------------------------------------------------
!    3.	-BIGCOPY Write the first coordinate set from the 9 odd-numbered input files
!	to output file #1, and the first coordinate set from the 9 even-
!	numbered input files to output file #2. This test tries out the
!	maximum of 20 graphics files.

	subroutine copy_files
	implicit none
	real x, y
	integer i
	logical zero, eof
	

! use lots of 2-D input files
 	do i = 1, 18
	    call rdgr (i, i, 2)
	end do
	call wrgr (1, 19, 2)
	call wrgr (2, 20, 2)

! Write the first coordinate in the odd-numbered input files to the first
! output file
	do i = 1, 17, 2
	    call getgr (i, zero, eof, x, y,0)
	    call putgr (19, x, y,0)
	end do

! Write the first coordinate in the even-numbered input files to the second
! output file
	do i = 2, 18, 2
	    call getgr (i, zero, eof, x, y,0)
	    call putgr (20, x, y,0)
	end do

	do i = 1, 20
	    call clgr (i)
	end do
	
	return
	end

