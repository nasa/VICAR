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
