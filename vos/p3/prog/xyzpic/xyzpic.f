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
	
