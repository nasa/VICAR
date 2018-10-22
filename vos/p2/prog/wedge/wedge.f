      include 'VICMAIN_FOR'
      subroutine main44
	implicit none
	integer*4 outunit,ind,cnt,count,def,nl,ns,sl,ss
	integer*4 i,j,dnmin,dnmax,x0,y0,quad
	integer*4 size(4),row1(2048),row2(2048)
	real*4 stangle,arc,ang,ang2,tangarc,c,s,t,t2,opp,opp2
	real*4 limit
	character*10 reflect
	byte dn(2048)
c
c	initialze variables
c
      sl=1
      ss=1
      dnmax=255
      dnmin=0
      stangle=0.
      arc=0.
      x0=0.
      y0=0.
	do i=1,2048
	    row1(i)=0
            row2(i)=0
	enddo
      call xvmessage('WEDGE version 2016-06-09',' ')
c      call xvsize(sl,ss,nl,ns,nli,nsi)
	call xvparm ('SIZE',size,count,def,4)
c	print *, 'def = ',def
	if (size(3).eq.0) then
	    call xvp ('NL',nl,cnt)
            call xvp ('NS',ns,cnt)
	else
	    nl=size(3)
	    ns=size(4)
        end if
	if (nl.gt.2048) then
		call xvmessage ('??E - Number of lines > 2048',' ')
		call abend
	endif
	if (ns.gt.2048) then
                call xvmessage ('??E - Number of samples > 2048',' ')     
                call abend
        endif
	
c	print *, 'nl = ns = ',nl, ns
	call xvunit(outunit,'OUT',1,ind,' ')
	call xvopen(outunit,ind,'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
	call xvp('DNMAX',dnmax,cnt)
	call xvp('DNMIN',dnmin,cnt)
	call xvp('STANGLE',stangle,cnt)
	call xvp('ARC',arc,cnt)
	call xvp('X0',x0,cnt)
	if (cnt.eq.0) x0=ns/2
	call xvp('Y0',y0,cnt)
	if (cnt.eq.0) y0=nl/2
	reflect='reflect'
	call xvp('BOWTIE',reflect,cnt)
	if (reflect .eq.'bowtie') then
		call xvmessage ('??E - Bowtie opiton has not been implemented yet',' ')
		call abend		
	endif
c	print *, 'reflect cnt = ',cnt
c	print *, 'reflect = ',reflect
c	print *, 'stangle, arc =', stangle, arc
c	print *, 'x0, y0 = ',x0,y0

	tangarc = stangle+arc
	limit = stangle+180.
	if ( tangarc.gt.limit) then
		call xvmessage ('??E - Quad1 - The sum of stangle and arc cannot be > 180',' ')
		call abend
	endif
	ang=stangle*3.14159/180.
	ang2=arc*3.14159/180.
c	ang3=(stangle+arc-90)*3.14159/180.
c	ang4=-stangle*3.14159/180.
c	print *, 'ang (radians) = ',ang
	c=cos(ang)
	s=sin(ang)
	t=tan(ang)
	t2=tan(ang+ang2)
c	t3=tan(ang3)
c	t4=tan(ang4)
	opp = y0*t
	opp2 = y0*t2
c	print *,'opp = ',opp
c	print *,'opp + arc = ',opp2,tangarc

	quad=0
c	Quadrants 1,1
	if (stangle.le.90. .and. tangarc.le.90.) then
	    quad=11		!(1,1)
	    do i=1,y0
		if (tangarc.gt.stangle) then
		   row1(i) = int((y0 - i )*t + 0.5) + y0
		   row2(i) = int((y0 - i )*t2 + 0.5) + y0
c		   print *, 'row1, row2 = ',i,row1(i),row2(i)
		else
                   row2(i) = int((y0 - i )*t + 0.5) + y0
                   row1(i) = int((y0 - i )*t2 + 0.5) + y0
c                   print *, 'row1, row2 = ',i,row1(i),row2(i)
		endif
	    enddo
c	Quadrants 1,2
	elseif (stangle.le.90. .and. tangarc.ge.90. .and. tangarc.le.180.) then
	    quad=12		!(1,2)
	    do i=1,y0
                   row1(i) = int((y0 - i )*t + 0.5) + y0
c                   print *, '!> row1 = ',i,row1(i)
	    enddo
	    do i=x0,ns
                   row2(i) = int((x0 - i )*t2 + 0.5) + x0
c                   print *, '!> row2 = ',i,row2(i)
            enddo
c 	Quadrants 1,3 - nothing greater than 180
	elseif (stangle.le.90. .and. tangarc.ge.180.) then
	    quad=13		!(1,3)
            do i=1,y0
                   row1(i) = int((y0 - i )*t + 0.5) + y0
c                   print *, '#> row1 = ',i,row1(i)
            enddo
  	    do i=y0,nl
                   row2(i) = int((y0 - i )*t2 + 0.5) + y0
c                   print *, '#> row2 = ',i,row2(i)
            enddo
c       Quadrants 2,2 - 
        elseif (stangle.ge.90. .and. stangle.le.180. .and.tangarc.ge.90. .and. tangarc.le.180.) then
            quad=22             !(2,2)
	    do i=y0,nl
                   row1(i) = int((y0 - i )*t + 0.5) + y0
c                   print *, '@> row1 = ',i,row1(i)
            enddo
            do i=x0,ns
                   row2(i) = int((x0 - i )*t2 + 0.5) + x0
c                   print *, '@> row2 = ',i,row2(i)
            enddo
c       Quadrants 2,3
        elseif (stangle.ge.90. .and. stangle.le.180. .and.tangarc.ge.180. .and. tangarc.le.270.) then
	    quad=23
            do i=y0,nl
                   row1(i) = int((y0 - i )*t + 0.5) + y0
c                print *,'*> y0,t = ',y0,t,t2
c                   print *, '*> row1 = ',i,row1(i)
            enddo
            do i=y0,nl
                   row2(i) = int((y0 - i )*t2 + 0.5) + y0
c                   print *, '*> row2 = ',i,row2(i)
            enddo
        elseif (stangle.ge.90. .and. stangle.le.180. .and.tangarc.ge.270. ) then
            quad=24

c		print *, 'Quad24'
            do i=y0,nl
                   row1(i) = int((y0 - i )*t + 0.5) + y0
c                print *,'%> y0,t = ',y0,t,t2
c                   print *, '%> row1 = ',i,row1(i)
            enddo
            do i=1,y0
                   row2(i) = int((y0 - i )*t2 + 0.5) + y0
c                   print *, '%> row2 = ',i,row2(i)
            enddo


	endif
c	if (tang.le.90) then
	    do i=1,nl
            	do j=1,ns
		    dn(j) = dnmin	
		    if (quad.eq.11) then
c			print *, 'i,j, quad = ',i,j, quad,row1(i),row2(i)
			if (j.ge.row1(i) .and. j.le.row2(i)) dn(j)=dnmax
		    elseif (quad.eq.12) then
c		        print *, 'i,j, quad = ',i,j, quad
		        if (j.ge.row1(i) .and. i.le.y0) dn(j)=dnmax 
			if (j.ge.row2(i) .and. i.ge.y0) dn(j)=dnmax
		    elseif (quad.eq.13) then
c	                print *, 'i,j, quad = ',i,j, quad
			if (j.ge.row1(i) .and. i.le.y0) dn(j)=dnmax
			if (j.ge.row2(i) .and. i.ge.y0) dn(j)=dnmax
		    elseif (quad.eq.22) then
c			print *, 'i,j, quad = ',i,j, quad
			if (i.ge.y0 .and. j.ge.row2(i) .and. j.le.row1(i)) dn(j)=dnmax
		    elseif (quad.eq.23) then
c			print *, 'i,j, quad = ',i,j, quad
			if (i.ge.y0 .and. j.ge.row2(i) .and. j.le.row1(i)) dn(j)=dnmax
                    elseif (quad.eq.24) then
c			print *, 'i,j, quad = ',i,j, quad
			if (j.le.row2(i)) dn(j)=dnmax
			if (i.ge.y0 .and. j.le.row1(i)) dn(j)=dnmax
		    else
			dn(j)=dnmin
		    endif

	        enddo
	        call xvwrit(outunit,dn,ind,' ')
	    enddo
c	else
c	   do i=1,nl
c                do j=1,ns
c                    dn(j) = dnmin
c                    if (i.le.y0) then
c                       print *, 'j, row1(i), row2(i) = ',j, row1(i), row2(i)
c                        if (j.ge.row2(i).and.j.le.row1(i)) dn(j)=dnmax
cc                    endif
c                enddo
c                call xvwrit(outunit,dn,ind,' ')
c          enddo
c	endif
	call xvclose(outunit,ind,' ' )
	return
	end	
