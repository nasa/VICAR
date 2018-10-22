	include 'VICMAIN_FOR'

	subroutine main44

c
c	"pltgraf" plots an IBIS graphics-1 file using Calcomp calls.
c	Axes with labels and a title may be plotted if desired.
c	Three dimensional graphics-1 files can also be plotted.
c	The direction the axes increase can be any of the four
c	corners, so line,sample or lat,long can be plotted.  Note,
c	however, that the first value of the coordinate pair is 
c	plotted in the y direction (i.e. up-down).
c
c
c	Much of the code is taken from POLYCLIP written by L. Bynum
c
c
c	Programmer	Frank Evans
c
c	Revision	New		November 1985
c
c	Revision	A		
c				Put in calls to standard IBIS
c				  graphics-1 file subroutines.
c				Added 3-D option.
c				Frank Evans	February 1986
c
c	Revision	B		
c				Allowed different coordinate systems
c				Modified 3-D option.
c				Frank Evans	March 1986
c	
c	Revision	C
c				Added interface attribute file
c				Frank Evans	June 1986
c	
c	Revision	D
c				Added dataform parameter
c				Frank Evans	January 1987
c	
c	Revision	E
c				Used call to SETGR to avoid opening graphics
c				file twice (done to reset file to its first
c				coordinate)
c				Michael Tschudi	June 1987
c	
c	Revision	F
c				 1. add PEN param
c				 2. add ZTOPEN param
c				 3. add COMMENT param
c				 4. add DATE param
c				 5. add FINALPOS param
c				 6. add NOBOX to the MODE param
c				 7. change formula for centering title from
c					xlen/2.-0.25*.67*(slength(title)/2.+2)
c				    to
c				 	(xlen-0.2490*slength(title))/2.0
c				 8. if xlabel specified along with 'NOAXIS or
c				    'NOBOX, then it is displayed without axis 
c				    numbers; same for ylabel
c				 9. change pen number calculation for scaled z
c				    from
c					pen = int(z)
c				    to
c					pen = nint(z)
c				    to avoid round-off error
c				10. fixed ZRANGE so that the range is checked
c				11. set plot to draw in same location 
c				    regardless of MODE switch value
c				12. fixed default FORMAT value
c				Michael Tschudi	September 17, 1987
c
c	REVISION	G
c				Check that interface file annotation is within
c				the plot area.  Also, center the annotation
c				and add TSIZE parameter.
c				Howard Frieden	 February 1990
c
c       REVISION        H       MSTP S/W CONVERSION (VICAR PORTING)
c                               CRI              July 1995
c
c       REVISION        I       corrected subroutine plottext
c				bam 5/98



	implicit none


	real*4 char_width_1_4, char_width_1_8
	integer*4 max_z_to_pen,idate
	parameter (
     +	    char_width_1_4 = 0.2490,	! avg. width of a 1/4" high character
!     +	    char_width_1_8 = 0.1296,	! avg. width of a 1/8" high character
     +	    char_width_1_8 = 0.06,	! avg. width of a 1/8" high character
     +	    max_z_to_pen = 20)		! max. size of z-to-pen table
					! (definition is repeated in DRAW)

	integer*4 unit,i,isize,jj,jst
	integer*4 status				! returns status from routines
	integer*4 dim	  		! number of dimensions in input file
c	integer*4 slength, slen			! pseudo string length function
	integer*4 count				! dummy count for xvp routine
	integer*4 xrandef, yrandef, zrandef	! default flags for ranges
	integer*4	dataformat			! code for data file format 
	integer*4 plotwid,plotht,dblzero
        integer*4 nxlabel,nylabel,ntitle,ntbl,ncomment
	integer*4 axplot,axisdef,nplotgpi,nplotgpi2,nplotname
        integer*4 nploteps,ninpfile,ni2file,nannot
        integer*4 ncolx(5),ncoly(5)

	real*4	xrange(2), yrange(2), zrange(2)	! window range input parameters
	real*4	xlen, ylen			! length of the axis
	real*4	xmin, xmax, ymin, ymax, zmin, zmax	! the window limits
c	real*4	x1, y1, z1, x2, y2, z2		! points defining line segments
	real*4	x, y, z				! coordinates
	real*4	prev_x, prev_y			! contains the last pair output
	real*4	xscale, xoffs, yscale, yoffs    ! scaling factors
	real*4	firstx, firsty, deltax, deltay	! for AXIS subroutine
        real*4  xrang1,xrang2, yrang1,yrang2,zrang1,zrang2
	real*4 tmpx, tmpy, tmpz
	real*4  annotx,annoty			!location for annotation

c	logical*4 inside				! used for "clipping" a point
	logical*4	eof, zero			! end of file and zero flags
	logical*4	it_all_fits			! flag indicates auto scaling
	logical*4 xvptst
	logical*4 epsplot,annotate

	integer*4 inpcount, rdgr,setgr,getgr
        integer*4 flag,fsize

	character*2  direction, dataform
	character*6 mode
	character*8 pformat
	character*20 annot
	character*80 inpfiles(2)
        character*256 plot_ds
	character*60 xlabel, ylabel
	character*120 title
	character*70 comment
        character*80 plotname,plotgpi,plotgpi2
        character*80  tbl,ploteps,i2file        
c
        character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/


	common / form / dataformat
	common / window / xmin, xmax, ymin, ymax
	common / prev / prev_x, prev_y, dim
	common / scaling / xscale, xoffs, yscale, yoffs, zmin, zmax




c---------------------------------------------------------------------
c	Start of executable code
c---------------------------------------------------------------------

        call IFMESSAGE('PLTGRAF version 13-Jul-2013 (64-bit) - rjb')

	comment = ' '
        plot_ds = ' '

	epsplot = .false.
	nplotgpi = 0
	nplotgpi2 = 0
	xmax = 0
	xmin = 0
	ymin = 0
	ymax = 0
	xrang1 = 0
	xrang2 = 0
	yrang1 = 0
	yrang2 = 0
	it_all_fits = .false.
	axisdef = 0
        call xvp( 'XLEN', xlen, count )                 !in inches
        plotwid = 100*xlen
        call xvp( 'YLEN', ylen, count )
        plotht = 100*ylen
c       call xvp( 'NUMPENS', numpens, count )
        call xvp( 'DIRECT', direction, count)
c                       calculate the scaling factors
        if (direction(1:1) .eq. 'T' .or. direction(1:1) .eq. 't') then
	    direction(1:1) = 'T'
            yscale = ylen/(ymax-ymin)
            yoffs = -yscale*ymin
            firsty = ymin
        else
	    direction(1:1) = 'B'
            yscale = -ylen/(ymax-ymin)
            yoffs = ylen - yscale*ymin
            firsty = ymax
        endif
        if (direction(2:2) .eq. 'R' .or. direction(2:2) .eq. 'r') then
	    direction(2:2) = 'R'
            xscale = xlen/(xmax-xmin)
            xoffs = -xscale*xmin
            firstx = xmin
        else
	    direction(2:2) = 'L'
            xscale = -xlen/(xmax-xmin)
            xoffs = xlen - xscale*xmin
            firstx = xmax
        endif
        deltay = 1.0/yscale
        deltax = 1.0/xscale

	dataformat = 1
        call xvp ('DATAFORM', dataform, count)
        if (dataform(1:2) .eq. 'XY' .or. dataform(1:2) .eq. 'xy') then
            dataform(1:2) = 'XY'
	    dataformat = 1
        else 
            dataform(1:2) = 'YX'
	    dataformat = 2
        endif

c	3-d eliminated earlier - use plot3d instead
	call xvp ('DIM', dim, count)		!dimension of IBIS graphics-1 file, 2 or 3
	
c	    get input parameters

	call xvparm( 'XRANGE', xrange, count, xrandef, 2 )
	call xvparm( 'YRANGE', yrange, count, yrandef, 2 )
	call xvparm( 'ZRANGE', zrange, count, zrandef, 2 )

	xrang1 = xrange(1)
	xrang2 = xrange(2)
        yrang1 = yrange(1)
        yrang2 = yrange(2)
        zrang1 = zrange(1)
        zrang2 = zrange(2)

        call xvp ('INP', inpfiles, inpcount)

          ninpfile=index(inpfiles(1),'   ') - 1
        if (inpcount .gt. 1) then
            i2file=inpfiles(2)
            ni2file = index(i2file,'     ') - 1
	endif

        CALL XVP('PLOT',PLOT_DS,COUNT)
        if(count.gt.0) then
            plotname = PLOT_DS
            nplotname=index(plotname,'   ') - 1
            plotgpi=plotname(1:nplotname)//gpi
            nplotgpi=index(plotgpi,'  ') - 1
            plotgpi2=plotname(1:nplotname)//eps//gpi
            nplotgpi2=index(plotgpi2,'  ') - 1
            ploteps=plotname(1:nplotname)//eps
            nploteps=index(ploteps,'  ') - 1
            tbl = plotname(1:nplotname)//asc
            ntbl = index(tbl,'  ') - 1
        else
            tbl = 'plotdata.asc'
            ntbl = index(tbl,'     ') - 1
        ENDIF

       call xvp ('PLOTFMT',pformat,count)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.

c	    open input graphics-1 file
	STATUS = RDGR (1, 1, dim)
        if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
c	print *,'xrandef, yrandef, zrandef, dim = ',xrandef, yrandef,zrandef,dim
c		if either range is defaulted then find the actual range
	if (xrandef .eq. 1.0 .or. yrandef .eq. 1.0
     +		.or.  (dim .eq. 3 .and. zrandef .eq. 1.0) ) then
	    xmin = +1.0e30
	    xmax = -1.0e30
	    ymin = +1.0e30
	    ymax = -1.0e30
	    zmin = +1.0e30
	    zmax = -1.0e30
c   open asc file for  
        OPEN(13,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
        i = 0
        dblzero = 0
	    eof = .false.
	    do while (.not. eof)		! go thru file until eof
		STATUS = GETGR (1, zero, eof, x, y, z)
                if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
                if (.not. eof) then
                    i = i + 1
                    if (zero) then
                        dblzero = dblzero + 1

c                        print *,'dblzero = ',dblzero
                        if (dblzero .ge. 2) go to 100
c                        print *, 'zero   i = ',i
                        write(13,10090)
10090 format (1x,' ')
                    else
                        dblzero =  0
c                        print *,'i,input x,y,z =',i,x,y,z

cc		if (.not. zero .and. .not. eof) then
		    call format (x, y, z)
		    xmin = min( xmin, x)
		    xmax = max( xmax, x)
		    ymin = min( ymin, y)
		    ymax = max( ymax, y)
		    zmin = min( zmin, z)
		    zmax = max( zmax, z)
                        write (13,10100)  x, y, z
10100 format (3f10.2)
	            endif
		endif
	    enddo
100     continue

	    STATUS = SETGR (1, 1)	! reset the graphics file to its start
            if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
C	TYPE *,XMIN,XMAX,YMIN,YMAX
	close(13)
        xrang2 = xmax*1.5
	tmpx = xrang2 - xmax
	xrang1 = xmin - tmpx 
        yrang2 = ymax*1.5
	tmpy = yrang2 - ymax
	yrang1 = ymin - tmpy
        zrang2 = zmax*1.5
	tmpz = zrang2 - zmax
	zrang1 = zmin - tmpz

c	if (dataformat .eq. 2) then
c	    tmpx = yrang1
c	    tmpy = yrang2
c	    xrang1 = yrang1
c	    xrang2 = yrang2
c	    yrang1 = tmpx
c	    yrang2 = tmpy
c	endif
	else

        OPEN(13,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
        i = 0
        dblzero = 0
            eof = .false.
            do while (.not. eof)                ! go thru file until eof
                STATUS = GETGR (1, zero, eof, x, y, z)
                if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
                if (.not. eof) then
                    i = i + 1
                    if (zero) then
                        dblzero = dblzero + 1

c                        print *,'dblzero = ',dblzero
                        if (dblzero .ge. 2) go to 150
c                        print *, 'zero   i = ',i
                        write(13,10090)
                    else
                        dblzero =  0
c                        print *,'i,input x,y,z =',i,x,y,z

cc              if (.not. zero .and. .not. eof) then
                    call format (x, y, z)
                        write (13,10100)  x, y, z
                    endif
                endif
            enddo
150     continue
            STATUS = SETGR (1, 1)       ! reset the graphics file to its start
            if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
C       TYPE *,XMIN,XMAX,YMIN,YMAX
        close(13)



	endif


c		if one of the dimensions was not defaulted use the input range
	if (xrandef .eq. 0) then
	    xmin = xrange(1)
	    xmax = xrange(2)
	    xrang1 = xmin
            xrang2 = xmax
	endif
	if (yrandef .eq. 0) then
	    ymin = yrange(1)
	    ymax = yrange(2)
	    yrang1 = ymin
            yrang2 = ymax
	endif
	if (zrandef .eq. 0) then
	    zmin = zrange(1)
	    zmax = zrange(2)
	    zrang1 = zmin
            zrang2 = zmax
	endif
	if (xmin .eq. 0 .and. xmax .eq. 0) then
		call xvmessage('??E - xran cannot be (0,0)',' ')
		call ABEND
c	   call MABEND('xran cannot be (0,0)')
	endif
	if (xmin .eq. xmax) then
	   call MABEND('??E - xrange must vary...')
	endif
	if (ymin .eq. ymax) then
	   call MABEND('??E - yrange must vary...')
	endif

	if (ymin .eq. 0 .and. ymax .eq. 0) then
C		call xvmessage('yran cannot be (0,0)',' ')
C		call ABEND
	   call MABEND('??E - yran cannot be (0,0)')
	endif 

c		if both were defaulted then the whole file fits
	it_all_fits = (xrandef .eq. 1.0 .and. yrandef .eq. 1.0)
c
c	go back and get annotation
c
        annotate = .false.
        annotx = 0.0
        annoty = 0.0
        fsize = 10
        nannot = 0
        annot = ' '
        if (inpcount .gt. 1) then
            call plottext(xrang2,yrang2,annotate,annot,nannot,
     1 annotx,annoty,fsize)
c                print *,'annotate,annot,nannot,annotx,annoty,fsize = ',
cv     1 annotate,annot,nannot,annotx,annoty,fsize

        endif

c	if (dim .eq. 3) then
c     Get the z-to-pen table
c	    call xvp ('ZTOPEN', z_to_pen, num_z_to_pen)

c     If it isn't specified, set up for scaling of z value to find the pen
c	    if (num_z_to_pen .eq. 0) then
c		zscale = numpens/(zmax-zmin)
c		zoffs =  -zscale*zmin + 1.0
c	    endif
c	endif

c Get the various text strings we'll display
	call xvp( 'XLABEL', xlabel, count)
        nxlabel = index(xlabel,'    ') - 1

	call xvp( 'YLABEL', ylabel, count)
        nylabel = index(ylabel,'    ') - 1
	call xvp( 'TITLE', title, count)
        ntitle = index(title,'    ') - 1
	if (xvptst('DATE')) then
            flag=2
	    idate = 0
	    call datfmt (flag,comment,idate)
	    call xvp ('COMMENT', comment(19:70), count)
	else
	    flag = 3
	    call xvp ('COMMENT', comment, count)
	endif
	ncomment = index(comment,'    ') - 1
 
       axplot = 0	!mode = NOAXIS
        call xvparm ('MODE', mode, count, axisdef,1)
c        print *,'mode, axisdef = ',mode, axisdef
	if (mode .eq. 'AXIS') axplot = 1
c Get the basepen parameter--the basepen is the pen used for 2D plots
c and for 3D plots where the z-value is out of the range of the z-to-pen
c table
c	call xvp ('PEN', basepen, count)
c        print *,'inpfiles(1) = ',inpfiles(1)(1:ninpfile)
c	print *,'plotgpi = ',plotgpi(1:nplotgpi)
c	print *,'tbl = ',tbl(1:ntbl)
c	print *,'mode = ',mode
c	 print *,'direction = ', direction
c        print *,'title = ',title
c        print *,'dataformat = ',dataformat,' dataform = ',dataform(1:2)
c        print *,'axplot = ',axplot
c        print *,'xlabel = ',xlabel
c        print *,'ylabel = ',ylabel
c        print *,'plotwid = ',plotwid
c        print *,'plotht = ',plotht
c        print *,'xrang1,xrang2 = ',xrang1,xrang2
c        print *,'yrang1,yrang2 = ',yrang1,yrang2

	isize = 10
	unit = 98
        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
        call write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,title,ntitle,ploteps,nploteps,i2file,ni2file,
     2 isize,plotwid,plotht,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,axplot,inpcount,ncolx,ncoly,flag,fsize,
     4 comment,ncomment,annotate,annot,nannot,annotx,annoty,
     5 direction,dataform)
        close (98)
        if (epsplot) then
            unit=97
            open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
            call write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,title,ntitle,ploteps,nploteps,i2file,ni2file,
     2 isize,plotwid,plotht,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,axplot,inpcount,ncolx,ncoly,flag,fsize,
     4 comment,ncomment,annotate,annot,nannot,annotx,annoty,
     5 direction,dataform)
            close(97)
        endif
        return

995     continue
        call xvmessage('??E - Error opening/writing gnuplot file',' ')
        call abend
        return

996     continue
        call xvmessage('??E - Error opening/writing gnuplot eps file',' ')
        call abend
        return

999     continue
         CALL XVMESSAGE ('??E - Error opening data file',' ')
         CALL PRNT(4,1,JST,' IOSTAT=.')

	return
	end
c*******************************************************************
	subroutine plottext(xlen,ylen,annotate,annot,nannot,
     1 annotx,annoty,fsize)
c	This subroutine is for plotting text and numbers in interface files
	implicit none

c
c       recoded for new IBIS subroutine calls - 5/98 bam
c

        integer*4 i,k
	integer*4	status, count, runit, ibis
        integer*4 record1,record2
        integer*4 rk, fsize
	integer*4	ncol, clen
	integer*4	cols(5)
        integer*4 rcols(5)
c        integer*4 acols(5)
	integer*4	heightdef, angledef, def
	integer*4	nchar, row, nelms, nfmts, nannot		!inteq
	real*4	zmin, zmax
	real*4	height, angle, x, y, xlen, ylen, arad, sin, cos
	real*4	xscale, xoffs, yscale, yoffs
	real*4 annotx,annoty
	logical*4 annotate
        character*4 format(5)   ! for columns
	character*20 annot
        real*4 rnum(5)
        real*4 val
        integer*4 ndec

	common / scaling / xscale, xoffs, yscale, yoffs, zmin, zmax

c*******************************************************************

	angle = 0.0      ! default the angle
        ndec = 2         ! # of places after the decimal point
	nchar = 0
	rnum(5) = 0
c       read the user parameters for this file

	call xvp ('XCOL', cols(1), count)  ! x value column
	call xvp ('YCOL', cols(2), count ) ! y value column
					   ! character size value column
	call xvparm ('HEIGHCOL', cols(3), count, heightdef,' '  )
					   ! character angle value column
	call xvparm ('ANGLECOL', cols(4), count, angledef,' ' )
					   ! which columns
	call xvparm ('DATACOLS', cols(5), nelms, def,' ')
					   ! column formats
	call xvparm ('FORMAT', format, nfmts, def,' ')
					   ! default character size 
c	print *, 'nfmts , format = ',nfmts , format

	call xvp ('TSIZE',height,count) ! default text height

c	print *,'after TSIZE'
c get a unit for the interface file
        CALL XVUNIT(RUNIT, 'INP', 2, STATUS, ' ')
 

c open the interface file
	call ibis_file_open(runit,ibis,'read',0,0,' ',' ',status)
        if (status.ne.1) call ibis_signal_u(runit,status,1)


c get # of rows and columns
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
c	call rdfil( infile, 2, clen, ncol, nofile )	! open input


c open records for data columns and ascii text

        rcols(1) = cols(1) 
        rcols(2) = cols(2)

c	print *, 'ncol,cols(1), cols(2) = ',ncol,cols(1), cols(2) , cols(3), cols(4), cols(5)
c	print *, 'nelms, heightdef, angledef = ',nelms,heightdef, angledef
        k = 2
        if (heightdef .eq. 0) then
            rcols(3) = cols(3) 
            k = k + 1
        end if
        if (angledef .eq. 0) then
            rcols(4) = cols(4) 
            k = k + 1
        end if
c	print *,'format(1) = ',format(1)
c	print *,'format(2) = ',format(2)
c	print *,'format(3) = ',format(3)

        rk = 0        
        count = 0         ! count of ascii columns
        do i = 1, nelms   ! loop through datacols
            k = k + 1
            if ( format(k) .eq. 'ASCI' .or.
     -         format(k) .eq. 'asci') then
               count = count + 1
            else			      ! numerical text
                rcols(k) = cols(4+i) 
                rk = 1
            end if

        end do        
c
c       open appropriate IBIS records
c
c	print *,'before record open count ,rk, k = ',count,rk,k
        if ( rk .eq. 0 ) k = k - 1
        call ibis_record_open(ibis,record1,'format:real',
     -          rcols,k,'REAL',status)

        if ( count .gt. 0 ) then
            go to ( 1,2,3,4,5), count
 1          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a4',status)
            nchar = 4
            go to 6
 2          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a8',status)
            nchar = 8
            go to 6
 3          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a12',status)
            nchar = 12
            go to 6
 4          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a16',status)
            nchar = 16
            go to 6
 5          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a20',status)
            nchar = 20

 6          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
        end if

c	print *,'nchar = ',nchar
c	print *,'after record open clen = ',clen
	do row = 1, clen        ! process each row
c	print *, 'here'
            call ibis_record_read(record1,rnum,row,status)
	    if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
            x = rnum(1)
            y = rnum(2)

c		print *,'x,y = ',x,y
c		print *,'rnum(3),rnum(4), rnum(5) = ',rnum(3),rnum(4),rnum(5)
            if (heightdef .eq. 1) height = rnum(3)
            if (angledef .eq. 1) angle = rnum(4)
         
c	    x = x * xscale + xoffs
c            y = y * yscale + yoffs - height/2.
					! check annotation fits in the plot
  	    if(x.lt.0.or.x.gt.xlen.or.y.lt.0.or.y.gt.ylen) goto 50
		annotx = x
		annoty = y
		annotate = .true.
		fsize = height
c		print *,'x,y = ',x,y

            if ( format(cols(5)) .ne. 'ASCI'
     -          .and. format(cols(5)) .ne. 'asci' ) then
               val = rnum(5)
		write(annot,10400) val
10400 format(f10.3)
	       nannot=10
cccccc               call number (x, y, height, val, angle, ndec)
            end if

            if ( format(cols(5)) .eq. 'ASCI'
     -          .or. format(cols(5)) .eq. 'asci' ) then
            call ibis_record_open(ibis,record2,'format:ASCII',
     1               0,0,'a20',status)
                if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

                arad=angle*3.1416/180. ! convert angle to radians
                x=x-nchar*height/2.*cos(arad) ! center the string of characters
                y=y-nchar*height/2.*sin(arad)
                call ibis_record_read(record2,annot,row,status)
		if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
cccccc	        call symbol (x,y,height,astr,inteq,angle,nchar)
		nannot = index(annot,'  ') - 1
c                if ( rk .eq. 1 ) then
c                    val = rnum(3)
c                    y = y - .25
ccccc                    call number (x, y, height, val, angle, ndec)
c                end if
            end if
c	print *, 'annot = ',annot(1:nannot)
 50         continue       ! point outside plot
        end do
c	print *, 'before file close'
    	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)

	return
	end
c*******************************************************************
	integer*4 function slength(string)
	integer*4	i
	character*(*) string

	i = len(string)
	do while (ichar(string(i:i)) .eq. 32 .and. i .gt. 1)
	    i = i - 1
	enddo
	slength = i
	end
c*******************************************************************
	subroutine format (x, y, z)
c		Formats data from (x,y) to (x,y) or (y,x)
	implicit none
	real*4	x, y, z
	real*4	tmp
	integer*4	dataformat
	common / form / dataformat

	if (dataformat .eq. 2) then
	    tmp = x
	    x = y
	    y = tmp
	endif

	return
	end
C*******************************************************************************
        subroutine write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,title,ntitle,ploteps,nploteps,i2file,ni2file,
     2 isize,plotwid,plotht,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,axplot,inpcount,ncolx,ncoly,flag,fsize,
     4 comment,ncomment,annotate,annot,nannot,annotx,annoty,
     5 direction,dataform)

        implicit none
        integer*4 unit,ntbl,nylabel,nxlabel,ntitle,nploteps
        integer*4 isize,plotwid,plotht,axplot,ni2file
        integer*4 ii,jj,inpcount,gcol,flag,ncomment,nannot
        integer*4 ncolx(5),ncoly(5),fsize,psize

        real*4 xrang1,xrang2, yrang1,yrang2, zrang1,zrang2
	real*4 fpos,annotx,annoty

	logical*4 annotate
c        real*4 viewelev,viewazi,scale,zscale
        character*2  direction, dataform

	character*20 annot
        character*60 xlabel,ylabel
	character*70 comment
        character*80 tbl,ploteps,i2file
        character*120 title,outline
c
c       bring in gunplot colors, lines, points, etc
	include 'gnuplotchar'
c
	psize = isize
	if (unit .eq. 97) psize = 16

10100 format('# Created by program pltgraf')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for plotting ibis files')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)

	if (inpcount.gt.1) then
10115 format('# Annotations from  ',a)
	      write(unit,fmt=10115,iostat=jj,err=995) i2file(1:ni2file)
	endif

        if (unit .eq. 97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,' size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) ploteps(1:nploteps)
        else
! size is X,Y
10315 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10315,iostat=jj,err=995) isize,plotwid,plotht
10316 format('set output')                              !set output to screen
            write(unit,fmt=10316,iostat=jj,err=995)

        endif

10120 format('set grid ')
            write(unit,fmt=10120,iostat=jj,err=995)

c       print *, 'set grid'
        if (axplot .eq.0) then
10123 format('unset tics')
            write(unit,fmt=10123,iostat=jj,err=995)

c           print *,'unset tics'
        endif

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
10150 format("# This coord system is '",a2,"' - '",a1,"'")
             write(unit,fmt=10150,iostat=jj,err=995) dataform(1:2),direction(1:1)
C *****
C     DATAFORM = YX
C *****
	if (dataform .eq. 'YX') then
c       T/B is for Y-axis
            if (direction(1:1) .eq. 'T') then
10165 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10165,iostat=jj,err=995) yrang1,yrang2
	    else
10166 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10166,iostat=jj,err=995) yrang2,yrang1
	    endif
c	R/L is for X-axis
	    if (direction(2:2) .eq. 'R') then
10170 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10170,iostat=jj,err=995) xrang1,xrang2
	    else
10171 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10171,iostat=jj,err=995) xrang2,xrang1
	    endif
	else
C
C   Note Y and X axes are interchanged for plot
C
            if (direction(1:1) .eq. 'T') then
10175 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10175,iostat=jj,err=995) xrang1,xrang2
            else
10176 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10176,iostat=jj,err=995) xrang2,xrang1
            endif
c       R/L is for X-axis
            if (direction(2:2) .eq. 'R') then
10180 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10180,iostat=jj,err=995) yrang1,yrang2
            else
10181 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10181,iostat=jj,err=995) yrang2,yrang1
            endif

	endif
C
C  End of X and Y axes plot
	
10195 format("set zrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10195,iostat=jj,err=995) zrang1,zrang2

10200 format("set xlab '",a,"'" )
            write(unit,fmt=10200,iostat=jj,err=995) xlabel(1:nxlabel)
10205 format("set ylab '",a,"'")
            write(unit,fmt=10205,iostat=jj,err=995) ylabel(1:nylabel)

	ii = 0
	if (flag .eq. 2 .or. flag .eq. 3) then
	fpos =.95
	ii = 1
10220 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
	   write(unit,fmt=10220,iostat=jj,err=995) ii,comment(1:ncomment),fpos,psize
	endif

	if (annotate) then
	ii = ii + 1
10230 format('set label ',i2,' "',a,'" at ',f10.2,',',f10.2,
     1 ' font "ariel,',i3,'" front nopoint tc def')
           write(unit,fmt=10230,iostat=jj,err=995) ii,annot(1:nannot),annotx,annoty,fsize

	endif
	gcol = 1
10253 format ("plot  '",a,"' u 1:2:3  w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"' ")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),
     1 lntype(gcol),pttype(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

        if (unit.eq.98) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
        endif

        return
995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_gpi: Error writing gnuplot file',' ')
        write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif
	return
        end
