	include 'VICMAIN_FOR'
c	10-July-95  CRI  MSTP S/W Conversion (VICAR porting)
	subroutine main44

	implicit none
	integer*4 status, idef, def, iplot, count,tcount
	integer*4   i, jj, jst, pen, axisdef, axesdef,dblzero, unit
	integer*4 nxlabel,nylabel,nzlabel,ntitle,ntbl
	integer*4 isize,plotht,plotwid,nheader,nplotname
        integer*4 dataformat,axplot,nplotgpi,nplotgpi2
        integer*4 nploteps,ninpfile
        integer*4 rdgr,getgr,setgr,clgr
	real*4	plotsize, viewelev, viewazi
	real*4 plotbx,plotby
	real*4	theta, phi			!  origx, origy
	real*4	x, y, z				!	  x2, y2,  plotx, ploty
c	real*4	minx2, miny2, maxx2, maxy2
	real*4 maxx,minx,maxy,miny,maxz,minz,tmax
	real*4	plotxsize, plotysize, plotoffsets(2)
c	real*4    aspctrx,aspctry,measuredx,measuredy
	real*4	xrtscalex,xrtscaley
        real*4  rho, origin(3), zscale, axes(3)
        real*4  sinth, costh, costhcosph, sinthcosph, sinph
        real*4  costhsinph, sinthsinph, cosph
        real*4  scale, plotxoff, plotyoff,d2r
        real*4  xrang1,xrang2, yrang1,yrang2,zrang1,zrang2

	logical*4 eof, zero, axlengths,  autoscaling, xvptst
	logical*4 epsplot, parmplot
	character*3  dataform
        CHARACTER*8 pformat
	character*30 xlabel,ylabel,zlabel
	character*80 plot_ds,plotname,plotgpi,plotgpi2
	character*80  tbl,ploteps,header			!(60)
	character*120 title,inpfile

        character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/

	common /perspeccom/ dataformat, origin, zscale, rho,
     +			sinth, costh, costhcosph, sinthcosph, sinph,
     +			costhsinph, sinthsinph, cosph

	common /scalecom/ scale,plotxoff,plotyoff,xrtscalex,xrtscaley
c
c	unit	file
c	INP     input graphic
ca	13	gpi data file
c	97	gpi eps plot
c	98	gpi gnuplot plot
c

        call ifmessage('PLOT3D version 7-13-2013 (64-bit) - rjb')

c______________________________________________________________________________
c			start executable code
c------------------------------------------------------------------------------
c
cc  splot 'datafile' u 1:2:3:4 lc variable
        minx = 1.0e20
        miny = 1.0e20
        maxx = -1.0e20
        maxy = -1.0e20
        minz = 1.0e20
        minz = 1.0e20

	epsplot = .false.
	ninpfile = 0
	nplotgpi = 0
	nplotgpi2 = 0
	nploteps = 0 
	ntbl = 0
	title='    '
	tcount = 1

	axes(1) = 0.0
	axes(2) = 0.0
	axes(3) = 0.0
        d2r = acos(-1.0)/180.0     !degree to radian conversion PI/180

c	aspctrx = 18.0    ! Max box for scaling purposes using:
c	aspctry = 13.50   !  1.5 * default widget aspect ratio (1200 x 900) 
c        measuredx = 9.0
c	if (xvptst('TITLE')) then
c		measuredy = 6.375
c	else
c		measuredy = 6.5625	!if no title
c	endif
c	xrtscalex = aspctrx/measuredx
c	xrtscaley = aspctry/measuredy

	call xvparm('INP',inpfile,count,def,1)
	  ninpfile=index(inpfile,'   ') - 1

	
        call xvparm ('TITLE', title, count, def,1)
	ntitle = index(title,'    ') - 1

	call xvp ('DATAFORM', dataform, count)
	if (dataform(1:3) .eq. 'XYZ') then		!X,Y,Z
	    dataformat = 1
	else if (dataform(1:3) .eq. 'YXZ') then		!Y,X,Z
	    dataformat = 2
	else if (dataform(1:3) .eq. 'LSZ') then		!line,sample,Z
	    dataformat = 3
	else
	    dataformat = 1				!default is X,Y,Z
	endif

c		get the scaling parameters
	call xvp ('PLOTSIZE', plotsize, count )		!in inches
	print *,'PLOTSIZE = ',plotsize
        plotht =  100 * plotsize
        plotwid = 100 * plotsize

	call xvp ('ZSCALE', zscale, count )		!vs. units of x,y

	call xvparm ('SCALE', scale, count, def,1 )
	print *, 'SCALE = ',scale
	autoscaling = (def .eq. 1)		!autoscaling if no entry
	print *,'AUTOSCALING = ',autoscaling
c	if (scale .ne. 0.0)  scale = 1.0/scale		!get divisor scale factor to inches
	call xvp ('PLOTOFFS', plotoffsets, count )	!only for manual scaling  - offset (X,Y) from view origin

	call xvp ('DISTANCE', rho, count)		!distance of observer from origin in units of graphics
	call xvp ('ELEV', viewelev, count)		!elev of viewer in degrees above horizon (looking at origin)
	call xvp ('AZIMUTH', viewazi, count)		!azimuthal angle of observer in deg E of N (North is Y direction)
	call xvp ('ORIGIN', origin(1), count)	 	!3-d location of origin		
	phi = 90.0 - viewelev				!Angle from origin above XY plane
	theta = 90.0 - viewazi				!angle from North on XY plane in clockwise direction 

c	sinth = sin (theta * d2r)
c	costh = cos (theta * d2r)
c	sinph = sin (phi * d2r)
c	cosph = cos (phi * d2r)
c	costhcosph = costh*cosph
c	sinthcosph = sinth*cosph
c	costhsinph = costh*sinph
c	sinthsinph = sinth*sinph
c	xrang2 =  costh * cosph * rho
c	yrang2 =  sinth * cosph * rho
c	zrang2 = sinph*rho
	    
	call xvp ('PEN', pen, count)

	print *,'title = ',title
	print *,'dataformat = ',dataformat
        print *,'plotsize = ',plotsize
        print *,'scale = ',scale
        print *,'zscale = ',zscale
	print *,'plotoffsets = ',plotoffsets
        print *,'distance = ',rho
	print *,'elev = ',viewelev
	print *,'azimuth = ',viewazi

c			Open the input IBIS graphics-1 file
c	rdgr = INP
cc        status = rdgr (1,1,3)
cc        if (status.ne.1) call signalgr(1,status,1)

c			If output exists then output IBIS graphics-1 2-D perspective
cc	call xvparm ('OUT', outfile, count, def,1 )
cc	if (def .eq. 0) then
c cc           status = wrgr ( 1, 2, 3 )		!Open
cc            if (status.ne.1) call signalgr(2,status,1)
cc	    eof = .false.
cc	    do while (.not. eof)
cccc			scan for the beginning of a line string
cc                status = nextgr (1,eof,x,y,z)
cc               if (status.ne.1) call signalgr(1,status,1)
cc		if (eof) go to 199

cc		zero = .false.
cc		do while (.not. zero .and. .not. eof)
cc		    call perspective (x, y, z,  x2, y2)
cc                    status = putgr (2,x2,y2,0.0)
cc                    if (status.ne.1) call signalgr(2,status,1)
cc                    status = getgr ( 1,zero,eof,x,y,z)
cc                    if (status.ne.1) call signalgr(1,status,1)
cc		enddo
cc                status = putgr (2,0.0,0.0,0.0)
cc                if (status.ne.1) call signalgr(2,status,1)
cc	    enddo
cc199	    continue
cc            status = clgr (1)
cc            if (status.ne.1) call signalgr(1,status,1)
cc            status = clgr (2)
cc
cc            if (status.ne.1) call signalgr(2,status,1)
cc	    return
cc	endif	!only if output file
c
cc	status = setgr (1,1) ! reset input IBIS graphics-1 file to its first coordinate
cc	if (status.ne.1) call signalgr(1,status,1)
	eof = .false.
c	find zeroes in file
c               open plotter
        CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
        IF (IDEF.EQ.0) THEN
cc          CALL PLOTFN(PLOT_DS)
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

cc       rdgr = INP - OPEN IBIS-1 graphics-1 file
        eof = .false.
        status = rdgr (1,1,3)
        if (status.ne.1) call signalgr(1,status,1)

	OPEN(13,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
	i = 0
	dblzero = 0
	do while (.not. eof)
                status = getgr (1,zero,eof,x,y,z)               !get 
                if (status.ne.1) call signalgr(1,status,1)
                if (.not. eof) then
	            i = i + 1
	            if (zero) then
			dblzero = dblzero + 1

			print *,'dblzero = ',dblzero
	                if (dblzero .ge. 2) go to 100
			print *, 'zero   i = ',i
	                write(13,10090)
10090 format (1x,' ')
		    else
			dblzero =  0
                        print *,'i,input x,y,z =',i,x,y,z
			if (x .gt. maxx) maxx = x
			if (x .lt. minx) minx = x
                        if (y .gt. maxx) maxy = y
                        if (y .lt. miny) miny = y
                        if (z .gt. maxz) maxz = z
                        if (z .lt. minz) minz = z
		        write (13,10100)  x, y, z
10100 format (3f10.2)
		    endif
                endif
            enddo
100	continue
            status = setgr (1,1) ! reset input IBIS graphics-1 file to its first coordinate
            if (status.ne.1) call signalgr(1,status,1)
        eof = .false.


	print *, 'total count = ',i
	tmax = maxx
	if (maxy .gt. tmax) then
	   tmax = maxx
	endif
	if (maxy .gt. tmax) then
	   tmax = maxz
	endif
	
	print *, 'maxx,minx,maxy,miny,maxz,minz = ',maxx,minx,maxy,miny,maxz,minz
c
	print *, 'tmax = ',tmax
c	print *, 'xrang2,yrang2,zrang2 = ',xrang2,yrang2,zrang2

c
	close(13)
	if (autoscaling) then
ccc	    minx2 = 1.0e20
ccc	    miny2 = 1.0e20
ccc	    maxx2 = -1.0e20
ccc	    maxy2 = -1.0e20
ccc	    eof = .false.
ccc	    print *, 'az,el  phi,theta = ',viewazi,viewelev,phi,theta
ccc	    do while (.not. eof)
ccc                status = getgr (1,zero,eof,x,y,z)		!get 
ccc                if (status.ne.1) call signalgr(1,status,1)
ccc		if (.not. zero .and. .not. eof) then
ccc                    print *,'input x,y,z =',x,y,z
c		    call perspective (x, y, z, x2, y2)
ccc		    minx2 = min (minx2, x)
ccc		    miny2 = min (miny2, y)
ccc		    maxx2 = max (maxx2, x)
ccc		    maxy2 = max (maxy2, y)
ccc		endif
ccc	    enddo
ccc            status = setgr (1,1) ! reset input IBIS graphics-1 file to its first coordinate
ccc            if (status.ne.1) call signalgr(1,status,1)

ccc	    print *,'minx2,miny2,maxx2,maxy2 = ',minx2,miny2,maxx2,maxy2
ccc	  print *,'max(maxx-minx, maxy-miny) = ',max(maxx-minx, maxy-miny)
	    scale = 1.0		!	plotsize		!/tmax 				!max(maxx-minx, maxy-miny)
	    plotxoff = -scale*minx
	    plotyoff = -scale*miny
	    plotxsize = scale*(maxx-minx) 
	    plotysize = scale*(maxy-miny)
	else
	    plotxoff = plotoffsets(1)
	    plotyoff = plotoffsets(2)
	    plotxsize = plotsize
	    plotysize = plotsize
	endif

	print *,'plotxoff, plotyoff, plotxsize, plotysize,scale = ',plotxoff, plotyoff, plotxsize, plotysize,scale
c	call plots ( 0, 0, 9)
cc	call xrtbegin(status)
cc        if (status.ne.1) call mabend('Unable to OPEN Plotter')
ccc        CALL DISPLAYAXES(0,0,0)   ! turn auto axis feature off    
c		make new origin and get right pen
ccc	call plot (0.5, 0.5, -3)	!origin at 0.5,0.5
ccc	call newpen (pen)		!line width
c  	        scale drawing for paper size
ccc	call plot (0.0, 0.0, 3)		!move to 0,0
ccc	call plot (aspctrx,aspctry, 3)

c		draw box
	if (xvptst ('BOX')) then
	    plotbx = plotxsize * xrtscalex
	    plotby = plotysize * xrtscaley
ccc	    call plot (0.0, 0.0, 3)		!move to 0,0
ccc	    call plot (plotbx, 0.0, 2)		!line from box top left to 0.0
ccc	    call plot (plotbx, plotby, 2)	!line to top left to top right
ccc	    call plot (0.0, plotby, 2)		!line from 0 to top right  
ccc	    call plot (0.0, 0.0, 2)		!line back to 0,0
	endif

c		put on title
c	call xvparm ('TITLE', title, count, def,1)
ccc        CALL HEADER (TITLE,tcount,1)

c    XRTP does not provide capability to scale the TITLE on the plot
c      (so the following is commented out 7/8/95)
c	if (def .eq. 0) then
c	    plotx = plotxsize/2. - 0.25*.67*(slength(title)/2.+2)
c	    call symbol(plotx, plotysize+0.15, 0.25, 
c     +			 title, 0, 0.0, slength(title) )
c	endif
c		draw 3-D axis 
	axplot = 0
	call xvparm ('AXIS', axplot, count, axisdef,1)
	print *,'axplot, axisdef = ',axplot, axisdef   

	axlengths = .false.
	call xvparm ('AXES', axes(1), count, axesdef,3)
	if (axesdef .eq. 0) axlengths = .true.

	print *, 'axlengths = ',axlengths
ccc	if (axisdef .eq. 0 .and. axplot .ne. 0.0) then
ccc	    call perspective (origin(1), origin(2), origin(3), x2, y2)
ccc	    call scaleplot (x2, y2,  origx, origy)
ccc	    call perspective (origin(1)+axlen,origin(2),origin(3),x2,y2)
ccc	    call scaleplot (x2, y2,  plotx, ploty)
ccc	    call plot ( origx, origy, 3)	!mv to origx,origy
ccc	    call plot ( plotx, ploty, 2)	!line from plotx to ploty
ccc	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
ccc     +					dataform(1:1), 0, 0.0, 1)	!for XYZ
ccc	    call perspective (origin(1),origin(2)+axlen,origin(3),x2,y2)
ccc	    call scaleplot (x2, y2,  plotx, ploty)
ccc	    call plot ( origx, origy, 3)	!mv to origx,origy
ccc	    call plot ( plotx, ploty, 2)	!line from plotx to ploty
ccc	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
ccc   +					dataform(2:2), 0, 0.0, 1)	!for YXZ
ccc	    call perspective (origin(1), origin(2),
ccc     +                           origin(3)+axlen*zscale,x2,y2)
ccc	    call scaleplot (x2, y2,  plotx, ploty)
ccc	    call plot ( origx, origy, 3)	!mv to origx,origy
ccc	    call plot ( plotx, ploty, 2)	!line from plotx to ploty
ccc	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
ccc     +					dataform(3:3), 0, 0.0, 1)	!for line,saample,Z
ccc	endif

c		plot view parameters
	header = ' '
	parmplot = .false.
	if (xvptst ('PARM')) then
	     write (header,10200) viewelev,viewazi,scale,zscale
10200 format(" - initial view: el = ",f5.1," az = ",f5.1," scale = ",f5.1," zscale = ",f5.1)
             nheader = index(header,'    ') - 1
	     parmplot = .true.
	endif
cc	    if (axisdef .eq. 0) then
cc		write (string, '(a5,i3,a6,i4,a8,i4)') 
c c    +			'ELEV=', nint(viewelev), 
cc     +			'  AZI=', nint(viewazi), 
cc     +			'  AXPLOT=', axplot
ccc		call symbol (0.2, -0.35, 0.15, string, 0, 0.0, 33)
cc	    else
cc		write (string, '(a5,i3,a6,i4)') 
cc     +			'ELEV=', nint(viewelev), 
cc     +			'  AZI=', nint(viewazi)
c		call symbol (0.2, -0.35, 0.15, string, 0, 0.0, 18)
cc	    endif
ccc	    call footer(string,1,1)
cc	endif

cc	eof = .false.
cc	do while (.not. eof)
ccc		scan for the beginning of a line string
cc            status = nextgr (1,eof,x,y,z)
cc            if (status.ne.1) call signalgr(1,status,1)
cc	    if (eof) go to 299
cc
ccc		Move to the first point with pen up
cc	    call perspective (x, y, z,  x2, y2)
cc	    call scaleplot (x2, y2,  plotx, ploty)
ccccc	    call plot ( plotx, ploty, 3)	!mv to plotx,ploty
cc
cc	    zero = .false.
cc	    do while (.not. zero .and. .not. eof)
cc		call perspective (x, y, z,  x2, y2)
cc		call scaleplot (x2, y2,  plotx, ploty)
ccccc		call plot ( plotx, ploty, 2)	!line from previous to plotx,ploty
cc                status = getgr (1,zero,eof,x,y,z)
cc                if (status.ne.1) call signalgr(1,status,1)
cc	    enddo
cc	enddo

cc299	continue
        status = clgr (1)
        if (status.ne.1) call signalgr(1,status,1)

c		Close the plotter and be done.
ccc	call plot( 0.0, 0.0, 999 )
	if (rho .eq. 0.0) then
		rho = tmax + 0.1 * tmax
		if (rho .gt.1) then
		    rho = nint(rho) * 1.0
		endif
	endif
        xlabel = 'EAST'
        nxlabel = index(xlabel,'    ') - 1
        ylabel = 'NORTH'
        nylabel = index(ylabel,'    ') - 1
        zlabel = 'VERT'
        nzlabel = index(zlabel,'    ') - 1
        xrang1 = origin(1)
        yrang1 = origin(2)
        zrang1 = origin(3)
	print *, 'axes = ',axes
	if (axlengths) then
	    xrang2 = axes(1)
	    yrang2 = axes(2)
	    zrang2 = axes(3)
	    if (rho .ne. 0) then
	         scale = axes(1)/rho
	         zscale = axes(3)/rho
	    endif	
	else
	    xrang2 = rho
            yrang2 = rho
            zrang2 = rho
        endif
c        header = " - initial view: el = ,f5.1  az = 202  scale = 1  zscale = 1"

	if (parmplot) then
            title = title(1:ntitle)//header(1:nheader)
            ntitle = index(title,'      ')
	endif

        isize = 10              !type size
        unit = 98
	open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
        call write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,zlabel,nzlabel,title,ntitle,ploteps,nploteps,header,nheader,
     2 isize,plotwid,plotht,pen,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,viewelev,viewazi,scale,zscale,axplot)
	close (98)
	if (epsplot) then
	    unit=97
	    open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
            call write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,zlabel,nzlabel,title,ntitle,ploteps,nploteps,header,nheader,
     2 isize,plotwid,plotht,pen,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,viewelev,viewazi,scale,zscale,axplot)
	    close(97)
	endif
	return

995	continue
	call xvmessage('??E - Error opening/writing gnuplot file',' ')
        call abend
	return

996	continue
	call xvmessage('??E - Error opening/writing gnuplot eps file',' ')
        call abend
	return


999	continue
	 CALL XVMESSAGE ('??E - Error opening data file',' ')
         CALL PRNT(4,1,JST,' IOSTAT=.')

	return
	end

C***********************************************************
	subroutine perspective (x, y, z, x2, y2)
	implicit none
	real*4	x, y, z,  x2, y2
	real*4	xp, yp, zp, x_eye, y_eye, z_eye, tmp

	integer*4	dataformat
	real*4	rho, origin(3), zscale
	real*4	sinth, costh, costhcosph, sinthcosph, sinph
	real*4	costhsinph, sinthsinph, cosph

	common /perspeccom/ dataformat, origin, zscale, rho,
     +			sinth, costh, costhcosph, sinthcosph, sinph,
     +			costhsinph, sinthsinph, cosph

	xp = x - origin(1)
	yp = y - origin(2)
	zp = (z - origin(3))/zscale
	if (dataformat .eq. 2) then
	    tmp = xp
	    xp = yp
	    yp = tmp
	else if (dataformat .eq. 3) then
	    tmp = xp
	    xp = yp
	    yp = -tmp
	endif
	
	x_eye = -xp*sinth + yp*costh
	y_eye = -xp*costhcosph - yp*sinthcosph  + zp*sinph
	z_eye = 1.0 - (xp*costhsinph + yp*sinth*sinph + zp*cosph)/rho
	x2 = x_eye/z_eye
	y2 = y_eye/z_eye

c	print *,'perspective = xorigin,yorigin,x,y,z,x2,y2 = ',origin(1),origin(2),x,y,z,x2,y2
	return
	end

C******************************************************************
	subroutine scaleplot (x2, y2, plotx, ploty)
	implicit none
	real*4	x2, y2,  plotx, ploty, xrtscalex,xrtscaley

	real*4	scale, plotxoff, plotyoff
	common /scalecom/ scale,plotxoff,plotyoff,xrtscalex,xrtscaley

	plotx = (scale*x2 + plotxoff)*xrtscalex
	ploty = (scale*y2 + plotyoff)*xrtscaley

c	call 
	return
	end

C********************************************************************
c	integer function slength(string)
c	implicit none
c	integer	i
c	character*(*) string
c
c	i = len(string)
c	do while (ichar(string(i:i)) .eq. 32 .and. i .gt. 1)
c	    i = i - 1
c	enddo
c	slength = i
c	return
c	end
C*******************************************************************
	subroutine write_gpi (unit,tbl,ntbl,ylabel,nylabel,xlabel,
     1 nxlabel,zlabel,nzlabel,title,ntitle,ploteps,nploteps,header,nheader,
     2 isize,plotwid,plotht,pen,xrang1,xrang2, yrang1,yrang2,
     3 zrang1,zrang2,viewelev,viewazi,scale,zscale,axplot)

	implicit none
        integer*4 unit,ntbl,nylabel,nxlabel,nzlabel,ntitle,nploteps
	integer*4 isize,plotwid,plotht,pen,axplot,nheader
	integer*4 jj,psize

	real*4 xrang1,xrang2, yrang1,yrang2, zrang1,zrang2
	real*4 viewelev,viewazi,scale,zscale
        character*30 xlabel,ylabel,zlabel	
	character*80 tbl,ploteps,header
	character*120 title,outline
c
	psize = isize
	if (unit .eq. 97) psize =  16
10100 format('# Created by program plot3d')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for 2d perspective of 3-D data')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)

c	print *, '# Created by program plot3d'
c	print *, '# Gnuplot commands for 2d perspective of 3-D data'
c	print *, '# Data in ',tbl(1:ntbl)

        if (unit .eq. 97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,' size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) ploteps(1:nploteps)

c	    print *,'set terminal postscript eps enhanced color' 
c	    print *, 'set output'
	else
c size is X,Y
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
            write(unit,fmt=10116,iostat=jj,err=995)

c	     print *, 'set term x11 font "ariel,',isize,'," size ',plotht,',', plotwid
c	     print *, 'set output'
	endif

10120 format('set grid ')
            write(unit,fmt=10120,iostat=jj,err=995)

c	print *, 'set grid'
	if (axplot .eq.0) then
10123 format('unset tics')
            write(unit,fmt=10123,iostat=jj,err=995)

c	    print *,'unset tics'
	endif
10140 format('# First angle is elevation, second is azimuth')
	    write(unit,fmt=10140,iostat=jj,err=995)
10142 format("set view ",f5.1,",",f5.1,",",f5.1,",",f5.1)    
            write(unit,fmt=10142,iostat=jj,err=995) viewelev,viewazi,scale,zscale
10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
10160 format('# This coord system with xrang2 and xrang1 reversed gives proper N and E orientation')
	     write(unit,fmt=10160,iostat=jj,err=995)
10165 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10165,iostat=jj,err=995) yrang1,yrang2
10170 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10170,iostat=jj,err=995) xrang1,xrang2		! xrang2,xrang1
10175 format("set zrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10175,iostat=jj,err=995) zrang1,zrang2

c	print *, 'set view ',viewelev,',',viewazi,',',scale,',',zscale
c        print *, "set title '",title(1:ntitle),"'"
c	print *, 'set xrange [',xrang1,':',xrang2,']'
c	print *, 'set yrange [',yrang1,':',yrang2,']'
c	print *, 'set zrange [',zrang1,':',zrang2,']'

10180 format('set xyplane 0')
            write(unit,fmt=10180,iostat=jj,err=995)

10200 format("set xlab '",a,"'" )
            write(unit,fmt=10200,iostat=jj,err=995) xlabel(1:nxlabel)
10205 format("set ylab '",a,"'")
            write(unit,fmt=10205,iostat=jj,err=995) ylabel(1:nylabel)
10210 format("set zlab '",a,"'" )
            write(unit,fmt=10210,iostat=jj,err=995) zlabel(1:nzlabel)

c	print *, 'set xyplane 0'
c	print *, "set xlab '",xlabel(1:nxlabel),"'"
c	print *, "set ylab '",ylabel(1:nylabel),"'"
c	print *, "set zlab '",zlabel(1:nzlabel),"'"

10215 format('set multiplot')                
            write(unit,fmt=10215,iostat=jj,err=995)

c	print *, 'set multiplot'

10250 format ("splot  '",a,"' u 1:2:3:4 w labels right offset 0,1 notitle")
           write(unit,fmt=10250,iostat=jj,err=995) tbl(1:ntbl)

10260 format ("splot  '",a,"' u 1:2:3 w linespoints ps 2 lw ",i2," lc rgb 'red'")
           write(unit,fmt=10260,iostat=jj,err=995) tbl(1:ntbl),pen

c	print *, "splot '",tbl(1:ntbl),"' u 1:2:3:4 w labels right offset 0,1 notitle"
c	print *, "splot '",tbl(1:ntbl),"' u 1:2:3 w linespoints ps 2 lw ",pen," lc rgb 'red'"


10275 format('unset multiplot')               
            write(unit,fmt=10275,iostat=jj,err=995)

c	print *, 'unset multiplot'
	if (unit .eq. 98) then

10285 format("pause -1")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10285,iostat=jj,err=995)

c		print *, 'pause -1'

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

	end
