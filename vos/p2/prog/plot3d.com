$!****************************************************************************
$!
$! Build proc for MIPL module plot3d
$! VPACK Version 2.1, Friday, January 08, 2016, 12:20:42
$!
$! Execute by entering:		$ @plot3d
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
$ write sys$output "*** module plot3d ***"
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
$ write sys$output "Invalid argument given to plot3d.com file -- ", primary
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
$   if F$SEARCH("plot3d.imake") .nes. ""
$   then
$      vimake plot3d
$      purge plot3d.bld
$   else
$      if F$SEARCH("plot3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake plot3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @plot3d.bld "STD"
$   else
$      @plot3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create plot3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack plot3d.com -mixed -
	-s plot3d.f -
	-i plot3d.imake -
	-p plot3d.pdf -
	-t tstplot3d.pdf tstplot3d.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create plot3d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create plot3d.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM plot3d

   To Create the build file give the command:

		$ vimake plot3d			(VMS)
   or
		% vimake plot3d			(Unix)


************************************************************************/


#define PROGRAM	plot3d
#define R2LIB

#define MODULE_LIST plot3d.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create plot3d.pdf
PROCESS HELP=*
 PARM INP      TYPE=(STRING,80)  COUNT=1
! PARM OUT      TYPE=(STRING,80)  COUNT=0:1 DEFAULT=--
 PARM PLOT     TYPE=STRING  COUNT=(0:1)     DEFAULT="plot3d"
 PARM PLOTFMT  TYPE=STRING  COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
 PARM ELEV     TYPE=REAL    DEFAULT=0       valid=(0:180)      !30
 PARM AZIMUTH  TYPE=REAL    DEFAULT=180 valid=(0:360)     !180
 PARM DISTANCE TYPE=REAL    DEFAULT=0
 PARM ORIGIN   TYPE=REAL    COUNT=3   DEFAULT=(0,0,0)
 PARM PLOTSIZE TYPE=REAL    DEFAULT=8.0
 PARM ZSCALE   TYPE=REAL    DEFAULT=1.0
 PARM SCALE    TYPE=REAL    COUNT=0:1  DEFAULT=1
 PARM PLOTOFFS TYPE=REAL    COUNT=2  DEFAULT=(0,0)
 PARM DATAFORM TYPE=(STRING,3)  VALID=(XYZ,YXZ,LSZ) DEFAULT=XYZ
 PARM TITLE    TYPE=(STRING,80)  DEFAULT="3d Plot"
 PARM AXIS     TYPE=INTEGER COUNT=0:1  DEFAULT=--
 PARM AXES     TYPE=REAL    COUNT=(0:3) DEFAULT=--
 PARM PARMPLOT TYPE=KEYWORD VALID=(PARM,NOPARM) DEFAULT=PARM
 PARM BOXPLOT  TYPE=KEYWORD VALID=(BOX,NOBOX) DEFAULT=BOX
 PARM PEN      TYPE=INTEGER DEFAULT=1

END-PROC
.TITLE
VICAR/IBIS Program "plot3d"
.HELP
PURPOSE

    "plot3d" plots 3-D IBIS graphics-1 files in true 2-D perspective
using the gnuplot plotting software. 


.PAGE
PLOT OUTPUTS

    The other type of output come from the PLOT and PLOTFMT parameters.
PLOT allows the user to display data from 5 areas on the CCD on an x,y
plot using the gnuplot package after exiting the program. PLOT produces
a file of gnuplot commands contained in a file having a .gpi file extension.
Another file with an .asc extension is create containing columns of data
that are displayed by the gpi file.

   The PLOTFMT parameter allows the user to generate a postscript file of
the output for use in documentation by choosing PLOTFMT=EPS. The default
is to generate a gnuplot interactive display.

.PAGE

  PLOT NAMING CONVENTIONS

  The user should enter only the parent file name without an extension
  for the PLOTOUT parameter.  The program will supply the extensions.

  For example, if the user has an input file of indata.dat and  PLOTOUT=outplot
  then for the interactive plot the following files are produced:

     outplot.gpi
     indata.dat.asc

  The first file is the gnuplot instruction file and the second is the
  data file used by gnuplot.

  If the user wanted an encapsulate postscript file with PLOTFMT=eps
  then the following files are produced:

     outplot.eps.gpi
     indata.dat.asc

  Remember entering the following command gives the eps file, outplot.eps

  ush gnuplot outplot.eps.gpi

  If you move the gpi file to another directory, you must also move the
  input data file, indata.dat.asc to the same directory.

  Note that the gpi file produced by this program has the name of the
  input file embedded in the plot command inside the gpi file, e.g..
  plot  'indata.dat.asc' u  1: 9 t .......


.PAGE
USING GNUPLOT


  INTERACTIVE:

    This program uses the gnuplot package for its plots. Gnuplot is a
  separate package from Vicar and is not actually invoked inside this
  program.  Instead this program creates a template of gnuplot commands
  which are written out as a separate file. The plot is then viewed after
  exiting this program. The file has the extension .gpi. You view
  the plot by issuing the following command in the vicar shell.

  ush gnuplot output.gpi

  or external to vicar as

  gnuplot output.gpi

   After viewing the data, you close the plot by clicking the mouse anywhere
  except on the top bar of the plot. Clicking on the top bar allows you
  to move the plot to any convenient place on the terminal screen.  (While
  the plot is displayed you cannot enter any commands to the vicar shell).

  The data to be plotted by gnuplot is read from a separate file, created
  by this program, which contains columns of data in ascii text.
  File naming conventions are discussed in the OUTPUT section, but in this
  case that file extension will be .asc.

  It is possible to keep the plot alive for comparison purposes by issuing
  the following command.

  ush gnuplot --persist output.gpi
  (You will be able to enter commamds to the vicar shell after clicking on
  the mouse on the plot).


  HARDCOPY:

  This program also allows you to create a hardcopy encapsulated postscript
  plot suitable for publications. This file can be viewed with ghostscript
  or gimp. The encapsulated postscript file is not created by this program
  by by the gnuplot program from a gpi file made especially for this purpose.
  this file has the extension, eps.gpi. You create the hardcopy plot via
  the following command

  ush gnuplot output.eps.gpi

  This creates the eps file output.eps. You can view this file by

  ush gimp output.eps
.PAGE

   2-D PERSPECTIVE PLOTS OF 3-D DATA

   The previous plotting package required the user to input the
   perspective viewing angles and distance, ELEV, AZIMUTH and DISTANCE.
   The viewing angles are with repect to an observer at some
   DISTANCE looking back toward the coordinate system origin.
   These angles are the trig complement of the coordinate system
   angles, phi (ELEV) and theta (AZIMUTH). Phi is the angle above
   or below X-Y plane and theta the angle clockwise from Y axis
   (usually called North in mapping systems).

   If the chosen perspective wasn't pleasing then the user kept
   entering appropriate angles or distance until it became so.

   Gnuplot does not have this limitation. If you plot a 3-D data
   set the user can rotate the viewing angles or change distances
   with appropriate mouse commands on the plot panel. The lower 
   left hand portion of the plot panel is updated with the new 
   angles and position:

   
   view:  elev,  azimuth   scale: 1.0000  1.0000

   The scale values are the scale of the X-Y plane and the Z-axis.

   The original angles and distances are in the title of the
   plot.
 
   When you have the desirable view then there is a problem
   with saving the plot that didn't exist with the old xrt
   plotting package. The updated angles and distance are
   not available to vicar.

   The purpose of plot3d originally was to create a 3-D object
   in an IBIS-1 graphics file, then find a perspective and then
   convert that graphics file into a new graphics file. The
   older program had all the mathematical operations to do that
   perspective rendering that is part of gnuplot.

   So plot3d cannot do that function. 

   A new program perspec was created for that purpose.

   Once you have chosen the desired view you must record the
   four values at the bottom of the plot panel and enter them
   into program perspect using the input file as for plot3d. This
   will create the output perspective that formerly was done
   by the old plot3d program.

   The vicar program pltgraf is and alternate way to view the
   IBIS-1 graphics format. 

.PAGE
    DEVELOPER Note:

    This program used to link to the XRT plot library -lxrt. Calls to
  that library were mitigated through a Calcomp conversion library,
  xrtps located in the p2 subroutine library. With the conversion to
  gnuplot, neither of these packages are used.


.PAGE
EXECUTION

Examples:

A standard autoscaling 3-D plot:
plot3d  THREE.GRA  PLOTSIZE=6.0 ELEV=30 AZIMUTH=135 DISTANCE=200 

To plot a file in (line,sample,Z) format and scale the Z value down:
plot3d  THREE.GRA  DATAFORM=LSZ PLOTSIZE=3 ZSCALE=100

To plot a 3-D axis and title:
plot3d  THREE.GRA  PLOTSIZE=6.0 ELEV=30 AZIMUTH=135  AXIS=10 +
			'NOBOX TITLE='THREE D PLOT'

For manual scaling:
plot3d  THREE.GRA  SCALE=10 PLOTOFFS=(4.5,6)

For 2-D file output:
plot3d  THREE.GRA  TWO.GRA  ZSCALE=10  ORIGIN=(100,100,10)

For the default plot:
plot3d  THREE.GRA  



.PAGE
Original Programmer:	Frank Evans	January 1987

Cognizant Programmer:	Michael Tschudi	June 1987

Revision History:

    10-July-95 - Randy Schenk (CRI) - Ported to Unix
     7 Feb 2013 - Ray Bambery - Updated to Linux 64-bit, gnuplot
    13 Feb 2013 - Ray Bambery - Updated documentation 
    10 Jul 2013 - Ray Bambery - Consistent file naming conventions
    13 Jul 2013 - Ray Bambery - Adjusted eps format to more readable fonts
                                Remove vestiges of debug statements

.LEVEL1
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
Optional 2-D IBIS graphics file.
No plot produced if output file.
.VARIABLE PLOT
Output Plot file name.
Default="plot3d"
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE ELEV
Observer elevation angle
in degrees above horizon.
.VARIABLE AZIMUTH
Observer azimuthial angle
in degrees east of north.
.VARIABLE DISTANCE
Observer distance from origin
(in same units as graphics).
.VARIABLE ORIGIN   
The view origin (observer looks
toward origin) in same format
as 3-D graphics file.
.VARIABLE PLOTSIZE 
The plot size in inches.
.VARIABLE ZSCALE
Divisor to convert scale of
Z values into X,Y units.
.VARIABLE SCALE
Specify for manual scaling.
Divisor to convert graphics
file units to inches on plot.
.VARIABLE PLOTOFFS
Only used for manual scaling.
Offset of view origin (X,Y) 
from plot origin (in inches).
.VARIABLE DATAFORM 
3-D data format:
XYZ for (X,Y,Z)
YXZ for (Y,X,Z)
LSZ for (line,sample,Z)
.VARIABLE TITLE    
Title for top of plot.
.VARIABLE AXIS
3-D axis plotted if specified.
Length of axis in units of
graphics file.
.VARIABLE PARMPLOT
Keyword to plot parameter
values on plot.
.VARIABLE BOXPLOT  
Keyword to plot box
around plotted data.
.VARIABLE PEN
Width of plotting pen to use.

.LEVEL2
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
If an output file is specified then a perspective transformation will be
performed and the result output in an 2-D IBIS graphics file.  The output
is in (X,Y) format.  No plot will be produced an output file is specified.  
The ORIGIN and ZSCALE parameters are used in the transformation, but the 
plotting scaling is not.  Thus the output will be in the same units as 
the input 3-D file.
.VARIABLE PLOTOUT
STRING - Output Plot file name.
Default="plot3d"
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE ELEV
The elevation angle of the observer in degrees above the horizon.
.VARIABLE AZIMUTH
The azimuthial angle of the observer in degrees east of north where 
north is the y direction.  Default is to look from the south (180 degrees).
.VARIABLE DISTANCE
The distance of the observer from origin (in same units as graphics file).  
This controls the amount of perspective but not the size of the plot.
.VARIABLE ORIGIN   
The 3-D location of the origin toward which the observer looks.  
In same format as the 3-D graphics file (e.g. XYZ, LSZ).
.VARIABLE PLOTSIZE 
The maximum size of the plotted data not including the plot annotation.
.VARIABLE ZSCALE
Divisor scale factor to convert scale of Z values same scale as the 
X and Y values.
.VARIABLE SCALE
If specified then the autoscaling is disabled.  
SCALE is a divisor scale factor to convert graphics file units to 
inches on plot.
.VARIABLE PLOTOFFS
Only used for manual scaling.  Offset of view origin (X,Y) from plot 
origin (in inches).
.VARIABLE DATAFORM 
The 3-D graphics-1 file contains triplets of real numbers.  
The DATAFORM parameter specifies the format of the triplet:  
XYZ for (X,Y,Z),  YXZ for (Y,X,Z),  LSZ for (line,sample,Z).
.VARIABLE TITLE    
String for centered title at top of plot.
.VARIABLE AXIS
If the AXIS parameter is specified then the 3-D axis will be plotted.
The 3-D axis consists of three lines starting at the view origin, and
drawn along the coordinate axis, with a length given by the AXIS value
in units of graphics file.
.VARIABLE PARMPLOT
Keyword to plot parameter values at bottom of plot.  The value of the
ELEV and AZIMUTH parameters, and the AXIS if specified, are plotted.
.VARIABLE BOXPLOT  
Keyword to plot a box around the plotted data.
.VARIABLE PEN
Number of plotting pen to use.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstplot3d.pdf
procedure
refgbl $echo
refgbl $autousage
body
enable-log

let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! Create a polygon consisting of a cube with a diamond to one side; this
! is an attempt to make a shape that will have a recognizable orientation.
ibis-gen cube.pic NC=3 NR=26 'IBIS-1 'ROW DATACOL=(1,2,3) +
    data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51, +
    1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0, +
    51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0, +
    36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
!
    perspec cube.pic cube_perspective azimuth=0 elev=30 distance=100 origin=(26,26,26)
    perspec cube.pic cube_perspective1 azimuth=0 elev=30 distance=1000 origin=(26,26,26)
    perspec cube.pic cube_perspective2 azimuth=90 elev=30 distance=1000 origin=(26,26,26)
    perspec cube.pic cube_perspective3 azimuth=0 elev=60 distance=1000 origin=(26,26,26)


plot3d cube.pic title="A" plot="p3da"  ! minimal perspective, diamond in upper-right corner

pltgraf cube_perspective  title="B via PLTGRAF - Az=0 elev=30" DIM=3 dataform="YX" direct=BR PLOT="pg1"
pltgraf cube_perspective1 title="B via PLTGRAF - Az=0 elev=30" DIM=3 dataform="XY" direct=BR PLOT="pg2"
pltgraf cube_perspective2 title="B via PLTGRAF - Az=90 elev=30" DIM=3 direct=TR PLOT="pg3"
pltgraf cube_perspective3 title="B via PLTGRAF - Az=0 elev=60" DIM=3 direct=TR PLOT="pg4"

plot3d cube_perspective title="B" origin=(26,26,26) distance=100  plot="p3db1" 

end-proc
! Plot the cube default style to the printer and to an output file
!!!! plotting print		! plot to printronix printer
plot3d cube title="A" plot="p3da"  ! minimal perspective, diamond in upper-right corner
plot3d cube cube_perspective title="B" 
ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
pltgraf cube_perspective title="B via PLTGRAF" DIM=3 PLOT="p3db"
!
! Float around the cube, using the center of the cube as the origin
plot3d cube title="C" origin=(26,26,26) distance=1000 plot="p3dc" ! zoom in
plot3d cube title="D" origin=(26,26,26) distance=100 plot="p3dd"  ! zoom in
plot3d cube title="E" origin=(26,26,26) distance=100 +
    azimuth=90 plot="p3de"	! circle to right
plot3d cube title="F" origin=(26,26,26) distance=100 +
    azimuth=30 elev=60 plot="p3df"	! right & up
plot3d cube title="G" origin=(26,26,26) distance=40 +
    azimuth=0 elev=0 plot="p3dg"	! right,in,down
plot3d cube title="H" origin=(26,26,26) distance=100 +
    azimuth=360 elev=90 plot="p3dh"	! above
plot3d cube title="I" origin=(26,26,26) distance=100 +
    azimuth=360 elev=-90 plot="p3di"	! below
!
! Squeeze the cube from top to bottom (half size)
plot3d cube title="J" origin=(26,26,26) distance=100 zscale=2 plot="p3dj"
!
! Try a small paper version of the above (about 1 inch wide & half-inch high)
plot3d cube title="K" origin=(26,26,26) distance=100 zscale=2 scale=50 +
    plot="p3dk"
!
! Again, but shifted to the right 2 inches and up 1 inch
plot3d cube title="L" origin=(26,26,26) distance=100 zscale=2 +
    scale=50 plotoffs=(2,1) plot="p3dl"
!
! Repeat "K", but specifying a bounding box size rather than a plot scale
! factor. Plot should be 1 inch high & wide, not counting annotation.
plot3d cube title="M" origin=(26,26,26) distance=100 zscale=2 plotsize=1 +
    plot="p3dm"
! Make zscale larger than life (use autoscaling for plot)
plot3d cube title="N" origin=(26,26,26) distance=100 zscale=0.5 plot="p3dn"
!
! 10-unit axis & no title
plot3d cube axis=10 azimuth=20 plot="p3do"
!
! Again, but without parameter values or a bounding box as well
plot3d cube axis=10 azimuth=20 'noparm 'nobox plot="p3dp"
!
! Switch pens (can't see results on Printronix, however)
plot3d cube title="O" pen=5 plot="p3dq"
!
! Manipulate the program's interpretation of the data format
plot3d cube title="P: xyz" dataform=xyz	plot="p3dr" ! should be same as default (A)
plot3d cube title="P: yxz" dataform=yxz plot="p3ds"
plot3d cube title="P: lsz" dataform=lsz plot="p3dt"
!
! Print the plots
!!!! dcl print/nofeed/delete printronx.plt;*
!
disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstplot3d.log
ibis-gen cube.pic NC=3 NR=26 'IBIS-1 'ROW DATACOL=(1,2,3)  +
    data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51,  +
    1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0,  +
    51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0,  +
    36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
Beginning VICAR task ibis
    perspec cube.pic cube_perspective azimuth=0 elev=30 distance=100 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    perspec cube.pic cube_perspective1 azimuth=0 elev=30 distance=1000 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    perspec cube.pic cube_perspective2 azimuth=90 elev=30 distance=1000 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    perspec cube.pic cube_perspective3 azimuth=0 elev=60 distance=1000 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
plot3d cube.pic title="A" plot="p3da"
Beginning VICAR task plot3d
PLOT3D version 7-13-2013 (64-bit) - rjb
pltgraf cube_perspective  title="B via PLTGRAF - Az=0 elev=30" DIM=3 dataform="YX" direct=BR PLOT="pg1"
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
pltgraf cube_perspective1 title="B via PLTGRAF - Az=0 elev=30" DIM=3 dataform="XY" direct=BR PLOT="pg2"
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
pltgraf cube_perspective2 title="B via PLTGRAF - Az=90 elev=30" DIM=3 direct=TR PLOT="pg3"
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
pltgraf cube_perspective3 title="B via PLTGRAF - Az=0 elev=60" DIM=3 direct=TR PLOT="pg4"
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
plot3d cube_perspective title="B" origin=(26,26,26) distance=100  plot="p3db1"
Beginning VICAR task plot3d
PLOT3D version 7-13-2013 (64-bit) - rjb
end-proc
$ Return
$!#############################################################################
