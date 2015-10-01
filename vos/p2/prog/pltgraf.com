$!****************************************************************************
$!
$! Build proc for MIPL module pltgraf
$! VPACK Version 1.9, Tuesday, July 01, 2014, 12:13:30
$!
$! Execute by entering:		$ @pltgraf
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
$ write sys$output "*** module pltgraf ***"
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
$ write sys$output "Invalid argument given to pltgraf.com file -- ", primary
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
$   if F$SEARCH("pltgraf.imake") .nes. ""
$   then
$      vimake pltgraf
$      purge pltgraf.bld
$   else
$      if F$SEARCH("pltgraf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pltgraf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pltgraf.bld "STD"
$   else
$      @pltgraf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pltgraf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pltgraf.com -mixed -
	-s pltgraf.f -
	-i pltgraf.imake -
	-p pltgraf.pdf -
	-t tstpltgraf.pdf tstpltgraf_linux.log tstpltgraf_sun.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pltgraf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pltgraf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM pltgraf

   To Create the build file give the command:

		$ vimake pltgraf			(VMS)
   or
		% vimake pltgraf			(Unix)


************************************************************************/


#define PROGRAM	pltgraf
#define R2LIB

#define MODULE_LIST pltgraf.f 

#define FTNINC_LIST gnuplotchar

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create pltgraf.pdf
process      help=*
    parm INP      type=(string,80)  count=1:2
    PARM PLOT     type=STRING  COUNT=(0:1)     DEFAULT="pltgraf"
    PARM PLOTFMT TYPE=STRING  COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT

    parm TITLE    type=(string,120) default=""
    parm XLABEL   type=(string,60) default=""
    parm YLABEL   type=(string,60) default=""
    parm COMMENT  type=(string,70) default=""
    parm DATE     type=keyword valid=(NODATE,DATE) default=NODATE

    parm MODE     type=keyword valid=(AXIS,NOAXIS,NOBOX) default=AXIS

    parm XLEN     type=real  valid=(0.5:10.0)  default=8.0
    parm YLEN     type=real  valid=(0.5:10.0)  default=8.0

    parm XRANGE   type=real count=2 default=(0,1)
    parm YRANGE   type=real count=2 default=(0,1)
    parm ZRANGE   type=real count=2 default=(1,8)

    parm DATAFORM type=(string,2) valid=(YX,XY) default="XY"
    parm DIM      type=integer default=2 valid=2:3
    parm DIRECT   type=(string,2) valid=(BR,TR,TL,BL) default="BR"

    parm FINALPOS type=keyword valid=(CURR,NEXT) default=CURR

    parm DATACOLS type=integer valid=(1:100) count=(1:6) default=3
    parm FORMAT   type=(string,4) count=(1:11) default=" "
    parm XCOL     type=integer count=1 valid=(1:40) default=1
    parm YCOL     type=integer count=1 valid=(1:40) default=2
    parm HEIGHCOL type=integer count=1 valid=(1:40) default=1
    parm ANGLECOL type=integer count=1 valid=(1:40) default=1
    parm TSIZE	  type=real valid=(.0375:.6) default=.15
end-proc

.title
VICAR/IBIS Program "pltgraf"
.help
PURPOSE
    "PLTGRAF" plots an IBIS graphics file inside a labeled  box.  



OPERATION

    PLRTGRAF plots whatever information is given in an IBIS-1 graphics file.
    If a 3-D object is described in the graphics file, its viewpoint is
    in whatever perspective that the vertices are given. Thus, if the
    true 3-D is hidden by looking at an elev of 0 and Azimuth of 180, then
    the depth perception is lost.

    If a different perspective is desired then that graphics-1 file perspective
    can be changed by the program PERSPEC and the results from that transformation
    can be plotted by PLTGRAF.

    The window size and plot size can be selected or  automatic window sizing
    invoked.  Graphics outside of  the window are clipped.  Three
    dimensional graphics-1  files can also be plotted.  The format of the
    data in the file can be specified (XY or YX), and the direction of the
    axes on the plot can be chosen.   

    "PLTGRAF" can also plot attribute information such as text and numbers 
    from an (optional) IBIS interface file.  

.PAGE
EXECUTION
If the display parameters are set to defaults,

	pltgraf INP=file.gra

will plot the graphics file "pltgraf"

Display parameters:
                                title
            3.0 +--------|--------|--------|--------|
                |                                   |
            2.0 +                                   +
  y-axis anno.  |                                   |
            1.0 +                                   +
                |                                   |
            0.0 +--------|--------|--------|--------|
               0.0      1.0      2.0      3.0      4.0
                            x-axis anno.      comment

  text labeling data (default for each is no text):
    title="title"		Title (above data)
    xlabel="x-axis anno."	X-axis annotation (below data)
    ylabel="y-axis anno."	Y-axis annotation (to left of data)
    comment="comment"		Comment text (below data, right justified)
    'date			Date, in the form "day-mon-da, year",
                                prepended to comment text (comment not
				necessary to get date) (DATE parameter)

  box and numbers bordering data (MODE parameter; default is 'axis):
    'axis			Draw box, tick marks, and numbers
    'noaxis			Draw box only
    'nobox			No border around data

  plot size (defaults shown):
    xlen=4.0			Width of plot in inches, not counting text 
				and numbers outside of box
    ylen=3.0			Height of plot in inches, not counting text 
				and numbers outside of box

  window into data (default for each is to use min. & max. of data):
    xrange=(lower_x,upper_x)	Only points with x values in the range
				    lower_x <= x <= upper_x
				are displayed.
    yrange=(lower_y,upper_y)	Only points with y values in the range
				    lower_y <= y <= upper_y
				are displayed.
    zrange=(lower_z,upper_z)	Only points with z values in the range
				    lower_z <= z <= upper_z
				are displayed.

  data format (defaults shown):
    dataform="YX"		Data in file is in (y,x) or (y,x,z) format.
				"XY" indicates (x,y) or (x,y,z) format.
    dim=2			Number of dimensions for each data point--here,
				2d. dim=3 indicates 3d.
    direct="BR"			Direction of increasing x and y values--here,
				from top to bottom for y and from left to right
				for x. Other directions are "TR", "TL", "BL".

  control of final pen position (default shown):
    'curr			At the end of the plot, the cursor is positioned
				at the origin of the plot just finished. 'next
				positions the cursor 1 inch to the right of the 
				plot. (FINALPOS parameter)

  special annotation--only used if an interface file is supplied as the 
  second file in the INP parameter (defaults shown):
    datacols=3			A list of 1 to 6 numbers indicating the 
				columns in the interface file that contain
				text and/or numbers to be plotted.
    format="REAL,HALF,ASCI,     The format of the input data to be displayed.
           etc"	                One format for each column. The first two
                                formats must be present and represent the
                                format of the x and y value columns.  The
                                next two columns, if present, represent the
                                angle and size of the text.  Column 5 may 
                                be either a number to display or text columns
                                to display. A maximum of 20 characters 
                                (5 columns can be displayed).  The user may 
                                optionally plot both text and a value at an
                                x,y location.                 
    xcol=1			The number of the column that contains
				x-coordinate of the left edge of the text
				and/or numbers (before rotation).
    ycol=2			The number of the column that contains
				y-coordinate of the bottom edge of the text
				and/or numbers (before rotation).
    tsize=.15			Default text size if heighcol is defaulted.
    heighcol=1			The number of the column that contains the
				height for the text and/or numbers. If the
				default is taken, then a height of tsize is
				used for all rows in the interface file.
    anglecol=1			The number of the column that contains the
				CCW rotation angle--in degrees from horizontal--
				for the text and/or numbers. If the default is
				taken, then an angle of 0.0 degrees is used
				for all rows in the interface file.
.PAGE
IBIS GRAPHICS-1 FILES

    IBIS graphics-1 files are a way to store 2-dimensional or 3-dimensional
polygonal objects in a file. The file contains a list of the successive vertices
of the polygons. When the list contains a line of all zeroes then it terminates
that face of the polygon. In the view of a drawing program it would be a
"pen-up" command. 

    With the advent of the IBIS-2 format graphics-1 files are deprecated.
In the new IBIS-2 format graphics-1 can replace that format via an index
column. In the IBIS-2 context each row is associated with a single vertex,
and the row order in a column determines the sequence of plotting the
vertices. A pen-up type command would involve a change of the value in
the index column. 


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

  Note: This program creates multiple output plots per run. You bring up each plot
  panel sequentially. You close each plot by clicking the mouse on any
  portion of the plot.


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
    DEVELOPER Note:

    This program used to link to the XRT plot library -lxrt. Calls to
  that library were mitigated through a Calcomp conversion library,
  xrtps located in the p2 subroutine library. With the conversion to
  gnuplot, neither of these packages are used.

.PAGE
EXAMPLES

    pltgraf INP=FILE.GRA  TITLE="Map of an Unknown Land"  +
        XLEN=10  YLEN=5.0  +
	XRANGE=(-120,-115)  YRANGE=(34,36)  +
	XLABEL="LONGITUDE" YLABEL="LATITUDE"  DIRECT="TR" 

In this example the data window is (-120,-115) in longitude and (34,36) in
latitude, and the size of the plot is 10 by 5 inches. The direction of the axes
is set to top-right since this is a latitude-longitude plot. 


    pltgraf  FILE.GRA  XLEN=7 YLEN=3.5 'NOAXIS TITLE="Another Map" +
	XLABEL="south" 'DATE

In this example, the data window is found from the extent of the data so that
the whole file is displayed. Because of the 'NOAXIS keyword, a box is drawn
around the data but the axes are not labeled with numbers and no tick marks
are drawn. A title and the x-axis annotation are displayed; today's date is
drawn in the lower right corner of the plot.


    pltgraf  THREE.GRA  DIM=3  XRANGE=(1000,1400) YRANGE=(600,900) +
	ZRANGE=(0,10000)

This example demonstrates how to plot 3-D graphics files. The first two
dimensions are treated identically as with 2-D graphics files.


    ibis-gen LINES NC=3 NR=8 'IBIS-1 'ROW DATACOL=(1,2,3) DATA=( +
	 1,  1,  1,	 2,  2,  2,	 3,  3,  3,	 4,  4,  4,
	 5,  5,  5,	 6,  6,  6,	 7,  7,  7,	 8,  8,  8)
    pltgraf LINES DIM=3 'NOAXIS XLEN=8 YLEN=8 'DATE COMMENT=" test" +
	ZRANGE=(1,7)

In this plot, a box will be drawn around the data and the comment
"WED SEP 17, 1987 test" (current date) will be drawn in the lower right
corner of the plot just outside of the box. The last line segment,
(7,7) to (8,8), will not be drawn, however, because the second point's z
value--8--is outside of ZRANGE.


.PAGE
Optional Interface File Examples:

    pltgraf (PLOT.GRA,TEXT.INT) XCOL=1 YCOL=3 DATACOLS=(5,6,7,8) +
	FORMAT=("REAL","REAL","HALF","ASCI",'ASCI','ASCI') +
        HEIGHCOL=10 ANGLECOL=11 'NOBOX +
	COMMENT="Test annotation"

This example shows the use of an interface file to plot annotation. First, the
data in the first file will get plotted; no box, tick marks, axes number 
labels, axes annotation, or title will be plotted ('NOBOX + omission of
XLABEL, YLABEL, & TITLE). A comment will be drawn in the lower right corner
of the plot.

Next, the annotation from the interface file will be plotted.  The position of
the text to be plotted is in columns specified by XCOL and YCOL (these
positions are scaled to fit the plot in the same way that data points are
scaled). The DATACOLS specifies the interface columns that hold the text and
numbers to be plotted.  The data in the columns is specified with the 
FORMAT parameter. For this example, the first two columns (1,3) are real 
numbers and contain the x and y location at which the following datacols will
be plotted.  Column 5 contains a half word value to plotted at x,y and columns
6,7,8 contain associated text to be plotted at the same location.

.page
RESTRICTIONS
 
 1. Plotted text must not be longer than 60 characters.
 2. Interface file text may not be longer than 20 characters.

WRITTEN BY:                     Frank Evans
COGNIZANT PROGRAMMER:           Barbara McGuffie

REVISIONS:                       

    Feb     1986 Frank Evans   - Put in calls to standard IBIS
                                 graphics-1 file subroutines.
                                 Added 3-D option.
    Mar     1986 Frank Evans   - Allowed different coordinate systems
                                 Modified 3-D option.
    Jun     1986 Frank Evans   - Added interface attribute file
    Jan     1987 Frank Evans   - Added dataform parameter
    Jun     1987  Michael Tschudi Used call to SETGR to avoid opening graphics
                                  file twice (done to reset file to its first
                                  coordinate)
    Sep 17, 1987 Michael Tschudi 1. add PEN param
                                 2. add ZTOPEN param
                                 3. add COMMENT param
                                 4. add DATE param
                                 5. add FINALPOS param
                                 6. add NOBOX to the MODE param
                                 7. change formula for centering title from
                                        xlen/2.-0.25*.67*(slength(title)/2.+2)
                                    to
                                        (xlen-0.2490*slength(title))/2.0
                                 8. if xlabel specified along with 'NOAXIS or
                                    'NOBOX, then it is displayed without axis 
                                    numbers; same for ylabel
                                 9. change pen number calculation for scaled z
                                    from
                                        pen = int(z)
                                    to
                                        pen = nint(z)
                                    to avoid round-off error
                                10. fixed ZRANGE so that the range is checked
                                11. set plot to draw in same location 
                                    regardless of MODE switch value
                                12. fixed default FORMAT value
    Feb 23, 1990 Howard Frieden Check that interface file annotation is within
                                the plot area.  Also, center the annotation
                                and add TSIZE parameter.
    Jul 10, 1996 A. Scop (CRI) - Made portable for UNIX
    May  1, 1998 BAM           - Corrected subroutine PLOTTEXT 
                           which had not been ported properly.
    Feb 13, 2013 R. J. Bambery - Converted to gnuplot and 
                                gfortran 4.6.3 64-bit compatiability
    Feb 20, 2013 R. J. Bambery - Updated test and documentation
    Mar 15, 2013 R. J. Bambery - remove all debugging statements,
                                dataform is default=XY not YX
    Jul 07, 2013 R. J. Bambery - Renamed table file and fixed logic
                                in XY vs YX and TR, BR, TL, BL
                                (capitalization)  and ranges in gpi files
    Jul 13, 2013 R. J. Bambery - Adjusted eps format to more readable fonts

.LEVEL1
.VARIABLE INP
 1. Input IBIS graphics-1 file
 2. Optional interface file with
    special annotation
.VARIABLE PLOT
 STRING-OPTIONAL
 Turns on PLOT.
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE TITLE
String for title
.VARIABLE XLABEL
String for x-axis annotation
.VARIABLE YLABEL
String for y-axis annotation
.VARIABLE COMMENT
String for lower-right corner
annotation
.VARIABLE DATE
Switch indicating that the curr.
date should be prepended to 
COMMENT ('DATE) or not ('NODATE)

.VARIABLE MODE
Switch indicating level of 
border detail around data:
  'AXIS: box, ticks, numbers
  'NOAXIS: box only
  'NOBOX: nothing

.VARIABLE XLEN
Length of x-axis in inches
.VARIABLE YLEN
Length of y-axis in inches

.VARIABLE XRANGE
Range for x-variable
.VARIABLE YRANGE
Range for y-variable
.VARIABLE ZRANGE
Range for z-variable

.VARIABLE DATAFORM
File format:  XY, YX.
.VARIABLE DIM
Dimension of graphics
file (2 or 3)
.VARIABLE DIRECT
Increasing direction for axes:
  TR for top right
  BR for bottom right
  TL for top left
  BL for bottom left

.VARIABLE FINALPOS
Position for pen following plot:
  'CURR: at origin of plot (for
    overprinting)
  'NEXT: to right of plot (for
    adjacent plot)

.VARIABLE DATACOLS
Columns (up to 10) that hold the
text or numbers to be plotted
.VARIABLE FORMAT
String containing IBIS FORMAT
statement to use to format the 
data columns (parentheses req'd)
.VARIABLE XCOL
Column number that holds left-
edge x coords for text/numbers
(before rotation)
.VARIABLE YCOL
Column number that holds bottom-
edge y coords for text/numbers
(before rotation)
.VARIABLE TSIZE
Text size in inches when 
HEIGHCOLis defaulted. 
Default is .15 
.VARIABLE HEIGHCOL
Column number that holds the
height of the text in inches.
(Default yields TSIZE inches)
.VARIABLE ANGLECOL
Column number that holds the
angle of the text in degrees.
(Default yields 0 degrees)

.LEVEL2
.VARIABLE INP
    Type:	string, 72 characters
    Count:	1 or 2
    Valid:	any
    Default:	none
.VARIABLE PLOT
    STRING-OPTIONAL
    Specifies the filename to 
    receive the output plot data.
.VARIABLE PLOTFMT
    STRING-OPTIONAL
    Output plot format
    GNUPLOT or EPS

.VARIABLE TITLE
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE XLABEL
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE YLABEL
    Type:	string, 12 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE COMMENT
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE DATE
    Type:	keyword
    Count:	1
    Valid:	'NODATE, 'DATE
    Default:	'NODATE

.VARIABLE MODE
    Type:	keyword
    Count:	1
    Valid:	'AXIS, 'NOAXIS, 'NOBOX
    Default:	'AXIS

.VARIABLE XLEN
    Type:	real
    Count:	1
    Valid:	0.5 to 10.0 inches
    Default:	4.0
.VARIABLE YLEN
    Type:	real
    Count:	1
    Valid:	0.5 to 10.0 inches
    Default:	3.0

.VARIABLE XRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)
.VARIABLE YRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)
.VARIABLE ZRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)

.VARIABLE DATAFORM
    The DATAFORM parameter specifies the way to assign the coordinates in
    the file to the x and y axes of the plot.  The default (YX) uses the
    first coordinate for the Y axis and the second for the X axis. 

    Type:	string, 2 characters
    Count:	1
    Valid:	"YX", "XY"
    Default:	"YX"
.VARIABLE DIM
    Type:	integer
    Count:	1
    Valid:	2 or 3
    Default:	2
.VARIABLE DIRECT
    Type:	string, 2 characters
    Count:	1
    Valid:	"BR", "TR", "TL", "BL"
    Default:	"BR"

.VARIABLE FINALPOS
    Type:	keyword
    Count:	1
    Valid:	'CURR or 'NEXT
    Default:	'CURR

.VARIABLE DATACOLS
    Type:	integer
    Count:	1 to 10
    Valid:	1 to 40
    Default:	3
.VARIABLE FORMAT
    Type:	string, 4 characters
    Count:	1:100
    Valid:	IBIS format information enclosed in parentheses
    Default:	none
.VARIABLE XCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1
.VARIABLE YCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	2
.VARIABLE TSIZE
    Type:	real
    Count:	1
    Valid:	.0075 to 3.0
    Default:	.15
.VARIABLE HEIGHCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1 (if defaulted, then a height of TSIZE inches is 
		used for all rows)
.VARIABLE ANGLECOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1 (if defaulted, then an angle of 0.0 degrees is
		used for all rows)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpltgraf.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

! Dec 15, 2012 - RJB
! TEST SCRIPT FOR PLTGRAF
!
! Vicar Programs:
!       ibis-gen mf3 ibis-list ibis2asc
!
! External programs
!       gnuplot 4.6.x, gimp 2.6
! 
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display but creates .eps files
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
! External test data: 
!       <none>
! Output:    
!   GENed test data sets, .gpi and .eps files and intermediate 
!       tmp* files 
!   the *.gpi data produced by statplt are gnuplot scripts
!
    refgbl $autousage
    refgbl $echo
body
    let $autousage="none"
    let _onfail="stop"
    let $echo="no"


!write "In order to create files for plotting, click the left mouse button"
!write "on the save button in the upper left hand corner of the plot"

let $echo="yes"
! Create a polygon consisting of a cube with a diamond to one side; this
! is an attempt to make a shape that will have a recognizable orientation.
! using IBIS-1 graphics file format
    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3) +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51, +
	1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0, +
	51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0, +
	36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)

! TEST 1A, 1B, 1C, and 1D - plot genned data
!              using dataform=YX and direct=(BR,TR,BL,TL) 
! Plot the cube default style to an output file, then use pltgraf to display
! the file
!  default x- and y-graph dimensions is 2*max values
!  4 test cases of displaying x vs y and reversed orientations

! Note: xrt graph created a file with no values on the coordinate axes
!       It's plot has z and x oriented pos to neg but y-axis neg on top
!       pos on bottom. Case Test 1A gives results same as xrtgraph Test 1
    pltgraf cube.pic title="Test 1A - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BR" dim=3 +
           dataform=YX plot=test1a

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1a.gpi
end-if

    pltgraf cube.pic title="Test 1B - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TR" dim=3 +
           dataform=YX direct=TR plot=test1b

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1b.gpi
end-if

    pltgraf cube.pic title="Test 1C - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BL" dim=3 +
           dataform=YX direct=BL plot=test1c

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1c.gpi
end-if

    pltgraf cube.pic title="Test 1D - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TL" dim=3 +
           dataform=YX direct=TL plot=test1d

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1d.gpi
end-if

! TEST 2A, 2B, 2C, and 2D - plot genned data
!              using dataform=XY and direct=(BR,TR,BL,TL) 
! Plot the cube default style to an output file, then use pltgraf to display
! the file

    pltgraf cube.pic title="Test 2A - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BR" dim=3 +
           dataform=XY direct=BR plot=test2a

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2a.gpi
end-if


    pltgraf cube.pic title="Test 2B - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TR" dim=3 +
           dataform=XY direct=TR plot=test2b

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2b.gpi
end-if

    pltgraf cube.pic title="Test 2C - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BL" dim=3 +
           dataform=XY direct=BL plot=test2c

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2c.gpi
end-if

    pltgraf cube.pic title="Test 2D - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TL" dim=3 +
           dataform=XY direct=TL plot=test2d

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2d.gpi
end-if

! TEST 3 - reorient data and plot with pltgraf
!       default parameters
!       bottom part of graph truncated
!       Test 3A is same as Test 2 in xrt graph
    perspec cube.pic cube_perspective azimuth=30 elev=30 distance=100 origin=(26,26,26)
!    plot3d cube.pic cube_perspective azimuth=30 distance=100 origin=(26,26,26)
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3

    pltgraf cube_perspective title="Test 3A - Perspective cube - AZ = 30, EL = 30, Plot=YX - BR" dim=3 +
            dataform=YX plot=test3a

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3a.gpi
end-if

    pltgraf cube_perspective title="Test 3B - Perspective cube - AZ = 30, EL = 30, Plot=XY - BR" dim=3 +
            dataform=XY direct=BR plot=test3b

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3b.gpi
end-if

! TEST 4 - smaller graph - polygon circumscribed in graph - date
! Plot the cube as a flat polygon; the z-values are used as pen numbers
! after partitioning into 1 of 3, 4, 5, or 6 groups
! dataform xy
!  This is TEST 3 in xrt graph
    pltgraf cube_perspective title="Test 4 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR" +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0 +
          dataform=xy direct=tr 'date plot=test4

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
end-if
! TEST 5 -
! Plot the cube as a flat polygon;
! dataform yx - reversed from above - bottom truncated
!   This is TEST 4 in xrt graph
    pltgraf cube_perspective title="Test 5 - Perspective cube - AZ = 30, EL = 30, Plot=YX - TR" +
         dim=3 comment="comment" xlen=5.0 ylen=5.0 +
         dataform=yx direct=tr comment="test5" plot=test5
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test5.gpi
end-if
! TEST 6 - Elongated graph - circumscribed
!   This is TEST 5 in xrt graph
    pltgraf cube_perspective title="Test 6 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR" +
         dim=3 'date xlen=6.0 ylen=3.0 +
        dataform=xy direct=tr comment="test6"  plot=test6

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test6.gpi
end-if   


! Run tests in which the z-value is directly interpreted as a pen number
    ibis-gen latlong.pic NC=3 NR=28 'ibis-1 'row DATACOL=(1,2,3) data=( +
     159.2450,  152.5256, 1.0000,  176.0350,  788.9391, 1.0000, +
     183.6950, 1425.4431, 1.0000,  182.2350, 2061.9700, 1.0000, +
     171.6550, 2698.4524, 1.0000,  151.9450, 3334.8225, 2.0000, +
     123.1000, 3971.0132, 2.0000,   85.1200, 4606.9551, 2.0000, +
      37.9850, 5242.5796, 2.0000,    0.0000,    0.0000, 0.0000, +
    4274.6050,   17.6281, 6.0000, 4292.0552,  711.7065, 6.0000, +
    4300.0200, 1405.8488, 6.0000, 4298.5000, 2100.0068, 6.0000, +
    4287.5000, 2794.1338, 6.0000, 4267.0151, 3488.1819, 5.0000, +
    4237.0298, 4182.1030, 5.0000, 4197.5400, 4875.8501, 5.0000, +
    4148.5249, 5569.3721, 5.0000,    0.0000,    0.0000, 0.0000, +
     159.2450,  152.5256, 8.0000, 2216.9250,   85.0768, 8.0000, +
    4274.6050,   17.6281, 7.0000,    0.0000,    0.0000, 0.0000, +
      37.9850, 5242.5796, 3.0000, 2093.2550, 5405.9759, 3.0000, +
    4148.5249, 5569.3721, 4.0000,    0.0000,    0.0000, 0.0000)


! TEST 7 - Interpret values as pen numbers - no axes
!       
!   This is Test 6 in xrt graph
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
    xlabel="xlabel" ylabel="ylabel" comment="no axis version" +
    title="Test 7 - abcdefghijklmn" dim=3 'noaxis dataform=yx plot=test7
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test7.gpi
end-if

! TEST 8 - Interpret values as pen numbers - no axes
!
!   This is Test 7 in xrt graph
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
    xlabel="" ylabel="" 'date dataform=yx +
    title="Test 8 - ABCDEFGHIJKLMN" dim=3 'noaxis plot=test8

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test8.gpi
end-if

! TEST 9 - Interpret values as pen numbers - with axes
!       wider x,y ranges
!
!   Test 8 in xrt graph, but with labels and axes 
    pltgraf latlong.pic xran=(0,7500) yran=(0,7500) xlen=6 ylen=6 +
    xlabel="xlabel" ylabel="ylabel" 'date comment="test9" dataform=YX +
    title="Test 9 - ABCDEFGHIJKLMN" dim=3 plot=test9

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test9.gpi
end-if

! TEST 10 -
!
! Different than Test 9 in xrt graph
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
    xlabel="xlabel" ylabel="ylabel" 'date zran=(1,5) +
    title="Test 10 - abcdefghijklmn" dim=3 direct=tr plot=test10

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test10.gpi
end-if

! TEST 11 - Plot successive lines - no axes
!
!   Test 10 in xrt graph - but with points - no way in current
!   code to suppress points
    ibis-gen lines.pic NC=3 NR=8 'ibis-1 'row DATACOL=(1,2,3) data=( +
     1,  1,  1,  2,  2,  2,  3,  3,  3,  4,  4,  4, +
     5,  5,  5,  6,  6,  6,  7,  7,  7,  8,  8,  8)

    pltgraf lines.pic dim=3 'noaxis xlen=8 ylen=8 'date comment=" TEST 11 - No Title " +
         zrange=(1,7) plot=test11

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test11.gpi
end-if

! TEST 12 - Add ascii notation 
!
! Test that multiple plots are properly aligned when plotted together
! The following three plots should be done on the same page if possible
    ibis-gen latlong2.pic NC=2 NR=26 'ibis-1 'row DATACOL=(1,2) data=( +
     159.2450,  152.5256,  176.0350,  788.9391, +
     183.6950, 1425.4431,  182.2350, 2061.9700, +
     171.6550, 2698.4524,  151.9450, 3334.8225, +
     123.1000, 3971.0132,   85.1200, 4606.9551, +
      37.9850, 5242.5796,    0.0000,    0.0000, +
    4274.6050,   17.6281, 4292.0552,  711.7065, +
    4300.0200, 1405.8488, 4298.5000, 2100.0068, +
    4287.5000, 2794.1338, 4267.0151, 3488.1819, +
    4237.0298, 4182.1030, 4197.5400, 4875.8501, +
    4148.5249, 5569.3721,    0.0000,    0.0000, +
     159.2450,  152.5256, 4274.6050,   17.6281, +
       0.0000,    0.0000,   37.9850, 5242.5796, +
    4148.5249, 5569.3721,    0.0000,    0.0000)
!   string=("Test text1","Test text2") 

    ibis-gen test.int  NC=5 NR=1 +
      format=("REAL","REAL","A20","REAL","REAL") strcols=(3) +
      datacols=(1,2,4,5) data=(2400,2400,10,0) +
       string=("This is It")

    ibis-li test.int

!   Test 12 is same as Test 11 in xrt graph
!
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
    xlabel="Longitude" ylabel="Latitude" dataform=YX +
    title="TEST 12" dim=3  plot=test12

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test12.gpi
end-if

! TEST 13 - Add ascii notation
!
! Test with annotated interface file as second input  with TEXT 
! Not same as Test 13 in xrt graph - xrt graph put no annotation on graph
! even though requested         
    pltgraf (latlong2.pic,test.int) xran=(0,5500) yran=(0,5500) xlen=6 +
    ylen=6 'curr title="Test 13 - test inferface file with ASCII annotation" datacols=3 +
    heighcol=4 anglecol=5 xcol=1 ycol=2 +
        format=("REAL","REAL","ASCI","REAL","REAL") plot=test13
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test13.gpi
end-if

! TEST 14 - Add numeric notation
!
! Test 14 in xrt graph put no notation on graph even though requested
    ibis-gen test2.int  NC=5 NR=1 +
      format=("REAL","REAL","REAL","REAL","REAL")  +
      datacols=(1,2,3,4,5) data=(2000,2400,10,0,33.33) 

    pltgraf (latlong2.pic,test2.int) xran=(0,5500) yran=(0,5500) xlen=6 +
    ylen=6 'curr title="Test 14 - test inferface file with numeric annotation" datacols=5 +
    heighcol=3 anglecol=4 xcol=1 ycol=2 +
        format=("REAL","REAL","REAL","REAL","REAL") plot=test14

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test14.gpi
end-if

! TEST 15 - No axes - no notation
!
!   No equivalent test in xrt graph
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr +
    title=" TEST 15  - No Axes" xlabel=" I  noax  I" ylabel=" I noax I" +
    'noaxis comment=" I  noax  I" plot=test15

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test15.gpi
end-if

! TEST 16 -
!
!  Test 16 in xrt only had bounding box
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr +
    title=" TEST 16 - No Box" xlabel=" I  nobxI" ylabel=" I  nobxI" +
    'nobox comment=" I  nobxI" plot=test16

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test16.gpi
end-if

! TEST 17 - Data out of range
!
!   only bounding box with axes
    pltgraf latlong.pic xran=(0,1) yran=(0,1) xlen=6 ylen=6 'curr +
    title=" TEST 17  " xlabel=" Iaxis     I" ylabel=" Iaxis  I" +
    comment=" Iaxis  I" plot=test17

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test17.gpi
end-if

! TEST 18 - 
!
! Rotated XY version of Test 18 of xrt graph 

    pltgraf latlong.pic xran=(0,1) yran=(0,5500) xlen=6 ylen=6 'curr +
    title=" TEST 18  " xlabel=" Iaxis  I" ylabel=" Iaxis  I" +
    comment=" Iaxis  I" direct=tr plot=test18

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test18.gpi
end-if

! TEST 19  - same as Test 4 - smaller graph - polygon circumscribed in graph - date
! Plot the cube as a flat polygon; the z-values are used as pen numbers
! after partitioning into 1 of 3, 4, 5, or 6 groups
! dataform xy - eps output
!  This is TEST 3 in xrt graph
    pltgraf cube_perspective title="Test 19 - (Test 4) Perspective cube - AZ = 30, EL = 30, Plot=XY - TR" +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0 +
          dataform=xy direct=tr 'date plot=test19 plotfmt=eps

 ush gnuplot test19.eps.gpi
let $echo="no"
end-proc

$!-----------------------------------------------------------------------------
$ create tstpltgraf_linux.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3)  +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51,  +
	1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0,  +
	51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0,  +
	36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
Beginning VICAR task ibis
    pltgraf cube.pic title="Test 1A - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BR" dim=3  +
           dataform=YX plot=test1a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1B - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TR" dim=3  +
           dataform=YX direct=TR plot=test1b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1C - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BL" dim=3  +
           dataform=YX direct=BL plot=test1c
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1D - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TL" dim=3  +
           dataform=YX direct=TL plot=test1d
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2A - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BR" dim=3  +
           dataform=XY direct=BR plot=test2a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2B - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TR" dim=3  +
           dataform=XY direct=TR plot=test2b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2C - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BL" dim=3  +
           dataform=XY direct=BL plot=test2c
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2D - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TL" dim=3  +
           dataform=XY direct=TL plot=test2d
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    perspec cube.pic cube_perspective azimuth=30 elev=30 distance=100 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:26
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        6.44       -3.22        0.00
       32.66      -25.08        0.00
      -11.03      -46.70        0.00
      -28.36      -14.18        0.00
        6.44       -3.22        0.00
        7.82       33.08        0.00
       42.92       21.46        0.00
      -15.80        7.90        0.00
      -35.79       27.48        0.00
        7.82       33.08        0.00
        0.00        0.00        0.00
       32.66      -25.08        0.00
       42.92       21.46        0.00
        0.00        0.00        0.00
      -11.03      -46.70        0.00
      -15.80        7.90        0.00
        0.00        0.00        0.00
      -28.36      -14.18        0.00
      -35.79       27.48        0.00
        0.00        0.00        0.00
       -1.37      -10.66        0.00
       -7.11       -2.05        0.00
       -1.56       11.19        0.00
        3.89        1.12        0.00
       -1.37      -10.66        0.00
        0.00        0.00        0.00
    pltgraf cube_perspective title="Test 3A - Perspective cube - AZ = 30, EL = 30, Plot=YX - BR" dim=3  +
            dataform=YX plot=test3a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 3B - Perspective cube - AZ = 30, EL = 30, Plot=XY - BR" dim=3  +
            dataform=XY direct=BR plot=test3b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 4 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0  +
          dataform=xy direct=tr 'date plot=test4
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 5 - Perspective cube - AZ = 30, EL = 30, Plot=YX - TR"  +
         dim=3 comment="comment" xlen=5.0 ylen=5.0  +
         dataform=yx direct=tr comment="test5" plot=test5
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 6 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
         dim=3 'date xlen=6.0 ylen=3.0  +
        dataform=xy direct=tr comment="test6"  plot=test6
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen latlong.pic NC=3 NR=28 'ibis-1 'row DATACOL=(1,2,3) data=(  +
     159.2450,  152.5256, 1.0000,  176.0350,  788.9391, 1.0000,  +
     183.6950, 1425.4431, 1.0000,  182.2350, 2061.9700, 1.0000,  +
     171.6550, 2698.4524, 1.0000,  151.9450, 3334.8225, 2.0000,  +
     123.1000, 3971.0132, 2.0000,   85.1200, 4606.9551, 2.0000,  +
      37.9850, 5242.5796, 2.0000,    0.0000,    0.0000, 0.0000,  +
    4274.6050,   17.6281, 6.0000, 4292.0552,  711.7065, 6.0000,  +
    4300.0200, 1405.8488, 6.0000, 4298.5000, 2100.0068, 6.0000,  +
    4287.5000, 2794.1338, 6.0000, 4267.0151, 3488.1819, 5.0000,  +
    4237.0298, 4182.1030, 5.0000, 4197.5400, 4875.8501, 5.0000,  +
    4148.5249, 5569.3721, 5.0000,    0.0000,    0.0000, 0.0000,  +
     159.2450,  152.5256, 8.0000, 2216.9250,   85.0768, 8.0000,  +
    4274.6050,   17.6281, 7.0000,    0.0000,    0.0000, 0.0000,  +
      37.9850, 5242.5796, 3.0000, 2093.2550, 5405.9759, 3.0000,  +
    4148.5249, 5569.3721, 4.0000,    0.0000,    0.0000, 0.0000)
Beginning VICAR task ibis
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" comment="no axis version"  +
    title="Test 7 - abcdefghijklmn" dim=3 'noaxis dataform=yx plot=test7
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="" ylabel="" 'date dataform=yx  +
    title="Test 8 - ABCDEFGHIJKLMN" dim=3 'noaxis plot=test8
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,7500) yran=(0,7500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" 'date comment="test9" dataform=YX  +
    title="Test 9 - ABCDEFGHIJKLMN" dim=3 plot=test9
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" 'date zran=(1,5)  +
    title="Test 10 - abcdefghijklmn" dim=3 direct=tr plot=test10
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen lines.pic NC=3 NR=8 'ibis-1 'row DATACOL=(1,2,3) data=(  +
     1,  1,  1,  2,  2,  2,  3,  3,  3,  4,  4,  4,  +
     5,  5,  5,  6,  6,  6,  7,  7,  7,  8,  8,  8)
Beginning VICAR task ibis
    pltgraf lines.pic dim=3 'noaxis xlen=8 ylen=8 'date comment=" TEST 11 - No Title "  +
         zrange=(1,7) plot=test11
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen latlong2.pic NC=2 NR=26 'ibis-1 'row DATACOL=(1,2) data=(  +
     159.2450,  152.5256,  176.0350,  788.9391,  +
     183.6950, 1425.4431,  182.2350, 2061.9700,  +
     171.6550, 2698.4524,  151.9450, 3334.8225,  +
     123.1000, 3971.0132,   85.1200, 4606.9551,  +
      37.9850, 5242.5796,    0.0000,    0.0000,  +
    4274.6050,   17.6281, 4292.0552,  711.7065,  +
    4300.0200, 1405.8488, 4298.5000, 2100.0068,  +
    4287.5000, 2794.1338, 4267.0151, 3488.1819,  +
    4237.0298, 4182.1030, 4197.5400, 4875.8501,  +
    4148.5249, 5569.3721,    0.0000,    0.0000,  +
     159.2450,  152.5256, 4274.6050,   17.6281,  +
       0.0000,    0.0000,   37.9850, 5242.5796,  +
    4148.5249, 5569.3721,    0.0000,    0.0000)
Beginning VICAR task ibis
    ibis-gen test.int  NC=5 NR=1  +
      format=("REAL","REAL","A20","REAL","REAL") strcols=(3)  +
      datacols=(1,2,4,5) data=(2400,2400,10,0)  +
       string=("This is It")
Beginning VICAR task ibis
    ibis-li test.int
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
     2400.00     2400.00  This is It       10.00        0.00
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="Longitude" ylabel="Latitude" dataform=YX  +
    title="TEST 12" dim=3  plot=test12
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf (latlong2.pic,test.int) xran=(0,5500) yran=(0,5500) xlen=6  +
    ylen=6 'curr title="Test 13 - test inferface file with ASCII annotation" datacols=3  +
    heighcol=4 anglecol=5 xcol=1 ycol=2  +
        format=("REAL","REAL","ASCI","REAL","REAL") plot=test13
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen test2.int  NC=5 NR=1  +
      format=("REAL","REAL","REAL","REAL","REAL")   +
      datacols=(1,2,3,4,5) data=(2000,2400,10,0,33.33)
Beginning VICAR task ibis
    pltgraf (latlong2.pic,test2.int) xran=(0,5500) yran=(0,5500) xlen=6  +
    ylen=6 'curr title="Test 14 - test inferface file with numeric annotation" datacols=5  +
    heighcol=3 anglecol=4 xcol=1 ycol=2  +
        format=("REAL","REAL","REAL","REAL","REAL") plot=test14
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 15  - No Axes" xlabel=" I  noax  I" ylabel=" I noax I"  +
    'noaxis comment=" I  noax  I" plot=test15
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 16 - No Box" xlabel=" I  nobxI" ylabel=" I  nobxI"  +
    'nobox comment=" I  nobxI" plot=test16
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,1) yran=(0,1) xlen=6 ylen=6 'curr  +
    title=" TEST 17  " xlabel=" Iaxis     I" ylabel=" Iaxis  I"  +
    comment=" Iaxis  I" plot=test17
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,1) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 18  " xlabel=" Iaxis  I" ylabel=" Iaxis  I"  +
    comment=" Iaxis  I" direct=tr plot=test18
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 19 - (Test 4) Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0  +
          dataform=xy direct=tr 'date plot=test19 plotfmt=eps
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 ush gnuplot test19.eps.gpi
let $echo="no"
$!-----------------------------------------------------------------------------
$ create tstpltgraf_sun.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3)  +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51,  +
	1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0,  +
	51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0,  +
	36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
Beginning VICAR task ibis
    pltgraf cube.pic title="Test 1A - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BR" dim=3  +
           dataform=YX plot=test1a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1B - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TR" dim=3  +
           dataform=YX direct=TR plot=test1b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1C - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - BL" dim=3  +
           dataform=YX direct=BL plot=test1c
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 1D - Direct view of a cube - AZ = 180, EL = 0, Plot=YX - TL" dim=3  +
           dataform=YX direct=TL plot=test1d
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2A - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BR" dim=3  +
           dataform=XY direct=BR plot=test2a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2B - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TR" dim=3  +
           dataform=XY direct=TR plot=test2b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2C - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - BL" dim=3  +
           dataform=XY direct=BL plot=test2c
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube.pic title="Test 2D - Direct view of a cube - AZ = 180, EL = 0, Plot=XY - TL" dim=3  +
           dataform=XY direct=TL plot=test2d
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    perspec cube.pic cube_perspective azimuth=30 elev=30 distance=100 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:26
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        6.44       -3.22        0.00
       32.66      -25.08        0.00
      -11.03      -46.70        0.00
      -28.36      -14.18        0.00
        6.44       -3.22        0.00
        7.82       33.08        0.00
       42.92       21.46        0.00
      -15.80        7.90        0.00
      -35.79       27.48        0.00
        7.82       33.08        0.00
        0.00        0.00        0.00
       32.66      -25.08        0.00
       42.92       21.46        0.00
        0.00        0.00        0.00
      -11.03      -46.70        0.00
      -15.80        7.90        0.00
        0.00        0.00        0.00
      -28.36      -14.18        0.00
      -35.79       27.48        0.00
        0.00        0.00        0.00
       -1.37      -10.66        0.00
       -7.11       -2.05        0.00
       -1.56       11.19        0.00
        3.89        1.12        0.00
       -1.37      -10.66        0.00
        0.00        0.00        0.00
    pltgraf cube_perspective title="Test 3A - Perspective cube - AZ = 30, EL = 30, Plot=YX - BR" dim=3  +
            dataform=YX plot=test3a
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 3B - Perspective cube - AZ = 30, EL = 30, Plot=XY - BR" dim=3  +
            dataform=XY direct=BR plot=test3b
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 4 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0  +
          dataform=xy direct=tr 'date plot=test4
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 5 - Perspective cube - AZ = 30, EL = 30, Plot=YX - TR"  +
         dim=3 comment="comment" xlen=5.0 ylen=5.0  +
         dataform=yx direct=tr comment="test5" plot=test5
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 6 - Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
         dim=3 'date xlen=6.0 ylen=3.0  +
        dataform=xy direct=tr comment="test6"  plot=test6
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen latlong.pic NC=3 NR=28 'ibis-1 'row DATACOL=(1,2,3) data=(  +
     159.2450,  152.5256, 1.0000,  176.0350,  788.9391, 1.0000,  +
     183.6950, 1425.4431, 1.0000,  182.2350, 2061.9700, 1.0000,  +
     171.6550, 2698.4524, 1.0000,  151.9450, 3334.8225, 2.0000,  +
     123.1000, 3971.0132, 2.0000,   85.1200, 4606.9551, 2.0000,  +
      37.9850, 5242.5796, 2.0000,    0.0000,    0.0000, 0.0000,  +
    4274.6050,   17.6281, 6.0000, 4292.0552,  711.7065, 6.0000,  +
    4300.0200, 1405.8488, 6.0000, 4298.5000, 2100.0068, 6.0000,  +
    4287.5000, 2794.1338, 6.0000, 4267.0151, 3488.1819, 5.0000,  +
    4237.0298, 4182.1030, 5.0000, 4197.5400, 4875.8501, 5.0000,  +
    4148.5249, 5569.3721, 5.0000,    0.0000,    0.0000, 0.0000,  +
     159.2450,  152.5256, 8.0000, 2216.9250,   85.0768, 8.0000,  +
    4274.6050,   17.6281, 7.0000,    0.0000,    0.0000, 0.0000,  +
      37.9850, 5242.5796, 3.0000, 2093.2550, 5405.9759, 3.0000,  +
    4148.5249, 5569.3721, 4.0000,    0.0000,    0.0000, 0.0000)
Beginning VICAR task ibis
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" comment="no axis version"  +
    title="Test 7 - abcdefghijklmn" dim=3 'noaxis dataform=yx plot=test7
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="" ylabel="" 'date dataform=yx  +
    title="Test 8 - ABCDEFGHIJKLMN" dim=3 'noaxis plot=test8
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,7500) yran=(0,7500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" 'date comment="test9" dataform=YX  +
    title="Test 9 - ABCDEFGHIJKLMN" dim=3 plot=test9
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="xlabel" ylabel="ylabel" 'date zran=(1,5)  +
    title="Test 10 - abcdefghijklmn" dim=3 direct=tr plot=test10
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen lines.pic NC=3 NR=8 'ibis-1 'row DATACOL=(1,2,3) data=(  +
     1,  1,  1,  2,  2,  2,  3,  3,  3,  4,  4,  4,  +
     5,  5,  5,  6,  6,  6,  7,  7,  7,  8,  8,  8)
Beginning VICAR task ibis
    pltgraf lines.pic dim=3 'noaxis xlen=8 ylen=8 'date comment=" TEST 11 - No Title "  +
         zrange=(1,7) plot=test11
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen latlong2.pic NC=2 NR=26 'ibis-1 'row DATACOL=(1,2) data=(  +
     159.2450,  152.5256,  176.0350,  788.9391,  +
     183.6950, 1425.4431,  182.2350, 2061.9700,  +
     171.6550, 2698.4524,  151.9450, 3334.8225,  +
     123.1000, 3971.0132,   85.1200, 4606.9551,  +
      37.9850, 5242.5796,    0.0000,    0.0000,  +
    4274.6050,   17.6281, 4292.0552,  711.7065,  +
    4300.0200, 1405.8488, 4298.5000, 2100.0068,  +
    4287.5000, 2794.1338, 4267.0151, 3488.1819,  +
    4237.0298, 4182.1030, 4197.5400, 4875.8501,  +
    4148.5249, 5569.3721,    0.0000,    0.0000,  +
     159.2450,  152.5256, 4274.6050,   17.6281,  +
       0.0000,    0.0000,   37.9850, 5242.5796,  +
    4148.5249, 5569.3721,    0.0000,    0.0000)
Beginning VICAR task ibis
    ibis-gen test.int  NC=5 NR=1  +
      format=("REAL","REAL","A20","REAL","REAL") strcols=(3)  +
      datacols=(1,2,4,5) data=(2400,2400,10,0)  +
       string=("This is It")
Beginning VICAR task ibis
    ibis-li test.int
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
     2400.00     2400.00  This is It       10.00        0.00
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6  +
    xlabel="Longitude" ylabel="Latitude" dataform=YX  +
    title="TEST 12" dim=3  plot=test12
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf (latlong2.pic,test.int) xran=(0,5500) yran=(0,5500) xlen=6  +
    ylen=6 'curr title="Test 13 - test inferface file with ASCII annotation" datacols=3  +
    heighcol=4 anglecol=5 xcol=1 ycol=2  +
        format=("REAL","REAL","ASCI","REAL","REAL") plot=test13
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    ibis-gen test2.int  NC=5 NR=1  +
      format=("REAL","REAL","REAL","REAL","REAL")   +
      datacols=(1,2,3,4,5) data=(2000,2400,10,0,33.33)
Beginning VICAR task ibis
    pltgraf (latlong2.pic,test2.int) xran=(0,5500) yran=(0,5500) xlen=6  +
    ylen=6 'curr title="Test 14 - test inferface file with numeric annotation" datacols=5  +
    heighcol=3 anglecol=4 xcol=1 ycol=2  +
        format=("REAL","REAL","REAL","REAL","REAL") plot=test14
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 15  - No Axes" xlabel=" I  noax  I" ylabel=" I noax I"  +
    'noaxis comment=" I  noax  I" plot=test15
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong2.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 16 - No Box" xlabel=" I  nobxI" ylabel=" I  nobxI"  +
    'nobox comment=" I  nobxI" plot=test16
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,1) yran=(0,1) xlen=6 ylen=6 'curr  +
    title=" TEST 17  " xlabel=" Iaxis     I" ylabel=" Iaxis  I"  +
    comment=" Iaxis  I" plot=test17
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf latlong.pic xran=(0,1) yran=(0,5500) xlen=6 ylen=6 'curr  +
    title=" TEST 18  " xlabel=" Iaxis  I" ylabel=" Iaxis  I"  +
    comment=" Iaxis  I" direct=tr plot=test18
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
if (mode = "nobatch" or mode = "inter")
end-if
    pltgraf cube_perspective title="Test 19 - (Test 4) Perspective cube - AZ = 30, EL = 30, Plot=XY - TR"  +
          dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0  +
          dataform=xy direct=tr 'date plot=test19 plotfmt=eps
Beginning VICAR task pltgraf
PLTGRAF version 13-Jul-2013 (64-bit) - rjb
 Note: IEEE floating-point exception flags raised: 
    Inexact;  Division by Zero;  Invalid Operation; 
 See the Numerical Computation Guide, ieee_flags(3M) 
 ush gnuplot test19.eps.gpi
let $echo="no"
$ Return
$!#############################################################################
