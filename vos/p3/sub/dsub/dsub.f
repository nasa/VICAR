c************************************************************************
c
c        subroutine name: dsub
c
c        update history
c        --------------
c        revision a - initial release    1/1/84     bam
c        revision b - 3/19/84   bam
c                     1. added entry point autot for autotracking
c                     2. modified rcurse for autotracking on
c
c        revision c - 5/4/84    bam
c                     1. chnged entry point symbol name to dsymbol
c                     2. added a rounding factor to ddraw and dmove
c
c        revision d - 2/85      ram
c                     1. added ability to choose different cursors
c                     2. added ability to selectively erase graphics
c                        colors
c
c        revision e - 4/85      bam/ram
c                     1. removed all calls to error conditions
c                     2. inserted calls to error subroutine
c                     3. made code flexible for all display devices
c                        with differing types of hardware
c
c        revision f - 9/86      bam
c                     1. added 1024 graphics capability
c                     2. added pan capability
c                     3. added zoom capability
c
c        purpose:
c        this is the interface subroutine between the
c        display device interface routines and the
c        ramtek 'D' routines
c
c
         subroutine dsub

         logical xddactivate, xddconfigure, xddinfo, xddopen
         logical xdlconnect, xdlramp, xdlwrite, xdipolyline
         logical xdidwset, xdifill, xdilineread, xdilinewrite,  xdizoom
         logical xdcautotrack, xdclocation, xdcoff, xdcon, xdcset
         logical xdglwrite, xdtcolor, xdtfont,  xdtrotate, xdgconnect
         logical xdtsize, xdttext, xdx2d, xdimfill, xdgon, xdgoff
c
         real*4 cx, cy
	 integer  ipen, iprox, lines, samples, outmodes
         integer  u, i, p, ilad, isad, nsamps
         integer  h, ilds, isds, no, ivd, gmask, ib
         integer  th, itemp, itemp1
         integer  c, iform, iconoff, iblink, iw
         integer  section, loc, icount, ifont
         integer  ix1, iy1, gplane
         integer  csetup(4), asetup(4), larray(256)
         integer  xarray(2), yarray(2)
         integer  red(256), green(256), blue(256) 
         integer  kr,currd3
         integer  ihzoom, irb
c
         integer n, autoflag, prev_auto, ip
         integer dlds, dsds, dzoom
c
         logical flag, cauto, on
         byte buf(*), a(*), v, w
         dimension trans1(9), trans2(9), trans3(9), trans4(9)
c
	 character*80 msg
         character*30 ctbl0(3)/ ' Video Output = 512x512       ',
     -                          ' Video Output = 1024x1024     ',
     -                          ' Video Output = 640x480       '/
         character*30 ctbl7A/' GRAPHICS PLANE AVAILABLE     '/
         character*30 ctbl7B/' GRAPHICS PLANE NOT AVAILABLE '/
         character*30 ctbl7C/' GRAPHICS WILL BE PLACED ON   '/
         character*30 ctbl7D/'       IMAGE PLANE 1          '/
c
         character check(4), gdisid, vdisid
         equivalence ( jdisid, check )
	 byte bcheck(4)
	 equivalence (check, bcheck)
c
c        save all pertinant variables for later entries
c
         integer lmax, smax
         save u, i, c, iform, iblink, iconoff, h, s, v, w,
     -        section, loc, x1, y1, x1c, y1c, no, p, gplane,
     -        red, green, blue, autoflag, lmax, smax

         common/xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     -                 ncurs, nintio
c
c        here are some of the more reasonable dset possibilities
c
         character*2 jkey(7) / 'DW','DH','NW','NH','PO','CO','VL'/
	 character*2 itest
c
c        set up the cursor array for cset here
c
	 character*4 key
         character*4 ikey(7) / 'VISI', 'INVI', 'BLIN', 'NBLI',
     &                     'C1  ', 'C2  ', 'C3  ' /
c
c        initially, set up dapply transformation function to a
c        unit matrix. transformation function 3 will be required
c        to transform ramtek x, y cartesian coordinates to
c        picture coordinates in line/sample mode.
c
         data trans1/1.,0.,0.,0.,1.,0.,0.,0.,1./
         data trans3/0.,511.,0.,-511.,0.,0.,512.,1.,0./
         data trans4/0.,1023.,0.,-1023.,0.,0.,1023.,1.,0./
c
c        turn autotracking flag off
c
         data autoflag / 0 /
c
c        set v to 0 for erase call
c
         data v / 0 /
c
c        set w to white for drawing lines
c
         data w / 1 / 
         data red   / 0,255*255 /
         data green / 0,255*255 /
         data blue  / 0,255*255 /
c
c        set the no. of elements in the xarray and yarrays
c        to 2 for the xdipolyline routine
c
         data no / 2 /
c
c        use defualt cursor and default unit
c
         data u / 1 /, c / 1 / 
c
c        set up arrays for configuration and allocation
c
         data csetup / 0, 0, 0, 0 /
         data asetup / 1, 1, 0, 0 /
c
c        set location value ( loc ) for dtext
c
         data loc / 1 /
c
c        set default section of lut for xdlwrite 
c
         data section / 1 /
c
c        set the default cursor form to 0
c        and the blink rate off
c
         data iform / 0 /, cauto / .true. / 
         data iblink / 0 /, iconoff / 0 / 
c
c        default pan and zoon factors
c
         data dlds/0/, dsds/0/, dzoom/1/
c
c        set initial xdtsize values
c
         data h / 7 /, s / 1.0 /
c
c        set xdtcolor percision to 0
c
         data p / 0 /
c
c        initialize the graphics origin to the
c        lower l hand corner
c
         data x1c / 0.0 /, y1c / 0.0 /, x1 / 1.0 /, y1 / 512.0 / 
c
c        set up data information to check for graphics
c        or video erases
c
         data gdisid / 'G' /, vdisid / 'V' /
c
c---------------------------------------------------------
c
c
c
c        devopen
c         the devopen call is the first call that must be
c         made to the d routines. it will open the required
c         unit, allocate devices, configure them as required,
c         and activate them so that they can be read to and
c         from. the graphics plane is set to 4 and turned on.
c         a font is read in; character height is specified;
c         and text angle rotation is set to 0.
c
c         calling sequence ( iunit )
c         where :
c                iunit - device logical unit no.
c
          entry devopen ( iunit )
          u = iunit
c
c         allocate display unit
c
          ierr = xddallocate ( u, asetup )       
          call errors ( ' XDDALLOCATE', ierr )
          if ( ierr .eq. 0 ) then
           call xvmessage(' Display not active or allocated',' ')
           call abend
          end if
c 
c         normal return - we were able to allocate the device required
c
c         open unit u
c
          ierr = xddopen ( u )
          call errors ( ' XDDOPEN', ierr )
          if (ierr .ne. 1 ) then
            call qprint( ' Device open failed aborting.....' )
            call abend
          end if
c
c         activate the display unit so that we can write on it
c
          flag = .true.
          ierr = xddactivate ( u, flag )
          call errors ( ' XDDACTIVATE', ierr )
c
c         now configure the display (csetup is all 0's - default)
c
          ierr = xddconfigure ( u, csetup )
          call errors ( ' XDDCONFIGURE', ierr )
c
c         find out what type of device we have
c
          ierr = xddinfo ( u, 3, 1,  nluts )
          ierr = xddinfo ( u, 4, 1,  nimps )
          ierr = xddinfo ( u, 5, 1,  maxlines )
          ierr = xddinfo ( u, 6, 1,  maxsamps )
          ierr = xddinfo ( u, 30, 1, igraph )
          ierr = xddinfo ( u, 34, 1, gplane )
          ierr = xddinfo ( u, 48, 1, ncurs )
          ierr = xddinfo ( u, 60, 1, nintio )
          lmax = maxlines
          smax = maxsamps
c
c         read in a font file
c
          ifont = 1
          ierr  = xdtfont ( ifont )
          call errors ( ' XDTFONT', ierr )
c
c         set the initial size
c
          if ( lmax .le. 512 ) then
            h = 7
          else
            h = 14
          end if
 
          ierr = xdtsize ( h, s )                      
          call errors ( ' XDTSIZE', ierr )
c
c         rotate at 0 degrees 
c
          angle = 0.0
          ierr  = xdtrotate ( angle )
          call errors ( ' XDTROTATE', ierr )
c
c         turn on the cursor
c
          if ( ncurs .gt. 0 ) then
           iconoff = 1
           ierr = xdcon ( u, c, iform, iblink )
           call errors ( ' XDCON', ierr )
          end if
c
c         and the ramps
c
          do nl = 1, nluts
           ierr = xdlramp ( u, nl, section )
           call errors ( ' XDLRAMP', ierr )
           ierr = xdlconnect ( u, 1, nl, section, .FALSE. )
           call errors ( ' XDLCONNECT', ierr )
          end do
c
c         connect the graphics plane to image plane 
c
          if ( igraph .gt. 0 ) then
           ierr   = xdgconnect ( u, gplane, section, .false. )
           call errors ( ' XDGCONNECT', ierr )
c
c          turn on the graphics overlay plane
c
           ierr = xdgon ( u )
           call errors ( ' XDGON', ierr )
c
c          initialize graphics lut
c
           red(2)   = 255	! red
           green(2) = 0
           blue(2)  = 0
           red(3)   = 0		! green
           green(3) = 255
           blue(3)  = 0
           red(5)   = 0		! blue
           green(5) = 0
           blue(5)  = 255
           red(9)   = 255	! white
           green(9) = 255
           blue(9)  = 255
           red(17)   = 0	! black
           green(17) = 0
           blue(17)  = 0
           red(33)   = 255      ! yellow
           green(33) = 255
           blue(33)  = 0
           red(65)   = 0	! cyan
           green(65) = 255
           blue(65)  = 255
           red(129)   = 255	! magenta
           green(129) = 0
           blue(129)  = 255
           ierr = xdglwrite( u, section, red, green, blue )
           call errors ( ' XDGLWRITE', ierr )
          end if
c
          return

c--------------------------------------------------------------------------
c
c	devconf
c		configure the device given a number of 
c	lines and samples.  If the lines and samples are 0,
c	the the default configuration for the device is used
c	and the number of lines and samples are set to the
c	appropriate values.  Otherwise the device will be
c	configured to one of the following if it is available:
c		512x512 when lines=512 and samples=512
c		640x480 when lines=480 and samples=640
c		1024x1024 when lines=1024 and samples=1024
c	If the given lines and samples do not match any of
c	the above, the device's default is used but the values
c	of lines and samples are not changed.
c
	entry devconf( lines, samples )
c
      ierr = xddinfo( u, 7, 1, outmodes )
      if ( (lines .eq. 1024) .and. (samples .eq. 1024) ) then
       if ( btest(outmodes,9) ) then
         csetup(2) = 2
         csetup(3) = 2
       else
         call xvmessage(' 1024x1024 Output Not Available ',' ')
       end if

      else if ( (lines .eq. 480) .and. (samples .eq. 640) ) then
       if ( btest(outmodes,10) ) then
         csetup(2) = 2
         csetup(3) = 3
       else
         call xvmessage(' 640x480 Output Mode Not Available ',' ')
       end if

      else if ( (lines .eq. 512) .and. (samples .eq. 512) ) then
       if ( btest(outmodes,8) ) then
         csetup(2) = 1
         csetup(3) = 1
       else
         call xvmessage( ' 512x512 Output Mode Not Available ',' ')
       end if

      else
       csetup(1) = 0
       csetup(2) = 0
       csetup(3) = 0
       csetup(4) = 0
       if ( (lines .ne. 0) .and. (samples.ne.0) ) call xvmessage(
     +     ' Unrecognized Output Size, Display Default Used ',' ')
      end if
c
c         now configure the display
c
          ierr = xddconfigure ( u, csetup )
	  call errors(' XDDCONFIGURE', ierr)
c
c         find out what type of device we have
c
          ierr = xddinfo ( u, 3, 1,  nluts )
          ierr = xddinfo ( u, 4, 1,  nimps )
          ierr = xddinfo ( u, 5, 1,  maxlines )
          ierr = xddinfo ( u, 6, 1,  maxsamps )
          ierr = xddinfo ( u, 12, 1, currd3 )
          ierr = xddinfo ( u, 30, 1, igraph )
          ierr = xddinfo ( u, 34, 1, gplane )
          ierr = xddinfo ( u, 48, 1, ncurs )
          ierr = xddinfo ( u, 60, 1, nintio )
c
c         print out what we have
c
	  call xvmessage(' ',' ')
          call xvmessage(' Display Device Characteristics',' ')
	  call xvmessage(' ',' ')
	  call xvmessage(ctbl0(currd3),' ')
	  write (msg,110) nluts
  110	  format(' No. of LUTs =',i6)
	  call xvmessage(msg,' ')
	  write (msg,120) nimps
  120	  format(' No. of IMPs =',i6)
	  call xvmessage(msg,' ')
	  write (msg,130) maxlines
  130	  format(' No. of LINES =',i5)
	  call xvmessage(msg,' ')
	  write (msg,140) maxsamps
  140	  format(' No. of SAMPS =',i5)
	  call xvmessage(msg,' ')
	  write (msg,150) ncurs
  150	  format(' No. of CURSORS =',i3)
	  call xvmessage(msg,' ')
	  write (msg,160) nintio
  160	  format(' No. of IO DEVS =',i3)
	  call xvmessage(msg,' ')

          if ( igraph .eq. 1 ) then
           call xvmessage(ctbl7a,' ')
          else
           call xvmessage(ctbl7b,' ')
           call xvmessage(' ',' ')
           call xvmessage(ctbl7c,' ')
           call xvmessage(ctbl7d,' ')
           gplane = 1
          end if
          call xvmessage(' ',' ')

	if ( (lines.eq.0).and. (samples.eq.0) ) then
	 lines = maxlines
	 samples = maxsamps
	end if
c
c         connect the graphics plane to image plane 
c
          if ( igraph .gt. 0 ) then
           ierr   = xdgconnect ( u, gplane, section, .false. )
	   call errors(' xdgconnect', ierr)
c
c          initialize graphics lut
c
           ierr = xdglwrite( u, section, red, green, blue )
	   call errors(' xdglwrite', ierr)
          end if

        lmax = lines
        smax = samples

        if ( lmax .le. 512 ) then
           h = 7
        else
           h = 14
        end if
 
        ierr = xdtsize ( h, s )                      
	call errors(' xdtsize',ierr)

	return
c
c-----------------------------------------------------------------
c
c
c        devclose
c         to deactivate the ability to modify the display unit u
c         and to deallocate it for the next user
c
c
         entry devclose
c
c        deactivate the device 
c
         flag = .false.    
         ierr = xddactivate ( u, flag )           
	 call errors(' XDDACTIVATE', ierr)
c
c        now close unit
c
         ierr = xddfree ( u )
	 call errors(' XDDFREE', ierr)
c
         return
c
c
c----------------------------------------------------------------
c
c        dcolor
c         this is routine that connects the image planes
c         to the color luts. for full color, image planes
c         1,2, and 3 are connected to the associasted luts
c         and the graphics plane is connected to image
c         plane 4.
c
c
         entry dcolor(ind)
c
          if ( (nimps .ge. 3) .and. (nluts .eq. 3) ) then
           icolor = 0
           ierr  = xdlconnect ( u, 1, 1, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ierr  = xdlconnect ( u, 2, 2, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ierr  = xdlconnect ( u, 3, 3, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ind = 1
          else
           call xvmessage(' COLOR mode not available for this device',
     &			  ' ')
           ind = 0
          end if   
         return
c
c----------------------------------------------------------------
c
c        dpscolor
c         this is routine that connects the image planes
c         to the color luts. for pseudo color, image planes
c         1,2, and 3 are connected to lut 1.
c
c
c
         entry dpscolor(ind)
c
          if ( nluts .eq. 3 ) then
           icolor = 1
           ierr = xdlconnect ( u, 1, 1, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ierr = xdlconnect ( u, 1, 2, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ierr = xdlconnect ( u, 1, 3, 1, .false. )
	   call errors(' xdlconnect', ierr)
           ind = 1
          else
	   call xvmessage(' Pseudocolor not available for this device',
     +			  ' ')
           ind = 0
          end if   
         return
c
c----------------------------------------------------------------
c
c
c        bw
c         this is the routine that connects image 1 to lut 1
c         2, and 3 and turns on the linear ramps.
c
          entry bw
c
          do nl = 1, nluts
           icolor = 0
           ierr = xdlconnect ( u, 1, nl, 1, .false. )
	   call errors(' xdlconnect', ierr)
c
c          and the ramp
c
           ierr = xdlramp ( u, nl, section )
	   call errors(' xdlramp', ierr)
          end do
         return
c----------------------------------------------------------------
c
c
c        autot
c         this is the routine that turns autotracking on
c
          entry autot( on )
c
          if ( nintio .gt. 0 ) then
           if ( on ) then
            autoflag = 1
           else
            autoflag = 0
           end if
           ierr = xdcautotrack ( u, c, 0, autoflag )
	   call errors(' xdcautotrack', ierr)
          end if
          return
c
c----------------------------------------------------------------
c
c        gon
c         this is a routine to turn on the graphics overlay plane
c
c
          entry gon
c
c         turn on the graphics overlay plane
c
          if ( igraph .gt. 0 ) then
           ierr = xdgon ( u )
	   call errors(' xdgon', ierr)
          else
           call xvmessage(ctbl7b,' ')
          end if
          return
c
c----------------------------------------------------------------
c
c        goff
c         this is a routine to turn off the graphics overlay plane
c
c
          entry goff
c
c         turn off the graphics overlay plane
c
          if ( igraph .gt. 0 ) then
           ierr = xdgoff ( u )
	   call errors(' xdgoff', ierr)
          else
           call xvmessage(ctbl7b,' ')
          end if
          return
c
c----------------------------------------------------------------
c
c        dopen
c         this is basically a dummy routine now
c
c         calling sequence ( ind, idisid )
c         where :
c                ind    - is the returned error code
c                idisid - device logical unit no.
c
          entry dopen ( ind, idisid )
c
c         successful open
c
          ind = 0
          return
c
c-----------------------------------------------------------------
c
c
c        dclose
c         also a dummy routine
c
c
          entry dclose ( ind, idisid )
c
c         successful close
c
          ind = 0
          return
c
c----------------------------------------------------------------
c
c
c        dcheck
c         this is a dummy routine where the return code is
c         set to 0
c
c
         entry dcheck ( ind, idisid )
c
         ind = 0
         return
c
c-----------------------------------------------------------------
c
c
c       dini
c       the dini call will be used to get the logical
c       device name for the variable i which is 
c       required for the interface graphics routines
c
        entry dini ( idisid )
c
        jdisid = idisid
        i = gplane
c
c       set up w as required
c
        w = iv ( bcheck(2) ) - 48 
c
        return
c
c-----------------------------------------------------------------
c
c
c       dapply
c        the original purpose of this routine was to
c        transform picture coordinates to graphics plane
c        coordinates in the range of 0 to 1 
c
c        calling sequence: ( matrix )
c        where: matrix - the required transformation matrix
c                        of length 9
c
c
        entry dapply ( trans2 )
         call mve (7,9,trans2,trans1,1,1)
        return
c
c------------------------------------------------------------------
c
c
c       dmove
c        dmove will move the current operating point ( cop )
c        to a new location without drawing a line
c
c        calling sequence: ( x, y )
c         where  x - the x coordinate of the new cop
c                y - the y coordinate of the new cop
c
c
         entry dmove ( x, y )
c
         x1c = x * trans1(1) + y * trans1(4) + trans1(7)
         y1c = x * trans1(2) + y * trans1(5) + trans1(8)
         go to 10
c
c------------------------------------------------------------------
c
c
c       drmove
c        drmove will move the current operating point ( cop )
c        to a new location relative to where it is currently
c        positioned without drawing a line.
c
c        calling sequence: ( dx, dy )
c         where  dx - the x coordinate to be added to the
c                     current location in order to update 
c                     the cop
c                dy - the y coordinate to be added to the 
c                     current location in order to update
c                     the cop
c
c
         entry drmove ( dx, dy )
c
         x1c = x1c + dx * trans1(1) + dy * trans1(4)
         y1c = y1c + dx * trans1(2) + dy * trans1(5)
c
c        now to sample (x) and line (y) coordinates
c
 10      continue

         if ( lmax .le. 512 ) then
             x1 = x1c * trans3(2) + trans3(8) + 0.5
             y1 = y1c * trans3(4) + trans3(7) + 0.5
         else
             x1 = x1c * trans4(2) + trans4(8) + 0.5
             y1 = y1c * trans4(4) + trans4(7) + 0.5
         end if
         return
c
c------------------------------------------------------------------
c
c
c       ddraw
c        ddraw will draw a line from the cop to a new point ( np ).
c        after the line is drawn, the cop will be updated to the
c        np.        
c
c        calling sequence: ( x, y )
c         where  x - the x coordinate of the terminus of the line
c                y - the y coordinate of the terminus of the line
c
c
         entry ddraw ( x, y )
c
         x2c = x * trans1(1) + y * trans1(4) + trans1(7)
         y2c = x * trans1(2) + y * trans1(5) + trans1(8)
         go to 20
c
c------------------------------------------------------------------
c
c
c
c       drdraw
c        drdraw will draw a vector to a new point ( np ) 
c        whose terminus is computed by x2 = x1 + dx
c        and y2 = y1 + dy. after the draw is completed,
c        the cop will be updated to ( x2, y2 ).
c
c        calling sequence: ( dx, dy )
c         where  dx - the x coordinate to be added to the
c                     current location 
c                dy - the y coordinate to be added to the 
c                     current location 
c
c
         entry drdraw ( dx, dy )
c
         x2c = x1c + dx * trans1(1) + dy * trans1(4)
         y2c = y1c + dx * trans1(2) + dy * trans1(5)
c
c        now to sample (x) and line (y) coordinates
c
 20      continue
         if ( lmax .le. 512 ) then
             x2 = x2c * trans3(2) + trans3(8) + 0.5
             y2 = y2c * trans3(4) + trans3(7) + 0.5
         else
             x2 = x2c * trans4(2) + trans4(8) + 0.5
             y2 = y2c * trans4(4) + trans4(7) + 0.5
         end if
c
         xarray(1) = x1
         xarray(2) = x2
         yarray(1) = y1
         yarray(2) = y2
c
         if ( w .eq. 0 ) then
          gmask = 0
         else
          gmask = w
          gmask = 2**(gmask-1)
         end if
         if ( igraph .eq. 0 ) gmask = 255
         ierr = xdipolyline ( u, i, gmask, no, xarray, yarray )
	 call errors(' xdipolyline', ierr)
c
c        update the cop
c
         x1  = x2
         y1  = y2
         x1c = x2c
         y1c = y2c
         return
c
c------------------------------------------------------------------
c
c
c       dtext
c        dtext will write out a line of text to an image
c        memory plane
c
c        calling sequence: ( a, jcount )
c         where: a      - byte array containing text
c                jcount - no. of characters in array a
c
c
        entry dtext ( a, jcount )
c
        icount = jcount
c
c       set the 'complimentary or positive' mode on
c
        if ( w .eq. 0 ) then
         iw = 0
        else
         iw = w
         iw = 2**(iw-1)
        end if
        if ( igraph .eq. 0 ) iw = 255
        ierr = xdtcolor ( iw, p )        
	call errors(' xdtcolor',ierr)
c
c       now write the text string
c
        ix1    = x1
        iy1    = y1
        ierr   = xdttext ( u, i, ix1, iy1, loc, icount, a )       
	call errors(' xdttext', ierr)
c
        return
c
c------------------------------------------------------------------
c
c
c        dsymbol 
c         dsymbol will write out a line of text to the graphics plane 
c         at a user speciied height and angle
c
c        calling sequence ( tx, ty, tsize, a, tangle, nchar )
c        where:
c              tx     - x position of the text string
c              ty     - y position of the text string
c              tsize  - user specified height of the text
c              texbuf - buffer containing the text
c              tangle - angle to print the text
c              nchar  - no. of characters to print
c
         entry dsymbol ( tx, ty, tsize, a, tangle, nchar )
c
c
c         find where to put the text
c
          xtc = tx * trans1(1) + ty * trans1(4) + trans1(7)
          ytc = tx * trans1(2) + ty * trans1(5) + trans1(8)
          if ( lmax .le. 512 ) then
              xt  = xtc * trans3(2) + trans3(8)
              yt  = ytc * trans3(4) + trans3(7)
          else
              xt  = xtc * trans4(2) + trans4(8)
              yt  = ytc * trans4(4) + trans4(7)
          end if
          ts    = 1.0
c
          if ( tsize .lt. 1.0 ) then
           tsize = tsize * 511.0
          end if
c
          th = tsize  
c
          ierr  = xdtsize ( th, ts )
	  call errors(' xdtsize', ierr)
c
c         rotate to user specified angle
c
          if ( tangle .gt. 180.0 ) then
           tangle = tangle - 360.0
          else if ( tangle .lt. -180.0 ) then
           tangle = tangle + 360.0
          end if
c
          ierr  = xdtrotate ( tangle )
	  call errors(' xdtrotate', ierr)
c
c       set the 'complimentary or positive' mode on
c
        icount = nchar
        if ( w .eq. 0 ) then
         iw = 0
        else
         iw = w
         iw = 2**(iw-1)
        end if
        if ( igraph .eq. 0 ) iw = 255
        ierr   = xdtcolor ( iw, p )        
	call errors(' xdtcolor', ierr)
c
c       now write the text string
c
        ix1    = xt
        iy1    = yt
        ierr   = xdttext ( u, i, ix1, iy1, loc, icount, a )       
	call errors(' xdttext', ierr)
c
c       rotate back to 0 degrees 
c
        angle = 0.0
        ierr  = xdtrotate ( angle )
	call errors(' xdtrotate', ierr)
c
c       and reset the character size
c
        ierr = xdtsize ( h, s )                      
	call errors(' xdtsize', ierr)
        return
c
c------------------------------------------------------------------
c
c
c        vline/rline
c         vline will write from 1 to n lines of information
c         to a video device while rline will read information
c         from the video device.
c
c        calling sequence ( idisid, buf, islds, issds, nlds, nsds )
c        where:
c              idisid - video device logical unit no.
c              buf    - buffer for transfer of information
c              islds  - starting line coordinate
c              issds  - starting sample coordinate
c              nlds   - no. of lines to be written
c              nsds   - no. of samples per line
c
         entry vline ( idisid, buf, islds, issds, nlds, nsds )
c
c        set up start line and sample addresses
c
         isad = issds
         ilad = islds 
c
c        nsamps is the no. of samples to display / line
c        ensure nsamps is even so that xdilinewrite will work
c        correctly.
c
         if ( (isad-1) + nsds .gt. maxsamps ) then
          nsamps = maxsamps - isad - 1
         else
          nsamps = nsds
         end if
         nsamps = (( nsamps + 1 ) / 2 ) * 2
c
c        extract the display device no.
c
         jdisid = idisid
         ivd    = iv ( bcheck(2) ) - 48
c
c        write pixels 
c
         n    = nlds
         do j = 1, n  
          ierr  = xdilinewrite ( u, ivd, isad, ilad, nsamps, buf )
	  call errors(' xdilinewrite', ierr)
          ilad = ilad + 1
         end do
         return
c
c
c
         entry rline ( idisid, buf, islds, issds, nlds, nsds )
c
c        set up start line and sample addresses
c
         isad = issds
         ilad = islds 
c
c        nsamps is the no. of samples to display / line
c        ensure nsamps is even so that xdilinewrite will work
c        correctly.
c
         if ( (isad-1) + nsds .gt. maxsamps ) then
          nsamps = maxsamps - isad - 1
         else
          nsamps = nsds
         end if
         nsamps = (( nsamps + 1 ) / 2 ) * 2
c
c        extract the display device no.
c
         jdisid = idisid
         if ( check(1) .eq. vdisid ) then
          ivd    = iv ( bcheck(2) ) - 48
c     
c        now the graphics screen
c
         else
          ivd    = gplane
         end if
c
c        read pixels 
c
         n    = nlds
         do j = 1, n  
          ierr  = xdilineread ( u, ivd, isad, ilad, nsamps, buf )
	  call errors(' xdilineread', ierr)
          ilad = ilad + 1
         end do
         return
c
c----------------------------------------------------------------------------
c
c
c        rcurse/wcurse
c         this is the cursor routine that will read where a cursor is located
c         or position the cursor where the user requests. rcurse is the read
c         routine and wcurse is the write routine.
c
c         where:
c               itid - cursor logical device no.
c               lds  - line coordinate
c               jsds - sample coordinate
c
         entry rcurse ( itid, lds, jsds )
c
         if ( ncurs .eq. 0 ) return
c
c        flip around via autotracking flag
c
         if ( autoflag .eq. 0 ) then
c
c        turn autotracking on
c
           ierr = xdcautotrack ( u, c, 0, .TRUE. )
	   call errors(' xdcautotrack', ierr)
c
c         while the pen is not down and not in proximity
c            read the tablet pen
c
       	  ipen = 0
	  iprox = 0
	  do while ( .not. ( ipen .eq. 1 .and. iprox .eq. 1 ) )
	    ierr = xdx2d( u, 1, cx, cy, iprox, ipen )
	    call errors(' xdx2d', ierr)
	  end do
c
c        find where the cursor is hiding
c
          ierr = xdclocation ( u, c, isds, ilds )
	  call errors(' xdclocation', ierr)
c
c        turn autotracking off
c
           ierr = xdcautotrack ( u, c, 0, .FALSE. )
	   call errors(' xdcautotrack', ierr)
c
c         autotracking on
c
          else
c
c        find where the cursor is hiding
c
           ierr = xdclocation ( u, c, isds, ilds )
	   call errors(' xdclocation', ierr)
          end if
c
c         normal return - 
c 
           lds  = ilds / dzoom + dlds            ! compensate for panning
           jsds = isds / dzoom + dsds
           return
c
c
c        wcurse 
c
c
         entry wcurse ( itid, lds, jsds )
c
         if ( ncurs .eq. 0 ) return
c
         ilds = ( lds - dlds ) * dzoom
         isds = ( jsds- dsds ) * dzoom
         ierr = xdcset (u, c, isds, ilds )
	 call errors(' xdcset', ierr)
c
         return
c
c---------------------------------------------------------------
c
c        pdown
c         this will return a flag = 1 if the
c         pen is down and in proximity
c
         entry pdown ( ipflag )
c
         ipflag = 1
         if ( nintio .eq. 0 ) return
c
            ipen  = 0
	    iprox = 0
 	    ierr  = xdx2d( u, 1, cx, cy, iprox, ipen )
	    call errors(' xdx2d', ierr)
c
          if ( ipen .eq. 1. and .iprox .eq. 1 ) then
           ipflag = 1
          else
           ipflag = 0
          end if
c
          return
c---------------------------------------------------------------
c
c        hzoom
c         this will envoke the hardware zoom
c         option on the DeAnza
c
         entry hzoom ( ihzoom )
c
         do jnmc = 1, 4        ! for all 3 memory planes and the graphics
            irb = jnmc
            ierr = xdizoom ( u, irb, ihzoom )
	    call errors(' xdizoom', ierr)
         end do
c            
         dzoom = ihzoom
         return
c---------------------------------------------------------------
c
c        pan
c         this routine will provide pan capabilities for 2048
c         memory planes with a 1024 monitor
c
         entry pan 
c
         if ( nintio .eq. 0 ) return       ! if no button - quit here
c
c         if ( icolor .eq. 1 ) then         ! if pseudo color  - quit here
c            call xvmessage('Pan not available in pseudocolor mode',' ')
c            return       
c         end if
c
         ilds = 1                          ! set cursor to upper left hand 
         isds = 1                          ! corner
         ierr = xdcset (u, c, isds, ilds )
	 call errors(' xdcset', ierr)
c
         prev_auto = autoflag              ! save previous autoflag
         if ( autoflag .eq. 0 ) then       ! turn on autotracking
             autoflag  = 1
             ierr = xdcautotrack ( u, c, 0, autoflag )
	     call errors(' xdcautotrack', ierr)
         end if
c
c        loop until ssw 1 is depressed
c
         ip = 0                            ! button down switch
         do while ( ip .eq. 0 )
             ipen  = 0
     	     iprox = 0
    	     ierr  = xdx2d( u, 1, cx, cy, iprox, ipen )
	     call errors(' xdx2d', ierr)
c
             if ( ipen .eq. 1. and .iprox .eq. 1 ) then
                ip = 1
             else                                        ! still moving
                ip = 0
                ierr = xdclocation ( u, c, isds, ilds )  !find the cursor 
		call errors(' xdclocation', ierr)
                do krb = 1, 4                            ! all 4 planes 
                    kr = krb
                    ierr = xdidwset ( u, kr, isds, ilds ) ! and pan
		    call errors(' xdidwset', ierr)
                end do
             end if
         end do
c
c        turn autotracking off - if off to start with
c
         if ( prev_auto .ne. autoflag ) then
            ierr = xdcautotrack ( u, c, 0, .FALSE. )
	    call errors(' xdcautotrack', ierr)
         end if
c
         dlds = ilds - 1
         dsds = isds - 1
         return
c
c---------------------------------------------------------------
c
c
c        cset
c         this is the cursor set routine and may be used to
c         control the visibility characteristics of the 
c         cursor. it has the following posibilities:
c         a) cursor off
c         b) cursor on
c         the cursor on/off flag , iconoff, has the following values:
c         0 = off
c         1 = on
c         initially, the cursor is set to off.
c
c         in both these cases, the cursor can be blinking
c         or not blinking. obviously, if the cursor is off,
c         we can't see if it is blinking. 
c
c         where : itid - logical cursor identifier
c                 key  - cursor visibility characteristics
c                        flag
c                        visi = visible
c                        invi = invisible
c                        blin = blinking
c                        nbli = not blinking
c			 c1   = form 1 (five dots)
c			 c2   = form 2 
c	                 c3   = form 3
c
c
         entry cset ( itid, key )

c
         if ( ncurs .eq. 0 ) return
c
c        see if the cursor is alive and visible
c
         if ( key .eq. ikey(1) ) then
          ierr = xdcon ( u, c, iform, iblink )
	  call errors(' xdcon', ierr)
c
c         save cursor visibility flag
c
          iconoff = 1
          return
         end if
c
c        now invisible
c
         if ( key .eq. ikey(2) ) then
          ierr = xdcoff ( u, c )
	  call errors(' xdcoff', ierr)
c
c         save cursor visibility flag
c
          iconoff = 0
          return
         end if
c
c        set the blink on here if the cursor is visible
c
         if ( key .eq. ikey(3) ) then
c
          if ( iconoff .eq. 1 ) then
           iblink = 3 
           ierr   = xdcon ( u, c, iform, iblink )
	   call errors(' xdcon', ierr)
           return
c
          else 
           iblink = 3
           return
          end if
         end if
c
c        set the blink off here if the cursor is visible
c
         if ( key .eq. ikey(4) ) then
c
          if ( iconoff .eq. 1 ) then
           iblink = 0 
           ierr   = xdcon ( u, c, iform, iblink )
	   call errors(' xdcon', ierr)
           return
c
          else 
           iblink = 0
           return
          end if
         end if
c
c        set the cursor form to 1 for 'C1'
c
         if ( key .eq. ikey(5) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 1
           ierr   = xdcon ( u, c, iform, iblink )
	   call errors(' xdcon', ierr)
           return
c
          else 
           iform = 1
           return
          end if
         end if
c
c        set the cursor form to 4 for 'C2'
c        set the blink off here if the cursor is visible
c
         if ( key .eq. ikey(6) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 4
           ierr   = xdcon ( u, c, iform, iblink )
	   call errors(' xdcon', ierr)
           return
c
          else 
           iform = 4
           return
          end if
         end if
c
c        set the cursor form to 5 for 'C3'
c
         if ( key .eq. ikey(7) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 5
           ierr   = xdcon ( u, c, iform, iblink )
	   call errors(' xdcon', ierr)
           return
c
          else 
           iform = 5
           return
          end if
         end if
         return
c
c--------------------------------------------------------------
c
c
c        erase
c         this is the routine that erases a screen
c         the screen may either be video or graphics
c
        entry erase(idisid)
c
c       call xdifill to clear the screen, and use
c       the value, v, of zero to blacken the screen.       
c
        jdisid = idisid
c
c       first the video screen
c        
        if ( check(1) .eq. vdisid ) then
         ivd    = iv ( bcheck(2) ) - 48
         ierr   = xdifill ( u, ivd, v ) 
	 call errors(' xdifill', ierr)
c     
c       now the graphics screen
c
        else
         if ( igraph .eq. 0 ) then
          call xvmessage(ctbl7b,' ')
         else
          ib   = iv ( bcheck(2) ) -48
          gmask  = 2**(ib-1)
          ivd    = gplane
          ierr   = xdimfill ( u, ivd, gmask, v ) 
	  call errors(' xdimfill', ierr)
         end if
        end if
c
        return
c
c
c----------------------------------------------------------
c
c
c        trkon
c         this is the enable for the track mode.
c         however, at this point this is only a
c         dummy.
c  
         entry trkon ( icid )
         call xvmessage(' TRKON - FUNCTION NOT IMPLEMENTED',' ')
         return
c
c        trkoff
c         this is the disable for the track mode.
c         however, at this point this is only a 
c         dummy.
c
         entry trkoff ( icid )
         call xvmessage(' TRKOFF - FUNCTION NOT IMPLEMENTED',' ')
         return
c
c        track
c         this is the routine which stores values
c         delimited by the trackball and returns
c         the no. of values stored. it, as the above
c         routines, is also a dummy at the present 
c         time.
c
         entry track ( icid, buf, nsamp )
         call xvmessage(' TRACK - FUNCTION NOT IMPLEMENTED',' ')
         return
c
c
c---------------------------------------------------------------------
c
c
c        dset
c         dset appears to be a catch all routine that
c         allows the user to modify the characteristics
c         and display mode of the text as well as 
c         update the look up tables. as an added attraction,
c         we also get to connect up lookup tables to image
c         memory planes. this final, more interesting mode
c         is triggered by the fact that itest is none of the
c         more reasonable options. yuck...
c
c         calling sequence ( idisid, itest, larray )
c         where: idisid - logical device
c                itest  - character variation flag
c                         dw = double width
c                         dh = double height
c                         nw = normal width
c                         nh = normal height
c                         po = positive
c                         co = complementary
c                larray - look up table buffer
c
         entry dset ( idisid, itest, larray )
c
         jdisid = idisid
         ivd    = iv ( bcheck(2) ) - 48
c
c        normal display mode.
c
         if ( itest .eq. jkey(5) ) then
           w = ivd
           return
c
c         complimentary display mode
c
         else if ( itest .eq. jkey(6) ) then
           w = 0
           return
c
c        modify the video lookup table
c
         else if ( itest .eq. jkey(7) ) then
           ierr = xdlwrite ( u, ivd, section, larray )
	   call errors(' xdlwrite', ierr)
           return
c
c         and now for something entirely different...
c         height and width
c
c         double the width
c
         else if ( itest .eq. jkey(1) ) then
           s    = 2.0
           ierr = xdtsize ( h, s )                      
	   call errors(' xdtsize', ierr)
           return
c
c         double the height
c
         else if ( itest .eq. jkey(2) ) then
           if ( lmax .le. 512 ) then
               h = 14
           else
               h = 28
           end if
           ierr = xdtsize ( h, s )                      
	   call errors(' xdtsize', ierr)
           return
c
c         normal width
c
         else if ( itest .eq. jkey(3) ) then
           s    = 1.0
           ierr = xdtsize ( h, s )                      
	   call errors(' xdtsize', ierr)
           return
c
c         normal height
c
         else if ( itest .eq. jkey(4) ) then
           if ( lmax .le. 512 ) then
               h = 7
           else
               h = 14
           end if
           ierr = xdtsize ( h, s )                      
	   call errors(' xdtsize', ierr)
           return
c
c         now connect up lut's to image memory planes
c
         else
           itemp  = idisid
           itemp1 = ichar(itest(1:1))	! probably wrong... rgd 3/2010
c was:	   itemp1 = itest
           ierr   = xdlconnect ( u, itemp, itemp1, 1, .false. )
	   call errors(' xdlconnect', ierr)
           return
         end if
c
        return
        end
c-----------------------------------------------------------------------
c
         subroutine errors ( name, code )
c
         character *(*) name
         integer code
         character*80 msg
c
         if ( code .eq. 1 ) return
c
	 write(msg,100) name,code
  100	 format(a,' returned error =',i3)
	 call xvmessage(msg,' ')
         return
         end
