        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44
        IMPLICIT NONE
	integer*4 NCOL,maxpix,maxpts,maxground,maxborder
	integer*4  PASS, TRUE
	PARAMETER (NCOL = 6,maxpix=200,maxpts=1000)
	parameter (maxground=100,maxborder=25)
	parameter (TRUE = 1, PASS = 1)

	integer*4 left_image,right_image,pair,nground
	integer*4 npict,noverlap,ncol1,ncol3,ind
	integer*4 fds(maxpix),npoly(maxpix),overlap(maxpts,2)
	integer*4 unito,unit3,unit4,file,count,idground
	integer*4 idatal(40),idatar(40),idgeom,idpoints
	integer*4 status,nofile,col,i,j,jj,k,m,n,tiep,ns,nl,incr
	integer*4 line,samp,ibisbuf(maxpts,ncol),iground(maxground,5)
	integer*4 idata(40),idef,icam,iplot
	integer*4 inibiso, inibis3
	integer*4 outibis
	integer*4 nplotname,noutfile,ntbl,nplotgpi,nplotgpi2,nploteps

	real*8 data8l(20),data8r(20)

	REAL*4 rbisbuf(maxpts,NCOL),ground(maxground,5)
	real*4 realover(maxpts,2)
	real*4 x(maxborder+3),y(maxborder+3)
	real*4 corner(2,maxborder,maxpix),xy(2)
	real*4 radpol(maxpix),eqpol(maxpix),focal(maxpix)
	real*4 optaxl(maxpix),optaxs(maxpix),scale(maxpix)
	real*4 rsvec(maxpix,3),omangl(maxpix,3),conv(3600)
	real*4 datal(40),datar(40),xx(4),yy(4)
	real*4 latlon(2),data(40),area(4),save_lat,save_lon
	real*4 exag,rline,rsamp,lat,lon,bignum,cam_real(maxpix)
	real*4 xscale, yscale
        character*4  ibisformat(2)
	character*5  project
	character*8 pformat
	CHARACTER*63 plotname
	character*80 string
	character*80 plotout
	character*80 plotgpi,plotgpi2,ploteps,tbl
        character*100 cbuf,outfile
	character*132 msg
	character*132 msg1

	logical*4 new,plotit,xvptst,print,in,inside,stereographic
	logical*4 use_area,epsplot

        !! The equivalence statements below have been determined to be
        !! OK as is when porting to UNIX platforms.  The equivalence 
        !! statment as used, is a 'FORTRAN' way of building a C structure.
	equivalence (ibisbuf,rbisbuf),(data8l,datal,idatal)
	equivalence (data8r,datar,idatar),(ground,iground)
	equivalence (overlap,realover),(idata,data)
c
	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/
c
        !! Specification for the two columns of the 'overlap' output file
	ibisformat(1) = 'REAL'
	ibisformat(2) = 'REAL'
        do i = 1,3600
           conv(i) = 0.0
        end do
        do i = 1,40
           datal(i) = 0.0
        end do
        rline = 0.0
        rsamp = 0.0
        lat = 0.0
        lon = 0.0
        xscale = 1.0
        yscale = 1.0

	msg = ' '
	msg1 = ' '
        CALL IFMESSAGE('MOSPLOT version Aug 23 2013 (64-bit) - rjb')
        bignum=1.1e+30

C Get the parameters
        call xvp('EXAG',exag,count)
        new= xvptst('NEW')
        call xvparm('PROJECT',project,n,idef,5)

        call xvpcnt('OUT',count)
        if(count.eq.0) plotit=.true.
c
c  interactive plot
        call xvparm('OUT',cbuf,n,idef,1)
          outfile = cbuf
          noutfile=index(outfile,'   ') - 1
	if (plotit) then
               plotname = outfile
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               tbl = outfile(1:noutfile)//asc
               ntbl = index(tbl,'  ') - 1
	endif
        print=xvptst('PRINT')
        call xvp('INCR',incr,count)
        call xvp('NS',ns,count)
        call xvp('NL',nl,count)
        call xvparm('LATLON',latlon,count,idef,2)
        if(idef .eq. 1)then
          stereographic=.true.
        else
          stereographic=.false.
        endif
        use_area=.false.
        call xvp('AREA',area,count)
        if(count.eq.4) use_area=.true.

        if(new)then
            call xvmessage('*** Using updated SEDR, columns: 21-23',' ')
        else
            call xvmessage('*** Using original SEDR, columns: 8-10',' ')
        endif

c Obtain output PostScript filename, or default filename
C if this called then you want a postscript output
c  override names for out
c
	epsplot = .false.
        call xvparm('PLOTOUT',plotout,iplot,idef,80)
         IF (IPLOT .GT. 0) THEN
c            CALL PLOTFN(PLOT_DS)
               plotname = plotout
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotname(1:nplotname)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
               ploteps=plotname(1:nplotname)//eps
               nploteps=index(ploteps,'  ') - 1
               tbl = outfile(1:noutfile)//asc
               ntbl = index(tbl,'  ') - 1
c	       epsplot = .true.
         ENDIF

        call xvp ('PLOTFMT',pformat,count)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.
	

C       Open output PostScript file  
c        call plotfn (plotout)

c initialize XRT/graph
c        call xrtbegin (status)					!XRT
C        if (status .ne. 1) then
C           call mabend('??E - MOSPLOT failed to initialize XRT/graph')
C        endif 

c set word 39 of convev buffer
        if(xvptst('OBJECT'))then
           call xvmessage('*** Treating images as OBJECT space images',' ')
           idatal(39)=8
           idatar(39)=8
        else
           call xvmessage('*** Treating images as IMAGE space images',' ')
           idatar(39)=7
           idatal(39)=7
        endif         

c Set the DATA buffer for a stereographic projection for convev.
c we use this to convert from lat,lon to line,samp in a
c stereographic projection.
        if(stereographic)then
           data(1)=500.
           data(2)=500.
           data(3)=latlon(1)
           data(4)=latlon(1)
           data(5)=latlon(1)
           data(6)=latlon(2)
           data(7)=1.0
           data(8)=1.0
           if(latlon(1).lt.0.0) data(8)=-1.0
           data(9)=0.0
           data(25)=1000.
           data(26)=1000.
           idata(39)=4
        endif

c Read in the SEDR.INT file
	npict = 0
        call rdfil_readonly(unito,inibiso,1,npict,ncol1,nofile)
        call xvmessage('   ',' ')
        write (msg( 1:23),'(a)') '# images in SEDR file= '
        write (msg(24:26),'(i3)') npict
        call xvmessage (msg,' ')
        if (npict.gt.maxpix)then
           call xvmessage('??E - Too many SEDR entries',' ')
           write (msg( 1:14),'(a)') 'upper limit = '
           write (msg(15:17),'(i3)') maxpix
           call xvmessage (msg,' ')
           call abend
        endif
        if (ncol1.lt.23) then
           call xvmessage('??E - First input file not a SEDR.INT file',' ')
           write (msg( 1:19),'(a)') 'Number of columns= '
           write (msg(20:22),'(i3)') ncol1
           call xvmessage (msg,' ')
           call abend
        endif
        call ibis_column_read(inibiso,fds,1,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        if(ncol1.lt.31) then
           call xvmessage('??E - SEDR.INT file not created by VGRIBIS',' ')
           write (msg( 1:19),'(a)') 'Number of columns= '
           write (msg(20:22),'(i3)') ncol1
           call xvmessage (msg,' ')
           call abend
        endif
        call ibis_column_read(inibiso,cam_real, 4,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,1),5,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,2),6,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,3),7,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        if(new)then    ! new sedr updated by omcor.
          call ibis_column_read(inibiso,omangl(1,1),21,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
          call ibis_column_read(inibiso,omangl(1,2),22,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
          call ibis_column_read(inibiso,omangl(1,3),23,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        else           ! original sedr unmodified.
            call ibis_column_read(inibiso,omangl(1,1),8,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
            call ibis_column_read(inibiso,omangl(1,2),9,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
            call ibis_column_read(inibiso,omangl(1,3),10,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        endif
        call ibis_column_read(inibiso,radpol,26,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,eqpol,27,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,focal,28,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,optaxl,29,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,optaxs,30,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,scale,31,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_file_close (INIBISO, 'UDELETE',status)
        if (status.NE.PASS) call IBIS_SIGNAL(inibiso,status,true)

        if (print) then
          do i=1,npict
             msg = ' '
             write (msg( 1:26),'(a)') 'rsvector,omangle, point#= '
             write (msg(27:29),'(i3)') i
             call xvmessage (msg,' ')
             write (msg(1:),'(6e12.4)') rsvec(i,1),rsvec(i,2),
     +              rsvec(i,3), omangl(i,1),omangl(i,2),omangl(i,3)
             call xvmessage(msg,' ')
             msg = ' '
             call xvmessage('polr,eqr,foc,optal,optas,scal',' ')
             write (msg(1:),'(6f12.4)') radpol(i),eqpol(i),focal(i),
     +                   optaxl(i),optaxs(i),scale(i)
             call xvmessage(msg,' ')
          enddo 
        endif

c Determine the file IDs of files 2,3 and 4 are (if present).
c idgeom=INP# of geom file, 0 means not there.
c idpoints=INP# of tiepoints file, 0 means not there.
c idground=INP# of ground control points, 0 means not there.

      call getinput_id(idgeom,idpoints,idground)

c check consistency.
      if (project.ne.'GLL  ')then
        if((idatal(39).eq.7).and.(idgeom.eq.0))then
             call xvmessage('??E - IMAGE space requires GEOMA file',' ')
             call abend
        endif
      endif


c Read tiepoints file as input if present.
c ibisbuf(n,1)=left image ID #.
c ibisbuf(n,2)=right image ID #.
c rbisbuf(n,3)=line # of left image.
c rbisbuf(n,4)=sample # of left image.
c rbisbuf(n,5)=line # of right image.
c rbisbuf(n,6)=sample # of right image.
      if(idpoints.gt.0)then
           call rdfil_readonly(unit3,inibis3,idpoints,tiep,ncol3,nofile)
           call xvmessage('   ',' ')
           msg = ' '
           write (msg( 1:28),'(a)') 'points located in tiep file='
           write (msg(29:31),'(i3)') tiep
           call xvmessage (msg,' ')
           if(tiep.gt.maxpts)then
              call xvmessage('??E - Too many tiepoints',' ')
              write (msg( 1:13),'(a)') 'upper limit ='
              write (msg(14:17),'(i3)') maxpts
              call xvmessage (msg,' ')
              call abend
           endif
           if(ncol3.ne.12)then
              call xvmessage('??E - Incorrect # cols in tiepoints file',' ')
              call abend
           endif
           do col=1,ncol
              call ibis_column_read (inibis3,rbisbuf(1,col),
     +             col,1,tiep,status)
              if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           enddo
           call ibis_file_close (INIBIS3, 'UDELETE',status)
           if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           do i=1,tiep
              !! Convert columns 1 & 2 to integer.  The column 1 & 2 values
              !! were originally converted to real*4 before storing into the
              !! file.
              ibisbuf(i,1)=nint(rbisbuf(i,1))
              ibisbuf(i,2)=nint(rbisbuf(i,2))
           enddo
           if(print)then
              msg1 = ' '
              write (msg1( 1:22),'(a)') 'left   right      line'
              write (msg1(23:40),'(a)') '         samp     '
              write (msg1(41:62),'(a)') '   line          samp'
              call xvmessage(msg1, ' ')
              do i=1,tiep
                 write(string,4)ibisbuf(i,1),ibisbuf(i,2),
     +                  rbisbuf(i,3),rbisbuf(i,4),
     +                  rbisbuf(i,5),rbisbuf(i,6)
                 call xvmessage(string,' ')
              enddo
           endif
4          format(1x,2i5,4f12.3)
      endif

c Read ground control file as input if present.
c iground(n,1)=image ID #.
c ground(n,2)=line.
c ground(n,3)=sample.
c ground(n,4)=latitude.
c ground(n,5)=longitude.
      if(idground.gt.0)then
           call rdfil_readonly(unit3,inibis3,idground,
     +                         nground,ncol3,nofile)
           call xvmessage('   ',' ')
           msg = ' '
           write (msg(1:31),'(a)') 'ground control points located= '
           write (msg(32:34),'(i3)') nground
           call xvmessage (msg,' ')
           if(nground.gt.maxground)then
              call xvmessage('??E - Too many ground points',' ')
              write (msg( 1:14),'(a)') 'upper limit = '
              write (msg(15:17),'(i3)') maxground
              call xvmessage (msg,' ')
              call abend
           endif
           if(ncol3.ne.5)then
              call xvmessage('??E - Incorrect # cols in ground file',' ')
              call abend
           endif
           do i=1,5
              call ibis_column_read(inibis3,ground(1,i),
     +             i,1,nground,status)
              if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           enddo
           do i=1,nground    ! convert to integer
              iground(i,1)=nint(ground(i,1))
           enddo
           call ibis_file_close (INIBIS3, 'UDELETE', STATUS)
           if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           if(print)then
              msg1 = ' '
              write (msg1( 1:25),'(a)') 'ID#      line           '
              write (msg1(25:34),'(a)') 'sample   '
              write (msg1(35:59),'(a)') ' latitude      longitude'
              call xvmessage(msg1, ' ')
              do i=1,nground
                 write(string,5) iground(i,1),ground(i,2),
     +                  ground(i,3),ground(i,4),ground(i,5)
                 call xvmessage(string,' ')
              enddo
           endif
5          format(1x,i5,4f12.3)
      endif

c read in the geoma correction file ... an IBIS file.
      if(idgeom.gt.0)then
           call xvunit(unit4,'INP',idgeom,status,' ')
           call geoma2conv (unit4, conv)
           call xvmessage
     +          ('*** GEOMA image space correction file read ok',' ')
           if(xvptst('OBJECT'))then
              call xvmessage
     +             ('WARNING: geoma file will be ignored',' ')
           endif
      endif

c put 'GLL' into CONV if project is galileo.
      if(project.eq.'GLL  ')then
        call mvcl('GLL  ',conv(1),5)
        DO I=2,npict
          IF (cam_real(I).NE.cam_real(1)) THEN
            call mabend('??E - Images are not taken with the same SSI mode')
          END IF  
        END DO
        icam=cam_real(1)
        IF (icam.EQ.2) THEN !Summation Mode Images, change default NS, NL, INCR
          ! check to see that all NS, NL and INCR are not changed by the user
          call xvp('INCR',incr,count)
          IF (count.EQ.0) THEN
            call xvp('NS', ns, count)
            IF (count.EQ.0) THEN
              call xvp('NL', nl, count)
              IF (count.EQ.0) THEN
                call xvmessage ('Summation Mode images detected', ' ')
                call xvmessage ('  changing parameter NS=>400', ' ')
                call xvmessage ('                     NL=>400', ' ')
                call xvmessage ('                     INCR=>66',' ')
                NS = 400
                NL = 400
                INCR = 66
              END IF
            END IF
          END IF
        END IF
      CALL MVE(4,1,2,CONV(3),icam,1)

      else
         call xvmessage('*** Assuming project is not Galileo',' ')
      endif

c Create footprint plots ************************************
c Loop over images.
      do 30 file=1,npict

c setup SEDR for the latest image.
         call setnav1(file,maxpix,omangl,rsvec,
     +      radpol,eqpol,focal,optaxl,optaxs,scale,data8l,
     +      datal)

c Locate points every INCR around picture border.
         line=1
         samp=1
         k=0
31       if(line.eq.1.and.samp.lt.ns)then
            samp=samp+incr
            if(samp.gt.ns) samp=ns
         else if(samp.eq.ns.and.line.lt.nl)then
            line=line+incr
            if(line.gt.nl) line=nl
         else if(line.eq.nl.and.samp.gt.1)then
            samp=samp-incr
            if(samp.lt.1) samp=1
         else if(samp.eq.1.and.line.gt.1)then
            line=line-incr
            if(line.lt.1) line=1
         else
            call xvmessage('??W - Not supposed to be here',' ')
         endif

c convert from line,samp to lat,lon
         rline=line
         rsamp=samp
         call convev(ind,datal,datal,rline,rsamp,
     +               lat,lon,2,conv)
c        if(ind.ne.0) point is off planet

c convert from lat,lon to line,sample in stereographic proj.
         if((ind.eq.0).and.stereographic)then
            call convev(ind,data,data,rline,rsamp,lat,lon,
     +                  1,conv)
c           if(ind.ne.0)then      !point not visible.
         endif

c Save coordinates for all files.
         if(ind.eq.0)then
           if(stereographic)then
              k=k+1
              corner(1,k,file)=rsamp   ! horizontal
              corner(2,k,file)=rline   ! vertical
           else
              k=k+1
              corner(1,k,file)=lon
              corner(2,k,file)=lat
           endif
           if(k.gt.maxborder)then
              call xvmessage('??E - Too many vertices for routine INSIDE',' ')
              write (msg( 1:14),'(a)') 'upper limit = '
              write (msg(15:17),'(i3)') maxborder
              call xvmessage (msg,' ')
              call xvmessage('increase parameter INCR',' ')
              call abend
           endif
         endif

         if(.not.(line.eq.1.and.samp.eq.1)) goto 31

         npoly(file)=k       ! save # polygon vertices.

30    continue  !! End of DO 30 ... create footprint plots

c     compute the scale for the plots & draw axes.
c     xx(4) & yy(4) contain the limits and scales.

        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
        if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
        endif


      call plot_scale(corner,npoly,npict,maxborder
     +               ,maxpix,xx,yy,stereographic,
     +                use_area,area,xscale,yscale,
     +                tbl,ntbl,epsplot,ploteps,nploteps,
     +                plotgpi,nplotgpi,plotgpi2,nplotgpi2)

c     draw footprint plots only.
      call plot_foot(corner,x,y,npoly,npict,maxborder
     +              ,maxpix,xx,yy,xscale,yscale,epsplot,
     +      tbl,ntbl)

c compute the tiepoint residuals ********************************
      if (idpoints.gt.0) then
         left_image=0
         right_image=0
         if (print) then
            call xvmessage('   ',' ')
            call xvmessage('Tiepoint residuals',' ')
            call xvmessage
     +           ('point#  left   right    del_lat   del_lon',' ')
         endif
         do 50 pair=1,tiep

c setup sedr for the image pair.
            if (left_image.eq.ibisbuf(pair,1).and.
     +         right_image.eq.ibisbuf(pair,2)) then
            else
               left_image=ibisbuf(pair,1)
               right_image=ibisbuf(pair,2)
               call setnav (left_image,right_image,
     +         maxpix,omangl,rsvec,
     +         radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +         datal,datar)
            endif

c compute lat,lon for each image

c left image
            rline=rbisbuf(pair,3)
            rsamp=rbisbuf(pair,4)
            ind = 0
            call convev(ind,datal,datal,rline,rsamp,
     +               lat,lon,2,conv)
            if (ind.ne.0) then
               write (msg(1:),'(a)') 'Left pnt off planet, pt#='
               write (msg(26:28),'(i3)') pair
               call xvmessage (msg,' ')
               rbisbuf(pair,3)=bignum
               goto 50
            else
               rbisbuf(pair,3)=lat
               rbisbuf(pair,4)=lon
            endif
            save_lat=lat
            save_lon=lon

c convert to projection in line,sample
            if (stereographic) then
               call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
               if (ind.ne.0) then
                  write (msg(1:),'(a)') 'Left pnt not visbl, pt#='
                  write (msg(26:28),'(i3)') pair
                  call xvmessage (msg,' ')
                  rbisbuf(pair,3)=bignum
                  goto 50
               else
                  rbisbuf(pair,3)=rline
                  rbisbuf(pair,4)=rsamp
               endif
            endif

c right image
            rline=rbisbuf(pair,5)
            rsamp=rbisbuf(pair,6)
            call convev(ind,datar,datar,rline,rsamp,
     +               lat,lon,2,conv)
            if (ind.ne.0) then
               write (msg(1:),'(a)') 'Right pnt off planet, pt#='
               write (msg(28:30),'(i3)') pair
               call xvmessage (msg,' ')
               rbisbuf(pair,3)=bignum
               goto 50
            else
               rbisbuf(pair,5)=lat
               rbisbuf(pair,6)=lon
            endif

c convert to projection in line,sample
            if (stereographic) then
               call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
               if (ind.ne.0) then
                  write (msg(1:),'(a)') 'Right pnt not visbl, pt#='
                  write (msg(27:29),'(i3)') pair
                  call xvmessage (msg,' ')
                  rbisbuf(pair,3)=bignum
                  goto 50
               else
                  rbisbuf(pair,5)=rline
                  rbisbuf(pair,6)=rsamp
               endif
            endif

           if (print) then
                 write(string,7)pair,left_image,right_image,
     +                  save_lat - lat,
     +                  save_lon - lon
                 call xvmessage(string,' ')
           endif
7          format(1x,3i5,2f12.3)

50       continue

c Convert the ground control points from line,sample
c to lat,lon  to  compare with the given lat,lon.
         if (idground.gt.0) then
           file=0
           if (print) then
              call xvmessage('   ',' ')
              call xvmessage('Ground control residuals',' ')
              call xvmessage('file#   del_lat   del_lon',' ')
           endif
           do 70 i=1,nground

c setup sedr for frame numbers
              if (file.ne.iground(i,1)) then
                file=iground(i,1)
                call setnav1(file,maxpix,omangl,rsvec,
     +            radpol,eqpol,focal,optaxl,
     +            optaxs,scale,data8l,
     +            datal)
              endif

c convert from line,samp to lat,lon
              rline=ground(i,2)
              rsamp=ground(i,3)
              ind = 0
              call convev(ind,datal,datal,rline,rsamp,
     +                    lat,lon,2,conv)
              if (ind.ne.0) then
                  write (msg(1:),'(a)') 'Ground pt off planet, pt#='
                  write (msg(27:29),'(i3)') i
                  call xvmessage (msg,' ')
                 ground(i,2)=bignum
                 goto 70
              else
                 ground(i,2)=lat  ! replace line/samp by lat/lon
                 ground(i,3)=lon
              endif
              save_lat=lat
              save_lon=lon

c convert to projection in line,sample
              if (stereographic) then
                 call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
                 if (ind.ne.0) then
                    write (msg(1:),'(a)') 'Ground pt not visbl, pt#='
                    write (msg(27:29),'(i3)') i
                    call xvmessage (msg,' ')
                    ground(i,2)=bignum
                    goto 70
                 else
                    ground(i,2)=rline
                    ground(i,3)=rsamp
                 endif
              endif

c convert given lat,lon to projection in line,sample
              if (stereographic) then
                 lat=ground(i,4)
                 lon=ground(i,5)
                 call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
                 if (ind.ne.0) then
                    write (msg(1:),'(a)') 'Ground pt not visbl, pt#='
                    write (msg(27:29),'(i3)') i
                    call xvmessage (msg,' ')
                    ground(i,2)=bignum
                    goto 70
                 else
                    ground(i,4)=rline
                    ground(i,5)=rsamp
                 endif
              endif

              if (print) then
                 write(string,8)file,
     +                  save_lat - lat,
     +                  save_lon - lon
                 call xvmessage(string,' ')
              endif
8             format(1x,i5,2f12.3)

70          continue
         endif

         call plot_resid(rbisbuf,maxpts,ncol,tiep,xx,yy,
     +                   exag,idground,ground,maxground,
     +                   nground,xscale,yscale)

      endif              

c Compute overlap & write output file *****************************
      call xvpcnt('OUT',count)
      if(count.gt.0) then

c Create the overlap buffer
        m=0
        do 40 i=1,npict
           do 41 j=1,npict
              if(i.eq.j)goto 41
              do 42 k=1,npoly(j)
                 xy(1)=corner(1,k,j)
                 xy(2)=corner(2,k,j)
                 in=inside(xy,corner(1,1,i),npoly(i))
                 if(in)then
                    do 43 n=1,m  ! reject redundancies
                       if((overlap(n,1).eq.j).and.(overlap(n,2)
     +                       .eq.i)) goto 41
43                  continue
                    if(m.eq.maxpts)then
                       call xvmessage('??E - Overlap buffer exceeded',' ')
                       call abend
                    endif
                    m=m+1
                    overlap(m,1)=i
                    overlap(m,2)=j
                    goto 41
                 endif
42            continue
41         continue
40      continue
        noverlap=m
        if(print)then
           call xvmessage('   ',' ')
           call xvmessage('Overlap file contents:',' ')
           call xvmessage('Left-file#   Right-file#',' ')
           do i=1,noverlap
              write(string,6) overlap(i,1),overlap(i,2)
              call xvmessage(string,' ')
           enddo
        endif
6       format(1x,2i10)

c  WRITE OUT OVERLAP FILE.
        CALL XVUNIT(UNITO,'OUT',1,STATUS,' ')
        IF (STATUS.NE.1) THEN
           msg1 = ' '
           write (msg1( 1:31),'(a)') 'OUTPUT INITIALIZATION ERROR - '
           write (msg1(32:62),'(a)') '            PROGRAM TERMINATED'
           call xvmessage(msg1, ' ')
           CALL ABEND
        END IF
        CALL IBIS_FILE_OPEN (UNITO,OUTIBIS,'WRITE',2,noverlap,
     +                       ibisformat,' ',STATUS)
        IF (STATUS .NE. PASS) CALL IBIS_SIGNAL_U(UNITO,STATUS,TRUE)
        DO COL = 1, 2
          do i=1,noverlap                  ! convert to real
             realover(i,col)=overlap(i,col)
          enddo
          call ibis_column_write (OUTIBIS, realover(1,col), col,
     +       1,noverlap,status)
       ENDDO
        CALL IBIS_FILE_CLOSE (OUTIBIS,'UDELETE',STATUS)
        IF (STATUS .NE. PASS) CALL IBIS_SIGNAL_U(OUTIBIS,STATUS,TRUE)
      endif

c close XRT/graph
ccc      call plot(0,0,999)		!XRT ====================
	close (98)
	if (epsplot) close(97)
      return

c   error returns
995     call xvmessage('??E - nplot: Error opening/writing gnuplot file',' ')
        call abend
        return
996     call xvmessage('??E - nplot: Error opening/writing gnuplot eps file',' ')
        call abend
        return


      end


c *********************************************************
      SUBROUTINE geoma2conv (unit, conv)
	implicit none
      integer*4 unit
      real*4    conv(*)

      integer*4  colcount
      character*8 nahstr, navstr, tiestr 

      nahstr = 'NAH'
      navstr = 'NAV'
      tiestr = 'TIEPOINT'
      colcount = 4

      call mvcl (nahstr, conv(1), 8)
      call mvcl (navstr, conv(4), 8)
      call mvcl (tiestr, conv(7), 8)

!!  FORTRAN Calling Sequence:  CALL IREAD_TIEPOINTS(UNIT,NAH,NAV,MAXPTS,
!!                                                  TIEPOINTS,COLCOUNT)

      conv(6) = 0.0
      call iread_tiepoints (unit, conv(3), conv(6), 3600,
     +                      conv(9), colcount)

      return
      end 

c *********************************************************
	SUBROUTINE RDFIL_READONLY (UNIT,INIBIS,INST,CLEN,NCOL,NOFILE)
c IBIS open
C Version for no update mode (read only).

	implicit none
	INTEGER*4 UNIT,CLEN,NCOL,INST
	INTEGER*4 NOFILE
c	INTEGER DEF,STATUS,inibis
	INTEGER*4 STATUS,inibis
	CHARACTER*200 FILENAME

      character*132 msg1

      CALL XVPONE('INP',FILENAME,INST,200)
C
      CALL XVUNIT (UNIT,'XXX',INST,STATUS,'U_NAME',FILENAME,' ')
      IF (STATUS.NE.1) THEN
        msg1 = ' '
        write (msg1( 1:35),'(a)') ' INPUT FILE INITIALIZATION ERROR - '
        write (msg1(36:66),'(a)') '             PROGRAM TERMINATED'
        call xvmessage(msg1, ' ')
        call abend
      END IF
      CALL IBIS_FILE_OPEN (UNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INIBIS,STATUS,1)
      CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)
      CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
	  RETURN
	  END
c*****************************************************************
        subroutine write_gpi_1(unit,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,title,ntitle,
     2 xrange1,xrange2,yrange1,yrange2,
     2 ploteps,nploteps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,psize
        integer*4 ntbl,nylabel,nxlabel,nploteps
        integer*4 ntitle
        real*4 xrange1,xrange2,yrange1,yrange2
        character*80 title
        character*16 ylabel,xlabel
        character*80 tbl,ploteps

c
	psize=isize
	if (unit .eq.97) psize = 16
10100 format('# Created by program mosplt')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for polygon')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)

        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,'  size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) ploteps(1:nploteps)
        else
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
            write(unit,fmt=10116,iostat=jj,err=995)
        endif

10120 format('set grid ')
            write(unit,fmt=10120,iostat=jj,err=995)
10125 format("set ylab '",a,"'" )
            write(unit,fmt=10125,iostat=jj,err=995) ylabel(1:nylabel)
10130 format("set xlab '",a,"'")
            write(unit,fmt=10130,iostat=jj,err=995) xlabel(1:nxlabel)

10141 format("set clip points")                         !how to deal with points out of range
            write(unit,fmt=10142,iostat=jj,err=995)
10142 format("set clip one")                            !how to deal with connecting lines out of range
            write(unit,fmt=10141,iostat=jj,err=995)

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c
10155 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10155,iostat=jj,err=995) yrange1,yrange2
10160 format("set xrange [",f8.2,":",f8.2,"]")
             write(unit,fmt=10160,iostat=jj,err=995) xrange1,xrange2

        return

995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi_1: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi_1: Error opening/writing gnuplot file',' ')
            call abend
        endif


        return
        end
c*****************************************************************
c*****************************************************************
        subroutine write_poly_gpi(unit,tbl,ntbl,ncolx,ncoly,npict,npoly,
     +     npolymax,msg,maxborder)
c
c       write out data in gpi form for polygons
c       Requires gnuplot V 4.4.0 or higher
c
        implicit none
        integer*4 unit,ntbl,maxborder,npolymax
        integer*4 gcol,i,jj,npict
	integer*4 npoly(npict)
        real*4 ncolx(npict,npolymax),ncoly(npict,npolymax)
        CHARACTER*12 MSG(npict)
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'

c	set object 1 polygon from 0,0 to 1,1 to 2,0
c       set object 1 fc rgb "cyan" fillstyle solid 1.0 border lt -1

	do i = 1,npict
cc	  do j=1,4
cc        print *, i, ' ncolx(ii,j) = ',ncolx(i,j),' ncoly(i) = ',ncoly(i,j)
cc	  enddo
        gcol = i
10240 format ("set object ",i2," polygon from ",f8.2,",",f8.2," to ",f8.2,",",f8.2," to ",
     + f8.2,",",f8.2," to ",f8.2,",",f8.2," lw 0.5 fc lt ",i2)     !," to ",i2,",",i2)
	write (unit,fmt=10240,iostat=jj,err=995) gcol,ncolx(gcol,1),ncoly(gcol,1),
     + ncolx(gcol,2),ncoly(gcol,2),ncolx(gcol,3),ncoly(gcol,3) ,ncolx(gcol,4),ncoly(gcol,4),gcol 
	enddo

10245 format ("plot NaN  notitle")		!tells it to plot whatever object(s) is in the canvas
	write (unit,fmt=10245,iostat=jj,err=995) 
        if (unit.eq.98) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
        endif

        return
995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_poly_gpi: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_poly_gpi: Error writing gnuplot file',' ')
        write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif
        return
        end

c*****************************************************************
      subroutine plot_resid(rbisbuf,maxpts,ncol,tiep,xx,yy,
     +                      exag,idground,ground,maxground,
     +                      nground,xscale,yscale)
c  plot residuals for each tiepoint & ground cntl pt.
c  NOTE: if stereographic is true then all latitudes are really lines
c       and all longitudes are really samples.
	implicit none
        integer*4 tiep,maxpts,ncol,j	
	integer*4 nground,idground,maxground
        real*4 ground(maxground,5)
        real*4 rbisbuf(maxpts,ncol),xx(4),yy(4)
        real*4   xscale, yscale,flag,x,y,x1,y1
	real*4 exag
        character*1 text
C    XRT ========================
        flag=1.0e+30

c rbisbuf(n,3)=left pix latitude
c rbisbuf(n,4)=left pix longitude
c rbisbuf(n,5)=right pix latitude
c rbisbuf(n,6)=right pix longitude
!### jj
c plot tiepoint residuals
        do 10 j=1,tiep
          if(rbisbuf(j,3).gt.flag)goto 10
          x1=rbisbuf(j,4)/xscale
          y1=rbisbuf(j,3)/yscale
ccc          call number(x1,y1,.14,real(j),0.0,-1)

          !! Put circle on plot 
ccc          call symbolanchor (9) !! XRT_ANCHOR_HOME
          text = 'o'
ccc          call symbol(x1,y1,.14,text,1,0.0,-1)
ccc          call symbolanchor (8) !! XRT_ANCHOR_BEST

ccc          call plot(x1,y1,3)
          x=rbisbuf(j,6)/xscale
          y=rbisbuf(j,5)/yscale
          x=exag*(x-x1)+x1
          y=exag*(y-y1)+y1
ccc          call plot(x,y,2)
10      continue 

c ground(n,2)=computed latitude using SEDR
c ground(n,3)=computed longitude using SEDR
c ground(n,4)=given latitude 
c ground(n,5)=given longitude 

c plot ground control point residuals
        if(idground.gt.0)then
         do 20 j=1,nground
          if(ground(j,2).gt.flag)goto 20
          x1=ground(j,3)/xscale
          y1=ground(j,2)/yscale

ccc          call number(x1,y1,.14,real(j),0.0,-1)

          !! Put 'x' on plot 
ccc          call symbolanchor (9) !! XRT_ANCHOR_HOME
          text = 'x'
ccc          call symbol(x1,y1,.14,text,2,0.0,-1)
ccc          call symbolanchor (8) !! XRT_ANCHOR_BEST
ccc          call plot(x1,y1,3)
          x=ground(j,5)/xscale
          y=ground(j,4)/yscale
          x=exag*(x-x1)+x1
          y=exag*(y-y1)+y1
ccc          call plot(x,y,2)
20       continue 
        endif

ccc        call plot( 0.0, 0.0,-3)
        return
        end

c****************************************************************
      subroutine plot_scale(corner,npoly,npict,
     +      maxborder,maxpix,xx,yy,stereographic,
     +      use_area,area,xscale,yscale,tbl,ntbl,
     +      epsplot,ploteps,nploteps,plotgpi,nplotgpi,
     +      plotgpi2,nplotgpi2)
c  Compute scale and draw axes of plots..
	implicit none
	integer*2 flag
	integer*4   II,npict,maxborder,maxpix
	integer*4 npoly(maxpix)
        integer*4 status,i,j,n
        integer*4 valscale,ntbl,nbanner,isize,nxlabel,nylabel
	integer*4 plotht,plotwid,icnt,nplotgpi,nplotgpi2,nploteps

        real*4 xx(4),yy(4),corner(2,maxborder,maxpix),area(4)
        real*4   firstv, xscale,yscale,temp,xrange,yrange
	real*4 labstep,tmp,xrange1,xrange2,yrange1,yrange2
	logical*4 stereographic,use_area,epsplot
	
	character*6 time_now
        character*8 timestr
        character*20 line
        character*20 sample
	character*20 ylabel,xlabel
	character*20 datestr
        character*80 banner
	character*80 tbl,ploteps,plotgpi,plotgpi2
        
        status = 1
c Establish the axis lengths
       xrange=1.0 
       yrange=1.0 

c title
        banner  = ' '
        timestr = ' '
        flag = 2
        II = 0
        call datfmt (flag, datestr, II)
cc        call time(timestr)
	call xtime(time_now)	!vicar rtl
c        call xvmessage (datestr,' ')
	write (timestr,10100) time_now(1:2),time_now(3:4),time_now(5:6)
10100 format (a2,':',a2,':',a2)
c        call xvmessage (timestr,' ')
        write (BANNER( 1:15),'(a15)') 'FOOTPRINT PLOT '
        write (BANNER(16:33),'(a17)')  datestr 
        write (BANNER(34:41),'(a8 )')  timestr
ccc        call header (banner, 1, 1) !! Title string, 1 line, adjust center
c***** commented out 8-23-2013 - Not really necessary in log and causes
c      testing problems when differencing logs
cc        call xvmessage(banner,' ')
	nbanner=index(banner,'   ') - 1
	icnt = 1
c
c        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
c        if (epsplot) then
c           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
c        endif

C--  GET THE SIZE OF THE GNUPLOT PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 16) then
           tmp = icnt/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10


c compute plot limits
        if (stereographic) then
           xx(1)=1.e+30
           xx(2)=-1.e+30
           yy(1)=1.e+30
           yy(2)=-1.e+30
           if (use_area) then
             xx(1)=area(3)
             xx(2)=area(4)
             yy(1)=area(1)
             yy(2)=area(2)
           else
             do i=1,npict
               do j=1,npoly(i)
                 if(corner(1,j,i).lt.xx(1)) xx(1)=corner(1,j,i)
                 if(corner(2,j,i).lt.yy(1)) yy(1)=corner(2,j,i)
                 if(corner(1,j,i).gt.xx(2)) xx(2)=corner(1,j,i)
                 if(corner(2,j,i).gt.yy(2)) yy(2)=corner(2,j,i)
               enddo
             enddo
           endif
           temp=yy(1)
           yy(1)=yy(2)
           yy(2)=temp
        else
           xx(1)=360.		!latitude - 
           xx(2)=0.0
           yy(1)=-90.
           yy(2)=90.
        endif

c compute scales
        xx(3)=xx(1)
        xx(4)=(xx(2)-xx(1))
        yy(3)=yy(1)
        yy(4)=(yy(2)-yy(1))

c plot axes & set scales
        n=0
        if (stereographic) then
           firstv = xx(4)
c           line   = ' '
c           write (line(1:4),'(a)') 'line'
c          find the scaling for the annotation values
           valscale = int(alog10(abs(firstv)))
           if (valscale .ge. -1  .and.  valscale .le. 1) valscale = 0

c          if the axis labels are scaled then print the exponent value 
           xscale = 1.0
	   sample(1:6) = 'sample'
           if (valscale .ge. 3) then
              xscale = 10**valscale
              write (sample( 7:12), '(a)') ' *10**'
              write (sample(13:13 ),'(i1)') valscale

           endif
           sample(14:20) = '       '

           firstv = yy(4)
c           sample = ' '
c           write (sample(1:6),'(a)') 'sample'
c          find the scaling for the annotation values
           valscale = int(alog10(abs(firstv)))
           if (valscale .ge. -1  .and.  valscale .le. 1) valscale = 0

c          if the axis labels are scaled then print the exponent value 
           yscale = 1.0
            line(1:4) = 'line'
           if (valscale .ge. 3) then
              yscale = 10**valscale
              write (line( 5:10), '(a)') ' *10**'   
              write (line(11:11 ),'(i1)') valscale   
           endif
          line(12:20) = '         '
           temp = xscale
           xscale = yscale
           yscale = temp

           ylabel = line
           nylabel =index(ylabel,'  ') - 1
           xlabel = sample
           nxlabel=index(xlabel,'  ') - 1
	   xrange1 = xx(1)/1.0e3
	   xrange2 = xx(2)/1.0e3
	   yrange1 = yy(1)/1.0e3
	   yrange2 = yy(2)/1.0e3
           call write_gpi_1(98,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,banner,nbanner,
     2 xrange1,xrange2,yrange1,yrange2,ploteps,nploteps)

           if (epsplot) then
               call write_gpi_1(97,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,banner,nbanner,
     2 xrange1,xrange2,yrange1,yrange2,ploteps,nploteps)

           endif

c   XRT=================
ccc           call axestitles (sample,line,90,' ',0)
           !! Reverse X axis annotation ... increase from right to left
ccc           call axesreverse (0, 1)  
           !! Set origins to cross at XRT_ORIGIN_MIN, Y=XRT_ORIGIN_MAX
ccc           call axesoriginplacement (2,3)
        else
	   ylabel = 'Latitude'
           nylabel =index(ylabel,'   ') - 1
           xlabel = 'West Longitude'
           nxlabel=index(xlabel,'  ') - 1
	   xrange1 = 360.0
           xrange2 = 0.0
           yrange1 = -90.0
           yrange2 = 90.0

           call write_gpi_1(98,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,banner,nbanner,
     2 xrange1,xrange2,yrange1,yrange2,
     3 ploteps,nploteps)

           if (epsplot) then
               call write_gpi_1(97,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,banner,nbanner,
     2 xrange1,xrange2,yrange1,yrange2,
     3 ploteps,nploteps)

           endif


           !! Display all three axes ... X, Y, & Y2 axes
ccc           call displayaxes (1, 1, 1)
           !! Reverse X axis annotation ... increase from right to left
ccc           call axesreverse (1, 0)  
           !! Set X axis at XRT_ORIGIN_ZERO & set Y axis at XRT_OTIGIN_MIN
ccc           call axesoriginplacement (1,2)
           !! Label the X, Y, & Y2 axes
cccc           call axestitles ('West Longitude','latitude',270,
c     &                      'latitude',90)
ccc           call plot(  0.0, 0.0, -3)
ccc           call plot(  0.0, 90.0, 3)
ccc           call plot(  0.0,-90.0, 3)
ccc           call plot(  0.0,  0.0, 3)
ccc           call plot(360.0,  0.0, 3)
ccc           call setaxesmaximums (360.0,  90.0,  90.0)
ccc           call setaxesminimums (  0.0, -90.0, -90.0)
        endif
       
        return

        end

c*****************************************************************
        subroutine plot_foot(corner,xbuf,ybuf,npoly,npict,
     +      maxborder,maxpix,xx,yy,xscale,yscale,epsplot,
     +      tbl,ntbl)
c  plot footprints for each frame.
	implicit none
	integer*4 maxborder,maxpix,npict,i,ii,j,n,val
	integer*4 npoly(maxpix),ntbl,npolymax
	real*4 x,y
        real*4 xbuf(maxborder+3),ybuf(maxborder+3)
        real*4 xx(4),yy(4),corner(2,maxborder,maxpix)
        real*4   xscale,yscale
	real*4 ncolx(npict,maxborder+3),ncoly(npict,maxborder+3)
	logical*4 epsplot

	character*12 MSG(maxborder+3)
	character*80  tbl
c
C Do not need to write out data to data file - polygons are objects in gnuplot       
c   XRT=========
c plot each footprint.
	npolymax = 0
        do i=1,npict
          n=npoly(i)
cc	print *,'npoly(i) = ',i,npoly(i)
	  if (n.gt.npolymax) npolymax = n
          do j=1,n                      ! copy data from each footprint
             xbuf(j)=corner(1,j,i)
             ybuf(j)=corner(2,j,i)
          enddo
          val=i
        
          x=(xbuf(n))/xscale
          y=(ybuf(n))/yscale
ccc          call number(x,y,.14,val,0.0,-1)  ! plot file number
          n=n+1
          xbuf(n)=xbuf(1)     ! repeat last point to close polygon
          ybuf(n)=ybuf(1)
          xbuf(n+1)= 0.0      ! add scale & offset
          xbuf(n+2)= 1.0
          ybuf(n+1)= 0.0
          ybuf(n+2)= 1.0
          if (xscale .ne. 1.0) then
             do II = 1, n
                xbuf(II) = xbuf(II)/xscale
             enddo
          endif
          if (yscale .ne. 1.0) then
             do II = 1, n
                ybuf(II) = ybuf(II)/yscale
             enddo
          endif
cc	  do ii = 1,n
cc		print *,'xbuf = ',xbuf(II)
cc		print *,'ybuf = ',ybuf(II)
cc	  enddo
	     write (msg(i),10005) i
10005 format ('footprint ',i2)
ccc          call line(xbuf,ybuf,n,1,0,3)  ! plot polygon

           do j=1,n
	      ncolx(i,j) = xbuf(j)
	      ncoly(i,j) = ybuf(j)
	   enddo
cc	print *,">",i,(xbuf(j), ybuf(j), j=1,n) 

        enddo		!npict
	call write_poly_gpi (98,tbl,ntbl,ncolx,ncoly,npict,npoly,
     +     npolymax,msg,maxborder)
        if (epsplot) then
           call write_poly_gpi (97,tbl,ntbl,ncolx,ncoly,npict,
     +     npoly,npolymax,msg,maxborder)
	endif
ccc        call plot(0.0,0.0,-3)
        return
        end

c **************************************************************
        subroutine setnav(left,right,
     +          maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
c Fills the DATAL & DATAR for convev subroutine.
	implicit none
	integer*4 maxpix,i
        real*8 r81,r82,r83,data8l(20),data8r(20)
        integer*4 left,right
        real*4 omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real*4 focal(1),optaxl(1),optaxs(1),scale(1),datal(40),datar(40)

c left image
        i=left
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8l)
        data8l(10)=rsvec(i,1)
        data8l(11)=rsvec(i,2)
        data8l(12)=rsvec(i,3)
        datal(25)=radpol(i)
        if(datal(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
c right image
        i=right
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8r)
        data8r(10)=rsvec(i,1)
        data8r(11)=rsvec(i,2)
        data8r(12)=rsvec(i,3)
        datar(25)=radpol(i)
        if(datar(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datar(26)=eqpol(i)
        datar(27)=focal(i)
        datar(28)=optaxl(i)
        datar(29)=optaxs(i)
        datar(30)=scale(i)         
        return
        end

c **************************************************************
        subroutine setnav1(file,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,
     +          datal)
c Fills the DATAL for convev subroutine.
	implicit none
	integer*4 maxpix
        real*8 r81,r82,r83,data8l(20)
        integer*4 file,i
        real*4 omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real*4 focal(1),optaxl(1),optaxs(1),scale(1),datal(40)

        i=file
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8l)
        data8l(10)=rsvec(i,1)
        data8l(11)=rsvec(i,2)
        data8l(12)=rsvec(i,3)
        datal(25)=radpol(i)
        if(datal(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
        return
        end

c *************************************************************

      subroutine getinput_id(idgeom,idpoints,idground)
c returns the INP # of the geom & tiepoints files if present
	implicit none
      integer*4 count,unit,status,inibis,nah
	integer*4 idgeom,idpoints,idground,i

      character*132 msg
      nah = 0
      call xvpcnt('INP',count)
      idgeom=0
      idpoints=0
      idground=0
      if(count.lt.2)return
      do 9999 i=2,count
        call xvunit(unit,'INP',i,status,' ')
        CALL IBIS_FILE_OPEN (UNIT,INIBIS,'READ',0,0,' ',' ',STATUS)

        if (status .ne. 1) then
           write (msg( 1:36),'(a)')
     +            'neither an IBIS or GEOMA file, inp# '
           write (msg(37:39),'(i3)') i
           call xvmessage (msg,' ')
           call abend
        end if
 
C      Get area size fields...
       NAH = 0       ! 0 IF NOT IN LABEL.
       CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',
     &        NAH,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')

       if (status .eq. 1) then
          idgeom=i
       else 
          if(idpoints.eq.0)then
             idpoints=i
          else
             idground=i
          endif
       endif
       call xvclose(unit,status,' ')
       call ibis_file_close (INIBIS, 'UDELETE',status)
9999   enddo
       return
       end


c **************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(3,3)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c
	real*8	cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8	cos_kappa, sin_kappa,dtr

        dtr = 3.141592653589793D0/180.d0
	sin_alpha = sin(alpha*dtr)
	cos_alpha = cos(alpha*dtr)
	sin_delta = sin(delta*dtr)
	cos_delta = cos(delta*dtr)
	sin_kappa = sin(kappa*dtr)
	cos_kappa = cos(kappa*dtr)
	c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta * sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta * sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta * cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta * cos_kappa
	c(2,3) =  cos_delta * cos_kappa
	c(3,1) =  cos_alpha * cos_delta
	c(3,2) =  sin_alpha * cos_delta
	c(3,3) =  sin_delta
	return
	end


