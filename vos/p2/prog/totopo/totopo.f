c
c program totopo
c
      include 'VICMAIN_FOR'
      subroutine main44

      include 'mp_for_defs'  !needed for MP software
      parameter (maxbuf=1000)
      character*12 planet/'            '/
      character*5 project(2),mode
      integer*4 fdsno,ilab(100,2),scr(1800),camera(2)
      integer*4 idata(40,2),tpt,inunit(2),count,def
      real*4 data(40,2),image_coords(4,32)
      real*4 lat,long,line,samp
      real*4 xyz(4,32),nodata,inten
      real*8 latrad,x,y,z,r8
      real*8 raddeg,rpole,req,MP(2)
      logical xvptst,debug/.false./,xyzmode/.false./
      real*4 inbuf(maxbuf*maxbuf)
      real*4 outbuf(maxbuf*maxbuf)
      equivalence (r8,idata,data)      

      raddeg=180.0d0/3.141592653589d0

c Get parameters:
      if(xvptst('DEBUG')) debug=.true.
      if(xvptst('XYZ')) xyzmode=.true.
      call xvparm('MODE',mode,count,def,1)
      call xvparm('NODATA',nodata,count,def,1)
      call xvparm('RADIUS',nw,count,def,1)
      call xvparm('MINPTS',minpts,count,def,1)
      call xvparm('MAXPASS',maxpass,count,def,1)

c open files
      call xvunit(inunit(1),'INP',1,status,' ')
      call xvopen(inunit(1),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS','U_FORMAT','REAL',' ')
      call xvget(inunit(1),status,'NL',nl,'NS',np,' ')
      if(nl*np.gt.maxbuf*maxbuf)then
        write(*,*)'Max # input pixels is:',maxbuf*maxbuf
        call abend
      endif

      call xvunit(inunit(2),'INP',2,status,' ')
      call xvopen(inunit(2),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS','U_FORMAT','REAL',' ')

      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(inxyz,'INP',4,status,' ')
      call xvopen(inxyz,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inxyz,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','REAL','O_FORMAT',
     +            'REAL','OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')


c Extract the navigation for the input images and put it in DATA.
c DATA(1-40,1) is for the Map projected first input image.
c DATA(1-40,2) is for the Map projected second input image.
      do image=1,2

c        Get project names
         call getproj(inunit(image),project(image),camera(image),
     +                fdsno,ind)
         if(ind.ne.0)then
            call mabend('Unknown flight project')
         endif
      
c        Get label contents
         call getlabcon(inunit(image),project(image),ilab(1,image),
     +                  ind)
         if(ind.gt.1)then
            write(*,*)'Getlabcon: Fatal error',ind
         endif

c        Retrieve the MAP labels from the projections for navigating
c        around in projection space.
c         call searcv2(inunit(image),i,%descr(scr),data(1,image),
c     +                data(1,image))
         call mp_init(mp(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_init')
         call mp_label_read(mp(image),inunit(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_read')
         call mp_mpo2buf( mp(image),data(1,image),istat)    !C MP
         if(istat.ne.mp_success) call mabend('error in mpo2buf')
        
c        Check input types
         if((idata(39,image).eq.7).or.(idata(39,image).eq.8))then
            call mabend('Inputs must be map projections')
         endif

      enddo

c Load input into memory
      if(mode.eq.'ORTHL')then
        image=1
        index=1
        do 160 lrec=1,nl
          i=(lrec-1)*np+1
          call xvread(inunit(1),inbuf(i),status,'LINE',lrec,' ')
160     continue
      else if(mode.eq.'ORTHR')then
        image=2
        index=3
        do 161 lrec=1,nl
          i=(lrec-1)*np+1
          call xvread(inunit(2),inbuf(i),status,'LINE',lrec,' ')
161     continue
      else if(mode.eq.'TOPO ')then
c       no image needed !
        image=1
      else if(mode.eq.'ERROR')then
c       no image needed !
        image=1
      else
        call mabend('Unrecognized mode')
      endif

c clear output buffer.
      call mve(7,nl*np,nodata,outbuf,0,1)

c close first 2 inputs.
      call xvclose(inunit(1),ind,' ')
      call xvclose(inunit(2),ind,' ')

      rpole=data(25,1) ! planet polar radius
      req=data(26,1)   ! planet equatorial radius
      slope=1.0
      offset=0.0

c Loop on coordinate records.
      do 140 lrec=1,nl3
        call xvread(inxyz,xyz,status,'LINE',lrec,' ')
        call xvread(inmark,image_coords,status,'LINE',lrec,' ')

c       Loop on tiepoints
        do 150 tpt=1,32
          if((xyz(1,tpt).eq.0.0).and.
     +       (xyz(2,tpt).eq.0.0)) goto 150

          if(xyzmode)then  ! convert from XYZ to LAT,LON
             x=xyz(1,tpt)
             y=xyz(2,tpt)
             z=xyz(3,tpt)
             latrad=datan2(z,dsqrt(x*x+y*y)) ! centric latitude
             lat=raddeg*latrad
             long=360.d0-raddeg*datan2(y,x)  ! W longitude
             if(long.gt.360.d0) long=long-360.d0
             range=dsqrt(x*x+y*y+z*z)
             rad=rpole*req/dsqrt(rpole*rpole*(dcos(latrad))**2+
     +                           req*req*(dsin(latrad))**2)
             xyz(1,tpt)=lat        ! Geocentric lat. deg.
             xyz(2,tpt)=long       ! W long deg.
             xyz(3,tpt)=range-rad  ! Elevation above geoid Km.
          endif

c         Convert lat,lon to input projection line,samp
c         This is the really correct line,sample.
          lat=xyz(1,tpt)
          long=xyz(2,tpt)
          call convev(ind,data(1,image),data(1,image),
     +                line,samp,lat,long,1,scr)
          if(ind.ne.0) goto 150

          if(debug)then
             write(*,*)'Projection lat,long,->line,samp'
             write(*,99) lat,long,line,samp
          endif

c         Check if in picture.
          iline=nint(line)
          isamp=nint(samp)
          if(isamp.lt.1.or.isamp.gt.np) goto 150
          if(iline.lt.1.or.iline.gt.nl) goto 150

          if(mode.eq.'TOPO ')then

c           Write the elevation to the output at the true location.
            i=(iline-1)*np+isamp
            elev=slope*xyz(3,tpt)+offset
            outbuf(i)=elev

          else if(mode.eq.'ERROR')then

c           Write the error to the output at the true location.
            i=(iline-1)*np+isamp
            elev=slope*xyz(4,tpt)+offset
            outbuf(i)=elev

          else

c           Get the DN value from the input location designated by the
c           original tiepoint line,sample pair.
            i=(image_coords(index,tpt)-1)*np+image_coords(index+1,tpt)
            inten=inbuf(i)

c           Write this intensity to the output at the true location.
            i=(iline-1)*np+isamp
            outbuf(i)=inten

          endif

150     continue
140   continue

c fillin gores
      call fill_gores(outbuf,inbuf,nl,np,nodata,
     +  nw,minpts,maxpass)

c     Write output.
      i=1-np
      do lrec=1,nl
        i=i+np
        call xvwrit(outunit,outbuf(i),status,'LINE',lrec,' ')
      enddo

99    format(4f10.5)

      return
      END

c**************************************************************
      subroutine fill_gores(buf,bufnew,nl,np,nodata,
     + nw,minpts,maxpass)
      real*4 buf(np,nl),bufnew(np,nl),nodata
      real*4 dn(1000),r(1000)
      integer*4 count,pass

c     Fillin the missing pixels.

      do j=1,nl
        do i=1,np
          bufnew(i,j)=nodata
        enddo
      enddo
      nl1=nl
      ns1=np
      rnw=nw*nw+.1 ! radius squared of collection circle + a bit
      pass=0
100   count=0
      pass=pass+1
      do j=1,nl1
        do i=1,ns1
          if(buf(i,j).ne.nodata)goto 10
          k=0
          do jj=max(j-nw,1),min(j+nw,nl1)
            j2=(j-jj)**2
            do ii=max(i-nw,1),min(i+nw,ns1)
              if(buf(ii,jj).ne.nodata)then
c               rad=(i-ii)**2+(j-jj)**2
                rad=(i-ii)**2+j2
                if(rad.le.rnw)then
                  k=k+1
                  dn(k)=buf(ii,jj)
                  r(k)=rad
                endif
              endif
            enddo
          enddo
          if(k.lt.minpts)goto 10
          sum1=0.0
          sum2=0.0
          do n=1,k
            sum1=sum1+dn(n)/r(n)
            sum2=sum2+1.0/r(n)
          enddo
          bufnew(i,j)=sum1/sum2
          count=count+1
10        continue
        enddo
      enddo
      do j=1,nl1
        do i=1,ns1
          if(bufnew(i,j).ne.nodata)then
            buf(i,j)=bufnew(i,j)
            bufnew(i,j)=nodata
          endif
        enddo
      enddo
      write(*,*)count,' pixels interpolated'
      if((count.gt.0).and.(pass.lt.maxpass))then
        goto 100
      endif

      return
      end

