c
c program mapiso
c
      include 'VICMAIN_FOR'
      subroutine main44
      include 'mp_for_defs'

      parameter ( maxsamp=2000, max_ns_model=1440, max_nl_model=720)
      parameter ( max_nl=180, max_ns=360)
      character*100 filename,path,msg
      character*12 planet
      character*30 projection_name
      integer*2 obuf1(maxsamp),object(max_ns_model,max_nl_model)
      integer*4 ounit(2),def,count
      integer*4 unit,inunit(4),status,nl(4),ns(4)
      real*4 cent_lat(max_ns,max_nl),cent_lon(max_ns,max_nl)
      real*4 tissot(max_ns,max_nl),obuf2(maxsamp)
      real*8 map_lin,map_sam,map_lat,map_lon
      real*8 radius(3),mp
      logical xvptst,north,south

      call xvmessage('*** mapaux version 2017-08-09 ***', ' ')

c parameters
      call xvpcnt('INP',nids)      
      call xvpcnt('OUT',nods)      
      call xvparm('PATH',path,count,def,1)
      call xvparm('PLANET',planet,count,def,1)
      call xvparm('GRID',grid,count,def,1)
      north=xvptst('NORTH')
      south=xvptst('SOUTH')

c read object projection into memory
c This is the DN map of the object stored as a rectangular projection
c with west longitude and zero longitude and latitude in the center..
      call projectionmodel(filename,path,planet)
      call xvmessage('Reading object projection '//filename,' ')
      call xvunit(unit,'OLD',1,status,'U_NAME',filename,' ')
      call xvsignal(unit,status,1)
      call xvopen(unit,status,'U_FORMAT','HALF',' ')
      call xvsignal(unit,status,1)
      call xvget(unit,status,'NL',nl_map,'NS',ns_map,' ')
      call xvsignal(unit,status,1)
      if(ns_map.gt.max_ns_model)then
        call xvmessage('Model storage buffer too small',' ')
        call abend
      endif
      if(nl_map.gt.max_nl_model)then
        call xvmessage('Model storage buffer too small',' ')
        call abend
      endif
      do line=1,nl_map                         ! line loop
        call xvread(unit,object(1,line),status,'LINE',line,' ')
        call xvsignal(unit,status,1)
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      write(msg,*)'Map dimensions nl=',nl_map,' ns=',ns_map
      call xvmessage(msg,' ')

c draw grid on map
      if(grid.gt..001) call grid_it(grid,object,nl_map,ns_map,
     +                              max_nl_model,max_ns_model)

c open inputs
      do i=1,nids
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvsignal(inunit(i),status,1)
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(inunit(i),status,1)
        call xvget(inunit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(inunit(i),status,1)
        if(i.gt.1)then
          if(ns(i).gt.max_ns)then
            call xvmessage('Line length too long on files 2-4',' ')
            call abend
          endif
          if(nl(i).gt.max_nl)then
            call xvmessage('Too many lines on files 2-4',' ')
            call abend
          endif
        endif
      enddo

c load inputs 2-3 into buffers
      do line=1,nl(2)                         ! line loop
        call xvread(inunit(2),cent_lat(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(2),status,1)
      enddo
      do line=1,nl(3)                         ! line loop
        call xvread(inunit(3),cent_lon(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(3),status,1)
      enddo
      do line=1,nl(4)                         ! line loop
        call xvread(inunit(4),tissot(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(4),status,1)
      enddo

c open outputs
      nlo=nl(1)
      nso=ns(1)
      do i=1,nods
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvsignal(ounit(i),status,1)
        if(i.eq.1)
     +    call xvopen(ounit(i),status,'U_FORMAT','HALF',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        if(i.eq.2)
     +    call xvopen(ounit(i),status,'U_FORMAT','REAL',
     +          'O_FORMAT','REAL','U_NL',nlo,'U_NS',nso,
     +          'OP','WRITE',' ')
        call xvsignal(ounit(i),status,1)
      enddo

c initialize mp routines
      if(north.or.south) goto 11
      call mp_init( mp,istat) 
      if(istat.ne.mp_success) call mabend('error in mp_init')
      call mp_label_read( mp, inunit(1), istat)
      if(istat.ne.mp_success) call mabend(' mp_label_read error')

c extract output projection properties
      call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',
     +  projection_name,istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value_str error')
      call mp_get_value(mp,'A_AXIS_RADIUS',radius(1),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')
      call mp_get_value(mp,'B_AXIS_RADIUS',radius(2),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')
      call mp_get_value(mp,'C_AXIS_RADIUS',radius(3),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')

c check for equal radii in map3 picture label
      if((radius(1).ne.radius(2)).or.(radius(1).ne.radius(3)))then
        call xvmessage('Input label radii must be equal',' ')
        call abend
      endif
11    continue

c process image

c     Process every pixel
      radtodeg=57.2957795
      scale=nso/180.
      do line=1,nlo                         ! line loop
        map_lin=line
        do i=1,nso                        ! pixel loop
          map_sam=i
  
c         Convert output line & sample to west auxiliary lat & lon.
          if(north.or.south)then
            radiuss=sqrt((nlo/2.0-line)**2+(nso/2.0-i)**2)
c            if(radiuss.gt.nso/2)then
c                dn=0
c                obuf2(i)=0.0
c                ind=1
c                goto 10
c            endif
            aux_lon=radtodeg*atan2(nso/2.0-i,line-nlo/2.0)
            if(aux_lon.lt.0.0)aux_lon=aux_lon + 360.
            aux_lat=90.-radiuss/scale
            if(south)then
              aux_lon=360.-aux_lon+180.
              if(aux_lon.gt.360.)aux_lon=aux_lon-360.
              aux_lat=-aux_lat
            endif
          else
            call mp_xy2ll(mp,map_lin,map_sam,map_lat,map_lon,1,status)
            if(status.ne.0)then
              if(status.eq.-1)then
                dn=0
                obuf2(i)=0.0
                ind=1
                goto 10
              endif
              call xvmessage('mp_xy2ll: fatal status',' ')
              write(msg,*)'Status =',status
              call xvmessage(msg,' ')
              call abend
            endif
            aux_lat=map_lat
            aux_lon=map_lon
          endif

c         Convert auxiliary lat & lon to line & sample in the lat/lon
c         lookup table (input images 2 & 3).
          call ll2xy(aux_lat,aux_lon,nl(2),ns(2),rline,sample)

c         Convert auxiliary lat & lon to planetocentric lat & lon
c         Latitude
          call get_dnr(rline,sample,cent_lat,nl(2),ns(2),dn,ind,
     +          max_nl,max_ns)
          centric_lat=dn
c         Longitude
          call get_dnlon(rline,sample,cent_lon,nl(3),ns(3),dn,ind,
     +          max_nl,max_ns)
          centric_lon=dn

c         Get the tissot angle at this location.
          call get_dnr(rline,sample,tissot,nl(4),ns(4),dn,ind,
     +          max_nl,max_ns)
          obuf2(i)=dn

c         Convert centric lat & lon into image coordinates for the object map
          call ll2xy(centric_lat,centric_lon,nl_map,ns_map,
     +         rline,sample)
 
c         and return the DN value at this planetocentric coordinate.
          call get_dni(rline,sample,object,nl_map,ns_map,
     +         dn,ind,max_nl_model,max_ns_model)

10        obuf1(i)=nint(dn)

        enddo                                ! pixel loop
        call xvwrit(ounit(1),obuf1,status,' ')
        call xvsignal(ounit(1),status,1)
        call xvwrit(ounit(2),obuf2,status,' ')
        call xvsignal(ounit(2),status,1)
      enddo                                  ! line loop

      return
      end


c********************************************************************
      subroutine grid_it(grid,object,nl_map,ns_map,
     +       max_nl_model,max_ns_model)
c draw grid on image

      integer*2 object(max_ns_model,max_nl_model)
      real*4 sample,line,lat,lon

      lon=-grid
10    lon=lon+grid

c       convert lat/lon to line/sample
        t=(ns_map+1.0)/2.0
        if(lon.gt.180.)then
           sample=-(ns_map-t)*lon/180. + 2.0*ns_map - t
        else
           sample=-(t-1.0)*lon/180.+t
        endif
        i=nint(sample)
        if((i.ge.1).and.(i.le.ns_map))then
          do k=1,nl_map
             object(i,k)=255
          enddo
        endif
      if(lon+grid.lt.360.)goto 10

      lat=-90-grid
20    lat=lat+grid

c       convert lat/lon to line/sample
        line=(1.0-nl_map)*(lat-90.0)/180. + 1.0
        j=nint(line)
        if((j.ge.1).and.(j.le.nl_map))then
          do k=1,ns_map
             object(k,j)=255
          enddo
        endif
      if(lat+grid.lt.90.)goto 20

      return
      end

c********************************************************************
      subroutine ll2xy(lat,lon,nl,ns,line,sample)
c Convert lat & lon into image coordinates for the object map
 
      real*4 lat,lon
      real*4 line,sample
 
c convert lat/lon to line/sample
      lon=mod(lon,360.)
      if(lon.lt.0.) lon=360.-lon
      t=(ns+1.0)/2.0
      if(lon.gt.180.)then
         sample=-(ns-t)*lon/180. + 2.0*ns - t
      else
         sample=-(t-1.0)*lon/180.+t
      endif
      line=(1.0-nl)*(lat-90.0)/180. + 1.0
 
      return
      end

c********************************************************************
      subroutine get_dnlon(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the longitude value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0
 
      real*4 object(maxsamp,maxline),dn
      real*4 sample,line
 
c if are inside picture interpolate
      i=sample
      j=line
      if((i.ge.1).and.(i.lt.ns).and.(j.ge.1).and.
     + (j.lt.nl))then
         if(abs(object(i,j)-object(i+1,j)).gt.180.)goto 10
         if(abs(object(i,j)-object(i,j+1)).gt.180.)goto 10
         if(abs(object(i,j)-object(i+1,j+1)).gt.180.)goto 10

c        No 360 wraparound condition.
         wt=sample-i
         dntop=wt*object(i+1,j)+(1.0-wt)*object(i,j)
         dnbot=wt*object(i+1,j+1)+(1.0-wt)*object(i,j+1)
         dn=(line-j)*dnbot+(j+1-line)*dntop
         ind=0
         return

c        Have a 360 wraparound condition.
10       continue
         t1=object(i,j)
         t2=object(i+1,j)
         t3=object(i,j+1)
         t4=object(i+1,j+1)
         if(t1.lt.180.)t1=t1+360.
         if(t2.lt.180.)t2=t2+360.
         if(t3.lt.180.)t3=t3+360.
         if(t4.lt.180.)t4=t4+360.
         wt=sample-i
         dntop=wt*t2+(1.0-wt)*t1
         dnbot=wt*t4+(1.0-wt)*t3
         dn=(line-j)*dnbot+(j+1-line)*dntop
         if(dn.gt.360.)dn=dn-360.
         ind=0
         return
      endif
 
c if are outside picture return a zero
      ind=1
      dn=0.0
      i=nint(sample)
      j=nint(line)
      if(i.lt.1)return
      if(i.gt.ns)return
      if(j.lt.1)return
      if(j.gt.nl)return
 
c if are on picture border take nearest pixel
      ind=0
      dn=object(i,j)
 
      return
      end

c********************************************************************
      subroutine get_dnr(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the REAL DN value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0
 
      real*4 object(maxsamp,maxline),dn
      real*4 sample,line
 
c if are inside picture interpolate
      i=sample
      j=line
      if((i.ge.1).and.(i.lt.ns).and.(j.ge.1).and.
     + (j.lt.nl))then
         wt=sample-i
         dntop=wt*object(i+1,j)+(1.0-wt)*object(i,j)
         dnbot=wt*object(i+1,j+1)+(1.0-wt)*object(i,j+1)
         dn=(line-j)*dnbot+(j+1-line)*dntop
         ind=0
         return
      endif
 
c if are outside picture return a zero
      ind=1
      dn=0.0
      i=nint(sample)
      j=nint(line)
      if(i.lt.1)return
      if(i.gt.ns)return
      if(j.lt.1)return
      if(j.gt.nl)return
 
c if are on picture border take nearest pixel
      ind=0
      dn=object(i,j)
 
      return
      end

c********************************************************************
      subroutine get_dni(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the INTEGER DN value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0
 
      integer*2 object(maxsamp,maxline)
      real*4 sample,line,dn
 
c if are inside picture interpolate
      i=sample
      j=line
      if((i.ge.1).and.(i.lt.ns).and.(j.ge.1).and.
     + (j.lt.nl))then
         wt=sample-i
         dntop=wt*object(i+1,j)+(1.0-wt)*object(i,j)
         dnbot=wt*object(i+1,j+1)+(1.0-wt)*object(i,j+1)
         dn=(line-j)*dnbot+(j+1-line)*dntop
         ind=0
         return
      endif
 
c if are outside picture return a zero
      ind=1
      dn=0.0
      i=nint(sample)
      j=nint(line)
      if(i.lt.1)return
      if(i.gt.ns)return
      if(j.lt.1)return
      if(j.gt.nl)return
 
c if are on picture border take nearest pixel
      ind=0
      dn=object(i,j)
 
      return
      end


c********************************************************************
      subroutine projectionmodel(filename,path,planet)
c build projection filename
      character*100 filename,path
      character*12 planet
      do i=1,100
        filename(i:i)=' '
      enddo
      do i=1,100
        if(path(i:i).ne.' ')then
           filename(i:i)=path(i:i)
           j=i
        else
           goto 1
        endif
      enddo
1     if(filename(j:j).ne.'/')then
        j=j+1
        filename(j:j)='/'
      endif
      do i=1,12
        if(planet(i:i).ne.' ')then
           j=j+1
           filename(j:j)=planet(i:i)
        else
           goto 2
        endif
      enddo
2     filename(j+1:j+5)='.img'
      return
      end

