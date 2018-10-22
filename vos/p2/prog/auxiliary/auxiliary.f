c
c program auxiliary
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=360, maxline=180)
      real*4 obuf(maxsamp,5),inbuf(maxsamp,maxline,6)
      real*4 lat,lon,line,sample
      real*4 ee(maxsamp,maxline),ff(maxsamp,maxline)
      real*4 gg(maxsamp,maxline)
      real*4 conf_lat(maxsamp,maxline),conf_lon(maxsamp,maxline)
      real*4 cent_lat(maxsamp,maxline),cent_lon(maxsamp,maxline)
      real*4 values(2),candidate(2,4),violation(4)
      real*8 viol
      integer*4 ounit(5),def,count,passes,pass
      integer*4 unit(6),status,nl(6),ns(6)
      logical xvptst,conformal,authalic

c parameters
      call xvpcnt('INP',nids)      
      passes=nids/3-1    ! number of groups of EFG inputs
      conformal=xvptst('CONFORMAL')
      authalic=xvptst('AUTHALIC')
      call xvparm('NL',nlo,count,def,1)
      call xvparm('NS',nso,count,def,1)
      call xvparm('LOOP',nloop,count,def,1)
      call xvparm('RANGE',range,count,def,1)
      call xvparm('ANGLE',angle_weight,count,def,1)
      call xvparm('SCALE',scale_weight,count,def,1)

c set auxiliary conversions
      if(conformal)then
        call xvmessage('Conformal auxiliary coord conversions',' ')
      endif
      if(authalic)then
        call xvmessage('Authalic auxiliary coord conversions',' ')
      endif

c read EFG maps into memory
      do i=1,3
        call xvunit(unit(i),'INP',i,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        if(nl(i).gt.maxline)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        do j=1,nl(i)                    ! line loop
          call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
          call xvsignal(unit(i),status,1)
        enddo
      enddo

c read efg maps into memory
      do n=1,3
        i=n+3
        k=nids-3+n
        call xvunit(unit(i),'INP',k,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        if(nl(i).gt.maxline)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        do j=1,nl(i)                    ! line loop
          call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
          call xvsignal(unit(i),status,1)
        enddo
      enddo

c open 5 outputs
      if(nlo.eq.0)nlo=nl(1)
      if(nso.eq.0)nso=ns(1)
      do i=1,5
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvsignal(ounit(i),status,1)
        call xvopen(ounit(i),status,'U_FORMAT','REAL','O_FORMAT','REAL',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        call xvsignal(ounit(i),status,1)
      enddo


c renormalize the e,f,g data for comparability with the E,F,G equivalents.
      constraint_iso=0.0
      constraint_map=0.0
      do j=1,nl(4)                         ! line loop
        do i=1,ns(4)                          ! pixel loop

          if(conformal)then
            constraint_iso=constraint_iso+
     +         abs(inbuf(i,j,1))+abs(inbuf(i,j,3)) ! E + G
            constraint_map=constraint_map+
     +         abs(inbuf(i,j,4))+abs(inbuf(i,j,6)) ! e + g

          else if(authalic)then
            constraint_iso=constraint_iso+
     +      abs(inbuf(i,j,1)*inbuf(i,j,3)-(inbuf(i,j,2))**2) ! E*G-F*F
            constraint_map=constraint_map+
     +      abs(inbuf(i,j,4)*inbuf(i,j,6))  ! e*g
          endif
        enddo
      enddo

      efg_scale=constraint_iso/constraint_map
      write(*,*)'e,f,g rescaling factor= ',efg_scale
      do j=1,nl(4)                         ! line loop
        do i=1,ns(4)                          ! pixel loop
          inbuf(i,j,4)=inbuf(i,j,4)*efg_scale
          inbuf(i,j,5)=inbuf(i,j,5)*efg_scale
          inbuf(i,j,6)=inbuf(i,j,6)*efg_scale
        enddo
      enddo

c store ee,ff,gg buffers for the sphere

      do j=1,nlo                         ! line loop
        do i=1,nso                          ! pixel loop
  
c         Convert output image coordinate to auxiliary lat lon .
c         Auxiliary is either conformal or authalic.
          line=j
          sample=i
          call xy2ll(line,sample,nlo,nso,lat,lon)
          conf_lat(i,j)=lat
          conf_lon(i,j)=lon
          cent_lat(i,j)=lat
          cent_lon(i,j)=lon

c         Compute e,f,g for the sphere. They are called ee, ff, gg
c         Note: the efg sphere is in auxiliary coordinates already.
          call ll2xy(lat,lon,nl(4),ns(4),line,sample)
          call get_dn(line,sample,inbuf(1,1,4),nl(4),ns(4),
     +      ee(i,j),ind,maxline,maxsamp)
          call ll2xy(lat,lon,nl(5),ns(5),line,sample)
          call get_dn(line,sample,inbuf(1,1,5),nl(5),ns(5),
     +      ff(i,j),ind,maxline,maxsamp)
          if(conformal) ff(i,j)=0.0
          call ll2xy(lat,lon,nl(6),ns(6),line,sample)
          call get_dn(line,sample,inbuf(1,1,6),nl(6),ns(6),
     +      gg(i,j),ind,maxline,maxsamp)
        enddo
      enddo
      grid=conf_lat(1,1)-conf_lat(1,2)
      write(*,*)'grid spacing is ',grid,' degrees'

      pass=0
100   pass=pass+1

c compute largest violation of the constraint to normalize motion.
      biggest=-1.0e+30
      do j=2,nlo-1
        do i=1,nso
          candidate(1,1)=cent_lat(i,j)
          candidate(2,1)=cent_lon(i,j)
          violation(1) = 
     +           constraint(candidate(1,1),ee(i,j),ff(i,j),
     +            gg(i,j),inbuf,nl,ns,
     +            maxline,maxsamp,conformal,authalic,E,F,G,
     +            angle_weight,scale_weight,ind)
          if(ind.gt.0)then
            write(*,*)'Constraint 1'
            call abend
          endif
          if(violation(1).gt.biggest)biggest=violation(1)
        enddo
      enddo
      write(*,*)'largest constraint violation=',biggest

c iteration

      delta=range
      d=0.1
      do loop=1,nloop
        delta=delta*0.9
        viol=0.d0
        do j=1,nlo
          do i=1,nso

c           Evaluate constraint at 4 locations
            candidate(1,1)=cent_lat(i,j)+d  ! up
            candidate(2,1)=cent_lon(i,j)
            if(candidate(1,1).gt.90.0)candidate(1,1)=90.0
            candidate(1,2)=cent_lat(i,j)-d  ! down
            candidate(2,2)=cent_lon(i,j)
            if(candidate(1,2).lt.-90.0)candidate(1,2)=-90.0
            candidate(1,3)=cent_lat(i,j)
            candidate(2,3)=cent_lon(i,j)+d  ! left
            if(candidate(2,3).gt.360.0)
     +         candidate(2,3)=candidate(2,3)-360.
            candidate(1,4)=cent_lat(i,j)
            candidate(2,4)=cent_lon(i,j)-d  ! right
            if(candidate(2,4).lt.0.0)
     +         candidate(2,4)=candidate(2,4)+360.

            do k=1,4
              violation(k) = 
     +           constraint(candidate(1,k),ee(i,j),ff(i,j),
     +            gg(i,j),inbuf,nl,ns,
     +            maxline,maxsamp,conformal,authalic,E,F,G,
     +            angle_weight,scale_weight,ind)
              if(ind.gt.0)then
                write(*,*)'Constraint 2'
                write(*,*)'loop,i,j',loop,i,j
                write(*,*)'cand',candidate(1,k),candidate(2,k)
                write(*,*)'cent',cent_lat(i,j),cent_lon(i,j)
                call abend
              endif
            enddo
            v=(violation(1)+violation(2)+violation(3)+
     +         violation(4))/4.
            viol=viol+v

c           Compute gradient components
            dvdy=(violation(1)-violation(2))/
     +         (candidate(1,1)-candidate(1,2))
            dvdx=(violation(3)-violation(4))/(2.0*d)

c           Reset point
            scale=sqrt(dvdx*dvdx+dvdy*dvdy)+1.0e-6
            dvdx=dvdx/scale
            dvdy=dvdy/scale 
            cent_lat(i,j)=cent_lat(i,j)-delta*dvdy*v/biggest
            cent_lon(i,j)=cent_lon(i,j)-delta*dvdx*v/biggest
            if(cent_lat(i,j).gt.90.)cent_lat(i,j)=90.
            if(cent_lat(i,j).lt.-90.)cent_lat(i,j)=-90.

          enddo
        enddo
        viol=viol/(nlo*nso)
        write(*,*)'iteration ',loop,' violation ',viol

c       detect point swapping
        k1=0
        do j=1,nlo
          do i=2,nso
            x=cent_lon(i-1,j)-cent_lon(i,j)
            if(x.lt.0.0)then
              if(x.gt.-180.) k1=k1+1
            endif
          enddo
        enddo
        k2=0
        do i=1,nso
          do j=2,nlo
            if(cent_lat(i,j-1)-cent_lat(i,j).lt.0.0) k2=k2+1
          enddo
        enddo
        if(k1+k2.gt.0)then
           write(*,*)k1+k2,' crossovers detected on iteration ',loop
           write(*,*)'   ',k1,' in longitude ',k2,' in latitude '
        endif
      enddo

      call xvclose(unit(1),status,'CLOS_ACT','FREE',' ')
      call xvclose(unit(2),status,'CLOS_ACT','FREE',' ')
      call xvclose(unit(3),status,'CLOS_ACT','FREE',' ')

c read next EFG maps into memory
      if(pass.lt.passes)then
        do i=1,3
          k=pass*3+i
          call xvunit(unit(i),'INP',k,status,' ')
          call xvsignal(unit(i),status,1)
          call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
          call xvsignal(unit(i),status,1)
          call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
          call xvsignal(unit(i),status,1)
          if(ns(i).gt.maxsamp)then
            call xvmessage(' Model storage buffer too small',' ')
            call abend
          endif
          if(nl(i).gt.maxline)then
            call xvmessage(' Model storage buffer too small',' ')
            call abend
          endif
          do j=1,nl(i)                    ! line loop
            call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
            call xvsignal(unit(i),status,1)
          enddo
        enddo
        goto 100
      endif

c Write output files
      do j=1,nlo
        do i=1,nso
          obuf(i,1)=cent_lat(i,j)
          obuf(i,2)=cent_lon(i,j)
          values(1)=cent_lat(i,j)
          values(2)=cent_lon(i,j)
          x = constraint(values,ee(i,j),ff(i,j),gg(i,j),inbuf,nl,ns,
     +          maxline,maxsamp,conformal,authalic,E,F,G,
     +          angle_weight,scale_weight,ind)
          if(ind.gt.0)then
            write(*,*)'Constraint 3'
            call abend
          endif
          if(conformal)then
c           Compute tissot's angle
            call tissot(E,F,G,ee(i,j),ff(i,j),gg(i,j),dn,ind)
            obuf(i,3)=dn
          else
c           Compute area ratio
            top=ee(i,j)*gg(i,j)
            if(top.le.0.0)then
              top=0.0
            else
              top=sqrt(top)
            endif
            bottom=E*G-F*F
            if(bottom.le.0.0)then
              bottom=top
              if(bottom.eq.0.0)bottom=1.0
            else
              bottom=sqrt(bottom)
            endif
            obuf(i,3)=top/bottom
          endif
          obuf(i,4)=cent_lat(i,j)-conf_lat(i,j)
          obuf(i,5)=cent_lon(i,j)-conf_lon(i,j)
          if(obuf(i,5).lt.-180.)obuf(i,5)=obuf(i,5)+360.
          if(obuf(i,5).gt.180.)obuf(i,5)=obuf(i,5)-360.
        enddo
        do k=1,5
          call xvwrit(ounit(k),obuf(1,k),status,' ')
          call xvsignal(ounit(k),status,1)
        enddo
      enddo                                  ! line loop

      return
      end

c*************************************************************************
      function constraint(values,ee,ff,gg,inbuf,nl,ns,
     +  maxline,maxsamp,conformal,authalic,E,F,G,
     +  angle_weight,scale_weight,ind)

c returns the constraint given:
c  ee,ff,gg for the sphere,
c  centric lat & lon,
c  inbuf(*,*,3) containing the 1=E,2=F,3=G values

      real*4 inbuf(maxsamp,maxline,6),lat,lon,line,sample
      real*4 values(2) ! 1=centric lat, 2=centric lon
      integer*4 nl(6),ns(6)
      logical conformal,authalic

c     Compute E,F,G for the ISO. They are called E, F, G.
c     Note: the EFG maps are in centric coordinates already.

      lat=values(1)
      lon=values(2)
      call ll2xy(lat,lon,nl(2),ns(2),line,sample)
      call get_dn(line,sample,inbuf(1,1,2),nl(2),ns(2),F,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10
      call ll2xy(lat,lon,nl(1),ns(1),line,sample)
      call get_dn(line,sample,inbuf(1,1,1),nl(1),ns(1),E,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10
      call ll2xy(lat,lon,nl(3),ns(3),line,sample)
      call get_dn(line,sample,inbuf(1,1,3),nl(3),ns(3),G,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10

      if(conformal)then
c        angle_constraint=abs(F/(sqrt(E*G)+1.0))  ! ff=0
c        scale_constraint=abs(E/ee-G/(gg+1.0))
        angle_constraint=abs(F*sqrt(ee*gg))  ! ff=0
        scale_constraint=abs(E*gg-ee*G)
        constraint=angle_weight*angle_constraint +
     +             scale_weight*scale_constraint
      else if(authalic)then
        constraint=abs( E*G-F*F-ee*gg ) + abs(F*sqrt(ee*gg)) ! ff=0
c        constraint=abs( (E*G-F*F)-(ee*gg-ff*ff) )
      else
        write(*,*)'must be conformal or authalic'
        ind=1
        return
      endif
      constraint=(constraint)**0.5
      ind=0
      return

10    continue
      write(*,*)'constraint error'
      write(*,*)'lat,lon=',lat,lon
      write(*,*)'line,sample=',line,sample
      ind=1
      return
      end

c*************************************************************************
      subroutine tissot(E,F,G,ee,ff,gg,dn,ind)
c returns as dn the Tissot indicatrix in degrees.
      implicit real*4(a-z)
      integer*4 ind

          ind=1
          dn=0.0
          degtorad=57.2957795

c             Solve quadratic for two roots t1 & t2
              a1=G*ff-F*gg
              if(a1.eq.0.d0)return
              b1=G*ee-E*gg
              c1=F*ee-E*ff
              d1=b1*b1-4.d0*a1*c1
              if(d1.lt.0.d0)return
              d1=sqrt(d1)
              t1=(-b1+d1)/(2.d0*a1)
              t2=(-b1-d1)/(2.d0*a1)

c             Solve for two mu values
              a1=E+2.d0*F*t1+G*t1*t1
              if(a1.eq.0.d0)return
              mu1=(ee+2.d0*ff*t1+gg*t1*t1)/a1
              if(mu1.lt.0.d0)return
              mu1=sqrt(mu1)
              a1=E+2.d0*F*t2+G*t2*t2
              if(a1.eq.0.d0)return
              mu2=(ee+2.d0*ff*t2+gg*t2*t2)/a1
              if(mu2.lt.0.d0)return
              mu2=sqrt(mu2)

c             Compute tissot's ellipse dimensions
              tissot_a = max(mu1,mu2)
              tissot_b = min(mu1,mu2)

c             Compute the angle deformation in degrees
              dn=(tissot_a - tissot_b)/(tissot_a + tissot_b)
              dn=2.0*asin(dn)*degtorad

          ind=0
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
      subroutine xy2ll(line,sample,nl,ns,lat,lon)
c convert line sample to lat lon.

      real*4 lat,lon
      real*4 line,sample

      lat=((line-1.0)*180./(1.0-nl))+90.0
      t=(ns+1.0)/2.0
      if(sample .gt. t)then
        lon=(sample+t-2.0*ns)*180./(t-ns)
      else
        lon=(sample-t)*180./(1.0-t)
      endif

      return
      end


c********************************************************************
      subroutine get_dn(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the DN value at this image coordinate.
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


