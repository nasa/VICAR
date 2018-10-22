      subroutine triaxtran(ain,bin,cin,cc,cp,ac,ap,nlimit,
     +     klimit,mlimit,inlat,inlon,infmt,
     +     outlat,outlon,outfmt,ind)

c Routine to convert between any type of lat/lon and any other type of 
c lat/lon on the triaxial ellipsoid.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c A = major axis radius of the triaxial ellipsoid 
c     at lat=0 eastlon=0.                              input real*8
c     Normalized to A=1, B=B/A, C=C/A
c B = next major axis radius of the triaxial ellipsoid 
c     at lat=0 eastlon=90.                             input real*8
c     Normalized to A=1, B=B/A, C=C/A
c C = polar axis radius of the triaxial ellipsoid.     input real*8
c     Normalized to A=1, B=B/A, C=C/A
c CC = C matrix of coefficients             input      real*8
c      for computing conformal longitude
c      CC is dimensioned cc(nlimit,mlimit)
c CP = C primed matrix of coefficients      input      real*8
c      for computing conformal latitude
c      CP is dimensioned cp(nlimit,mlimit)
c AC = COEF matrix of coefficients          input      real*8
c      for computing authalic longitude
c      AC is dimensioned AC(0:mlimit,0:klimit)
c AP = COEFP primed array of coefficients   input      real*8
c      for computing authalic latitude
c      AP is dimensioned AP(nlimit)
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c inlat= input latitude in degrees          input      real*8
c inlon= input longitude in degrees east    input      real*8
c infmt= the input lat/lon type             input      integer*4
c outlat= output latitude in degrees        input      real*8
c outlon= output longitude in degrees east  input      real*8
c outfmt= the output lat/lon type           input      integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4
c The infmt & outfmt codes are:
c  1 means planetocentric latitude & longitude
c  2 means planetodetic latitude & longitude
c  3 means Snyder latitude & longitude
c  4 means conformal latitude & longitude
c  5 means authalic latitude & longitude
c For example, to convert from centric to conformal, infmt=1 & outfmt=4

      implicit real*8 (a-h,o-z)
      real*8 cc(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 ac(0:mlimit,0:klimit),ap(nlimit)
      real*8 inlat,inlon,outlat,outlon
      integer*4 infmt,outfmt

      ind=0
      degtorad=datan(1.d0)/45.d0

      if(ain.eq.1.d0)then
        a=ain
        b=bin
        c=cin
      else
        a=1.d0
        b=bin/ain
        c=cin/ain
      endif
      
      if(outfmt.eq.5)then            ! authalic output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.3)then     ! snyder input
            call authtran(ac,ap,nlimit,klimit,mlimit,inlat,inlon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.5)then     ! authalic input
            outlat=inlat
            outlon=inlon
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.4)then            ! conformal output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.3)then     ! snyder input
            call conftran(cc,cp,nlimit,mlimit,inlat,inlon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.4)then     ! conformal input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.3)then            ! snyder output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.3)then     ! snyder input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,outlat,outlon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,outlat,outlon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.2)then            ! detic output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.2)then     ! detic input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.3)then     ! snyder input
            call snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.1)then            ! centric output

         if(infmt.eq.1)then          ! centric input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.3)then     ! snyder input
            call snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else
        call xvmessage('TRIAXTRAN: Illegal output format.',' ')
        ind=1
      endif

      return
      end


c ********************************************************************
      subroutine xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to planetocentric lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= planetocentric latitude in degrees
c outlon= planetocentric longitude in degrees east

      implicit real*8 (a-z)
c     r=dsqrt(x*x+y*y+z*z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      outlat=datan2(z,dsqrt(x*x+y*y))/degtorad
      outlon=datan2(y,x)/degtorad
      if(outlon.lt.0.d0) outlon=360.d0+outlon
      return
      end

c ********************************************************************
      subroutine xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to planetodetic lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= planetodetic latitude in degrees
c outlon= planetodetic longitude in degrees east

      implicit real*8 (a-z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      cosphi=(b*b*a*a*z)/
     +            dsqrt(c**4*b**4*x*x+a**4*c**4*y*y+b**4*a**4*z*z)
      if(cosphi.gt.1.d0) cosphi=1.d0
      if(cosphi.lt.-1.d0) cosphi=-1.d0
      phi=dacos(cosphi)/degtorad
      outlat=90.d0-phi
      coslon=b*b*x/dsqrt(b**4*x*x+y*y*a**4)
      if(coslon.gt.1.d0) coslon=1.d0
      if(coslon.lt.-1.d0) coslon=-1.d0
      outlon=dacos(coslon)/degtorad
      if(y.lt.0.d0)outlon=-outlon
      if(outlon.lt.0.d0) outlon=360.d0+outlon

      return
      end

c ********************************************************************
      subroutine xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to snyder lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= snyder latitude in degrees
c outlon= snyder longitude in degrees east

      implicit real*8 (a-z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      outlat=datan2(z,b*dsqrt(1.d0-(z*z)/(c*c)))/degtorad
      outlon=datan2(y,x)/degtorad
      if(outlon.lt.0.d0) outlon=360.d0+outlon
      return
      end

c ********************************************************************
      subroutine snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from snyder lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetocentric latitude in degrees
c inlon= planetocentric longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      coslat=dcos(inlat*degtorad)
      sinlat=dsin(inlat*degtorad)
      coslon=dcos(inlon*degtorad)
      sinlon=dsin(inlon*degtorad)
      sqrtflon=dsqrt(sinlon**2+(b**2/a**2)*coslon**2)   ! equation 9
      sqrtflat=dsqrt(sinlat**2+(c**2/b**2)*coslat**2)   ! equation 10
      z=c*sinlat/sqrtflat                               ! equation 6
      y=c*coslat*sinlon/(sqrtflon*sqrtflat)             ! equation 11
      x=c*coslat*coslon/(sqrtflon*sqrtflat)             ! equation 8

      return
      end

c ********************************************************************
      subroutine centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from centric lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetocentric latitude in degrees
c inlon= planetocentric longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      lon=inlon
      if(lon.lt.0.d0) lon=360+lon
      tanlat=dtan(inlat*degtorad)
      if(lon.eq.90.d0.or.lon.eq.270.d0)then
        x=0.d0
        y=dsqrt(c*c*b*b/(b*b*tanlat*tanlat+c*c))
        z=tanlat*y
        if(lon.gt.180.d0) y=-y
      else
        tanlon=dtan(inlon*degtorad)
        x=dsqrt(1.d0/(1.d0/a**2 + tanlon**2/b**2 + tanlat**2/c**2 +
     +              tanlon**2*tanlat**2/c**2) )
        if(lon.gt.90.d0.and.lon.lt.270.d0) x=-x
        y=x*tanlon
        z=tanlat*dsqrt(x*x+y*y)      
      endif

      return
      end

c ********************************************************************
      subroutine detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from detic lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetodetic latitude in degrees
c inlon= planetodetic longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      cosang2=(dcos((90.d0-inlat)*degtorad))**2
      coslon2=(dcos(inlon*degtorad))**2
      if(coslon2.ne.1.d0)then                           ! normal
        k1=cosang2*c*c*(1.d0-coslon2/(coslon2-1.d0))
        k2=(1.d0-cosang2)*( (coslon2*a*a)/(coslon2-1.d0)-b*b)
        y2=(1.d0-cosang2)*b**4/(k1-k2)
        if (y2.le.0.d0) then
            y=0.d0     
        else 
            y=dsqrt(y2)
        endif
        x2=(-coslon2*y*y*a**4)/(b**4*(coslon2-1.d0))
      else                                    ! longitude=0 or 180
        y2=0.d0
        y=0.d0
        x2=(a**4) * (1.d0-cosang2)/(cosang2*c*c-cosang2*a*a+a*a)
      endif
      if (x2.le.0.d0) then
           x=0.d0
      else 
           x=dsqrt(x2)
      endif
      x=dsqrt(x2)
      z2=c*c*(1.d0-x2/(a*a)-y2/(b*b))
      if (z2.le.0.d0) then
           z=0.d0
      else 
           z=dsqrt(z2)
      endif
      lon=inlon
      if(lon.lt.0.d0) lon=360+lon
      if(lon.gt.90.d0.and.lon.lt.270.d0) x=-x
      if(lon.gt.180.d0) y=-y
      if(inlat.lt.0.d0)z=-z
      return
      end

c ********************************************************************
      subroutine authtran1(coef,coefp,nlimit,klimit,mlimit,
     +                     lat,lon,alat,alon,ind)

c Routine to convert from Snyder lat/lon to authalic lat/lon.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
c Is called by authtran.

c coef = longitude matrix of coefficients              input      real*8
c coefp= latitude primed array of coefficients        input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c lat= snyder latitude in degrees           input      real*8
c lon= snyder longitude in degrees east     input      real*8
c alat= authalic latitude in degrees        output     real*8
c alon= authalic longitude in degrees east  output     real*8
c ind= return status.  0 is OK, 1 is error condition.  integer

      implicit real*8 (a-h,o-z)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)
      real*8 lat,lon,latitude,longitude

      ind=0
      degtorad=datan(1.d0)/45.d0
      if(coef(1,1).eq.0.d0.or.coefp(1).eq.0.d0)then
         call xvmessage('AUTHTRAN1: COEF or COEFP buffer is empty',' ')
         ind=1
         return
      endif

      if(lat.eq.90.d0.or.lat.eq.-90.d0)then
         clat=lat
         clon=lon
         return
      endif
         
      latitude=lat*degtorad
      longitude=lon*degtorad

c     Compute longitude
      alon=longitude
      do m=0,mlimit
         alonn=0.d0
         do k=0,klimit
            alonn=alonn+coef(m,k)*dcos(2*k*latitude)
         enddo
         alon=alon+alonn*dsin(2*m*longitude)
      enddo
      alon=alon/degtorad

c     Compute latitude
      alat=latitude
      do n=1,nlimit
         alat=alat+coefp(n)*dsin(2*n*latitude)
      enddo
      alat=alat/degtorad      

      return
      end

c ********************************************************************
      subroutine conftran1(c,cp,nlimit,mlimit,lat,lon,clat,clon,ind)

c Routine to convert from Snyder lat/lon to conformal lat/lon.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
c Is called by conftran.

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c lat= snyder latitude in degrees           input      real*8
c lon= snyder longitude in degrees east     input      real*8
c clat= conformal latitude in degrees          output  real*8
c clon= conformal longitude in degrees east    output  real*8
c ind= return status.  0 is OK, 1 is error condition.  integer

      implicit real*8 (a-h,o-z)
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 lat,lon,latitude,longitude

      ind=0
      degtorad=datan(1.d0)/45.d0
      if(c(1,1).eq.0.d0.or.cp(1,1).eq.0.d0)then
         call xvmessage('CONFTRAN1: C or CP buffer is empty',' ')
         ind=1
         return
      endif

      if(lat.eq.90.d0.or.lat.eq.-90.d0)then
         clat=lat
         clon=lon
         return
      endif
         
      latitude=lat*degtorad
      longitude=lon*degtorad

c        Compute latitude
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+cp(n,m+1)*dsin(n*latitude)
            enddo
            t=t+t1*dcos(2*m*longitude)
         enddo
         q=dexp(t)*dtan(45.d0*degtorad+latitude/2.d0)
         clat=2.d0*datan(q)/degtorad - 90.d0

c        Compute longitude
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+c(n+1,m)*dcos(n*latitude)
            enddo
            t=t+t1*dsin(2*m*longitude)
         enddo
         clon=(longitude+t)/degtorad

      return
      end


c ********************************************************************
      subroutine authtran(c,cp,nlimit,klimit,mlimit,slat,slon,
     +                    alat,alon,mode,ind)

c Routine to convert between Snyder lat/lon and authalic lat/lon on
c the triaxial ellipsoid (both directions). 
c Note in that authtran1 only goes from snyder to authalic. To get the
c reverse we fit a polynomial to 3 points near alat & alon and compute
c the reverse from the coefficients.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed array of coefficients       input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c slat= snyder latitude in degrees           input/output real*8
c slon= snyder longitude in degrees east     input/output real*8
c alat= authalic latitude in degrees           input/output real*8
c alon= authalic longitude in degrees east     input/output real*8
c mode=1 for snyder to authalic , mode=2 for authalic to snyder
c        input integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4

      implicit real*8 (a-h,o-z)
      real*8 c(0:mlimit,0:klimit),cp(nlimit)
      real*8 a(3,3),b(3),aa(3,3),bb(3)
      real*8 rlat(3),rlon(3),ala(3),alo(3)

      ind=0
      ipass=0

      if(mode.eq.1)then         ! snyder to authalic

         if(slat.ge.90.d0)then
           alat=90.d0
           alon=slon
           return
         else if(slat.le.-90.d0)then
           alat=-90.d0
           alon=slon
           return
         endif

         call authtran1(c,cp,nlimit,klimit,mlimit,slat,slon,
     +                  alat,alon,ind)

      else if(mode.eq.2)then      ! authalic to snyder

         if(alat.ge.90.d0)then
           slat=90.d0
           slon=alon
           return
         else if(alat.le.-90.d0)then
           slat=-90.d0
           slon=alon
           return
         endif

100      ipass=ipass+1
         if(ipass.eq.1)then
           rlat(1)=alat           ! take authalic as snyder input
           rlon(1)=alon
           delta=.1               ! increment in degrees
         else
           rlat(1)=slat           ! take first iterated snyder as input
           rlon(1)=slon
           delta=.001             ! increment in degrees
         endif

         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(1),rlon(1),
     +                  ala(1),alo(1),ind)
         rlat(2)=rlat(1)-sign(delta,rlat(1))
         rlon(2)=rlon(1)
         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(2),rlon(2),
     +                  ala(2),alo(2),ind)
         rlat(3)=rlat(1)
         rlon(3)=rlon(1)+delta
         if(rlon(3).gt.360.d0) rlon(3)=rlon(1)-delta
         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(3),rlon(3),
     +                  ala(3),alo(3),ind)
         do i=1,3
            a(i,1)=alo(i)
            a(i,2)=ala(i)
            a(i,3)=1.d0
            b(i)=rlat(i)
            aa(i,1)=a(i,1)
            aa(i,2)=a(i,2)
            aa(i,3)=a(i,3)
         enddo
         call dsimq_solution(a,b,3,ind)      ! solve latitude eqn
         if(ind.ne.0) then
           call xvmessage('AUTHTRAN: singular solution on inverse',' ')
           return
         endif
         do i=1,3
            bb(i)=rlon(i)
         enddo
         call dsimq_solution(aa,bb,3,ind)      ! solve longitude eqn
         if(ind.ne.0) then
           call xvmessage('AUTHTRAN: singular solution on inverse',' ')
           return
         endif
         slat=b(1)*alon+b(2)*alat+b(3)
         slon=bb(1)*alon+bb(2)*alat+bb(3)

         if(ipass.eq.1) goto 100

      else
         call xvmessage('AUTHTRAN: Invalid argument for mode',' ')
         ind=1
         return
      endif

      return
      end

c ********************************************************************
      subroutine conftran(c,cp,nlimit,mlimit,slat,slon,clat,clon,mode,
     +     ind)

c Routine to convert between Snyder lat/lon and conformal lat/lon on
c the triaxial ellipsoid (both directions). 
c Note in that conftran1 only goes from snyder to conformal. To get the
c reverse we fit a polynomial to 3 points near clat & clon and compute
c the reverse from the coefficients.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c slat= snyder latitude in degrees           input/output real*8
c slon= snyder longitude in degrees east     input/output real*8
c clat= conformal latitude in degrees           input/output real*8
c clon= conformal longitude in degrees east     input/output real*8
c mode=1 for snyder to conformal, mode=2 for conformal to snyder
c        input integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4

      implicit real*8 (a-h,o-z)
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 a(3,3),b(3),aa(3,3),bb(3)
      real*8 rlat(3),rlon(3),cla(3),clo(3)

      ind=0
      ipass=0

      if(mode.eq.1)then         ! snyder to conformal

         if(slat.ge.90.d0)then
           clat=90.d0
           clon=slon
           return
         else if(slat.le.-90.d0)then
           clat=-90.d0
           clon=slon
           return
         endif

         call conftran1(c,cp,nlimit,mlimit,slat,slon,clat,clon,ind)

      else if(mode.eq.2)then      ! conformal to snyder

         if(clat.ge.90.d0)then
           slat=90.d0
           slon=clon
           return
         else if(clat.le.-90.d0)then
           slat=-90.d0
           slon=clon
           return
         endif

100      ipass=ipass+1
         if(ipass.eq.1)then
           rlat(1)=clat           ! take authalic as snyder input
           rlon(1)=clon
           delta=.1               ! increment in degrees
         else
           rlat(1)=slat           ! take first iterated snyder as input
           rlon(1)=slon
           delta=.001             ! increment in degrees
         endif

         call conftran1(c,cp,nlimit,mlimit,rlat(1),rlon(1),
     +                  cla(1),clo(1),ind)
         rlat(2)=rlat(1)-sign(delta,rlat(1))
         rlon(2)=rlon(1)
         call conftran1(c,cp,nlimit,mlimit,rlat(2),rlon(2),
     +                  cla(2),clo(2),ind)
         rlat(3)=rlat(1)
         rlon(3)=rlon(1)+delta
         if(rlon(3).gt.360.d0) rlon(3)=rlon(1)-delta
         call conftran1(c,cp,nlimit,mlimit,rlat(3),rlon(3),
     +                  cla(3),clo(3),ind)
         do i=1,3
            a(i,1)=clo(i)
            a(i,2)=cla(i)
            a(i,3)=1.d0
            b(i)=rlat(i)
            aa(i,1)=a(i,1)
            aa(i,2)=a(i,2)
            aa(i,3)=a(i,3)
         enddo
         call dsimq_solution(a,b,3,ind)      ! solve latitude eqn
         if(ind.ne.0) then
           call xvmessage('CONFTRAN: singular solution on inverse',' ')
           return
         endif
         do i=1,3
            bb(i)=rlon(i)
         enddo
         call dsimq_solution(aa,bb,3,ind)      ! solve longitude eqn
         if(ind.ne.0) then
           call xvmessage('CONFTRAN: singular solution on inverse',' ')
           return
         endif
         slat=b(1)*clon+b(2)*clat+b(3)
         slon=bb(1)*clon+bb(2)*clat+bb(3)

         if(ipass.eq.1) goto 100

      else
         call xvmessage('CONFTRAN: Invalid argument for mode',' ')
         ind=1
         return
      endif

      return
      end

C*********************************************************************
      SUBROUTINE DSIMQ_SOLUTION(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ_SOLUTION(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

