c
c subroutine triaxcoef
c
c Function extracted from program TRICOEF.COM by Jean Lorre.
c
c	Date extracted:		October 1993
c	Extracted by:		Justin McNeill
c
c This has been modified to now include both authalic as well as conformal
c auxiallary coefficient functionality. Code was adopted from Jean Lorre's
c tricoef routine. Recent changes also made in the randum number generator
c function, to 'rangen', to correct for errors occurring on the sgi system
c with the older 'ran1' function   -  pxa  august '96

c oct96 -lwk- hard-coded the random-number seed in order to allow comparison
c		of test logs;  this will be replaced in the future by 
c		allowing the user to specify this (as well as the 3 limits),
c		which will require a new routine to give the user access

c jun98 -lwk- replaced rangen with modified version of RAN1 because of 
c		problems on DEC-Unix 
c aug98 -lwk- made all xvmessage calls dependent on flag.eq.1 (except for
c		one initial one)

      subroutine triaxcoef( radii,cc,cp,ac,ap,nlimit,mlimit,klimit,
     +	flag,status)
      implicit real*8 (a-h,o-z)
      real*8 cc(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 ac(0:mlimit,0:klimit),ap(nlimit)
      parameter (maxpts=800) ! Changes must be made in other places too !
      parameter (maxeqs=400)
      parameter (latpts=72,lonpts=72)
      character*80 msg
      character*12 planet
      integer*4 flag
      integer*4 count,status
      real*4 rannum, ran1
      real*8 radii(3),radsph
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      real*8 amatrix(maxeqs*maxeqs),bvector(maxeqs)
      real*8 authcoef1(maxpts),authcoef2(maxpts)

      common/c1/a,b,c,lat,lon,costheta,sintheta,f

c initialize spice for planet radii retrieval.
c      call init_spice
c
c parameters
      planet='            '
      a=0.d0
c     call xvparmd('RADIUS',radii,count,def,3)

      count = 3

      if(count.gt.0)then
        a=radii(1)
        b=radii(2)
        c=radii(3)
      endif
c      call xvparm('PLANET',planet,count,def,1)
      
      count = 0 

c      if(count.eq.1)then
c      call ccase(planet,1)
c        call getplacon(planet,id,planet_data,ind)
c        if(ind.ne.0)then
c           call xvmessage('GETPLACON: unrecognizable planet',' ')
c           call xvmessage('Input name is:'//planet,' ')
c           if(a.eq.0.d0) call abend
c        endif
c        call xvmessage('Name will be placed in coefficnt file',' ')
c        a=planet_data(1)
c        b=planet_data(2)
c        c=planet_data(3)
c      endif
c
c      call xvparmd('RADIUS',radii,count,def,3)
c
c      if(count.gt.0)then
c       a=radii(1)
c       b=radii(2)
c       c=radii(3)
c     endif
c      call xvparm('MLIMIT',mlimit,count,def,1)
c      call xvparm('NLIMIT',nlimit,count,def,1)

      if (flag.eq.1) then
        write(msg,108)a,b,c
108     format('Planet radii a,b,c:',3f10.2)
c        call xvmessage(msg,' ')
      else
c	call xvmessage(' Computing triaxial coefficients ...',' ')
      endif

      if(a.eq.0.d0)then
         call xvmessage('Planet radii unspecified',' ')
         status = -401
         return
c         call abend
      endif
      if((c.ge.b).or.(b.ge.a))then
         call xvmessage('Radii out of order, should be decreasing.',' ')
         call xvmessage('Oblate spheroids not supported.',' ')
         status = -402
         return
c         call abend
      endif
      c=c/a
      b=b/a
      a=1.d0

      nequations=nlimit*mlimit*2
      if(nequations.gt.maxeqs)then
         call xvmessage('Too many equations requested',' ')
         write(msg,100) maxeqs
100      format(' Maximum number of equations = ',i5)
         call xvmessage(msg,' ')
         status = -403
         return
c         call abend
      else
	 if (flag.eq.1) then
          write(msg,110) nequations
110       format(i4,' simultaneous equations.')
          call xvmessage(msg,' ')
         endif
      endif

      npoints=nequations*2           ! npoints > nequations/2 (Snyder).
      if(npoints.gt.maxpts)then
         npoints=maxpts
	 if (flag.eq.1) then
          call xvmessage('Warning: #points must be > #equations/2',' ')
          write(msg,102) nequations
102       format(' Number of equations = ',i5)
          call xvmessage(msg,' ')
         endif
      endif
      if (flag.eq.1) then
       write(msg,111) npoints
111    format(i4,' random points selected to fit equations.')
       call xvmessage(msg,' ')
      endif

c compute the fit points from a random grid in the range lat=0 to 85
c long=0 to 90.
      idum=-768825576		! establish a seed for random numbers
c      call get_seconds(idum)
c      if (flag.eq.1) then
c      write(msg,112) idum
c112    format(' The random number seed = ',i10)
c       call xvmessage(msg,' ')
c      endif
      degtorad=datan(1.0d0)/45.d0
      do i=1,npoints
	rannum = ran1(idum)
	lat(i)=degtorad*85.*rannum
	rannum = ran1(idum)
	lon(i)=degtorad*90.*rannum
      enddo

      if (flag.eq.1) then
        call xvmessage('Printing out random Lat/Lon values ...',' ')
        do i=1,npoints
          write(msg,101) lat(i), lon(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage(' ',' ')
        call xvmessage('Printing out intermediate values ...',' ')
      endif

c compute & save values unique to each point.
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
         if (flag.eq.1) then
           write(msg,105) costheta(i), sintheta(i), f(i)
           call xvmessage(msg,' ')
         endif
      enddo

c load matrices to do the linear fit.                              ! eqn 80
      ip=1
      iq=0
      do i=1,mlimit*nlimit       ! first 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+p1(m,n,ii)*p1(ip,iq,ii)+p3(m,n,ii)*p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+p2(m,n,ii)*p1(ip,iq,ii)+p4(m,n,ii)*p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*p1(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*p3(ip,iq,ii)
         enddo
         bvector(i)=t
         if(iq.eq.nlimit-1)then
            iq=0
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

c      ip=1
c      iq=0
      ip=0
      iq=1
      do i=mlimit*nlimit+1,nequations       ! second 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+p1(m,n,ii)*p2(ip,iq,ii)+p3(m,n,ii)*p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+p2(m,n,ii)*p2(ip,iq,ii)+p4(m,n,ii)*p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
c            if(i.eq.5)write(*,*)ip,iq,p2(ip,iq,ii),p4(ip,iq,ii)
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*p2(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*p4(ip,iq,ii)
         enddo
         bvector(i)=t
c         if(iq.eq.nlimit-1)then
c            iq=0
         if(iq.eq.nlimit)then
            iq=1
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

      if (flag.eq.1) then

c print the a-matrix
        call xvmessage('A-matrix terms by row..',' ')
        do i=1,nequations*nequations,nequations
          do j=i,i+nequations-1,6
            m=j+5
            if(m.gt.i+nequations-1) m=i+nequations-1
            write(msg,104)(amatrix(k),k=j,m)
104         format(6f13.0)
            call xvmessage(msg,' ')
          enddo
        enddo

c print the b-vector

        call xvmessage('B-vector terms top down..',' ')
        do j=1,nequations,8
          m=j+7
          if(m.gt.nequations)m=nequations
          write(msg,101)(bvector(k),k=j,m)
101       format(8f10.4)
          call xvmessage(msg,' ')
        enddo

      endif

C       k=1
C       do i=1,nequations
C          check(i)=amatrix(k)
C          k=k+nequations
C       enddo
C       result=bvector(1)

c perform the solution.
      if (flag.eq.1) then
       call xvmessage('Beginning matrix solution.',' ')
      endif
      call mp_dsimq(amatrix,bvector,nequations,ks)
      if(ks.ne.0)then
         call xvmessage('Ill conditioned equations, no solution.',' ')
	 status = -1
         return
      endif

c      r=0.d0
c      do i=1,nequations
c         r=r+bvector(i)*check(i)
c      enddo
c      write(*,*)'should,actual ',result,r

c print terms
      if (flag.eq.1) then
        call xvmessage('C(m,n) coefficients. Rows m=1,2,3,...',' ')
        call xvmessage('Columns begin at n=0,1,2,3...',' ')
      endif
      m=0
      do i=1,mlimit*nlimit,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
	    if (flag.eq.1) then
              write(msg,105)(bvector(k),k=j,l)
105           format(5f16.11)
              call xvmessage(msg,' ')
	    endif
         enddo
      enddo
      if (flag.eq.1) then
         call xvmessage('C(m,n)primed coefficients. Rows m=0,1,2,...',
     &    ' ')
         call xvmessage('Columns begin at n=1,2,3...',' ')
      endif
      m=-1
      do i=mlimit*nlimit+1,nequations,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
	    if (flag.eq.1) then
              write(msg,105)(bvector(k),k=j,l)
              call xvmessage(msg,' ')
	    endif
         enddo
      enddo

c Recompute NPOINTS random points for RMS error computation.
c The same points won't do since we've fitted to them.

      do i=1,npoints
	rannum = ran1(idum)
	lat(i)=degtorad*85.*rannum
	rannum = ran1(idum)
	lon(i)=degtorad*90.*rannum
      enddo
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
      enddo

c compute rms error for the coefficient set.
      if (flag.eq.1) then
        call xvmessage('    ',' ')
        call xvmessage('The RMS error is the constraint violation',' ')
        call xvmessage('ie: angle and scale or equal area    ',' ')
      endif
      call rmserror(bvector,bvector(mlimit*nlimit+1),
     +              nlimit,mlimit,npoints,rms)
      if (flag.eq.1) then
        write(msg,106)rms
106     format(' Rms error using these coefficients is ',f15.11)
        call xvmessage(msg,' ')
      endif
c
c abandon coefficients
c      if(planet.eq.'            ') then
c        call xvmessage('Coefficients discarded',' ')
c        return
c     endif
c
c save coefficients
     
       k=0
       m=0
       if (flag.eq.1) then
	call xvmessage('These are the loaded cc,cp arrays...',' ')
       endif
       do i=1,nlimit
	 k=m*nlimit
         do j=1,mlimit
		cc(i,j)=bvector(j+k)
	        cp(i,j)=bvector(nlimit*mlimit+j+k)
c		type *,i,j,cc(i,j),cp(i,j)
	        if (flag.eq.1) then
		  write(msg,107) i,j,cc(i,j),cp(i,j)
107		  format(i4,i4,f16.11,f16.11)
		  call xvmessage(msg,' ')
		endif
         enddo
	 m=m+1
       enddo
c      endif

c *******************************************************
c compute authalic coefficients : authcoef1 & 2
c *******************************************************
      call authalic_coef(a,b,c,mlimit,klimit,nlimit,
     + 	amatrix(1),
     +  amatrix((latpts+1)*(lonpts+1)+1),
     +  amatrix((latpts+1)*(lonpts+1)*2+1),
     +  authcoef1,authcoef2,radsph)
c	print terms
      if (flag.eq.1) then
	call xvmessage('Authalic Case:',' ')
	call xvmessage('COEF(m,k) coefficients. Rows m=0,1,2...',' ')
	call xvmessage('Columns begin at k=0,1,2,....',' ')
	do k=1,(klimit+1)*(mlimit+1),mlimit+1
	  do i=k,k+mlimit,5
	    ii=i+4
	    if(ii.gt.k+mlimit) ii=k+mlimit
		write (msg,105) (authcoef1(j),j=i,ii)
		call xvmessage(msg,' ')
	   enddo
	enddo
	call xvmessage('COEFP(n) coefficient array. n=0,1,2,....',' ')
	do i=1,nlimit,5
	   ii=i+4
	   if(ii.gt.nlimit) ii=nlimit
	   write (msg,105) (authcoef2(j),j=i,ii)
	   call xvmessage(msg,' ')
	enddo
	write(msg,117)radsph
117	format('Radius of equivalent area sphere is ',f16.8)
	call xvmessage(msg,' ')
      endif
      
c compute rms error for the coefficient set
      if (flag.eq.1) then
        call xvmessage('     ',' ')
        call xvmessage('The RMS error is the constraint violation',' ')
        call authalic_rmserror(authcoef1,authcoef2,nlimit,klimit,mlimit,
     +		radsph,degtorad,a,b,c,rms)
        write(msg,116)rms
116     format('Authalic rms error using these coefficients is ',f15.11)
        call xvmessage(msg,' ')
      endif

c save coefficients
      k=0
      if (flag.eq.1) call xvmessage('Loaded ac array...',' ')
      do i=0,mlimit,1
	do j=0,klimit,1
	  ac(i,j)=authcoef1(k+1)
	  if (flag.eq.1) then
	    write(msg,135) i,j,ac(i,j)
135	    format(i4,i4,f16.11)
	    call xvmessage(msg,' ')
	  endif
	  k=k+1
	enddo
      enddo
      if (flag.eq.1) call xvmessage('Loaded ap array...',' ')
      do i=1,nlimit,1
	ap(i)=authcoef2(i)
	if (flag.eq.1) then
		write(msg,136) i,ap(i)
136		format(i4,f16.11)
		call xvmessage(msg,' ')
	endif
      enddo
	
      status=0

      return 
      end

c ********************************************************************
      subroutine rmserror(c,cp,nlimit,mlimit,npoints,rms)

c Routine to compute solution rms error equation #73.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c rms= rms error                            returned   real*8

      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,cc,lat,lon,costheta,sintheta,f

      rms=0.d0
      do i=1,npoints ! loop over random points used for fitting

c        Compute equation 67
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+n*c(n+1,m)*dsin(n*lat(i))
            enddo
            t=t+t1*dsin(2*m*lon(i))
         enddo
         eq67=a*(-t)

c        Compute equation 68
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+c(n+1,m)*dcos(n*lat(i))
            enddo
            t=t+m*t1*dcos(2*m*lon(i))
         enddo
         eq68=a*(1+2*t)

c        Compute equation 69
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+n*cp(n,m+1)*dcos(n*lat(i))
            enddo
            t=t+t1*dcos(2*m*lon(i))
         enddo
         eq69=a*(1.d0/dcos(lat(i))+t)

c        Compute equation 70
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+cp(n,m+1)*dsin(n*lat(i))
            enddo
            t=t+m*t1*dsin(2*m*lon(i))
         enddo
         eq70=a*(-2.d0*t)

         e1=eq68*sintheta(i)+eq70*costheta(i)-eq69*f(i)        !eqn 71
         e2=eq69*f(i)*sintheta(i)+eq67*f(i)*costheta(i)-eq68   !eqn 72
         rms=rms+(e1*e1+e2*e2)                                 !eqn 73
         
      enddo
      rms=dsqrt(rms/npoints)           ! equation 81

      return
      end


c*********************************************************************
c functions used often
      real*8 function p1(m,n,i)      ! equation 76
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p1=2.d0*a*m*sintheta(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      real*8 function p2(m,n,i)      ! equation 77
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p2=-2.d0*a*m*costheta(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))-
     +   a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      real*8 function p3(m,n,i)      ! equation 78
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p3=-2.d0*a*m*dcos(n*lat(i))*dcos(2.d0*m*lon(i))-
     +   a*n*f(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))*costheta(i)
      return
      end

      real*8 function p4(m,n,i)      ! equation 79
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p4=a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))*sintheta(i)
      return
      end

c*********************************************************************
      REAL*4 FUNCTION RAN1(IDUM)
c     Returns random number between 0.0 and 1.0. To initialize provide
c     negative idum value.
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773)
      PARAMETER (M2=134456,IA2=8121,IC2=28411)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      PARAMETER (RM1=1./M1,RM2=1./M2)
      DATA IFF /0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97) j=97
      IF(J.LT.1) j=1
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END

C*********************************************************************
      SUBROUTINE MP_DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL MP_DSIMQ(A,B,N,KS)
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
      real*8       A(1),B(1),biga,save,tol
      character*80 msg
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
      write( msg, 36 ) BIGA
   36 format( 'BIGA ',D12.6 )
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

c ***********************************************************************
      subroutine authalic_coef(a,b,c,mlimit,klimit,nlimit,
     +        areafun,autlon,acoef,coef,coefp,radsph)
 
c To compute coefficients permitting the snyder to authalic computation.
c a= major axis normalized to 1              real*8  input
c b= middle axis normalized                  real*8 input
c c= minor axis normalized                   real*8 input
c mlimit= m maximum                          integer*4 input
c klimit= k maximum                          integer*4 input
c nlimit= n maximum                          integer*4 input
c arefun= scratch space for buffer areafun   real*8    input
c autlon= scratch space for buffer autlon    real*8    input
c acoef= scratch space for buffer acoef      real*8    input
c coef= longitude computation coefficients   real*8  returned
c coefp= latitude computation coefficients   real*8 returned
c radsph= radius of equivalent sphere        real*8 returned
 
      implicit real*8 (a-h,o-z)
      parameter (latpts=72,lonpts=72)
      real*8 elllat(0:latpts),elllon(0:lonpts)
      real*8 areafun(0:latpts,0:lonpts)
      real*8 acoef(0:latpts,0:mlimit),autlat(0:latpts)
      real*8 autlon(0:latpts,0:lonpts)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)

      do i=0,latpts
	do j=0,mlimit
	   acoef(i,j)=0.0
	enddo
      enddo
      do i=0,latpts
	do j=0,lonpts
	   autlon(i,j)=0.0
	enddo
      enddo
      do i=0,mlimit
	do j=0,klimit
	   coef(i,j)=0.0
	enddo
      enddo
      do i=0,latpts
	autlat(i)=0.0
      enddo
      do i=1,nlimit
	coefp(i)=0.0
      enddo
 
      dg1=datan(1.d0)/45.d0
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0
      cb2=c*c/(b*b)
      ba2=b*b
 
      do i=0,latpts-1
        elllat(i)=i*dg1*90.d0/latpts
      enddo
      elllat(latpts)=(latpts-.001)*dg1*90.d0/latpts
      do j=0,lonpts
        elllon(j)=j*dg1*90.d0/lonpts
      enddo
 
c compute area function for integration
      do i=0,latpts
        do j=0,lonpts
          slon=dsin(elllon(j))
          clon=dcos(elllon(j))
          slat=dsin(elllat(i))
          clat=dcos(elllat(i))
          flon=slon*slon+ba2*clon*clon       ! eqn 9
          flat=slat*slat+cb2*clat*clat       ! eqn 10
          t1=(1.d0-ba2)*slon*clon*slat
          t2=dsqrt(slat*slat+cb2*cb2*flon*clat*clat)
          t3=dsqrt(slon*slon+ba2*ba2*clon*clon)
	  costheta=t1/(t2*t3)                ! eqn 48
          sintheta=dsqrt(dabs(1.d0-costheta*costheta))
          areafun(i,j)=clat*t3*t2*sintheta/(flon*flon*flat*flat)
        enddo
      enddo
 
c compute total area of triaxial ellipsoid
      latlmt=latpts
      call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +           elllat,areafun,pih,area)
      tarea=area
 
c compute authalic latitudes
      latlmt=0
      autlat(0)=0.d0
      do latlmt=6,latpts,6
        call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +             elllat,areafun,pih,area)
        sphiaut=area/tarea
        if(dabs(sphiaut).gt. 0.9999999)then
          autlat(latlmt)=pih
        else
          autlat(latlmt)=
     +        datan(sphiaut/dsqrt(dabs(1.d0-sphiaut*sphiaut)))
        endif
      enddo
 
c compute coefficients for authalic latitude using simpson integration
      do n=1,nlimit
        sum1=0.d0
        do i=6,latpts-6,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+4.d0*autfunc
        enddo
        do i=12,latpts-12,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+2.d0*autfunc
        enddo
        coefp(n)=4.d0*sum1/latpts
      enddo
      radsph=a*dsqrt(8.d0*tarea/(4.d0*pi)) !radius of equivalent sphere
 
c compute authalic longitude
      do i=0,latpts,6
c       first compute area of zone of quadrant along latitude
        lonlmt=lonpts
        call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
        tcirc=fsimlon
c       now compute portion of zone for each step of longitude
        lonlmt=0
        autlon(i,0)=0.d0
        do lonlmt=6,lonpts,6
          call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
          autlon(i,lonlmt)=(fsimlon/tcirc)*pih
        enddo
      enddo
 
c compute elements for simpson integration for acoef of authalic longitude
      do m=0,mlimit
        do i=0,latpts,6
          sum1=0.d0
          do j=6,lonpts-6,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,lonpts-12,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+2.d0*autfunc
          enddo
          acoef(i,m)=4.d0*sum1/lonpts
        enddo
      enddo
 
c compute elements for simpson integration for C coef of authalic longitude
      do m=0,mlimit
        k=0
        sum1=acoef(0,m)
        do j=6,latpts-6,12
          sum1=sum1+4.d0*acoef(j,m)
        enddo
        do j=12,latpts-12,12
          sum1=sum1+2.d0*acoef(j,m)
        enddo
        sum1=sum1+acoef(latpts,m)
        coef(m,0)=2.d0*sum1/latpts
c	write(msg,800)(coef(m,0))
c 800	format(5f16.11)
      enddo
 
      do m=1,mlimit
        do k=1,klimit
          sum1=acoef(0,m)
          do j=6,latpts-6,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,latpts-12,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+2.d0*autfunc
          enddo
          sum1=sum1+acoef(latpts,m)*dcos(2*k*elllat(latpts))
          coef(m,k)=4.d0*sum1/latpts
c	  write(msg,801)(coef(m,k))
c 801	  format(5f16.11)
        enddo
      enddo
 
      return
      end
 
 
c *********************************************************************
      subroutine simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
 
c Compute elements for Simpsons integration.
c fsimlon is returned
      implicit real*8 (a-h,o-z)
      real*8 areafun(0:latpts,0:lonpts)
 
      sum1=areafun(i,0)+areafun(i,lonlmt)
      do j=1,lonlmt-1,2
         sum1=sum1+4.d0*areafun(i,j)
      enddo
      do j=2,lonlmt-2,2
         sum1=sum1+2.d0*areafun(i,j)
      enddo
      fsimlon=sum1*pih/(lonpts*3)
      return
      end
 
c ******************************************************************
      subroutine simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +                  elllat,areafun,pih,area)
 
c Computes areas of zones
c lonlmt & area are returned
      implicit real*8 (a-h,o-z)
 
      real*8 elllat(0:latpts)
      real*8 areafun(0:latpts,0:lonpts)
 
      i=0
      lonlmt=lonpts
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=fsimlon
      do i=1,latlmt-1,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+4.d0*fsimlon
      enddo
      do i=2,latlmt-2,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+2.d0*fsimlon
      enddo
      i=latlmt
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=sum2+fsimlon
 
      area=c*c*sum2*elllat(latlmt)/(latlmt*3)
      return
      end
          
c********************************************************************
      subroutine authalic_rmserror(coef,coefp,nlimit,klimit,mlimit,
     +     radsph,degtorad,a,b,c,rmse)
 
c Routine to compute authalic solution rms error equation.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
 
c COEF =  matrix of coefficients              input      real*8
c COEFP = primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c radsph= radius of equivalent sphere       input      real*8
c degtorad=degrees/radian                   input      real*8
c a= major ellipsoid radius                 input      real*8
c b= middle ellipsoid radius                 input      real*8
c c= small ellipsoid radius                 input      real*8
c rms= rms error                            returned   real*8
 
      implicit real*8 (a-h,o-z)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)
 
      bige=0.d0
      lptcount=0
      cb2=c*c/(b*b)
      ba2=b*b
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0
 
      do i=0,90,5
        do j=90,0,-5
          elat=j
          elon=i
          call authtran(coef,coefp,nlimit,klimit,mlimit,
     +          elat,elon,alat,alon,1,ind)
          elat=elat*degtorad
          elon=elon*degtorad
          alat=alat*degtorad
          alon=alon*degtorad
          sp=dsin(elat)
          cp=dcos(elat)
          sl=dsin(elon)
          cl=dcos(elon)
          f1p=sp*sp+cb2*cp*cp
          f1l=sl*sl+ba2*cl*cl
          f2p=dsqrt(sp*sp+f1l*cb2*cb2*cp*cp)
          f2l=dsqrt(sl*sl+ba2*ba2*cl*cl)
          dxdp=0.d0
          dydp=1.d0
          dxdl=1.d0
          dydl=0.d0
          do m=1,mlimit
            dxdll=0.d0
            dxdpl=0.d0
            do k=0,klimit
              dxdll=dxdll+coef(m,k)*dcos(2*k*elat)
              dxdpl=dxdpl+2*k*coef(m,k)*dsin(2*k*elat)
            enddo
            dxdl=dxdl+dxdll*2.d0*m*dcos(2*m*elon)
            dxdp=dxdp-dxdpl*dsin(2*m*elon)
          enddo
          do n=1,nlimit
            dydp=dydp+n*2*coefp(n)*dcos(2*n*elat)
          enddo
          dxdl=dxdl*radsph/a
          dxdp=dxdp*radsph/a
          dydp=(dydp*radsph/a)*dcos(alat)
          dsdp=c*f2p/(dsqrt(f1l)*f1p*dsqrt(f1p))
          dsdl=c*cp*f2l/(dsqrt(f1p)*f1l*dsqrt(f1l))
          sinth=dydp/dsqrt(dxdp*dxdp+dydp*dydp)
          sh=dsqrt(dxdp*dxdp+dydp*dydp)/dsdp  ! scale factr on meridian
          sk=dxdl/dsdl                        ! scale factr on parallel
          th=(pih-datan(dxdp/dydp))/degtorad
          arerr=sh*sk*sinth-1.d0
          if(j.lt.89) then
            bige=bige+arerr*arerr
            lptcount=lptcount+1
          endif
        enddo    ! j
      enddo      ! i
      rmse=dsqrt(bige/lptcount)
 
      return
      end
 
c ***************************************************************
