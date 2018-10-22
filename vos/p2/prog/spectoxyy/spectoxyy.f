c
c program spectoxyy
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer maxin,mapxy,nlamda
      parameter (mapxy=256,maxin=10,nlamda=41)

c maxin=     the largest number of input images
c mapxy=     the nl and ns of the 4th output chromaticity map.
c nlamda=    the number of wavelengths in the color matching functions

      character*10 modes,illumin
      integer*2 xybuf(mapxy,mapxy)
      integer*4 ounit(3),xyunit,def,index(maxin)
      integer*4 unit(maxin),status,nli(maxin),nsi(maxin)
      real*4 conv(maxin),lamda(maxin),buf(40000,maxin)
      real*4 cmfx(nlamda),cmfy(nlamda),cmfz(nlamda)
      real*4 wavelength(nlamda),illuminant(nlamda)
      real*4 sun_space(nlamda),D65(nlamda),save(maxin)
      real*4 spectrum(maxin),sec_deriv(maxin)

c wavelengths for the following tables in nm
      DATA wavelength/380.,390.,400.,410.,420.,430.,440.,450.,
     1 460.,470.,480.,490.,500.,510.,520.,530.,540.,550.,560.,
     2 570.,580.,590.,600.,610.,620.,630.,640.,650.,660.,670.,
     3 680.,690.,700.,710.,720.,730.,740.,750.,760.,770.,780./

c x color matching function from 380 to 780 nm
      DATA cmfx/.0014,.0042,.0143,.0435,.1344,.2839,.3483,.3362,
     1 .2908,.1954,.0956,.032,.0049,.0093,.0633,.1655,.2904,.4334,
     2 .5945,.7621,.9163,1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,
     3 .1649,.0874,.0468,.0227,.0114,.0058,.0029,.0014,.0007,.0003,
     4 .0002,.0001,0.0/

c y color matching function from 380 to 780 nm
      DATA cmfy/0.,.0001,.0004,.0012,.004,.0116,.023,.038,
     1 .06,.091,.139,.208,.323,.503,.71,.862,.954,.995,.995,.952,
     3 .87,.757,.631,.503,.381,.265,.175,.107,.061,.032,.017,.0082,
     5 .0041,.0021,.001,.0005,.0002,.0001,.0001,0.,0./

c z color matching function from 380 to 780 nm
      DATA cmfz/.0065,.0201,.0679,.2074,.6456,1.3856,1.7471,
     1 1.7721,1.6692,1.2876,.813,.4652,.272,.1582,.0782,.0422,.0203,
     2 .0087,.0039,.0021,.0017,.0011,.0008,.0003,.0002,16*0./

c D65 illuminant (normalized to 100 at 560 nm) 
c Earth average daylight at typical phase including scattering
      DATA D65/50.,54.6,82.8,91.5,93.4,86.7,104.9,117.,117.8,114.9,
     1 115.9,108.8,109.4,107.8,104.8,107.7,104.4,104.,100.,96.3,
     2 95.8,88.7,90.,89.6,87.7,83.3,83.7,80.,80.2,82.3,78.3,69.7,
     3 71.6,74.3,61.6,69.9,75.1,63.6,46.4,66.8,63.4/

c solar spectrum above Earth's atmosphere in watts/(meter**2,micron)
c spectral irradiance units. We'll divide by 2pi later,
c and correct for range to the sun.
      DATA sun_space/1120.,1098.,1429.,1751.,1747.,1639.,1810.,
     1 2006.,2066.,2033.,2074.,1950.,1942.,1882.,1833.,1842.,
     2 1783.,1725.,1695.,1712.,1715.,1700.,1666.,1635.,1602.,
     3 1570.,1544.,1511.,1486.,1456.,1427.,1402.,1369.,1344.,
     4 1314.,1290.,1260.,1235.,1211.,1185.,1159./

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.lt.3)then
         call mabend('Require at least 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.lt.3)then
         call mabend('Require 3 outputs')
      endif

c defaults
      do i=1,nin
         conv(i)=1.0
      enddo

c adjust solar spectrum for range to target
c convert to per steradian
      call xvparm('RANGE',range,ncount,def,1)
      do i=1,nlamda
        sun_space(i)=sun_space(i)/(6.28318*range**2)
      enddo

c adjust D65 to have same amplitude as the sun at 780 nm 
      scale=sun_space(41)/D65(41)
      do i=1,nlamda
        D65(i)=D65(i)*scale
      enddo

c parameters
      call xvparm('CONVERT',conv,ncount,def,maxin)
      if(ncount.ne.nin)then
         call mabend('Require as many CONVs as input images')
      endif
      call xvparm('LAMDA',lamda,ncount,def,maxin)
      if(ncount.ne.nin)then
         call mabend('Require as many LAMDAs as input images')
      endif
      call xvparm('ILLUMIN',illumin,ncount,def,1)
      if(illumin.eq.'D65')call mve(7,nlamda,D65,illuminant,1,1)
      if(illumin.eq.'SUN')call mve(7,nlamda,sun_space,illuminant,1,1)
      call xvparm('MODE',modes,ncount,def,1)
      if(modes.eq.'RADIANCE') mode=1
      if(modes.eq.'REFLECT') mode=2
      call xvparm('SOURCE',illuminant,ncount,def,nlamda)
      if((ncount.gt.0).and.(ncount.ne.nlamda))then
         call mabend('SOURCE requires 41 spectral values')
      endif

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nli(i),'NS',nsi(i),' ')
      enddo

c determine output image size
      ns=nsi(1)
      nl=nli(1)
      do i=2,nin
         nl=min(nl,nli(i))
         ns=min(ns,nsi(i))
      enddo
      if(ns.gt.40000)call mabend('Line length > 40000')

c open outputs
c     xyY images
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c     Histogram
      if(nout.eq.4)then
        call xvunit(xyunit,'OUT',4,status,' ')
        call xvopen(xyunit,status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',mapxy,'U_NS',mapxy,'OP','WRITE',' ')
        xy_slope=mapxy/1.2
        xy_offset=mapxy- xy_slope*1.1
        call mve(2,mapxy*mapxy,0,xybuf,0,1)  ! set histogram buffer to 0
      endif

c sort the wavelengths into ascending order for spline.
c index points to the inputs in ascending order
      call mve(7,nin,lamda,save,1,1)        
      do j=1,nin
        x=1.0e+20
        do i=1,nin
          if(lamda(i).lt.x)then
            x=lamda(i)
            k=i
          endif
        enddo
        index(j)=k
        lamda(k)=2.0e+20
      enddo
c     Sort wavelengths
      do j=1,nin
        lamda(j)=save(index(j))
      enddo
c     Sort conv values
      call mve(7,nin,conv,save,1,1)        
      do j=1,nin
        conv(j)=save(index(j))
      enddo

c compute normalizing factor
      tristim_Y=0.0
      do l=1,nlamda
         tristim_Y=tristim_Y+illuminant(l)*cmfy(l)
      enddo
      factor=100./tristim_Y


c process the image**************************************************

      do j=1,nl                         ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(index(k)),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns                       ! pixel loop

c         Compute spline coefficients for spectrum modelling
          do l=1,nin
            spectrum(l)=buf(i,l)*conv(l)
            if(buf(i,l).eq.32767.)then
              buf(i,1)=0.3333 ! x
              buf(i,2)=0.3333 ! y
              buf(i,3)=32767. ! Y
              goto 11
            endif
          enddo
          call spline(lamda,spectrum,nin,1.0e+30,1.0e+30,sec_deriv)

c         Compute tristimulus values by integrating with color matching fcns
          tristim_X=0.0
          tristim_Y=0.0
          tristim_Z=0.0
          do l=1,nlamda
c           compute spectrum amplitude at wavelength l
            call splint(lamda,spectrum,sec_deriv,nin,wavelength(l),
     +                  spectrum_at_l)
            if(mode.eq.1)then  ! radiance input images
              tristim_X=tristim_X+spectrum_at_l*cmfx(l)
              tristim_Y=tristim_Y+spectrum_at_l*cmfy(l)
              tristim_Z=tristim_Z+spectrum_at_l*cmfz(l)
            else           ! reflectance input images
              tristim_X=tristim_X+illuminant(l)*spectrum_at_l*cmfx(l)
              tristim_Y=tristim_Y+illuminant(l)*spectrum_at_l*cmfy(l)
              tristim_Z=tristim_Z+illuminant(l)*spectrum_at_l*cmfz(l)
            endif
          enddo
          tristim_X=tristim_X * factor
          tristim_Y=tristim_Y * factor
          tristim_Z=tristim_Z * factor
          
c         Convert to x,y,Y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         check for negative luminance
          if(tristim_Y.lt.0.0)then
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
          else
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=tristim_Y
          endif

c         update the 4th output image if desired
          if(nout.eq.4)then
            l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
            m=nint(x*xy_slope+xy_offset)           ! sample
            if(l.lt.1) l=1
            if(m.lt.1) m=1
            if(l.gt.mapxy) l=mapxy
            if(m.gt.mapxy) m=mapxy
            if(xybuf(m,l).lt.32767) xybuf(m,l)=xybuf(m,l)+1
          endif
11        continue

        enddo   ! pixel loop      

        do k=1,3             ! write one line loop
          call xvwrit(ounit(k),buf(1,k),status,' ')
        enddo

      enddo     ! line loop


c stretch and write out the 4th output image
      if(nout.eq.4)then
c       stretch xybuf
        x=0.0
        y=0.0
        l_of_zero=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
        m_of_zero=nint(x*xy_slope+xy_offset)           ! sample
        maxhist=0
        do l=1,mapxy
          do m=1,mapxy
            if(l.ne.l_of_zero.and.m.ne.m_of_zero)then
              if(xybuf(m,l).gt.maxhist) maxhist=xybuf(m,l)
            endif
          enddo
        enddo
        xy_sl=255./log(maxhist+1.0)
        do l=1,mapxy
          do m=1,mapxy
              k=nint(log(xybuf(m,l)+1.0)*xy_sl)
              if(k.gt.255) k=255
              if(k.lt.0) k=0
              xybuf(m,l)=k
          enddo
        enddo
        k=nint(xy_offset)
        l=mapxy- nint(xy_offset) +1
        do i=1,mapxy
          xybuf(k,i)=128 ! draw y axis
          xybuf(i,l)=128 ! draw x axis
          xybuf(i,i)=128 ! draw diagonal
        enddo
c       write output file
        do l=1,mapxy             ! picture loop
          call xvwrit(xyunit,xybuf(1,l),status,' ')
        enddo
      endif

      end

c******************************************************************
      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

c******************************************************************
      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.)then
         call mabend('SPLINT: Wavelengths not unique')
      endif
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END

