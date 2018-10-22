c program sparse

      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsize=512*512, maxpar=100, maxns=2048)
      complex*8 pupil(maxsize),fftpix(maxsize),otf(maxsize)
      integer*4 inunit,outunit,status,def,count_circles
      integer*4 line,samp,nn(2),count_sn,outunit2,outunit3
      integer*4 count_defocus,count_find,count_noise
      integer*4 count,outunit4,count_sp
      real*4 par(4,maxpar),bufout(maxns),pix(maxsize)
      real*4 lamda,reflamda,ring(2),spokes(6),scr(maxsize)
      equivalence (fftpix,pix),(otf,pupil)
      character*32 format
      logical coherent,incoherent,xvptst,rotate,optimize
      
      twopi=6.283185308
      
c get number of files
      call xvpcnt('INP',nids)
      call xvpcnt('OUT',nods)

c open inputs
      call xvunit(inunit,'INP',1,status,' ')
      call xvsignal(inunit,status,1)
      call xvopen(inunit,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit,status,1)
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      call xvsignal(inunit,status,1)
      if(ns*nl.gt.maxsize)then
        call xvmessage('Picture too big',' ')
        call abend
      endif
      do i=20,1,-1
        if(2**i.eq.ns)then
          nn(1)=ns
          goto 10
        endif
      enddo
      call xvmessage('NS must be a power of 2',' ')
      call abend
10    continue
      do i=20,1,-1
        if(2**i.eq.nl)then
          nn(2)=nl
          goto 11
        endif
      enddo
      call xvmessage('NL must be a power of 2',' ')
      call abend
11    continue

      if(nids.gt.1)then  ! open reference image.
        call xvunit(inunit2,'INP',2,status,' ')
        call xvsignal(inunit2,status,1)
        call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
        call xvsignal(inunit2,status,1)
      endif
      if(nids.gt.2)then  ! open pupil image
        call xvunit(inunit3,'INP',3,status,' ')
        call xvsignal(inunit3,status,1)
        call xvopen(inunit3,status,'U_FORMAT','COMP',' ')
        call xvsignal(inunit3,status,1)
      endif

c open outputs
      call xvunit(outunit,'OUT',1,status,' ')  ! image
      call xvsignal(outunit,status,1)
      call xvopen(outunit,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
      call xvsignal(outunit,status,1)
      if(nods.gt.1)then
        call xvunit(outunit2,'OUT',2,status,' ') ! pupil
        call xvsignal(outunit2,status,1)
        call xvopen(outunit2,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
        call xvsignal(outunit2,status,1)
      endif
      if(nods.gt.2)then
        call xvunit(outunit3,'OUT',3,status,' ') ! OTF
        call xvsignal(outunit3,status,1)
        call xvopen(outunit3,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')        
        call xvsignal(outunit3,status,1)
      endif
      if(nods.gt.3)then
        call xvunit(outunit4,'OUT',4,status,' ') ! PSF
        call xvsignal(outunit4,status,1)
        call xvopen(outunit4,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')        
        call xvsignal(outunit4,status,1)
      endif
      
c get parameters
      call xvparm('CIRCLE',par,count_circles,def,maxpar)
      if(count_circles.gt.0)then
        if((count_circles/4)*4.ne.count_circles)then
          call xvmessage('CIRCLE num args not div by 4',' ')
          call abend
        endif
      endif
      nm=count_circles/4
      call xvparm('SN',sn,count_sn,def,1)
      coherent=xvptst('COHERE')
      if(coherent)then
        call xvmessage('Coherent case',' ')
        incoherent=.false.
      else
        call xvmessage('Incoherent case',' ')
        incoherent=.true.
      endif
      call xvparm('DEFOCUS',defocus,count_defocus,def,1)
      call xvparm('FIND',target_sn,count_find,def,1)
      rotate=xvptst('ROTATE')
      call xvparm('NOISE',desired_sn,count_noise,def,1)
      call xvparm('LAMDA',lamda,count,def,1)
      call xvparm('REFLAMDA',reflamda,count,def,1)
      scale_lamda=lamda/reflamda
      call xvparm('FULLWELL',fullwell,count,def,1)
      call xvparm('SCALE',pupil_scale,count,def,1)
      call xvparm('QE',quant_eff,count,def,1)
      call xvparm('PHOTONS',photons,count,def,1)
      call xvparm('RING',ring,count,def,2)
      call xvparm('SPOKES',spokes,count_sp,def,6)
      optimize=xvptst('OPTIMIZE')
      if(optimize)then
        count_sn=1
        sn=10.
      endif

c create the sub pupil locations.

      if(nids.lt.3)then    ! initialize pupil to 0
        do i=1,nl*ns
          pupil(i)=cmplx(0.,0.)
        enddo
      else                 ! else read it from 3rd input file
        do line=1,nl
          call xvread(inunit3,pupil((line-1)*ns+1),status,
     +      'LINE',line,' ')
          call xvsignal(inunit2,status,1)
        enddo
      endif

c ring
      if(ring(1).gt..5)then
        m=nint(ring(1))
        count_circles=nint(ring(1)*4.0)+count_circles
        radius=(nl/4.0)-ring(2)
        do n=1,m
          angle=twopi*(n-1)/ring(1)
          nm=nm+1
          par(1,nm)=radius*cos(angle) ! x
          par(2,nm)=radius*sin(angle) ! y
          par(3,nm)=ring(2)
          par(4,nm)=0.0
        enddo
      endif
      
c spokes
      if(spokes(1).gt..5)then
        nbranches=nint(spokes(1))
        mirrors=nint(spokes(2))
        center=spokes(3)
        firstrad=spokes(4)
        power=spokes(5)
        angle=0.0
        count_circles=nbranches*mirrors*4+count_circles
        do i=1,nbranches
          radius=firstrad
          distance=center+radius
          do j=1,mirrors
            nm=nm+1
            par(1,nm)=distance*cos(angle) ! x
            par(2,nm)=distance*sin(angle) ! y
            par(3,nm)=radius
            par(4,nm)=0.0
            distance=distance+radius*3.5
            radius=radius*power
          enddo
          angle=angle+twopi/nbranches
        enddo 
        if(count_sp.eq.6)then
          m=nint(spokes(6))
          count_circles=nint(spokes(6)*4.0)+count_circles
          radius=radius/power
          distance=distance-radius*3.5
          do n=1,m
            angle=twopi*(n-1)/spokes(6)
            nm=nm+1
            par(1,nm)=distance*cos(angle) ! x
            par(2,nm)=distance*sin(angle) ! y
            par(3,nm)=radius
            par(4,nm)=0.0
          enddo
        endif
      endif

c rescale coordinates
      if(count_circles.gt.0)then
        iflag=0
        do n=1,count_circles/4  ! see if data is in x,y format
          if(par(1,n).lt.0.0)iflag=1 ! x
          if(par(2,n).lt.0.0)iflag=1 ! y
        enddo
        if(iflag.eq.1)then ! get scale to size up to full aperture
          rmax=0.
          do n=1,count_circles/4
            r=sqrt(par(1,n)**2+par(2,n)**2)+par(3,n)
            if(r.gt.rmax)rmax=r
          enddo
          if(pupil_scale.lt..00001)then
            pupil_scale=(nl/4.0)/rmax
            write(*,*)'pupil scaled up by ',pupil_scale
          endif
          do n=1,count_circles/4
            rsamp=par(1,n)*pupil_scale+nl/2.0+1.0
            rline=nl-(par(2,n)*pupil_scale+nl/2.0)+1.0
            par(1,n)=rline
            par(2,n)=rsamp
            par(3,n)=par(3,n)*pupil_scale
          enddo
        endif 
        
c create the pupil image 
        do n=1,count_circles/4 ! after data is in l,s format
          y=par(1,n) ! line
          y=nl/2.0 + (y-nl/2.0)/scale_lamda
          x=par(2,n) ! sample
          x=ns/2.0 + (x-ns/2.0)/scale_lamda
          sr=par(3,n) ! radius
          sr=sr/scale_lamda
          path=par(4,n)*twopi ! piston error in radians
          path=path/scale_lamda
          r=abs(sr)
          rr=r*r
          do line=max(nint(y-r),1),min(nint(y+r),nl)
            do samp=max(nint(x-r),1),min(nint(x+r),ns)
              r2=(line-y)**2+(samp-x)**2
              if(r2.lt.rr)then
                if(sr.gt.0.)then
c                 pupil(samp+(line-1)*ns)=cmplx(1.0,0.0)
                  pupil(samp+(line-1)*ns)=exp(cmplx(0.,path))
                else
                  pupil(samp+(line-1)*ns)=cmplx(0.0,0.0)
                endif
              endif
            enddo
          enddo
        enddo
      endif
      
c print the area of the pupil as a % of a filled aperture.
      call pupil_area(pupil,ns,nl,fillfactor)
      
c add defocus wavefront
      if(count_defocus.gt.0)then
        defocus=defocus/scale_lamda
        call focus(pupil,ns,nl,defocus)
      endif
      
c write the pupil out
      if(nods.gt.1)then
        do line=1,nl
          do i=1,ns
            k=(line-1)*ns+i
c           bufout(i)=sqrt((real(pupil(k)))**2+(aimag(pupil(k)))**2)
            bufout(i)=real(pupil(k))
          enddo
          call xvwrit(outunit2,bufout,status,' ')
          call xvsignal(outunit2,status,1)
        enddo
      endif
      
c create the optical transfer function as the autocorrelation
c of the pupil.
      if(incoherent)then
        call fourn(pupil,nn,2,1)  ! fft of pupil
        do i=1,nl*ns              ! autocorrelation (the psf)
          pupil(i)=cmplx((real(pupil(i)))**2+(aimag(pupil(i)))**2,0.)
        enddo
        call fourn(otf,nn,2,-1) ! inverse fft, unnormalized otf
        scale=real(otf(1))
        do i=1,nl*ns              ! normalized otf
          otf(i)=otf(i)/scale
        enddo
      endif
      
c create the coherent transfer function as the pupil shifted to center.
      if(coherent)then            ! the pupil is the otf
        call swapc(pupil,fftpix,ns,nl) ! centers pupil in fft    
      endif
      
c rotate to smear the incoherent otf 180 degrees.
      if(rotate)then
        if(coherent)then
          write(*,*)'Rotate & coherent not supported'
          call abend()
        endif
        write(*,*)'begin otf rotation'
        call swapc(otf,fftpix,ns,nl)
        call rotate_otf(otf,fftpix,ns,nl)
        call swapc(otf,fftpix,ns,nl)
        write(*,*)'end otf rotation'
      endif
     
c write the otf out
      if(nods.gt.2)then
        if(incoherent)call swapc(otf,fftpix,ns,nl) ! center in image
        call otf_merit(otf,ns,nl) ! compute moments & mean otf
        do line=1,nl
          do i=1,ns
            k=(line-1)*ns+i
c           bufout(i)=sqrt((real(otf(k)))**2+(aimag(otf(k)))**2)
            bufout(i)=real(otf(k))
          enddo
          call xvwrit(outunit3,bufout,status,' ')
          call xvsignal(outunit3,status,1)
        enddo
        if(incoherent)call swapc(otf,fftpix,ns,nl) ! center at fft origin
      endif

c compute the PSF & write out
      if(nods.gt.3)then
        if(coherent)then
          write(*,*)'coherent psf not supported'
          call abend()
        endif
        do i=1,nl*ns
          fftpix(i)=otf(i)
        enddo
        call fourn(fftpix,nn,2,1) ! fft of otf = psf
        do i=1,nl*ns
          pix(i)=sqrt((real(fftpix(i)))**2+(aimag(fftpix(i)))**2)
        enddo
        psfmax=pix(1)
        do i=1,nl*ns
          pix(i)=pix(i)/psfmax
        enddo
        call swapr(pix,scr,ns,nl)
        call psf_center(pix,ns,nl)
        do line=1,nl
          call xvwrit(outunit4,pix((line-1)*ns+1),status,' ')
          call xvsignal(outunit4,status,1)
        enddo
      endif

c loop to iterate to best sn value if OPTIMIZE is selected.
      loop=0
20    loop=loop+1
      
c read the image into memory
      do line=1,nl
        call xvread(inunit,fftpix((line-1)*ns+1),status,'LINE',line,' ')
        call xvsignal(inunit,status,1)
      enddo
      
c fft the image
      call fourn(fftpix,nn,2,1) ! fft of image

c convolve the image
      if(incoherent)then
        if(count_sn.eq.0)then
          do i=1,nl*ns
            fftpix(i)=fftpix(i)*otf(i) ! convolution of image with otf
          enddo
        endif
      endif
      if(coherent)then
        if(count_sn.eq.0)then
          do i=1,nl*ns
            fftpix(i)=fftpix(i)*otf(i) ! convolution of image with otf
          enddo
        endif
      endif
      
c deconvolve the image
      if(incoherent)then
        if(count_sn.gt.0)then
          sn2=1./(sn*sn)
          scale=1./(1./(1.+sn2))
          do i=1,nl*ns
            ampl=(real(otf(i)))**2+(aimag(otf(i)))**2
            fftpix(i)=fftpix(i)*scale*conjg(otf(i))/(ampl+sn2)
          enddo
        endif
      endif
      if(coherent)then
        if(count_sn.gt.0)then
          sn2=1./(sn*sn)
          scale=1./(1./(1.+sn2))
          do i=1,nl*ns
            ampl=(real(otf(i)))**2+(aimag(otf(i)))**2
            fftpix(i)=fftpix(i)*scale*conjg(otf(i))/(ampl+sn2)
          enddo
        endif
      endif
     
c inverse fft & rescale
      call fourn(fftpix,nn,2,-1) ! inverse fft of image
      scale=1.0/(nl*ns)
      if(incoherent)then
        do i=1,nl*ns
          pix(i)=scale*sqrt((real(fftpix(i)))**2+
     +                      (aimag(fftpix(i)))**2)
        enddo
      endif
      if(coherent)then
        do i=1,nl*ns
          pix(i)=scale*sqrt((real(fftpix(i)))**2+
     +                      (aimag(fftpix(i)))**2)
        enddo
      endif

c compute residual and converge upon the best sn value if OPTIMIZE
c specified.
      if(optimize)then
        if(nids.lt.2)then
          write(*,*)'Need a reference file'
          call abend()
        endif
        call difference(inunit2,pix,nl,ns,residual)
        if(loop.eq.1)then
          resid1=residual
          snval1=sn
          step=1.
          sn=snval1+step
          goto 20
        endif
        if(loop.eq.2)then
          resid2=residual
          snval2=sn
          if(resid2.gt.resid1)then
            step=-step
            sn=snval1+step
          else
            resid1=resid2
            snval1=snval2
            sn=snval2+step
          endif
          goto 20
        endif
        resid2=residual
        snval2=sn
        if(resid2.lt.resid1)then
          resid1=resid2
          snval1=snval2
          if(sn.gt.15)step=sign(2.,step)
          if(sn.gt.25)step=sign(4.,step)
          if(sn.gt.40)step=sign(7.,step)
          if(sn.lt.5)step=sign(.5,step)
          if(sn.lt.3)step=sign(.2,step)
          if(sn.lt.2)step=sign(.1,step)
          write(*,*)'sn,residual,snval2+step',sn,residual,snval2+step
          sn=snval2+step
          goto 20
        else
          optimize=.false.
          sn=sn-step
          write(*,*)'optimum sn value= ',sn,' residual= ',resid1
          goto 20
        endif                
      endif
      
c get the image signal to noise for a gain.
      if(fullwell.gt..001)then
        call get_meandn(pix,ns,nl,dnmean)
        gain_fw=(fullwell)/dnmean
      else if(photons.gt..001)then
        call get_meandn(pix,ns,nl,dnmean)
        fullwell=photons*quant_eff
        gain_fw=(fullwell)/dnmean
        write(*,*)'fullwell= ',fullwell,' electrons'
      else
        gain_fw=1.0
      endif
      call get_sn_new(pix,ns,nl,snr,gain_fw,pupil)
      write(*,*)'Image signal to shot noise ratio ',snr
      
c FIND option: compute the exposure required to get the same s/n ratio 
c as a filled aperture, as input from the FIND keyword. 
      if(count_find.gt.0)then
        call find_sn(pix,ns,nl,target_sn,snr,gain,pupil,gain_fw)
        write(*,*)'gain factor required to restore s/n=',gain/gain_fw
        write(*,*)'number of photons required=',fullwell*(gain/gain_fw) 
        exposure=(gain/gain_fw)/fillfactor 
        write(*,*)'total exposure factor required',exposure 
        exponent=log10(exposure)/log10(1.0/fillfactor)
        write(*,*)'exposure=(1/fillfactor)**x, x=',exponent
      endif
     
c NOISE option: create noise in the image such that the s/n = desired_sn.
      if(count_noise.gt.0)then
        call get_sn_new(pix,ns,nl,snr,1.0,pupil)
        call find_sn(pix,ns,nl,desired_sn,snr,gain,pupil,1.0)
        call set_sn(pix,ns,nl,gain,1.0)
        write(*,*)'Noise added, output snr=',desired_sn
      else if(fullwell.gt..001)then
        call set_sn(pix,ns,nl,gain_fw,1.0)
      else
        write(*,*)'Output image is noiseless'
      endif

c write out image intensity
      do line=1,nl
        do i=1,ns
          k=(line-1)*ns+i
          bufout(i)=pix(k)          
        enddo
c        if(format.eq.'BYTE')then
c          do i=1,ns
c            if(bufout(i).lt.0.)bufout(i)=0.
c            if(bufout(i).gt.255.)bufout(i)=255.
c          enddo
c        endif
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo

      return
      end

c*********************************************************************
      subroutine difference(inunit2,pix,nl,ns,residual)
c returns mean difference between restored image and reference image.
c reference image is the second input file.
      real*4 ref(1024),pix(ns,nl)
      real*8 sum
      integer*4 status
      sum=0.d0
      do line=1,nl
        call xvread(inunit2,ref,status,'LINE',line,' ')
        call xvsignal(inunit2,status,1)
        do i=1,ns
          sum=sum+abs(ref(i)-pix(i,line))
        enddo
      enddo
      residual=sum/(ns*nl)
      return
      end
      
c*********************************************************************
      subroutine rotate_otf(otf,pix,ns,nl)
      complex*8 otf(ns,nl),pix(ns,nl)
      integer*4 s2,l2
      do j=1,nl
        do i=1,ns
          pix(i,j)=otf(i,j)
        enddo
      enddo
      s2=ns/2 + 1
      l2=nl/2 + 1      
      do k=2,179,1 ! rotate 180 degrees in 1 degree increments
        th=k*3.141592654/180.
        costh=cos(th)
        sinth=sin(th)
        do j=1,nl
          do 10 i=1,ns
            ii=nint((i-s2)*costh+(j-l2)*sinth) + s2
            jj=nint((j-l2)*costh-(i-s2)*sinth) + l2
            if(ii.lt.1.or.ii.gt.ns)goto 10
            if(jj.lt.1.or.jj.gt.nl)goto 10
            pix(i,j)=pix(i,j)+otf(ii,jj)
10        continue
        enddo
      enddo
      do j=1,nl
        do i=1,ns
          otf(i,j)=pix(i,j)/real(pix(s2,l2))
        enddo
      enddo
      
      return
      end

c*********************************************************************
      subroutine find_sn(pix,ns,nl,target_sn,snr,gain2,scr,gain_fw)
      real*4 pix(ns,nl)
      complex*8 scr(ns,nl)
      gain1=gain_fw
      snr1=snr
      k=0
      if(target_sn.lt.snr1)gain2=0.5*gain_fw
      if(target_sn.ge.snr1)gain2=2.0*gain_fw
      call get_sn_new(pix,ns,nl,snr2,gain2,scr)
10    k=k+1
      slope=(snr2-snr1)/(gain2-gain1)
      offset=snr2-slope*gain2
      snr1=snr2
      gain1=gain2
      gain2=(target_sn - offset)/slope
      if(gain2.le.0.0)gain2=gain1/2.0
      call get_sn_new(pix,ns,nl,snr2,gain2,scr)
      write(*,*)'gain',gain2,' snr',snr2
      if(k.gt.20)then
        write(*,*)'Exceeded iterations in find_sn'
        return
      endif
      if(abs(snr2-target_sn)/target_sn.gt..001)goto 10            
      return
      end

c*********************************************************************
      subroutine psf_center(pix,ns,nl)
      real*4 pix(ns,nl)
      psfsum=0.0
      psfcenter=0.0
      do j=1,nl
        do i=1,ns
          psfsum=psfsum+pix(i,j)
        enddo
      enddo
      do j=nl/2,nl/2+2
        do i=ns/2,ns/2+2
          psfcenter=psfcenter+pix(i,j)
        enddo
      enddo
      write(*,*)'Psf center/total ratio= ',psfcenter/psfsum
      return
      end

c*********************************************************************
      subroutine otf_merit(otf,ns,nl)
      complex*8 otf(ns,nl)
      otfsum=0.0
      sumof=0.0
      bestotf=0.0
      nl2=nl/2+1
      ns2=ns/2+1
      do j=1,nl
        do i=1,ns
          a=sqrt((real(otf(i,j)))**2+(aimag(otf(i,j)))**2)
          freq=(sqrt(float((nl2-j)**2+(ns2-i)**2)))/ns ! cycles/sample
          if(freq.le.0.5)then
            otfsum=otfsum+a
            sumof=sumof+freq*a        ! first moment
            bestotf=bestotf+(1.0-2.0*freq)   ! ideal otf
          endif
        enddo
      enddo
      write(*,*)'Ratio otf/ideal_otf= ',otfsum/bestotf
      write(*,*)'otf*freq/ideal_otf= ',sumof/bestotf,' cycles/sample'
      write(*,*)'otf*freq/otf= ',sumof/otfsum,' cycles/sample'
      
      return
      end

c*********************************************************************
      subroutine get_meandn(pix,ns,nl,dnmean)
      real*4 pix(ns,nl)
      dnmean=0.0
      do j=1,nl
        do i=1,ns
          dnmean=dnmean+pix(i,j)
        enddo
      enddo
      dnmean=dnmean/(nl*ns)

      return
      end

c*********************************************************************
      subroutine set_sn(pix,ns,nl,desired_sn,snr)
      real*4 pix(ns,nl),noise
      integer*4 seed
      
      gain=(desired_sn/snr)
      call get_seconds( seed)
      seed=-seed
      do j=1,nl
        do i=1,ns
          call ccd_noise(pix(i,j),gain,seed,noise)
          pix(i,j)=pix(i,j)+noise/gain
        enddo
      enddo

      return
      end

c*********************************************************************
c creating ccd noise.
c dn=input dn value.
c gain=electrons/dn
c noise=returned noise.
      subroutine ccd_noise(dn,gain,seed,noise)
      integer*4 seed
      real*4 noise
      
      dark_sigma=10.
      read_sigma=20.
      nlevels=4096
      max_electrons=80000
           
c     Dark current noise.
      ranx=ran1(seed)
      rany=ran1(seed)
      x = max( ranx, 1.0e-10)
      u = sqrt(-2.*log(x)) * cos(2*3.1415927*rany)
      dark_noise = u * dark_sigma

c     Read noise.
      ranx=ran1(seed)
      rany=ran1(seed)
      x = max( ranx, 1.0e-10)
      u = sqrt(-2.*log(x)) * cos(2*3.1415927*rany)
      read_noise = u * read_sigma

c     Quantization noise.
c      ranx=ran1(seed)
c      rany=ran1(seed)
c      x = max( ranx, 1.0e-10)
c      u = sqrt(-2.*log(x)) * cos(2*3.1415927*rany)
c      quant_noise = u * max_electrons/(nlevels*3.464)
      quant_noise=0.0
        
c     Poisson noise.
      shot_noise=poidev(dn*gain,seed)-dn*gain
      
      noise=dark_noise+read_noise+quant_noise+shot_noise
c      noise=shot_noise
        
      return
      end
      

c*********************************************************************
      subroutine get_sn_new(pix,ns,nl,snr,gain,scr)
      real*4 pix(ns,nl),noise
      complex*8 scr(ns/2,nl/2)
      integer*4 seed,nn(2)
      real*8 sums,sumn
      
      nn(1)=ns/2
      nn(2)=nl/2
      ns2=ns/2
      nl2=nl/2
      ns4=ns/4
      nl4=nl/4
      seed=-1234567
      sums=0.0d0
      sumn=0.0d0
      
c compute signal as sum of fft amplitudes of picture
      do j=1,nl2
        do i=1,ns2
          scr(i,j)=cmplx(gain*pix(i+ns4,j+nl4),0.0)
        enddo
      enddo
      call fourn(scr,nn,2,1)
      do j=2,nl2
        do i=2,ns2
          sums=sums+sqrt((real(scr(i,j)))**2+(aimag(scr(i,j)))**2)
        enddo
      enddo
      
c compute noise as sum of fft amplitudes of sqrt(picture)      
      k=0
      do j=1,nl2
        do i=1,ns2
          call ccd_noise(pix(i+ns4,j+nl4),gain,seed,noise)
          scr(i,j)=cmplx(noise,0.0)                
        enddo
      enddo
      call fourn(scr,nn,2,1)
      do j=2,nl2
        do i=2,ns2
          sumn=sumn+sqrt((real(scr(i,j)))**2+(aimag(scr(i,j)))**2)
        enddo
      enddo
      
      snr=sums/sumn

      return
      end



c*********************************************************************
      subroutine pupil_area(pupil,ns,nl,fillfactor)
      complex*8 pupil(ns,nl)
      sum=0.
      pi=3.141592654
      do j=1,nl
        do i=1,ns
          if(abs(real(pupil(i,j)))+abs(aimag(pupil(i,j))).gt.
     +      0.001) sum=sum+1
        enddo
      enddo
      fillfactor=sum/(pi*(ns/4.0)**2)
      write(*,*)'pupil fill factor is',fillfactor
      return
      end

c*********************************************************************
c introduce a defocus condition of "defocus" wavelengths.
      subroutine focus(pupil,ns,nl,defocus)
      complex*8 pupil(ns,nl)
      n=min(nl,ns)
      twopi=6.283185308
      wavefront_radius=((n/4)**2+defocus**2)/(2.0*defocus)
      wavefront_radius_sq=wavefront_radius**2
      centers=ns/2+1
      centerl=nl/2+1
      do j=1,nl
        do i=1,ns
          rad_sq=((j-centerl)**2+(i-centers)**2)
          if(wavefront_radius_sq - rad_sq .gt. 0.)then
            path=sqrt(wavefront_radius_sq - rad_sq) +defocus 
     +         -wavefront_radius
            pupil(i,j)=pupil(i,j)*exp(cmplx(0.,twopi*path))
          endif
        enddo
      enddo
      return
      end

c*********************************************************************
c switches the buffer quadrants of buf1.
c buf2 is scr space.
      subroutine swapc(buf1,buf2,ns,nl)
      complex*8 buf1(ns,nl),buf2(ns,nl)
      ns2=ns/2
      nl2=nl/2
      do j=1,nl2
        do i=1,ns2
          buf2(i,j)=buf1(i+ns2,j+nl2)
          buf2(i+ns2,j)=buf1(i,j+nl2)
          buf2(i,j+nl2)=buf1(i+ns2,j)
          buf2(i+ns2,j+nl2)=buf1(i,j)
        enddo
      enddo
      do j=1,nl
        do i=1,ns
          buf1(i,j)=buf2(i,j)
        enddo
      enddo
      return
      end
      
c*********************************************************************
c switches the buffer quadrants of buf1.
c buf2 is scr space.
      subroutine swapr(buf1,buf2,ns,nl)
      real*4 buf1(ns,nl),buf2(ns,nl)
      ns2=ns/2
      nl2=nl/2
      do j=1,nl2
        do i=1,ns2
          buf2(i,j)=buf1(i+ns2,j+nl2)
          buf2(i+ns2,j)=buf1(i,j+nl2)
          buf2(i,j+nl2)=buf1(i+ns2,j)
          buf2(i+ns2,j+nl2)=buf1(i,j)
        enddo
      enddo
      do j=1,nl
        do i=1,ns
          buf1(i,j)=buf2(i,j)
        enddo
      enddo
      return
      end

c*********************************************************************
c 2-d fft routine
c DATA complex 2-d array of data
c NDIM # dimensions
c NN array of dimension sizes
c isign +1 direct, -1 inverse unscaled
      SUBROUTINE FOURN(DATA,NN,NDIM,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION NN(NDIM),DATA(*)
      NTOT=1
      DO 11 IDIM=1,NDIM
        NTOT=NTOT*NN(IDIM)
11    CONTINUE
      NPREV=1
      DO 18 IDIM=1,NDIM
        N=NN(IDIM)
        NREM=NTOT/(N*NPREV)
        IP1=2*NPREV
        IP2=IP1*N
        IP3=IP2*NREM
        I2REV=1
        DO 14 I2=1,IP2,IP1
          IF(I2.LT.I2REV)THEN
            DO 13 I1=I2,I2+IP1-2,2
              DO 12 I3=I1,IP3,IP2
                I3REV=I2REV+I3-I2
                TEMPR=DATA(I3)
                TEMPI=DATA(I3+1)
                DATA(I3)=DATA(I3REV)
                DATA(I3+1)=DATA(I3REV+1)
                DATA(I3REV)=TEMPR
                DATA(I3REV+1)=TEMPI
12            CONTINUE
13          CONTINUE
          ENDIF
          IBIT=IP2/2
1         IF ((IBIT.GE.IP1).AND.(I2REV.GT.IBIT)) THEN
            I2REV=I2REV-IBIT
            IBIT=IBIT/2
          GO TO 1
          ENDIF
          I2REV=I2REV+IBIT
14      CONTINUE
        IFP1=IP1
2       IF(IFP1.LT.IP2)THEN
          IFP2=2*IFP1
          THETA=ISIGN*6.28318530717959D0/(IFP2/IP1)
          WPR=-2.D0*DSIN(0.5D0*THETA)**2
          WPI=DSIN(THETA)
          WR=1.D0
          WI=0.D0
          DO 17 I3=1,IFP1,IP1
            DO 16 I1=I3,I3+IP1-2,2
              DO 15 I2=I1,IP3,IFP2
                K1=I2
                K2=K1+IFP1
                TEMPR=SNGL(WR)*DATA(K2)-SNGL(WI)*DATA(K2+1)
                TEMPI=SNGL(WR)*DATA(K2+1)+SNGL(WI)*DATA(K2)
                DATA(K2)=DATA(K1)-TEMPR
                DATA(K2+1)=DATA(K1+1)-TEMPI
                DATA(K1)=DATA(K1)+TEMPR
                DATA(K1+1)=DATA(K1+1)+TEMPI
15            CONTINUE
16          CONTINUE
            WTEMP=WR
            WR=WR*WPR-WI*WPI+WR
            WI=WI*WPR+WTEMP*WPI+WI
17        CONTINUE
          IFP1=IFP2
        GO TO 2
        ENDIF
        NPREV=N*NPREV
18    CONTINUE
      RETURN
      END

c*********************************************************************
      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END

c**************************************************************
      FUNCTION POIDEV(XM,IDUM)
      PARAMETER (PI=3.141592654)
      DATA OLDM /-1./
      save oldm
      IF (XM.LT.12.)THEN
        IF (XM.NE.OLDM) THEN
          OLDM=XM
          G=EXP(-XM)
        ENDIF
        EM=-1
        T=1.
2       EM=EM+1.
        T=T*RAN1(IDUM)
        IF (T.GT.G) GO TO 2
      ELSE
        IF (XM.NE.OLDM) THEN
          OLDM=XM
          SQ=SQRT(2.*XM)
          ALXM=ALOG(XM)
          G=XM*ALXM-GAMMLN(XM+1.)
        ENDIF
1       Y=TAN(PI*RAN1(IDUM))
        EM=SQ*Y+XM
        IF (EM.LT.0.) GO TO 1
        EM=INT(EM)
        T=0.9*(1.+Y**2)*EXP(EM*ALXM-GAMMLN(EM+1.)-G)
        IF (RAN1(IDUM).GT.T) GO TO 1
      ENDIF
      POIDEV=EM
      RETURN
      END

c**************************************************************
      FUNCTION RAN1(IDUM)
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF /0/
      save iff
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
      IF(J.GT.97.OR.J.LT.1)write(*,*)'RAN1 error'
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END

c**************************************************************
      FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END

c****************************************************************
      SUBROUTINE PIKSR2(N,ARR,BRR)
      DIMENSION ARR(N),BRR(N)
      DO 12 J=2,N
        A=ARR(J)
        B=BRR(J)
        DO 11 I=J-1,1,-1
          IF(ARR(I).LE.A)GO TO 10
          ARR(I+1)=ARR(I)
          BRR(I+1)=BRR(I)
11      CONTINUE
        I=0
10      ARR(I+1)=A
        BRR(I+1)=B
12    CONTINUE
      RETURN
      END
