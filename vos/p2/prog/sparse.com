$!****************************************************************************
$!
$! Build proc for MIPL module sparse
$! VPACK Version 1.9, Wednesday, February 23, 2000, 15:28:59
$!
$! Execute by entering:		$ @sparse
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
$ write sys$output "*** module sparse ***"
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
$ write sys$output "Invalid argument given to sparse.com file -- ", primary
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
$   if F$SEARCH("sparse.imake") .nes. ""
$   then
$      vimake sparse
$      purge sparse.bld
$   else
$      if F$SEARCH("sparse.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sparse
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sparse.bld "STD"
$   else
$      @sparse.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sparse.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sparse.com -
	-s sparse.f -
	-i sparse.imake -
	-p sparse.pdf -
	-t tstsparse.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sparse.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sparse.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM sparse

   To Create the build file give the command:

		$ vimake sparse			(VMS)
   or
		% vimake sparse			(Unix)


************************************************************************/


#define PROGRAM	sparse
#define R2LIB

#define MODULE_LIST sparse.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create sparse.pdf
process help=*
PARM INP TYPE=STRING COUNT=(1:3)
PARM OUT TYPE=STRING COUNT=(1:4)
PARM CIRCLE TYPE=REAL COUNT=(0:400) DEFAULT=--
PARM SN TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM OPTIMIZE TYPE=KEYWORD COUNT=(0:1) VALID=(OPTIMIZE,NOOPT) DEFAULT=NOOPT
PARM COHERE TYPE=KEYWORD COUNT=(0:1) VALID=(COHERE,INCOHERE) DEFAULT=INCOHERE
PARM DEFOCUS TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM FIND TYPE=REAL COUNT=(0:1) VALID=(.0001:1000.) DEFAULT=--
PARM ROTATE TYPE=KEYWORD COUNT=(0:1) VALID=(ROTATE,NOROTATE) DEFAULT=NOROTATE
PARM NOISE TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM REFLAMDA TYPE=REAL COUNT=(0:1) DEFAULT=.55
PARM LAMDA TYPE=REAL COUNT=(0:1) DEFAULT=.55
PARM FULLWELL TYPE=REAL COUNT=(0:1) DEFAULT=0.0
PARM PHOTONS TYPE=REAL COUNT=(0:1) DEFAULT=0.0
PARM QE TYPE=REAL COUNT=(0:1) DEFAULT=1.0
PARM SCALE TYPE=REAL COUNT=(0:1) DEFAULT=0.0
PARM RING TYPE=REAL COUNT=(0,2) DEFAULT=(0.,0.)
PARM SPOKES TYPE=REAL COUNT=(0:6) DEFAULT=(0.,0.,0.,0.,0.,0.)
END-PROC
.TITLE
VICAR2 Program SPARSE to simulate the effect of a sparse aperture.
.HELP

PURPOSE

Sparse simulates the effect of a sparse aperture by constructing a complex
pupil, computing the OTF, and applying it to the input image.

It can also apply a wiener filter to correct for the effect of an aperture
(see the SN keyword).

EXECUTION

direct:

sparse inp=(in,junk,pupil) out=blurred  (Note: junk is ignored)
or
sparse inp=(in) out=blurred circle=(257,257,50,0)

inverse:

sparse inp=(blurred,reference,pupil) out=corrected 'optimize
or
sparse inp=(blurred,reference) out=corrected 'optimize
or
sparse inp=(blurred) out=corrected sn=45 circle=(257,257,50,0)

SOME CLARIFICATIONS:

Note on centering the pupil: 
The images must be a power of 2 in nl and ns. This means there is no center
pixel. We have defined the center at nl/2+1 and ns/2+1. For incoherent
imaging it doesen't matter. For coherent imaging it makes a big difference.

Note on the size of the pupil:
In the incoherent case no part of the filled pupil (aperture) may lie 
outside a circle centered in the pupil and having a radius of 1/4 the image
nl or ns. It is up to you to check for this condition.
The largest "filled aperture" would be created like this:
sparse inp=original.img out=blurred.img circle=(257,257,128,0)
if the picture were 512 by 512 pixels.

Note on simulating spectral bandwidth:
The program creates monochromatic images. To simulate a spectrum you must 
run the program many times and add the images. Each run will require you to 
scale the pupil in proportion to the wavelength.

Note on pupil phase errors:
The phase errors introduced are in units of wavelength. To simulate a spectrum
you will need to make many runs, each with a scaled phase error in proportion to
the wavelength, and add the results.

Note on wiener correction of color imagery:
If you have a broadband input image the psf is a function of color. To correct
for this you must correct for a family of psf's and later select locally
the best one based upon a the local color.

METHOD:

Incoherent case:
1. The pupil (aperture) is constructed.
2. The otf is the autocorrelation of the pupil.
3. The otf is multiplied by the fft of the image.
4. Inverse fft.

Coherent case:
1. The pupil (aperture) is constructed.
2. The otf is the pupil.
3. The otf is multiplied by the fft of the image.
4. Inverse fft.

If SN is specified, requiring a wiener filter, then

Incoherent case:
1. The pupil (aperture) is constructed.
2. The otf is the autocorrelation of the pupil.
3. The filter F=otf/(otf*otf+1/(sn*sn))
4. The filter F is multiplied by the fft of the image.
5. Inverse fft.

Coherent case:
1. The pupil (aperture) is constructed.
2. The otf is the pupil.
3. The filter F=conjg(otf)/(|otf|**2+1/(sn*sn))
3. The filter F is multiplied by the fft of the image.
4. Inverse fft.

RESTRICTIONS

Image must be of a power of 2.
Image <= 512*512 total pixels.

Cognizant Programmer:  JJ Lorre   November 1998

.LEVEL1
.VARIABLE INP
1. input image
2. reference image
3. pupil
.VARIABLE OUT
1. output image
2. pupil image (optional)
3. otf (optional)
4. psf (optional)
.VARIABLE CIRCLE
Sub apertures
line,sample,radius,piston
.VARIABLE RING
1. number of sub apertures,
2. radius
.VARIABLE SPOKES
1. number of spokes.
2. number of mirrors/spoke.
3. center hole radius.
4. radius of inner circle.
5. radius increase factor.
6. (optional) # circles in ring.
.VARIABLE SN
Wiener s/n ratio
.VARIABLE OPTIMIZE
Finds best SN value.
.VARIABLE COHERE
Coherent case
.VARIABLE DEFOCUS
Amount of defocus.
.VARIABLE FIND
To match signal to noise.
.VARIABLE ROTATE
smear rotate otf 180.
.VARIABLE NOISE
Add noise amount.
.VARIABLE REFLAMDA
Reference wavelength.
.VARIABLE LAMDA
Actual wavelength.
.VARIABLE FULLWELL
Electrons/ccd bin
.VARIABLE SCALE
pupil scale

.LEVEL2
.VARIABLE INP
1. input image to be blurred or deconvolved.
2. reference image. This is usually a deconvolved image made from a
   filled aperture (optional).
3. the pupil function which can be complex (optional).

.VARIABLE OUT
1. output image in same format as input image
2. the real part of the complex pupil (optional). 
3. the optical transfer function (optional).
4. the point spread function (optional).

.VARIABLE CIRCLE
To create round sub apertures in the empty pupil.
Sets of four numbers representing:
1. line coordinate of circle center,
2. sample coordinate of circle center,
3. radius of the circle in pixels.
4. piston phase error in wavelengths ( w ).
Circles are set to a complex amplitude of 1.
If the radius is positive the dn will be set to exp(0,w*twopi).
If the radius is negative the sign and phase are ignored and the circle 
dn is set to (0,0).
 So, circle=(100,100,50,0,100,100,-40,0) creates an annulus 10
pixels thick centered on 100 100.
Circles can be written onto the second input file if it exists.
See METHOD for centering things.

Note: circle data can also be entered as x,y,radius,phase.
 See the SCALE keyword.

.VARIABLE RING
A ring of equally spaced sub apertures can be created at the maximum permitted
diameter each of the same radius.
The first argument is the number of sub apertures.
The second argument is the sub aperture radius in pixels.
Example: ring=(13,4.0)

.VARIABLE SPOKES
Creates spokes
1. number of spokes.
2. number of mirrors/spoke.
3. center hole radius.
4. radius of inner circle.
5. radius increase factor.
6. (optional) number of circles in an outer ring.

.VARIABLE SN
The Wiener signal to noise of the image.
This causes SPARSE to treat the input image as a blurred image and to
apply the Wiener restoration filter:
otf/(|otf|**2 +1/(sn)**2)
If SN is not specified the program applies the otf directly to the input image
and no deconvolution is performed.
See OPTIMIZE keyword.

.VARIABLE OPTIMIZE
You have the option to permit SPARSE to determine the SN value for you and
to then use it to deconvolve the input image. In this mode you MUST provide
a second input file which acts as a reference image. This file should be
a restored image obtained by a filled aperture. Optimize finds that SN
value which minimizes the difference between the restoration and this
reference image.

.VARIABLE COHERE
Specifies that the system behaves coherently.
The default is INCOHERE (incoherent behavior).

.VARIABLE DEFOCUS
Amount of defocus expressed as the maximum deviation of a spherical wave
centered at the pupil location nl/2+1, ns/2+1, in wavelengths.
We assume the wave coincides with the pupil on a circle of radius nl/4, ns/4
from the center.
Example: defocus=0.5 gives a 0.5 wavelength of defocus from the ideal focus.

.VARIABLE FIND
Find specifies the signal to noise value of a filled aperture.
(you get this by running sparse once for a filled aperture and entering the
printed s/n ratio as FIND value)
SPARSE will compute the exposure relative to a filled aperture
required to produce a convolved image
with this signal to noise assuming that photon statitics are the sole cause of
noise in the output image.
There is no effect upon the image.

.VARIABLE ROTATE
Smear the otf by rotating it 180 degrees.
This simulates a rotating sparse aperture or linear array of apertures.
The initial pupil orientation doesen't matter.
The default is to not rotate.
Only implemented in the incoherent case.

.VARIABLE NOISE
Add shot noise such that the signal to noise in the output image is the 
specified value.
Example: noise=2.0
The default is to create a noiseless output image.

.VARIABLE FULLWELL
Add shot noise to the output image assuming that the mean dn of the input image
is at the full well ccd charge and that the full well ccd charge is
FULLWELL electrons.

Example: fullwell=10000, causes shot noise to be added in the following manner:
md=mean input picture dn value.
gain=(10000)/md
newdn=olddn+[sqrt(gain*olddn)*r]/gain
where r is a random number centered on 0 with a mean deviation of 1.

The default is to create a noiseless output image.

.VARIABLE PHOTONS
The number of input photons/pixel (not the number detected).
The fullwell in electrons = number of photons * QE 
then the fullwell is used to compute the gain as above.

The default is to create a noiseless output image.

.VARIABLE QE
Quantum efficiency of the detector. See PHOTONS keqword.
Defaults to 1.0

.VARIABLE REFLAMDA
Reference wavelength.
REFLAMDA, in conjunction with LAMDA permit the user to scale the pupil and to
scale the phase in order to easily simulate bandwidth effects.
The default is for LAMDA=REFLAMDA=0.55 microns.
If they are unequal then the ratio R=LAMDA/REFLAMDA is used to scale the pupil
and the phase in the pupil as follows:
Parameter specified sub pupil locations (relative to the image center) 
 are multiplied by 1/R.
Parameter specified sub pupil sizes are multiplied by 1/R.
Parameter specified sub pupil phases are multiplied by 1/R.

Warning: To keep the pupil from being expanded and running the risk of exceeding
the nl/2 and ns/2 limits I suggest that REFLAMDA <= LAMDA.

.VARIABLE LAMDA
Actual wavelength.
See description for REFLAMDA parameter.

.VARIABLE SCALE
The pupil scaling factor.
Defaults to zero.
If the "CIRCLE" parameter values are input as x,y,r,phase instead of as
line,sample,r,phase then the x,y,r values are scaled up to fill the largest
permitted aperture and are then converted to line,sample coordinates. The
computed scaling value is printed out. This is the default for scale=0.
If scale is not zero then the auto scaling to fill the aperture is not
performed but the SCALE value is used instead to scale the circles to some
other value. This is useful to scale many sets of apertures the same.
X,y values are detected from line,samp values by their negative values.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsparse.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
copy /project/test_work/testdata/gll/s0349632000.u orig.img sl=75 ss=150 +
 nl=512 ns=512
sparse inp=(orig.img) out=(blurred.img,pupil.img,otf.img,psf.img) +
circle=(257,257,128,0,257,257,-107.,0) photons=100000
sparse inp=(blurred.img) out=(restored.img) sn=100 +
circle=(257,257,128,0,257,257,-107.,0)
xvd restored.img
sparse inp=(blurred.img,orig.img) out=(restored.img) 'optimize +
circle=(257,257,128,0,257,257,-107.,0)
xvd pupil.img
xvd otf.img
xvd blurred.img
xvd restored.img
!
end-proc
$ Return
$!#############################################################################
