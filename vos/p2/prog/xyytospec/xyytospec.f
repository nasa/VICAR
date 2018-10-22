c
c program xyytospec
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer maxbins,ntable,maxsamp,ndevices,nnearest
      parameter (maxbins=40,ntable=1000,maxsamp=20000,ndevices=13)

c maxbins= max number of bins/dimension in the Luv cube.
c ntable= max number of device calibration table entries
c maxsamp= max line length
c ndevices= number of supported devices

      character*100 path,filename,msg
      character*15 device,monitor,caldevices(ndevices)
      character*15 calnames_left(ndevices),calnames_right(ndevices)
      integer*2 red(maxsamp),green(maxsamp),blue(maxsamp)
      integer*4 ounit(3),def,count,nearpts(ndevices)
      integer*4 unit(3),status,nl,ns
      logical use_macbeth,xvptst
      real*4 r(ntable),g(ntable),b(ntable),nearest(30)
      real*4 L(ntable),u(ntable),v(ntable),range(ntable)
      real*4 x(maxsamp),y(maxsamp),yy(maxsamp)
      real*4 lo_L,hi_L,lo_u,hi_u,lo_v,hi_v
      real*4 coefr(4,maxbins,maxbins,maxbins)
      real*4 coefg(4,maxbins,maxbins,maxbins)
      real*4 coefb(4,maxbins,maxbins,maxbins)
      real*4 macbeth(3,24)
      real*4 cuber(2,2,2),cubeg(2,2,2),cubeb(2,2,2)
      real*8 c(30,4),cl(30),coef(4),vv(30),ex(4),wts(30),er,eg,eb
      real*8 sum_er,sum_eg,sum_eb

      data caldevices/'mda','fujix','alps','temblor','wasatch',
     +  'kodak168','kodak230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data calnames_left/'mda','fjx','alt','temlt','wlt',
     +  'xl168','xl230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data calnames_right/'mda','fjx','art','temrt','wrt',
     +  'xl168','xl230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data nearpts/8,8,8,8,8,8,8,8,8,8,8,8,8/

c table of macbeth Yxy values to test algorithm accuracy.
c only used with the MACBETH keyword.
      data macbeth/
     +  10.1,.400,.350,  35.8,.377,.345,  19.3,.247,.251,
     +  13.3,.337,.422,  24.3,.265,.240,  43.1,.261,.343,
     +  30.1,.506,.407,  12.0,.211,.175,  19.8,.453,.306,
     +   6.6,.285,.202,  44.3,.380,.489,  43.1,.473,.438,
     +   6.1,.187,.129,  23.4,.305,.478,  12.0,.539,.313,
     +  59.1,.448,.470,  19.8,.364,.233,  19.8,.196,.252,
     +  90.0,.310,.316,  59.1,.310,.316,  36.2,.310,.316,
     +  19.8,.310,.316,   9.0,.310,.316,   3.1,.310,.316/


c parameters
      call xvparm('PATH',path,count,def,1)
      call xvparm('DEVICE',device,count,def,1)
      call xvmessage(device,' ')
      call xvparm('MONITOR',monitor,count,def,1)
      use_macbeth=xvptst('MACBETH')
      if(use_macbeth)then
        nl=4
        ns=6
        kmac=0
      endif

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if((nin.ne.3).and.(.not.use_macbeth))then
         call mabend('Require 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.ne.3)then
         call mabend('Require 3 outputs')
      endif

c build calibration filename
      k=0
      do i=1,ndevices
        if(device.eq.caldevices(i)) k=i
c        call xvmessage(caldevices(i),' ')
      enddo
      if(k.eq.0) call mabend('unrecognized device')
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
      if(monitor.eq.'right')then
        do i=1,15
          if(calnames_right(k)(i:i).ne.' ')then
            j=j+1
            filename(j:j)=calnames_right(k)(i:i)
          else
            goto 2
          endif
        enddo
2       continue
      else
        do i=1,15
          if(calnames_left(k)(i:i).ne.' ')then
            j=j+1
            filename(j:j)=calnames_left(k)(i:i)
          else
            goto 3
          endif
        enddo
3       continue
      endif

c get NEAREST number of points to use in least squares fit
      nnearest=nearpts(k)
      call xvparm('NEAREST',nnearest,count,def,1)
      write(msg,*)'Using nearest ',nnearest,' points for fitting'
      call xvmessage(msg,' ')

      ntab=0
c read gray scale table for display device
c      filename(j+1:j+2)='.1'
c      call xvmessage('reading gray table '//filename,' ')
c      call read_table(filename,x,y,yy,r,g,b,ntable,maxsamp,ntab,1)
c      write(msg,*)ntab,' points located in gray table'
c      call xvmessage(msg,' ')

      n=ntab
c read color table for display device
      filename(j+1:j+2)='.2'
      call xvmessage('reading color table '//filename,' ')
      call read_table(filename,x,y,yy,r,g,b,ntable,maxsamp,ntab,2)
      write(msg,*)ntab-n,' points located in color table'
      call xvmessage(msg,' ')

c convert xyY in table to Luv
      do k=1,ntab
        call xyytoluv(x(k),y(k),yy(k),L(k),u(k),v(k),ind)
c        if(ind.eq.1)call mabend('xyytoluv ind=1')
      enddo

c determine Luv limits
      lo_L=L(1)
      hi_L=L(1)
      lo_u=u(1)
      hi_u=u(1)
      lo_v=v(1)
      hi_v=v(1)
      do k=2,ntab
        if(L(k).lt.lo_L)lo_L=L(k)
        if(L(k).gt.hi_L)hi_L=L(k)
        if(u(k).lt.lo_u)lo_u=u(k)
        if(u(k).gt.hi_u)hi_u=u(k)
        if(v(k).lt.lo_v)lo_v=v(k)
        if(v(k).gt.hi_v)hi_v=v(k)
      enddo
      call xvmessage('Device calibration file limits:',' ')
      write(msg,*)'low L= ',lo_L,' high L= ',hi_L
      call xvmessage(msg,' ')
      write(msg,*)'low u= ',lo_u,' high u= ',hi_u
      call xvmessage(msg,' ')
      write(msg,*)'low v= ',lo_v,' high v= ',hi_v
      call xvmessage(msg,' ')
      lo_L=lo_L - .01
      lo_u=lo_u - .01
      lo_v=lo_v - .01
      hi_L=hi_L + .01
      hi_u=hi_u + .01
      hi_v=hi_v + .01


c clear the polynomial coefficient cubes
      do i=1,maxbins
        do j=1,maxbins
          do k=1,maxbins
            coefr(1,i,j,k)=0.0
          enddo
        enddo
      enddo

c open all inputs
      if(.not.use_macbeth)then
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.maxsamp)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
      enddo
      endif

c open all outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c determine Luv limits for the input image 
      if(.not.use_macbeth)then
        plo_L=1.0e+20
        phi_L=-1.0e+20
        plo_u=1.0e+20
        phi_u=-1.0e+20
        plo_v=1.0e+20
        phi_v=-1.0e+20
        do line=1,nl,2                         ! line loop
          call xvread(unit(1),x,status,'LINE',line,' ')
          call xvread(unit(2),y,status,'LINE',line,' ')
          call xvread(unit(3),yy,status,'LINE',line,' ')
          do i=1,ns,2                       ! pixel loop
c           convert xyy to Luv
            call xyytoluv(x(i),y(i),yy(i),rl,ru,rv,ind)
            if(ind.eq.0)then
              if(rl.lt.plo_L)plo_L=rl
              if(rl.gt.phi_L)phi_L=rl
              if(ru.lt.plo_u)plo_u=ru
              if(ru.gt.phi_u)phi_u=ru
              if(rv.lt.plo_v)plo_v=rv
              if(rv.gt.phi_v)phi_v=rv
            endif
          enddo
        enddo
        call xvmessage('Input image Luv limits:',' ')
        write(msg,*)'low L= ',plo_L,' high L= ',phi_L
        call xvmessage(msg,' ')
        write(msg,*)'low u= ',plo_u,' high u= ',phi_u
        call xvmessage(msg,' ')
        write(msg,*)'low v= ',plo_v,' high v= ',phi_v
        call xvmessage(msg,' ')
        lo_L=plo_L - .01
        lo_u=plo_u - .01
        lo_v=plo_v - .01
        hi_L=phi_L + .01
        hi_u=phi_u + .01
        hi_v=phi_v + .01
      endif


c set up linear mapping from Luv coordinates to calibration cube coordinates
c where the polynomial coefficients reside which convert Luv to rgb.
      slope_L=(maxbins-1)/(hi_L-lo_L)
      offset_L=1-slope_L*lo_L
      slope_u=(maxbins-1)/(hi_u-lo_u)
      offset_u=1-slope_u*lo_u
      slope_v=(maxbins-1)/(hi_v-lo_v)
      offset_v=1-slope_v*lo_v

      if(use_macbeth)then
        call xvmessage('Macbeth color table',' ')
        call xvmessage('step    x         y         Y     r    g    b'
     +   ,' ')
      endif
      npoly=0

c **************** Invert the calibration cube *******************

      do iL=1,maxbins
        do iu=1,maxbins
          do iv=1,maxbins

c           compute the Luv values of the iL,iu,iv cube coordinate
            rrL=(iL-offset_L)/slope_L
            rru=(iu-offset_u)/slope_u
            rrv=(iv-offset_v)/slope_v

c           determine the nearest N table points to our Luv coordinate
            do k=1,ntab
              range(k)=(rrl-L(k))**2+(rru-u(k))**2+(rrv-v(k))**2
            enddo

            n=0
200         continue
c           quadrant 1
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.lt.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 2
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.lt.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 3
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.ge.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 4
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.ge.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 5
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.lt.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 6
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.lt.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 7
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.ge.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 8
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.ge.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

            if(n.lt.nnearest)goto 200
201         continue

c           compute red polynomial
            do k=1,nnearest
              c(k,1)=L(nearest(k))
              c(k,2)=u(nearest(k))
              c(k,3)=v(nearest(k))
              c(k,4)=1.d0
              cl(k)=r(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coef,vv,er,ex,wts,ind)
            if(ind.ne.0)then
              call mabend('red lsqp solution error')
            endif
            coefr(1,iL,iu,iv)=coef(1)
            coefr(2,iL,iu,iv)=coef(2)
            coefr(3,iL,iu,iv)=coef(3)
            coefr(4,iL,iu,iv)=coef(4)

c           compute green polynomial
            do k=1,nnearest
              cl(k)=g(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coef,vv,eg,ex,wts,ind)
            if(ind.ne.0)call mabend('green lsqp solution error')
            coefg(1,iL,iu,iv)=coef(1)
            coefg(2,iL,iu,iv)=coef(2)
            coefg(3,iL,iu,iv)=coef(3)
            coefg(4,iL,iu,iv)=coef(4)

c           compute blue polynomial
            do k=1,nnearest
              cl(k)=b(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coef,vv,eb,ex,wts,ind)
            if(ind.ne.0)call mabend('blue lsqp solution error')
            coefb(1,iL,iu,iv)=coef(1)
            coefb(2,iL,iu,iv)=coef(2)
            coefb(3,iL,iu,iv)=coef(3)
            coefb(4,iL,iu,iv)=coef(4)

          enddo
        enddo
      enddo
      
c ***************** process images ***************************

      del_L=1./slope_L  ! the grid spacing in Luv space.
      del_u=1./slope_u
      del_v=1./slope_v
      do line=1,nl                         ! line loop
        if(.not.use_macbeth)then
          call xvread(unit(1),x,status,'LINE',line,' ')
          call xvread(unit(2),y,status,'LINE',line,' ')
          call xvread(unit(3),yy,status,'LINE',line,' ')
        endif
        
        do i=1,ns                       ! pixel loop
        
c         if macbeth, cycle through the 24 colors
          if(use_macbeth)then
            kmac=kmac+1
            x(i)=macbeth(2,kmac)
            y(i)=macbeth(3,kmac)
            yy(i)=macbeth(1,kmac)
          endif

c         convert xyy to Luv
          call xyytoluv(x(i),y(i),yy(i),rl,ru,rv,ind)
          if(ind.eq.1)then
            red(i)=0
            green(i)=0
            blue(i)=0
            goto 100
          endif
          
c         compute low corner of enclosing cube il,iu,iv by truncation.
          iL=(slope_L*rl+offset_L)
          iu=(slope_u*ru+offset_u)
          iv=(slope_v*rv+offset_v)
          if(iL.lt.1)iL=1
          if(iL.ge.maxbins)iL=maxbins-1
          if(iu.lt.1)iu=1
          if(iu.ge.maxbins)iu=maxbins-1
          if(iv.lt.1)iv=1
          if(iv.ge.maxbins)iv=maxbins-1
          
c         compute an rgb solution from each corner of the enclosing
c         calibration cube. cube( il, iu, iv)
          do jl=1,2
            do ju=1,2
              do jv=1,2
                cuber(jl,ju,jv)=
     +            coefr(1,iL+jl-1,iu+ju-1,iv+jv-1)*rl+
     +            coefr(2,iL+jl-1,iu+ju-1,iv+jv-1)*ru+
     +            coefr(3,iL+jl-1,iu+ju-1,iv+jv-1)*rv+
     +            coefr(4,iL+jl-1,iu+ju-1,iv+jv-1)
                cubeg(jl,ju,jv)=
     +            coefg(1,iL+jl-1,iu+ju-1,iv+jv-1)*rl+
     +            coefg(2,iL+jl-1,iu+ju-1,iv+jv-1)*ru+
     +            coefg(3,iL+jl-1,iu+ju-1,iv+jv-1)*rv+
     +            coefg(4,iL+jl-1,iu+ju-1,iv+jv-1)
                cubeb(jl,ju,jv)=
     +            coefb(1,iL+jl-1,iu+ju-1,iv+jv-1)*rl+
     +            coefb(2,iL+jl-1,iu+ju-1,iv+jv-1)*ru+
     +            coefb(3,iL+jl-1,iu+ju-1,iv+jv-1)*rv+
     +            coefb(4,iL+jl-1,iu+ju-1,iv+jv-1)
              enddo
            enddo
          enddo
     
c         bilinearly interpolate rgb
          ril=(il-offset_L)/slope_L  ! luminance of low cube corner
          riu=(iu-offset_u)/slope_u  ! u of low cube corner
          riv=(iv-offset_v)/slope_v  ! v of low cube corner
          
          wuhi=(ru-riu)/del_u        ! interpolation weights
          wulo=(riu+del_u-ru)/del_u
          wvhi=(rv-riv)/del_v
          wvlo=(riv+del_v-rv)/del_v
          wlhi=(rl-ril)/del_L
          wllo=(ril+del_L-rl)/del_L
          
          dnvlo=cuber(1,2,1)*wuhi+cuber(1,1,1)*wulo
          dnvhi=cuber(1,2,2)*wuhi+cuber(1,1,2)*wulo
          dnllo=dnvlo*wvlo+dnvhi*wvhi
          dnvlo=cuber(2,2,1)*wuhi+cuber(2,1,1)*wulo
          dnvhi=cuber(2,2,2)*wuhi+cuber(2,1,2)*wulo
          dnlhi=dnvlo*wvlo+dnvhi*wvhi
          red(i)=nint(dnllo*wllo+dnlhi*wlhi)
                    
          dnvlo=cubeg(1,2,1)*wuhi+cubeg(1,1,1)*wulo
          dnvhi=cubeg(1,2,2)*wuhi+cubeg(1,1,2)*wulo
          dnllo=dnvlo*wvlo+dnvhi*wvhi
          dnvlo=cubeg(2,2,1)*wuhi+cubeg(2,1,1)*wulo
          dnvhi=cubeg(2,2,2)*wuhi+cubeg(2,1,2)*wulo
          dnlhi=dnvlo*wvlo+dnvhi*wvhi
          green(i)=nint(dnllo*wllo+dnlhi*wlhi)
          
          dnvlo=cubeb(1,2,1)*wuhi+cubeb(1,1,1)*wulo
          dnvhi=cubeb(1,2,2)*wuhi+cubeb(1,1,2)*wulo
          dnllo=dnvlo*wvlo+dnvhi*wvhi
          dnvlo=cubeb(2,2,1)*wuhi+cubeb(2,1,1)*wulo
          dnvhi=cubeb(2,2,2)*wuhi+cubeb(2,1,2)*wulo
          dnlhi=dnvlo*wvlo+dnvhi*wvhi
          blue(i)=nint(dnllo*wllo+dnlhi*wlhi)
          
          if(red(i).lt.0)red(i)=0
          if(red(i).gt.255)red(i)=255
          if(green(i).lt.0)green(i)=0
          if(green(i).gt.255)green(i)=255
          if(blue(i).lt.0)blue(i)=0
          if(blue(i).gt.255)blue(i)=255

100       continue
          if(use_macbeth)then
            write(msg,*)kmac,x(i),y(i),yy(i),red(i),green(i),blue(i)
            call xvmessage(msg,' ')
          endif

        enddo   ! pixel loop
        call xvwrit(ounit(1),red,status,' ')
        call xvwrit(ounit(2),green,status,' ')
        call xvwrit(ounit(3),blue,status,' ')
      enddo     ! line loop


c compute error estimate by running the device calibration file through
c the polynomials.
      sum_er=0.d0
      sum_eg=0.d0
      sum_eb=0.d0
      npoly=0
      do 101 i=1,ntab
          iL=nint(slope_L*L(i)+offset_L)
          iu=nint(slope_u*u(i)+offset_u)
          iv=nint(slope_v*v(i)+offset_v)
          if(iL.lt.1)goto 101
          if(iL.gt.maxbins)goto 101
          if(iu.lt.1)goto 101
          if(iu.gt.maxbins)goto 101
          if(iv.lt.1)goto 101
          if(iv.gt.maxbins)goto 101
          scr=coefr(1,iL,iu,iv)*L(i)+coefr(2,iL,iu,iv)*u(i)+
     +        coefr(3,iL,iu,iv)*v(i)+coefr(4,iL,iu,iv)
          if(scr.lt.0) scr=0.0
          if(scr.gt.255.0) scr=255.0
          sum_er=sum_er+abs(scr-r(i))
          scr=coefg(1,iL,iu,iv)*L(i)+coefg(2,iL,iu,iv)*u(i)+
     +        coefg(3,iL,iu,iv)*v(i)+coefg(4,iL,iu,iv)
          if(scr.lt.0) scr=0.0
          if(scr.gt.255.0) scr=255.0
          sum_eg=sum_eg+abs(scr-g(i))
          scr=coefb(1,iL,iu,iv)*L(i)+coefb(2,iL,iu,iv)*u(i)+
     +        coefb(3,iL,iu,iv)*v(i)+coefb(4,iL,iu,iv)
          if(scr.lt.0) scr=0.0
          if(scr.gt.255.0) scr=255.0
          sum_eb=sum_eb+abs(scr-b(i))
          npoly=npoly+1  ! just a counter
101   continue
      if(npoly.gt.0)then
        write(msg,*)'red error estimate is ',sum_er/npoly,' dn'
        call xvmessage(msg,' ')
        write(msg,*)'green error estimate is ',sum_eg/npoly,' dn'
        call xvmessage(msg,' ')
        write(msg,*)'blue error estimate is ',sum_eb/npoly,' dn'
        call xvmessage(msg,' ')
        write(msg,*)'based upon ',npoly,
     +      ' calibration points near actual solutions'
        call xvmessage(msg,' ')
      endif

      end

c*******************************************************************
      subroutine read_table(filename,x,y,yy,r,g,b,ntable,maxsamp,k,n)
      real*4 r(ntable),g(ntable),b(ntable)
      real*4 x(maxsamp),y(maxsamp),yy(maxsamp)
      character*100 filename

      open(unit=10,file=filename,access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)goto 6

      do i=1,5-n
        read(unit=10,fmt=*,iostat=ios)
        if(ios.gt.0)then
          call xvmessage('read error on headers',' ')
          goto 1
        endif
      enddo

3     k=k+1
      if(k.gt.ntable)goto 4
      read(unit=10,fmt=*,iostat=ios) i,yy(k),x(k),y(k),r(k),g(k),b(k)
      if((abs(r(k)-255.).lt..01).and.(abs(g(k)-255.).lt..01).and.
     +   (abs(b(k)-255.).lt..01))goto 2
      if(ios.eq.0)goto 3
      if(ios.lt.0)then  ! EOF
        k=k-1
        return
      endif

      call xvmessage('read error on table file',' ')
      goto 1
6     call xvmessage('cannot open table file',' ')
      goto 1
4     call xvmessage('Too many table entries',' ')
1     call mabend('Cannot read calibration file')
2     close(unit=10)
      return
      end
c******************************************************************
      subroutine xyytoluv(x,y,yy,L,u,v,ind)
      implicit real*4(a-z)
      integer*4 ind
      ind=0
      if(yy.le..00001)then
        L=0.
        u=0.
        v=0.
        ind=1
        return
      endif      
      ty=yy
      tx=x*ty/y
      tz=(tx-x*tx-x*ty)/x
      yoy=ty/100.
      if(yoy.gt..008856)then
        L=116.0*(yoy**.33333) - 16.0
      else
        L=903.3*yoy
      endif
      temp=tx+15.0*ty+3.0*tz
      up=4.0*tx/temp
      vp=9.0*ty/temp
      u=13.0*L*(up-0.2105263d0)
      v=13.0*L*(vp-0.4736842d0)
      return
      end

c**************************************************************
      SUBROUTINE DLSQP(NE,NU,C,CL,X1,V,E,EX,wts,ind)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts(i)=weights
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C          V(I) = RESIDUALS  (I.E. OBSERVED MINUS COMPUTED)
C          E = MEAN ERROR OF THE UNIT WEIGHT
C          EX(J) = MEAN ERRORS OF THE UNKNOWNS
c          ind=0 if OK  =1 if division by zero
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      integer s,ss,ind
      parameter (s=4,ss=30)
      REAL*8  A(s,s),AL(s),R(s,s),RL(s),Q(s,s),X(s),SL,SQ,P,SUM
      REAL*8 C(ss,s),CL(ss),X1(s),V(ss),EX(s),wts(ss),E
C
      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).ne.0.d0)then
        R(I,J)=A(I,J)/A(I,I)
      else
        ind=1
        return
      endif
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).ne.0.d0)then
        RL(1)=AL(1)/A(1,1)
      else
        ind=1
        return
      endif
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).ne.0.d0)then
        RL(I)=AL(I)/A(I,I)
      else
        ind=1
        return
      endif
125   continue
      X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      if(a(nu,nu).ne.0.d0)then
        Q(NU,NU)=1./A(NU,NU)
      else
        ind=1
        return
      endif
      DO 150 I=1,NUM
      NP=NUP-1
      DO 135 J=I,NUM
      NM=NU-J
      JP=NM+1
      P=0.
      DO 135 K=JP,NU
      P=P-R(NM,K)*Q(NP,K)
      Q(NP,NM)=P
135   Q(NM,NP)=P
      NPM=NP-1
      SQ=0.
      DO 145 L=NP,NU
145   SQ=SQ-R(NPM,L)*Q(L,NPM)
      if(a(npm,npm).ne.0.d0)then
        Q(NPM,NPM)=1./A(NPM,NPM)+SQ
      else
        ind=1
        return
      endif
150   continue
      DO 151 I=1,NE
      V(I)=0.
      DO 151 J=1,NU
151   V(I)=V(I)+C(I,J)* X(J)
      SL=0.
      DO 153 I=1,NE
      V(I)=CL(I)-V(I)
153   SL=SL+V(I)*V(I)
      FNE=NE
      FNU=NU
      if(fne.ne.fnu)then
        E=DSQRT(SL/(FNE-FNU))
      else
        ind=1
        return
      endif
c      DO 160 I=1,NU
c        IF ( Q(I,I) .GE. 0.D0 ) THEN
c          EX(I)=E*DSQRT(Q(I,I))
c        ELSE
c          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
c        END IF
c160   CONTINUE      
      RETURN
      END
 
