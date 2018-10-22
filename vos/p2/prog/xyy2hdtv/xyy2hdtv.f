c
c program xyy2hdtv
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=40000)

      integer*2 red(maxsamp),green(maxsamp),blue(maxsamp)
      integer*4 ounit(3)
      integer*4 unit(3),status
      logical use_macbeth,xvptst
      real*4 x(maxsamp),y(maxsamp),yy(maxsamp)
      real*4 macbeth(3,24)
      real*4 aa(3,3),bb(3,3),cc(3,3)
      character*80 msg
      
      data aa/3.240479, -1.537150, -0.498535,
     +        -0.969256, 1.875992, 0.041556,
     +        0.055648, -0.204043, 1.057311/
      data bb/0.,0.,0., 0.,0.,0., 0.,0.,0./
      data cc/0.,0.,0., 0.,0.,0., 0.,0.,0./

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
      use_macbeth=xvptst('MACBETH')
      if(use_macbeth)then
        nl=4
        ns=6
        kmac=0
      endif


c open all inputs
      if(.not.use_macbeth)then
        do i=1,3
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

      if(use_macbeth)then
        call xvmessage('Macbeth color table',' ')
        call xvmessage('step    x         y         Y     r    g    b'
     +   ,' ')
      endif
      
c process images
      do line=1,nl
                                   ! line loop
        if(.not.use_macbeth)then
          call xvread(unit(1),x,status,'LINE',line,' ')
          call xvread(unit(2),y,status,'LINE',line,' ')
          call xvread(unit(3),yy,status,'LINE',line,' ')
        endif
        
        do ii=1,ns                              ! pixel loop

c         if macbeth cycle through the 24 colors
          if(use_macbeth)then
            kmac=kmac+1
            x(ii)=macbeth(2,kmac)
            y(ii)=macbeth(3,kmac)
            yy(ii)=macbeth(1,kmac)
          endif

c         convert xyY to XYZ normalized from 0 to 1.
          y_tristim=yy(ii)
          x_tristim=yy(ii)*x(ii)/y(ii)
          z_tristim=yy(ii)/y(ii)-x_tristim-yy(ii)
          x_tristim=x_tristim/100.
          y_tristim=y_tristim/100.
          z_tristim=z_tristim/100.
          if(x_tristim.lt.0.)x_tristim=0.
          if(x_tristim.gt.1.)x_tristim=1.
          if(y_tristim.lt.0.)y_tristim=0.
          if(y_tristim.gt.1.)y_tristim=1.
          if(z_tristim.lt.0.)z_tristim=0.
          if(z_tristim.gt.1.)z_tristim=1.
          
c         convert XYZ to sRGB normalized 0 to 1.
   	  bb(1,1) = x_tristim
 	  bb(1,2) = y_tristim
 	  bb(1,3) = z_tristim
          do i=1,3
            do j=1,3
              cc(j,i)=0.
              do k=1,3
                cc(j,i)=cc(j,i)+aa(k,i)*bb(j,k)
              enddo
            enddo
          enddo
          r=cc(1,1) 
          g=cc(1,2)
          b=cc(1,3)
          if(r.lt.0.)r=0.
          if(r.gt.1.)r=1.
          if(g.lt.0.)g=0.
          if(g.gt.1.)g=1.
          if(b.lt.0.)b=0.
          if(b.gt.1.)b=1.
          
c         convert sRGB to spRGB
          if(r.le..00304)then
            r=12.92*r
          else
            r=1.055*r**(1./2.4) - .055
          endif
          if(g.le..00304)then
            g=12.92*g
          else
            g=1.055*g**(1./2.4) - .055
          endif
          if(b.le..00304)then
            b=12.92*b
          else
            b=1.055*b**(1./2.4) - .055
          endif
          red(ii)=nint(r*255.)
          green(ii)=nint(g*255.)
          blue(ii)=nint(b*255.)
          if(red(ii).lt.0)red(ii)=0
          if(red(ii).gt.255)red(ii)=255
          if(green(ii).lt.0)green(ii)=0
          if(green(ii).gt.255)green(ii)=255
          if(blue(ii).lt.0)blue(ii)=0
          if(blue(ii).gt.255)blue(ii)=255


          if(use_macbeth)then
            write(msg,*)kmac,x(ii),y(ii),yy(ii),
     +        red(ii),green(ii),blue(ii)
            call xvmessage(msg,' ')
          endif

        enddo                                  ! pixel loop
        call xvwrit(ounit(1),red,status,' ')
        call xvwrit(ounit(2),green,status,' ')
        call xvwrit(ounit(3),blue,status,' ')
      enddo                                    ! line loop

      RETURN
      END
 
