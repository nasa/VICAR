c
c program target
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=25000)
      integer*2 obufr(maxsamp),obufg(maxsamp),obufb(maxsamp)
      integer*4 unit(3),status,count,def,samp,dn,target

c parameters
      call xvpcnt('OUT',nods)      
      call xvparm('CYCLE',cycle,count,def,1)
      call xvparm('NL',nl,count,def,1)
      call xvparm('NS',ns,count,def,1)
      call xvparm('TARGET',target,count,def,1)
      pi=3.141592654
      
c open outputs
      nli=nl/2
      nsi=ns/2
      nl=nli*2
      ns=nsi*2
      do image=1,nods
        call xvunit(unit(image),'OUT',image,status,' ')
        call xvopen(unit(image),status,'U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',
     +              'O_FORMAT','BYTE',' ')
      enddo
      
      if(target.gt.1)goto 10
      
c radial spoke target
      alias=(cycle/pi)**2
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=x*x+y*y
          if(r.gt.alias)then
            theta=atan2(y,x)
            dn=nint(127.*cos(theta*cycle)+127.)
            obufr(samp)=dn
            obufr(samp+nsi)=dn
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=dn
            obufb(samp+nsi)=0
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=x*x+y*y
          if(r.gt.alias)then
            theta=atan2(y,x)
            dn=nint(127.*cos(theta*cycle)+127.)
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=dn
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      goto 20

c radial frequency target
10    alias=max((nli+1)/2,(nsi+1)/2)
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=sqrt(x*x+y*y)
          if(r.eq.0.0)r=.00001
          if(r.lt.alias)then
            wavelength=2.0*alias/r
            dn=nint(127.*cos(r*pi/wavelength)+127.)
            obufr(samp)=dn
            obufr(samp+nsi)=dn
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=dn
            obufb(samp+nsi)=0
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=sqrt(x*x+y*y)
          if(r.eq.0.0)r=.00001
          if(r.lt.alias)then
            wavelength=2.0*alias/r
            dn=nint(127.*cos(r*pi/wavelength)+127.)
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=dn
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo

20    return
      end

