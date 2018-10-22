c
c program polarect2
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxnl=3002,maxns=3002)

      integer*4 ounit,def,count,status,unit,samp
      real*4 buf(maxns,maxnl),obuf(maxns)
      real*4 center(2)
      logical xvptst,interp

c checks
      call xveaction('SA',' ')

c open input
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      call xvget(unit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.maxns)then
        call xvmessage('Input image line too long',' ')
        call abend
      endif
      if(nl.gt.maxnl)then
        call xvmessage('Input image column too long',' ')
        call abend
      endif

c parameters
      pi=3.14159265
      call xvparm('NL',nlo,count,def,1)
      call xvparm('NS',nso,count,def,1)
      call xvparm('CENTER',center,count,def,2)
      if(xvptst('INTERP'))then
        interp=.true.
        write(*,*)'Using bilinear interpolation'
      else
        interp=.false.
        write(*,*)'Using nearest neighbor'
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',
     +  'U_NL',nlo,'U_NS',nso,' ')

c copy input into memory
      do line=1,nl
        call xvread(unit,buf(1,line),status,' ')
      enddo

c create polar image
      if(xvptst('direct'))then
        nl1=nl-1
        ns1=ns-1
        degpline=2.0*pi/nlo
        do line=1,nlo
          theta=(line-1)*degpline
          sinth=sin(theta)
          costh=cos(theta)
          do samp=1,nso
            y=(samp-1)*sinth+center(1)
            x=(samp-1)*costh+center(2)
            if(interp)then ! interpolate
              j=y
              i=x
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                top=buf(i+1,j)*(x-i)+buf(i,j)*(i+1.-x)
                bot=buf(i+1,j+1)*(x-i)+buf(i,j+1)*(i+1.-x)
                obuf(samp)=top*(j+1.-y)+bot*(y-j)
              else
                obuf(samp)=0
              endif
            else ! no interpolate
              j=nint(y)
              i=nint(x)
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                obuf(samp)=buf(i,j)
              else
                obuf(samp)=0
              endif
            endif
          enddo
          call xvwrit(ounit,obuf,status,' ')
        enddo
      endif

c create cartesian image
      if(xvptst('inverse'))then
        do i=1,ns
          buf(i,nl+1)=buf(i,1)  ! copy first line at end
        enddo
        nl1=nl
        ns1=ns-1
        degpline=2.0*pi/nl
        do line=1,nlo
          do samp=1,nso
            radius=sqrt((line-center(1))**2+(samp-center(2))**2)
            if(radius.gt..001)then
              theta=atan2(line-center(1),samp-center(2))
            else
              theta=pi
            endif
            if(theta.lt.0.0)theta=2.0*pi+theta
            y=theta/degpline+1.0
            x=radius+1.0
            if(interp)then ! interpolate
              j=y
              i=x
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                top=buf(i+1,j)*(x-i)+buf(i,j)*(i+1.-x)
                bot=buf(i+1,j+1)*(x-i)+buf(i,j+1)*(i+1.-x)
                obuf(samp)=top*(j+1.-y)+bot*(y-j)
              else
                obuf(samp)=0
              endif
            else  ! no interpolate
              j=nint(y)
              i=nint(x)
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                obuf(samp)=buf(i,j)
              else
                obuf(samp)=0
              endif
            endif
          enddo
          call xvwrit(ounit,obuf,status,' ')
        enddo
      endif

      return
      end
     
