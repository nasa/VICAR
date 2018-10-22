c
c program mapiso
c
      include 'VICMAIN_FOR'
      subroutine main44
      include 'mp_for_defs'

      integer*2 obuf(1002,502)
      integer*4 ounit,def,count
      integer*4 status,nl,ns,samp
      real*8 mp
      integer*4 idata(40)
      real*4 data(40)
      real*8 a,b,r2d,d2r,x,y,t,x_center,y_center
      equivalence (data,idata)

c parameters
      call xvparm('NL',nl,count,def,1)
      call xvparm('NS',ns,count,def,1)
      a=(ns-2)/2.d0
      b=(nl-2)/2.d0
      r2d=45.d0/datan(1.d0)
      d2r=1.d0/r2d
      x_center=ns/2.0
      y_center=nl/2.0

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvsignal(ounit,status,1)
      call xvopen(ounit,status,'U_FORMAT','HALF',
     +        'U_NL',nl,'U_NS',ns,'OP','WRITE',
     +        'O_FORMAT','BYTE',' ')
      call xvsignal(ounit,status,1)

c initialize mp routines
      call mp_init( mp,istat) 
      if(istat.ne.mp_success) call mabend('error in mp_init')
c write mp label
      data(25)=b         !r pole in km
      data(26)=a         !r equator in km
      data(31)=0.0       !sclat
      data(32)=0.0       !sclon
      data(33)=y_center  !centline
      data(34)=x_center  !centsamp
      data(38)=1000.*b   !range in km
      data(27)=1000.*b   !focal in pixels or mm
      data(30)=1.0       !scale
      data(35)=0.0       !north
      data(28)=y_center  !oaline
      data(29)=x_center  !oasamp
      idata(39)=16       ! perspective type
      call mp_buf2mpo( idata, mp, status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP error on buffer transfer',0)
          return
      end if
      call mp_label_write( mp, ounit, 'HISTORY', status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP label write error',0)
          return
      end if
      call mp_label_write( mp, ounit, 'PROPERTY', status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP label write error',0)
          return
      end if
 
c process image

c set background.
      do line=1,nl                         ! line loop
        do samp=1,ns                          ! pixel loop
          obuf(samp,line)=1
        enddo
      enddo
  
c draw limb.
      dangle=(1.d0/a)*r2d
      do angle=0,89.999,dangle
        t=tan(angle*d2r)
        x=a*b*dsqrt(1.d0/(b*b+a*a*t*t))
        y=x*t
        i=nint(x + x_center)
        j=nint(y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(-x + x_center)
        j=nint(y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(x + x_center)
        j=nint(-y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(-x + x_center)
        j=nint(-y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
      enddo

c draw latitudes.
      dangle=10.
      do angle=0,89.999,dangle
        t=tan(angle*d2r)
        x=a*b*dsqrt(1.d0/(b*b+a*a*t*t))
        y=x*t
        i1=nint(-x + x_center)
        i2=nint(x + x_center)
        j1=nint(-y + y_center)
        j2=nint(y + y_center)
        if(i1.lt.1)i1=1
        if(i2.gt.ns)i2=ns
        if(j1.ge.1.and.j1.le.nl)then
          do i=i1,i2
            obuf(i,j1)=255
          enddo
        endif
        if(j2.ge.1.and.j2.le.nl)then
          do i=i1,i2
            obuf(i,j2)=255
          enddo
        endif
      enddo

c write image
      do line=1,nl                         ! line loop
        call xvwrit(ounit,obuf(1,line),status,' ')
        call xvsignal(ounit,status,1)
      enddo

      return
      end
