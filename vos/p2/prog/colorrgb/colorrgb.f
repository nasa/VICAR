c colorrgb
      include 'VICMAIN_FOR'
      subroutine main44
      parameter (maxns=30000,maxpix=10,maxcolors=30)

      integer*4 inunit(maxpix),outunit(3),def,nl,ns,status
      integer*4 line(maxcolors),samp(maxcolors)
      real*4 buf(maxns,maxpix),obuf(maxns,3)
      real*4 red_target(maxcolors),green_target(maxcolors)
      real*4 rpar(maxcolors*5),blue_target(maxcolors)
      real*4 dn(maxpix,maxcolors)
      real*8 c(maxcolors,maxpix+1),cl(maxcolors),wts(maxcolors)
      real*8 red_coef(maxpix+1),green_coef(maxpix+1)
      real*8 blue_coef(maxpix+1),r,g,b

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      call xvparm('COLOR',rpar,npar,def,300)
      call xvparm('AREA',nlw,n,def,maxcolors)
      if(npar.gt.0)then
        ncolors=npar/5
        if(ncolors.lt.nin+2)then
          write(*,*)'Requires at least ',nin+2,' colors'
          call abend()
        endif
        if(ncolors*5.ne.npar)then
          write(*,*)'Incorrect number of color parameters'
          call abend()
        endif
        write(*,*)'Data from parameters:'
        write(*,*)' color line  sample    red    green      blue'
        k=1
        do i=1,ncolors
          line(i)=nint(rpar(k))
          k=k+1
          samp(i)=nint(rpar(k))
          k=k+1
          red_target(i)=rpar(k)
          k=k+1
          green_target(i)=rpar(k)
          k=k+1
          blue_target(i)=rpar(k)
          k=k+1
          write(*,10)i,line(i),samp(i),red_target(i),green_target(i),
     +      blue_target(i)
        enddo
10      format(3i6,3f10.2)
      else
        ncolors=0
      endif

c open all inputs
      do i=1,nin
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        if(i.eq.1)then
          call xvget(inunit(i),status,'NL',nl,'NS',ns,' ')
          if(ns.gt.maxns)then
            call xvmessage('Input image line too long',' ')
            call abend
          endif
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(outunit(i),'OUT',i,status,' ')
        call xvopen(outunit(i),status,'O_FORMAT','BYTE',
     +    'U_FORMAT','REAL','OP','WRITE',' ')
      enddo

c read cal file
      if(ncolors.eq.0)then
        write(*,*)'Reading calibration file'
        nu=nin+1
        call read_calibration_file(red_coef,green_coef,blue_coef,nin+1)
        goto 50
      endif

c get the input dn's
      write(*,*)' color  Dn values from the images -->'
      do i=1,ncolors
        do j=1,nin
          sum=0.0
          isum=0
          do m=line(i)-nlw/2,line(i)+nlw/2
            call xvread(inunit(j),buf(1,j),status,'LINE',m,' ')
            do n=samp(i)-nlw/2,samp(i)+nlw/2
              isum=isum+1
              sum=sum+buf(n,j)
            enddo
          enddo
          dn(j,i)=sum/isum
        enddo
        write(*,11)i,(dn(j,i),j=1,nin)
      enddo
11    format(i6,11f9.1)
        
c solve for the least squares relation between input and target color.
c target_color=c1*dn1+c2*dn2+...+cn*dnn+c
      ne=ncolors
      nu=nin+1
      do i=1,ncolors
        do j=1,nin
          c(i,j)=dn(j,i)
          cl(i)=red_target(i)
          wts(i)=1.d0
        enddo
        c(i,nu)=1.d0
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,red_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for red'
        stop
      endif
      do i=1,ncolors
        cl(i)=green_target(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,green_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for green'
        stop
      endif
      do i=1,ncolors
        cl(i)=blue_target(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,blue_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for blue'
        stop
      endif

c write results
      write(*,*)'Writing calibration file'
      call write_calibration_file(red_coef,green_coef,blue_coef,nin+1)

50    continue
c create output r g b.
      do j=1,nl
        do k=1,nin
          call xvread(inunit(k),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns
          r=red_coef(nu)
          g=green_coef(nu)
          b=blue_coef(nu)
          do k=1,nin
            r=r+red_coef(k)*buf(i,k)
            g=g+green_coef(k)*buf(i,k)
            b=b+blue_coef(k)*buf(i,k)
          enddo
          if(r.lt.0.0)r=0.0
          if(r.gt.255.)r=255.
          if(g.lt.0.0)g=0.0
          if(g.gt.255.)g=255.
          if(b.lt.0.0)b=0.0
          if(b.gt.255.)b=255.
          obuf(i,1)=r
          obuf(i,2)=g
          obuf(i,3)=b
        enddo
        do k=1,3
          call xvwrit(outunit(k),obuf(1,k),status,'LINE',j,' ')
        enddo
      enddo

      return
      end
     
c***********************************************************************
      subroutine read_calibration_file(
     + coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        write(*,*)'cannot open calibration file:'
        write(*,*)'color_calibration.txt'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      close(unit=10)
      return
      end

c***********************************************************************
      subroutine write_calibration_file(
     +   coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        write(*,*)'cannot open calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      close(unit=10)
      return
      end

c***********************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1,len)
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
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      parameter (id=11)
      REAL*8  A(id,id),AL(id),R(id,id),RL(id)
      real*8 Q(id,id),X(id),SUM
      REAL*8 C(len,id),CL(len),X1(id),wts(len)

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
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
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
      RETURN
999   ind=1
      return
      END                                                                       


