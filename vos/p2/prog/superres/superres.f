c
c program superres
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=10000,maxpix=50,maxns=256,maxnl=256,maxtpts=9)
      real*4 obuf(maxsamp)
      integer*2 inbuf(maxns,maxnl,maxpix)
      integer*4 unit(maxpix),nl(maxpix),ns(maxpix),pixel(2,4)
      integer*4 samp,status,count,def,ounit,ntpts(maxpix)
      real*4 s_out,l_out,intensity(maxpix),residuals(maxpix)
      real*8 sum,xcoef(4,maxpix),ycoef(4,maxpix)
      real*8 ixcoef(4,maxpix),iycoef(4,maxpix)
      real*8 c(10,4),cl(10),wts(10),c1,c2 
      real*4 tpts(5,9,maxpix)
      character*100 tiepoints,listoffiles,filenames,data
      
c parameters
      call xveaction('SA','error')
      call xvpcnt('INP',nids)      
      call xvparm('SCALE',scale,count,def,1)
      call xvparm('RESOLUTION',resolution,count,def,1)
      resolution=resolution*resolution
      call xvparm('MODE',mode,count,def,1)

c read listoffiles & open inputs.
      call xvpone('INP',listoffiles,1,100)
      open(unit=10,file=listoffiles,status='old',
     + iostat=i,access='sequential')
      if(i.gt.0)then
        write(*,*)'Error opening listoffiles file ',listoffiles
        call abend()
      endif
      do image=1,maxpix
        read(10,30,iostat=i)filenames
30      format(A100)
        if(i.gt.0)then
          call xvmessage('read error on listoffiles',' ')
          call abend()
        endif
        if(i.lt.0)  goto 10
        call xvunit(unit(image),'OLD',image,status,
     +   'U_NAME',filenames,' ')
        call xvopen(unit(image),status,'U_FORMAT','HALF',' ')
        call xvget(unit(image),status,'NL',nl(image),
     +             'NS',ns(image),' ')
       if(ns(image).gt.maxns)then
          call xvmessage('Input line length too big',' ')
          call abend()
        endif
        if(nl(image).gt.maxnl)then
          call xvmessage('Too many input lines',' ')
          call abend()
        endif
      enddo
10    nids=image-1
      close(10)
      write(*,*)nids,' input files located'


c read tiepoints file
      call xvpone('INP',tiepoints,2,100)
      open(unit=10,file=tiepoints,status='old',
     + iostat=i,access='sequential')
      if(i.gt.0)then
        write(*,*)'Error opening tiepoints file ',tiepoints
        call abend()
      endif
      k=0
11    read(10,30,iostat=i)data
      if(i.ne.0)then
        call xvmessage('Error reading tiepoints file',' ')
        call abend()
      endif
      if(data(1:7).eq.'Picture')then
        k=k+1
        if(k.gt.maxpix)then
          write(*,*)'Read too many tiepoint sets'
          call abend()
        endif
        j=0
        goto 11
      endif
      if(data(1:3).eq.'end')goto 12
      j=j+1
      if(j.gt.maxtpts)then
        write(*,*)'Read too many tiepoints for image ',k+1
        call abend()
      endif
      read(data,*)(tpts(n,j,k),n=1,5)
c     write(*,*)j,k,(tpts(n,j,k),n=1,5)
      ntpts(k)=j
      goto 11
12    if(k.ne.nids-1)then
        write(*,*)'Unexpected # tiepoint sets.'
        write(*,*)'Read ',k,' expected ',nids-1
        call abend()
      endif     
      close(10)
      
c Compute polynomials mapping first image to all the others.
c tpts(1,*)=reference line
c tpts(2,*)=reference sample
c tpts(3,*)=line
c tpts(4,*)=samp
c tpts(5,*)=quality
c line=ycoef(1)*refline+ycoef(2)*refsamp+ycoef(3)
c samp=xcoef(1)*refline+xcoef(2)*refsamp+xcoef(3)

      if(mode.eq.2)then            ! affine polynomial
        do image=1,nids-1
          do i=1,ntpts(image)
            c(i,1)=tpts(1,i,image)
            c(i,2)=tpts(2,i,image)
            c(i,3)=1.d0
            cl(i)=tpts(3,i,image)
            wts(i)=tpts(5,i,image)
          enddo
          call lsqp(ind,ntpts(image),3,c,cl,wts,ycoef(1,image+1))
          if(ind.ne.0)write(*,*)'LSQP error'
          do i=1,ntpts(image)
            c(i,1)=tpts(1,i,image)
            c(i,2)=tpts(2,i,image)
            c(i,3)=1.d0
            cl(i)=tpts(4,i,image)
            wts(i)=tpts(5,i,image)
          enddo
          call lsqp(ind,ntpts(image),3,c,cl,wts,xcoef(1,image+1))
          if(ind.ne.0)write(*,*)'LSQP error'
        enddo
      endif

      if(mode.eq.1)then            ! offset only
        do image=1,nids-1
          dy=0.
          dx=0.
          do i=1,ntpts(image)
            dy=dy+tpts(3,i,image)-tpts(1,i,image)
            dx=dx+tpts(4,i,image)-tpts(2,i,image)
          enddo
          dx=dx/ntpts(image)
          dy=dy/ntpts(image)
          xcoef(1,image+1)=0.d0
          xcoef(2,image+1)=1.d0
          xcoef(3,image+1)=dx
          ycoef(1,image+1)=1.d0
          ycoef(2,image+1)=0.d0
          ycoef(3,image+1)=dy
        enddo
      endif

      xcoef(1,1)=0.d0 ! reference image to itself.
      xcoef(2,1)=1.d0
      xcoef(3,1)=0.d0
      ycoef(1,1)=1.d0
      ycoef(2,1)=0.d0
      ycoef(3,1)=0.d0

c check least squares for precision.
      do image=2,nids
        sum=0.d0
        do i=1,ntpts(image-1)
          y1=tpts(1,i,image-1)
          x1=tpts(2,i,image-1)
          y=ycoef(1,image)*y1+ycoef(2,image)*x1+ycoef(3,image)
          x=xcoef(1,image)*y1+xcoef(2,image)*x1+xcoef(3,image)
          dy=tpts(3,i,image-1)-y
          dx=tpts(4,i,image-1)-x
          sum=sum+sqrt(dy*dy+dx*dx)
        enddo
        residuals(image)=sum/ntpts(image-1)
      enddo
      residuals(1)=0.
      
c invert the polynomials
      do image=1,nids
        c1=ycoef(1,image)-ycoef(2,image)*xcoef(1,image)/xcoef(2,image)
        c2=ycoef(3,image)-ycoef(2,image)*xcoef(3,image)/xcoef(2,image)
        iycoef(1,image)=1.d0/c1
        iycoef(2,image)=-ycoef(2,image)/(xcoef(2,image)*c1)
        iycoef(3,image)=-c2/c1
        ixcoef(1,image)=-xcoef(1,image)/xcoef(2,image)
        ixcoef(2,image)=1.d0/xcoef(2,image)
        ixcoef(3,image)=-xcoef(3,image)/xcoef(2,image)
      enddo

c read inputs into memory
      do image=1,nids
        do j=1,nl(image)
          call xvread(unit(image),inbuf(1,j,image),status,' ')
        enddo
      enddo
      
c determine size of output as largest input * scale
      nlo=0
      nso=0
      do image=1,nids
        nlo=max(nlo,int(scale*nl(image)))
        nso=max(nso,int(scale*ns(image)))
      enddo
      if(nso.gt.maxsamp)then
        call xvmessage('Output line length too big',' ')
        call abend()
      endif
            
c determine intensities of each input
      do image=1,nids
        sum=0.d0
        count=0
        do j=1,nl(1)
          do i=1,ns(1)
            iy=nint(ycoef(1,image)*j+ycoef(2,image)*i+ycoef(3,image))
            ix=nint(xcoef(1,image)*j+xcoef(2,image)*i+xcoef(3,image))
            if((ix.ge.1).and.(ix.le.ns(image)).and.(iy.ge.1).and.
     +         (iy.le.nl(image)))then
              count=count+1
              sum=sum+inbuf(ix,iy,image)
            endif
          enddo
          count=count+k
        enddo
        intensity(image)=sum/count
      enddo
      do image=nids,1,-1
        intensity(image)=intensity(1)/intensity(image)
      enddo
      
c print out status
      call xvmessage('   least squares   intensity',' ')
      call xvmessage('   residuals       correction',' ')
      do image=1,nids
        write(*,15)residuals(image),intensity(image)
15      format(2f13.6)
      enddo  
      
c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +   'O_FORMAT','HALF','U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        
c compute offset relating output to reference input
      offset=1.0-(scale+1.0)/(2.0*scale)
      offset=0.
      
c process image data.
c input picture 1 is the reference image.
      do line=1,nlo                     ! output line 
        do samp=1,nso                   ! output sample

c         convert from output to location in first input.
          y1=line/scale+offset          ! reference input line
          x1=samp/scale+offset          ! reference input sample
          k=0
          sum1=0.0
          sum2=0.0
          
          do image=1,nids               ! input image

c           get sub pixel location in the input images.
            y=ycoef(1,image)*y1+ycoef(2,image)*x1+ycoef(3,image)
            x=xcoef(1,image)*y1+xcoef(2,image)*x1+xcoef(3,image)

c           get 4 nearest input pixels.
            ix=x
            iy=y
            pixel(1,1)=iy
            pixel(2,1)=ix
            pixel(1,2)=iy+1
            pixel(2,2)=ix
            pixel(1,3)=iy
            pixel(2,3)=ix+1
            pixel(1,4)=iy+1
            pixel(2,4)=ix+1

            do kk=1,4
              iy=pixel(1,kk)
              ix=pixel(2,kk)
              if((ix.ge.1).and.(ix.le.ns(image)).and.(iy.ge.1).and.
     +           (iy.le.nl(image)))then
     
                dnin=inbuf(ix,iy,image)*intensity(image)  ! input dn's

c               get sub pixel location of nearest pixel in first input.
c               the lone y below is not a syntax error !
                y=iycoef(1,image)*iy+iycoef(2,image)*ix+iycoef(3,image)
                x=ixcoef(1,image)*y+ixcoef(2,image)*ix+ixcoef(3,image)

c               get output pixel location.
                s_out=scale*(x-offset)    ! output sub pixel samp
                l_out=scale*(y-offset)    ! output sub pixel line
                r=(s_out-samp)**2+(l_out-line)**2 ! range
                if(r.lt.resolution)r=resolution
                sum1=sum1+dnin/r
                sum2=sum2+1.0/r
                k=k+1
              endif
            enddo
          enddo
          if(k.gt.0)then
            obuf(samp)=sum1/sum2
          else
            obuf(samp)=0.0
          endif
        enddo
        call xvwrit(ounit,obuf,status,' ')       ! output line  

      enddo      
      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
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
      REAL*8  A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SUM
      REAL*8 C(10,4),CL(10),X1(4),wts(10)
      
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

