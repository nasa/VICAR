c
c program dntoxyy
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxns=30000, maxpix=10, maxcolors=50)

      integer*4 inunit(maxpix),outunit(3),def,nl,ns,status
      integer*4 wtcount,colorcount,maccount,line,samp
      real*4 inbuf(maxns,maxpix),data(500)
      real*4 xcolor(maxcolors),ycolor(maxcolors),yycolor(maxcolors)
      real*4 xxcolor(maxcolors),zzcolor(maxcolors)
      real*4 dn(maxpix,maxcolors)
      real*4 x(maxns),y(maxns),yy(maxns),macloc(2,4)
      real*8 c(maxcolors,maxpix+1),cl(maxcolors),wts(maxcolors)
      real*8 coefx(maxpix+1),coefy(maxpix+1),coefz(maxpix+1)
      real*8 wts2(maxcolors)
      REAL*8  A(maxpix+1,maxpix+1),AL(maxpix+1)
      REAL*8 R(maxpix+1,maxpix+1),RL(maxpix+1)
      REAL*8 Q(maxpix+1,maxpix+1),LSQPX(maxpix+1)

c macbeth D65 color table measured 4/25/2001 w. Spectrolina at MIPL.
c data are ordered in rows top left to bottom right with gray row
c on bottom as last row. data are in x,y,Y for each of 24 patches.
      real*4 macbeth(3,24)/
     + .4024,.357,9.98,
     + .3841,.3556,36.3088,
     + .253,.2706,19.4492,
     + .3424,.4355,12.7141,
     + .2732,.2638, 25.0737, 
     + .2683,.366,43.5551,
     + .4986,.4054,28.6264,
     + .2196,.204,12.7054,
     + .4493,.3143,19.7654,
     + .2914,.2278,7.2636,
     + .383,.485,40.4741,
     + .4764,.4377,41.9126,
     + .1955,.1607,7.3632,
     + .3099,.4772,23.75,
     + .5281,.3161,12.0682,
     + .4533,.4743,59.4811,
     + .3659,.2504,20.0266,
     + .2043,.2824,20.8024,
     + .3221,.342,88.8639,
     + .317,.3348,59.9815,
     + .3158,.3335,35.9724,
     + .3168,.3335,20.5227,
     + .3159,.3323,9.691,
     + .3126,.328,3.4519/

c coordinates of macbeth color patches in same order as above in
c the order sample,line.
      real*4 macbeth_coord(2,24)/
     + 1.,1.,  2.,1.,  3.,1.,  4.,1.,  5.,1.,  6.,1.,
     + 1.,2.,  2.,2.,  3.,2.,  4.,2.,  5.,2.,  6.,2.,
     + 1.,3.,  2.,3.,  3.,3.,  4.,3.,  5.,3.,  6.,3.,
     + 1.,4.,  2.,4.,  3.,4.,  4.,4.,  5.,4.,  6.,4./

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nids)
      call xvparm('COLORS',data,colorcount,def,500)
      call xvparmd('WEIGHTS',wts,wtcount,def,maxcolors)
      call xvparm('MACBETH',macloc,maccount,def,8)

c open all inputs
      do i=1,nids
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
        call xvopen(outunit(i),status,'O_FORMAT','REAL',
     +    'U_FORMAT','REAL','OP','WRITE',' ')
      enddo

c Optionally read calibration file & bypass it's computation if no
c parameters are input.
      if(colorcount+maccount.eq.0)then
        call xvmessage('reading color_calibration.txt',' ')
        call read_calibration_file(
     +   coefX,coefY,coefZ,nids)
        goto 100
      endif

c sort out the color information from the COLORS keyword.
      if(colorcount.gt.0)then
        ncolors=colorcount/(3+nids)
        if(ncolors*(3+nids).ne.colorcount)then
          write(*,*)'incorrect number of parameters'
          call abend()
        endif
        if(ncolors.le.nids)then
          write(*,*)'colors must exceed input files'
          call abend()
        endif
        write(*,*)'Input data:'
        write(*,*)'      x           y           Y           dn--->'
        k=1
        do i=1,ncolors
          xcolor(i)=data(k)
          ycolor(i)=data(k+1)
          yycolor(i)=data(k+2)
          k=k+3
          do j=1,nids
            dn(j,i)=data(k+j-1)
          enddo
          k=k+nids
          write(*,*)xcolor(i),ycolor(i),yycolor(i),
     +             (dn(j,i),j=1,nids)
        enddo
      endif

c Extract the macbeth dn values from the image.
      if(maccount.gt.0)then
        ncolors=24
        ne=4
        nu=3
c       line solution: cl=c(1)*line+c(2)*samp+c(3)
        c(1,1)=1.d0
        c(1,2)=1.d0
        c(1,3)=1.d0
        cl(1)=macloc(1,1)
        wts2(1)=1.d0
        c(2,1)=1.d0
        c(2,2)=6.d0
        c(2,3)=1.d0
        cl(2)=macloc(1,2)
        wts2(2)=1.d0
        c(3,1)=4.d0
        c(3,2)=1.d0
        c(3,3)=1.d0
        cl(3)=macloc(1,3)
        wts2(3)=1.d0
        c(4,1)=4.d0
        c(4,2)=6.d0
        c(4,3)=1.d0
        cl(4)=macloc(1,4)
        wts2(4)=1.d0
        call lsqp(ind,ne,nu,c,cl,wts2,coefy,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
        if(ind.eq.1)then
          write(*,*)'LSQP: error in y solution for geometry'
          write(*,*)'of the Macbeth target location.'
          call abend()
        endif
c       sample solution: cl=c(1)*line+c(2)*samp+c(3)
        cl(1)=macloc(2,1)
        cl(2)=macloc(2,2)
        cl(3)=macloc(2,3)
        cl(4)=macloc(2,4)
        call lsqp(ind,ne,nu,c,cl,wts2,coefx,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
        if(ind.eq.1)then
          write(*,*)'LSQP: error in x solution for geometry'
          write(*,*)'of the Macbeth target location.'
          call abend()
        endif
c       Extraction of dn's.
        width=sqrt((macloc(1,1)-macloc(1,2))**2+
     +             (macloc(2,1)-macloc(2,2))**2)
        patchsize=width/5.0
        nsw2=patchsize/6.0
        write(*,*)'Averaging patches',2*nsw2+1,' pixels square'
        area=(2*nsw2+1)**2
        write(*,*)'Input data:'
        write(*,*)'      x           y           Y           dn--->'
        do i=1,ncolors
          xcolor(i)=macbeth(1,i)
          ycolor(i)=macbeth(2,i)
          yycolor(i)=macbeth(3,i)
          line=nint(coefy(1)*macbeth_coord(1,i)+
     +         coefy(2)*macbeth_coord(2,i)+coefy(3))
          samp=nint(coefx(1)*macbeth_coord(1,i)+
     +         coefx(2)*macbeth_coord(2,i)+coefx(3))
          if((line.lt.1+nsw2).or.(samp.lt.1+nsw2).or.
     +       (line.gt.nl-nsw2).or.(samp.gt.ns-nsw2))then
             call xvmessage('Macbeth chart outside image',' ')
             call xvmessage('MACBETH keywords in error',' ')
             call abend()
          endif
          do j=1,nids
            sum=0.0
            do k1=line-nsw2,line+nsw2
              call xvread(inunit(j),inbuf(1,1),
     +        status,'LINE',k1,' ')
              do k2=samp-nsw2,samp+nsw2
                sum=sum+inbuf(k2,1)
              enddo
            enddo
            dn(j,i)=sum/area
          enddo
          write(*,*)xcolor(i),ycolor(i),yycolor(i),
     +             (dn(j,i),j=1,nids)
        enddo
      endif

c compute X and Z tristimulus from chromaticities x and y
      write(*,*)'Computed tristimulus colors (X,Y,Z):'
      do i=1,ncolors
        xxcolor(i)=yycolor(i)*xcolor(i)/ycolor(i)
        zzcolor(i)=yycolor(i)/ycolor(i)-xxcolor(i)-yycolor(i)
        write(*,*)'color',i,xxcolor(i),yycolor(i),zzcolor(i)
      enddo

c assign weights & dimensions
      ne=ncolors
c     nu=nids+1
      nu=nids
      if(wtcount.eq.0)then
        do i=1,ncolors
          wts(i)=1.d0
        enddo
      else if(wtcount.ne.ncolors)then
        write(*,*)'Incorrect number of weights'
        write(*,*)'There should be ',ncolors,' weights'
        call abend()
      endif

c solve for coefficients relating dn to X tristumulus
      do i=1,ncolors
        cl(i)=xxcolor(i)
        do j=1,nids
          c(i,j)=dn(j,i)   ! the C terms
        enddo
c       c(i,nids+1)=1.d0   ! the K term
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefx,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in X solution'
        call abend()
      endif

c solve for coefficients relating dn to Y tristumulus
      do i=1,ncolors
        cl(i)=yycolor(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefy,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in Y solution'
        call abend()
      endif

c solve for coefficients relating dn to Z tristumulus
      do i=1,ncolors
        cl(i)=zzcolor(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefz,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in Z solution'
        call abend()
      endif

      call write_calibration_file(coefx,coefy,coefz,nids)

      write(*,*)'Solution for tristimulus X'
      write(*,*)'X=',(coefx(i),i=1,nids)
      write(*,*)'Solution for tristimulus Y'
      write(*,*)'Y=',(coefy(i),i=1,nids)
      write(*,*)'Solution for tristimulus Z'
      write(*,*)'Z=',(coefz(i),i=1,nids)

100   continue

c process images.
      do line=1,nl
        do image=1,nids
          call xvread(inunit(image),inbuf(1,image),
     +      status,'LINE',line,' ')
        enddo

        do i=1,ns
c         trix=coefx(nids+1)
c         triy=coefy(nids+1)
c         triz=coefz(nids+1)
          trix=0.0
          triy=0.0
          triz=0.0
          do j=1,nids
            trix=trix+coefx(j)*inbuf(i,j)
            triy=triy+coefy(j)*inbuf(i,j)
            triz=triz+coefz(j)*inbuf(i,j)
          enddo
          yy(i)=triy
          x(i)=trix/(trix+triy+triz)
          y(i)=triy/(trix+triy+triz)
        enddo

        call xvwrit(outunit(1),x,status,' ')
        call xvwrit(outunit(2),y,status,' ')
        call xvwrit(outunit(3),yy,status,' ')
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
        call xvmessage('cannot open calibration file:',' ')
        call xvmessage('color_calibration.txt',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
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
        call xvmessage('cannot open calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      close(unit=10)
      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1,n,m,
     +  A,AL,R,RL,Q,X)
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
      REAL*8  A(n,n),AL(n),R(n,n),RL(n),Q(n,n),X(n),SUM
      REAL*8 C(m,n),CL(m),X1(n),wts(m)
      
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
 

