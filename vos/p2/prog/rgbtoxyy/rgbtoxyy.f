c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit NONE

      integer maxin,maxcolors,mapxy
      parameter (maxin=10,maxcolors=10,mapxy=256)

c maxin=     the largest number of input images
c maxcolors= the largest number of special colors
c mapxy=     the nl and ns of the 4th output chromaticity map.
c
c WARNING: change array sizes in DLSQP if you change MAXIN or MAXCOLORS

      character*5 project
      character*80 msg

      integer*4 def,colors(maxcolors),filter(maxin),status
      integer*4 unit(maxin),camera,fds,label_data(80),ncolors,nconv
      integer*4 ounit(3),iproject
      integer*4 nli(maxin),nsi(maxin),nin,nout
      integer*4 nioverf,nfilter,nxyz,nres,i,ns,nl,ind,j,k
      integer*4 nequations,nunknowns,maxhist
      integer*4 n,l,m
      integer*4 lbounds,ii

      integer*2 xybuf(mapxy,mapxy)

      real*4 buf(8000,maxin)
      real*4 conv(maxin),ioverf(maxin)
      real*4 special_response(maxin,maxcolors)
      real*4 special_XYZ(3,maxcolors),solution(maxin,3),xyunit,xy_slope
      real*4 units(maxin),storage(maxin*maxcolors)
      real*4 radiance_XYZ(3,3),xy_offset
      real*4 reflectance_response(0:7,3),radiance_response(0:7,3)
      real*4 xy_sl,dnscale
      real*4 tristim_X,tristim_Y,tristim_Z,sum,x,y

      real*8 c(maxcolors*2,maxin),cl(maxcolors*2),soln(maxin,3)
      real*8 resid(maxcolors*2),err,err_soln(maxin),wts(maxcolors*2)

      logical radiance,xvptst,print

     
c radiance tristimulus values for special colors
      data radiance_XYZ/
c my table
     + 24.72,24.44,21.28, ! x=.351 y=.347 desert 1
     + 8.131,8.114,11.274, ! x=.295 y=.295 water  2
     + 75.15,77.36,80.00/ ! x=.323 y=.332 cloud  3
c klaasen table
c     + 14.32,14.16,12.33, ! x=.351 y=.347 desert 1 
c     + 4.711,4.701,6.532, ! x=.295 y=.295 water  2
c     + 43.54,44.82,46.35/ ! x=.323 y=.332 cloud  3

c dn * I/F  (10000 means 100% reflectance ) for special colors
c filter#1=green filter#2=red filter#3=blue
      data reflectance_response/            ! filters 0-7
c my table
c     + 0.,3558.,4309.,3725.,0.,0.,0.,0.,        ! desert 1
c     + 0.,1362.,834.,3697.,0.,0.,0.,0.,         ! water 2
c     + 0.,5893.,5921.,6450.,0.,0.,0.,0./        ! cloud 3
c klaasen table
     + 2530.,2280.,2750.,1970.,2400.,2880.,2660.,3200.,! desert 1
     + 721.,745.,667.,1260.,559.,520.,608.,676.,       ! water  2
     + 7110.,7220.,7110.,7240.,7100.,6690.,7080.,6600./! cloud 3

c dn * CONV ( nanowatts/cm**2 ster nano ) for special colors
      data radiance_response/    ! filters 0-7
c my table
c     + 0.,17671.,21398.,18499.,0.,0.,0.,0.,     ! desert 1
c     + 0.,6762.,4140.,17802.,0.,0.,0.,0.,       ! water 2
c     + 0.,29267.,29405.,32028.,0.,0.,0.,0./     ! cloud 3
c klaasen table
     + 12100.,13400.,13500.,9820.,9540.,7150.,12200.,9800.,! desert 1
     + 3440.,4380.,3270.,6260.,2220.,1290.,2560.,2080.,    ! water 2
     + 33900.,42500.,34900.,36100.,28200.,16600.,29800.,20300./! cloud 3


      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.lt.3)then
         call mabend('Require at least 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.lt.3)then
         call mabend('Require 3 outputs')
      endif
      call xvparm('COLORS',colors,ncolors,def,maxcolors)
      if(ncolors.lt.nin.and.ncolors.gt.0)then
         call mabend('#special colors >= #inputs')
      endif
      call xvparm('CONV',conv,nconv,def,maxin)
      call xvparm('IOVF',ioverf,nioverf,def,maxin)
      call xvparm('FILTER',filter,nfilter,def,maxin)
      call xvparm('XYZ',special_XYZ,nxyz,def,3*maxcolors)
      call xvparm('RESPONSE',storage,nres,def,maxin*maxcolors)
      call xvparm('PROJECT',project,i,def,1)
      call xvparm('DNSCALE',dnscale,i,def,1)
      radiance=xvptst('RADIANCE')
      print=xvptst('PRINT')

c determine # special colors
      if(ncolors.eq.0)then
        if(nfilter.gt.0) ncolors=nfilter
        if(nxyz.gt.0)    ncolors=nxyz/3
        if(nres.gt.0)    ncolors=nres/nin
        if(ncolors.eq.0)then
          call xvmessage('Must specify either special colors or',' ')
          call xvmessage('XYZ and RESPONSE keywords.',' ')
          call abend
        endif
      endif

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nli(i),'NS',nsi(i),' ')
      enddo

c determine output image size
      ns=nsi(1)
      nl=nli(1)
      do i=2,nin
         nl=min(nl,nli(i))
         ns=min(ns,nsi(i))
      enddo

c extract project ID and label information
      if(project.eq.'NONE')goto 102
      iproject=0
      if(project.eq.'LABEL') iproject=1
      do i=1,nin
        if(iproject.eq.1)then
          call getproj(unit(i),project,camera,fds,ind)
        endif
        call getlabcon(unit(i),project,label_data,ind)
        if(ind.le.1)then
          if(i.gt.nfilter) filter(i)=label_data(4)        
          if(i.gt.nconv) conv(i)=float(label_data(29)) * 1.0e+12
          if(i.gt.nioverf) ioverf(i)=float(label_data(28)) * 1.0e+4  
        else
          if(i.gt.nfilter) filter(i)=-1        
          if(i.gt.nconv) conv(i)=-1.
          if(i.gt.nioverf) ioverf(i)=-1.  
        endif
      enddo
102   continue

c print status
      call xvmessage(' ',' ')
      call xvmessage('input filter        conv        ioverf',' ')
      do i=1,nin
        write(msg,100) i,filter(i),conv(i),ioverf(i)
100     format(3x,i2,5x,i2,8x,e10.3,e10.3)
        call xvmessage(msg,' ')
      enddo

c check filter and color
      do i=1,ncolors
        if(colors(i).lt.1.or.colors(i).gt.3)then
          call xvmessage('Valid colors are 1,2,3 ',' ')
          call abend
        endif
      enddo
      do i=1,nin
        if(filter(i).lt.1.or.filter(i).gt.3)then
          call xvmessage('Valid filters are 1,2,3 ',' ')
          call abend
        endif
      enddo


c extract the XYZ values to be used in the fit
      if(nxyz.eq.0)then
        do i=1,ncolors
          do j=1,3
            special_XYZ(j,i)=radiance_XYZ(j,colors(i))
          enddo
        enddo        
      endif

c extract the response values to be used in the fit
      if(nres.eq.0)then
        do i=1,ncolors
          do j=1,nin
            if(radiance)then
              special_response(j,i)=
     +                    radiance_response(filter(j),colors(i))
            else
              special_response(j,i)=
     +                    reflectance_response(filter(j),colors(i))
            endif
          enddo
        enddo        
      else
        k=0
        do i=1,ncolors
          do j=1,nin
            k=k+1
            special_response(j,i)=storage(k)
          enddo
        enddo
      endif


c extract the scaling values to use (I/F or CONV).
      do i=1,nin
        if(radiance)then
          units(i)=conv(i)*dnscale
        else
          units(i)=ioverf(i)*dnscale
        endif
      enddo


c set up and solve the least squares problem relating camera response 
c through each of the input filters to special color XYZ tristimulus
c values. The equations are like:
c  X(color1)=Ax*response(filter1)+Bx*response(filter2)+...
c  X(color2)=Ax*response(filter1)+Bx*response(filter2)+...
c  X.....................................................
c solve for Ax,Bx...
c  Y(color1)=Ay*response(filter1)+By*response(filter2)+...
c  Y(color2)=Ay*response(filter1)+By*response(filter2)+...
c  Y.....................................................
c solve for Ay,By...
c  the same for Z
      
      nequations=ncolors
      nunknowns=nin
      if(nequations.lt.nunknowns)then
        call mabend('#special colors must be > #inputs')
      else if(nequations.eq.nunknowns)then
        nequations=2*nequations     ! to trick DLSQP
      endif

c     Solve for X coefficients   soln(1-nin,1)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(1,ii)
        do j=1,nunknowns
          c(i,j)=special_response(j,ii)
        enddo
        wts(i)=1.d0
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,1),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in X tristimulus fit')
      endif
      if(print)then
        call xvmessage(' ',' ')
        call
     *   xvmessage('Solution for DN to Tristimulus transformation.',' ')
        call xvmessage('X solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,1),err_soln(i)
101       format(4x,i2,6x,d11.4,4x,d11.4)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c     Solve for Y coefficients   soln(1-nin,2)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(2,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,2),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Y tristimulus fit')
      endif
      if(print)then
        call xvmessage('Y solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,2),err_soln(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c     Solve for Z coefficients   soln(1-nin,3)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(3,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,3),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Z tristimulus fit')
      endif
      if(print)then
        call xvmessage('Z solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,3),err_soln(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c convert solutions from real*8 to real*4
      do j=1,3
        do i=1,nin
          solution(i,j)=soln(i,j)
        enddo
      enddo


c open outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c prepare 4th output
      if(nout.eq.4)then
        call xvunit(xyunit,'OUT',4,status,' ')
        call xvopen(xyunit,status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',mapxy,'U_NS',mapxy,'OP','WRITE',' ')
        xy_slope=mapxy/1.2
        xy_offset=mapxy- xy_slope*1.1
	do i=1,mapxy
	  do j=1,mapxy
	    xybuf(i,j)=100
	  enddo
	enddo
c        call mve(2,mapxy*mapxy,100,xybuf,0,1)  ! set buffer to 100
        k=nint(xy_offset)
        l=mapxy- nint(xy_offset) +1
        do i=1,mapxy
          xybuf(k,i)=0 ! draw y axis
          xybuf(i,l)=0 ! draw x axis
          xybuf(i,i)=0 ! draw diagonal
        enddo
      endif

c process the image**************************************************

      lbounds=0
      do j=1,nl                         ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(k),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns                       ! pixel loop

c         Compute tristimulus values from coefficients.
          tristim_X=0.
          tristim_Y=0.
          tristim_Z=0.
          do n=1,nin                    ! picture loop
            tristim_X = tristim_X + solution(n,1)*buf(i,n)*units(n)
            tristim_Y = tristim_Y + solution(n,2)*buf(i,n)*units(n)
            tristim_Z = tristim_Z + solution(n,3)*buf(i,n)*units(n)
          enddo
          
c         Convert to Y,x,y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         update the 4th output image if desired
          if(nout.eq.4)then
            l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
            m=nint(x*xy_slope+xy_offset)           ! sample
            if(l.lt.1) l=1
            if(m.lt.1) m=1
            if(l.gt.mapxy) l=mapxy
            if(m.gt.mapxy) m=mapxy
            if(xybuf(m,l).lt.32767) xybuf(m,l)=xybuf(m,l)+1
          endif

c         check for negative luminance
          if(tristim_Y.lt.0.0)then
            lbounds=lbounds+1
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=0.
          else
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=tristim_Y
          endif


        enddo   ! pixel loop      

        do k=1,3             ! picture loop
          call xvwrit(ounit(k),buf(1,k),status,' ')
        enddo

      enddo     ! line loop

      call xvmessage(' ',' ')
      call prnt(4,1,lbounds,'number of negative luminance pixels=.')

c stretch and write out the 4th output image
      if(nout.eq.4)then
c       stretch xybuf
        maxhist=100
        do l=2,mapxy-1
          do m=2,mapxy-1
            if(xybuf(m,l).gt.maxhist) maxhist=xybuf(m,l)
          enddo
        enddo
        if(maxhist.gt.100)then
          xy_sl=155./(maxhist-100)
          do l=1,mapxy
            do m=1,mapxy
              if(xybuf(m,l).gt.100)then
                k=(xybuf(m,l)-100)*xy_sl +100.5
                if(k.gt.255) k=255
                xybuf(m,l)=k
              endif
            enddo
          enddo
        endif                    

        do l=1,mapxy             ! picture loop
          call xvwrit(xyunit,xybuf(1,l),status,' ')
        enddo
      endif

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
c      parameter (s=8,ss=20)
      parameter (s=10,ss=20)
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
      DO 160 I=1,NU
        IF ( Q(I,I) .GE. 0.D0 ) THEN
          EX(I)=E*DSQRT(Q(I,I))
        ELSE
          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
        END IF
160   CONTINUE      
      RETURN
      END


