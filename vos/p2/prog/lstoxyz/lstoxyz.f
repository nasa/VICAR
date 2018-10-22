c
c program lstoxyz
c
      include 'VICMAIN_FOR'
      subroutine main44

      include 'mp_for_defs'  !needed for MP software
      character*12 planet/'            '/
      character*5 project(2),source(2)
      character*4 csource1,csource2
      integer*4 fdsno,ilab(100,2),scr(1800),buf(200,2),camera(2)
      integer*4 idata(40,4),tpt,inunit(2),count,def
      real*4 data(40,4),image_coords(4,32)
      real*4 lat1,lat2,long1,long2,line1,line2,samp1,samp2
      real*4 xyz(4,32)
      real*8 buf8(100,2),r8,rs_vector(3,2),om_mat(3,3,2)
      real*8 focal1,focal2,scale1,scale2
      real*8 x1p,y1p,x2p,y2p,mat1(3,3),mat2(3,3)
      real*8 x,y,z,raddeg,range,rad,rpole,req
      real*8 lat,long,latrad
      real*8 MP(2)
      logical xvptst,debug/.false./,xyzmode/.false./
      equivalence (csource1,buf(11,1)),(csource2,buf(11,2))
      equivalence(buf,buf8),(r8,idata,data)      

      raddeg=180.0d0/3.141592653589d0

c open files
      call xvunit(inunit(1),'INP',1,status,' ')
      call xvopen(inunit(1),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit(1),status,'NL',nl,'NS',np,
     +  'FORMAT',oformat,' ')

      call xvunit(inunit(2),'INP',2,status,' ')
      call xvopen(inunit(2),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')

      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','REAL','O_FORMAT',
     +            'REAL','OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS','U_NS',128,'U_NL',nl3*32/42,
     +            ' ')

c Get parameters:
      call xvparm('SOURCEL',source(1),count,def,1)
      call xvparm('SOURCER',source(2),count,def,1)
      if(xvptst('DEBUG')) debug=.true.
      if(xvptst('XYZ')) xyzmode=.true.

c Extract the navigation for the input images and put it in DATA.
c DATA(1-40,1) is for the Map projected first input image.
c DATA(1-40,2) is for the Map projected second input image.
c DATA(1-40,3) is for the object space first input image.
c DATA(1-40,4) is for the object space second input image.
      do image=1,2

c        Get project names
         call getproj(inunit(image),project(image),camera(image),
     +                fdsno,ind)
         if(ind.ne.0)then
            call mabend('Unknown flight project')
         endif
      
c        Get label contents
         call getlabcon(inunit(image),project(image),ilab(1,image),
     +                  ind)
         if(ind.gt.1)then
            call mabend('Getlabcon: Fatal error')
         endif

c        Retrieve the MAP labels from the projections for navigating
c        around in projection space.
c         call searcv2(inunit(image),i,%descr(scr),data(1,image),
c     +                data(1,image))
         call mp_init(mp(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_init')
         call mp_label_read(mp(image),inunit(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_read')
         call mp_mpo2buf( mp(image),data(1,image),istat)    !C MP
         if(istat.ne.mp_success) call mabend('error in mpo2buf')
        
c        Check input types
         if((idata(39,image).eq.7).or.(idata(39,image).eq.8))then
            call mabend('Inputs must be map projections')
         endif

c        Retrieve the OM and RS vectors and target radii.
c         if((project(image).eq.'VGR-1').or.
c     +      (project(image).eq.'VGR-2')) planet='            '
c         call getspice(project(image),buf8(1,image),source(image),
c     +                 planet,ilab(1,image),ind)
         call getspice2(inunit(image),.false.,buf8(1,image),ind)
         if(ind.ne.1)then
            write(*,*)'getspice2 indicator:',ind
            call mabend('Getspice2: error ')
         endif
         call mve(8,9,buf8(59,image),data(1,image+2),1,1)
         call mve(8,3,buf8(22,image),data(19,image+2),1,1)
         data(25,image+2)=buf8(15,image)         
         data(26,image+2)=(buf8(14,image)+buf8(13,image))/2.0
         data(38,image+2)=buf8(27,image)         
         idata(39,image+2)=8         !set to object space

c        Get camera constants
         call getcamcon(project(image),camera(image),
     +                  data(27,image+2),data(28,image+2),
     +                  data(29,image+2),data(30,image+2),ind)
         if(ind.ne.0) then
            call qprint('Getcamcon bad ind')
            call abend
         endif

      enddo

c close first 2 inputs.
      call xvclose(inunit(1),ind,' ')
      call xvclose(inunit(2),ind,' ')

c Extract pertinent constants for clarity.
c Focal lengths are negative for the spacecraft data. For
c Manual Of Photogrammetry aircraft data they should be positive.
      focal1=-data(27,3)/1.0d+06 ! focal length camera1 in KM.
      focal2=-data(27,4)/1.0d+06 ! focal length camera2 in KM.
      scale1=data(30,3)*1.0d+06 ! pixels/KM camera1 image plane.
      scale2=data(30,4)*1.0d+06 ! pixels/KM camera2 image plane.
      x1p=data(29,3)/scale1     ! optical axis sample in KM.
      y1p=data(28,3)/scale1     ! optical axis line in KM
      x2p=data(29,4)/scale2     ! optical axis sample in KM
      y2p=data(28,4)/scale2     ! optical axis line in KM
      call mve(8,3,buf8(22,1),rs_vector(1,1),1,1)
      call mve(8,3,buf8(22,2),rs_vector(1,2),1,1)
      call mve(8,9,buf8(59,1),om_mat(1,1,1),1,1)
      call mve(8,9,buf8(59,2),om_mat(1,1,2),1,1)
      do i=1,3
        do j=1,3
          mat1(i,j)=om_mat(j,i,1)
          mat2(i,j)=om_mat(j,i,2)
        enddo
      enddo

c      call qprint('   ')
c      call qprint('Picture 1 project is: '//project(1))
c      call qprint('Navigation source for first input is: '//csource1)
c      call qprint('Picture # 1 RS vector:')
c      call prnt2(8,3,rs_vector(1,1),'XYZ=.')
c      call qprint('Picture # 1 OM matrix:')
c      call prnt2(8,3,om_mat(1,1,1),'row 1=.')
c      call prnt2(8,3,om_mat(1,2,1),'row 2=.')
c      call prnt2(8,3,om_mat(1,3,1),'row 3=.')
c      call qprint('   ')
c      call qprint('Picture 2 project is: '//project(2))
c      call qprint('Navigation source for second input is: '//csource2)
c      call qprint('Picture # 2 RS vector:')
c      call prnt2(8,3,rs_vector(1,2),'XYZ=.')
c      call qprint('Picture # 2 OM matrix:')
c      call prnt2(8,3,om_mat(1,1,2),'row 1=.')
c      call prnt2(8,3,om_mat(1,2,2),'row 2=.')
c      call prnt2(8,3,om_mat(1,3,2),'row 3=.')
c      call qprint('   ')

c     Loop on tiepoint records
      lout=0
      iout=0
      rpole=data(25,3) ! planet polar radius
      req=data(26,3)   ! planet equatorial radius
      do 140 lrec=1,nl3
        call xvread(inmark,image_coords,status,'LINE',lrec,' ')

c       Loop on tiepoints
        do 150 tpt=1,32
          if((image_coords(1,tpt).eq.0.0).and.
     +       (image_coords(2,tpt).eq.0.0)) goto 151

c         Convert map projection line,samp to lat,lon
          call convev(ind,data(1,1),data(1,1),image_coords(1,tpt),
     +                image_coords(2,tpt),lat1,long1,2,scr)
          if(ind.ne.0) goto 151
          call convev(ind,data(1,2),data(1,2),image_coords(3,tpt),
     +                image_coords(4,tpt),lat2,long2,2,scr)
          if(ind.ne.0) goto 151

          if(debug)then
             write(*,*)'left projection: line,samp,->lat,long'
             write(*,99) image_coords(1,tpt),image_coords(2,tpt),
     +                     lat1,long1
             write(*,*)'right projection: line,samp,->lat,long'
             write(*,99) image_coords(3,tpt),image_coords(4,tpt),
     +                     lat2,long2
          endif

c         Convert lat,lon to object space image line,samp
          call convev(ind,data(1,3),data(1,3),line1,samp1,lat1,long1,
     +                1,scr)
          if(ind.ne.0) goto 151
          call convev(ind,data(1,4),data(1,4),line2,samp2,lat2,long2,
     +                1,scr)
          if(ind.ne.0) goto 151

          if(debug)then
             write(*,*)'left object space: lat,long,->line,samp'
             write(*,99) lat1,long1,line1,samp1
             write(*,*)'right object space: lat,long,->line,samp'
             write(*,99) lat2,long2,line2,samp2
          endif

c         convert image coords to Km.
          samp1=samp1/scale1
          line1=line1/scale1
          samp2=samp2/scale2
          line2=line2/scale2

c         convert image coords to xyz in planet coordinates in KM.
          call xvector(mat1,mat2,focal1,focal2,rs_vector(1,1),
     +                rs_vector(1,2),x1p,y1p,x2p,y2p,
     +                dble(samp1),dble(line1),dble(samp2),
     +                dble(line2),x,y,z,error,ind)
          if(ind.ne.0) goto 151

c         update output buffer
          iout=iout+1
          if(xyzmode)then
             xyz(1,iout)=x
             xyz(2,iout)=y
             xyz(3,iout)=z
             xyz(4,iout)=error
          else
             latrad=datan2(z,dsqrt(x*x+y*y)) ! centric latitude
             lat=raddeg*latrad
             long=360.d0-raddeg*datan2(y,x)  ! W longitude
             if(long.gt.360.d0) long=long-360.d0
             range=dsqrt(x*x+y*y+z*z)
             rad=rpole*req/dsqrt(rpole*rpole*(dcos(latrad))**2+
     +                           req*req*(dsin(latrad))**2)
             xyz(1,iout)=lat        ! Geocentric lat. deg.
             xyz(2,iout)=long       ! W long deg.
             xyz(3,iout)=range-rad  ! Elevation above geoid Km.
             xyz(4,iout)=error
          endif


          if(debug)then
             if(xyzmode)then
                write(*,*)'X, Y, Z, error(km)'
                write(*,98) x,y,z,error
             else
                write(*,*)'Lat, Long, Range-Radius, Error(km)'
                write(*,98) lat,long,range-rad,error
             endif
          endif

c         Error handling.
c         Assures a 1:1 correspondence of input & output
          goto 152
151       iout=iout+1
          xyz(1,iout)=0.0
          xyz(2,iout)=0.0
          xyz(3,iout)=0.0
          xyz(4,iout)=0.0
152       continue

c         Occasionally write out a full record.
          if(iout.eq.32) then
             lout=lout+1
             call xvwrit(outunit,xyz,status,'LINE',lout,' ')
             iout=0
          endif

150     continue
140   continue

c write out partially filled buffer.
      if(iout.gt.0) then
         call mve(7,(32-iout)*4,0.0,xyz(1,iout+1),0,1)
         lout=lout+1
         call xvwrit(outunit,xyz,status,'LINE',lout,' ')
      endif

c update nl in the label
      call xldel(outunit,'SYSTEM','NL',status,' ')
      call xladd(outunit,'SYSTEM','NL',lout,status,
     + 'FORMAT','INT',' ')

98    format(3d15.5,e15.5)
99    format(4f10.5)

      return
      END

       subroutine xvector(mat1,mat2,focal1,focal2,cam1,cam2,
     +     x1p,y1p,x2p,y2p,x1,y1,x2,y2,x,y,z,error,ind)
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c mat1=rotation matrix for camera 1 
c mat2=rotation matrix for camera 2
c focal1=camera1 focal length
c focal2=camera2 focal length
c cam1=x,y,z object space position of camera 1
c cam2=x,y,z object space position of camera 2
c x1p,y1p= x & y image plane coord of optical axis, camera 1
c x2p,y2p= x & y image plane coord of optical axis, camera 2
c x1,y1= x & y image plane coord of common point of interest, camera 1
c x2,y2= x & y image plane coord of common point of interest, camera 2
c x,y,z= xyz object space coord of object (returned)
c ind=0 OK, ind=1 no solution (returned)
c Reference: Manual of Photogrammetry, page 64.
       implicit real*8 (a-z)
       real*8 mat1(9),mat2(9),cam1(3),cam2(3)
       real*8 a(9),b(3),c(9)
       real*4 error
       integer*4 ind

c compute direction cosines u,v,w for ray1 and ray2
       ind=0
       dx=x1-x1p
       dy=y1-y1p
       u1=mat1(1)*dx+mat1(4)*dy+mat1(7)*(-focal1)
       v1=mat1(2)*dx+mat1(5)*dy+mat1(8)*(-focal1)
       w1=mat1(3)*dx+mat1(6)*dy+mat1(9)*(-focal1)
       d=dsqrt(u1*u1+v1*v1+w1*w1)
       u1=u1/d
       v1=v1/d
       w1=w1/d
       dx=x2-x2p
       dy=y2-y2p
       u2=mat2(1)*dx+mat2(4)*dy+mat2(7)*(-focal2)
       v2=mat2(2)*dx+mat2(5)*dy+mat2(8)*(-focal2)
       w2=mat2(3)*dx+mat2(6)*dy+mat2(9)*(-focal2)
       d=dsqrt(u2*u2+v2*v2+w2*w2)
       u2=u2/d
       v2=v2/d
       w2=w2/d

c solve for x,y,z point on ray1 nearest to ray2
       as=v1*w2-w1*v2
       bs=u2*w1-u1*w2
       cs=u1*v2-v1*u2
       as1=bs*w1-v1*cs
       bs1=u1*cs-as*w1
       cs1=as*v1-u1*bs
       as2=bs*w2-v2*cs
       bs2=u2*cs-as*w2
       cs2=as*v2-u2*bs
       a(1)=as
       a(2)=as1
       a(3)=as2
       a(4)=bs
       a(5)=bs1
       a(6)=bs2
       a(7)=cs
       a(8)=cs1
       a(9)=cs2
       do 10 i=1,9
          c(i)=a(i)
10     continue
       b(1)=as*cam1(1)+bs*cam1(2)+cs*cam1(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(a,b,3,ind)
       x=b(1)
       y=b(2)
       z=b(3)
       if(ind.gt.0) return

c solve for xx,yy,zz point on ray2 nearest to ray1
       b(1)=as*cam2(1)+bs*cam2(2)+cs*cam2(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(c,b,3,ind)
       if(ind.gt.0) return
       xx=b(1)
       yy=b(2)
       zz=b(3)

c point inbetween is the closest approach point to both vectors
       error=dsqrt((z-zz)**2+(y-yy)**2+(x-xx)**2)
       x=(x+xx)/2.d0
       y=(y+yy)/2.d0
       z=(z+zz)/2.d0
       return
       end

      SUBROUTINE DSIMQ(A,B,N,KS)
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END
