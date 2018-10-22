c
c program stereocam
c
      include 'VICMAIN_FOR'
      subroutine main44

      real*8 focal,camera1(9),camera2(9),x,y,z,coords(4)
      real*8 mat1(3,3),mat2(3,3),mat3(3,3),cam1(9),cam2(9)
      integer*4 count,def,inunit(2)
      logical xvptst

      raddeg=180.0d0/3.141592653589d0

c open files
c      call xvunit(inunit(1),'INP',1,status,' ')
c      call xvopen(inunit(1),status,'IO_ACT','AS'
c     +            ,'OPEN_ACT','AS',' ')
c      call xvget(inunit(1),status,'NL',nl,'NS',np,
c     +  'FORMAT',oformat,' ')

c      call xvunit(inunit(2),'INP',2,status,' ')
c      call xvopen(inunit(2),status,'IO_ACT','AS'
c     +            ,'OPEN_ACT','AS',' ')

c Get parameters:
c 1=focal length in pixels
c 2=camera x location
c 3=camera y location
c 4=camera z location
c 5=first rotation degrees
c 6=second rotation degrees
c 7=third rotation degrees
c 8=sample of optical axis
c 9=line of optical axis
      call xvparmd('CAMERA1',camera1,count,def,9)
      call xvparmd('CAMERA2',camera2,count,def,9)
      call xvparm('NL',nl,count,def,1)
      call xvparm('NS',ns,count,def,1)
      call xvparmd('COORDS',coords,count,def,4)
      camera1(9)=nl-camera1(9)+1
      camera2(9)=nl-camera2(9)+1

c set up rotations
      call rotate_about_y(camera1(5),mat3)
      call rotate_about_x(camera1(6),mat2)
      call rotate_about_z(camera1(7),mat1)
      call multiply_matrices(mat3,mat2,mat1,cam1)
c      call multiply_matrices(mat1,mat2,mat3,cam1)
      call rotate_about_y(camera2(5),mat3)
      call rotate_about_x(camera2(6),mat2)
      call rotate_about_z(camera2(7),mat1)
      call multiply_matrices(mat3,mat2,mat1,cam2)
c      call multiply_matrices(mat1,mat2,mat3,cam2)
c      write(*,*)(cam1(i),i=1,9)
c      write(*,*)(cam2(i),i=1,9)

c convert image coords to xyz.
c      write(*,*)(camera1(i),i=1,9)
c      write(*,*)(camera2(i),i=1,9)
      call xvector(cam1,cam2,camera1(1),camera2(1),camera1(2),
     +   camera2(2),camera1(8),camera1(9),camera2(8),camera2(9),
     +   coords(1),nl-coords(2)+1,coords(3),nl-coords(4)+1,
     +   x,y,z,error,ind)

      if(ind.ne.0) then
        write(*,*)'no solution'
      else
        write(*,*)'s1=',coords(1),' l1=',coords(2),' s2=',coords(3),
     +    ' l2=',coords(4)
        write(*,*)'xyz=',x,y,z ! correct camera space
c        write(*,*)'xyz=',z,-x,y ! for air bag test coordinates only
        write(*,*)'error=',error
      endif

      return
      end

      subroutine rotate_about_x(angle,matrix)
      real*8 raddeg,angle,matrix(3,3) ! in row order
      raddeg=180.0d0/3.141592653589d0
      matrix(1,1)=1.d0
      matrix(2,1)=0.d0
      matrix(3,1)=0.d0
      matrix(1,2)=0.d0
      matrix(2,2)=dcos(angle/raddeg)
      matrix(3,2)=-dsin(angle/raddeg)
      matrix(1,3)=0.d0
      matrix(2,3)=dsin(angle/raddeg)
      matrix(3,3)=dcos(angle/raddeg)
      return
      end
      subroutine rotate_about_y(angle,matrix)
      real*8 raddeg,angle,matrix(3,3) ! in row order
      raddeg=180.0d0/3.141592653589d0
      matrix(1,1)=dcos(angle/raddeg)
      matrix(2,1)=0.d0
      matrix(3,1)=dsin(angle/raddeg)
      matrix(1,2)=0.d0
      matrix(2,2)=1.d0
      matrix(3,2)=0.d0
      matrix(1,3)=-dsin(angle/raddeg)
      matrix(2,3)=0.d0
      matrix(3,3)=dcos(angle/raddeg)
      return
      end
      subroutine rotate_about_z(angle,matrix)
      real*8 raddeg,angle,matrix(3,3) ! in row order
      raddeg=180.0d0/3.141592653589d0
      matrix(1,1)=dcos(angle/raddeg)
      matrix(2,1)=-dsin(angle/raddeg)
      matrix(3,1)=0.d0
      matrix(1,2)=dsin(angle/raddeg)
      matrix(2,2)=dcos(angle/raddeg)
      matrix(3,2)=0.d0
      matrix(1,3)=0.d0
      matrix(2,3)=0.d0
      matrix(3,3)=1.d0
      return
      end

      subroutine multiply_matrices(m1,m2,m3,result)
c m1,m2,m3 in row order, result returned in column order.
c order of multiplies is m1*(m2*m3)  right to left
      real*8 m1(3,3),m2(3,3),m3(3,3),m(3,3),result(9)

      m(1,1)=m2(1,1)*m3(1,1)+m2(2,1)*m3(1,2)+m2(3,1)*m3(1,3)
      m(2,1)=m2(1,1)*m3(2,1)+m2(2,1)*m3(2,2)+m2(3,1)*m3(2,3)
      m(3,1)=m2(1,1)*m3(3,1)+m2(2,1)*m3(3,2)+m2(3,1)*m3(3,3)
      m(1,2)=m2(1,2)*m3(1,1)+m2(2,2)*m3(1,2)+m2(3,2)*m3(1,3)
      m(2,2)=m2(1,2)*m3(2,1)+m2(2,2)*m3(2,2)+m2(3,2)*m3(2,3)
      m(3,2)=m2(1,2)*m3(3,1)+m2(2,2)*m3(3,2)+m2(3,2)*m3(3,3)
      m(1,3)=m2(1,3)*m3(1,1)+m2(2,3)*m3(1,2)+m2(3,3)*m3(1,3)
      m(2,3)=m2(1,3)*m3(2,1)+m2(2,3)*m3(2,2)+m2(3,3)*m3(2,3)
      m(3,3)=m2(1,3)*m3(3,1)+m2(2,3)*m3(3,2)+m2(3,3)*m3(3,3)

      result(1)=m1(1,1)*m(1,1)+m1(2,1)*m(1,2)+m1(3,1)*m(1,3)
      result(4)=m1(1,1)*m(2,1)+m1(2,1)*m(2,2)+m1(3,1)*m(2,3)
      result(7)=m1(1,1)*m(3,1)+m1(2,1)*m(3,2)+m1(3,1)*m(3,3)
      result(2)=m1(1,2)*m(1,1)+m1(2,2)*m(1,2)+m1(3,2)*m(1,3)
      result(5)=m1(1,2)*m(2,1)+m1(2,2)*m(2,2)+m1(3,2)*m(2,3)
      result(8)=m1(1,2)*m(3,1)+m1(2,2)*m(3,2)+m1(3,2)*m(3,3)
      result(3)=m1(1,3)*m(1,1)+m1(2,3)*m(1,2)+m1(3,3)*m(1,3)
      result(6)=m1(1,3)*m(2,1)+m1(2,3)*m(2,2)+m1(3,3)*m(2,3)
      result(9)=m1(1,3)*m(3,1)+m1(2,3)*m(3,2)+m1(3,3)*m(3,3)

c      result(1)=m1(1,1)*m(1,1)+m1(2,1)*m(1,2)+m1(3,1)*m(1,3)
c      result(2)=m1(1,1)*m(2,1)+m1(2,1)*m(2,2)+m1(3,1)*m(2,3)
c      result(3)=m1(1,1)*m(3,1)+m1(2,1)*m(3,2)+m1(3,1)*m(3,3)
c      result(4)=m1(1,2)*m(1,1)+m1(2,2)*m(1,2)+m1(3,2)*m(1,3)
c      result(5)=m1(1,2)*m(2,1)+m1(2,2)*m(2,2)+m1(3,2)*m(2,3)
c      result(6)=m1(1,2)*m(3,1)+m1(2,2)*m(3,2)+m1(3,2)*m(3,3)
c      result(7)=m1(1,3)*m(1,1)+m1(2,3)*m(1,2)+m1(3,3)*m(1,3)
c      result(8)=m1(1,3)*m(2,1)+m1(2,3)*m(2,2)+m1(3,3)*m(2,3)
c      result(9)=m1(1,3)*m(3,1)+m1(2,3)*m(3,2)+m1(3,3)*m(3,3)

      return
      end      

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
c       write(*,*)'uvw1',u1,v1,w1
       d=dsqrt(u1*u1+v1*v1+w1*w1)
       u1=u1/d
       v1=v1/d
       w1=w1/d
       dx=x2-x2p
       dy=y2-y2p
       u2=mat2(1)*dx+mat2(4)*dy+mat2(7)*(-focal2)
       v2=mat2(2)*dx+mat2(5)*dy+mat2(8)*(-focal2)
       w2=mat2(3)*dx+mat2(6)*dy+mat2(9)*(-focal2)
c       write(*,*)'uvw2',u2,v2,w2
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
