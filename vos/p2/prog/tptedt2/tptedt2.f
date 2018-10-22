c
c program tptedt2
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (ntable=100000)
c ntable = max # tiepoints
      integer*4 status,count,def,located(20),point,pass
      real*4 left_line(ntable),left_samp(ntable)
      real*4 right_line(ntable),right_samp(ntable)
      real*4 buf(4,32)
      real*8 coefx(4),coefy(4)
      real*8 a(4,4),aa(4,4)
      real*8 c(20,4),cl(20),weights(20)
      byte flag(ntable)
      character*80 msg
      logical xvptst,exceed_length,exceed_angle,both

      radeg=180./3.141592653

      call xvparm('NPTS',nptsfit,count,def,1)
      call xvparm('DISTANCE',distance,count,def,1)
      call xvparm('LENGTH',size,count,def,1)
      size=size*size
      call xvparm('ANGLE',angle_limit,count,def,1)
      call xvparm('RANGE',range_limit,count,def,1)
      call xvparm('BIAS',bias,count,def,1)
      both=xvptst('BOTH')

c load the tiepoint table from the mark file.
      call xvunit(inmark,'INP',1,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl,'NS',ns,' ')
      npts=0
      nptsgood=0
      nptsbad=0
      do i=1,nl
         call xvread(inmark,buf,status,'LINE',i,' ')
         do j=1,32
            npts=npts+1
            flag(npts)=1
            if(buf(1,j).eq.0.0) flag(npts)=0
            if(buf(2,j).eq.0.0) flag(npts)=0
            if(buf(3,j).eq.0.0) flag(npts)=0
            if(buf(4,j).eq.0.0) flag(npts)=0
            if(flag(npts).eq.1)then
               nptsgood=nptsgood+1
            else
               nptsbad=nptsbad+1
            endif
            if(npts.gt.ntable)then
               write(msg,5) ntable
5              format(' Too many input points, limit= ',i6)
               call xvmessage(msg,' ')
               call abend
            endif
            left_line(npts)=buf(1,j)            
            left_samp(npts)=buf(2,j)            
            right_line(npts)=buf(3,j)            
            right_samp(npts)=buf(4,j)            

c           Delete vectors that are too long
            if(((right_line(npts)-left_line(npts))**2+
     +         (right_samp(npts)-left_samp(npts))**2).gt.size)
     +         flag(npts)=0

         enddo
      enddo

      call xvmessage('Input tiepoints file contains:',' ')
      write(msg,4) nptsgood,nptsbad
4     format(1x,i6,' Good points and ',i6,' empty spaces')
      call xvmessage(msg,' ')
      call xvclose(inmark,status,'CLOS_ACT','FREE',' ')

      pass=1
      number_deleted=0               ! true number for any reason
1000  number_deleted_range=0
      number_deleted_angle=0

c Loop on tiepoints
      do 100 point=1,npts
            if(flag(point).eq.0) goto 100
            gridl=left_line(point)
            grids=left_samp(point)

c           determine the 4 nearest points, one in each quadrant.
            d_topleft=1.0e+10
            d_topright=1.0e+10
            d_botleft=1.0e+10
            d_botright=1.0e+10
            i_topleft=0
            i_topright=0
            i_botleft=0
            i_botright=0
            do 101 i=1,npts
               if(flag(i).eq.0) goto 101
               if(i.eq.point)   goto 101
               if(left_line(i).lt.gridl)then
                  if(left_samp(i).lt.grids)then  ! upper left
                     d=gridl-left_line(i) + grids-left_samp(i)
                     if(d.lt.d_topleft)then
                        d_topleft=d
                        i_topleft=i
                     endif
                  else                            ! upper right
                     d=gridl-left_line(i) + left_samp(i)-grids
                     if(d.lt.d_topright)then
                        d_topright=d
                        i_topright=i
                     endif
                  endif
               else                               
                  if(left_samp(i).lt.grids)then  ! lower left
                     d=left_line(i)-gridl + grids-left_samp(i)
                     if(d.lt.d_botleft)then
                        d_botleft=d
                        i_botleft=i
                     endif
                  else                            ! lower right
                     d=left_line(i)-gridl + left_samp(i)-grids
                     if(d.lt.d_botright)then
                        d_botright=d
                        i_botright=i
                     endif
                  endif
               endif                     
101         continue

c           PERFORM FITTING STEP. 

            if(nptsfit.gt.4) goto 200

c           perform exact fit using 4 points only, no weighting.

c           skip analysis if all 4 quadrants dont have a point
            if(i_topleft.eq.0.or.i_topright.eq.0.or.
     +         i_botleft.eq.0.or.i_botright.eq.0)then
               goto 100
            endif

            a(1,1)=dble(left_line(i_topleft))*
     +             dble(left_samp(i_topleft))
            a(1,2)=dble(left_line(i_topleft))
            a(1,3)=dble(left_samp(i_topleft))
            a(1,4)=1.d0
            a(2,1)=dble(left_line(i_topright))*
     +             dble(left_samp(i_topright))
            a(2,2)=dble(left_line(i_topright))
            a(2,3)=dble(left_samp(i_topright))
            a(2,4)=1.d0
            a(3,1)=dble(left_line(i_botleft))*
     +             dble(left_samp(i_botleft))
            a(3,2)=dble(left_line(i_botleft))
            a(3,3)=dble(left_samp(i_botleft))
            a(3,4)=1.d0
            a(4,1)=dble(left_line(i_botright))*
     +             dble(left_samp(i_botright))
            a(4,2)=dble(left_line(i_botright))
            a(4,3)=dble(left_samp(i_botright))
            a(4,4)=1.d0
            call mve(8,16,a,aa,1,1) ! save a

c           to right image line
            coefy(1)=right_line(i_topleft)
            coefy(2)=right_line(i_topright)
            coefy(3)=right_line(i_botleft)
            coefy(4)=right_line(i_botright)
            call dsimq(a,coefy,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               goto 100
            endif

c           to right image sample
            call mve(8,16,aa,a,1,1) ! restore a
            coefx(1)=right_samp(i_topleft)
            coefx(2)=right_samp(i_topright)
            coefx(3)=right_samp(i_botleft)
            coefx(4)=right_samp(i_botright)
            call dsimq(a,coefx,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               goto 100
            endif
            goto 300            

c           Least squares fit using from 5 to 20 nearest points.
200         continue

c           move 4 quadrant points into located() buffer
            n=0
            if(i_topleft.gt.0)then
              n=n+1
              located(n)=i_topleft
            endif
            if(i_topright.gt.0)then
              n=n+1
              located(n)=i_topright
            endif
            if(i_botleft.gt.0)then
              n=n+1
              located(n)=i_botleft
            endif
            if(i_botright.gt.0)then
              n=n+1
              located(n)=i_botright
            endif

c           locate additional points as the nearest ones remaining
            do i=n+1,nptsfit
              d=1.0e+20
              jj=0
              do 102 j=1,npts
                if(flag(j).eq.0) goto 102
                if(j.eq.point)   goto 102
                do k=1,i-1   ! avoid points already located
                  if(located(k).eq.j) goto 210
                enddo
                dist=(gridl-left_line(j))**2+(grids-left_samp(j))**2
                if(dist.lt.d) then
                   d=dist
                   jj=j
                endif
210             continue
102           continue
              if(jj.eq.0)then
                 call xvmessage(' Insufficient points remain',' ')
                 call abend
              endif
              located(i)=jj
            enddo

c           compute weights for points inversely with distance
            do i=1,nptsfit
              dist=sqrt((gridl-left_line(located(i)))**2 +
     +                  (grids-left_samp(located(i)))**2 )
              weights(i)=distance/(dist+1.0)
            enddo

c           fit for right image line direction.
            do i=1,nptsfit
              c(i,1)=dble(left_line(located(i))) *
     +               dble(left_samp(located(i)))
              c(i,2)=dble(left_line(located(i)))
              c(i,3)=dble(left_samp(located(i)))
              c(i,4)=1.d0
              cl(i)=right_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,coefy)
            if(kstat.ne.0)then
               call xvmessage('LSQP singular solution',' ')
               goto 100
            endif

c           fit for right image sample direction.
            do i=1,nptsfit
              cl(i)=right_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,coefx)
            if(kstat.ne.0)then
               call xvmessage('LSQP singular solution',' ')
               goto 100
            endif

c END OF FITTING

300         continue
c           compute the x,y position in the right image.
            y=coefy(1)*dble(gridl)*grids+coefy(2)*gridl+
     +                coefy(3)*grids+coefy(4)
            x=coefx(1)*dble(gridl)*grids+coefx(2)*gridl+
     +                coefx(3)*grids+coefx(4)

c           Compute range constraint
            exceed_length=.false.
            p_range=sqrt((y-gridl)**2+(x-grids)**2)  ! predicted range
            t_range=sqrt((right_line(point)-gridl)**2+
     +                   (right_samp(point)-grids)**2) ! actual range
            if(abs(t_range-p_range)/(t_range+p_range+bias).gt.
     +         range_limit)then
               number_deleted_range=number_deleted_range+1
               exceed_length=.true.
            endif

c           Compute angle constraint
            exceed_angle=.false.
            p_angle=radeg*atan2(y-gridl,x-grids)    ! predicted angle
            t_angle=radeg*atan2(right_line(point)-gridl,
     +                    right_samp(point)-grids)  ! actual angle
            if(p_angle.lt.0.) p_angle=p_angle+360.  ! assure 0 to 360
            if(t_angle.lt.0.) t_angle=t_angle+360.
            dangle=abs(t_angle - p_angle)
            if(dangle.gt.180.) dangle=360.-dangle ! on both sides of 0
            if(dangle*t_range/(t_range+bias).gt.angle_limit)then
               number_deleted_angle=number_deleted_angle+1
               exceed_angle=.true.
            endif

c           Decide on point
            if(both)then
               if(exceed_length.and.exceed_angle)then
                  flag(point)=0
                  number_deleted=number_deleted+1
               endif
            else
               if(exceed_length.or.exceed_angle)then
                  flag(point)=0
                  number_deleted=number_deleted+1
               endif
            endif
            
100   continue              ! end point loop

      write(msg,9) pass
9     format(' Pass number ',i4)
      call xvmessage(msg,' ')
      write(msg,7) number_deleted_range
7     format(' Number of points failing range check =',i6)
      call xvmessage(msg,' ')
      write(msg,8) number_deleted_angle
8     format(' Number of points failing angle check =',i6)
      call xvmessage(msg,' ')

c return for a second pass
      pass=pass+1
      if(pass.lt.3) goto 1000

      write(msg,6) number_deleted
6     format(' Actual number of points deleted =',i6)
      call xvmessage(msg,' ')
      call xvmessage('   ',' ')

c re-load the mark file.
      call xvunit(outmark,'OUT',1,status,' ')
      call xvopen(outmark,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'OP','WRITE','O_FORMAT','REAL','OPEN_ACT','AS',' ')
      n=0
      do i=1,nl
         do j=1,32
            n=n+1
            if(flag(n).eq.0)then
               buf(1,j)=0.  ! deleted tiepoint
               buf(2,j)=0.
               buf(3,j)=0.
               buf(4,j)=0.
            else
               buf(1,j)=left_line(n)
               buf(2,j)=left_samp(n)
               buf(3,j)=right_line(n)
               buf(4,j)=right_samp(n)
            endif
         enddo
         call xvwrit(outmark,buf,status,'LINE',i,' ')
      enddo
      call xvclose(outmark,status,'CLOS_ACT','FREE',' ')

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
      REAL*8 C(20,4),CL(20),X1(4),wts(20)
      
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


C*********************************************************************
      SUBROUTINE DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C
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
      if(biga.eq.0.d0)then
        ks=1
        return
      endif
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

