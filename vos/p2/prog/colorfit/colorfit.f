c
c program colorme
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxpix=3,maxtable=10000)
c      character*200 msg
c      integer*4 def,count
      integer*4 inunit(maxpix),status,nl(maxpix),ns(maxpix)
      real*4 buf(maxpix)
      real*8 C(maxtable,4),CL(maxtable),coef(4),error

c parameters
c      call xvparm('PERCENT',percent,count,def,1)

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.eq.0)then
        call xvmessage('No inputs',' ')
        call abend()
      endif

c open all inputs to get nl & ns
      do i=1,nin
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        call xvget(inunit(i),status,'NL',nl(i),'NS',ns(i),' ')
      enddo

c make sizes equal
      i=ns(1)
      j=nl(1)
      do image=1,nin
        i=min(ns(image),i)
        j=min(nl(image),j)
      enddo
      do image=1,nin
        ns(image)=i
        nl(image)=j
      enddo

c collect data points for least squares fitting
      call get_seconds(iseed)
      do index=1,maxtable
100     call rangen(iseed,rannum) ! 0.0 < rannum < 1.0
        i=nint((ns(1)-1)*rannum+1.0)
        call rangen(iseed,rannum) ! 0.0 < rannum < 1.0
        line=nint((nl(1)-1)*rannum+1.0)
        if((i.lt.1).or.(i.gt.ns(1)))goto 100
        if((line.lt.1).or.(line.gt.nl(1)))goto 100
        do image=1,nin
          call xvread(inunit(image),buf(image),status,'LINE',line,
     +                'SAMP',i,'NSAMPS',1,' ')
          if(buf(image).eq.0.0)goto 100
          if(buf(image).eq.32767)goto 100
        enddo
        c(index,1)=buf(1)
        c(index,2)=buf(2)
        c(index,3)=1.d0
        c(index,4)=buf(1)/buf(2)
        cl(index)=buf(3)
      enddo

c solve for coefficients
c WARNING: the print statements below are necessary to get the code to compile
c on sgi machines. God help us !
      write(*,*)'   '
      call lsqp(maxtable,4,c,cl,coef,maxtable)
      write(*,*)'   '
c      write(msg,*)coef(1),'*IN1+',coef(2),'*IN2+',coef(3),'+',coef(4),
c     + '*IN1/IN2'
c      call xvmessage(msg,' ')
      write(*,*)coef(1),'*IN1+',coef(2),'*IN2+',coef(3),'+',coef(4),
     + '*IN1/IN2'

      error=0.d0
      do index=1,maxtable
        error=error+abs( coef(1)*c(index,1) +
     +        coef(2)*c(index,2) + coef(3) +
     +        coef(4)*c(index,1)/c(index,2) - cl(index) )
      enddo
      error=error/maxtable
c      write(msg,*)'Mean dn residual is ',error
c      call xvmessage(msg,' ')
      write(*,*)'Mean dn residual is ',error

      end

c *********************************************************************
      SUBROUTINE LSQP(NE,NU,C,CL,X1,maxtable)
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C     CL%I<#ARRAY OF FREE TERMS,  NE#NUMBER OF
C     EQUATIONS AND NU#NUMBER OF UNKNOWNS%NE AND NU NOT TO EXCEED THE
C     DIMENSION SPECIFICATIONS BELOW<.
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS,X1(J)=THE COMPUTED
C     VALUES OF THE UNKNOWNS, V#RESIDUALS I.E. OBSERVED MINUS COMPUTED,
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.

      REAL*8 C(maxtable,4),CL(maxtable),X1(4)
      REAL*8 A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SUM

      DO 57 J=1,NU
         DO 57 I=1,NU
            A(I,J)=0.
            R(I,J)=0.
57       Q(I,J)=0.
      DO 100 I=1,NU
         DO 100 J=1,NU
            DO 100 K=1,NE
100           A(I,J)=A(I,J)+C(K,I)*C(K,J)
      DO 102 I=1,NU
         AL(I)=0.
         DO 102 K=1,NE
102            AL(I)=AL(I)+ C(K,I)*CL(K)
 
      DO I=1,NU
         if (A(I,I) .eq. 0.0)  call mabend(
     .     'ERROR in routine LSQP: Please check input parameters')
      END DO
 
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
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
      END

