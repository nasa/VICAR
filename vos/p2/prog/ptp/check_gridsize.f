CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check the grid size (INC) by computing the approximate transformation (a,b)
C using the four corner points (x,y).  The type of interpolation chosen
C is as follows:
C  INTERP=0  Zero out this quadrant
C  INTERP=1  Use exact formula for entire quadrant
C  INTERP=2  Use approximate formula for entire quadrant  
C  INTERP=3  Use fixed offsed (DL,DS)
C
      SUBROUTINE CHECK_GRIDSIZE(INCLUDE,LBEG,SBEG,
     &		a,b,inc,address,interp)
      IMPLICIT NONE
      INTEGER*4 INCLUDE,LBEG,SBEG,INC,INTERP,ADDRESS
      REAL*4 A(4),B(4)

      INTEGER*4 IND,INDS(4),IERR,I,Xo,Yo,X(4),Y(4)
      REAL*8 U(4),V(4),Uo,Vo,Uop,Vop,ERR

      X(1) = SBEG
      Y(1) = LBEG

   20 X(2) = X(1) + INC - 1
      Y(2) = Y(1)
      X(3) = X(2)
      Y(3) = Y(2) + INC - 1
      X(4) = X(1)
      Y(4) = Y(3)
      IERR = 0

C     ....Project 4 corners using exact equations
      DO 10 I=1,4
      CALL PSUB(INCLUDE,Y(I),X(I),v(i),u(i),inds(i))
      IF (INDS(I).NE.1) THEN		!If point is off the target,
         IF (INC.GT.4) THEN		!reduce grid size if possible.
            INC = INC/2
            ADDRESS = 10*ADDRESS + 1
            GOTO 20
         ENDIF
         IERR = IERR + 1
      ENDIF
   10 CONTINUE

      IF (IERR.GT.0) GOTO 980

C     ....Compute approximate transformation (a,b) by fitting four corners
   25 CALL FINDT (X,Y,U,V,a,b,ind)

C     ....Calculate midpoint of square using exact and approx transform
      Xo = 0.5*(X(1) + X(2))
      Yo = 0.5*(Y(1) + Y(4))
      CALL PSUB(INCLUDE,Yo,Xo,vo,uo,ind)		!Exact

      Uop = A(1) + A(2)*Xo + A(3)*Yo + A(4)*Xo*Yo	!Approximate
      Vop = B(1) + B(2)*Xo + B(3)*Yo + B(4)*Xo*Yo
 
C     ....Difference between exact and approximate location of midpoint
      ERR = SQRT((Vo-Vop)**2 + (Uo-Uop)**2)
      IF (ERR.GT.0.5) GOTO 970
      INTERP = 2
      RETURN

C     ...Here if grid size is too large
  970 IF (INC.GT.4) THEN	!Reduce grid size if possible
         INC = INC/2
         ADDRESS = 10*ADDRESS + 1
         GOTO 20
      ELSE     
         INTERP = 1		!Otherwise, use exact equations
      ENDIF
      RETURN
C
C     ....Here if INC=4 and some points are off the planet
  980 IF (IERR.LT.4) THEN		!If at least one point is on the planet
         INTERP = 1			!use exact equation for entire quadrant
      ELSE				!If completely off target body and
         INTERP = 0			!background not included, skip square
         IF (INCLUDE.EQ.1) INTERP=3	!Else, use fixed offset
      ENDIF
      RETURN
      END
