C------------------------------------------------------------------	  
C  Project input image onto output image.  This version maintains
C  the entire input image in-core (IC).
C
      SUBROUTINE ICPROJECT(PIC,N1,BUF,N2)
      IMPLICIT NONE
      BYTE PIC(*),BUF(*)
      INTEGER*4 N1,N2

      COMMON/CINP/SL,SS,NLO,NSO,NLI,NSI,INCo
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,INCo

      COMMON/CINC/DL,DS,INCLUDE
      REAL*8 DL,DS
      INTEGER*4 INCLUDE

      COMMON/CIO/IUNIT,OUNIT,DCODE
      INTEGER*4 IUNIT,OUNIT,DCODE

      INTEGER LBEG,SBEG,ROW,COL,I,L
      INTEGER*4 INC,ADDRESS,INTERP,IND
      REAL*4 A(4),B(4)
      LOGICAL XVPTST,EXACT

      IF (N1.LT.NLI*NSI*DCODE) GOTO 990
      IF (N2.LT.NSO*INCo*DCODE) GOTO 990
C           Read in input image
      I = 1
      DO L=1,NLI
         CALL XVREAD(IUNIT,PIC(I),ind,' ')
         I = I + NSI*DCODE
      ENDDO

      EXACT = XVPTST('EXACT')

C     ....Process image in INCo X INCo blocks
      DO 110 ROW=1,NLO,INCo             !Row loop
      DO 100 COL=1,NSO,INCo             !Column loop
      INC = INCo
      ADDRESS = 0
      LBEG = ROW
      SBEG = COL

   20 IF (LBEG.GT.NLO .OR. SBEG.GT.NSO) GOTO 40
      IF (EXACT) THEN		!If EXACT is specified
         INTERP = 1		!project using exact equations.
      ELSE			!Else, see if grid size is appropriate.
         CALL CHECK_GRIDSIZE(INCLUDE,LBEG,SBEG,
     &		a,b,inc,address,interp)
      ENDIF
      IF (DCODE.EQ.1) THEN
         CALL PROJECT_QUAD(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      ELSE
         CALL PROJECT_QUAD2(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      ENDIF

   40 CALL UPDATE_ADDRESS(address,inc,lbeg,sbeg)
      IF (ADDRESS.GT.0) GOTO 20
  100 CONTINUE

      I = 1
      DO L=1,INCo
         CALL XVWRIT(OUNIT,BUF(I),ind,' ') !Write completed lines
         I = I + NSO*DCODE
      ENDDO
  110 CONTINUE
  
      RETURN

  990 CALL XVMESSAGE('***Not enough memory',' ')
      CALL XVMESSAGE('***PTP task cancelled',' ')
      CALL ABEND
      END
