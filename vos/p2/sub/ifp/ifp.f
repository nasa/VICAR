	FUNCTION IFP(X,Y,M,N,BUF,INTERP,IHALF)

!       IFP: A four point interpolation function

!       IFP passed parameters
        REAL     X, Y
        INTEGER  M, N, INTERP, IHALF
	BYTE     BUF(1)

!       IFP local parameters
        REAL*8  E0,E2,E4,E6,E,F,G,H,R7,R8,R9,R10
	INTEGER HALF, status
        INTEGER byte_to_half (12), half_to_half (12)
        INTEGER half_to_doub (12), byte_to_doub (12)

	INTEGER*2 IOUT, ifpout
	BYTE      IOUTEQV(2)

!       IFP local parameter initialization
        DATA E0 /0.0/, E2 /0.0/, E4 /0.0/, E6  /0.0/
        DATA E  /0.0/, F  /0.0/, G  /0.0/, H   /0.0/
        DATA R7 /0.0/, R8 /0.0/, R9 /0.0/, R10 /0.0/
	DATA HALF /0/, status /0/
	DATA half_to_doub /12*0/, byte_to_doub /12*0/
	DATA IOUTEQV(1) /0/, IOUTEQV(2) /0/
        DATA IOUT /0/, IFPOUT /0/

C    -------------------------------------------------------------      
C      07 Mar 95    ...CRI...   Removed LIB_LOCAL from imake
C      01 Jul 94    ...CRI...   Made portable for UNIX
C      FEB 1985     ...BZB...   CONVERTED TO VAX FORTRAN 
C      21 AUG 80    ...JBS...   CORRECT ERROR WHEN X=M OR Y=N
C      28 DEC 78    ...WDB...   INITIAL RELEASE
C      TITLE 'FOUR POINT INTERPOLATION SUBROUTINE'
C              OUT=IFP(X,Y,M,N,BUF,INTERP,HALF)
C              WHERE X & Y ARE THE REAL VALUES OF THE THE DESIRED VALUE
C              BUF(M,N) IS THE INPUT BUFFER OF DIMENSION M,N
C              INTERP IS AN INTERPOLATION FLAG. 0 IF INTERPOLATION,
C                 ELSE 1 FOR NO INTERPOLATION                           
C              HALF IS A PARM SPECIFYING WHETHER THE
C                 INPUT IS HALFWORD OR BYTE. 0 IF BYTE, 1 IF HALF.
C
C
C        (PX,PY)              (PX+1,PY)
C        O----------------------------O
C        I*                          *I
C        I *                        * I
C        I  *                      *  I
C        I   *                    *   I
C        I    *                  *    I
C        I     *                *     I
C        I      E              G      I
C        I        *           *       I
C        I         *         *        I
C        I          *       *         I
C        I           *     *          I
C        I            *   *           I
C        I             * *            I
C        I              $-(X,Y)       I
C        I             * *            I
C        I            *   *           I
C        I           *     *          I
C        I          *       *         I
C        I         *         *        I
C        I        *           *       I
C        I      F              H      I
C        I     *                *     I
C        I    *                  *    I
C        I   *                    *   I
C        I  *                      *  I
C        I *                        * I
C        I*                          *I
C        O----------------------------O
C        (PX,PY+1)          (PX+1,PY+1)
C
C    -------------------------------------------------------------
C
        HALF = IHALF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (byte_to_half, 'BYTE',
     &                   'HALF', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (half_to_half, 'HALF',
     &                   'HALF', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (half_to_doub, 'HALF',
     &                   'DOUB', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to real*8 
        call xvtrans_set (byte_to_doub, 'BYTE',
     &                   'DOUB', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

	IF (INTERP.NE.0) GO TO 8000
	IPX=X
	E0=X-IPX
	IPY=Y
	E2=Y-IPY
	IF ((E0.EQ.0.).AND.(E2.EQ.0.)) GO TO 8000
	IF (E0.NE.0.) GO TO 10
9	CONTINUE
	IF (IPX.NE.M) GO TO 10
	IPX=IPX-1
	E0=.999
10	CONTINUE
	IF (E2.NE.0.) GO TO 20
19	CONTINUE
	IF (IPY.NE.N) GO TO 20
	IPY=IPY-1
	E2=.999

20	CONTINUE
	E4=1.-E0
	E6=1.-E2
	E0=E0*E0
	E2=E2*E2
	E4=E4*E4
	E6=E6*E6
	E=1./(E0+E2)
	H=1./(E4+E6)
	F=1./(E0+E6)
	G=1./(E2+E4)

	IF (HALF.EQ.1) GO TO 200
        J1 = (IPY-1)*M+IPX
        J2 = J1+M

        CALL xvtrans (byte_to_doub, buf(J1),   R7, 1)
        CALL xvtrans (byte_to_doub, buf(J1+1), R8, 1)
        CALL xvtrans (byte_to_doub, buf(J2),   R9, 1)
        CALL xvtrans (byte_to_doub, buf(J2+1), R10, 1)

	IFP = (R7*E+R8*G+R9*F+R10*H)/(E+G+F+H)+.5

	RETURN

200	CONTINUE
	IOUT=0
        J1 = 2*(IPY-1)*M+2*IPX-1
        J2 = J1+2*M

	IOUTEQV(1)=BUF(J1)
	IOUTEQV(2)=BUF(J1+1)
        CALL xvtrans (half_to_doub, IOUTEQV(1), R7, 1)

	IOUTEQV(1)=BUF(J1+2)
	IOUTEQV(2)=BUF(J1+3)
        CALL xvtrans (half_to_doub, iouteqv(1),   R8, 1)

	IOUTEQV(1)=BUF(J2)
	IOUTEQV(2)=BUF(J2+1)
        CALL xvtrans (half_to_doub, iouteqv(1),   R9, 1)

	IOUTEQV(1)=BUF(J2+2)
	IOUTEQV(2)=BUF(J2+3)
        CALL xvtrans (half_to_doub, iouteqv(1),   R10, 1)

	IFP=(R7*E+R8*G+R9*F+R10*H)/(E+G+F+H)+.5

	RETURN

8000	CONTINUE
C	NO INTERPOLATION, 
C	JUST NEAREST NEIGHBOR
	IPX=X+.5
	IPY=Y+.5
	IF (HALF.EQ.1) GO TO 8200
	IOUTEQV(1)=BUF( (IPY-1)*M+IPX )
        CALL xvtrans (byte_to_half, 
     &                IOUTEQV(1), ifpout, 1)
	IFP = ifpout
        RETURN

8200	CONTINUE
	IOUT=0
	IOUTEQV(1)=BUF( 2*(IPY-1)*M+2*IPX-1 )
	IOUTEQV(2)=BUF( 2*(IPY-1)*M+2*IPX )
        CALL xvtrans (half_to_half, 
     &                IOUTEQV(1), ifpout, 1)
	IFP = ifpout
	RETURN
	END
