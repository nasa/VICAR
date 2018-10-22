      INCLUDE 'VICMAIN_FOR'

C TEST SUBROUTINE "PRNT" -- ALL DCODES, 2 N'S (LT/GT line SIZE)
C
      SUBROUTINE MAIN44
      BYTE      ABUF(256)
      INTEGER*2 HBUF(120)
      INTEGER*4 IBUF(30)
      REAL*4    RBUF(30),R
      REAL*8    DBUF(30),D
      COMPLEX   ZBUF(30) 
      INTEGER   CODE(7) / 0,  1,  2,  4,  7,  8, 10/
      INTEGER   SIZ(7)  /32, 30, 20, 10, 10, 10, 10/
      CHARACTER*8 TITL(7)/' DUMP   ',' BYTE   ',' HALFWD.',' FULLWD.',
     : ' REAL*4.',' REAL*8.','COMPLEX.'/
      CHARACTER*8 TITLE/'CHARACT.'/
      CHARACTER*160 LONGSTR
C
      DO I=1,120
	 ABUF(I)  = MOD(I-1,26) + 65
	 HBUF(I)  = -16705 + (I - 1)*200
      ENDDO
      abuf(1)=0
      abuf(2)=127
      abuf(3)=-1   ! Should be interperted as 255.

      DO I=1,30
         IBUF(I) = 1000*(I-10)
	 RBUF(I) = 1.E3*I
	 DBUF(I) = 1.D4*I
         R       = I - 1
         ZBUF(I) = CMPLX(R,0.0)
      ENDDO
      ibuf(1)= -(10**9)
C
      CALL XVMESSAGE('Test #1:  WITH TITLES',' ')
      DO D=1,7     !  TRY VARIOUS DCODES
	 DO NN=1,3,2         !  LINE LENGTH
	    N = NN*SIZ(D)
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, TITL(D))
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, TITL(D))
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, TITL(D))
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, TITL(D))
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, TITL(D))
	    CALL XVMESSAGE(' ',' ')
	 ENDDO
      ENDDO
      CALL XVMESSAGE('Test #2:  REPEAT WITHOUT TITLES',' ')

      DO D=1,7     !  TRY VARIOUS DCODES
	 DO NN=1,3,2         !  LINE LENGTH
	    N = NN*SIZ(D)
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, ' ')
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, ' ')
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, ' ')
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, ' ')
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, ' ')
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, ' ')
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, ' ')
	    CALL XVMESSAGE(' ',' ')
	 ENDDO
      ENDDO

      CALL XVMESSAGE('Test #3:  REPEAT WITH TITLES and N=1',' ')
      DO D=1,7     !  TRY VARIOUS DCODES
	    N = 1
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, TITL(D))
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, TITL(D))
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, TITL(D))
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, TITL(D))
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, TITL(D))
	    CALL XVMESSAGE(' ',' ')
      ENDDO


c  Repeat test cases in C to test C interface: zprnt

      CALL XVMESSAGE('REPEAT ABOVE TESTS FROM C LANGUAGE',' ')
      call tzprnt(CODE,SIZ,ABUF,HBUF,IBUF,RBUF,DBUF,ZBUF)

C LONG TITLE
      LONGSTR='123456789012345678901234567890123456789012345678901234567
     C890123456789072345678901234567890123456789012345678901234567890123
     C4567890123456789012345678907234567890'

      CALL XVMESSAGE(
     .  'The following PRNT output should all fit on one line', ' ')
      CALL PRNT(4, 1, IBUF, LONGSTR(1:110) )
      CALL XVMESSAGE(
     . 'Next title should be truncated to 132 chars. Data on 2nd line.',
     .  ' ')
      CALL PRNT(4, 1, IBUF, LONGSTR )

C     ....ASCII string

      CALL MVCL(LONGSTR, ABUF, 150)
      CALL PRNT(99,12,ABUF,' This is a short ASCII string:.')
      CALL PRNT(99,150,ABUF,' This is a longer ASCII string:.')

      CALL XVMESSAGE(
     . 'Test cases of a single real value of various magnitudes.', ' ')
      R = .0000123123123
      DO I = 1,15
         CALL PRNT(7,1,R,' R =')
         CALL PRNT(7,1,-R,'-R =')
         R = R*10.
      END DO
      D = .0000123123123D0
      DO I = 1,15
         CALL PRNT(8,1,D,' D =')
         CALL PRNT(8,1,-D,'-D =')
         D = D*10.
      END DO
      RETURN
      END
