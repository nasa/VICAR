c  test subroutine UNIFLT

        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44

	IMPLICIT INTEGER(A-Z)

	BYTE A(10)/1,2,3,4,5,6,7,8,9,10/, B(10)
	INTEGER*2 C(10)/1,2,3,4,5,6,7,8,9,10/, D(10)
	INTEGER*4 E(10)/1,2,3,4,5,6,7,8,9,10/, F(10)
	REAL*4 P(10)/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./, Q(10)
	REAL*8 R(10), S(10)
        CHARACTER*250 MSG
	NSW = -9

c byte-to-byte
1	DC = 1
	CALL UNIFLT(DC,10,A,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')
100	FORMAT(' DCODE=',I3)
150     FORMAT(10I5) 
    
c byte-to-halfword
	DC = 3
	CALL UNIFLT(DC,10,A,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c halfword-to-byte
        DC = -3
	CALL UNIFLT(DC,10,C,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')

c halfword-to-halfword
	DC = 2
	CALL UNIFLT(DC,10,C,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c byte-to-fullword
	DC = 5
	CALL UNIFLT(DC,10,A,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c fullword-to-byte
	DC = -5
	CALL UNIFLT(DC,10,E,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')

c fullword-to-fullword
	DC = 4
	CALL UNIFLT(DC,10,E,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c halfword-to-fullword
	DC = 6
	CALL UNIFLT(DC,10,C,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c fullword-to-halfword
	DC = -6
	CALL UNIFLT(DC,10,E,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c real-to-real
	DC = 7
	CALL UNIFLT(DC,10,P,Q,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) Q
        CALL XVMESSAGE(MSG,' ')
200	FORMAT(' DCODE=',I3)
250     FORMAT(10E10.3)
c real-to-double
	DC = 9
	CALL UNIFLT(DC,10,P,R,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) R
        CALL XVMESSAGE(MSG,' ')

c double-to-double
	DC = 8
	CALL UNIFLT(DC,10,R,S,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) S
        CALL XVMESSAGE(MSG,' ')

c double-to-real
	DC = -9
	CALL UNIFLT(DC,10,R,Q,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) Q
        CALL XVMESSAGE(MSG,' ')

10	IF (NSW.GT.0) THEN
	  IF (A(1).LT.0) GO TO 500 

	  DO I = 1,10 
	    A(I) = -A(I)
	    C(I) = -C(I)
	    E(I) = -E(I)
	    P(I) = -P(I)
	  ENDDO

	  GO TO 1
	ENDIF

	NSW = 9
	GO TO 1

500	RETURN
        END

