      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C-----THIS IS A ROUTINE TO TEST  FUNCTION  CMPR
	LOGICAL CMPR,T
        character*3 not
	character A*28
	INTEGER*4 S1/'ABCD'/,S2/'ABCE'/
	CALL XVMESSAGE('COMPARING ABCD WITH ABCE',' ')
C
	DO 10 I=1,4
        not = ' '
	T=CMPR(S1,S2,I)
	IF(.NOT.T)         not = 'NOT'
        write (A,9000) I,not
10	CALL XVMESSAGE(A,' ')
C
	return
9000    format( 'FIRST ', I1,' CHARACTERS ', A3,' SAME')
	END
