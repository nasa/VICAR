        INCLUDE 'VICMAIN_FOR'
        subroutine MAIN44
c-------program nutinp
C-------1/88   CCA   CHANGE PROMPT TO 'NUT'
C-------1/88   CCA   ADD XZEXIT TO CORRECT TIMING
        integer ITYPE, LINES, SAMPS, XADD, IST
	integer*4 parb(100)
	character*80 L
	CHARACTER*4 V
	DATA V/'NET>'/
c
	CALL XTINIT(ITYPE,LINES,SAMPS)
	read (*,1), L
1	format (a)
        if (L .eq. ' ') L = 'CONTINUE'
c
        CALL XQINI(parb,500,XABORT)
        CALL XQSTR(parb,'CMD',1,L,XADD,IST)
        CALL XVQOUT(parb,IST)
	return
	end
