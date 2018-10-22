      INCLUDE 'VICMAIN_FOR'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   REVISION HISTORY
C      3-97   SP  Made portable for UNIX.  Removed non-portable carriage-control
C                 modifier in FORTRAN OPEN statement.  Added "1X" in FORMAT 
C                 statements to compensate.  Changed PSCL from I*2 to I*4
C                 to work on all machines with XLGET.  Corresponding changes
C                 for DCODE parameter for PRNT and DIVV.  Added FORMAT INT
C                 for xlget.

	subroutine main44
	implicit integer (A-Z)
	INTEGER*4 HBUF(32768),RNG(2),INSTANCES(20),PSCL
	INTEGER*2 INBUF(60000)
        CHARACTER*1 TAB
	CHARACTER*5 FORM
        CHARACTER*80 TABLE_DS
	CHARACTER*8 TASKS(20)
	LOGICAL XVPTST
C
	CALL XVUNIT(INU,'INP',1,IST,' ')
	CALL XVOPEN(INU,IST,'U_FORMAT','HALF','IO_ACT', 'SA' ,' ')
	IF (IST .NE. 1) GO TO 998
	CALL XVGET(INU,IST,'FORMAT',FORM,'NS',NS,'NL',NL,' ')
	IF (FORM .NE. 'HALF' .AND. FORM .NE. 'BYTE') THEN
		CALL XVMESSAGE('ONLY BYTE AND HALF FORMATS HANDLED',' ')
		CALL ABEND
	END IF
C
C-------GET PARAMETERS
	CALL XVPARM('RANGE',RNG,ICNT,IDEF,2)
	IF (ICNT .EQ. 2) GO TO 5
	IF (FORM .EQ. 'BYTE') THEN
		RNG(1) = 0
		RNG(2) = 255
	ELSE 
		RNG(1) = 0
		RNG(2) = 4096
	END IF

5	CONTINUE
	LOW = MAX(0,RNG(1))
	HIGH = MIN(32767,RNG(2))
C
c-------Was PICSUM run?  If so, get scale (number of images used) so it
c-------can be used in divison below.
C-------Search for last value of picture scale in the label.
        ICNT = 20
        CALL XLHINFO(INU,TASKS,INSTANCES,ICNT,IND,' ')
        DO J=ICNT,1,-1		
           CALL XLGET(INU,'HISTORY','PICSCALE',PSCL,IND,'FORMAT','INT',
     &            'HIST',TASKS(J),'INSTANCE',INSTANCES(J),' ')
           IF (IND.EQ.1) GOTO 7
        ENDDO
        PSCL = 1		!Default picture scale
7	CONTINUE
	CALL PRNT(4,1,PSCL,'IMAGES SUMMED=.')
C
C-------Could make my own loop for dividing and histograming but
c-------might be faster to call the two assembler routines (div & hsub)
C
C-------ACCUMULATE HISTOGRAM
	DO I=1,NL
	   CALL XVREAD(INU,INBUF,IST,' ')
	   IF (PSCL .GT. 1) CALL DIVV(-6,NS,PSCL,INBUF,0,1)
	   CALL HSUB(2,NS,INBUF,HBUF,LOW,HIGH)
	END DO
	CALL XVCLOSE(INU,IST,' ')
C
C-------WRITE OUTPUT FILE 
C-------ASCII TEXT WITH COLUMNS: DN and POPULATION
	CALL XVPARM('TABLE',TABLE_DS,ICNT,IDEF,1) 
	OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',
     1       IOSTAT=JST,ERR=999)
	TAB= CHAR(9)

C-------Write header and table, leave out the DN column
	IF (XVPTST('NODN')) THEN
	  IF (.NOT.XVPTST('NOHEADER'))	
     1         WRITE(12,8) 'POPULATION',TAB

	  DO I=LOW,HIGH
		WRITE(12,11) HBUF(I-LOW+1),TAB
	  END DO

	ELSE

C-------Write header and table with DN column
	  IF (.NOT.XVPTST('NOHEADER'))	
     1         WRITE(12,6) 'DN',TAB,'POPULATION',TAB

	  DO I=LOW,HIGH
		WRITE(12,10) I,TAB,HBUF(I-LOW+1),TAB
	  END DO

	END IF


	CLOSE(12)
	RETURN
C.................ERROR HANDLING
998	CALL XVMESSAGE('ERROR OPENING INPUT FILE',' ')
	CALL ABEND
999	CALL XVMESSAGE('ERROR OPENING OUTPUT FILE',' ')
	CALL PRNT(4,1,JST,'IOSTAT.')
	CALL ABEND
C
8	FORMAT(1X,A10,A1)
6	FORMAT(1X,A2,A1,A10,A1)
10	FORMAT(1X,I5,A1,I7,A1)
11	FORMAT(1X,I7,A1)
	END
