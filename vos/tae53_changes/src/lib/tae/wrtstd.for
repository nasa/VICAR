C	>>>VAX/VMS ONLY<<<<
C 
C	WRTSTD.  Write to standard output device if different from terminal.
C	
C	
C	CHANGE LOG:
C
C	15-sep-86	Allow carriage control in FORMAT statement...dm
C


	SUBROUTINE  WRTSTD(MSG, LENGTH) 

	BYTE 		MSG(1)			
	INTEGER		LENGTH			

	INTEGER		OUTLEN			
	LOGICAL 	TERMNL			
	INTEGER		LUN			
	INTEGER		TTYPE			

	CALL GETLUN_TAE(LUN)
	CALL GETSTD(TERMNL)
	IF (.NOT. TERMNL) THEN			
	    OUTLEN = LENGTH
	    IF (LENGTH .GT. 132) OUTLEN = 132	
	    WRITE(LUN, 100) (MSG(I),I=1,OUTLEN)
	ENDIF
100	FORMAT(1X, 132(:,A1))
	RETURN
	END

C
C	PREOPN. Prepare for opening the standard output file.
C	This is a no-op module under VAX/VMS.
C

	SUBROUTINE PREOPN(LUN, STDREC, OPENED)

	INTEGER		LUN
	CHARACTER*(*)	STDREC
	LOGICAL 	OPENED

	RETURN
	END	
	
