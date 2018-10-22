C 2 JAN 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
	INCLUDE 'VICMAIN_FOR'
C
	SUBROUTINE MAIN44
        IMPLICIT NONE
	INTEGER*2 BUF(1000000)
        INTEGER WRGR, STATUS, INUNIT, ISTAT, ISL, ISS, NL, NS, NLI, NSI
        INTEGER IDIM, ICNT, IDEF, IBG, NLPERBLK, NBLKS,LINOFFSET,CLGR,I
C
        CALL IFMESSAGE('RASTOGRAF version 2-JAN-95')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')	! open input dataset
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     *              'U_FORMAT','HALF',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLI,NSI)		! get size field
	CALL XVPARM('DIM',IDIM,ICNT,IDEF,1)	! get dimension of output
	CALL XVPARM('BACK',IBG,ICNT,IDEF,1)	! get background value
	STATUS = WRGR (1,1,IDIM)		! open ibis graphics-1 file
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C
	IF (ISL.NE.1) CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',ISL-1,' ')
	NLPERBLK = MIN(1000000/NS,NL)
	NBLKS = NL/NLPERBLK
C						! loop thru blocks of lines
	DO I=1,NBLKS
	    LINOFFSET = (I-1)*NLPERBLK
	    CALL TOVECTOR(INUNIT,BUF,NLPERBLK,NS,IBG,LINOFFSET,IDIM,
     +			  ISS)
	END DO
	NLPERBLK = NL-NBLKS*NLPERBLK		! check for extra lines
	IF (NLPERBLK .NE. 0) CALL TOVECTOR(INUNIT,BUF,NLPERBLK,NS,IBG,
     +					   LINOFFSET,IDIM,ISS)
C
	STATUS = CLGR (1)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	RETURN
	END
C***************************************************************************
	SUBROUTINE TOVECTOR(INUNIT,BUF,NL,NS,IBG,LINOFFSET,IDIM,ISS)
        IMPLICIT NONE
C
        INTEGER INUNIT, ISTAT, ISS, NL, NS, IDIM, IBG, LINOFFSET, I, J
	INTEGER*2 BUF(NS,NL)
C
	DO I=1,NL
	   CALL XVREAD(INUNIT,BUF(1,I),ISTAT,'SAMP',ISS,'NSAMPS',NS,' ')
	END DO
C					loop through all pixels, searching for
C					vector
	DO I=1,NL
	    DO J=1,NS
		IF (BUF(J,I).NE.IBG) CALL BUILDVEC(BUF,NL,NS,I,J,IBG,
     +						    LINOFFSET,IDIM)
	    END DO
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE BUILDVEC(BUF,NL,NS,LINE,ISAMP,IBG,LINOFFSET,IDIM)
C
        IMPLICIT NONE
        INTEGER STATUS, NL, NS, IDIM, IBG, LINOFFSET, LINE, ISAMP, L, M
        INTEGER PUTGR, IDN
	INTEGER*2 BUF(NS,NL)
        REAL A,B,C
	LOGICAL QFIRST
C
	QFIRST = .TRUE.
	L = LINE
	M = ISAMP
	IF (IDIM.EQ.3) GO TO 200
C					*************************************
C					Processing for 2-D output starts here
C					*************************************
  100	CONTINUE
	A = L+LINOFFSET			! Output the current vertex
	B = M
	STATUS = PUTGR(1,A,B,0.0)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C					The following tests look at each of
C					the neighboring 8 pixels, trying to
C					find a new vector to follow. If it
C					finds one, it follows it, erasing the
C					vector as it goes.
	IF (     M.LT.NS .AND.               BUF(M+1,L).NE. IBG) THEN
	    DO WHILE (M.LT.NS .AND. BUF(M+1,L).NE.IBG)
		BUF(M+1,L) = IBG
		M = M+1
	    END DO
	ELSE IF (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).NE.IBG) THEN
	    DO WHILE (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).NE.IBG)
		BUF(M+1,L+1) = IBG
		M = M+1
		L = L+1
	    END DO
	ELSE IF (	    L.LT.NL .AND. BUF(M  ,L+1).NE.IBG) THEN
	    DO WHILE (L.LT.NL .AND. BUF(M,L+1).NE.IBG)
		BUF(M,L+1) = IBG
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).NE.IBG) THEN
	    DO WHILE (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).NE.IBG)
		BUF(M-1,L+1) = IBG
		M = M-1
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. 		     BUF(M-1,L  ).NE.IBG) THEN
	    DO WHILE (M.GT.1 .AND. BUF(M-1,L).NE.IBG)
		BUF(M-1,L) = IBG
		M = M-1
	    END DO
	ELSE IF (M.GT.1  .AND. L.GT.1  .AND. BUF(M-1,L-1).NE.IBG) THEN
	    DO WHILE (M.GT.1 .AND. L.GT.1  .AND. BUF(M-1,L-1).NE.IBG)
		BUF(M-1,L-1) = IBG
		M = M-1
		L = L-1
	    END DO
	ELSE IF (	       L.GT.1  .AND. BUF(M  ,L-1).NE.IBG) THEN
	    DO WHILE (L.GT.1 .AND. BUF(M,L-1).NE.IBG)
		BUF(M,L-1) = IBG
		L = L-1
	    END DO
	ELSE IF (M.LT.NS .AND. L.GT.1  .AND. BUF(M+1,L-1).NE.IBG) THEN
	    DO WHILE (M.LT.NS .AND. L.GT.1 .AND. BUF(M+1,L-1).NE.IBG)
		BUF(M+1,L-1) = IBG
		M = M+1
		L = L-1
	    END DO
	ELSE				! finished
	    IF (QFIRST) THEN
		A = L+LINOFFSET			! Output the first vertex again
		B = M
		STATUS = PUTGR(1,A,B,0.0)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    END IF
	    STATUS = PUTGR(1,0.0,0.0,0.0)	! end of vector set; send
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    RETURN				! terminator and return
	END IF
C
	QFIRST = .FALSE.
	GO TO 100
C
C
C					*************************************
C					Processing for 3-D output starts here
C					*************************************
  200	CONTINUE
	IDN = BUF(M,L)
	C = IDN
  300	CONTINUE
	A = L+LINOFFSET			! Output the current vertex
	B = M
	STATUS = PUTGR(1,A,B,C)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C					The following tests look at each of
C					the neighboring 8 pixels, trying to
C					find a new vector to follow. If it
C					finds one, it follows it, erasing the
C					vector as it goes.
	IF      (M.LT.NS .AND. 		     BUF(M+1,L  ).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. BUF(M+1,L).EQ.IDN)
		BUF(M+1,L) = IBG
		M = M+1
	    END DO
	ELSE IF (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).EQ.IDN)
		BUF(M+1,L+1) = IBG
		M = M+1
		L = L+1
	    END DO
	ELSE IF (	       L.LT.NL .AND. BUF(M  ,L+1).EQ.IDN) THEN
	    DO WHILE (L.LT.NL .AND. BUF(M,L+1).EQ.IDN)
		BUF(M,L+1) = IBG
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).EQ.IDN) THEN
	    DO WHILE (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).EQ.IDN)
		BUF(M-1,L+1) = IBG
		M = M-1
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. 		     BUF(M-1,L  ).EQ.IDN) THEN
	    DO WHILE (M.GT.1 .AND. BUF(M-1,L).EQ.IDN)
		BUF(M-1,L) = IBG
		M = M-1
	    END DO
	ELSE IF (M.GT.1  .AND. L.GT.1  .AND. BUF(M-1,L-1).EQ.IDN) THEN
	    DO WHILE (M.GT.1 .AND. L.GT.1  .AND. BUF(M-1,L-1).EQ.IDN)
		BUF(M-1,L-1) = IBG
		M = M-1
		L = L-1
	    END DO
	ELSE IF (	       L.GT.1  .AND. BUF(M  ,L-1).EQ.IDN) THEN
	    DO WHILE (L.GT.1 .AND. BUF(M,L-1).EQ.IDN)
		BUF(M,L-1) = IBG
		L = L-1
	    END DO
	ELSE IF (M.LT.NS .AND. L.GT.1  .AND. BUF(M+1,L-1).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. L.GT.1 .AND. BUF(M+1,L-1).EQ.IDN)
		BUF(M+1,L-1) = IBG
		M = M+1
		L = L-1
	    END DO
	ELSE				! finished
	    IF (QFIRST) THEN
		A = L+LINOFFSET			! Output the first vertex again
		B = M
		STATUS = PUTGR(1,A,B,C)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    END IF
	    STATUS = PUTGR(1,0.0,0.0,0.0)	! end of vector set; send
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    RETURN				! terminator and return
	END IF
C
	QFIRST = .FALSE.
	GO TO 300
	END
