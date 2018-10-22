C
C    CFORM -- PROGRAM TO CONVERT BETWEEN DIFFERENT STORAGE FORMATS FOR
C         IMAGES AND TO PERFORM LINEAR TRANSFORMATIONS ON THEM
C
C  ORIGINAL WRITTEN BY J. J. LORRE
C  5/83 - 6/83    ... TRANSLATED FROM PL/1 TO FORTRAN BY DAN STANFILL
C  84-12-17  ...LWK... PARTIAL (I/O ONLY) VICAR2 CONVERSION TO FIX LABEL
C			PROCESSING
C  85-1-26   ...LWK... CONVERT PARAMS TO VICAR2 & FIX BUGS
C  85-2-18   ...LWK... ENABLE 'COMPLEX' TYPE, REMOVE 'FORMAT' PARAMETER,
C			CHECK FOR OVERFLOW IN REAL TO INTEGER CONVERSION
C  89-5-26   ...TCG... SUPPORT 'COMP' TYPE
C  92-3-26   ...NDR... PORTED TO UNIX; RENAMED 'CFORM' FROM 'C'
C  92-3-38   ...SP.... removed some obsolete code for input FORMAT = REAL
C                      but PIX_SIZE=8, used XVSIZE in place of in-line code,
C                      used NINT of output DN for rounding for BYTE, HALF, 
C                      and FULL output (the old code did NINT(OF)).
C                      Corrected code for adding history label item.
C  93-07-13  ...LWK... ADDED BINARY OPTION AND BSQ FORMAT SUPPORT
C  94-02-18  ...LWK... added U_FORMAT back to XVOPEN of output, inadvertently
C			omitted in previous mod
C  98-06-05  ...RRP... Removed extra ')' from the pdf.
c
c
c  00-10-10  ...bam..  Corrected buffer allocations as per DLR error report.
c                      both orecl and irecl compares are corrected.


      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER (A-Z)

      INTEGER BUFSIZE
      PARAMETER (BUFSIZE=1000000)
      INTEGER FIN(1),FOUT(1)
      INTEGER*2 HIN(1),HOUT(1)
      REAL FLIN(1),FLOUT(1),SLP,OFF,SLOFF(2),RANG(2,2),X
      EQUIVALENCE (SLOFF(1),SLP), (SLOFF(2),OFF)
      DOUBLE PRECISION BUFIN(BUFSIZE),BUFOUT(BUFSIZE)
      DOUBLE PRECISION DIN(1),DOUT(1),D
      COMPLEX CIN(1),COUT(1)
      CHARACTER*7 IFMT, OFMT, UFMT
      CHARACTER*3 ORG
      INTEGER SIZE(4)
      REAL FI4MAX/2.14748E9/    ! MAX. FOR INTEGER*4 CONVERSION
      EQUIVALENCE (SIZE(1),SL), (SIZE(2),SS), (SIZE(3),NLO),
     & (SIZE(4),NSO)

      CHARACTER*80 MSG,MSG2,MSG3
      EQUIVALENCE (BUFIN(1),HIN(1),FIN(1),FLIN(1),DIN(1),CIN(1))
      EQUIVALENCE (BUFOUT(1),HOUT(1),FOUT(1),FLOUT(1),
     2             DOUT(1),COUT(1))
      LOGICAL XVPTST, BINARY

      CALL XVMESSAGE('*** CFORM version 2018-03-29 ***',' ')

      BINARY = XVPTST('BINARY')

      CALL XVUNIT(IUN,'INP',1,STAT, ' ')
      IF (BINARY) THEN 
	CALL XVOPEN(IUN,STAT,'OPEN_ACT',' ','IO_ACT','SA',
     1 'COND','BINARY',' ')
      ELSE 
	CALL XVOPEN(IUN,STAT,'OPEN_ACT',' ','IO_ACT','SA', ' ')
      ENDIF

      CALL XVGET( IUN, STAT, 'FORMAT', IFMT, 'NLB', NLBI, 'NBB', NSBI,
     & 'ORG', ORG, 'PIX_SIZE', IPIX, ' ')
      IF (ORG.NE.'BSQ') CALL MABEND(
     & ' ** CFORM ONLY SUPPORTS BSQ FORMAT **')
      IF (.NOT.BINARY) THEN
	NLBI = 0
	NSBI = 0
      ENDIF
      IF (IFMT.EQ.'WORD') IFMT = 'HALF'     ! FIX UP NON-STANDARD FORMATS
      IF (IFMT.EQ.'LONG') IFMT = 'FULL'
      IF (IFMT.EQ.'COMPLEX') IFMT = 'COMP'

      CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI)
      CALL XVBANDS( SB, NBO, NBI)

C  CAN NOT COPY BINARY PARTS UNAMBIGUOUSLY IF OUTPUT IS A
C  SUB-WINDOW OF INPUT:
      IF ( (( SL .NE. 1 ) .OR. ( SS .NE. 1 ) .OR. ( SB .NE. 1 ) .OR.
     *	   ( NLO .NE. NLI) .OR. ( NSO .NE. NSI) .OR. ( NBO .NE. NBI ))
     *     .AND. BINARY ) CALL MABEND(
     * ' ** CANNOT SPECIFY "BINARY" WITH SIZE WINDOW **')

C  OUTPUT FORMAT PARAMETER
      CALL XVPARM( 'OFORM', OFMT, I, IDEF,' ')
      IF (I.EQ.0) THEN            ! DEFAULTS:
	IF (IFMT.EQ.'BYTE') THEN
	  OFMT = 'HALF'
	ELSE
	  OFMT = 'BYTE'
	ENDIF
      ENDIF
      IF (OFMT.EQ.'REAL8') OFMT = 'DOUB'     ! FIX UP NON-STANDARD FORMATS
      IF (OFMT.EQ.'COMPLEX') OFMT = 'COMP'

      IF (OFMT.EQ.'BYTE') OPIX = 1
      IF (OFMT.EQ.'HALF') OPIX = 2
      IF (OFMT.EQ.'FULL' .OR. OFMT.EQ.'REAL') OPIX = 4
      IF (OFMT.EQ.'DOUB' .OR. OFMT.EQ.'COMP') OPIX = 8

C  CHECK BUFFER LENGTHS:
      IRECL = NSBI+IPIX*NLI
      ORECL = NSBI+OPIX*NLO
      IF (IRECL.GT.(BUFSIZE*8)) CALL MABEND(
     & ' ** INPUT LINE LENGTH EXCEEDS BUFFER SIZE **')
      IF (ORECL.GT.(BUFSIZE*8)) CALL MABEND(
     & ' ** OUTPUT LINE LENGTH EXCEEDS BUFFER SIZE **')

C  IF BINARY OPTION WILL CAUSE LOSS OF HEADER DATA, CANCEL TASK;  IF IT
C  EXPANDS HEADERS, ISSUE WARNING:
      IF (BINARY .AND. NLBI.GT.0) THEN
	IF (OPIX.LT.IPIX) THEN
	  CALL MABEND(
     & ' ** Format change will cause loss of binary header data! **')
	ELSEIF (OPIX.GT.IPIX) THEN
	  CALL XVMESSAGE(' Warning: format change will increase ' //
     & 'lengths of binary headers!',' ')
	  CALL XVMESSAGE(' Binary headers will be padded with zeroes.',
     1 ' ')
	ENDIF
      ENDIF

      IF (IFMT.EQ.'BYTE') THEN
	CALL XVCLOSE( IUN, STAT,' ')
	IF (BINARY) THEN 
	  CALL XVOPEN( IUN, STAT, 'OPEN_ACT', ' ', 'IO_ACT', 'SA',
     1  'COND', 'BINARY', 'U_FORMAT', 'HALF',' ')
	ELSE 
	  CALL XVOPEN( IUN, STAT, 'OPEN_ACT', ' ', 'IO_ACT', 'SA',
     1  'U_FORMAT', 'HALF',' ')
	ENDIF
      ENDIF

C  SLOPE/OFFSET
      CALL XVPARM( 'SO', SLOFF, I, IDEF,' ')
      IF (I.EQ.0) THEN
        SLOFF(1) = 1.0
        SLOFF(2) = 0.0
	CALL XVPARM( 'IRANGE', RANG(1,1), IRCNT ,IDEF,' ')
	CALL XVPARM( 'ORANGE', RANG(1,2), ORCNT ,IDEF,' ')
	IF (IRCNT.NE.ORCNT) THEN
	  CALL MABEND(' ** SPECIFY BOTH IRANGE/ORANGE, OR NEITHER **')
	ELSEIF (IRCNT.NE.0) THEN
	  SLP = (RANG(1,2)-RANG(2,2))/(RANG(1,1)-RANG(2,1))
	  OFF = RANG(1,2)-SLP*RANG(1,1)
	ENDIF
      ENDIF

C  old code to CORRECT FOR TRUNCATION ERRORS deleted. 
C  NINT function used instead in main loop below.
	  
      WRITE(MSG,1234) SLP,OFF
 1234 FORMAT('OUT = IN *',F10.3,'+',F10.3)
      CALL XVMESSAGE( MSG, ' ')

C  FINAL IN/OUT FORMAT MESSAGE 

      WRITE(MSG2,1235) IFMT(:4)
 1235 FORMAT('INPUT FORMAT = ',A)
      CALL XVMESSAGE(MSG2,' ')
	  
      WRITE(MSG3,1236) OFMT(:4)
 1236 FORMAT('OUTPUT FORMAT = ',A)
      CALL XVMESSAGE(MSG3,' ')

C  OPEN OUTPUT
      CALL XVUNIT( OUN, 'OUT', 1, STAT,' ')
      UFMT = OFMT
      IF (OFMT.EQ.'BYTE') UFMT = 'HALF'
      IF (BINARY) THEN
	CALL XVOPEN( OUN, STAT, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS', NSO,
     1 'U_NB', NBO, 'O_FORMAT', OFMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     2 'U_FORMAT', UFMT, 'COND', 'BINARY', 'U_NBB', NSBI, 'U_NLB', NLBI,
     3 ' ')
      ELSE 
	CALL XVOPEN( OUN, STAT, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS', NSO,
     1 'U_NB', NBO, 'O_FORMAT', OFMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     2 'U_FORMAT', UFMT, ' ')
      ENDIF

      CALL XLADD( OUN, 'HISTORY', 'CONV', MSG, STAT, 'FORMAT', 'STRING',
     & 'ULEN', 31, ' ')

      IF (BINARY .AND. NLBI.GT.0) THEN
	IF (ORECL.GT.IRECL) CALL MVE( 1, ORECL-IRECL, 0, BUFIN(ORECL+1),
     1      0, 1)
	DO REC = 1,NLBI
	  CALL XVREAD( IUN, BUFIN, STAT, ' ')
	  CALL XVWRIT( OUN, BUFIN, STAT, ' ')
	ENDDO 
      ENDIF

C  MAIN BANDS/LINES LOOP
      DO IB = SB,SB+NBO-1
      DO IL = 1,NLO

	LINE = NLBI+SL-1+IL
		! VICAR BINARY LABELS ARE IN BAND 1 ONLY!!
	IF (BINARY.AND.IB.GT.1) LINE = SL-1+IL

		! READ BINARY PREFIXES INTO END OF OUTPUT BUFFER:
	IF (BINARY) CALL XVREAD( IUN, BUFOUT(NLO+1), STAT, 'LINE', LINE,
     1 'NSAMPS', NSBI, 'BAND', IB, ' ')
	CALL XVREAD( IUN, BUFIN, STAT, 'LINE', LINE, 'SAMP', NSBI+SS,
     1 'NSAMPS', NSO, 'BAND', IB, ' ')
	  
	IF (IFMT.EQ.'BYTE' .OR. IFMT.EQ.'HALF') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
C             Run tstcform.pdf and compare output with tstcform.log.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      I=NINT( HIN(J)*SLP+OFF)
	      IF (I.LT.0) I = 0
	      IF (I.GT.255) I = 255
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      I = NINT( HIN(J)*SLP+OFF)
	      IF (I.LT.-32768) I = -32768
	      IF (I.GT.32767) I = 32767
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FOUT(J) = NINT( HIN(J)*SLP+OFF)
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FLOUT(J) = HIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      DOUT(J)=HIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      COUT(J) = CMPLX(HIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'FULL') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      I = NINT( FIN(J)*SLP+OFF)
	      IF (I.LT.0) I=0
	      IF (I.GT.255) I = 255
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      I = NINT( FIN(J)*SLP+OFF)
	      IF (I.LT.-32768) I = -32768
	      IF (I.GT.32767) I = 32767
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FOUT(J) = NINT( FIN(J)*SLP+OFF)
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FLOUT(J) = FIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      DOUT(J) = FIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      COUT(J) = CMPLX(FIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'REAL') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.0.) THEN
		HOUT(J) = 0
	      ELSEIF (X.GT.255.) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.-32768.) THEN
		HOUT(J) = -32768
	      ELSEIF (X.GT.32767.) THEN
		HOUT(J) = 32767
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FLOUT(J) = FLIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      DOUT(J) = FLIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      COUT(J) = CMPLX(FLIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'DOUB') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      D = DIN(J)*SLP+OFF
	      IF (D.LT.0.) THEN
		HOUT(J) = 0
	      ELSEIF (D.GT.255.) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( D)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      D = DIN(J)*SLP+OFF
	      IF (D.LT.-32768.) THEN
		HOUT(J) = -32768
	      ELSEIF (D.GT.32767.) THEN
		HOUT(J) = 32767
	      ELSE
		HOUT(J) = NINT( D)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = DIN(J)*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FLOUT(J) = DIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      DOUT(J) = DIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      COUT(J) = CMPLX(DIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'COMP') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.0) THEN
		HOUT(J) = 0
	      ELSEIF (X.GT.255) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.-32768.) THEN
		HOUT(J) = -32768.
	      ELSEIF (X.GT.32767.) THEN
		HOUT(J) = 32767.
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      FLOUT(J) = CABS(CIN(J))*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      DOUT(J) = CABS(CIN(J))*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
C             This dead code (NSO is always > 0) is inserted to prevent the
C             optimizer from breaking the incrementing of J.
C             Current gfortran version is 4.8.5-11.el7.
C             When gfortran is updated, this can be removed to test for a fix.
              if (NSO.lt.0) then
                 write(*,*) "j=",j
              endif   

	      COUT(J) = CIN(J)*SLP+OFF
	    END DO
	  END IF
C
	END IF

	LINE = NLBI+IL
	IF (BINARY.AND.IB.GT.1) LINE = IL
	IF (BINARY) CALL XVWRIT( OUN, BUFOUT(NLO+1), STAT, 'LINE', LINE,
     1 'NSAMPS', NSBI, 'SAMP', 1, 'BAND', IB, ' ')
        CALL XVWRIT( OUN, BUFOUT, STAT, 'LINE', LINE,
     1 'NSAMPS', NSO,'SAMP', NSBI+1, 'BAND', IB, ' ')
C
      END DO
      END DO
C  END OF MAIN BANDS & LINES LOOP

      CALL XVMESSAGE('CONVERSION COMPLETE',' ')
      CALL XVCLOSE( IUN, STAT,' ')
      CALL XVCLOSE( OUN, STAT,' ')

      RETURN
      END
