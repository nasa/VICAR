
C
C		program: "TIEPARM"
C		author: Leo M. Bynum
C		date written: Feb 14, 1985
C
C		purpose: To take an IBIS interface file containing
C			tiepoint coordinates as input and output a
C			parameter file of these same coordinates to
C			be used as input for geometric correction
C			programs. (TOIBIS mode transforms tiepoint
C                       coordinates in a parameter file into an
C                       IBIS interface file.)
C
C		Revisions:
C			1	Put in delta Z tiepoints (3 columns)
C				K. F. Evans	April 1986
C
C                       2       Added TOIBIS and PARMS parameters to allow
C                               conversion from parameter file to IBIS 
C                               interface file.
C                               Steve Pohorsky  June 1986
C
C			3	Corrected TOIBIS mode to be independent
C				of NAH and NAV - for none gridded tiepoints.
C				Frank Evans   March 1987
C
C			4	Ported to UNIX.
C				Randy Schenk (CRI) May 1995
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	IMPLICIT INTEGER (A-Z)
	LOGICAL	XVPTST
	INTEGER	COLS(5)
	REAL*4	DATA(5)
	REAL 	TIEPOINT(5*40000)
	CHARACTER*72  OUTFILE

C=======================================================

      CALL IFMESSAGE('TIEPARM Version 8-May-95')
      IF ( .NOT. XVPTST( 'TOIBIS' )  )   THEN

C......................................START OF REGULAR (TOPARM) MODE SECTION.
C		open input file
C
	CALL XVUNIT( UNIT,'INP', 1, STATUS,' ')
	IF (STATUS.NE.1) THEN
	   CALL XVMESSAGE('INPUT FILE NOT FOUND by XVUNIT',' ')
	   CALL ABEND
	ENDIF
	CALL XVPARM ('COLS', COLS, COLCOUNT, DEF, 5)
	CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
	CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
C
C		open output file
C
	CALL XVP ('OUT', OUTFILE, COUNT)
	CALL XVP ('NAV', NAV,     COUNT)
	CALL XVP ('NAH', NAH,     COUNT)
	CALL XVPOPEN(STATUS,3,4*COLCOUNT*CLEN + 8,OUTFILE,'U',OUNIT)
	IF (STATUS .NE.1) THEN
	  CALL XVMESSAGE('OUTPUT FILE CANNOT BE OPENED',' ')
	  CALL ABEND
	ENDIF
C
C		read in the column numbers to be used for
C		newline, newsample, oldline, and oldsample values.
C		 or line, sample, and delta Z values.
C
C
C		read and store tiepoint coordinates
C
	TINDX = 1
	CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',
     +                        COLS,COLCOUNT,'REAL',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	DO ROW = 1, CLEN
C	  CALL GETREC(IUNIT, COLCOUNT, COLS, DATA, ROW, CLEN, A)
	  CALL IBIS_RECORD_READ(RECORD,DATA,ROW,STATUS)

	  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TIEPOINT(TINDX) = DATA(1)
	  TIEPOINT(TINDX + 1) = DATA(2)
	  TIEPOINT(TINDX + 2) = DATA(3)
	  TIEPOINT(TINDX + 3) = DATA(4)
	  TIEPOINT(TINDX + 4) = DATA(5)
	  TINDX = TINDX + COLCOUNT
	ENDDO

	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
C		output tiepoint coordinates
C
	CALL XVPOUT(STATUS, 'NAV', NAV, 'INT', 1)
	CALL XVPOUT(STATUS, 'NAH', NAH, 'INT', 1)
	CALL XVPOUT(STATUS, 'TIEPOINT', TIEPOINT, 'REAL', CLEN*COLCOUNT)
C
	CALL XVPCLOSE(STATUS)

C......................................END OF REGULAR (TOPARM) MODE SECTION.

      ELSE

C......................................START OF TOIBIS MODE SECTION.

C..PROCESS PARAMETERS

        CALL XVPARM( 'COLS', COLS, COLCOUNT, IDEF, 5 )
        MAXCOL = COLS(1)
        DO I = 2, COLCOUNT
            MAXCOL = MAX( MAXCOL, COLS(I) )
        END DO

        CALL XVPARM( 'TIEPOINT', TIEPOINT, NTIE, IDEF,(5*40000))
        NTIE = NTIE / COLCOUNT

        CALL XVPARM( 'NCOL', NCOL, COUNT, IDEF, COLCOUNT)
	IF (IDEF .EQ. 1)  NCOL = MAXCOL

        CALL XVPCNT( 'INP', NI )         ! NUMBER OF INP AND OUT FILES.
        CALL XVPCNT( 'OUT', NO )
        IF ( (NI .NE. 0 .OR. NO .NE. 1)  .AND.
     .       (NI .NE. 1 .OR. NO .NE. 0)  )
     .       CALL MABEND( ' ERROR:INCORRECT NUMBER OF FILES SPECIFIED.')

C..OPEN IBIS INTERFACE FILE. USE UPDATE MODE IF FILE ALREADY EXISTS.

        IF (NO .EQ. 1)  THEN
	  CLEN = NTIE
C            CALL WRFIL( OUNIT, 1, CLEN, NCOL, NOFILE)
	  CALL XVUNIT( IFUNIT,'OUT', 1, STATUS,' ')
	  IF (STATUS.NE.1) THEN
	    CALL XVMESSAGE('OUTPUT FILE NOT FOUND by XVUNIT',' ')
	    CALL ABEND
	  ENDIF
	  CALL IBIS_FILE_OPEN(IFUNIT,IBIS,'WRITE',NCOL,CLEN,
     +                          ' ',' ',STATUS)
        ELSE                         ! UPDATE the INP file
C            CALL RDFIL( OUNIT, 1, CLEN, NCOL, NOFILE)
	  CALL XVUNIT( IFUNIT,'INP', 1, STATUS,' ')
	  IF (STATUS.NE.1) THEN
	    CALL XVMESSAGE('OUTPUT FILE NOT FOUND by XVUNIT',' ')
	    CALL ABEND
	  ENDIF
	  CALL IBIS_FILE_OPEN(IFUNIT,IBIS,'UPDATE',0,0,' ',' ',STATUS)
	  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
	  CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
          IF ( NCOL .LT. MAXCOL .OR. CLEN .NE. NTIE) CALL MABEND(
     .      '  ERROR: PARAMS DO NOT AGREE WITH SIZE OF IBIS FILE')
        END IF

C..WRITE DATA TO IBIS INTERFACE FILE.

        TINDX = 1
	CALL IBIS_RECORD_OPEN(IBIS,RECORD,'FORMAT:REAL',
     +                        COLS,COLCOUNT,'REAL',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

       DO I = 1, CLEN
           ROW = I
C           CALL PUTREC(OUNIT,COLCOUNT,COLS,TIEPOINT(TINDX),ROW,CLEN,A)
	    CALL IBIS_RECORD_WRITE(RECORD,TIEPOINT(TINDX),ROW,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           TINDX = TINDX+ COLCOUNT
        END DO
	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      END IF

C......................................END OF TOIBIS MODE SECTION.

      RETURN
      END
