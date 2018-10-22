	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C---- VICAR/IBIS PROGRAM ROWOP.
C     PURPOSE:  TO DELETE OR SELECT SELECTED ROWS OF THE INPUT IBIS
C	INTERFACE FILE WHILE COPYING THE FILE
C
C	REVISION 2	FRANK EVANS	MARCH 1986
C			ADDED SELECT OPTION, PUT IN NCOPY PARAMETER,
C			MADE THE PDF AGREE WITH THE CODE
C
C	REVISION 3	FRANK EVANS	JUNE 1987
C			ADDED PICK OPTION, IMPROVED HELP FILE
C
C	REVISION 4	NILES RITTER	MAY 1994
C			PORTED TO UNIX; USES IBIS2 CALLS
c
c       REVISION 5      BARBARA MCGUFFIE  OCTOBER 26, 1998
c			MODIFIED FOR DOUBLE PRECISION DATA
C
	IMPLICIT NONE

	REAL	BUF(100), PREC, RANGE(200), LOWER(100), UPPER(100)
        real*8  dbuf(100)
	INTEGER	KEYCOL(100), NCOPY(2)
	INTEGER STATUS,IBISIN,IBISOUT,USIZE,OUTSIZE
        INTEGER INRECORD,INRECORDD
	BYTE	COL(4000000)
	INTEGER*4 FULLCOL(1000000)
	INTEGER NKEY,DEF,NRANGE,COUNT,NUMCOPIES,INDEXCOL
	INTEGER RUNIT,WUNIT,CLEN,NCOL,I,OUTLEN,IREC
	INTEGER ICOL,PTR,N,INPTR,J,OFFSET,GR1DIM
	LOGICAL*1 KEEP(1000000)
	LOGICAL  DELETE, SELECT, PICK, XVPTST
	CHARACTER*12 ORG,CFORM
	CHARACTER*5 FORMATS(1024)
	EQUIVALENCE(COL,FULLCOL)  !Only used to save space.

        integer doubflag


C	Get the input parameters

	DELETE = XVPTST('DELETE')
	SELECT = XVPTST('SELECT')
	PICK = XVPTST('PICK')
	CALL XVPARM ('KEYCOL', KEYCOL, NKEY, DEF, 100)
	CALL XVPARM ('RANGE', RANGE, NRANGE, DEF, 200)
	CALL XVP ('PREC', PREC, COUNT)
	CALL XVP ('NCOPY', NCOPY, COUNT)
	NUMCOPIES = NCOPY(1)
	INDEXCOL = NCOPY(2)
	IF (COUNT .EQ. 1)  INDEXCOL = 0

c		Open the input interface file
	CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
	CALL XVP('GR1DIM',GR1DIM,DEF)

	CALL IBIS_FILE_OPEN( RUNIT, IBISIN, 'READ',
     +     GR1DIM, 0, ' ', ' ',STATUS )
     	IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)

C    Get the File description	
	CALL IBIS_FILE_GET( IBISIN, 'NR', CLEN, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'NC', NCOL, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'ORG', ORG, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'FORMATS', FORMATS, 1, 1024)
C
C    Check for double precision columns
C
         doubflag =  0
         do i = 1, ncol
             if ( formats(i) .eq. 'DOUB' ) then
                 doubflag = 1 
                 go to 10
             end if
         end do                 


C    Create the Record
 10     continue
        if ( doubflag .eq. 0 ) then
  	    CALL IBIS_RECORD_OPEN( IBISIN, INRECORD, ' ',
     +            KEYCOL, NKEY, 'REAL', STATUS)
        else
  	    CALL IBIS_RECORD_OPEN( IBISIN, INRECORDD, ' ',
     +            KEYCOL, NKEY, 'DOUB', STATUS)
        end if
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)

	IF (2*NKEY .NE. NRANGE) THEN
	    CALL XVMESSAGE ('The number of ranges must equal ' //
     * 'the number of key columns.', ' ')
	    CALL ABEND
	ENDIF

	DO I = 1, NKEY
	    LOWER(I) = RANGE(2*I-1) - PREC
	    UPPER(I) = RANGE(2*I) + PREC
	ENDDO

C		Go through each row and flag acceptable rows
	IF (DELETE) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
		KEEP(IREC) = .TRUE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF ( (BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I)) )  KEEP(IREC) = .FALSE.
                  else
		    IF ( (DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I)) ) KEEP(IREC) = .FALSE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ELSE IF (SELECT) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
		KEEP(IREC) = .TRUE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF (.NOT. ((BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I))) )  KEEP(IREC) = .FALSE.
                  else
		    IF ( .NOT. ((DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I))) ) KEEP(IREC) = .FALSE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ELSE IF (PICK) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
		KEEP(IREC) = .FALSE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF (((BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I))) )  KEEP(IREC) = .TRUE.
                  else
		    IF (((DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I))) ) KEEP(IREC) = .TRUE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ENDIF

        if ( doubflag .eq. 0 ) then
  	     CALL IBIS_RECORD_CLOSE( INRECORD, STATUS )
        else
  	     CALL IBIS_RECORD_CLOSE( INRECORDD, STATUS )
        end if
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)


c***************************************************************
c


C		Copy from input to output one column at a time
c		    keeping only the desired rows.
c		If necessary make duplicate copies of the rows
c		    and put out the index numbers.

	CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')

	CALL IBIS_FILE_OPEN(WUNIT,IBISOUT,'WRITE',
     +       NCOL,NUMCOPIES*OUTLEN, FORMATS, ORG, STATUS)
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(WUNIT,STATUS,1)


	DO ICOL = 1, NCOL
	  IF (ICOL .NE. INDEXCOL) THEN
	     CALL IBIS_COLUMN_GET(IBISOUT,'FORMAT',CFORM ,ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)

	     IF (CFORM(1:1).EQ.'A') THEN !dont format ASCII
	       CALL IBIS_COLUMN_SET(IBISIN,'U_FORMAT','NONE',ICOL,
     +                              STATUS)
    	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
	       CALL IBIS_COLUMN_SET(IBISOUT,'U_FORMAT','NONE',ICOL,
     +                              STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     ENDIF

	     CALL IBIS_COLUMN_GET(IBISOUT,'U_SIZE', USIZE ,ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     CALL IBIS_COLUMN_READ(IBISIN,COL,ICOL,1,CLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)

	     OUTSIZE = OUTLEN * USIZE

	     PTR = 1
	     INPTR = 1

	     DO I = 1, CLEN
               IF (KEEP(I)) THEN
		   DO J = 1, USIZE
			COL(PTR) = COL(INPTR)
			PTR=PTR+1
			INPTR=INPTR+1
		   ENDDO
	       ELSE
	    	   INPTR = INPTR+USIZE
	       ENDIF
  	     ENDDO

	     OFFSET=OUTSIZE
	     DO N = 1, NUMCOPIES-1
		DO I = 1, OUTSIZE
		   COL(OFFSET+I) = COL(I)
		 ENDDO
		 OFFSET=OFFSET+OUTSIZE
	     ENDDO
	     CALL IBIS_COLUMN_WRITE(IBISOUT,COL,
     +          ICOL,1,NUMCOPIES*OUTLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	  ELSE
	     CALL IBIS_COLUMN_SET(IBISOUT,'U_FORMAT','FULL',ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     OFFSET=0
	     DO N = 1, NUMCOPIES
	        DO I = 1, OUTLEN
		   FULLCOL(OFFSET+I) = N
	        ENDDO
		OFFSET=OFFSET+OUTLEN
	     ENDDO
	     CALL IBIS_COLUMN_WRITE(IBISOUT,FULLCOL,
     +               ICOL,1,NUMCOPIES*OUTLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	  ENDIF
	ENDDO

	CALL IBIS_FILE_CLOSE(IBISIN,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
	CALL IBIS_FILE_CLOSE(IBISOUT,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)

	RETURN
	END
