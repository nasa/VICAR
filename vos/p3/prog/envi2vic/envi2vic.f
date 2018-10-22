	INCLUDE 'VICMAIN_FOR'
C
C	initial release		6/16/95 Ron Alley
C	add datatypes > 6       3/1/01  Ron Alley
C
C	This program creates a VICAR labelled file, using the size and format
C	info contained in an ENVI .hdr file.
C
	SUBROUTINE MAIN44
C
	PARAMETER (IBUFSIZE=1000000)
	BYTE BUFFER(IBUFSIZE)
	INTEGER NBPP(15)/1,2,4,4,8,8,99999,99999,16,4,4,2,4,8,8/
	CHARACTER*80 INLINE,KEY,VALUE
	CHARACTER*60 INFILE,HDRFILE
	CHARACTER*4 FORMAT
	CHARACTER*4 FMT(15)/'BYTE','HALF','FULL','REAL','DOUB','COMP',
     +			    '0007','0008','0009','FULL','FULL','HALF',
     +			    'FULL','0014','0015'/
	CHARACTER*3 ORG
C					get input and output file unit numbers
	CALL XVP('ENVI',INFILE,ICNT)
	I = 1
	DO WHILE (INFILE(I:I).NE.' ' .AND. I.LT.60)
	    I = I + 1
	END DO
	HDRFILE = INFILE(1:I-1) // '.hdr'
	CALL XVUNIT(INUNIT,'DUMMY',1,ISTAT,'U_NAME',INFILE,' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
C
C					get VICAR label info from ENVI .hdr file
	OPEN (71,FILE=HDRFILE)
	MASK = 0
	DO WHILE (MASK .NE. 63)
	    READ (71,100,END=900) INLINE
	    CALL UPRCASE(INLINE)
  100	    FORMAT(A)
	    CALL FINDKEY(INLINE,KEY,VALUE)
	    IF (KEY(1:7) .EQ. 'SAMPLES') THEN
		READ (VALUE,*) NS
		MASK = MASK + 1
	    ELSE IF (KEY(1:5) .EQ. 'LINES') THEN
		READ (VALUE,*) NL
		MASK = MASK + 2
	    ELSE IF (KEY(1:5) .EQ. 'BANDS') THEN
		READ (VALUE,*) NB
		MASK = MASK + 4
	    ELSE IF (KEY(1:12).EQ.'HEADEROFFSET') THEN
		READ (VALUE,*) LBLSIZE
		MASK = MASK + 8
	    ELSE IF (KEY(1:8).EQ.'DATATYPE') THEN
		READ (VALUE,*) ITYPE
		FORMAT = FMT(ITYPE)
		NBYTES = NBPP(ITYPE)
		MASK = MASK + 16
	    ELSE IF (KEY(1:10) .EQ. 'INTERLEAVE') THEN
		ORG = VALUE(1:3)
		CALL UPRCASE(ORG)
		MASK = MASK + 32
	    END IF
	END DO
C						check for unsupported data types
	IF ((ITYPE.GE.7 .AND. ITYPE.LE.9) .OR. ITYPE.GT.13) THEN
	    CALL XVMESSAGE('VICAR does not support this ENVI DATATYPE',
     +						' ')
	    CALL ABEND
	ELSE IF (ITYPE .EQ. 12) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('WARNING:',' ')
	    CALL XVMESSAGE(
     +		'VICAR does not support unsigned 16-bit integer.',' ')
	    CALL XVMESSAGE('Pixel values greater than 32767 will be',' ')
	    CALL XVMESSAGE(
     +		'erroneously represented as negative numbers.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE IF (ITYPE .EQ. 13) THEN
	    CALL XVMESSAGE('WARNING:',' ')
	    CALL XVMESSAGE(
     +		'VICAR does not support unsigned 32-bit integer.',' ')
	    CALL XVMESSAGE(
     +		'Pixel values greater than 2,147,483,647 will',' ')
	    CALL XVMESSAGE(
     +		'be erroneously represented as negative numbers.',' ')
	    CALL XVMESSAGE(' ',' ')
	END IF
C
	NTOT = NBYTES * NL * NS * NB
	IF (LBLSIZE .EQ. 0) THEN
	    IRECLEN = NBYTES * NS
	ELSE
	    IRECLEN = LBLSIZE
	END IF
	IF (ORG .EQ. 'BIP') THEN
	    IOUTRECL = NBYTES * NB
	ELSE
	    IOUTRECL = NBYTES * NS
	END IF
C								open datasets
	CALL XVOPEN(INUNIT,ISTAT,'U_NS',IRECLEN,'COND','NOLABELS',
     +		    'OPEN_ACT','SA',' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_ORG',ORG,'U_NL',NL,
     +		    'U_NS',NS,'U_NB',NB,'U_FORMAT',FORMAT,
     +		    'O_FORMAT',FORMAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C								copy the data
C
	IF (LBLSIZE .NE. 0) CALL XVREAD(INUNIT,BUFFER,ISTAT,' ')
	LOC = 1
	ILEN = IBUFSIZE
	NTOGO = NTOT
	DO WHILE (NTOGO .GT. 0)
	    CALL READM(INUNIT,BUFFER(LOC),ILEN,NTOGO,IRECLEN)
	    ILEN = ILEN + LOC - 1
	    CALL WRITM(IOUTUNIT,BUFFER,ILEN,IOUTRECL)
	    LOC = ILEN + 1
	    ILEN = IBUFSIZE - ILEN
	END DO
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVCLOSE(IOUTUNIT,ISTAT,' ')
	RETURN
C
  900	CONTINUE
	CALL XVMESSAGE('Unable to read ENVI .hdr file',' ')
	CALL XVMESSAGE('No output written',' ')
	RETURN
	END
C******************************************************************************
	SUBROUTINE FINDKEY(INLINE,KEY,VALUE)
C
	CHARACTER*80 INLINE,KEY,VALUE
C
	DO I=1,80
	    KEY(I:I) = ' '
	    VALUE(I:I) = ' '
	END DO
C
	I = 1
	J = 1
	DO WHILE (I.LE.80 .AND. INLINE(I:I).NE.'=')
	    IF (INLINE(I:I) .NE. ' ') THEN
		KEY(J:J) = INLINE(I:I)
		J = J + 1
	    END IF
	    I = I + 1
	END DO
C
	I = I + 1
	J = 1
	DO WHILE (I .LE. 80)
	    IF (INLINE(I:I) .NE. ' ') THEN
		VALUE(J:J) = INLINE(I:I)
		J = J + 1
	    END IF
	    I = I + 1
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE READM(INUNIT,BUFFER,ILEN,NTOGO,IRECLEN)
C
	BYTE BUFFER(ILEN)
C
	LOC = 1
	IF (ILEN .GE. NTOGO) THEN
	    DO WHILE (NTOGO .GT. 0)
		IF (NTOGO .GE. IRECLEN) THEN
		    CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,' ')
		    LOC = LOC + IRECLEN
		    NTOGO = NTOGO - IRECLEN
		ELSE
		    CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,
     +				'NSAMPS',NTOGO,' ')
		    LOC = LOC + NTOGO
		    NTOGO = 0
		END IF
	    END DO
	ELSE
	    DO WHILE (LOC + IRECLEN -1 .LE. ILEN)
		CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,' ')
		LOC = LOC + IRECLEN
		NTOGO = NTOGO - IRECLEN
	    END DO
	    ILEN = LOC - 1
	END IF
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE WRITM(IOUTUNIT,BUFFER,ILEN,IRECLEN)
C
	BYTE BUFFER(ILEN)
C
	LOC = 1
	DO WHILE (LOC + IRECLEN - 1 .LE. ILEN)
	    CALL XVWRIT(IOUTUNIT,BUFFER(LOC),ISTAT,' ')
	    LOC = LOC + IRECLEN
	END DO
C
	ILEN = ILEN - LOC + 1
	IF (ILEN .GT. 0) CALL MVE(1,ILEN,BUFFER(LOC),BUFFER(1),1,1)
C
	RETURN
	END
