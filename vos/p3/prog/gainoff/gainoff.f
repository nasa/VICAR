	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL GAIN(500),OFFSET(500),BUF(10000)
	REAL ZMIN(4)/  0.0, -32768.0, 0.0, -1.5E38/
	REAL ZMAX(4)/255.0,  32767.0, 0.0,  1.5E38/
	CHARACTER*40 FILNAM(2)
	CHARACTER*8 FORMAT
C						open input, get size and org
	CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBI)
	CALL XVGET(INUNIT1,ISTAT,'PIX_SIZE',NBPP,'FORMAT',FORMAT,' ')
C			       					open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		  'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','REAL','U_ORG','BIL','O_FORMAT',FORMAT,' ')
C							get remaining parameters
	CALL XVPARM('INGAIN',GAININ,ICNT,IDEF,0)
	CALL XVPARM('OUTGAIN',GAINOUT,ICNT,IDEF,0)
	CALL XVPARM('INP',FILNAM,ICNT,IDEF,0)
C							read in gains & offsets
	CALL READDS(FILNAM(2),GAIN,OFFSET,NBX)
	IF (NB.NE.NBX) THEN
	    CALL XVMESSAGE(
     +      ' Number of bands in image does not match gain/offset file',
     +      ' ')
	    CALL ABEND
	ENDIF
C
	DO I=1,NB
	    GAIN(I) = GAININ*GAINOUT*GAIN(I)
	    OFFSET(I) = GAINOUT*OFFSET(I)
	    IF (FORMAT .NE. 'REAL') OFFSET(I)=OFFSET(I)+0.5
	END DO
C
	IEB = ISB+NB-1
	IEL = ISL+NL-1
	XLO = ZMIN(NBPP)
	XHI = ZMAX(NBPP)
	DO LINE=ISL,IEL
	    DO IBAND=ISB,IEB
		CALL XVREAD(INUNIT1,BUF,ISTAT,'LINE',LINE,'BAND',IBAND,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO N=1,NS
		    BUF(N) = GAIN(IBAND)*BUF(N) + OFFSET(IBAND)
		    BUF(N) = MIN(MAX(BUF(N),XLO),XHI)
		END DO
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,' ')
	    END DO
	END DO
	RETURN
	END
C**************************************************************************
      SUBROUTINE READDS(FILENAME,GAIN,OFFSET,NRECS)
C
C      This subroutine reads all records from the file FILENAME, and puts
C      the second numeric value that it finds in the GAIN array, and the
C      third numeric value in the OFFSET array. The delimiters may be space, 
C      tab, or comma. If something other than a numeric or delimiter is
C      encountered prior to finding three numeric fields, the entire line is 
C      discarded.
C
      CHARACTER*40 FILENAME
      REAL GAIN(*),OFFSET(*)
C
      OPEN (51,FILE=FILENAME,STATUS='OLD')
      NRECS = 1
  100 CONTINUE
          READ (51,*,END=900,ERR=100) X,GAIN(NRECS),OFFSET(NRECS)
	  NRECS = NRECS+1
          GO TO 100
  900 CONTINUE
      NRECS = NRECS-1
      RETURN
      END
