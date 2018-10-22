C     
C     AVHRRLOG - ADVANCED VERY HIGH RESOLUTION RADIOMETER
C     
C     PURPOSE:  LOG AVHRR IMAGES INTO VICAR 2 FORMAT
C
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
      REAL*8 SLOPE,INTER
      INTEGER*4 INBUF(3700),IOUT(5)
      INTEGER*2 OBUF(10242),OBUF2(2048)
      BYTE INBYTE(14800)
      CHARACTER*80 MSG
      CHARACTER*1 XL1,XL2,XL3,XL4
      EQUIVALENCE (INBYTE,INBUF)
c								open datasets
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      IF (NSIN .NE. 7400) THEN
	  CALL XVMESSAGE(' Input file is not a LAC/HRPT dataset',' ')
	  CALL ABEND
      ENDIF
      NL = (NLIN/2) - 1
      NS = 2048
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      DO I=1,5
	CALL XVUNIT(IOUT(I),'OUT',I,ISTAT,' ')
	CALL XVOPEN(IOUT(I),ISTAT,'U_NL',NL,'U_NS',NS,'U_NB',1,
     +		  'U_ORG','BSQ','U_FORMAT','HALF','O_FORMAT','HALF',
     +		  'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      END DO
C
C        DECODE AVHRR AND WRITE VICAR IMAGES AND CALIBRATION COEFFS
C
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
C						decode and store lats and longs
      WRITE(MSG,10) (INBYTE(J),J=76,89)
   10 FORMAT(14A1)
      READ (MSG,20) LAT1,LAT2,LONG1,LONG2
   20 FORMAT(I3,I3,I4,I4)
      IF (LAT1 .GE. 0) THEN
	XL1 = 'N'
      ELSE
	XL1 = 'S'
	LAT1 = -LAT1
      END IF
      IF (LAT2 .GE. 0) THEN
	XL2 = 'N'
      ELSE
	XL2 = 'S'
	LAT2 = -LAT2
      END IF
      IF (LONG1 .GE. 0) THEN
	XL3 = 'E'
      ELSE
        XL3 = 'W'
	LONG1 = -LONG1
      END IF
      IF (LONG2 .GE. 0) THEN
	XL4 = 'E'
      ELSE
        XL4 = 'W'
	LONG2 = -LONG2
      END IF
      WRITE (MSG,30) LAT1,XL1,LAT2,XL2,LONG1,XL3,LONG2,XL4
   30 FORMAT('Latitude Range: ',I2,A1,'-',I2,A1,'   Longitude Range: ',
     +       I3,A1,'-',I3,A1)
      CALL XVMESSAGE(MSG,' ')
      DO I=1,5
	CALL XLADD(IOUT(I),'HISTORY','BAND',I,ISTAT,'FORMAT','INT',' ')
      	CALL XLADD(IOUT(I),'HISTORY','LOCATION',MSG,ISTAT,
     +		   'FORMAT','STRING',' ')
      END DO
C								read past header
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
C
      DO L = 1,NL
C								read input data
        CALL XVREAD(INP,INBYTE,ISTAT,' ')
        CALL XVREAD(INP,INBYTE(7401),ISTAT,' ')
C							print calibration data
        IF (L.EQ.1) THEN
          CALL XVMESSAGE(
     +		   ' FIRST LINE - SLOPE AND INTERCEPT CALIBRATION:',' ')
	  DO I=1,5
	    M = 2*(I+1)
            SLOPE = DBLE(INBUF(M))/1073741824.D0
            INTER = DBLE(INBUF(M+1))/4194304.D0
	    WRITE (MSG,100) I,SLOPE,INTER
  100	    FORMAT(' Channel',I2,'  Slope =',F15.10,'  Intercept =',
     +		   F15.10)
	    CALL XVMESSAGE(MSG,' ')
	    X = SLOPE
	    CALL XLADD(IOUT(I),'HISTORY','CAL-SLOPE',X,ISTAT,
     +		       'FORMAT','REAL',' ')
	    X = INTER
	    CALL XLADD(IOUT(I),'HISTORY','CAL-INTER',X,ISTAT,
     +		       'FORMAT','REAL',' ')
          END DO
	END IF
C
	IF (L.EQ.NL) THEN
          CALL XVMESSAGE(
     +		   ' LAST LINE  - SLOPE AND INTERCEPT CALIBRATION:',' ')
	  DO I=1,5
	    M = 2*(I+1)
            SLOPE = DBLE(INBUF(M))/1073741824.D0
            INTER = DBLE(INBUF(M+1))/4194304.D0
	    WRITE (MSG,100) I,SLOPE,INTER
	    CALL XVMESSAGE(MSG,' ')
          END DO
        END IF
C								unpack data
        CALL UNPK10(INBUF(113),OBUF)
C							write out each band
	DO I=1,5
	  IOFF = I-5
	  DO LOC=1,2048
	    OBUF2(LOC) = OBUF(5*LOC+IOFF)
	  END DO
          CALL XVWRIT(IOUT(I),OBUF2,ISTAT,' ')
	END DO
      END DO
C								close files
      CALL XVCLOSE(INP,ISTAT,' ')
      DO I=1,5
	CALL XVCLOSE(IOUT(I),ISTAT,' ')
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE UNPK10(ISCAN,TBUF)
      INTEGER*2 TBUF(10242)
      INTEGER*4 ISCAN(3700)
C
      J = 1
      DO I = 1, 3414
        IWORD = ISCAN(I)
	TBUF(J+2) = MOD(IWORD,1024)
	IWORD = IWORD/1024
	TBUF(J+1) = MOD(IWORD,1024)
	TBUF(J) = IWORD/1024
	J = J+3
      END DO
      RETURN
      END
