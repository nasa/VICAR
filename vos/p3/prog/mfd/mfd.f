c	Fiddle to get around the fact that mf and knuth are both written
c       for floating poing data only
c
c
c       bam 10/98
c


C
	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE

        INTEGER iUNIT, iSTAT, DEF
        INTEGER INCOLS(100),OUTCOLS(100),COLS(100)
        REAL*8 VALUE
	INTEGER	INCOL_COUNT, OUTCOL_COUNT
        INTEGER I,J
        INTEGER IXTRAN
        REAL*8  DMIN,DMAX,SUM
        INTEGER IMIN,IMAX
        INTEGER COUNT    

        logical xvptst

        INTEGER IBIS
        INTEGER RECORDD
        INTEGER NR,NC
	INTEGER	ROW
	REAL*8 ROWBUF(100)
        REAL*8 TEMP(100)

C*********************************************************************


        CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')

        CALL IFMESSAGE('*** mfd version 2017-08-09 ***')
        CALL XVEACTION('SA',' ')


C       GET THE INPUT/OUTPUT COLUMN/S

	CALL XVPARM ('INCOLS', INCOLS, INCOL_COUNT, DEF, 100)
	CALL XVPARM ('OUTCOLS',OUTCOLS,OUTCOL_COUNT, DEF, 100)
	CALL XVPARMD('VALUE',VALUE,COUNT,DEF,1)

C	OPEN IBIS INTERFACE FILE

	CALL IBIS_FILE_OPEN(IUNIT,IBIS,'UPDATE',0,0,' ',' ',ISTAT)
        IF (ISTAT.NE.1) CALL IBIS_SIGNAL_U(IUNIT,ISTAT,1)

C       get # of rows and columns

        CALL IBIS_FILE_GET(IBIS,'NC',NC,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',NR,1,1)


       IF(XVPTST('ADD'))  IXTRAN = 1
       IF(XVPTST('SUB'))  IXTRAN = 2
       IF(XVPTST('MULT')) IXTRAN = 3
       IF(XVPTST('DIV'))  IXTRAN = 4
       IF(XVPTST('INT'))  IXTRAN = 5
       IF(XVPTST('ABS'))  IXTRAN = 6
       IF(XVPTST('SQR'))  IXTRAN = 7
       IF(XVPTST('MIN'))  IXTRAN = 8
       IF(XVPTST('MAX'))  IXTRAN = 9
       IF(XVPTST('SET'))  IXTRAN = 10
       IF(XVPTST('INDEX'))IXTRAN = 11
       IF(XVPTST('EQ'))   IXTRAN = 12
       IF(XVPTST('NE'))   IXTRAN = 13
       IF(XVPTST('LE'))   IXTRAN = 14
       IF(XVPTST('LT'))   IXTRAN = 15
       IF(XVPTST('GE'))   IXTRAN = 16
       IF(XVPTST('GT'))   IXTRAN = 17



C*******************************************************************


C      DEFAULT THE COLS ARRAY
       
       DO I = 1,NC      ! GET ALL THE COLUMNS JUST CAUSE IT'S EASIER
           COLS(I) = I 
       END DO                


C      OPEN A RECORD = ROW

       CALL IBIS_RECORD_OPEN(IBIS,RECORDD,'FORMAT:DOUB',
     *      COLS,NC,'DOUB',ISTAT)


C      READ THROUGH ALL THE ROWS

       DO ROW = 1,NR
          CALL IBIS_RECORD_READ(RECORDD, ROWBUF, ROW, ISTAT)
          IF (ISTAT.NE.1) CALL IBIS_SIGNAL(IBIS,ISTAT,1)

          GO TO (201,202,203,204,205,206,207,208,209,210, ! DO FUNCTION
     -        211,212,213,214,215,216,217),IXTRAN

 201      CONTINUE    ! ADD
          DO I = 1, OUTCOL_COUNT
              SUM = 0.0
              DO J = 1, INCOL_COUNT
                  SUM = SUM + ROWBUF(INCOLS(J))
              END DO
          ROWBUF(OUTCOLS(I)) = SUM         
          END DO
          GO TO 300

 202      CONTINUE    ! SUBTRACT
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROWBUF(INCOLS(1))-ROWBUF(INCOLS(2))
          END DO
          GO TO 300

 203      CONTINUE    ! MULTIPLY
          DO I = 1, OUTCOL_COUNT
              SUM = 1.0
              DO J = 1, INCOL_COUNT
                  SUM = SUM * ROWBUF(INCOLS(J))
              END DO
          ROWBUF(OUTCOLS(I)) = SUM         
          END DO
          GO TO 300

 204      CONTINUE    ! DIVIDE
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROWBUF(INCOLS(1))/ROWBUF(INCOLS(2))
          END DO
          GO TO 300

 205      CONTINUE    ! INT
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DINT(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 206      CONTINUE    ! ABS
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DABS(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 207      CONTINUE    ! SQUARE ROOT
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DSQRT(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 208      CONTINUE    ! MINIMUM
          DO I = 1, NC
              TEMP(I) = ROWBUF(I)
          END DO
          CALL MINMAX(8,NC,TEMP,DMIN,DMAX,IMIN,IMAX)
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DMIN
          END DO
          GO TO 300

 209      CONTINUE    ! MAXIMUM
          DO I = 1, NC
              TEMP(I) = ROWBUF(I)
          END DO
          CALL MINMAX(8,NC,TEMP,DMIN,DMAX,IMIN,IMAX)
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DMAX
          END DO
          GO TO 300

 210      CONTINUE    ! SET VALUE
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = VALUE
          END DO
          GO TO 300

 211      CONTINUE    ! INDEX
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROW
              ROWBUF(OUTCOLS(I)) = DINT(ROWBUF(OUTCOLS(I)))
          END DO
          GO TO 300

 212      CONTINUE    ! EQ
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .EQ. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 213      CONTINUE    ! NE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .NE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 214      CONTINUE    ! LE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .LE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 215      CONTINUE    ! LT
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .LT. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300


 216      CONTINUE    ! GE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .GE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 217      CONTINUE    ! GT
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .GT. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO

 300      CALL IBIS_RECORD_WRITE(RECORDD,ROWBUF,ROW,ISTAT)
       END DO
  
       CALL IBIS_RECORD_CLOSE(RECORDD,ISTAT)
       CALL IBIS_FILE_CLOSE(IBIS,' ',ISTAT)
       IF (ISTAT.NE.1) CALL IBIS_SIGNAL(IBIS,ISTAT,1)

       RETURN
       END
