      SUBROUTINE IWRITE_TIEPOINTS(UNIT,NAH,NAV,NPTS,TIEPOINTS,COLCOUNT)
C#######################################################################
C  NAME OF ROUTINE
C      IWRITE_TIEPOINTS (Ibis WRITE TIEPOINTS)
C  PURPOSE
C      IWRITE_TIEPOINTS writes the tiepoint data to an IBIS2 file.
C      This includes opening and closing the file.
C
C    In the most common case,
C      the TIEPOINTS array defines a geometric transformation which is applied
C      to the input image to produce the output image.  The transformation is 
C      defined via a grid of tiepoints.  The convention (see HELP for programs
C      GEOMA and TIEPARM) is that there are NAV+1 rows of tiepoints in the grid,
C      with NAH+1 tiepoints in each row.  ("row" here means a horizontally 
C      arranged set of points in an image.)  NPTS is 0 in this case.
C      
C      The IBIS tiepoint file will have one record (IBIS row) per tiepoint.
C      For the case of COLCOUNT = 4, IBIS columns 1 and 2 will contain the line
C      sample coordinates of the tiepoint in the output image.  IBIS columns
C      3 and 4 will contain the line and sample coordinates of the tiepoint in
C      the input image.
C      
C    In the other case the tiepoints are not constrained to form a grid.
C      In this case NAH and NAV are 0 and the number of points to be written
C      is specified in NPTS.
C  WRITTEN BY   
C      STEVE POHORSKY   JET PROPULSION LABORATORY        JULY 1995
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE
C      Standard subroutine call and return if no errors.  ABEND called if
C      an error occurs.  The calling routine should call XVUNIT before
C      this routine but should not open the file.
C
C  INPUT PARAMETERS     
C      (see the accompanying HELP file.)
C  REVISION HISTORY
C    JUL 95  SP  INITIAL RELEASE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INTEGER UNIT, NAH, NAV, NPTS, COLCOUNT
      REAL    TIEPOINTS(*)

      INTEGER IBIS, IRECORD, NUMPTS, STATUS, IROW, TINDX
      INTEGER INPUTCOLS(3), OUTPUTCOLS(3), TIECOLS(5)
      CHARACTER*20 SUBTYPE_NAME

      DATA INPUTCOLS/  3,4,0 /  ! see above under PURPOSE
      DATA OUTPUTCOLS/ 1,2,0 /
      DATA TIECOLS/    1,2,3,4,0 /

C=====START OF EXECUTABLE CODE=================================================

      IF (COLCOUNT .NE. 4)
     .   CALL MABEND('ERROR: Option not available in IWRITE_TIEPOINTS')

      IF (NAH*NAV .NE. 0)  THEN
          NUMPTS = (NAH+1) * (NAV+1)    ! GRIDDED SET OF TIEPOINTS.
      ELSE IF (NPTS .EQ. 0) THEN
          CALL MABEND('ERROR: NPTS or NAH*NAV = 0 in IWRITE_TIEPOINTS')
      ELSE
          NUMPTS = NPTS
      END IF

C.. Open IBIS tiepoint file for WRITE organized by (IBIS) ROW with
C.. COLCOUNT columns and NUMPTS (IBIS) rows.

      CALL IBIS_FILE_OPEN(UNIT,IBIS,'WRITE',COLCOUNT, NUMPTS,
     +                          ' ','ROW',STATUS)
       IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      SUBTYPE_NAME='TIEPOINT'
      CALL ibis_file_set(IBIS,'type',SUBTYPE_NAME,STATUS)
       IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL ICL_NEW_POS_IMAGE(IBIS, OUTPUTCOLS(1),OUTPUTCOLS(2),0,
     .                       ' ', STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL ICL_NEW_POS_IMAGE(IBIS, INPUTCOLS(1),INPUTCOLS(2),0,
     .                       ' ', STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL ICL_NEW_POINT(IBIS,INPUTCOLS,2,0,0,'PIXEL','INPUT',STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL ICL_NEW_POINT(IBIS,OUTPUTCOLS,2,0,0,'PIXEL','OUTPUT',STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      IF (NAH*NAV .NE. 0)  THEN
        CALL XLADD(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',NAH,
     &           STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
         IF (STATUS .NE. 1)   
     &     CALL MABEND('ERROR from XLADD in IWRITE_TIEPOINTS')
       
        CALL XLADD(UNIT,'PROPERTY','NUMBER_OF_AREAS_VERTICAL',  NAV,
     &           STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
         IF (STATUS .NE. 1)   
     &     CALL MABEND('ERROR from XLADD in IWRITE_TIEPOINTS')
      END IF

      CALL IBIS_RECORD_OPEN(IBIS,IRECORD,' ',
     +                        TIECOLS,COLCOUNT,'NONE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      TINDX = 1

      DO IROW = 1, NUMPTS
	  CALL IBIS_RECORD_WRITE(IRECORD,TIEPOINTS(TINDX),IROW,STATUS)
          IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TINDX = TINDX + COLCOUNT
      ENDDO

      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      RETURN
      END

      SUBROUTINE IREAD_TIEPOINTS(UNIT,NAH,NAV,MAXPTS,TIEPOINTS,COLCOUNT)
C#######################################################################
C  NAME OF ROUTINE
C      IREAD_TIEPOINTS (Ibis READ TIEPOINTS)
C  PURPOSE
C      IREAD_TIEPOINTS reads the tiepoint data from an IBIS2 file.
C      This includes opening and closing the file.
C
C    In the most common case,
C      the IBIS2 file defines a geometric transformation which is applied
C      to the input image to produce the output image.  The transformation is 
C      defined via a grid of tiepoints.  The convention (see HELP for programs
C      GEOMA and TIEPARM) is that there are NAV+1 rows of tiepoints in the grid,
C      with NAH+1 tiepoints in each row.  ("row" here means a horizontally 
C      arranged set of points in an image.)  This case is distinguished by
C      the presence of NUMBER_OF_AREAS_HORIZONTAL and NUMBER_OF_AREAS_VERTICAL
C      (the NAH and NAV values) in the VICAR property label.
C      
C      The IBIS tiepoint file will have one record (IBIS row) per tiepoint.
C      For the case of COLCOUNT = 4, IBIS columns 1 and 2 will contain the line
C      sample coordinates of the tiepoint in the output image.  IBIS columns
C      3 and 4 will contain the line and sample coordinates of the tiepoint in
C      the input image.
C      
C    In the other case the tiepoints are not constrained to form a grid.
C      In this case NAH and NAV are set to 0 and the number of points to be 
C      read is the minimum of MAXPTS and the number of IBIS rows in the file.
C  WRITTEN BY   
C      STEVE POHORSKY   JET PROPULSION LABORATORY        JULY 1995
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE
C      Standard subroutine call and return if no errors.  ABEND called if
C      an error occurs.  The calling routine should call XVUNIT before
C      this routine but should not open the file.
C
C  INPUT PARAMETERS     
C      (see the accompanying HELP file.)
C  REVISION HISTORY
C    JUL 95  SP  INITIAL RELEASE
C    AUG 95  SP  Changed call to IBIS_RECORD_OPEN to use UFORMAT 'REAL'
C                instead of 'NONE' to handle files form other machine types.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INTEGER UNIT, NAH, NAV, MAXPTS, COLCOUNT
      REAL    TIEPOINTS(*)

      INTEGER IBIS, IRECORD, NUMPTS, STATUS, IROW, TINDX,CLEN,NCOL
      INTEGER TIECOLS(5)

      DATA TIECOLS/    1,2,3,4,0 /  ! see above under PURPOSE

C=====START OF EXECUTABLE CODE=================================================

      IF (COLCOUNT .NE. 4)
     .   CALL MABEND('ERROR: Option not available in IREAD_TIEPOINTS')

C.. Open IBIS tiepoint file for READ.

      CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
      CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)

      NAH = 0       ! 0 IF NOT IN LABEL.
      NAV = 0
      CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',
     &        NAH,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
      CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_VERTICAL',
     &        NAV,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')

      IF (NAH*NAV .NE. 0 )   THEN
          NUMPTS = (NAH+1) * (NAV+1)    ! GRIDDED SET OF TIEPOINTS.
          NUMPTS = MIN( NUMPTS,MAXPTS)
      ELSE IF (MAXPTS .LE. 0) THEN
          CALL MABEND('ERROR:MAXPTS and NAH*NAV = 0 in IREAD_TIEPOINTS')
      ELSE
          NUMPTS = MIN(CLEN,MAXPTS)    ! READ WHOLE FILE UNLESS CLEN>MAXPTS.
          NAV    = CLEN                ! RETURN NUMBER OF POINTS IN FILE
          NAH    = 0                   ! INDICATES NON-GRIDDED SET.
      END IF

      IF (NCOL .LT. COLCOUNT)
     .  CALL MABEND('ERROR: COLCOUNT > file''s NCOL in IREAD_TIEPOINTS')
      IF (CLEN .LT. NUMPTS)
     .  CALL MABEND('ERROR: NUMPTS > file''s CLEN in IREAD_TIEPOINTS')

      CALL IBIS_RECORD_OPEN(IBIS,IRECORD,' ',
     +                        TIECOLS,COLCOUNT,'REAL',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      TINDX = 1

      DO IROW = 1, NUMPTS
	  CALL IBIS_RECORD_READ(IRECORD,TIEPOINTS(TINDX),IROW,STATUS)
          IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TINDX = TINDX + COLCOUNT
      ENDDO

      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      RETURN
      END
