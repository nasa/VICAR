      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- IBIS PROGRAM "ZIPCOL".
C
C    6-March-1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C
C     PURPOSE: TO ENTER THE DATA INTO AN EXISTING IBIS
C      INTERFACE FILE FROM ANOTHER INTERFACE FILE. DATA IS ENTERED BY MATCHING
C      KEYS BETWEEN THE INTERFACE FILES. PRESORTING OF      BOTH FILES IS REQUIRED.
C     USER PARAMETERS:
C      INCOL,X1,...XK    INCOL IS USED TO SPECIFY COLUMNS IN INT1 WHICH ARE
C            TO BE USED AS MATCHING KEYS.
C      OUTCOL,Y1,...YN   OUTCOL IS USED TO SPECIFY COLUMNS OF INT1 WHICH WILL
C            RECEIVE DATA WHEN INCOL MATCHES ARE FOUND.
C      NULL,C1,...CN     NULL IS USED TO SPECIFY WHAT OPERATION WILL TAKE
C            PLACE IF NO MATCH IS FOUND FOR ANY OF THE KEYS IN INT1. IF
C            NULL IS NOT SPECIFIED THEN DATA IN COLUMNS Y1,...YN REMAIN
C            UNCHANGED. OTHERWISE VALUES C1,...CN ARE USED AS A REPLACEMENT.
C      FILE,D1,...DK,    COLUMNS D1,...DK ARE THE MATCHING E1,...EN KEYS IN
C            INT2 CORRESPONDING TO INCOL MATCHING KEYS. E1,...EN ARE THE
C            COLUMNS OF INT2 WHERE DATA TO BE TRANSFERED TO INT1 ARE FOUND.
C
      IMPLICIT NONE  !IMPLICIT INTEGER is just asking for trouble!
      INTEGER RUNIT1,RUNIT2,IBIS,IBIS_R,NIN,NOUT
      INTEGER COUNT,STATUS,CLENA,CLENB,NCOLA,NCOLB
      INTEGER INCODF,OUTCDF,NFIL,FILEDF,NULL,NULLDF,NULSDF
      INTEGER I,CSIZE,CSZ,IBIS_FILE_GET
      INTEGER RECORD,RECORD_R,JROW,OJROW,IROW
      INTEGER FILE,INCOL,OUTCOL,MAXOUT,MAXIN,MAXCOL
      INTEGER COL(1000000)
      LOGICAL USE_A256, ALPHA,XVPTST,IBIS2R,IBIS2U
      PARAMETER (MAXOUT=30,MAXIN=20,MAXCOL=MAXOUT+MAXIN)
      CHARACTER*4     CBUF1_A4(MAXIN),CBUF2_A4(MAXIN)
      CHARACTER*4     STRNUL_A4(MAXOUT)
      character*7     ibisftype,infmt,outfmt
      character*256   CBUF1_A256(MAXIN)
      character*256   STRNUL_A256(MAXOUT)
      REAL*4          RBUF1(MAXIN),RBUF2(MAXIN), VALNUL(MAXOUT)
      DIMENSION INCOL(MAXIN),OUTCOL(MAXOUT),FILE(MAXCOL)
C
      equivalence (CBUF1_A256(1),CBUF1_A4(1))
C
C       Initialize:
C
      ibisftype = '      '
      infmt     = '      '
      outfmt    = '      '
C
C---- OPEN FILES. READ PARAMETERS.
C
      CALL IFMESSAGE('ZIPCOL version 6-MAR-95')
      CALL XVUNIT(RUNIT1,'INP',1,STATUS,' ')
       CALL IBIS_FILE_OPEN(RUNIT1,IBIS,'UPDATE',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      CALL XVUNIT(RUNIT2,'INP',2,STATUS,' ')
      CALL IBIS_FILE_OPEN(RUNIT2,IBIS_R,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS,'VERSION',ibisftype,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      IBIS2U =  (ibisftype(1:6).eq.'IBIS-2')
      if (IBIS2U) then
           call xvmessage('UPDATE file is IBIS-2',' ')
      else
           call xvmessage('UPDATE file is IBIS-1',' ')
      endif
C
      COUNT=IBIS_FILE_GET(IBIS_R,'VERSION',ibisftype,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      IBIS2R = (ibisftype(1:6).eq.'IBIS-2')
      if (IBIS2R) then
         call xvmessage('Reference  file is IBIS-2',' ')
      else
         call xvmessage('Reference  file is IBIS-1',' ')
      endif
C
      COUNT=IBIS_FILE_GET(IBIS,'NR',CLENA,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS,'NC',NCOLA,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS_R,'NR',CLENB,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS_R,'NC',NCOLB,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      ALPHA = XVPTST('ALPHA')
      CALL XVPARM ('INCOL',INCOL,NIN,INCODF,MAXIN)
      CALL XVPARM ('OUTCOL',OUTCOL,NOUT,OUTCDF,MAXOUT)
      CALL XVPARM ('FILE',FILE,NFIL,FILEDF,MAXCOL)
      CALL XVPARM ('NULL',VALNUL,NULL,NULLDF,MAXOUT)
      IF (ALPHA.AND.NULLDF.EQ.0)
     +    call mabend('*** Cant specify NULL for ALPHA ***')
      CALL XVPARM ('NULSTR',STRNUL_A256,NULL,NULSDF,MAXOUT)
      IF (.NOT.ALPHA.AND.NULSDF.EQ.0)
     +    call mabend('*** Cant specify NULSTR for Non-ALPHA ***')
       if (ALPHA.and.IBIS2R.and..not.IBIS2U)
     +    call mabend('*** File types not compatable! ***')
C
C ---- set the ibis file formats
C
      USE_A256=.false.
      if (ALPHA)  then

           infmt='A4'  ! read and sort only 1st 4 characters even if ibis-2
           outfmt='A4'  ! read and sort only 1st 4 characters even if ibis-2

           !
           ! -- Set up Reference File
	   !
           if (.not.IBIS2R) then  ! coerce ibis-1 columns to ascii
              do i=1,NFIL
               call ibis_column_set(IBIS_R,'FORMAT','A4',
     +                 FILE(i),STATUS)
              enddo
           endif

           !
           ! -- Set up Update File
	   !
           USE_A256=.FALSE.
           if (IBIS2U) then    ! UPDATE file is ibis-2
                  if (NULL.gt.1) then       ! NULSTR is user specified
		      do i=1,NOUT
                         call ibis_column_get(IBIS,'U_SIZE',
     +                         CSIZE,i,STATUS)
                         IF (STATUS.NE.1) 
     +				CALL IBIS_SIGNAL(IBIS,STATUS,1)
			 csz = csize -1
			 if (csz.GT.4) USE_A256 = .TRUE.
                      enddo
                  endif
                  if (USE_A256) outfmt = 'A256' 
          else                ! UPDATE file is ibis-1 -- coerce to ascii
              do i=1,NOUT
                call ibis_column_set(IBIS,'FORMAT','A4',
     +                          OUTCOL(i),STATUS)
                IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              enddo
              do i=1,NIN
                 call ibis_column_set(IBIS,'FORMAT','A4',
     +                          INCOL(i),STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              enddo
         endif
	 if (.not.USE_A256) THEN
	 	DO I=1,NULL
		   STRNUL_A4(I) = STRNUL_A256(I)(1:4)
		ENDDO
	 ENDIF
      else    ! not ALPHA
         infmt ='REAL'
         outfmt='REAL'
      endif
C
C-- Sanity check
C
      IF (((NULLDF.EQ.0).AND.(NULSDF.EQ.0)).OR.    
     +       (NIN+NOUT.NE.NFIL)) GO TO 666   ! a parameter fault
     
C
C---- READ AND COMPARE KEYS.
C
C
C
C--- open & read the update file (INCOL) into RBUF1 or CBUF1_A4
C
      CALL IBIS_RECORD_OPEN (IBIS,RECORD,' ',
     +                               INCOL,NIN,infmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_RECORD_OPEN (IBIS_R,RECORD_R,' ',
     +                        FILE,NIN,infmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      
      
      JROW = 1                    !
      OJROW = 0
      DO IROW=1,CLENA             !
            COL(IROW) = 0             !
            if (ALPHA) then
                  call ibis_record_read(RECORD,CBUF1_A4,IROW,STATUS)
            else
                  CALL IBIS_RECORD_READ(RECORD,RBUF1,IROW,STATUS)
            endif
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

            !   --- open & read Ref file (matching col) into RBUF2 or CBUF2_A4

 5          IF (JROW.NE.OJROW) THEN
                  if (ALPHA) then
                     call ibis_record_read(RECORD_R,CBUF2_A4,
     +                                                JROW,STATUS)
                  else
                     CALL IBIS_RECORD_READ(RECORD_R,RBUF2,JROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            ENDIF

            OJROW = JROW
            DO I=1,NIN
                  IF ((.NOT.ALPHA.AND.RBUF1(I).GT.RBUF2(I))
     *              .OR. (ALPHA.AND.CBUF1_A4(I).GT.CBUF2_A4(I))) THEN
                  JROW = JROW+1
                  IF (JROW.GT.CLENB) GO TO 51
                  GO TO 5
                  ELSEIF (.NOT.ALPHA.AND.RBUF1(I).LT.RBUF2(I)
     *              .OR. ALPHA.AND.CBUF1_A4(I).LT.CBUF2_A4(I) ) THEN
                  GO TO 50
                  ENDIF
            ENDDO
            COL(IROW) = JROW ! if COL(IROW) = matching row no.; 0=no match
 50   ENDDO
 51   DO I=1,NOUT             ! move out col numbers 
            FILE(I) = FILE(I+NIN)
      ENDDO
      CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
      CALL IBIS_RECORD_CLOSE(RECORD_R,STATUS)

      !---- MERGE.

      do i=1,NOUT
            call ibis_column_set(IBIS,'U_FORMAT',outfmt,
     +                                     OUTCOL(i),STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            call ibis_column_set(IBIS_R,'U_FORMAT',outfmt,
     +                                     file(i),STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      enddo
      CALL IBIS_RECORD_OPEN (IBIS_R,RECORD_R,' ',
     +                        FILE,NOUT,outfmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_RECORD_OPEN (IBIS,RECORD,' ',
     +                         OUTCOL,NOUT,outfmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      JROW = 1
      DO IROW=1,CLENA
	    JROW = COL(IROW)
            IF (JROW.NE.0) THEN   ! if match read out data to buffers
                  if (ALPHA) then
		     IF (USE_A256) THEN
                        call ibis_record_read(RECORD_R,CBUF1_A256,
     +                                            JROW,STATUS)
		     ELSE
                        call ibis_record_read(RECORD_R,CBUF1_A4,
     +                                            JROW,STATUS)
		     ENDIF
                  else
                     CALL IBIS_RECORD_READ(RECORD_R,RBUF1,JROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  if (ALPHA) then
		     IF (USE_A256) THEN
                        call ibis_record_write(RECORD,CBUF1_A256,
     +                                           IROW,STATUS)
                     ELSE
                        call ibis_record_write(RECORD,CBUF1_A4,
     +                                           IROW,STATUS)
                     ENDIF
                  else
                        CALL IBIS_RECORD_WRITE(RECORD,RBUF1,
     +                                           IROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            ELSE    ! no matching data COL(IROW)=0
                  IF (ALPHA.AND.NULSDF.EQ.0) THEN
                      !--   write the string specified by user
                      if (USE_A256) then
                          call ibis_record_write(RECORD, 
     +                           STRNUL_A256,IROW,STATUS)  
                      else    ! for ibis-1 files write STRNUL_A256              
	                   call ibis_record_write(RECORD,
     +                            STRNUL_A4,IROW,STATUS)
                      endif
                      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  ELSEIF (.NOT.ALPHA.AND.NULLDF.EQ.0) THEN 
	              call ibis_record_write(RECORD,
     +                            VALNUL,IROW,STATUS)
                      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  ENDIF
            ENDIF
      ENDDO
      CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
      CALL IBIS_RECORD_CLOSE(RECORD_R,STATUS)
      CALL IBIS_FILE_CLOSE(IBIS,'UDELETE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_CLOSE(IBIS_R,'UDELETE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE (RUNIT1,STATUS,' ')
      CALL XVCLOSE (RUNIT2,STATUS,' ')
      RETURN
      
  666   CONTINUE  !Failure
      CALL XVMESSAGE ('PARAMETER ERROR',' ')
      RETURN
      END
