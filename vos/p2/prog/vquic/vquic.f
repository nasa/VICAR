      INCLUDE 'VICMAIN_FOR'
C
C---- VICAR PROGRAM VQUIC
C
C     2 JAN 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING) 
C     13 OCT 1998...BAM... ADDED OPTION FOR DOUBLE PRECISION 
C                          FREE FORMAT INPUT; added debug flag for
C                          informational output
C
C     READS NUMBERS IN FROM AN ASCII TEXT FILE INTO AN IBIS INTERFACE FILE
C
	SUBROUTINE MAIN44
	IMPLICIT NONE
	INTEGER	OUTUNIT, STATUS, COUNT, DEF, ICNT, RCNT, ACNT
	INTEGER	LCOL, LCOLDEF, NCOL, NUMCOL, COLCNT, ACNTSTORE(150)
	INTEGER	I, J, INDEX, ROW
	INTEGER COLS(100), IBIS, MAXATEMP, INTLINE(100)
        INTEGER RECORD, RECORDF, RECORDR, RECORDA, RECORDD 
	REAL*4  ROWBUF(100), REALLINE(100)
        real*8  drowbuf(100)

        logical xvptst
	LOGICAL	FREEFORMAT, INTFLAG, REALFLAG, CHARFLAG, doubflag, debug

	CHARACTER*72  INNAME, FORMAT, IFORMAT, RFORMAT, AFORMAT
	CHARACTER*255 DATALINE, C255LINE(100), MSGBUF
        CHARACTER*128 C128LINE(5)
        CHARACTER*64  C64LINE(5)
        CHARACTER*32  C32LINE(10)
        CHARACTER*16  C16LINE(20)
        CHARACTER*8   C8LINE(40)
        CHARACTER*4   C4LINE(80)
        CHARACTER*5   FMT_BUF(150), NEWMAX

C This is for saving memory. Only, one variable used per run
        EQUIVALENCE (C255LINE(1),C128LINE(1),C64LINE(1),C32LINE(1),
     *               C16LINE(1),C8LINE(1),C4LINE(1) )


c*********************************************************************


        CALL IFMESSAGE('*** VQUIC version 2017-08-15 ***')
        CALL XVEACTION('SA',' ')

        MAXATEMP=1

C		Get the parameters
	CALL XVP ('NCOL',NCOL,COUNT)
	CALL XVPARM ('LCOL',LCOL, COUNT, LCOLDEF,1)
	CALL XVP ('INPUT', INNAME, COUNT)
	CALL XVP ('FORMAT', FORMAT, COUNT)
c
c       check for a double precision input and a debug flag
c
        doubflag = xvptst('DOUB')
        debug = xvptst('DEBUG')

        if ( DEBUG) call qprint('start',' ')

	CALL XVPARM ('COLS', COLS, NUMCOL,DEF,100)
! make sure cols and nocl are set up properly
	IF (DEF .EQ. 1) THEN   
	    NUMCOL = NCOL
	    DO I = 1, NUMCOL
		COLS(I) = I
	    ENDDO
	ELSE
	    DO I = 1, NUMCOL
		NCOL = MAX(NCOL, COLS(I))
	    ENDDO
	ENDIF

c
c set free format flag
c
	FREEFORMAT = (FORMAT(1:1) .EQ. '*') 
        if ( DEBUG ) call qprint('end parameter processing',' ')



c*********************************************************************



C  If LCOL was not input then find the length of the file

	IF (LCOLDEF .EQ. 1) THEN
            OPEN (UNIT=1, STATUS='OLD', FILE=INNAME)
	    INDEX = 0
50	    CONTINUE
	    READ (UNIT=1 ,FMT=51,  END=90) DATALINE
51          FORMAT (A)
	    INDEX = INDEX + 1
	    GOTO 50
90	    CONTINUE
            CLOSE (UNIT=1)
	    LCOL = INDEX
	ENDIF

C		Open the output interface (tabular) file
        CALL XVUNIT(OUTUNIT, 'OUT', 1, STATUS, ' ')
        if ( DEBUG) call qprint('after xvopen',' ')


c*********************************************************************
c
c
c       first do the free format data reduction
c

        IF (FREEFORMAT) THEN

           if ( .not. doubflag ) then   ! whatever
    	       CALL IBIS_FILE_OPEN(OUTUNIT,IBIS,'WRITE',NCOL,LCOL,
     *          ' ',' ',STATUS)
   	   else                    ! double precision
               do i = 1, ncol
                  fmt_buf(i)='DOUB'
               end do
    	       CALL IBIS_FILE_OPEN(OUTUNIT,IBIS,'WRITE',NCOL,LCOL,
     *          fmt_buf,' ',STATUS)
           end if

           IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
 
C		Loop through each row (line) in the text file
C		Open the input text file
           OPEN (UNIT=1,STATUS='OLD',FILE=INNAME)
c
c	  open appropriate record
c
           if ( .not. doubflag ) then
              CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',COLS,NUMCOL,
     *              'NONE',STATUS)
           else
              CALL IBIS_RECORD_OPEN(IBIS,RECORDD,'FORMAT:DOUB',
     *              COLS,NUMCOL,'DOUB',STATUS)
           end if
           IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           if ( DEBUG) call qprint('after record open',' ')
c
c          loop through all rows
c
	   ROW = 0                 ! initialize row
100	   CONTINUE

	      ROW = ROW + 1        ! increment row
	      DO J = 1, NUMCOL     ! clear data buffers
                 if ( .not. doubflag ) then
   		     ROWBUF(J) = 0.0
                 else
   		     DROWBUF(J) = 0.0
                 end if
	      ENDDO

              if ( .not. doubflag ) then
  	           READ (UNIT=1, FMT=*, ERR=110, END=150)  
     +				(ROWBUF(J), J=1,NUMCOL)
              else
	           READ (UNIT=1, FMT=*, ERR=110, END=150)  
     +				(DROWBUF(J), J=1,NUMCOL)
              end if
	      GOTO 120
	
110	      CONTINUE	!  Here for read error
	      WRITE (MSGBUF, '(A,I6)' )  'READ ERROR IN ROW : ', ROW
	      CALL XVMESSAGE (MSGBUF,' ')

120	      CONTINUE
              if ( .not. doubflag ) then
                  CALL IBIS_RECORD_WRITE(RECORD,ROWBUF,ROW,STATUS)
              else
                  CALL IBIS_RECORD_WRITE(RECORDD,DROWBUF,ROW,STATUS)
              end if
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	   IF (ROW .LT. LCOL) GOTO 100


150	   CONTINUE
	   CLOSE (UNIT=1)

        ELSE        !formatted input

            OPEN (UNIT=1, STATUS='OLD', FILE=INNAME)

C must parse the format line to define column types for ibis_file_open
           I=2
           COLCNT=1
           ICNT=1
           RCNT=1
           ACNT=1
           INTFLAG=.FALSE.
           REALFLAG=.FALSE.
           CHARFLAG=.FALSE.


           DO WHILE (FORMAT(I:I) .NE. ')')
              IF (FORMAT(I:I).EQ.'I' .OR. FORMAT(I:I).EQ.'i') THEN
                 CALL IBUFSETUP(FORMAT,COLCNT,I,FMT_BUF,ICNT)
                 INTFLAG=.TRUE.
              ELSE IF (FORMAT(I:I).EQ.'F' .OR. FORMAT(I:I).EQ.'f') THEN
                 CALL FBUFSETUP(FORMAT,COLCNT,I,FMT_BUF,RCNT)
                 REALFLAG=.TRUE.
              ELSE IF (FORMAT(I:I).EQ.'A' .OR. FORMAT(I:I).EQ.'a') THEN
                 CALL ABUFSETUP
     *                (FORMAT,COLCNT,I,FMT_BUF,MAXATEMP,ACNT,ACNTSTORE)
                 CHARFLAG=.TRUE.
              END IF
              I=I+1
           END DO

	   CALL IBIS_FILE_OPEN(OUTUNIT,IBIS,'WRITE',NCOL,LCOL,
     *                         FMT_BUF,' ',STATUS)
           IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

           IF (INTFLAG) THEN
              CALL IBIS_RECORD_OPEN(IBIS,RECORDF,'FORMAT:FULL',
     *                              0,0,'FULL',STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF
           IF (REALFLAG) THEN
              CALL IBIS_RECORD_OPEN(IBIS,RECORDR,'FORMAT:REAL',
     *                              0,0,'REAL',STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF
           IF (CHARFLAG) THEN
C round up maxatemp for nearest size of cxxxline
              IF (MAXATEMP .GT. 128) THEN
                 NEWMAX='A255'
              ELSE IF (MAXATEMP .GT. 64 .AND. MAXATEMP .LE. 128) THEN
                 NEWMAX='A128'
              ELSE IF (MAXATEMP .GT. 32 .AND. MAXATEMP .LE. 64) THEN
                 NEWMAX='A64'
              ELSE IF (MAXATEMP .GT. 16 .AND. MAXATEMP .LE. 32) THEN
                 NEWMAX='A32'
              ELSE IF (MAXATEMP .GT. 8 .AND. MAXATEMP .LE. 16) THEN
                 NEWMAX='A16'
              ELSE IF (MAXATEMP .GT. 4 .AND. MAXATEMP .LE. 8) THEN
                 NEWMAX='A8'
              ELSE 
                 NEWMAX='A4'
              END IF
              CALL IBIS_RECORD_OPEN(IBIS,RECORDA,'FORMAT:ASCII',
     *                              0,0,NEWMAX,STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF

C split the format statement into seperate homogenous types
           I=1
           IF (INTFLAG) THEN    ! create iformat with a,f masks
              DO WHILE (FORMAT(I:I) .NE. ')')
                 IF (FORMAT(I:I).EQ.'A' .OR. FORMAT(I:I).EQ.'a') THEN
                    CALL MASKAORI (FORMAT, I, IFORMAT)
                 ELSE IF(FORMAT(I:I).EQ.'F'.OR.FORMAT(I:I).EQ.'f') THEN
                    CALL MASKF (FORMAT, I, IFORMAT)
                 ELSE
                    IFORMAT(I:I)=FORMAT(I:I)
                 END IF
                 I=I+1
              END DO
              IFORMAT(I:I)=FORMAT(I:I)
           END IF
           I=1
           IF (REALFLAG) THEN    ! create iformat with a,f masks
              DO WHILE (FORMAT(I:I) .NE. ')')
                 IF (FORMAT(I:I).EQ.'A' .OR. FORMAT(I:I).EQ.'a' .OR.
     *               FORMAT(I:I).EQ.'I' .OR. FORMAT(I:I).EQ.'i') THEN
                    CALL MASKAORI (FORMAT, I, RFORMAT)
                 ELSE
                    RFORMAT(I:I)=FORMAT(I:I)
                 END IF
                 I=I+1
              END DO
              RFORMAT(I:I)=FORMAT(I:I)
           END IF
           I=1
           IF (CHARFLAG) THEN    ! create iformat with a,f masks
              DO WHILE (FORMAT(I:I) .NE. ')')
                 IF (FORMAT(I:I).EQ.'I' .OR. FORMAT(I:I).EQ.'i') THEN
                    CALL MASKAORI (FORMAT, I, AFORMAT)
                 ELSE IF(FORMAT(I:I).EQ.'F'.OR.FORMAT(I:I).EQ.'f') THEN
                    CALL MASKF (FORMAT, I, AFORMAT)
                 ELSE
                    AFORMAT(I:I)=FORMAT(I:I)
                 END IF
                 I=I+1
              END DO
              AFORMAT(I:I)=FORMAT(I:I)
           END IF
           ROW=1
           DO WHILE (ROW .LE. LCOL)
	      READ (UNIT=1 ,FMT='(A)',  END=200) DATALINE
              IF (INTFLAG) THEN
                 READ (DATALINE,IFORMAT) (INTLINE(J), J=1,ICNT-1)
	         CALL IBIS_RECORD_WRITE(RECORDF,INTLINE,0,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              END IF
              IF (REALFLAG) THEN
                 READ (DATALINE,RFORMAT) (REALLINE(J), J=1,RCNT-1)
	         CALL IBIS_RECORD_WRITE(RECORDR,REALLINE,0,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              END IF
              IF (CHARFLAG) THEN
                 IF (MAXATEMP .GT. 128) THEN 
                    READ (DATALINE,AFORMAT)(C255LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C255LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE IF (MAXATEMP .GT. 64 .AND. MAXATEMP .LE. 128) THEN
                    READ (DATALINE,AFORMAT)(C128LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C128LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE IF (MAXATEMP .GT. 32 .AND. MAXATEMP .LE. 64) THEN
                    READ (DATALINE,AFORMAT)(C64LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C64LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE IF (MAXATEMP .GT. 16 .AND. MAXATEMP .LE. 32) THEN
                    READ (DATALINE,AFORMAT)(C32LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C32LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE IF (MAXATEMP .GT. 8 .AND. MAXATEMP .LE. 16) THEN
                    READ (DATALINE,AFORMAT)(C16LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C16LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE IF (MAXATEMP .GT. 4 .AND. MAXATEMP .LE. 8) THEN
                    READ (DATALINE,AFORMAT) (C8LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C8LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 ELSE 
                    READ (DATALINE,AFORMAT) (C4LINE(J),J=1,ACNT-1)
	            CALL IBIS_RECORD_WRITE(RECORDA,C4LINE,0,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                 END IF
              END IF
              ROW = ROW + 1
	   ENDDO
        END IF

 200    CONTINUE
 
        IF (FREEFORMAT) THEN
           if ( .not.doubflag ) then
               CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
           else
               CALL IBIS_RECORD_CLOSE(RECORDD,STATUS)
           end if
           IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
        ELSE 
           IF (INTFLAG) THEN
              CALL IBIS_RECORD_CLOSE(RECORDF,STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF
           IF (REALFLAG) THEN
              CALL IBIS_RECORD_CLOSE(RECORDR,STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF
           IF (CHARFLAG) THEN
              CALL IBIS_RECORD_CLOSE(RECORDA,STATUS)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           END IF
        END IF

	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

	RETURN
	END


        SUBROUTINE IBUFSETUP (FORMAT,COLCNT,I,FMT_BUF,ICNT)

	IMPLICIT NONE
	INTEGER*4     COLCNT, I, J, ITEMP, INDEX, ICNT
	CHARACTER*72  FORMAT
        CHARACTER*5   FMT_BUF(1)
        LOGICAL       GFLAG

        IF (I .EQ. 2) THEN
           FMT_BUF(COLCNT)='FULL'
           COLCNT=COLCNT+1
           ICNT=ICNT+1
           RETURN
        END IF
        IF (I .EQ. 3) THEN
           READ (FORMAT(2:2),'(I1)') INDEX
           DO J=1,INDEX
              FMT_BUF(COLCNT)='FULL'
              COLCNT=COLCNT+1
              ICNT=ICNT+1
           END DO
           RETURN
        END IF

        CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)  !if gflag true - itemp=#
        IF (GFLAG) THEN
           CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
           IF (GFLAG) THEN
              CALL TESTCHAR (FORMAT(I-3:I-3),GFLAG)
              IF (GFLAG) THEN
                 READ (FORMAT(I-3:I-1),'(I3)') ITEMP
              ELSE
                 READ (FORMAT(I-2:I-1),'(I2)') ITEMP
              END IF
           ELSE
              READ (FORMAT(I-1:I-1),'(I1)') ITEMP
           END IF
           DO J=1,ITEMP
              FMT_BUF(COLCNT)='FULL'
              COLCNT=COLCNT+1
              ICNT=ICNT+1
           END DO
        ELSE
           FMT_BUF(COLCNT)='FULL'
           COLCNT=COLCNT+1
           ICNT=ICNT+1
        END IF
        RETURN
	END

        SUBROUTINE FBUFSETUP (FORMAT,COLCNT,I,FMT_BUF,RCNT)

	IMPLICIT NONE
	INTEGER*4     COLCNT, I, J, ITEMP, RCNT
	CHARACTER*72  FORMAT
        CHARACTER*5   FMT_BUF(1)
        LOGICAL       GFLAG

        IF (I .EQ. 2) THEN
           FMT_BUF(COLCNT)='REAL'
           COLCNT=COLCNT+1
           RCNT=RCNT+1
           RETURN
        END IF
        IF (I .EQ. 3) THEN
           READ (FORMAT(2:2),'(I1)') ITEMP
           DO J=1,ITEMP
              FMT_BUF(COLCNT)='REAL'
              COLCNT=COLCNT+1
           RCNT=RCNT+1
           END DO
           RETURN
        END IF

        CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)  !if gflag true - itemp=#
        IF (GFLAG) THEN
           CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
           IF (GFLAG) THEN
              READ (FORMAT(I-2:I-1),'(I2)') ITEMP
           ELSE
              READ (FORMAT(I-1:I-1),'(I1)') ITEMP
           END IF
           DO J=1,ITEMP
              FMT_BUF(COLCNT)='REAL'
              COLCNT=COLCNT+1
              RCNT=RCNT+1
           END DO
        ELSE
           FMT_BUF(COLCNT)='REAL'
           COLCNT=COLCNT+1
           RCNT=RCNT+1
        END IF
        RETURN
	END

        SUBROUTINE ABUFSETUP 
     *             (FORMAT,COLCNT,I,FMT_BUF,MAXATEMP,ACNT,ACNTSTORE)

	IMPLICIT NONE                             !I is position of A in format
	INTEGER*4     COLCNT, I, J, ITEMP, TEMPA  !colcnt is column count
        INTEGER*4     MAXATEMP, ACNT, ACNTSTORE(1)
	CHARACTER*72  FORMAT                      !fmt_buf is character array
        CHARACTER*5   FMT_BUF(1), ATEMP           !input to ibis_file_open
        LOGICAL       GFLAG

C first find format number -- 'A' format type must be followed by a number
C all 'A' types will be the size of the largest one in the line

        CALL TESTCHAR (FORMAT(I+2:I+2),GFLAG)
        IF (GFLAG) THEN
           CALL TESTCHAR (FORMAT(I+3:I+3),GFLAG)
           IF (GFLAG) THEN
              READ (FORMAT(I+1:I+3),'(I3)') TEMPA
              MAXATEMP= MAX(MAXATEMP,TEMPA)
              IF (MAXATEMP .GT. 128) THEN
                 ATEMP='A255'
              ELSE 
                 ATEMP='A128'
              END IF
           ELSE
              READ (FORMAT(I+1:I+2),'(I2)') TEMPA
              MAXATEMP= MAX(MAXATEMP,TEMPA)
              IF (MAXATEMP .EQ. TEMPA) THEN
                 IF (MAXATEMP .GT. 64) THEN
                    ATEMP='A128'
                 ELSE IF (MAXATEMP .GT. 32 .AND. MAXATEMP .LE. 64) THEN
                    ATEMP='A64'
                 ELSE IF (MAXATEMP .GT. 16 .AND. MAXATEMP .LE. 32) THEN
                    ATEMP='A32'
                 ELSE
                    ATEMP='A16'
                 END IF
              END IF
           END IF
        ELSE
           READ (FORMAT(I+1:I+1),'(I1)') TEMPA
           MAXATEMP= MAX(MAXATEMP,TEMPA)
           IF (MAXATEMP .EQ. TEMPA) THEN
              IF (MAXATEMP .GT. 8) THEN
                 ATEMP='A16'
              ELSE IF (MAXATEMP .GT. 4 .AND. MAXATEMP .LE. 8) THEN
                 ATEMP='A8'
              ELSE 
                 ATEMP='A4'
              END IF
           END IF
        END IF
        ACNTSTORE(ACNT)=COLCNT

C update any previous char columns to max size in line
        DO J=1,ACNT
           FMT_BUF(ACNTSTORE(J))=ATEMP
        END DO
        
C format(1:1) always '(' so start looking at position 2
        IF (I .EQ. 2) THEN
           FMT_BUF(COLCNT)=ATEMP
           ACNTSTORE(ACNT)=COLCNT
           COLCNT=COLCNT+1
           ACNT=ACNT+1
           RETURN
        END IF

        IF (I .EQ. 3) THEN
           READ (FORMAT(I-1:I-1),'(I1)') ITEMP
           DO J=1,ITEMP
              FMT_BUF(COLCNT)=ATEMP
              ACNTSTORE(ACNT)=COLCNT
              COLCNT=COLCNT+1
              ACNT=ACNT+1
           END DO
           RETURN
        END IF

C any position in format past the second
C determining repeater number between 1 and max possible if present

        CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)  !if gflag true -> itemp=#
        IF (GFLAG) THEN
           CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
           IF (GFLAG) THEN
              CALL TESTCHAR (FORMAT(I-3:I-3),GFLAG)
              IF (GFLAG) THEN
                 READ (FORMAT(I-3:I-1),'(I3)') ITEMP
              ELSE
                 READ (FORMAT(I-2:I-1),'(I2)') ITEMP
              END IF
           ELSE
              READ (FORMAT(I-1:I-1),'(I1)') ITEMP
           END IF
           DO J=1,ITEMP
              FMT_BUF(COLCNT)=ATEMP
              ACNTSTORE(ACNT)=COLCNT
              COLCNT=COLCNT+1
              ACNT=ACNT+1
           END DO
        ELSE
           FMT_BUF(COLCNT)=ATEMP
           ACNTSTORE(ACNT)=COLCNT
           COLCNT=COLCNT+1
           ACNT=ACNT+1
        END IF
        RETURN
	END

        SUBROUTINE TESTCHAR (TSTCHAR,GFLAG)

	IMPLICIT NONE
        CHARACTER*1   TSTCHAR
        LOGICAL       GFLAG

        IF (TSTCHAR .EQ. ',' .OR. TSTCHAR .EQ. '(' .OR.
     *      TSTCHAR .EQ. '.' .OR. TSTCHAR .EQ. ')' ) THEN
           GFLAG=.FALSE.
        ELSE
           GFLAG=.TRUE.
        END IF
        RETURN
	END

        SUBROUTINE MASKAORI (FORMAT, I, XFORMAT)

	IMPLICIT NONE
	INTEGER*4     I, TEMP1, TEMP2, TEMP3
	CHARACTER*72  FORMAT, XFORMAT
        LOGICAL       GFLAG

        CALL TESTCHAR (FORMAT(I+2:I+2),GFLAG)
        IF (GFLAG) THEN
           CALL TESTCHAR (FORMAT(I+3:I+3),GFLAG)
           IF (GFLAG) THEN                        !3 #s after I
              IF (I .EQ. 2) THEN                  !can't have two
                 XFORMAT(2:4)=FORMAT(3:5)         !digit repeater
                 XFORMAT(5:5)='X'
                 I=5
              ELSE
                 CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)
                 IF (GFLAG) THEN
                    READ (FORMAT(I-1:I-1),'(I1)') TEMP1
                    READ (FORMAT(I+1:I+3),'(I3)') TEMP2
                    TEMP3 = TEMP1 * TEMP2
                    WRITE(XFORMAT(I-1:I+1),'(I3)') TEMP3
                    XFORMAT(I+2:I+3)='X '
                    I=I+3
                 ELSE                             ! no repeat
                    XFORMAT(I:I+2)=FORMAT(I+1:I+3)
                    XFORMAT(I+3:I+3)='X'
                    I=I+3
                 END IF
              END IF
           ELSE              !two numbers after A
              IF (I .EQ. 2) THEN
                 XFORMAT(2:3)=FORMAT(3:4)
                 XFORMAT(4:4)='X'
                 I=4
              ELSE IF (I .EQ. 3) THEN
                 READ (FORMAT(2:2),'(I1)') TEMP1
                 READ (FORMAT(4:5),'(I2)') TEMP2
                 TEMP3 = TEMP1 * TEMP2
                 IF (TEMP3 .LE. 99) THEN
                    WRITE(XFORMAT(2:3),'(I2)') TEMP3
                    XFORMAT(4:5)='X '
                 ELSE
                    WRITE(XFORMAT(2:4),'(I3)') TEMP3
                    XFORMAT(5:5)='X'
                 END IF
                 I=5
              ELSE
                 CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)
                 IF (GFLAG) THEN
                    CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
                    IF (GFLAG) THEN
                       READ (FORMAT(I-2:I-1),'(I2)') TEMP1
                       READ (FORMAT(I+1:I+2),'(I2)') TEMP2
                       TEMP3 = TEMP1 * TEMP2
                       WRITE(XFORMAT(I-2:I),'(I3)') TEMP3
                       XFORMAT(I+1:I+2)='X '
                       I=I+2
                    ELSE
                       READ (FORMAT(I-1:I-1),'(I1)') TEMP1
                       READ (FORMAT(I+1:I+2),'(I2)') TEMP2
                       TEMP3 = TEMP1 * TEMP2
                       IF (TEMP3 .LE. 99) THEN
                          WRITE(XFORMAT(I-1:I),'(I2)') TEMP3
                          XFORMAT(I+1:I+2)='X '
                       ELSE
                          WRITE(XFORMAT(I-1:I+1),'(I3)')TEMP3
                          XFORMAT(I+2:I+2)='X'
                       END IF
                       I=I+2
                    END IF
                 ELSE                 !no repeats
                    XFORMAT(I:I+1)=FORMAT(I+1:I+2)
                    XFORMAT(I+2:I+2)='X'
                    I=I+2
                 END IF
              END IF
           END IF
        ELSE                !only 1 # after A
           CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)
           IF (GFLAG) THEN
              CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
              IF (GFLAG) THEN
                 CALL TESTCHAR (FORMAT(I-3:I-3),GFLAG)
                 IF (GFLAG) THEN
                    READ (FORMAT(I-3:I-1),'(I3)') TEMP1
                    READ (FORMAT(I+1:I+1),'(I1)') TEMP2
                    TEMP3 = TEMP1 * TEMP2
                    WRITE(XFORMAT(I-3:I-1),'(I3)') TEMP3
                    XFORMAT(I:I+1)='X '
                    I=I+1
                 ELSE
                    READ (FORMAT(I-2:I-1),'(I2)') TEMP1
                    READ (FORMAT(I+1:I+1),'(I1)') TEMP2
                    TEMP3 = TEMP1 * TEMP2
                    IF (TEMP3 .LE. 99) THEN
                       WRITE(XFORMAT(I-2:I-1),'(I2)') TEMP3
                       XFORMAT(I:I+1)='X '
                    ELSE
                       WRITE(XFORMAT(I-2:I),'(I3)')TEMP3
                       XFORMAT(I+1:I+1)='X'
                    END IF
                    I=I+1
                 END IF
              ELSE        !1 repeat and 1 after 
                 READ (FORMAT(I-1:I-1),'(I1)') TEMP1
                 READ (FORMAT(I+1:I+1),'(I1)') TEMP2
                 TEMP3 = TEMP1 * TEMP2
                 IF (TEMP3 .LE. 9) THEN
                    WRITE(XFORMAT(I-1:I-1),'(I1)') TEMP3
                    XFORMAT(I:I+1)='X '
                 ELSE
                    WRITE(XFORMAT(I-1:I),'(I2)')TEMP3
                    XFORMAT(I+1:I+1)='X'
                 END IF
                 I=I+1
              END IF
           ELSE                 !no repeats
              XFORMAT(I:I)=FORMAT(I+1:I+1)
              XFORMAT(I+1:I+1)='X'
              I=I+1
           END IF
        END IF
        RETURN
        END

        SUBROUTINE MASKF (FORMAT, I, XFORMAT)

	IMPLICIT NONE
	INTEGER*4     I, TEMP1, TEMP2, TEMP3
	CHARACTER*72  FORMAT, XFORMAT
        LOGICAL       GFLAG

        CALL TESTCHAR (FORMAT(I+2:I+2),GFLAG)
        IF (GFLAG) THEN                        !max 2 #s before
           IF (I .EQ. 2) THEN                  !and 2 #s after f 
              XFORMAT(2:3)=FORMAT(3:4)
              XFORMAT(4:6)='X  '
              I=6
           ELSE IF (I .EQ. 3) THEN 
              READ (FORMAT(I-1:I-1),'(I1)') TEMP1
              READ (FORMAT(I+1:I+2),'(I2)') TEMP2
              TEMP3 = TEMP1 * TEMP2
              WRITE(XFORMAT(I-1:I),'(I2)') TEMP3
              XFORMAT(I+1:I+4)='X   '
              I=7
           ELSE
              CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)
              IF (GFLAG) THEN
                 CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
                 IF (GFLAG) THEN
                    READ (FORMAT(I-2:I-1),'(I2)') TEMP1
                    READ (FORMAT(I+1:I+2),'(I2)') TEMP2
                    TEMP3 = TEMP1 * TEMP2
                    WRITE(XFORMAT(I-2:I),'(I3)') TEMP3
                    XFORMAT(I+1:I+4)='X   '
                    I=I+4
                 ELSE
                    READ (FORMAT(I-1:I-1),'(I1)') TEMP1
                    READ (FORMAT(I+1:I+2),'(I2)') TEMP2
                    TEMP3 = TEMP1 * TEMP2
                    IF (TEMP3 .LE. 99) THEN
                       WRITE(XFORMAT(I-1:I),'(I2)') TEMP3
                       XFORMAT(I+1:I+4)='X   '
                    ELSE
                       WRITE(XFORMAT(I-1:I+1),'(I3)')TEMP3
                       XFORMAT(I+2:I+4)='X  '
                    END IF
                    I=I+4
                 END IF
              ELSE                 !no repeats
                 XFORMAT(I:I+1)=FORMAT(I+1:I+2)
                 XFORMAT(I+2:I+4)='X  '
                 I=I+4
              END IF
           END IF
        ELSE                !only 1 # after F
           CALL TESTCHAR (FORMAT(I-1:I-1),GFLAG)
           IF (GFLAG) THEN
              CALL TESTCHAR (FORMAT(I-2:I-2),GFLAG)
              IF (GFLAG) THEN
                 READ (FORMAT(I-2:I-1),'(I2)') TEMP1
                 READ (FORMAT(I+1:I+1),'(I1)') TEMP2
                 TEMP3 = TEMP1 * TEMP2
                 IF (TEMP3 .LE. 99) THEN
                    WRITE(XFORMAT(I-2:I-1),'(I2)') TEMP3
                    XFORMAT(I:I+3)='X   '
                 ELSE
                    WRITE(XFORMAT(I-2:I),'(I3)')TEMP3
                    XFORMAT(I+1:I+3)='X  '
                 END IF
                 I=I+1
              ELSE        !1 repeat and 1 after 
                 READ (FORMAT(I-1:I-1),'(I1)') TEMP1
                 READ (FORMAT(I+1:I+1),'(I1)') TEMP2
                 TEMP3 = TEMP1 * TEMP2
                 IF (TEMP3 .LE. 9) THEN
                    WRITE(XFORMAT(I-1:I-1),'(I1)') TEMP3
                    XFORMAT(I:I+3)='X   '
                 ELSE
                    WRITE(XFORMAT(I-1:I),'(I2)')TEMP3
                    XFORMAT(I+1:I+3)='X  '
                 END IF
                    I=I+3
              END IF
           ELSE                 !no repeats 1 # after I not counting after .
              XFORMAT(I:I)=FORMAT(I+1:I+1)
              XFORMAT(I+1:I+3)='X  '
              I=I+3
           END IF
        END IF
        RETURN
        END

