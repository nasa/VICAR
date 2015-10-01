$!****************************************************************************
$!
$! Build proc for MIPL module vquic
$! VPACK Version 1.9, Wednesday, May 16, 2012, 16:47:34
$!
$! Execute by entering:		$ @vquic
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module vquic ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vquic.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("vquic.imake") .nes. ""
$   then
$      vimake vquic
$      purge vquic.bld
$   else
$      if F$SEARCH("vquic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vquic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vquic.bld "STD"
$   else
$      @vquic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vquic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vquic.com -mixed -
	-s vquic.f -
	-i vquic.imake -
	-p vquic.pdf -
	-t tstvquic.pdf tstvquic.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vquic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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


        CALL IFMESSAGE('VQUIC version 13-OCT-98')
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vquic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM vquic

   To Create the build file give the command:

		$ vimake vquic			(VMS)
   or
		% vimake vquic			(Unix)


************************************************************************/


#define PROGRAM	vquic
#define R2LIB

#define MODULE_LIST vquic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create vquic.pdf
PROCESS HELP=*
PARM INPUT TYPE=(STRING)
PARM OUT  TYPE=(STRING)
PARM NCOL TYPE=INTEGER DEFAULT=1 valid=1:100
PARM LCOL TYPE=INTEGER DEFAULT=0
PARM COLS TYPE=INTEGER COUNT=1:100 DEFAULT=1
PARM FORMAT TYPE=STRING DEFAULT="*"
PARM DOUB TYPE=KEYWORD COUNT=(0:1) VALID="DOUB" DEFAULT=--
PARM DEBUG TYPE=KEYWORD COUNT=(0:1) VALID="DEBUG" DEFAULT=--


!# annot keywords=(IBIS,"ASCII file")

END-PROC
.TITLE
IBIS program coverting an ASCII file into an IBIS file
.HELP
PURPOSE

    "VQUIC" transfers data from an ascii text file to an IBIS interface
(tabular) file.  The data can be both numeric and character.



EXECUTION:

vquic INPUT=DATA.TXT  OUT=DATA.INT  NCOL=4  LCOL=1500 FORMAT="(1X,F5.3,A4)"

vquic DATA2.TXT DATA2.INT   COLS=(1,4,5)


  Each line in the input ascii text file is transfered to the corresponding 
row in the interface file.  The input data may be either free formatted 
numbers, in which case the FORMAT parameter is defaulted, or may be 
formatted characters and/or numbers, in which case the appropriate 
Fortran format statement must be specified.  If free formatting is used
the data separators may be spaces, tabs, or commas.  The FORMAT parameter
may only contain A, F or I types;  double-precision data must be entered
in free format ("*"), which is the default format.

  The output file is an IBIS interface file with NCOL columns and LCOL rows.  
If NCOL is defaulted then it will be found from the maximum column number
specified with the COLS parameter.  If there are less than LCOL lines in 
the input file then the remaining rows will be filled with zeros.  If there 
are more than LCOL lines then the remaining lines will not be read.   
If LCOL is defaulted then the LCOL will be found from the length of the 
input file. 

  The COLS parameter specifies the columns that will receive the data.
If there are more numbers on a line than columns, the remaining numbers 
will be ignored.  If there are less numbers then if free formatting is 
being used the missing numbers will be read from the next line, otherwise 
zeros will be put in for the missing numbers.

  If a input line generates a read error then a warning will be printed
and zeros will be transfered for the remaining columns in the row.


RESTRICTIONS:

The maximum input line length is 255 characters.
The maximum number of columns in the interface file is 100.

HISTORY:

2-JAN-1995  AMS  (CRI) Made portable for UNIX


.LEVEL1
.VARIABLE INPUT
Input text data file
.VARIABLE OUT
Output IBIS interface file
.VARIABLE NCOL
The number of columns in
the output interface file
.VARIABLE LCOL
The number of rows in
the output interface file
(Optional)
.VARIABLE COLS
The columns to receive
the data
.VARIABLE FORMAT
The Fortran format
specifier.  E.g.
"(2A4,2X,2F5.1,1X,I9)"
.VARIABLE DOUB
DOUB - for the case of
free-formatted input as
double precision values.
.VARIABLE DEBUG
Diagnostic information.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstvquic.pdf
procedure
refgbl $autousage
refgbl $echo
body

let _onfail="continue"
let $autousage="none"
let $echo="yes"

flag-add NOMESSAGE

ibis-gen a nc=3 nr=10 strcol=3 indexcol=2 format=(real,real,a4) +
  string=(this,is,a,strg,vald,colm,hee,hee,hee,hee) 'ibis-1
!
! Note: the additional "1X" in the vquic below is
! due to control characters in the output file data.txt
!
ibis-list |stdout=data.txt| a cform="%6.3f %6.3f %4.4s" 'nohead 'nocol a4col=3
vquic input=data.txt out=data.int ncol=3 lcol=15 format="(1X,2F6.3,1X,A4)"
ibis-list data.int a4col=3
!
ibis-gen a nc=3 nr=10 datacol=1 indexcol=2 data=(2,4,3,5.1,1000.2,6,7,8,9)
ibis-list |stdout=a.lis| a 'nohead 'nocol
! ar-9577
! test too big for ncol
let $echo="no"
write "The next test should generate an error message!"
let $echo="yes"
vquic input=a.lis out=b ncol=999 cols=(1,3,4)
!
vquic input=a.lis out=b ncol=4 cols=(1,3,4)
ibis-list b

!Test large-file FR#86967
ibis-gen a nc=3 nr=200
ibis-list |stdout=a.lis| a 'nohead 'nocol nr=10
vquic input=a.lis out=b ncol=3 cols=(1,2,3)
ibis-list b nr=10

!Test integer and mixed files
ibis-gen a nc=3 nr=3 format=(a5,full,real) indexcol=2 datacol=3 +
  strcol=1 data=(4,5,6) string=(xxxxx,yyyyy,zzzzz)
ibis-list |stdout=data2.txt| a cform="%5.5s%6.6d%7.2f" 'nohead 'nocol a4col=3
vquic input=data2.txt out=g ncol=3 format="(a5,I6,F7.2)"
ibis-list g

!Test double precision input
ush echo \"5 5 5 5 5 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 4 4 4 4 4 4\" >> x
vquic x a ncol=7 lcol=5 cols=(1,2,3,4,5,6,7) 'doub
ibis-list a 'format

flag-delete NOMESSAGE

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstvquic.log_solos
 tstvquic
flag-add NOMESSAGE
ibis-gen a nc=3 nr=10 strcol=3 indexcol=2 format=(real,real,a4)  +
  string=(this,is,a,strg,vald,colm,hee,hee,hee,hee) 'ibis-1
ibis-list |stdout=data.txt| a cform="%6.3f %6.3f %4.4s" 'nohead 'nocol a4col=3
 0.000  1.000 this
 0.000  2.000 is  
 0.000  3.000 a   
 0.000  4.000 strg
 0.000  5.000 vald
 0.000  6.000 colm
 0.000  7.000 hee 
 0.000  8.000 hee 
 0.000  9.000 hee 
 0.000 10.000 hee 
vquic input=data.txt out=data.int ncol=3 lcol=15 format="(1X,2F6.3,1X,A4)"
ibis-list data.int a4col=3
 
Number of Rows:15  Number of Columns: 3       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:15
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        0.00        1.00        this
        0.00        2.00          is
        0.00        3.00           a
        0.00        4.00        strg
        0.00        5.00        vald
        0.00        6.00        colm
        0.00        7.00         hee
        0.00        8.00         hee
        0.00        9.00         hee
        0.00       10.00         hee
        0.00        0.00            
        0.00        0.00            
        0.00        0.00            
        0.00        0.00            
        0.00        0.00            
ibis-gen a nc=3 nr=10 datacol=1 indexcol=2 data=(2,4,3,5.1,1000.2,6,7,8,9)
ibis-list |stdout=a.lis| a 'nohead 'nocol
        2.00        1.00        0.00
        4.00        2.00        0.00
        3.00        3.00        0.00
        5.10        4.00        0.00
     1000.20        5.00        0.00
        6.00        6.00        0.00
        7.00        7.00        0.00
        8.00        8.00        0.00
        9.00        9.00        0.00
        0.00       10.00        0.00
let $echo="no"
The next test should generate an error message!
vquic input=a.lis out=b ncol=999 cols=(1,3,4)
[TAE-RANGE] Value out of the defined range for 'NCOL'.;
 proc 'tstvquic', line 29
continue
vquic input=a.lis out=b ncol=4 cols=(1,3,4)
ibis-list b
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        2.00        0.00        1.00        0.00
        4.00        0.00        2.00        0.00
        3.00        0.00        3.00        0.00
        5.10        0.00        4.00        0.00
     1000.20        0.00        5.00        0.00
        6.00        0.00        6.00        0.00
        7.00        0.00        7.00        0.00
        8.00        0.00        8.00        0.00
        9.00        0.00        9.00        0.00
        0.00        0.00       10.00        0.00
ibis-gen a nc=3 nr=200
ibis-list |stdout=a.lis| a 'nohead 'nocol nr=10
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
vquic input=a.lis out=b ncol=3 cols=(1,2,3)
ibis-list b nr=10
 
Number of Rows:10  Number of Columns: 3       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
ibis-gen a nc=3 nr=3 format=(a5,full,real) indexcol=2 datacol=3  +
  strcol=1 data=(4,5,6) string=(xxxxx,yyyyy,zzzzz)
ibis-list |stdout=data2.txt| a cform="%5.5s%6.6d%7.2f" 'nohead 'nocol a4col=3
xxxxx000001   4.00
yyyyy000002   5.00
zzzzz000003   6.00
vquic input=data2.txt out=g ncol=3 format="(a5,I6,F7.2)"
ibis-list g
 
Number of Rows:3  Number of Columns: 3       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:3
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
       xxxxx           1        4.00
       yyyyy           2        5.00
       zzzzz           3        6.00
ush echo \"5 5 5 5 5 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 8 8 8 8 8 8\" >> x
ush echo \"8 4 4 4 4 4 4\" >> x
vquic x a ncol=7 lcol=5 cols=(1,2,3,4,5,6,7) 'doub
ibis-list a 'format
 
Number of Rows:5  Number of Columns: 7       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
        DOUB        DOUB        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
        5.00        5.00        5.00        5.00        5.00        8.00
        8.00        8.00        8.00        8.00        8.00        8.00
        8.00        8.00        8.00        8.00        8.00        8.00
        8.00        8.00        8.00        8.00        8.00        8.00
        8.00        4.00        4.00        4.00        4.00        4.00
 
Rows: 1:5
+-----------
         C:7
        DOUB
+-----------
        8.00
        8.00
        8.00
        8.00
        4.00
flag-delete NOMESSAGE
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
