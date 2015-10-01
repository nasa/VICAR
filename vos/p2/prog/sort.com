$!****************************************************************************
$!
$! Build proc for MIPL module sort
$! VPACK Version 1.8, Wednesday, January 18, 1995, 07:44:43
$!
$! Execute by entering:		$ @sort
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
$ write sys$output "*** module sort ***"
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
$ write sys$output "Invalid argument given to sort.com file -- ", primary
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
$   if F$SEARCH("sort.imake") .nes. ""
$   then
$      vimake sort
$      purge sort.bld
$   else
$      if F$SEARCH("sort.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sort
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sort.bld "STD"
$   else
$      @sort.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sort.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sort.com -
	-s sort.f sort.fin -
	-i sort.imake -
	-p sort.pdf -
	-t tstsort.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sort.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  IBIS ROUTINE SORT
C
C  PURPOSE:  SORT ALL COLUMNS OF AN IBIS INTERFACE FILE BASED UPON ONE
C  ONE OR MORE CONTROL KEY COLUMNS.  THE KEYS MAY BE NUMERIC OR
C  APHABETIC.  THE RESULTING LEXICOGRAPHIC ORDER MAY BE STORED IN 
C  AN INDEX COLUMN.
C
C  USER PARAMETERS:
C
C  SORTCOL,N1,...NK - THE INTEGERS N1 THROUGH NK SPECIFY THE COLUMNS 
C     	BEING SORTED. N1 IS THE PRIMARY SORT COLUMN, ETC.
C  DESCEND - THIS KEYWORD INDICATES THAT ALL COLUMNS WILL BE SORTED IN
C	DESCENDING ORDER.
C  INDEXCOL,N - THE INTEGER N DESIGNATES A COLUMN TO RECEIVE AN ORDINAL
C	CREATED BY THE SORTING PROCESS.  AN INDEX NUMBER IS ASSIGNED TO
C	EACH UNIQUE COMBINATION OF LETTERS OR NUMBERS IN THE SORTED LIST.
C  ALPHA -   INDICATES THAT THE SORT COLUMNS ARE ALPHABETIC.

      INCLUDE 'sort.fin'
      INTEGER RUNIT,STATUS,LST,I,PTR
C
C  INITIALIZE, GET PARAMETERS, OPEN FILES
C
      CALL IFMESSAGE('SORT version 6-MAR-95')
      CALL XVUNIT( RUNIT,'INP', 1, STATUS,' ')
      IF (STATUS.NE.1) THEN
	   CALL XVMESSAGE('INPUT FILE NOT FOUND by XVUNIT',' ')
	   CALL ABEND
      ENDIF
      CALL IBIS_FILE_OPEN(RUNIT,IBIS,'UPDATE',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL GET_PARMS
      
      CALL SET_UP_FILE
 
      CALL SORT_FILE     
	
C
C  MOVE ENTIRE FILE ACCORDING TO POINTERS CX, using the most
C   EFFICIENT METHODS. IN SOME CASES, BOTH METHODS WILL BE USED
C   (E.G. COLUMN-ORIENTED FILES HAVING A COLUMN WIDER THAN 8 BYTES).
C

      if (NCCOLS.GT.0) CALL CORMOVCOLUMNS
      
      if (NRCOLS.GT.0) CALL CORMOVRECORD

C
C  Set up INDEX Column, if requested
	IF (INDEX.GT.0) THEN
	    LST = -1
	    PTR = 0
	    DO I=1,CLEN
	       IF (CS(I).NE.LST) PTR = PTR+1.
	       COL1(I) = PTR
	       LST = CS(I)
	    ENDDO
 	    CALL IBIS_COLUMN_SET(IBIS,'U_FORMAT','FULL',INDEX,STATUS) 
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL IBIS_COLUMN_WRITE(IBIS,COL1,INDEX,1,CLEN,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDIF
C
	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)

	RETURN
	END



      SUBROUTINE GET_PARMS
      INCLUDE 'sort.fin'
      INTEGER DEF,COUNT,IBIS_FILE_GET,STATUS
      LOGICAL XVPTST
      CHARACTER*7  IBISFTYPE,IBISORG

      IBIS1 = .FALSE.
      IBIS2 = .FALSE.
      CALL XVPARM ('SORTCOL',SORTCL,SORTCNT,DEF,20)
      CALL XVP ('INDEXCOL',INDEX,COUNT)
      ALPH  = XVPTST ('ALPHA')
      DSCND = XVPTST ('DESCEND')     
      COUNT = IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'VERSION',IBISFTYPE ,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'ORG',IBISORG ,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      ROWORG = (IBISORG(1:3).EQ.'ORG')
      IF (IBISFTYPE (1:6).EQ.'IBIS-1') THEN
          IBIS1 = .TRUE.
          CF = 'A4' 
      ELSE
          IF (IBISFTYPE(1:6).EQ.'IBIS-2') THEN
             IBIS2 = .TRUE.
             CF = 'A8' 
          ELSE
             CALL MABEND('IBIS FILE TYPE UNKNOWN')
          ENDIF
      ENDIF 	
	
      RETURN
      END





      SUBROUTINE SET_UP_FILE
      INCLUDE 'sort.fin'
      INTEGER I,IY,STATUS,CSIZE
      CHARACTER*10 FMT

C
C  SET UP COLUMN FORMATS FOR IBIS-1
C
      if (IBIS1.AND.ALPH) then
         do i=1,SORTCNT
            iy=SORTCL(i)
 	    call IBIS_COLUMN_SET(IBIS,'FORMAT',CF,iy,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
         enddo
      endif
C
C  SET UP COLUMN TRANSLATION, and determine which method of
C     data shuffling (by row/by column) should be done for each.
C     Columns wider than 8 bytes must use record transfer, and
C     row-oriented files should use record transfer regardless.
C
      NCCOLS=0
      NRCOLS=0
      do i=1,ncol
         if (i.ne.INDEX) then
	    CALL IBIS_COLUMN_GET(IBIS,'FORMAT',FMT,I,STATUS)	    
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL IBIS_COLUMN_GET(IBIS,'U_SIZE',CSIZE,I,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    IF (.NOT.ROWORG.AND.CSIZE.LE.8) THEN
	    	NCCOLS = NCCOLS+1
		CCOLS(NCCOLS) = I
	    ELSE
	    	NRCOLS = NRCOLS+1
		RCOLS(NRCOLS) = I
	    ENDIF
            if (FMT(1:1).EQ.'A'.OR.FMT(1:1).EQ.'a') then
               call IBIS_COLUMN_SET(IBIS,'U_FORMAT',CF,i,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            else      ! (.not.) ALPH
               call IBIS_COLUMN_SET(IBIS,'U_FORMAT','REAL',i,STATUS) 
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    endif
         endif
      enddo

C
C  SET UP COLUMN ARRAYS
c

      DO I=1,CLEN
	 CX(I) = I
	 CS(I) = 1
	 if (IBIS1.AND.ALPH) then
            COL1ALPH(I) ='    '
            COL1ATEMP(I)='    '
	 endif
         if (IBIS2.AND.ALPH) then
            COL1I2A(I) = '        '
            COL1I2AT(I)= '        '
         endif
      ENDDO
      
      RETURN
      END



 
      SUBROUTINE SORT_FILE
      INCLUDE 'sort.fin'
      INTEGER IX,STATUS,PU,PL,SLEN,PK,IT1,IIT,K,KK,OLDCS
      INTEGER  TOP,BOT,TEMP

C
C  PERFORM SORT.  CS WILL CONTAIN LEXICOGRAPHIC INDEX.  CX WILL CONTAIN
C  POINTERS.  THE FILE IS NOT MOVED HERE. First sort is only for ibis-1 
C  files.

      DO IX=1,SORTCNT
	 if (ALPH) then
            if (IBIS1) then
     	       CALL IBIS_COLUMN_READ(IBIS,COL1ALPH,SORTCL(IX),1,
     +                              CLEN,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	       call CORMOVALPH (COL1ALPH,CX,CLEN)
	    else            ! IBIS2
     	       CALL IBIS_COLUMN_READ(IBIS,COL1I2A,SORTCL(IX),1,
     +                              CLEN,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	       call CORMOVI2A (COL1I2A,CX,CLEN)
            endif            ! IBIS1 & IBIS2 & ALPH
         else                ! (not) ALPH
            CALL IBIS_COLUMN_READ(IBIS,COL1,SORTCL(IX),1,
     +                             CLEN,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL CORMOV (COL1,CX,CLEN)
    	 endif
	 PU = 1
	 PL = 1
	 DO WHILE (PL.LE.CLEN)
	    DO WHILE (PU.LT.CLEN.AND.CS(PU+1).EQ.CS(PL))
	      PU = PU+1
	    ENDDO
	    SLEN = PU-PL+1
            call MVE(7,SLEN,CX(PL),CXTEMP,1,1)
	    IF (ALPH) THEN
               PK=PL+SLEN-1
               IT1=1
               DO IIT=PL,PK
		  if (IBIS1) COL1ATEMP(IT1) = COL1ALPH(IIT)
		  if (IBIS2) COL1I2AT(IT1)  = COL1I2A(IIT)
		  IT1=IT1+1
	       ENDDO
	       if (IBIS1) CALL CSORTP(COL1ALPH(PL),1,SLEN,1,4,IP)
	       if (IBIS2) call CSORTP(COL1I2A(PL) ,1,SLEN,1,8,IP)
	    ELSE             ! (not) ALPH
	       call MVE(7,SLEN,COL1(PL),COL1TEMP,1,1)
	       CALL SSORTP (COL1(PL),1,SLEN,IP)
	    ENDIF
	    do K = 1,SLEN
	       KK = K + PL - 1
               if (ALPH) then
                  if (IBIS1) COL1ALPH(KK) = COL1ATEMP(IP(K))
		  if (IBIS2) COL1I2A(KK)  = COL1I2AT(IP(K))
               else           ! (not) ALPH
	         COL1(KK) = COL1TEMP(IP(K))
               endif
               CX(KK)   = CXTEMP(IP(K))
	    enddo	   
	    PL = PU + 1
	 ENDDO                ! while
 	 PU = 1
	 PL = 1
	 OLDCS = CS(1)
	 CS(1) = 1
	 DO WHILE (PU.LT.CLEN)
            if (ALPH) then
               if (IBIS1) then
	          IF (COL1ALPH(PU+1).NE.COL1ALPH(PL)
     +	                  .OR.CS(PU+1).NE.OLDCS) THEN
	             PL = PU+1
	             OLDCS = CS(PL)
	          ENDIF
	       else      ! IBIS2
	          IF (COL1I2A(PU+1).NE.COL1I2A(PL)
     +	                  .OR.CS(PU+1).NE.OLDCS) THEN
              	     PL = PU+1
                     OLDCS = CS(PL)
	          ENDIF
               endif   ! IBIS1, IBIS2 & ALPH
	   else        ! (not) ALPH
 	       IF (COL1(PU+1).NE.COL1(PL).OR.CS(PU+1).NE.OLDCS) THEN
	          PL = PU+1
	          OLDCS = CS(PL)
	       ENDIF
           endif
	   PU = PU+1
	   CS(PU) = PL
	 ENDDO
      ENDDO
C
C  IF SORT NEEDS TO BE DESCENDING, FLIP THE TABLE NOW
C
      IF (DSCND) THEN
	 TOP = 1
	 BOT = CLEN
	 DO WHILE (TOP.LT.BOT)
	    TEMP = CS(BOT)
	    CS(BOT) = CS(TOP)
	    CS(TOP) = TEMP
	    TEMP = CX(BOT)
	    CX(BOT) = CX(TOP)
	    CX(TOP) = TEMP
	    TOP=TOP+1
	    BOT=BOT-1
	 ENDDO
      ENDIF
C
      
      RETURN
      END
     

	SUBROUTINE CORMOVRECORD
C
C  PERFORMS AN IN PLACE MOVE OF IBIS-RECORDS ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	INCLUDE 'sort.fin'
	REAL*8 RAWBUF(4000),TEMPBUF(4000)
        INTEGER*4 RECORD,STATUS,IX,I,IQ,K
	
C
        call ibis_record_open(IBIS,RECORD,' ',
     +                        RCOLS,NRCOLS,'NONE',STATUS)
	if (STATUS.ne.1) call ibis_signal(IBIS,STATUS,1)
	call ibis_record_set(RECORD,'NR',1,STATUS)   ! Don't buffer record
	if (STATUS.ne.1) call ibis_signal(IBIS,STATUS,1)

	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  call ibis_record_read(RECORD,TEMPBUF,IX,STATUS) !TEMP = COL(IX)
	  if (status.ne.1) call ibis_signal(ibis,status,1)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    call ibis_record_read(RECORD,RAWBUF,K,STATUS) 
	    if (status.ne.1) call ibis_signal(ibis,status,1)
	    call ibis_record_write(RECORD,RAWBUF,I,STATUS) ! COL(I) = COL(K)
	    if (status.ne.1) call ibis_signal(ibis,status,1)	   
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  call ibis_record_write(RECORD,TEMPBUF,IQ,STATUS) !COL(IQ) = TEMP
	  if (status.ne.1) call ibis_signal(ibis,status,1)
	ENDDO
        call ibis_record_close(record,status)
	if (status.ne.1) call ibis_signal(ibis,status,1)
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END



	SUBROUTINE CORMOVCOLUMNS
C
C  PERFORMS AN IN PLACE MOVE OF IBIS-COLUMNS (OF WIDTH <= 8)
C  ACCORDING TO POINTERS CX. FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	INCLUDE 'sort.fin'
	CHARACTER*8 FMT
        INTEGER*4 I,IX,STATUS
	LOGICAL ALPHCOL

        DO I=1,NCCOLS
             IX = CCOLS(I)  
C --         READ AND SHUFFLE	

	     CALL IBIS_COLUMN_GET(IBIS,'FORMAT',FMT,IX,STATUS)
	     ALPHCOL = (FMT(1:1).EQ.'A'.OR.FMT(1:1).EQ.'a')
	     
	     if (ALPHCOL) then
	       if (IBIS1) then
 	          CALL IBIS_COLUMN_READ(IBIS,COL1ALPH,IX,1,CLEN,STATUS) 
	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	          call CORMOVALPH (COL1ALPH,CX,CLEN)
 	       else         ! IBIS2
  	          CALL IBIS_COLUMN_READ(IBIS,COL1I2A,IX,1,CLEN,STATUS) 
 	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
 	          call CORMOVI2A (COL1ALPH,CX,CLEN)
	       endif        ! IBIS1, IBIS2 & ALPH
             else            ! (not) ALPH
	          CALL IBIS_COLUMN_READ(IBIS,COL1,IX,1,CLEN,STATUS)
	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	          CALL CORMOV (COL1,CX,CLEN)
             endif

C --        WRITE IT OUT

	    if (ALPHCOL) then
	      if (IBIS1) then
                 CALL IBIS_COLUMN_WRITE(IBIS,COL1ALPH,IX,1,CLEN,STATUS)
	      else    ! IBIS2
                 CALL IBIS_COLUMN_WRITE(IBIS,COL1I2A ,IX,1,CLEN,STATUS)
              endif   ! IBIS1, IBIS2 & ALPH
	    else       ! (not) ALPHCOL
	        CALL IBIS_COLUMN_WRITE(IBIS,COL1,IX,1,CLEN,STATUS)
 	    endif
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  
        ENDDO

	RETURN
	END


	SUBROUTINE CORMOV (COL,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COL ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
	DIMENSION COL(1),CX(1)
C
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMP = COL(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COL(I) = COL(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COL(IQ) = TEMP
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END
C
	SUBROUTINE CORMOVALPH (COLALPH,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COLALPH ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
        CHARACTER*4  COLALPH
        CHARACTER*5  TEMPA
	DIMENSION COLALPH(1),CX(1)
C
	TEMPA='     '   ! 5 SPACES
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMPA= COLALPH(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COLALPH(I)= COLALPH(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COLALPH(IQ)= TEMPA
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END
C
	SUBROUTINE CORMOVI2A (COLIALPH,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COLALPH ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
        CHARACTER*8  COLIALPH
        CHARACTER*9  TEMPIA
	DIMENSION COLIALPH(1),CX(1)
C
	TEMPIA='         '   ! 9 SPACES
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMPIA= COLIALPH(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COLIALPH(I)= COLIALPH(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COLIALPH(IQ)= TEMPIA
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sort.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
C  Common include file for SORT
      IMPLICIT NONE
      LOGICAL	   ALPH,DSCND,IBIS1,IBIS2,ROWORG
      INTEGER*4   SORTCL(100),CCOLS(1024),RCOLS(1024)
      INTEGER*4   IBIS,NCOL,NRCOLS,NCCOLS,SORTCNT
      INTEGER*4   BUFFSIZE,BUFSIZ2,CLEN,INDEX
      CHARACTER*2  CF
C
      PARAMETER (BUFFSIZE=1000000,BUFSIZ2=BUFFSIZE/2)
      INTEGER*4 COL1TEMP(BUFFSIZE),CXTEMP(BUFFSIZE),IP(BUFFSIZE)
      INTEGER*4 COL1(BUFFSIZE),CS(BUFFSIZE),CX(BUFFSIZE)
      CHARACTER*4  COL1ALPH(BUFFSIZE),COL1ATEMP(BUFFSIZE)
      CHARACTER*8  COL1I2A(BUFSIZ2),COL1I2AT(BUFSIZ2)
      COMMON /COMA/COL1TEMP,CXTEMP,IP
      COMMON /COM/COL1,CS,CX
      COMMON /SORTCOM/ALPH,DSCND,IBIS,IBIS1,IBIS2,ROWORG,CLEN,NCOL,
     +        NRCOLS,NCCOLS,CCOLS,RCOLS,SORTCNT,INDEX,SORTCL,CF
C
C   Equivalence is used to conserve memory since sort will be invoked
C   separately for each data type
C
      EQUIVALENCE (COL1(1),COL1ALPH(1),COL1I2A(1))     
      EQUIVALENCE (COL1TEMP(1),COL1ATEMP(1),COL1I2AT)
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sort.imake
#define PROGRAM sort
#define MODULE_LIST sort.f
#define INCLUDE_LIST sort.fin
#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$ Return
$!#############################################################################
$PDF_File:
$ create sort.pdf
PROCESS		HELP=*
! VERSION 1
! SORT PDF - VICAR/IBIS MOSAIC SOFTWARE
PARM INP TYPE=STRING
PARM SORTCOL TYPE=INTEGER COUNT=(1:20)
PARM INDEXCOL TYPE=INTEGER COUNT=1,DEFAULT=0
PARM ALPHA TYPE=KEYWORD VALID=(ALPHA,NOALPHA) DEFAULT=NOALPHA
PARM DESCEND TYPE=KEYWORD VALID=(DESCEND,ASCEND) DEFAULT=ASCEND
END-PROC
.TITLE
VICAR/IBIS Program sort
.HELP
PURPOSE

	SORT performs a multiple column sort on an IBIS interface file. The
program starts with the column defined as primary and performs an incore sort.
Subsequent columns are sorted only within limits established by equal values
in previously sorted columns. The program incorporates two temporary fields as
sorting and indexing columns. The index column can be retained for later use
if specified.

TAE COMMAND LINE FORMAT

	sort INP=A SORTCOL=(B,C,D,E,...F) INDEXCOL=G
	sort INP=A SORTCOL=(B,C,D,E,...F) DESCEND INDEXCOL=G ALPHA

	INP is a random access file which is read from or written to depending
on the parameters. SORTCOL specifies the columns to be sorted in the order in
which the sort is to occur. DESCEND is a keyword whose presence indicates that
all the columns are to be sorted in descending order. INDEXCOL is an integer
which designates the column to recieve the index numbers assigned to each
unique combination of letters or numbers in the sorted columns created by the
sorting process. ALPHA is a keyword whose presence indicates that the sorted
columns are alphabetic.

EXAMPLE

	sort INP=A SORTCOL=(1,2) INDEXCOL=3 ALPHA

	In this example an 8 character name is stored in the first 2 columns.
The file is sorted into alphabetic order. The index stored in column 3 can be
used in place of the names for other operations such as aggregation.

	sort INP=A SORTCOL=(1,2) INDEXCOL=3
	sort INP=A SORTCOL=(3,4)

	In this example, a file is sorted by 3 columns, but because 2 of the
columns are alphabetic, they must be sorted seperately and the position saved
for the second pass via the INDEXCOL parameter.

OPERATION

	SORT uses two temporary arrays. The first holds the SORT data and the
second defines the boundary limits. Limits are defined using an index number
which indicates the portion of the file to be sorted. The boundary index is
initialized to 1 which indicates that the entire column is to be sorted. As
subsequent sorts are made on the file the index number is broken into groupings
of the same number which indicate the sort limits. The holding array is sorted
each time the index changes to achieve a multiple column sort.

	At the completion of the sort process, the reordering is in the form
of pointers. The pointers are used to move all the columns of the file one at
a time. The limit array serves as an index.

	WRITTEN BY		A. L. Zobrist		15 Dec 1976
	COGNIZANT PROGRAMMER	K. F. Evans
	DOCUMENTED BY		R. Wayne Bannister
	REVISION		1			22 Sep 1978
	Ported to UNIX		C. Randy Schenk (CRI)    6 Mar 1995
.LEVEL1
.VARIABLE INP
File to be sorted.
.VARIABLE SORTCOL
Columns to be sorted in order.
.VARIABLE INDEXCOL
Column to get index numbers.
.VARIABLE DESCEND
Descending order keyword.
.VARIABLE ALPHA
Alphabetic keyword.
.LEVEL2
.VARIABLE INP
INP is a random access file
which is read from and written
to depending on the parameters
.VARIABLE SORTCOL
SORTCOL specifies 1 or more
columns to be sorted in the
order in which it is to occur.
.VARIABLE INDEXCOL
INDEXCOL specifies the column
to recieve the index numbers
assigned to each unique com-
bination of letters or numbers
in the sorted columns created
by the sorting process.
.VARIABLE DESCEND
DESCEND is a keyword whose
presence indicates that all
the columns are to be
sorted in descending order.
.VARIABLE ALPHA
ALPHA is a keyword indicating
that the sorted columns are
alphabetic.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstsort.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"

!In this example an 8 character name is stored in the first 2 columns.
!The file is sorted into alphabetic order. The index stored in column 3 can be
!used in place of the names for other operations such as aggregation.

  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-1 +
     index=4 strcol=(1,2) +
     string=(zzzz,zzzz,+
	     zzzz,yyyy, +
	     yyyy,yyyy, +
	     xxxx,xxxx, +
	     aaaa,bbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2) 'alpha indexcol=3
   ibis-list a a4col=(1,2)

  ibis-gen a nc=3 nr=5 datacol=(1,2) 'ibis-1 +
    data=(1,3, +
 	  5,3,+
	  1,4,+
	  5,4,+
	  2,5)
  ibis-list a
  sort a sortcol=(1,2) indexcol=3
  ibis-list a
  sort a sortcol=(1,2) indexcol=3 'descend
  ibis-list a
!
  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-2 +
     index=4 strcol=(1,2) +
     string=(zzzz,zzzz,+
	     zzzz,yyyy, +
	     yyyy,yyyy, +
	     xxxx,xxxx, +
	     aaaa,bbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2) 'alpha indexcol=3
   ibis-list a a4col=(1,2)

  ibis-gen a nc=4 nr=5 format=(a12,a12,real,real) 'ibis-2 +
     index=4 strcol=(1,2) +
     string=(zzzzzzzzzz,zzzzzzzzzz,+
	     zzzzzzzzzz,yyyyyyyyyy, +
	     yyyyyyyyyy,yyyyyyyyyy, +
	     xxxxxxxxxx,xxxxxxxxxx, +
	     aaaaaaaaaa,bbbbbbbbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2) 'alpha indexcol=3
   ibis-list a a4col=(1,2)


end-proc
$ Return
$!#############################################################################
