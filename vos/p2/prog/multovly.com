$!****************************************************************************
$!
$! Build proc for MIPL module multovly
$! VPACK Version 1.9, Thursday, September 11, 2014, 11:44:25
$!
$! Execute by entering:		$ @multovly
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
$ write sys$output "*** module multovly ***"
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
$ write sys$output "Invalid argument given to multovly.com file -- ", primary
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
$   if F$SEARCH("multovly.imake") .nes. ""
$   then
$      vimake multovly
$      purge multovly.bld
$   else
$      if F$SEARCH("multovly.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake multovly
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @multovly.bld "STD"
$   else
$      @multovly.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create multovly.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack multovly.com -mixed -
	-s multovly.f -
	-i multovly.imake -
	-p multovly.pdf -
	-t tstmultovly.pdf tstmultovly.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create multovly.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

C---- VICAR/IBIS PROGRAM "MULTOVLY"
C
C --- 8-May-95 ...CRS ... MSTP S/W CONVERSION (VICAR PORTING)
C
C	READ THE PDF FOR INFORMATION ON THE PURPOSE OF THE PROGRAM
C	AND A DESCRIPTION OF THE INPUT PARAMETERS.
C
C	MULTOVLY MAKES A HISTOGRAM OF THE PIXEL COMBINATIONS IN THE
C	IMAGES USING A HASH TABLE.  IF THE NUMBER OF DIFFERENT
C	PIXEL COMBINATIONS (THE LENGTH OF THE HISTOGRAM) IS GREATER
C	THAN THE INTERNAL MEMORY AVAILABLE TO THE PROGRAM THE VAX
C	WOULD PUT PART OF IT ON DISK IN VIRTUAL MEMORY.  BECAUSE
C	THE HASH TABLE IS ACCESSED FAIRLY RANDOMLY THE NUMBER OF
C	PAGE FAULTS BECOMES IMMENSE AND THE HASHING GRINDS VIRTUALLY
C	TO A HALT.  THUS THIS PROGRAM (WHICH IS AN ENTIRELY REWRITTEN
C	VERSION OF AN OLDER PROGRAM) DOES THE HASHING IN STAGES SO THE
C	HASH TABLE ALWAYS FITS IN INTERNAL MEMORY.  WHEN THE INTERNAL
C	HASH TABLE (REFERRED TO BY 'CORE' IN THIS PROGRAM) IS FULL
C	(WHICH IS WHEN IT IS ACTUALLY 60 PERCENT FULL)  IT IS
C	MERGED WITH THE REST OF THE HISTOGRAM WHICH IS IN VIRTUAL
C	MEMORY.  BECAUSE WE DON'T WANT TO HAVE A SEPARATE ARRAY TO MERGE
C	TO, THE TABLES ARE MERGED IN THE VIRTUAL ARRAY ITSELF.  THIS
C	IS DONE BY HAVING THIS ARRAY BE CIRCULAR SO THAT WE NEVER RUN
C	OUT OF IT.  SINCE BOTH THE HISTOGRAM AND THE KEYS (WHICH ARE
C	THE PIXEL COMBINATIONS) ARE IN THE HASH TABLE AND THE NUMBER
C	OF BYTES IN THE KEY VARIES THE LENGTH OF THE CORE HASH TABLE
C	VARIES.  THE PRIME NUMBERS FOR THE HASHING ARE SELECTED BY
C	PICKING THE 20 PRIMES UNDER 1000 THAT HAVE THE MAXIMUM FRACTIONAL
C	MODULUS WHEN DIVIDED INTO ALL OF THE TABLE LENGTHS.
C
C     PROGRAMMER: FRANK EVANS , APRIL 1985
C
C	REVISION A:  Increased output size.    
C				KFE  July 1985
C	REVISION B:  Allowed use of less bytes than actually in pixel. 
C				KFE August 1985
C	REVISION C:  ADDED TALLY OPTION
C				KFE JANUARY 1986
C       REVISION D:  Ported for UNIX
C                       Randy Schenk (CRI)  08-May-1995        
C
	IMPLICIT INTEGER*4 (A-Z)

C--- Note: if the following parameter is modified, you must also
C---       modify it in the subroutine SORTMERGE, below.
c	   In addition, the help file refers to the max number of unique
c	   overlay combinations; this should be given as
c	       "<PAGE_PARAM> or <VIRTBYTES> /KEYLEN" in the help file. 
	PARAMETER (PAGE_PARAM = 1000000)  !number of IBIS ROWS-ADDED JAN 90  NDR

	PARAMETER (CORMAX = 20000,VIRTMAX = PAGE_PARAM + CORMAX)
	PARAMETER (CORBYTES = 20*(CORMAX+10),VIRTBYTES = 6*VIRTMAX+200)
	INTEGER	PNTR(CORMAX+10),CORHIST(CORMAX+10),VIRTHIST(VIRTMAX+10)
	REAL	CORSUM(CORMAX+10), VIRTSUM(VIRTMAX+10), RBUFFER(10000)
	BYTE	CORKEYS(CORBYTES), VIRTKEYS(VIRTBYTES)
	BYTE	KEY(20), LASTKEY(20), IMAGE(40000,10)
	BYTE	TMPBYTE,TMPBYTES(4)
	LOGICAL 	NOZEROS(10), EMPTYHIST, ZERO, TALLY,XVPTST
	CHARACTER*72 	INPFILES(10), STRING 
	CHARACTER*8	FORMAT(10)
	INTEGER 	PRIMES(20)
	REAL 		ROWDATA(40)     !deleted: buffer(128,40)
	INTEGER 	DATACOL(40), BYTES(10), ZEROINP(10), OUTCOL(40)
	INTEGER 	UNIT(10), FORMBYTES(10)

	COMMON /MLTOVK/ KEYLEN,VIRTSIZ
	COMMON /MLTOV/  CORSIZ, TALLY,
     +			VIRTBEG,VIRTEND,VIRTENTRIES,ENTRIES, CORSUM,
     +			CORHIST,CORKEYS,VIRTHIST,VIRTKEYS,PNTR,VIRTSUM

	DATA PRIMES/983,929,811,797,773,683,659,607,599,521,
     +			   467,463,449,401,397,373,353,281,229,139/

        CALL IFMESSAGE('MULTOVLY version Sep 11 2014')


	DO I = 1,CORMAX+10
            PNTR(I) = 0
	    CORHIST(I) = 0
	    CORSUM(I) = 0.0
	ENDDO
	DO I = 1,VIRTMAX+10
	   VIRTHIST(I) = 0
	   VIRTSUM(I) = 0.0
	ENDDO
	DO I = 1,10000
	   RBUFFER(I) = 0.0
	ENDDO
	DO I=1,CORBYTES
	   CORKEYS(I) = 0
	ENDDO
	DO I=1,VIRTBYTES
	   VIRTKEYS(I) = 0
	ENDDO
	DO I=1,20
	   KEY(I) = 0
	   LASTKEY(I) = 0
	   PRIMES(I) = 0
	ENDDO
	DO I=1,40
	   ROWDATA(I) = 0.0
	   DATACOL(I) = 0
	   OUTCOL(I) = 0
	ENDDO
	DO I=1,10
	   DO J=1,40000
	      IMAGE(J,I) = 0
	   ENDDO
	   NOZEROS(I) = .FALSE.
	   BYTES(I) = 0
	   ZEROINP(I) = 0
	   UNIT(I) = 0
	   FORMBYTES(I) = 0
	ENDDO
	TMPBYTE = .FALSE.
	DO I=1,4
	   TMPBYTES(I) = 0
	ENDDO
	EMPTYHIST = .FALSE.
	ZERO = .FALSE.
	TALLY = .FALSE.

C---------		GET THE INPUT PARAMETERS FROM TAE
        STATUS = XVEACTION('SA',' ')
	CALL XVP('INP',INPFILES,NUMIMAGES)
	CALL XVPARM('NCOL',NCOL,NCOLCNT,NCOLDEF,1)
	CALL XVPARM('DATACOL',DATACOL,DATACOLCNT,DCOLDEF,10)
	CALL XVPARM('AREACOL',AREACOL,AREACOLCNT,AREACOLDEF,1)
	CALL XVPARM('SUMCOL',SUMCOL,SUMCOLCNT,SUMCOLDEF,1)
	CALL XVPARM('BYTES',BYTES,BYTESCNT,BYTESDEF,10)
	CALL XVPARM('ZEROES',ZEROINP,ZEROCNT,ZERODEF,10)
	TALLY = XVPTST ('TALLY')
	IF (TALLY) THEN
	    NUMOVLY = NUMIMAGES - 1
	ELSE
	    NUMOVLY = NUMIMAGES
	ENDIF
	IF (NUMOVLY .LE. 0) THEN
	    CALL XVMESSAGE ('MUST BE AT LEAST ONE OVERLAY IMAGE',' ')
	    CALL ABEND
	ENDIF


C--------			OPEN THE INPUT FILES
	DO I = 1,NUMOVLY
	    CALL XVUNIT(UNIT(I),'INP',I,STATUS,' ')
	    CALL XVOPEN(UNIT(I),STATUS,  'OP','READ',
     +		 	 'OPEN_ACT','AS', 'IO_ACT','AS',' ')
	    CALL XVGET(UNIT(I),STATUS,'FORMAT',FORMAT(I),' ')
	ENDDO
	IF (TALLY) THEN
	    CALL XVUNIT(UNIT(NUMIMAGES),'INP',NUMIMAGES,STATUS,' ')
	    CALL XVOPEN(UNIT(NUMIMAGES),STATUS,  'OP','READ', 
     +			 'U_FORMAT','REAL',
     +		 	 'OPEN_ACT','AS', 'IO_ACT','AS',' ')
	ENDIF

C--------			SET UP THE DEFAULTS AND CHECK INPUTS
	CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
	LL = SL+NL-1
	LS = SS+NS-1
	DO I = 1,NUMIMAGES
	    CALL XVGET(UNIT(I),STATUS, 'NL',NL0,' ')
	    CALL XVGET(UNIT(I),STATUS, 'NS',NS0,' ')
	    IF (LL .GT. NL0 .OR. LS .GT. NS0) THEN
		CALL XVMESSAGE('WINDOW IN IMAGES TOO LARGE',' ')
		CALL ABEND
	    ENDIF
	ENDDO

	IF (NCOLDEF .EQ. 1)  NCOL = NUMIMAGES+1
	IF (NCOL .LE. NUMIMAGES) THEN
	    CALL XVMESSAGE('NCOL TOO SMALL',' ')
	    CALL ABEND
	ENDIF
	IF (AREACOLDEF .EQ. 1)  AREACOL = NUMIMAGES+1
	DATACOL(NUMIMAGES+1) = AREACOL
	IF (TALLY) THEN
	    IF (SUMCOLDEF .EQ. 1)  SUMCOL = NUMIMAGES
	    DATACOL(NUMIMAGES) = SUMCOL
	ENDIF
	DO I = 1,NUMIMAGES+1
	    IF (DATACOL(I) .LE. 0 .OR. DATACOL(I) .GT. NCOL) THEN
		CALL XVMESSAGE ('COLUMN OUT OF RANGE',' ')
		CALL ABEND
	    ENDIF
	ENDDO
	DO I = 1,NUMIMAGES
	    DO J = I+1,NUMIMAGES+1
		IF (DATACOL(I) .EQ. DATACOL(J)) THEN
		    CALL XVMESSAGE('COLUMN SPECIFIED MORE THAN ONCE',' ')
		    CALL ABEND
		ENDIF
	    ENDDO
	ENDDO
	DO I = NUMIMAGES+2, NCOL
	    DATACOL(I) = I
	ENDDO

	DO I = 1,NUMOVLY
	    IF (FORMAT(I)(1:4) .EQ. 'BYTE') THEN
		FORMBYTES(I) = 1
	    ELSE IF (FORMAT(I)(1:4) .EQ. 'WORD') THEN
		FORMBYTES(I) = 2
	    ELSE IF (FORMAT(I)(1:4) .EQ. 'HALF') THEN
		FORMBYTES(I) = 2
	    ELSE IF (FORMAT(I)(1:4) .EQ. 'FULL') THEN
		FORMBYTES(I) = 4
	    ELSE
		CALL XVMESSAGE('LABEL MUST BE BYTE, HALF, OR FULL',' ')
		CALL EXIT(0)
	    ENDIF
	ENDDO
	IF (BYTESDEF .EQ. 1) THEN
	    DO I = 1,NUMOVLY
		BYTES(I) = FORMBYTES(I)
	    ENDDO
	ELSE
	    DO I = 1,NUMOVLY
		IF (BYTES(I) .LE. 0)  BYTES(I) = FORMBYTES(I)
		BYTES(I) = MIN(BYTES(I),FORMBYTES(I))
	    ENDDO
	ENDIF

	KEYLEN = 0
	DO I = 1,NUMOVLY
	    KEYLEN = KEYLEN + BYTES(I)
	ENDDO
	IF (KEYLEN .GT. 20) THEN
	    CALL XVMESSAGE ('MORE THAN 20 BYTES IN IMAGE PIXELS',' ')
	    CALL EXIT(0)
	ENDIF

	DO I = 1,NUMOVLY
	    NOZEROS(I) = .TRUE.
	ENDDO
	DO I = 1,ZEROCNT
	    IF (ZEROINP(I).GT.0 .AND. ZEROINP(I).LE.NUMOVLY)
     +			NOZEROS(ZEROINP(I)) = .FALSE.
	ENDDO


	CORSIZ = MIN( PAGE_PARAM/(KEYLEN+4) ,CORMAX)
	VIRTSIZ = MIN( VIRTBYTES/KEYLEN, VIRTMAX)

	VIRTBEG = VIRTSIZ
	VIRTEND = VIRTSIZ
	EMPTYHIST = .TRUE.


	BIN = 0

C---------------		GO THROUGH IMAGES LINE BY LINE
	DO LINE = SL,LL
	    DO I = 1,NUMOVLY
		CALL XVREAD(UNIT(I),IMAGE(1,I),STATUS, 'LINE',LINE,' ')
	    ENDDO
	    IF (TALLY) THEN
		CALL XVREAD (UNIT(NUMIMAGES),RBUFFER,STATUS,
     +                               'LINE',LINE,' ')
	    ENDIF

	    DO SAMP = SS,LS

C-----				PUT IMAGE PIXELS INTO KEY
		K = 1
		DO I = 1,NUMOVLY
		    ZERO = .TRUE.
		    DO J = BYTES(I),1,-1
			TMPBYTE = IMAGE(FORMBYTES(I)*(SAMP-SS)+J ,I)
			KEY(K) = TMPBYTE
			K = K+1
			IF (TMPBYTE .NE. 0)  ZERO = .FALSE.
		    ENDDO
		    IF (ZERO .AND. NOZEROS(I))  GO TO 900
		ENDDO



C--				COMPARE THIS KEY WITH LAST ONE
		IF (.NOT. EMPTYHIST) THEN
		    IF (COMPKEY(KEY,LASTKEY).EQ.0) THEN
			CORHIST(BIN) = CORHIST(BIN)+1
			IF (TALLY) CORSUM(BIN) = CORSUM(BIN)
     +                                               +RBUFFER(SAMP)
			GO TO 900
		    ENDIF
		ENDIF

C---				COMPUTE HASH ADDRESS
		HASH = 22000000
		DO K = 1,KEYLEN
		     HASH = HASH+KEY(K)*PRIMES(K)
		ENDDO
		BIN = MOD(HASH,CORSIZ)+1

C--				CHECK FOR COLLISION
		DO WHILE (CORHIST(BIN).NE.0)
		    IF (COMPKEY(KEY,CORKEYS(KEYLEN*BIN)).EQ.0) THEN
			CORHIST(BIN) = CORHIST(BIN)+1
			IF (TALLY) CORSUM(BIN) = CORSUM(BIN)
     +                                                 + RBUFFER(SAMP)
			CALL MOVEKEY(KEY,LASTKEY)
			GO TO 900
		    ELSE
			BIN = BIN+1
			IF (BIN.GT.CORSIZ) BIN = 1
		    ENDIF
		ENDDO
C--				ADD ENTRY TO HASH TABLE
		ENTRIES = ENTRIES+1
		CALL MOVEKEY(KEY,CORKEYS(KEYLEN*BIN))
		CORHIST(BIN) = 1
		IF (TALLY) CORSUM(BIN) = RBUFFER(SAMP)
		CALL MOVEKEY(KEY,LASTKEY)
		EMPTYHIST = .FALSE.

900		CONTINUE


C-------			IF HASH TABLE FULL THEN SORT AND MERGE
	    IF (ENTRIES .GT. 0.6*CORSIZ) THEN
		CALL SORTMERGE
		IF (VIRTENTRIES .GT. (VIRTSIZ-CORSIZ)) THEN
		    WRITE (STRING,'(I6,A,I5)')  VIRTSIZ-CORSIZ,
     +				 ' ENTRIES EXCEEDED AT LINE ', LINE
		    CALL XVMESSAGE(STRING,' ')
		    CALL XVMESSAGE('STOPPING HERE',' ')
		    GO TO 1000
		ENDIF
	    ENDIF


	    ENDDO
	ENDDO

C---				SORT AND MERGE WHATS LEFT WHEN DONE
	IF (ENTRIES .GT. 0)  CALL SORTMERGE

C--------			CLOSE THE IMAGE FILES
1000	CONTINUE
	DO I = 1,NUMIMAGES
	    CALL XVCLOSE(UNIT(I),STATUS,' ')
	ENDDO

C-----				PUT THE STUFF IN AN IBIS FILE

	WRITE (STRING,'(I7,A)') VIRTENTRIES,
     +                                 ' ENTRIES TO THE OUTPUT FILE'
	CALL XVMESSAGE(STRING,' ')
C -----                   OPEN THE OUTPUT FILE
        CALL XVUNIT(OUNIT,'OUT',1,STATUS,' ')
        NROWS = VIRTENTRIES 
        DO I=1,NCOL
           OUTCOL(I) = I
        ENDDO
        CALL IBIS_FILE_OPEN(OUNIT,IBIS,'WRITE',NCOL,NROWS,' ',
     +                  'ROW',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
        CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',OUTCOL,NCOL,
     +                                          'REAL',STATUS)     
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	VPNT = WRAP(VIRTBEG-1)
	DO WHILE (VPNT.NE.VIRTEND)
	    VPNT = WRAP(VPNT+1)
	    K = 0
	    DO I = 1,NUMOVLY
		TMPWORD = 0
		DO J = BYTES(I),1,-1
		    TMPBYTES(J) = VIRTKEYS(KEYLEN*VPNT+K)
		    K = K+1
		ENDDO
		DO II=1,4
		    IF (TMPBYTES(II).LT.0) THEN
			TMPWORD =  TMPWORD +
     +                          (256-TMPBYTES(II))*(256**(II-1))
		    ELSE
			TMPWORD =  TMPWORD +
     +                          TMPBYTES(II)*(256**(II-1))
		    ENDIF                     
		ENDDO
		ROWDATA(I) = FLOAT(TMPWORD) 
	    ENDDO
	    IF (TALLY)  ROWDATA(NUMIMAGES) = VIRTSUM(VPNT)
	    ROWDATA(NUMIMAGES+1) = FLOAT(VIRTHIST(VPNT))
	    ROW = ROW+1
            CALL IBIS_RECORD_WRITE(RECORD,ROWDATA,ROW,STATUS)
	ENDDO
        CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
        CALL IBIS_FILE_CLOSE(IBIS,'UDELETE',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)


	RETURN

	END



C------------------------------	SORT AND MERGE SECTION

	SUBROUTINE SORTMERGE
	IMPLICIT INTEGER*4 (A-Z)

	PARAMETER (PAGE_PARAM = 1000000)  !number of IBIS ROWS-ADDED JAN 90  NDR
	PARAMETER (CORMAX = 20000,VIRTMAX = PAGE_PARAM+CORMAX)
	PARAMETER (CORBYTES = 20*(CORMAX+10),VIRTBYTES = 6*VIRTMAX+200)
	INTEGER	PNTR(CORMAX+10),CORHIST(CORMAX+10),VIRTHIST(VIRTMAX+10)
	REAL	CORSUM(CORMAX+10), VIRTSUM(VIRTMAX+10)
	BYTE CORKEYS(CORBYTES),VIRTKEYS(VIRTBYTES)
	LOGICAL  TALLY
	COMMON /MLTOVK/ KEYLEN,VIRTSIZ
	COMMON /MLTOV/  CORSIZ, TALLY,
     +			VIRTBEG,VIRTEND,VIRTENTRIES,ENTRIES, CORSUM,
     +			CORHIST,CORKEYS,VIRTHIST,VIRTKEYS,PNTR,VIRTSUM
C------				SET UP POINTERS FOR THE SORT
	J = 0
	DO I = 1,CORSIZ
	    IF (CORHIST(I).NE.0) THEN
		J = J+1
		PNTR(J) = I
	    ENDIF
	ENDDO
	SORTLEN = J

C----				AND SORT THE CORE HASH TABLE
	CALL BNSORT(CORKEYS,PNTR,SORTLEN,KEYLEN)



C---------			JUST STUFF HISTOGRAM IN VIRTUAL IF EMPTY
	IF (VIRTBEG.EQ.VIRTEND) THEN
	    VIRTBEG = VIRTSIZ-SORTLEN+1
	    VIRTEND = VIRTSIZ
	    DO I = 1,SORTLEN
		VPNT = VIRTBEG+I-1
		VIRTHIST(VPNT) = CORHIST(PNTR(I))
		IF (TALLY) VIRTSUM(VPNT) = CORSUM(PNTR(I))
		CALL MOVEKEY( CORKEYS(KEYLEN*PNTR(I)),
     +                                      VIRTKEYS(KEYLEN*VPNT) )
	    ENDDO
	    GO TO 2100
	ENDIF
C-------			OTHERWISE MERGE THE TWO HISTOGRAMS
	CPNT = 1
	VPNT = VIRTBEG
	OPNT = WRAP(VIRTBEG-SORTLEN)
	DO WHILE (.TRUE.)
	    COND = COMPKEY( CORKEYS(KEYLEN*PNTR(CPNT)),
     +			 VIRTKEYS(KEYLEN*VPNT) )
	    IF (COND.EQ.0) THEN
		CALL MOVEKEY(VIRTKEYS(KEYLEN*VPNT),
     +                                 VIRTKEYS(KEYLEN*OPNT))
		VIRTHIST(OPNT) = CORHIST(PNTR(CPNT))+VIRTHIST(VPNT)
		IF (TALLY) VIRTSUM(OPNT) = CORSUM(PNTR(CPNT))
     +                                                  +VIRTSUM(VPNT)
		CPNT = CPNT+1
		VPNT = WRAP(VPNT+1)
		OPNT = WRAP(OPNT+1)
		IF (CPNT.GT.SORTLEN) GO TO 1800
		IF (VPNT.EQ.WRAP(VIRTEND+1)) GO TO 1900
	    ELSE IF (COND.EQ.-1) THEN
		VIRTHIST(OPNT) = CORHIST(PNTR(CPNT))
		IF (TALLY) VIRTSUM(OPNT) = CORSUM(PNTR(CPNT))
		CALL MOVEKEY( CORKEYS(KEYLEN*PNTR(CPNT)),
     +				VIRTKEYS(KEYLEN*OPNT) )
		CPNT = CPNT+1
		OPNT = WRAP(OPNT+1)
		IF (CPNT.GT.SORTLEN) GO TO 1800
	    ELSE
		VIRTHIST(OPNT) = VIRTHIST(VPNT)
		IF (TALLY) VIRTSUM(OPNT) = VIRTSUM(VPNT)
		CALL MOVEKEY(VIRTKEYS(KEYLEN*VPNT),
     +                                      VIRTKEYS(KEYLEN*OPNT))
		VPNT = WRAP(VPNT+1)
		OPNT = WRAP(OPNT+1)
		IF (VPNT.EQ.WRAP(VIRTEND+1)) GO TO 1900
	    ENDIF
	ENDDO

C----				COPY THE REST OF THE VIRTUAL HISTOGRAM
1800	    CONTINUE
	    DO WHILE (VPNT.NE.WRAP(VIRTEND+1))
		VIRTHIST(OPNT) = VIRTHIST(VPNT)
		IF (TALLY) VIRTSUM(OPNT) = VIRTSUM(VPNT)
		CALL MOVEKEY(VIRTKEYS(KEYLEN*VPNT),
     +                                      VIRTKEYS(KEYLEN*OPNT))
		VPNT = WRAP(VPNT+1)
		OPNT = WRAP(OPNT+1)
	    ENDDO
	    GO TO 2000

C----				COPY THE REST OF THE CORE HISTOGRAM
1900	    CONTINUE
	    DO WHILE (CPNT.LE.SORTLEN)
		VIRTHIST(OPNT) = CORHIST(PNTR(CPNT))
		IF (TALLY) VIRTSUM(OPNT) = CORSUM(PNTR(CPNT))
		CALL MOVEKEY(CORKEYS(KEYLEN*PNTR(CPNT))
     +				,VIRTKEYS(KEYLEN*OPNT))
		CPNT = CPNT+1
		OPNT = WRAP(OPNT+1)
	    ENDDO


2000	CONTINUE
	VIRTBEG = WRAP(VIRTBEG-SORTLEN)
	VIRTEND = WRAP(OPNT-1)

C------				ZERO THE CORE HISTOGRAM AND KEYS
2100	CONTINUE
	VIRTENTRIES = WRAP(VIRTEND-VIRTBEG+1)

	DO I = 1,CORSIZ
	    CORHIST(I) = 0
	    CORSUM(I) = 0.0
	ENDDO
	ENTRIES = 0
	EMPTYHIST = .TRUE.
	DO K = KEYLEN*1,KEYLEN*(CORSIZ+1)
	    CORKEYS(K) = 0
	ENDDO


	RETURN
	END


	SUBROUTINE MOVEKEY(KEY1,KEY2)
	IMPLICIT INTEGER (A-Z)
	BYTE KEY1(1),KEY2(1)
	COMMON /MLTOVK/ KEYLEN,VIRTSIZ

	DO K = 1,KEYLEN
	    KEY2(K) = KEY1(K)
	ENDDO

	RETURN
	END


	INTEGER FUNCTION COMPKEY(KEY1,KEY2)
C   COMPARES TWO KEYS		RETURNS -1 IF KEY1 .LT. KEY2
C				RETURNS  0 IF KEY1 .EQ. KEY2
C				RETURNS +1 IF KEY1 .GT. KEY2
	IMPLICIT INTEGER (A-Z)
	BYTE KEY1(1),KEY2(1)
	COMMON /MLTOVK/ KEYLEN,VIRTSIZ
	INCLUDE 'fortport'

	DO K = 1,KEYLEN
	    IKEY1 = IAND(BYTE2INT(KEY1(K)),255)
	    IKEY2 = IAND(BYTE2INT(KEY2(K)),255)
	    IF (IKEY1.NE.IKEY2) THEN
		IF (IKEY1.LT.IKEY2) THEN
		    COMPKEY = -1
		    GO TO 100
		ELSE
		    COMPKEY = +1
		    GO TO 100
		ENDIF
	    ENDIF
	ENDDO
	COMPKEY = 0

100	CONTINUE
	RETURN
	END

	INTEGER FUNCTION WRAP(ADDR)
C   PERFORMS THE WRAP AROUND OF THE POINTERS FOR THE CIRCULAR ARRAY
	IMPLICIT INTEGER (A-Z)
	COMMON /MLTOVK/ KEYLEN,VIRTSIZ

	IF (ADDR.GT.VIRTSIZ) ADDR = ADDR-VIRTSIZ
	IF (ADDR.LT.1) ADDR = ADDR+VIRTSIZ
	WRAP = ADDR

	RETURN
	END


C    BNSORT DOES A RADIX SORT OF A BYTE ARRAY WITH INDIVIDUAL
C	KEYS OF KEYLEN BYTES.  ONLY SWAPS THE POINTERS NOT THE KEYS.
C	MODIFIED FROM L4SORT WRITTEN BY AL ZOBRIST
C
      SUBROUTINE BNSORT(KEY,PTR,LEN,KEYLEN)
      IMPLICIT INTEGER(A-Z)
      BYTE      KEY

      DIMENSION KEY(1),PTR(1),BDRY(3,8*20),CBD(8*20),SM(8)
      DATA SM(1)/80/
C
      JMSK = 0

      DO 1 I = 2,8
 1    SM(I) = SM(I-1)/2
      LEV = 1
      BYT = 0
      BDRY(2,LEV) = 1
      BDRY(3,LEV) = LEN
      CBD(LEV) = 2
 72   CB = CBD(LEV)
      PL = BDRY(CB,LEV)
      PU = BDRY(CB+1,LEV)
      IF (PL.GE.PU) GO TO 75
      IMSK = SM(IAND(LEV-1,7)+1)
      TEMP = MOD(IMSK,256)
      IF (TEMP.GE.128) TEMP= -(256-TEMP)
      IMSL = TEMP    
 81   JMSL = KEY(KEYLEN*PTR(PL)+BYT)
      JMSL = IAND(JMSL,IMSL)
      JMSK = JMSK-MOD(JMSK,256) + JMSL
      IF (JMSK.NE.0) GO TO 85
      PL = PL+1
      IF (PL.EQ.PU) GO TO 73
      GO TO 81
 85   JMSL = KEY(KEYLEN*PTR(PU)+BYT)
      JMSL = IAND(JMSL,IMSL)
      JMSK = JMSK-MOD(JMSK,256) + JMSL
      IF (JMSK.NE.0) GO TO 86
      TEMP = PTR(PL)
      PTR(PL) = PTR(PU)
      PTR(PU) = TEMP
      GO TO 81
 86   PU = PU-1
      IF (PU.NE.PL) GO TO 85
 73   JMSL = KEY(KEYLEN*PTR(PL)+BYT)
      JMSL = IAND(JMSL,IMSL)
      JMSK = JMSK-MOD(JMSK,256) + JMSL
      IF (JMSK.NE.0) PL = PL-1
      IF (LEV.GE.8*KEYLEN) GO TO 75
      CB = CBD(LEV)
      LEV = LEV+1
      BYT = (LEV-1)/8
      CBD(LEV) = 1
      BDRY(1,LEV) = BDRY(CB,LEV-1)
      BDRY(2,LEV) = PL
      BDRY(3,LEV) = BDRY(CB+1,LEV-1)
      GO TO 72
 75   IF (CBD(LEV).EQ.2) GO TO 76
      CBD(LEV) = 2
      BDRY(2,LEV) = BDRY(2,LEV)+1
      GO TO 72
 76   LEV = LEV-1
      BYT = (LEV-1)/8
      IF (LEV.GT.0) GO TO 75
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create multovly.imake
#define  PROGRAM   multovly

#define MODULE_LIST multovly.f

#define FTNINC_LIST fortport

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create multovly.pdf
PROCESS HELP=*
PARM INP      TYPE = STRING  COUNT = 1:10 
PARM OUT      TYPE = STRING
PARM SIZE     TYPE = INTEGER COUNT = 4          DEFAULT = (1,1,0,0)
PARM SL       TYPE = INTEGER                    DEFAULT = 1
PARM SS       TYPE = INTEGER                    DEFAULT = 1
PARM NL       TYPE = INTEGER                    DEFAULT = 0
PARM NS       TYPE = INTEGER                    DEFAULT = 0
PARM NCOL     TYPE = INTEGER                    DEFAULT = 3 VALID = (1:40)
PARM DATACOL  TYPE = INTEGER COUNT = 1:10       DEFAULT = (1,2,3,4,5,6,7,8,9,10)
PARM AREACOL  TYPE = INTEGER                    DEFAULT = 3
PARM SUMCOL   TYPE = INTEGER                    DEFAULT = 2
PARM BYTES    TYPE = INTEGER COUNT = 1:10       DEFAULT = (0,0,0,0,0,0,0,0,0,0)
PARM ZEROES   TYPE = INTEGER COUNT = 1:10       DEFAULT = (0,0,0,0,0,0,0,0,0,0)
PARM TALLY    TYPE = KEYWORD COUNT = 0:1        DEFAULT=-- VALID=(--,TALLY)
END-PROC
.TITLE
VICAR/IBIS Program multovly
.HELP
PURPOSE:

  multovly performs image overlay to produce a table of DN-combination 
counts vs. DN-combinations (a histogram of DN-combinations) in sorted
order in an IBIS interface file format.  In TALLY mode one of the images
can have its pixels summed instead of histogrammed.


EXECUTION:

Examples

multovly  INP = (A,B,C)  OUT = OUT  SIZE = (50,50,150,250)  NCOL = 4 +
        DATA = (1,2,3)  AREA = 4  BYTES = (1,2,2)  ZEROES = (1,2)

  In this example, MULTOVLY will overlay three images (A, B, and C); the first
is a byte image while the other two are halfword images.  The table of DN-
combinations will be compiled only for the window listed in SIZE.  Pixel
combinations with a zero value in the first or second column will be included.
The possible pixels combinations will be listed in columns one through three,
while the number of times each combination occured will be in column four.  

  Only the input images and the output file must be specified; all of the
other parameters will default sensibly.

multovly  (A,B,C,D)  OUT

  In this example, the four images (A,B,C, and D) will be overlayed.  The
number of columns in the output file will be five with the pixel combinations
listed in columns one through four and the number of counts in column five.
The window size will be taken from the size of the first image, and the pixel
type from the format of each image.  Zero values in any image will cause that
pixel combination to be ignored.


multovly (POLYGONS.IMG,DATA.IMG) SUM.INT  'TALLY  SUMCOL=3 AREACOL=2

This example shows the use of the TALLY mode.  When 'TALLY is specified
the last input file is the summed file.  For each unique pixel combination
in the other input images, a sum is made of the pixels in the sum image.  Thus
in this case, for each polygon in the paint image POLYGONS.IMG the pixels
in DATA.IMG will be summed up.  The sum and the number of pixels for each 
unique pixel combination go into SUMCOL and AREACOL columns in the interface
file.  The default columns are, for n input images, column n for the sum column
and column n+1 for the area column.  All of the other parameters refer only
to the first n-1 images (the ones actually being overlayed).


  NCOL can be greater than the actual number of columns of data output: the
rest of the columns will be filled with zeros.
 
  There can be up to ten input images and each image can have byte, halfword,
or fullword (4 bytes) pixels.  There is a limit, however, of 20 bytes to the
total combined width of the pixels in all of the images.  For example, one
could have ten halfword images or five fullword images, but not ten fullword
images.  The BYTES parameter can be less than the actual pixel size, in which
case only the lower bytes of the pixel will be used.  For example, BYTES could
be 3 for a full word image if all of the values were less than 16,777,215
(the largest number that fits into 3 bytes).



OPERATION:

  MULTOVLY uses a hash table method of storing the DN-combinations and the
histogram of counts.  The hash method is only practical if the table is in
internal memory.  Thus if the table is too long for the core memory the
hashing is done in stages, and the parts are merged together and stored in
virtual memory on disk.



RESTRICTIONS:
  The maximum number of entries in the output interface file (i.e. the maximum
number of unique combinations) allowed is 1,000,000 or 3,000,000/KEYLEN, which
ever is smaller. (KEYLEN is the total combined width, in bytes, of the pixels).
MULTOVLY will often allow more than this number of entries, but this is the
maximum number that can always be counted on.
  The length of the lines in all of the input images must be less than or
equal to 40000 bytes (i.e. 40000 samples in a byte image, 20000 samples in
a halfword image, and 10000 samples in a fullword image.)

WRITTEN BY: 		F. Evans  April 1985
COGNIZANT PROGRAMMER:  	F. Evans
DOCUMENATION AUTHOR:  	F. Evans
REVISION:  C  		F. Evans  January 1986

PROGRAM HISTORY:
Sep 11 2014 wlb Initialized most variables in subroutine main

.LEVEL1
.VARIABLE INP
Input images
.VARIABLE OUT
IBIS output interface file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE NCOL
# of cols in output file
.VARIABLE DATACOL
Output file column order
.VARIABLE AREACOL
Output file totals column
.VARIABLE SUMCOL
Output file sum column
.VARIABLE BYTES
Format of input data
.VARIABLE ZEROES
Zero-value sample control
.VARIABLE TALLY
'TALLY for tally option

.LEVEL2
.VARIABLE INP
INP specifies the input image files.  Up to ten files are allowed.
.VARIABLE NCOL
NCOL is the number of columns to be created in the output file.
Extra columns, which will be zeros, may be created.
.VARIABLE DATACOL
DATACOL is a list of numbers specifying the output file columns which
receive the DN-values from the files overlayed.  DATACOL = (1,2), for
example, will list the DN-values from image file 1 in column 1, and
the values from image file 2 in column 2.  
The default is DATACOL=(1,2,3, etc).
.VARIABLE AREACOL
AREACOL specifies the column in the IBIS-interaface format output file which
gets the DN-combination totals.  The default column is the n+1'th column
if there are n input images.
.VARIABLE SUMCOL
SUMCOL specifies the column in the IBIS-interface format output file which
gets the sum totals in TALLY mode.  The default column is the n'th column
if there are n input images.
.VARIABLE BYTES
BYTES is a list of integers, each of which is 1, 2, or 4, depending on 
whether the data in the corresponding file is in a byte, halfword, or
fullword format.  The default is to determine image format from the labels.
.VARIABLE ZEROES
ZEROES is a list of input file numbers specifying those files
which, if containing a zero-valued sample, will cause the program
to include the sample and the resulting DN combination.  ZEROES = (2),for
example, will have the program include combinations where the second image
contains a zero-valued sample and the other images contain non-zero pixels.
The default is ZEROES = () which ignores zeroes in all images;  use 
ZEROES = (1,2,3,4,5,6,7,8,9,10) to include zero-valued samples for all 
image files.
.VARIABLE TALLY
TALLY is a keyword that specifies tally mode.  In tally mode the last input
image is the sum image (i.e. for each unique pixel combination in the other
images a sum over the pixels for this image will be made).  The sum results
go into the SUMCOL and the number of pixels (as usual) go into the AREACOL.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmultovly.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail = "continue"
let $echo = "yes"
let $autousage = "none"
GEN A LINC = 0
GEN B SINC = 0
LIST A
LIST B
multovly (A,B) C
ibis-list C
end-proc
$!-----------------------------------------------------------------------------
$ create tstmultovly.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

let $autousage = "none"
GEN A LINC = 0
Beginning VICAR task GEN
GEN Version 6
GEN task completed
GEN B SINC = 0
Beginning VICAR task GEN
GEN Version 6
GEN task completed
LIST A
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu Sep 11 11:39:03 2014
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       0   1   2   3   4   5   6   7   8   9
      3       0   1   2   3   4   5   6   7   8   9
      4       0   1   2   3   4   5   6   7   8   9
      5       0   1   2   3   4   5   6   7   8   9
      6       0   1   2   3   4   5   6   7   8   9
      7       0   1   2   3   4   5   6   7   8   9
      8       0   1   2   3   4   5   6   7   8   9
      9       0   1   2   3   4   5   6   7   8   9
     10       0   1   2   3   4   5   6   7   8   9
LIST B
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu Sep 11 11:39:03 2014
     Samp     1       3       5       7       9
   Line

      2       1   1   1   1   1   1   1   1   1   1
      3       2   2   2   2   2   2   2   2   2   2
      4       3   3   3   3   3   3   3   3   3   3
      5       4   4   4   4   4   4   4   4   4   4
      6       5   5   5   5   5   5   5   5   5   5
      7       6   6   6   6   6   6   6   6   6   6
      8       7   7   7   7   7   7   7   7   7   7
      9       8   8   8   8   8   8   8   8   8   8
     10       9   9   9   9   9   9   9   9   9   9
multovly (A,B) C
Beginning VICAR task multovly
MULTOVLY version Sep 11 2014
     81 ENTRIES TO THE OUTPUT FILE
ibis-list C
Beginning VICAR task ibis
 
Number of Rows:81  Number of Columns: 3       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        1.00
        1.00        2.00        1.00
        1.00        3.00        1.00
        1.00        4.00        1.00
        1.00        5.00        1.00
        1.00        6.00        1.00
        1.00        7.00        1.00
        1.00        8.00        1.00
        1.00        9.00        1.00
        2.00        1.00        1.00
        2.00        2.00        1.00
        2.00        3.00        1.00
        2.00        4.00        1.00
        2.00        5.00        1.00
        2.00        6.00        1.00
        2.00        7.00        1.00
        2.00        8.00        1.00
        2.00        9.00        1.00
        3.00        1.00        1.00
        3.00        2.00        1.00
        3.00        3.00        1.00
        3.00        4.00        1.00
        3.00        5.00        1.00
        3.00        6.00        1.00
        3.00        7.00        1.00
        3.00        8.00        1.00
        3.00        9.00        1.00
        4.00        1.00        1.00
        4.00        2.00        1.00
        4.00        3.00        1.00
 
Rows: 31:60
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        4.00        4.00        1.00
        4.00        5.00        1.00
        4.00        6.00        1.00
        4.00        7.00        1.00
        4.00        8.00        1.00
        4.00        9.00        1.00
        5.00        1.00        1.00
        5.00        2.00        1.00
        5.00        3.00        1.00
        5.00        4.00        1.00
        5.00        5.00        1.00
        5.00        6.00        1.00
        5.00        7.00        1.00
        5.00        8.00        1.00
        5.00        9.00        1.00
        6.00        1.00        1.00
        6.00        2.00        1.00
        6.00        3.00        1.00
        6.00        4.00        1.00
        6.00        5.00        1.00
        6.00        6.00        1.00
        6.00        7.00        1.00
        6.00        8.00        1.00
        6.00        9.00        1.00
        7.00        1.00        1.00
        7.00        2.00        1.00
        7.00        3.00        1.00
        7.00        4.00        1.00
        7.00        5.00        1.00
        7.00        6.00        1.00
 
Rows: 61:81
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        7.00        7.00        1.00
        7.00        8.00        1.00
        7.00        9.00        1.00
        8.00        1.00        1.00
        8.00        2.00        1.00
        8.00        3.00        1.00
        8.00        4.00        1.00
        8.00        5.00        1.00
        8.00        6.00        1.00
        8.00        7.00        1.00
        8.00        8.00        1.00
        8.00        9.00        1.00
        9.00        1.00        1.00
        9.00        2.00        1.00
        9.00        3.00        1.00
        9.00        4.00        1.00
        9.00        5.00        1.00
        9.00        6.00        1.00
        9.00        7.00        1.00
        9.00        8.00        1.00
        9.00        9.00        1.00
end-proc
$ Return
$!#############################################################################
