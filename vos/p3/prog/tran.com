$!****************************************************************************
$!
$! Build proc for MIPL module tran
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:38:29
$!
$! Execute by entering:		$ @tran
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
$ write sys$output "*** module tran ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tran.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("tran.imake") .nes. ""
$   then
$      vimake tran
$      purge tran.bld
$   else
$      if F$SEARCH("tran.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tran
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tran.bld "STD"
$   else
$      @tran.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tran.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tran.com -mixed -
	-s tran.f -
	-p tran.pdf -
	-i tran.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tran.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
      INTEGER*4    CNT, DEF, I, ILOC, ILOC2, INUNIT, J, K, L, NBI
      INTEGER*4    NBO, NBYTE, NLI, NSI, NSO, NOUT, STATUS, NINP
      INTEGER*4    BAND(64), OUTUNIT(64)
      LOGICAL*1    BUF(900000), BUF2(900000)
      LOGICAL*1    BFLAG/.FALSE./
      CHARACTER*50 FNAMES(64)
      CHARACTER*3  INORG, OUTORG, ORG
      CHARACTER*8  FORMAT
C
      CALL XVMESSAGE('TRAN Version 6-November-2003',' ')
C
      CALL XVPARM('OUTORG',OUTORG,CNT,DEF,1)
      CALL XVP('INP',FNAMES,NINP)
      IF (NINP.GT.1) THEN
	  CALL COMBINE(NINP,OUTORG,BUF)
	  RETURN
      END IF
C
C  OPEN INPUT FILE
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
      CALL XVGET(INUNIT,STATUS,'NL',NLI,'NS',NSI,'NB',NBI,'ORG',
     &           INORG,'FORMAT',FORMAT,' ')
C
      CALL XVP('OUT',FNAMES,NOUT)
C
C  CHECK INPUT IMAGE SIZE IF IT'S MSS
C
      CALL XVPARM('NBANDS',NBO,CNT,DEF,1)
      IF (DEF.EQ.0) THEN
         IF (INORG.EQ.'BSQ' .AND. NBI.EQ.1) THEN
            INORG = 'MSS'
            NSO = INT(NSI/NBO)
            IF (NBO*NSO.NE.NSI) THEN
	       CALL XVMESSAGE(' Parameter Error:',' ')
	       CALL XVMESSAGE(
     +         ' Number of samples in input is not divisible by NBANDS',
     +		' ')
	       CALL ABEND
            ENDIF
         ELSE
            CALL XVMESSAGE(' NBANDS IGNORED.  ONLY USED FOR MSS',' ')
         ENDIF
      ELSE
         NBO = NBI
         NSO = NSI
      ENDIF
C
C  IF OUTORG = MSS, THEN ORG = BSQ, NSO = NSI*NBI, NBO = 1, NLO = NLI
C
      IF (OUTORG.EQ.'MSS') THEN
         ORG = 'BSQ'
         NBO = 1
         NSO = NSI * NBI
      ELSE
         ORG = OUTORG
      ENDIF
C
C  IF BAND IS SPECIFIED, THEN EACH OUTPUT IS 1 BAND, BSQ
C
      CALL XVPARM('BANDS',BAND,CNT,DEF,64)
      IF (DEF.EQ.0) THEN
         IF (OUTORG.NE.'BSQ') THEN
	    CALL XVMESSAGE(' Parameter Error:',' ')
	    CALL XVMESSAGE(
     + ' BANDS is only specified for creating BSQ files of 1 band each',
     + 		' ')
	    CALL ABEND
         ENDIF
         IF (CNT.NE.NOUT) THEN
	    CALL XVMESSAGE(
     +          ' Number of bands does not match the number of outputs',
     +		' ')
	    CALL ABEND
	 ENDIF
         DO I = 1,NOUT
            IF (BAND(I).GT.NBO) THEN
		CALL XVMESSAGE(
     + ' Band number is larger than the number of bands in the input',
     +	' ')
		CALL ABEND
	    ENDIF
         ENDDO
         BFLAG = .TRUE.
      ENDIF
C
C  CHECK THAT BUF IS LARGE ENOUGH
C
      IF (FORMAT.EQ.'BYTE') THEN
         NBYTE = 1
      ELSEIF (FORMAT.EQ.'HALF') THEN
         NBYTE = 2
      ELSEIF (FORMAT.EQ.'FULL' .OR. FORMAT.EQ.'REAL') THEN
         NBYTE = 4
      ELSE
         NBYTE = 8
      ENDIF
      IF (NSI*NBI*NBYTE.GT.900000) THEN
	 CALL XVMESSAGE(
     +    ' Internal buffer is too small. Notify cog. programmer',' ')
	 CALL ABEND
      ENDIF
C
C  OPEN OUTPUT FILE
C
      IF (BFLAG) THEN
         DO I = 1,NOUT
            CALL XVUNIT(OUTUNIT(I),'OUT',I,STATUS,' ')
            CALL XVOPEN(OUTUNIT(I),STATUS,'OP','WRITE','OPEN_ACT',
     &           'SA','U_NL',NLI,'U_NS',NSO,'U_NB',1,'U_ORG',ORG,' ')
            CALL XLADD(OUTUNIT(I),'HISTORY','BAND',BAND(I),STATUS,
     &           'FORMAT','INT','ERR_ACT','SA',' ')
         ENDDO
      ELSE
         CALL XVUNIT(OUTUNIT(1),'OUT',1,STATUS,' ')
         CALL XVOPEN(OUTUNIT(1),STATUS,'OP','WRITE','OPEN_ACT',
     &        'SA','U_NL',NLI,'U_NS',NSO,'U_NB',NBO,'U_ORG',ORG,' ')
      ENDIF
C
C*******************
C
C  MSS TO BIL OR BIP
C
      IF (INORG.EQ.'MSS') THEN
         IF (OUTORG.EQ.'BIL' .OR. OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',1,
     &                     'NSAMPS',NSI,' ')
C
C     MSS TO BIL
C
               IF (OUTORG.EQ.'BIL') THEN
                  DO J = 1,NBO
                     ILOC = (NSO * (J-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
C
C     MSS TO BIP
C
               ELSE
                  DO J = 1,NSO
                     DO K = 1,NBO
                        ILOC = (NSO * (K-1) + (J-1))*NBYTE + 1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  MSS TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,' ')
                  DO J = 1,NOUT
                     ILOC = (NSO * (BAND(J)-1))*NBYTE  + 1
                     CALL XVWRIT(OUTUNIT(J),BUF(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBO
                  DO J = 1,NLI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                           'SAMP',1,'NSAMPS',NSI,' ')
                     ILOC = (NSO * (I-1))*NBYTE  + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE(' OUTORG must be BIL, BIP, or BSQ',' ')
	    CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BIL TO MSS OR BIP
C
      IF (INORG.EQ.'BIL') THEN
         IF (OUTORG.EQ.'MSS' .OR. OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               DO J = 1,NBI
                  ILOC = (NSI * (J-1))*NBYTE + 1
                  CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,'BAND',J,' ')
               ENDDO
C
C     BIL TO MSS
C
               IF (OUTORG.EQ.'MSS') THEN
                  CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
C
C     BIL TO BIP
C
               ELSE
                  DO J = 1,NSI
                     DO K = 1,NBI
                        ILOC = (NSI * (K-1) + (J-1))*NBYTE +1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  BIL TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  DO J = 1,NOUT
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,
     &                    'SAMP',1,'NSAMPS',NSI,'BAND',BAND(J),' ')
                     CALL XVWRIT(OUTUNIT(J),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBI
                  DO J = 1,NLI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                    'SAMP',1,'NSAMPS',NSI,'BAND',I,' ')
                     CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE(' OUTORG must be BIL, BIP, or BSQ',' ')
	    CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BIP TO BIL OR MSS
C
      IF (INORG.EQ.'BIP') THEN
         IF (OUTORG.EQ.'BIL' .OR. OUTORG.EQ.'MSS') THEN
            DO I = 1,NLI
               DO J = 1,NSI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',
     &                        J,'BAND',1,'NBANDS',NBI,' ')
                  DO K = 1,NBI
                     ILOC2 = (NSI * (K-1) + (J-1))*NBYTE + 1
                     ILOC = (K-1)*NBYTE + 1
                     DO L = 1,NBYTE
                        BUF2(ILOC2) = BUF(ILOC)
                        ILOC = ILOC + 1
                        ILOC2 = ILOC2 + 1
                     ENDDO
                  ENDDO
               ENDDO
C
C     BIP TO BIL
C
               IF (OUTORG.EQ.'BIL') THEN
                  DO K = 1,NBI
                     ILOC = (NSI * (K-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF2(ILOC),STATUS,' ')
                  ENDDO
C
C     BIP TO MSS
C
               ELSE
                  CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
               ENDIF
            ENDDO
C
C  BIP TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  DO J = 1,NSI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',
     &                           J,'BAND',1,'NBANDS',NBI,' ')
                     DO K = 1,NOUT
                        ILOC2 = (NSI*(BAND(K)-1)+(J-1))*NBYTE + 1
                        ILOC = (BAND(K)-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                  ENDDO
                  DO K = 1,NOUT
                     ILOC = (NSI * (BAND(K)-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(K),BUF2(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBI
                  DO J = 1,NLI
                     DO K = 1,NSI
                        ILOC = (K-1)*NBYTE + 1
                        CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',
     &                       J,'SAMP',K,'BAND',I,'NBANDS',1,' ')
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE(' OUTORG must be BIL, BIP, or BSQ',' ')
	    CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BSQ TO MSS, BIL, OR BIP
C
      IF (INORG.EQ.'BSQ') THEN
         IF (OUTORG.EQ.'MSS' .OR. OUTORG.EQ.'BIL' .OR.
     &       OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               DO J = 1,NBI
                  ILOC = (NSI * (J-1))*NBYTE + 1
                  CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,'BAND',J,' ')
               ENDDO
C
C     BSQ TO MSS
C
               IF (OUTORG.EQ.'MSS') THEN
                  CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
C
C     BSQ TO BIL
C
               ELSEIF (OUTORG.EQ.'BIL') THEN
                  DO J = 1,NBO
                     ILOC = (NSO * (J-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
C
C     BSQ TO BIP
C
               ELSEIF (OUTORG.EQ.'BIP') THEN
                  DO J = 1,NSO
                     DO K = 1,NBO
                        ILOC = (NSO * (K-1) + (J-1))*NBYTE + 1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  BSQ TO MULTIPLE OUTPUTS OF 1 BAND OF BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ' .AND. BFLAG) THEN
            DO I = 1,NOUT
               DO J = 1,NLI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                 'SAMP',1,'NSAMPS',NSI,'BAND',BAND(I),' ')
                  CALL XVWRIT(OUTUNIT(I),BUF,STATUS,' ')
               ENDDO
            ENDDO
         ELSE  
            CALL XVMESSAGE(
     +   ' OUTORG must be MSS, BIL, BIP, or output(s) of 1 band of BSQ',
     +		' ')
	    CALL ABEND
         ENDIF
      ENDIF
C
C  CLOSE FILES
C
      CALL XVCLOSE(INUNIT,STATUS,' ')
      DO I = 1,NOUT
         CALL XVCLOSE(OUTUNIT(I),STATUS,' ')
      ENDDO
      RETURN
      END
C*******************************************************************************
	SUBROUTINE COMBINE(NINP,ORGOUT,BUF)
C
	IMPLICIT NONE
	INTEGER INUNIT(64),NBIN(64)
	INTEGER NINP,I,J,K,NL,NS,NB,NLX,NSX,ISTAT,IOUTUNIT
	CHARACTER*80 MSG
	CHARACTER*3 ORGIN,ORGOUT
	LOGICAL QBSQIN(64)
	BYTE BUF(900000)
C					    check for a valid output ORG request
	IF (ORGOUT .EQ. 'MSS') THEN
	    CALL XVMESSAGE(
     +            ' Use the program MSS to combine to MSS format',' ')
	    CALL ABEND
	ENDIF
	IF (ORGOUT .EQ. 'BIP') THEN
	    CALL XVMESSAGE(
     +	      ' Combining datasets to BIP format not implemented',' ')
	    CALL ABEND
	ENDIF
C						open inputs, get size, number of
C					   bands, and organization of the inputs
	NB = 0
	DO I=1,NINP
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INUNIT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			' ')
	    CALL XVGET(INUNIT(I),ISTAT,'NL',NLX,'NS',NSX,'NB',NBIN(I),
     +		       'ORG',ORGIN,' ')
	    IF (I.EQ.1) THEN
		NL = NLX
		NS = NSX
	    ELSE
		IF (NL.NE.NLX .OR. NS.NE.NSX) THEN
		    CALL XVMESSAGE(
     +		    ' All images must be of the same size',' ')
		    CALL ABEND
		ENDIF
	    END IF
	    NB = NB + NBIN(I)
	    IF (ORGIN .EQ. 'BIP') THEN
		CALL XVMESSAGE(
     +	      ' Combining datasets of BIP format not implemented',' ')
		CALL ABEND
	    ELSE
		QBSQIN(I) = ORGIN .EQ. 'BSQ'
	    END IF
	END DO
	WRITE (MSG,100) NB,ORGOUT
100	FORMAT(' The output file has a total of',I5,
     +		' bands, with ORG = ',A3)
	CALL XVMESSAGE(MSG,' ')
C								     open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		    'U_NB',NB,'U_ORG',ORGOUT,'OPEN_ACT','SA',
     +		    'IO_ACT','SA',' ')
C
	IF (ORGOUT .EQ. 'BSQ') THEN
	    DO I=1,NINP
		DO J=1,NBIN(I)
		    DO K=1,NL
			CALL XVREAD(INUNIT(I),BUF,ISTAT,'BAND',J,
     +				    'LINE',K,' ')
			CALL XVWRIT(IOUTUNIT,BUF,ISTAT,' ')
		    END DO
		END DO
	    END DO
	ELSE 		! ORG .EQ. 'BIL'
	    DO I=1,NL
		DO J=1,NINP
		    DO K=1,NBIN(J)
			CALL XVREAD(INUNIT(J),BUF,ISTAT,'BAND',K,
     +				    'LINE',I,' ')
			CALL XVWRIT(IOUTUNIT,BUF,ISTAT,0)
		    END DO
		END DO
	    END DO
	ENDIF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tran.pdf
process help=*
PARM INP    TYPE=STRING  COUNT=(1:50)
PARM OUT    TYPE=STRING  COUNT=(1:50)
PARM OUTORG TYPE=KEYWORD COUNT=0:1  DEFAULT=BSQ VALID=(BIL,BIP,BSQ,MSS)
PARM NBANDS TYPE=INTEGER COUNT=0:1  DEFAULT=--
PARM BANDS  TYPE=INTEGER COUNT=0:50 DEFAULT=--
END-PROC
.TITLE
VICAR Program TRAN
.HELP
PURPOSE:

TRAN converts images from one organization to another. It can also split a
single multichannel input into single channel files, or combine multiple
files into a single multichannel file. 

For a single input, the possible output organizations are BSQ, BIL, BIP, and
MSS. The input image must have a VICAR label and can be any format. For 
multiple inputs, each input may have one or multiple bands, and the output 
may be BSQ or BIL. For multiple inputs of multiple band files, the inputs may
be BSQ, BIL, or a mixture of the two, but not BIP or MSS.

The parameter OUTORG is the output organization.  The default for OUTORG
is BSQ.  For input MSS images, the VICAR label is BSQ with 1 band, and the
parameter NBANDS must be given so that the number of bands is known and
the number of samples per band can be determined.

The parameter BANDS is used to output specific bands into separate BSQ
files.  Up to 50 bands can be specified with each band being output to
a separate BSQ file.
.page
Examples:

  TRAN INP.BSQ OUT.BIL OUTORG=BIL

This will convert the BSQ file into a BIL file.  The format, number of lines,
number of samples, and number of bands remain the same.


  TRAN INP.MSS OUT.BIP 'BIP NBANDS=3

This will convert the MSS file into a BIP file.  NBANDS is needed so that the
number of bands is known for the input file and the number of samples can be
determined for the output.  
.page
  TRAN INP.BSQ OUT.MSS 'MSS

This will convert the BSQ file into a MSS file.  Be aware that the VICAR
label of the MSS file will say BSQ, with the number of samples being the
number of samples of one band of the BSQ input file times the number of
bands.


  TRAN INP.BIP OUT.BSQ 'BSQ BANDS=(2,4,5)

This will convert the bands 2, 4, and 5 of the BIP file into 3 separate BSQ
files.

  TRAN (INP.1,INP.2,INP.3) OUT.BIL 'BIL

This will combine three inputs into a 3 band BIL format file.
.page
WRITTEN BY:  J. R. Heyada
COGNIZANT PROGRAMMER:  J. R. Heyada
MODIFIED FOR MULTIPLE INPUTS: Ron Alley
DATE:  November 15, 1988
MODIFIED FOR MULTIPLE MULTICHANNEL INPUTS: Ron Alley
DATE:  November 6, 2003
.LEVEL1
.VARIABLE INP
Input image file(s)
.VARIABLE OUT
Output image file(s)
.VARIABLE OUTORG
Output organization.
The choices are BSQ,
BIL, BIP, and MSS.
The default is BSQ.
.VARIABLE NBANDS
The number of bands
that the MSS input
image has.
.VARIABLE BANDS
A list of bands, up
to 50, which will be
output into separate
BSQ files.
.LEVEL2
.VARIABLE INP
The input image can be any format. The organization can be BSQ, BIL,
BIP, or MSS for a single input, but must be BSQ or BIL when combining
multiple inputs into a single output.  
.VARIABLE OUT
The output image(s) will be the same format as the input.  If the parameter
BANDS is specified, up to 50 BSQ files can be output.
.VARIABLE OUTORG
This is the output organization.  The choices are BSQ, BIL, BIP, or MSS.
Be aware that the VICAR label of an MSS image is BSQ.
.VARIABLE NBANDS
If the input is MSS, then NBANDS must be given so that the number of bands
can be determined.
.VARIABLE BANDS
OUTORG must be BSQ.  The number of output files must equal the number of
bands listed.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create tran.imake
#define  PROGRAM   tran

#define MODULE_LIST tran.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
