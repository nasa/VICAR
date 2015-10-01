$!****************************************************************************
$!
$! Build proc for MIPL module cform
$! VPACK Version 1.9, Monday, December 07, 2009, 16:01:53
$!
$! Execute by entering:		$ @cform
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
$ write sys$output "*** module cform ***"
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
$ write sys$output "Invalid argument given to cform.com file -- ", primary
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
$   if F$SEARCH("cform.imake") .nes. ""
$   then
$      vimake cform
$      purge cform.bld
$   else
$      if F$SEARCH("cform.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cform
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cform.bld "STD"
$   else
$      @cform.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cform.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cform.com -mixed -
	-s cform.f -
	-i cform.imake -
	-p cform.pdf -
	-t tstcform.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cform.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C    CFORM -- PROGRAM TO CONVERT BETWEEN DIFFERENT STORAGE FORMATS FOR
C         IMAGES AND TO PERFORM LINEAR TRANSFORMATIONS ON THEM
C
C  ORIGINAL WRITTEN BY J. J. LORRE
C  5/83 - 6/83    ... TRANSLATED FROM PL/1 TO FORTRAN BY DAN STANFILL
C  84-12-17  ...LWK... PARTIAL (I/O ONLY) VICAR2 CONVERSION TO FIX LABEL
C			PROCESSING
C  85-1-26   ...LWK... CONVERT PARAMS TO VICAR2 & FIX BUGS
C  85-2-18   ...LWK... ENABLE 'COMPLEX' TYPE, REMOVE 'FORMAT' PARAMETER,
C			CHECK FOR OVERFLOW IN REAL TO INTEGER CONVERSION
C  89-5-26   ...TCG... SUPPORT 'COMP' TYPE
C  92-3-26   ...NDR... PORTED TO UNIX; RENAMED 'CFORM' FROM 'C'
C  92-3-38   ...SP.... removed some obsolete code for input FORMAT = REAL
C                      but PIX_SIZE=8, used XVSIZE in place of in-line code,
C                      used NINT of output DN for rounding for BYTE, HALF, 
C                      and FULL output (the old code did NINT(OF)).
C                      Corrected code for adding history label item.
C  93-07-13  ...LWK... ADDED BINARY OPTION AND BSQ FORMAT SUPPORT
C  94-02-18  ...LWK... added U_FORMAT back to XVOPEN of output, inadvertently
C			omitted in previous mod
C  98-06-05  ...RRP... Removed extra ')' from the pdf.
c
c
c  00-10-10  ...bam..  Corrected buffer allocations as per DLR error report.
c                      both orecl and irecl compares are corrected.


      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER (A-Z)

      INTEGER BUFSIZE
      PARAMETER (BUFSIZE=1000000)
      INTEGER FIN(1),FOUT(1)
      INTEGER*2 HIN(1),HOUT(1)
      REAL FLIN(1),FLOUT(1),SLP,OFF,SLOFF(2),RANG(2,2),X
      EQUIVALENCE (SLOFF(1),SLP), (SLOFF(2),OFF)
      DOUBLE PRECISION BUFIN(BUFSIZE),BUFOUT(BUFSIZE)
      DOUBLE PRECISION DIN(1),DOUT(1),D
      COMPLEX CIN(1),COUT(1)
      CHARACTER*7 IFMT, OFMT, UFMT
      CHARACTER*3 ORG
      INTEGER SIZE(4)
      REAL FI4MAX/2.14748E9/    ! MAX. FOR INTEGER*4 CONVERSION
      EQUIVALENCE (SIZE(1),SL), (SIZE(2),SS), (SIZE(3),NLO),
     & (SIZE(4),NSO)

      CHARACTER*80 MSG,MSG2,MSG3
      EQUIVALENCE (BUFIN(1),HIN(1),FIN(1),FLIN(1),DIN(1),CIN(1))
      EQUIVALENCE (BUFOUT(1),HOUT(1),FOUT(1),FLOUT(1),
     2             DOUT(1),COUT(1))
      LOGICAL XVPTST, BINARY

      CALL XVMESSAGE('CFORM VERSION 06-JUN-1998',' ')

      BINARY = XVPTST('BINARY')

      CALL XVUNIT(IUN,'INP',1,STAT, ' ')
      IF (BINARY) THEN 
	CALL XVOPEN(IUN,STAT,'OPEN_ACT',' ','IO_ACT','SA',
     1 'COND','BINARY',' ')
      ELSE 
	CALL XVOPEN(IUN,STAT,'OPEN_ACT',' ','IO_ACT','SA', ' ')
      ENDIF

      CALL XVGET( IUN, STAT, 'FORMAT', IFMT, 'NLB', NLBI, 'NBB', NSBI,
     & 'ORG', ORG, 'PIX_SIZE', IPIX, ' ')
      IF (ORG.NE.'BSQ') CALL MABEND(
     & ' ** CFORM ONLY SUPPORTS BSQ FORMAT **')
      IF (.NOT.BINARY) THEN
	NLBI = 0
	NSBI = 0
      ENDIF
      IF (IFMT.EQ.'WORD') IFMT = 'HALF'     ! FIX UP NON-STANDARD FORMATS
      IF (IFMT.EQ.'LONG') IFMT = 'FULL'
      IF (IFMT.EQ.'COMPLEX') IFMT = 'COMP'

      CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI)
      CALL XVBANDS( SB, NBO, NBI)

C  CAN NOT COPY BINARY PARTS UNAMBIGUOUSLY IF OUTPUT IS A
C  SUB-WINDOW OF INPUT:
      IF ( (( SL .NE. 1 ) .OR. ( SS .NE. 1 ) .OR. ( SB .NE. 1 ) .OR.
     *	   ( NLO .NE. NLI) .OR. ( NSO .NE. NSI) .OR. ( NBO .NE. NBI ))
     *     .AND. BINARY ) CALL MABEND(
     * ' ** CANNOT SPECIFY "BINARY" WITH SIZE WINDOW **')

C  OUTPUT FORMAT PARAMETER
      CALL XVPARM( 'OFORM', OFMT, I, IDEF,' ')
      IF (I.EQ.0) THEN            ! DEFAULTS:
	IF (IFMT.EQ.'BYTE') THEN
	  OFMT = 'HALF'
	ELSE
	  OFMT = 'BYTE'
	ENDIF
      ENDIF
      IF (OFMT.EQ.'REAL8') OFMT = 'DOUB'     ! FIX UP NON-STANDARD FORMATS
      IF (OFMT.EQ.'COMPLEX') OFMT = 'COMP'

      IF (OFMT.EQ.'BYTE') OPIX = 1
      IF (OFMT.EQ.'HALF') OPIX = 2
      IF (OFMT.EQ.'FULL' .OR. OFMT.EQ.'REAL') OPIX = 4
      IF (OFMT.EQ.'DOUB' .OR. OFMT.EQ.'COMP') OPIX = 8

C  CHECK BUFFER LENGTHS:
      IRECL = NSBI+IPIX*NLI
      ORECL = NSBI+OPIX*NLO
      IF (IRECL.GT.(BUFSIZE*8)) CALL MABEND(
     & ' ** INPUT LINE LENGTH EXCEEDS BUFFER SIZE **')
      IF (ORECL.GT.(BUFSIZE*8)) CALL MABEND(
     & ' ** OUTPUT LINE LENGTH EXCEEDS BUFFER SIZE **')

C  IF BINARY OPTION WILL CAUSE LOSS OF HEADER DATA, CANCEL TASK;  IF IT
C  EXPANDS HEADERS, ISSUE WARNING:
      IF (BINARY .AND. NLBI.GT.0) THEN
	IF (OPIX.LT.IPIX) THEN
	  CALL MABEND(
     & ' ** Format change will cause loss of binary header data! **')
	ELSEIF (OPIX.GT.IPIX) THEN
	  CALL XVMESSAGE(' Warning: format change will increase ' //
     & 'lengths of binary headers!',' ')
	  CALL XVMESSAGE(' Binary headers will be padded with zeroes.',
     1 ' ')
	ENDIF
      ENDIF

      IF (IFMT.EQ.'BYTE') THEN
	CALL XVCLOSE( IUN, STAT,' ')
	IF (BINARY) THEN 
	  CALL XVOPEN( IUN, STAT, 'OPEN_ACT', ' ', 'IO_ACT', 'SA',
     1  'COND', 'BINARY', 'U_FORMAT', 'HALF',' ')
	ELSE 
	  CALL XVOPEN( IUN, STAT, 'OPEN_ACT', ' ', 'IO_ACT', 'SA',
     1  'U_FORMAT', 'HALF',' ')
	ENDIF
      ENDIF

C  SLOPE/OFFSET
      CALL XVPARM( 'SO', SLOFF, I, IDEF,' ')
      IF (I.EQ.0) THEN
        SLOFF(1) = 1.0
        SLOFF(2) = 0.0
	CALL XVPARM( 'IRANGE', RANG(1,1), IRCNT ,IDEF,' ')
	CALL XVPARM( 'ORANGE', RANG(1,2), ORCNT ,IDEF,' ')
	IF (IRCNT.NE.ORCNT) THEN
	  CALL MABEND(' ** SPECIFY BOTH IRANGE/ORANGE, OR NEITHER **')
	ELSEIF (IRCNT.NE.0) THEN
	  SLP = (RANG(1,2)-RANG(2,2))/(RANG(1,1)-RANG(2,1))
	  OFF = RANG(1,2)-SLP*RANG(1,1)
	ENDIF
      ENDIF

C  old code to CORRECT FOR TRUNCATION ERRORS deleted. 
C  NINT function used instead in main loop below.
	  
      WRITE(MSG,1234) SLP,OFF
 1234 FORMAT('OUT = IN *',F10.3,'+',F10.3)
      CALL XVMESSAGE( MSG, ' ')

C  FINAL IN/OUT FORMAT MESSAGE 

      WRITE(MSG2,1235) IFMT(:4)
 1235 FORMAT('INPUT FORMAT = ',A)
      CALL XVMESSAGE(MSG2,' ')
	  
      WRITE(MSG3,1236) OFMT(:4)
 1236 FORMAT('OUTPUT FORMAT = ',A)
      CALL XVMESSAGE(MSG3,' ')

C  OPEN OUTPUT
      CALL XVUNIT( OUN, 'OUT', 1, STAT,' ')
      UFMT = OFMT
      IF (OFMT.EQ.'BYTE') UFMT = 'HALF'
      IF (BINARY) THEN
	CALL XVOPEN( OUN, STAT, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS', NSO,
     1 'U_NB', NBO, 'O_FORMAT', OFMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     2 'U_FORMAT', UFMT, 'COND', 'BINARY', 'U_NBB', NSBI, 'U_NLB', NLBI,
     3 ' ')
      ELSE 
	CALL XVOPEN( OUN, STAT, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS', NSO,
     1 'U_NB', NBO, 'O_FORMAT', OFMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     2 'U_FORMAT', UFMT, ' ')
      ENDIF

      CALL XLADD( OUN, 'HISTORY', 'CONV', MSG, STAT, 'FORMAT', 'STRING',
     & 'ULEN', 31, ' ')

      IF (BINARY .AND. NLBI.GT.0) THEN
	IF (ORECL.GT.IRECL) CALL MVE( 1, ORECL-IRECL, 0, BUFIN(ORECL+1),
     1      0, 1)
	DO REC = 1,NLBI
	  CALL XVREAD( IUN, BUFIN, STAT, ' ')
	  CALL XVWRIT( OUN, BUFIN, STAT, ' ')
	ENDDO 
      ENDIF

C  MAIN BANDS/LINES LOOP
      DO IB = SB,SB+NBO-1
      DO IL = 1,NLO

	LINE = NLBI+SL-1+IL
		! VICAR BINARY LABELS ARE IN BAND 1 ONLY!!
	IF (BINARY.AND.IB.GT.1) LINE = SL-1+IL

		! READ BINARY PREFIXES INTO END OF OUTPUT BUFFER:
	IF (BINARY) CALL XVREAD( IUN, BUFOUT(NLO+1), STAT, 'LINE', LINE,
     1 'NSAMPS', NSBI, 'BAND', IB, ' ')
	CALL XVREAD( IUN, BUFIN, STAT, 'LINE', LINE, 'SAMP', NSBI+SS,
     1 'NSAMPS', NSO, 'BAND', IB, ' ')
	  
	IF (IFMT.EQ.'BYTE' .OR. IFMT.EQ.'HALF') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
	      I=NINT( HIN(J)*SLP+OFF)
	      IF (I.LT.0) I = 0
	      IF (I.GT.255) I = 255
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
	      I = NINT( HIN(J)*SLP+OFF)
	      IF (I.LT.-32768) I = -32768
	      IF (I.GT.32767) I = 32767
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
	      FOUT(J) = NINT( HIN(J)*SLP+OFF)
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
	      FLOUT(J) = HIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
	      DOUT(J)=HIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
	      COUT(J) = CMPLX(HIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'FULL') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
	      I = NINT( FIN(J)*SLP+OFF)
	      IF (I.LT.0) I=0
	      IF (I.GT.255) I = 255
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
	      I = NINT( FIN(J)*SLP+OFF)
	      IF (I.LT.-32768) I = -32768
	      IF (I.GT.32767) I = 32767
	      HOUT(J) = I
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
	      FOUT(J) = NINT( FIN(J)*SLP+OFF)
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
	      FLOUT(J) = FIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
	      DOUT(J) = FIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
	      COUT(J) = CMPLX(FIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'REAL') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.0.) THEN
		HOUT(J) = 0
	      ELSEIF (X.GT.255.) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.-32768.) THEN
		HOUT(J) = -32768
	      ELSEIF (X.GT.32767.) THEN
		HOUT(J) = 32767
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
	      X = FLIN(J)*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
	      FLOUT(J) = FLIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
	      DOUT(J) = FLIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
	      COUT(J) = CMPLX(FLIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'DOUB') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
	      D = DIN(J)*SLP+OFF
	      IF (D.LT.0.) THEN
		HOUT(J) = 0
	      ELSEIF (D.GT.255.) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( D)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
	      D = DIN(J)*SLP+OFF
	      IF (D.LT.-32768.) THEN
		HOUT(J) = -32768
	      ELSEIF (D.GT.32767.) THEN
		HOUT(J) = 32767
	      ELSE
		HOUT(J) = NINT( D)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
	      X = DIN(J)*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
	      FLOUT(J) = DIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
	      DOUT(J) = DIN(J)*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
	      COUT(J) = CMPLX(DIN(J)*SLP+OFF)
	    END DO
	  END IF
	ELSE IF (IFMT.EQ.'COMP') THEN
	  IF (OFMT.EQ.'BYTE') THEN
	    DO J = 1,NSO
	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.0) THEN
		HOUT(J) = 0
	      ELSEIF (X.GT.255) THEN
		HOUT(J) = 255
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'HALF') THEN
	    DO J = 1,NSO
	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.-32768.) THEN
		HOUT(J) = -32768.
	      ELSEIF (X.GT.32767.) THEN
		HOUT(J) = 32767.
	      ELSE
		HOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'FULL') THEN
	    DO J = 1,NSO
	      X = CABS(CIN(J))*SLP+OFF
	      IF (X.LT.-FI4MAX) THEN
		FOUT(J) = -FI4MAX
	      ELSEIF (X.GT.FI4MAX) THEN
		FOUT(J) = FI4MAX
	      ELSE
		FOUT(J) = NINT( X)
	      ENDIF
	    END DO
	  ELSE IF (OFMT.EQ.'REAL') THEN
	    DO J = 1,NSO
	      FLOUT(J) = CABS(CIN(J))*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'DOUB') THEN
	    DO J = 1,NSO
	      DOUT(J) = CABS(CIN(J))*SLP+OFF
	    END DO
	  ELSE IF (OFMT.EQ.'COMP') THEN
	    DO J = 1,NSO
	      COUT(J) = CIN(J)*SLP+OFF
	    END DO
	  END IF
C
	END IF

	LINE = NLBI+IL
	IF (BINARY.AND.IB.GT.1) LINE = IL
	IF (BINARY) CALL XVWRIT( OUN, BUFOUT(NLO+1), STAT, 'LINE', LINE,
     1 'NSAMPS', NSBI, 'SAMP', 1, 'BAND', IB, ' ')
        CALL XVWRIT( OUN, BUFOUT, STAT, 'LINE', LINE,
     1 'NSAMPS', NSO,'SAMP', NSBI+1, 'BAND', IB, ' ')
C
      END DO
      END DO
C  END OF MAIN BANDS & LINES LOOP

      CALL XVMESSAGE('CONVERSION COMPLETE',' ')
      CALL XVCLOSE( IUN, STAT,' ')
      CALL XVCLOSE( OUN, STAT,' ')

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cform.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM CFORM

   To Create the build file give the command:

		$ vimake CFORM			(VMS)
   or
		% vimake cform			(Unix)


************************************************************************/


#define PROGRAM	cform
#define R2LIB

#define MODULE_LIST cform.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create cform.pdf
process help=*
!
! FILE NAMES
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
!
! SIZE FIELD
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
!
! INPUT FILE PARAMETER
PARM IRANGE TYPE=REAL  COUNT=0:2  DEFAULT=--
!
! OUTPUT FILE PARAMETERS
PARM OFORM TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL,REAL8,DOUB,COMPLEX)+
 COUNT=0:1 DEFAULT=--
PARM ORANGE TYPE=REAL  COUNT=0:2  DEFAULT=--
PARM BINARY TYPE=KEYWORD VALID=BINARY COUNT=0:1 DEFAULT=--
!
! SLOPE AND OFFSET
PARM SO TYPE=REAL COUNT=0:2 DEFAULT=--

!# annot function="Vicar Data Conversion"
!# annot keywords=(image,linear,transform,buffer,halfwords,stretch)

END-PROC
!
! HELP TEXT
.TITLE
Converts images between data types with optional scaling
.HELP
PURPOSE
  CFORM is a VICAR applications program which converts images between any
  of the recognized data formats and performs linear transformations
  on images.

EXECUTION

  CFORM  INP  OUT  PARAMS

  where INP and OUT are input and output file parameters and PARAMS are
  other parameter that are described in Tutor mode.

METHOD

  The program reads each image line into a buffer and treats the contents
  of the buffer as data of type implied by the label.  Byte data are read
  in as halfwords.  A simple linear function (stretch) is then applied to 
  these data and they are written out in the output format specified.

  If no format code is specified for OUT the following action is taken:
    1.  If input format is BYTE, then output format is HALF.
    2.  If input format is not BYTE, then output format is BYTE.
.page
 COMPLEX DATA:

  The linear relation between IN and OUT must be qualified for complex
  data according to the following table:

       INPUT        OUTPUT       Transformation Type
      Format        Format

      non-complex   complex       OUT = (IN*SL+OF) + 0.0i
      complex       non-complex   OUT = |IN| * SL + OF
      complex       complex       OUT = IN * SL + OF
.page
EXAMPLES

  CFORM A B (1,1,10,20) IRANGE=(10,20) ORANGE=(-20,2000) OFORM=FULL
    Data set B is fullword integer where the values have been scaled
    linearly such that 10 -> -20 and 20 -> 2000.  Only the first 10
    lines and 20 samples of A are passed to B.

  CFORM A B OFORM=HALF SO=(2,0)
    Data set B's format is a halfword integer, with each input value
    multiplied by 2.
.page
RESTRICTIONS

 The maximum permitted line length (bytes per line) is 60,000 for BYTE
 format and 120,000 otherwise.
.page
COGNIZANT PROGRAMMER

Written by: J. J. Lorre    18 April 1980

REVISION:

Converted to VAX by: Dan Stanfill                          June 1983

Deleted FORMAT parameter, check for overflow in 
real-to-integer conversion, fixed test file
by: L. W. Kamp                                             Feb  1985

Put conversion factors into the label by: Florance Moss    Jan  1988

Current Cognizant Programmer:  L. W. Kamp

.LEVEL1
.VARI INP
Input file name
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR size field
.VARI SL
Starting line
.vari SS
Starting sample
.vari NL
Number of lines
.vari NS
Number of samples
.VARI IRANGE
Pair of numbers for performing
stretch.  ORANGE must also be
specified.
.VARI OFORM
Output storage format.
.VARI ORANGE
Output range values for stretch.
(See IRANGE.)
.VARI BINARY
Specifies that binary labels
must be copied to the output
.VARI SO
(Slope,offset) for linear
transformations.
.LEVEL2
.VARI INP
 Input file name
.VARI OUT
 Output file name
.VARI SIZE
The size field is specified with four arguments,
      SIZE=(SL,SS,NL,NS)
where:
SL is the starting line of the input file to be read.
SS is the starting sample to be read of each input line.
NL is the number of lines to be read.
NS is the number of samples to be read for each line.
For example, SIZE=(1,1,30,20)
would read the first 30 lines and 20 samples of the input picture.

These parameters can also be specified separately using SL, SS, NL,
and NS.
.VARI OFORM
OFORM (Keyword) specifies the format to which the input is to be converted.
The following are the valid values:

         BYTE              Unsigned 8-bit binary integer.
         HALF              Signed 16-bit binary integer.
         FULL              Signed 32-bit binary integer.
         REAL              VAX single precision floating point (REAL*4).
         DOUB or REAL8     VAX double precision floating point (REAL*8).
         COMPLEX           Two single precision floating point values
                           representing the real and imaginary parts of
                           a complex number (COMPLEX*8).
.VARI SO
     SO          SO is a keyword  followed by  two  values (real or
                 integer)  which specify  the slope (SL) and offset
                 (OF) of the  transformation  to be applied between
                 the input and output data sets in the sense:
                             OUT = IN * SL + OF
                 The default is SO=(1.0,0.0).
.VARI IRANGE
IRANGE  is a  keyword followed  by two values which specify two values
in the input  which  map to two values in the output (ORANGE must also 
be specified).  IRANGE and ORANGE values are converted into equivalent
SO values by "CFORM".  If SO is specified, then IRANGE and ORANGE are ignored.
If none of SO, IRANGE or ORANGE are specified, the default transformation
is performed:  SO=(1.0,0.0).
.VARI ORANGE
ORANGE is a keyword followed by two values which are used in conjunction
with the two values specified by IRANGE in order to determine a slope and
offset for a linear transformation.   See IRANGE.
.VARI BINARY
Specifies that any binary labels and prefixes in the input file must be 
copied to the output file.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcform.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
! 
write " This is a test of program CFORM"
write "  Requires SITOD1"
!
!Convert BYTE to HALF
! First, generate an input file
GEN X 10 10 IVAL=0 LINC=20 SINC=39
! Now convert first file to Halfword, which is default
CFORM X Y 
LIST Y
! Convert to other formats
CFORM X Y OFORM=FULL
LIST Y
!
CFORM X Y OFORM=REAL
LIST Y
!
CFORM X Y OFORM=DOUB
LIST Y
!
CFORM X Y OFORM=COMP
! Must print out complex data as reals
LIST Y 'REAL
!
! Now go from large format to small format performing linear transformations.
!
! Input file:
GEN X2 10 80 'REAL8 IVAL=-9999 LINC=100 SINC=200
CFORM X2 Y OFORM=BYTE SO=(2,3)
LIST Y
!
CFORM X2 Y IRANGE=(0,18)  ORANGE=(3,39) OFORM=HALF
LIST Y
!
! Test size field: read only 6 lines @ 16 bytes/line starting at sample 5
CFORM X2 Y SIZE=(1,5,6,16) OFORM=HALF SO=(3.46,1.6)
LIST Y
!
! test other input formats
GEN A 10 10 'HALF
CFORM A B
LIST B
!
GEN A 10 10 'FULL
CFORM A B
LIST B
!
GEN A 10 10 'REAL4
CFORM A B
LIST B
!
GEN A 10 10 'REAL8
CFORM A B
LIST B
!
GEN A 128 128 'REAL4
! convert to complex
CFORM A B oform=complex
LIST A (1,1,4,4)
CFORM B D
LIST D (1,1,4,4)
! Check FR 44888 COMPLEX->BYTE
CFORM B G oform=byte
list G (1,1,10,10)
! Check FR 44888 COMP->BYTE
label-replace B D items="format=comp" type=system 
CFORM D G oform=byte
list G (1,1,10,10)
!
!  test binary option:
! Verify that with 'BINARY, CFORM preserves the binary labels,
! but without 'BINARY, it drops them.
! To simulate an image with binary labels, just replace the label ...
gen a nl=10 ns=10
label-remove a b
label-create b c nl=8 ns=7 nlb=2 nbb=3 'bin 
label-list c
cform c d1 'binary
cform c d2
label-list d1
label-list d2
!  also check that the data were not corrupted:
list a
list c
list d1
list d2
!
!  test multi-spectral support:
gen a nl=5 ns=4 nb=3
list a
cform a b
list b
!
end-proc
$ Return
$!#############################################################################
