$!****************************************************************************
$!
$! Build proc for MIPL module uniflt
$! VPACK Version 1.6, Thursday, November 04, 1993, 11:19:31
$!
$! Execute by entering:		$ @uniflt
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module uniflt ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("uniflt.imake") .nes. ""
$   then
$      vimake uniflt
$      purge uniflt.bld
$   else
$      if F$SEARCH("uniflt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake uniflt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @uniflt.bld "STD"
$   else
$      @uniflt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create uniflt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack uniflt.com -
	-s uniflt.f -
	-i uniflt.imake -
	-t tuniflt.f tuniflt.imake tuniflt.pdf tstuniflt.pdf -
	-o uniflt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create uniflt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***************************************************************************
	SUBROUTINE UNIFLT(DCODE, NPIX, IN, OUT, NSW)
C
C	This routine performs a smoothing filter of NSW elements with unity
C	weights over the array of values in IN.  The output is placed in the
C	array OUT.  If NSW is positive, the result is normalized (i.e.,
C	DN(out) = sum of the NSW nearest pixels / NSW ); if NSW is negative,
C	the normalization by NSW is suppressed.  Output elements near the edges
C	are computed by assuming that the first and last elements are repeated
C	at all points beyond the edge.
C
C	Arguments:
C	    DCODE   Input    Integer    Code for the data types of IN and OUT
C					 IN	OUT
C				 1 =>	byte	byte
C				 2 =>	half	half
C				 3 =>	byte	half
C				 4 =>	full	full
C				 5 =>	byte	full
C				 6 =>	half	full
C				 7 =>	real	real
C				 8 =>	real*8	real*8
C				 9 =>	real	real*8
C				-3 =>	half	byte
C				-5 =>	full	byte
C				-6 =>	full	half
C				-9 =>	real*8	real
C
C	    NPIX    Input    Integer    Number of elements to be filtered
C
C	    IN      Input    Array      Array in input values
C
C	    OUT     Output   Array      Array of output values
C
C	    NSW     Input    Integer    Number of filter weights; NSW must be
C					odd.  If negative, the summation is
C					output, rather than the average.
	INTEGER DCODE, NPIX, NSW, IN(*), OUT(*)
C
C					force a valid NSW
	IF (NSW .GT. 0) THEN
	    DIVISOR = NSW
	    NSWX = NSW
	ELSE
	    DIVISOR = 1.0
	    NSWX = -NSW
	END IF
	IF (MOD(NSWX,2) .EQ. 0) NSWX = 2*NSWX + 1
C						call appropriate subroutine
	IF (DCODE .EQ. 1) THEN
	    CALL UFLT1(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 2) THEN
	    CALL UFLT2(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 3) THEN
	    CALL UFLT3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 4) THEN
	    CALL UFLT4(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 5) THEN
	    CALL UFLT5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 6) THEN
	    CALL UFLT6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 7) THEN
	    CALL UFLT7(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 8) THEN
	    CALL UFLT8(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 9) THEN
	    CALL UFLT9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -3) THEN
	    CALL UFLTM3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -5) THEN
	    CALL UFLTM5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -6) THEN
	    CALL UFLTM6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -9) THEN
	    CALL UFLTM9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE 
	    CALL XVMESSAGE('Invalid DCODE passed to UNIFLT', ' ')
	    CALL ABEND
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT1(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to byte filter
C
	BYTE IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT2(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to halfword filter
C
	INTEGER*2 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to halfword filter
C
	INTEGER*2 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT4(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to Fullword filter
C
	INTEGER*4 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to fullword filter
C
	INTEGER*4 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to fullword filter
C
	INTEGER*4 OUT(NPIX)
	INTEGER*2 IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT7(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*4 filter
C
	REAL*4 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT8(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*8 filter
C
	REAL*8 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*8 filter
C
	REAL*8 OUT(NPIX)
	REAL*4 IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Halfword to byte filter
C
	INTEGER*2 IN(NPIX)
	BYTE OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Fullword to byte filter
C
	INTEGER*4 IN(NPIX)
	BYTE OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to halfword filter
C
	INTEGER*4 IN(NPIX)
	INTEGER*2 OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*4 filter
C
	REAL*8 IN(NPIX)
	REAL*4 OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create uniflt.imake
/* Imake file for VICAR subroutine uniflt */

#define SUBROUTINE uniflt

#define MODULE_LIST uniflt.f

#define P2_SUBLIB

#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tuniflt.f
c  test subroutine UNIFLT

        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44

	IMPLICIT INTEGER(A-Z)

	BYTE A(10)/1,2,3,4,5,6,7,8,9,10/, B(10)
	INTEGER*2 C(10)/1,2,3,4,5,6,7,8,9,10/, D(10)
	INTEGER*4 E(10)/1,2,3,4,5,6,7,8,9,10/, F(10)
	REAL*4 P(10)/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./, Q(10)
	REAL*8 R(10), S(10)
        CHARACTER*250 MSG
	NSW = -9

c byte-to-byte
1	DC = 1
	CALL UNIFLT(DC,10,A,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')
100	FORMAT(' DCODE=',I3)
150     FORMAT(10I5) 
    
c byte-to-halfword
	DC = 3
	CALL UNIFLT(DC,10,A,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c halfword-to-byte
        DC = -3
	CALL UNIFLT(DC,10,C,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')

c halfword-to-halfword
	DC = 2
	CALL UNIFLT(DC,10,C,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c byte-to-fullword
	DC = 5
	CALL UNIFLT(DC,10,A,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c fullword-to-byte
	DC = -5
	CALL UNIFLT(DC,10,E,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ')

c fullword-to-fullword
	DC = 4
	CALL UNIFLT(DC,10,E,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c halfword-to-fullword
	DC = 6
	CALL UNIFLT(DC,10,C,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ')

c fullword-to-halfword
	DC = -6
	CALL UNIFLT(DC,10,E,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ')

c real-to-real
	DC = 7
	CALL UNIFLT(DC,10,P,Q,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) Q
        CALL XVMESSAGE(MSG,' ')
200	FORMAT(' DCODE=',I3)
250     FORMAT(10E10.3)
c real-to-double
	DC = 9
	CALL UNIFLT(DC,10,P,R,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) R
        CALL XVMESSAGE(MSG,' ')

c double-to-double
	DC = 8
	CALL UNIFLT(DC,10,R,S,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) S
        CALL XVMESSAGE(MSG,' ')

c double-to-real
	DC = -9
	CALL UNIFLT(DC,10,R,Q,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
        WRITE(MSG,250) Q
        CALL XVMESSAGE(MSG,' ')

10	IF (NSW.GT.0) THEN
	  IF (A(1).LT.0) GO TO 500 

	  DO I = 1,10 
	    A(I) = -A(I)
	    C(I) = -C(I)
	    E(I) = -E(I)
	    P(I) = -P(I)
	  ENDDO

	  GO TO 1
	ENDIF

	NSW = 9
	GO TO 1

500	RETURN
        END

$!-----------------------------------------------------------------------------
$ create tuniflt.imake
/* Imake file for Test of VICAR subroutine uniflt */

#define PROGRAM tuniflt

#define MODULE_LIST tuniflt.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create tuniflt.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstuniflt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tuniflt
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create uniflt.hlp
1 UNIFLT

  Filtering subroutine.

  Calling Sequence:  UNIFLT( DCODE, N, A, B, NSW)

  Arguments: DCODE  -  input, integer  -  in/output data types
             N  -  input, integer  -  number of elements
             A  -  input, any data type  -  input array
             B  -  output, any data type  -  output array
             NSW  -  input, integer  -  number of weights

  Original Programmer: L. W. Kamp, 21 Feb. 1984
  Original Source Language: Macro
  Current Cognizant Programmer: L. W. Kamp
  Current Source Language: Fortran
  Ported to Unix: D.D. Knight

2 OPERATION

 The input array A is convolved with a filter of |NSW| elements, all
 with unit weight. NSW must be odd. If NSW is positive then the result
 is normalized, if negative, then normalization is suppressed. Output
 elements on the edges are computed by assuming that the first and
 last input elements are repeated indefinitely.  

2 ARGUMENTS

  DCODE (integer) specifies data types:
        =1,   A is byte         B is byte
        =2,   A is halfword     B is halfword
        =3,   A is byte         B is halfword
        =4,   A is fullword     B is fullword
        =5,   A is byte         B is fullword
        =6,   A is halfword     B is fullword
        =7,   A is real*4       B is real*4
        =8,   A is real*8       B is real*8
        =9,   A is real*4       B is real*8

      Negative values -3, -5, -6 and -9 reverse of above.  There is
      no need to reverse the others values because they have the same
      data types.

   N (integer) specifies the number of elements in the input and output
    vectors.

   A is the input vector, containing N elements. Its data type must agree
    with the DCODE specified.

   B is the output vector containing N filtered elements. Its data type
    must agree with the DCODE specified.

   NSW is the number of filter weights. NSW must be odd, but may be positive
    or negative (the latter suppresses normalization).

$ Return
$!#############################################################################
