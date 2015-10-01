$!****************************************************************************
$!
$! Build proc for MIPL module uniflt
$! VPACK Version 1.5, Friday, December 16, 1994, 01:07:12
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
$ write sys$output "*** module uniflt ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
	-i uniflt.imake
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
C	at all points beyond the edge.  NSW is forced to be odd, and not greater
C	than NPIX.
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
	INTEGER DCODE
	LOGICAL*1 IN(NPIX),OUT(NPIX)
C
C					force a valid NSW
	IF (NSW .GT. 0) THEN
	    NSWX = MIN(NSW,NPIX)
	    IF (MOD(NSWX,2) .EQ. 0) NSWX=NSWX-1
	    DIVISOR = NSWX
	ELSE
	    NSWX = MIN(-NSW,NPIX)
	    IF (MOD(NSWX,2) .EQ. 0) NSWX=NSWX-1
	    DIVISOR = 1.0
	END IF
C						call appropriate subroutine
	IF (DCODE .EQ. 1) THEN
	    CALL FLT1(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 2) THEN
	    CALL FLT2(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 3) THEN
	    CALL FLT3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 4) THEN
	    CALL FLT4(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 5) THEN
	    CALL FLT5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 6) THEN
	    CALL FLT6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 7) THEN
	    CALL FLT7(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 8) THEN
	    CALL FLT8(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 9) THEN
	    CALL FLT9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -3) THEN
	    CALL FLTM3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -5) THEN
	    CALL FLTM5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -6) THEN
	    CALL FLTM6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -9) THEN
	    CALL FLTM9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE 
	    CALL XVMESSAGE(' Invalid DCODE passed to UNIFLT',' ')
	    CALL ABEND
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT1(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Byte to byte filter
C
	BYTE IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IV(IN(1))
	DO I=1,IHALF
	    SUM = SUM + IV(IN(1)) + IV(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(1))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IV(IN(NPIX)) - IV(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT2(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLT3(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Byte to halfword filter
C
	INTEGER*2 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IV(IN(1))
	DO I=1,IHALF
	    SUM = SUM + IV(IN(1)) + IV(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IV(IN(NPIX)) - IV(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT4(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLT5(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Byte to fullword filter
C
	INTEGER*4 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IV(IN(1))
	DO I=1,IHALF
	    SUM = SUM + IV(IN(1)) + IV(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IV(IN(I+IHALF)) - IV(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IV(IN(NPIX)) - IV(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT6(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLT7(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLT8(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLT9(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLTM3(IN,OUT,NPIX,NSW,DIVISOR)
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
	    OUT(I) = N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM5(IN,OUT,NPIX,NSW,DIVISOR)
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
	    OUT(I) = N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM6(IN,OUT,NPIX,NSW,DIVISOR)
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
	SUBROUTINE FLTM9(IN,OUT,NPIX,NSW,DIVISOR)
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
#define SUBROUTINE uniflt

#define MODULE_LIST uniflt.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
