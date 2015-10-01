$!****************************************************************************
$!
$! Build proc for MIPL module tntflt
$! VPACK Version 1.6, Tuesday, September 14, 1993, 13:39:14
$!
$! Execute by entering:		$ @tntflt
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
$ write sys$output "*** module tntflt ***"
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
$   if F$SEARCH("tntflt.imake") .nes. ""
$   then
$      vimake tntflt
$      purge tntflt.bld
$   else
$      if F$SEARCH("tntflt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tntflt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tntflt.bld "STD"
$   else
$      @tntflt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tntflt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tntflt.com -
	-s tntflt.f -
	-i tntflt.imake -
	-t ttntflt.f ttntflt.imake ttntflt.pdf tsttntflt.pdf -
	-o tntflt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tntflt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***************************************************************************
	SUBROUTINE TNTFLT(DCODE, NPIX, IN, OUT, NSW)
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
	    DIVISOR = (NSW/2+1)**2
	    NSWX = NSW
	ELSE
	    DIVISOR = 1.0
	    NSWX = -NSW
	END IF
	IF (MOD(NSWX,2) .EQ. 0) NSWX = 2*NSWX + 1
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
	    CALL XVMESSAGE('Invalid DCODE passed to TNTFLT', ' ')
	    CALL ABEND
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT1(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to byte filter
C
	BYTE IN(NPIX),OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
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
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C					    		all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to halfword filter
C
	INTEGER*2 OUT(NPIX)
	BYTE IN(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
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
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C					    		all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to fullword filter
C
	INTEGER*4 OUT(NPIX)
	BYTE IN(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
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
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT7(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*4 filter
C
	REAL*4 IN(NPIX),OUT(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT8(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*8 filter
C
	REAL*8 IN(NPIX),OUT(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO            
	    N = SUM/DIVISOR
	    OUT(I) = N
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
	REAL*4 IN(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Halfword to byte filter
C
	INTEGER*2 IN(NPIX)
	BYTE OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Fullword to byte filter
C
	INTEGER*4 IN(NPIX)
	BYTE OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
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
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*4 filter
C
	REAL*8 IN(NPIX),SUM,N
	REAL*4 OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tntflt.imake
/* Imake file for VICAR subroutine tntflt */

#define SUBROUTINE tntflt

#define MODULE_LIST tntflt.f

#define P2_SUBLIB

#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create ttntflt.f
c  test subroutine TNTFLT

        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44

	IMPLICIT INTEGER(A-Z)

	BYTE A(10)/1,2,3,4,5,6,7,8,9,10/, B(10)
	INTEGER*2 C(10)/1,2,3,4,5,6,7,8,9,10/, D(10)
	INTEGER*4 E(10)/1,2,3,4,5,6,7,8,9,10/, F(10)
	REAL*4 P(10)/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./, Q(10)
	REAL*8 R(10), S(10)
        CHARACTER*150 MSG
	NSW = -9

c byte-to-byte
1	DC = 1
	CALL TNTFLT(DC,10,A,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ') 
100	FORMAT(' DCODE=',I3)
150     FORMAT(10I5) 

c byte-to-halfword
	DC = 3
	CALL TNTFLT(DC,10,A,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) C
        CALL XVMESSAGE(MSG,' ') 

c halfword-to-byte
	DC = -3
	CALL TNTFLT(DC,10,C,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ') 

c halfword-to-halfword
	DC = 2
	CALL TNTFLT(DC,10,C,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ') 

c byte-to-fullword
	DC = 5
	CALL TNTFLT(DC,10,A,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ') 

c fullword-to-byte
	DC = -5
	CALL TNTFLT(DC,10,E,B,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) B
        CALL XVMESSAGE(MSG,' ') 

c fullword-to-fullword
        DC = 4
	CALL TNTFLT(DC,10,E,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ') 

c halfword-to-fullword
	DC = 6
	CALL TNTFLT(DC,10,C,F,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) F
        CALL XVMESSAGE(MSG,' ') 

c fullword-to-halfword
	DC = -6
	CALL TNTFLT(DC,10,E,D,NSW)
	WRITE(MSG,100) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,150) D
        CALL XVMESSAGE(MSG,' ') 

c real-to-real
	DC = 7
	CALL TNTFLT(DC,10,P,Q,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,250) Q
        CALL XVMESSAGE(MSG,' ') 
200	format(' DCODE=',I3)
250     FORMAT(10E12.3) 

c real-to-double
	DC = 9
	CALL TNTFLT(DC,10,P,R,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,250) R
        CALL XVMESSAGE(MSG,' ') 

c double-to-double
	DC = 8
	CALL TNTFLT(DC,10,R,S,NSW)
	WRITE(MSG,200) DC
        CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,250) S
        CALL XVMESSAGE(MSG,' ') 

c double-to-real
	DC = -9
	CALL TNTFLT(DC,10,R,Q,NSW)
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

500	END

$!-----------------------------------------------------------------------------
$ create ttntflt.imake
/* Imake file for Test of VICAR subroutine tntflt */

#define PROGRAM ttntflt 

#define MODULE_LIST ttntflt.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create ttntflt.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tsttntflt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
ttntflt 
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create tntflt.hlp
1 TNTFLT

  Filtering subroutine.

  Calling Sequence:  TNTFLT( DCODE, N, A, B, NSW)

  Arguments: DCODE  -  input, integer  -  in/output data types
             N  -  input, integer  -  number of elements
             A  -  input, any data type  -  input array
             B  -  output, any data type  -  output array
             NSW  -  input, integer  -  number of weights

  Original Programmer: L. W. Kamp, 21 Feb. 1984
  Current Cognizant Programmer: L. W. Kamp
  Original Source Language: Macro
  Current Source Language: Fortran  10 Sep 1993
  Ported to Unix: D. D. Knight 10 Sep 1993

2 OPERATION

 The input array A is convolved with a filter of |NSW| elements, with 
 weights of the form:

   1, 2, 3, ..., n-1, n, n-1, ..., 3, 2, 1.  (|NSW| = 2*n+1).

 NSW must be odd. If NSW is positive then the result is normalized, if
 negative, then normalization is suppressed. Output elements on the
 edges are computed by assuming that the first and last input elements
 are repeated indefinitely.  

3 ARGUMENTS

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
      negative values -3, -5, -6 and -9 reverse of above

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
