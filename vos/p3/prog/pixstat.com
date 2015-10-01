$!****************************************************************************
$!
$! Build proc for MIPL module pixstat
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:26:52
$!
$! Execute by entering:		$ @pixstat
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
$ write sys$output "*** module pixstat ***"
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
$ write sys$output "Invalid argument given to pixstat.com file -- ", primary
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
$   if F$SEARCH("pixstat.imake") .nes. ""
$   then
$      vimake pixstat
$      purge pixstat.bld
$   else
$      if F$SEARCH("pixstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pixstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pixstat.bld "STD"
$   else
$      @pixstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pixstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pixstat.com -mixed -
	-s pixstat.f -
	-p pixstat.pdf -
	-i pixstat.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pixstat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR PROGRAM PIXSTAT
C
C-------------------------------------------------------------------
C Edit History:
C     5/13/88  TCG  Added SIZE,NL,NS,SL,SS and updated test file
C     5/06/91  REA  Converted to UNIX/VICAR
C-------------------------------------------------------------------
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT INTEGER(A-Z)
	INTEGER*4 TEMP, STRIPSQ(4128,129)
	INTEGER*2 OUTBUF(4000), STRIP(4128,129)
        REAL*8 SUM, SUMSQ, MEAN, MOM2, SDEV, VARI, COL(2128)
	REAL*8 COLSQ(4128)
	REAL SCALE, OFFSET
	CHARACTER*4 FORM
	LOGICAL XVPTST
C
	CALL XVUNIT(IUNIT,'INP',1,STATUS,' ')
	CALL XVUNIT(OUNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN(IUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT','SA',
     +		    'IO_ACT','SA',' ')
	CALL XVOPEN(OUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +		    'IO_ACT','SA','U_FORMAT','HALF',' ')
	CALL XVP('NLW',NLW,ICOUNT)
	CALL XVP('NSW',NSW,ICOUNT)
	IF(((NLW/2)*2 .EQ. NLW) .OR.((NSW/2)*2 .EQ. NSW)) THEN
	    CALL XVMESSAGE('ERROR--WINDOW DIMENSIONS MUST BE ODD',' ')
	    CALL ABEND
	ENDIF
	CALL XVP('OFFSET',OFFSET,ICOUNT)
	CALL XVP('SCALE',SCALE,ICOUNT)
	CALL XVSIZE( SL, SS, NL, NS, NLIN, NSIN )
	IF(NS .GT. 4000) THEN
	    CALL XVMESSAGE(
     +	       'Input image must be no greater than 4000 samples wide.',
     +		' ')
	    CALL ABEND
	ENDIF
	HLFNSW = NSW / 2
	HLFNLW = NLW / 2
	NPIXW = NLW * NSW
	IMGLINE = 0
	STRIPINDX = 1
	OUTINDX = 1
	EDGSTRT = 65 - HLFNSW
	IMGEND = 64 + NS
	NSX2 = (NS + (NSW - 1)) * 2		! # BYTES IN A STRIP LINE
	NSX4 = NSX2 * 2				! # BYTES IN A STRIPSQ LINE
	BOTTOM = NL - HLFNLW
	RTWNDO = 65 + HLFNSW
	ADDSMPL = RTWNDO + 1
	EDGEND = IMGEND + HLFNSW
	LASTINDX = (NL - (NL / NLW) * NLW) + (HLFNSW - 1)
	LASTINDX = (LASTINDX - (LASTINDX / NLW) * NLW) + 1
C
	CALL XVREAD(IUNIT,STRIP(65,1),STATUS,' ')	! LINE #1
C
	DO I = EDGSTRT, 64				! PAD LEFT EDGE
	  STRIP(I,1) = STRIP(65,1)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND			! PAD RIGHT EDGE
	  STRIP(I,1) = STRIP(IMGEND,1)
	ENDDO
C
	DO I = 2, HLFNLW + 1				! FILL TOP EDGE
	  CALL MVL(STRIP(EDGSTRT,1),STRIP(EDGSTRT,I),NSX2)
	ENDDO
C
	DO I = HLFNLW + 2, NLW			! LOAD REST OF STARTING LINES
	  CALL XVREAD(IUNIT,STRIP(65,I), STATUS, ' ')
	  DO J = EDGSTRT, 64				! PAD LEFT EDGES
	    STRIP(J,I) = STRIP(65,I)
	  ENDDO
	  DO J = IMGEND + 1, EDGEND			! PAD  RIGHT EDGES
	    STRIP(J,I) = STRIP(IMGEND,I)
	  ENDDO
	ENDDO
C
	CALL XVGET(IUNIT, STATUS, 'FORMAT', FORM, ' ')
	IF(FORM .EQ. 'HALF') GO TO 99
C
   	IF(XVPTST('MEAN')) GO TO 21
C
	DO I = EDGSTRT, EDGEND			! INIT. SQUARES BUFFER
	  DO J = 1, NLW
	    TEMP = STRIP(I,J)
	    STRIPSQ(I,J) = TEMP**2
	  ENDDO
	ENDDO
C
	IF(XVPTST('MOMENT')) GO TO 41
	IF(XVPTST('VARIANCE')) GO TO 61
C
  1	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
  2	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	VARI = MOM2 - MEAN**2
	SDEV = DSQRT(VARI) * SCALE + OFFSET
C
	OUTBUF(OUTINDX) = MIN(255, MAX(NINT(SDEV), 0))	! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 10		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 2				! CRUNCH NEXT PIXEL
C
 10	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) THEN		! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
 	  CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),
     *		   NSX2)
	  CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  	   NSX4)
	ELSE
C						! READ AND PAD NEWLINE
	  CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	  DO I = EDGSTRT, 64			! PAD LEFT EDGE
	    STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	  ENDDO
C
	  DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	    STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	  ENDDO
C
	  DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	    TEMP = STRIP(I,STRIPINDX)
	    STRIPSQ(I,STRIPINDX) = TEMP**2
	  ENDDO
	END IF
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 1
C
C
C
C		code for calculation of mean only
C
C
C
C
 21	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 22	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MEAN = MIN(255, MAX(0, NINT(SCALE*MEAN+OFFSET)))
C
	OUTBUF(OUTINDX) = MEAN			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 30		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	COL(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 22				! CRUNCH NEXT PIXEL
C
 30	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 35      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 21
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 35	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 21
C
C
 41	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 42	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	MOM2 = MIN(255, MAX(0, NINT(SCALE*MOM2+OFFSET)))
C
	OUTBUF(OUTINDX) = MOM2			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 50		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 42				! CRUNCH NEXT PIXEL
C
 50	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 55      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
	DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	  TEMP = STRIP(I,STRIPINDX)
	  STRIPSQ(I,STRIPINDX) = TEMP**2
	ENDDO
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 41
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 55	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  NSX4)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 41
C
C
 61	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 62	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	VARI = MOM2 - MEAN**2
	VARI = MIN(255, MAX(0, NINT(SCALE*VARI+OFFSET)))
C
	OUTBUF(OUTINDX) = VARI			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 70		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 62				! CRUNCH NEXT PIXEL
C
 70	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 75      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
	DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	  TEMP = STRIP(I,STRIPINDX)
	  STRIPSQ(I,STRIPINDX) = TEMP**2
	ENDDO
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 61
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 75	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  NSX4)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 61
C
   99	IF(XVPTST('MEAN')) GO TO 2100

C
	DO I = EDGSTRT, EDGEND			! INIT. SQUARES BUFFER
	  DO J = 1, NLW
	    TEMP = STRIP(I,J)
	    STRIPSQ(I,J) = TEMP**2
	  ENDDO
	ENDDO
C
	IF(XVPTST('MOMENT')) GO TO 4100
	IF(XVPTST('VARIANCE')) GO TO 6100
C
  100	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
  200	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	VARI = MOM2 - MEAN**2
	SDEV = DSQRT(VARI) * SCALE + OFFSET
C
	OUTBUF(OUTINDX) = MIN(32767, MAX(-32768, NINT(SDEV)))	! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 1000		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 200				! CRUNCH NEXT PIXEL
C
 1000	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 1500      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
	DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	  TEMP = STRIP(I,STRIPINDX)
	  STRIPSQ(I,STRIPINDX) = TEMP**2
	ENDDO
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 100
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 1500	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  NSX4)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 100
C
C
C		code for calculation of mean only
C
C
C
C
 2100	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 2200	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MEAN = MIN(32767, MAX(-32768, NINT(SCALE*MEAN+OFFSET)))
C
	OUTBUF(OUTINDX) = MEAN			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 3000		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	COL(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 2200				! CRUNCH NEXT PIXEL
C
 3000	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 3500      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 2100
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 3500	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 2100
C
C
 4100	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 4200	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	MOM2 = MIN(32767, MAX(-32768, NINT(SCALE*MOM2+OFFSET)))
C
	OUTBUF(OUTINDX) = MOM2			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 5000		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 4200				! CRUNCH NEXT PIXEL
C
 5000	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 5500      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
	DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	  TEMP = STRIP(I,STRIPINDX)
	  STRIPSQ(I,STRIPINDX) = TEMP**2
	ENDDO
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 4100
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 5500	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  NSX4)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 4100
C
C
 6100	NXTSMPL = ADDSMPL
	LSTSMPL = EDGSTRT
	IMGLINE = IMGLINE + 1
	SUM = 0.0				! INIT. SUMS FOR START OF LINE
	SUMSQ = 0.0
C
	DO I = EDGSTRT, RTWNDO			! CLEAR COLUMN SUMS
	  COL(I) = 0.0
	  COLSQ(I) = 0.0
	ENDDO
C
	DO J = 1, NLW				! SUM UP WINDOW AND ITS COLUMNS
	  DO I = EDGSTRT,RTWNDO
	    SUM = SUM + STRIP(I,J)
	    SUMSQ = SUMSQ + STRIPSQ(I,J)
	    COL(I) = COL(I) + STRIP(I,J)
	    COLSQ(I) = COLSQ(I) + STRIPSQ(I,J)
	  ENDDO
	ENDDO
C
	M = 64					! INIT SAMPLE INDEX
C
 6200	M = M + 1				! CALC SDEV
	MEAN = SUM / NPIXW
	MOM2 = SUMSQ / NPIXW
	VARI = MOM2 - MEAN**2
	VARI = MIN(32767, MAX(-32768, NINT(SCALE*VARI+OFFSET)))
C
	OUTBUF(OUTINDX) = VARI			! OUTPUT PIXEL
        OUTINDX = OUTINDX + 1
C
	IF(M .GE. IMGEND) GO TO 7000		! END OF LINE ?
C
	SUM = SUM - COL(LSTSMPL)		!SET UP SUMS FOR NEXT PIXEL
	SUMSQ = SUMSQ - COLSQ(LSTSMPL)
	COL(NXTSMPL) = 0.0
	COLSQ(NXTSMPL) = 0.0
	DO I = 1, NLW
	  COL(NXTSMPL) = COL(NXTSMPL) + STRIP(NXTSMPL,I)
	  COLSQ(NXTSMPL) = COLSQ(NXTSMPL) + STRIPSQ(NXTSMPL,I)
	ENDDO
	SUM = SUM + COL(NXTSMPL)
	SUMSQ = SUMSQ + COLSQ(NXTSMPL)
	NXTSMPL = NXTSMPL + 1
	LSTSMPL = LSTSMPL + 1
	GO TO 6200				! CRUNCH NEXT PIXEL
C
 7000	CALL XVWRIT(OUNIT,OUTBUF,STATUS,' ')
	OUTINDX = 1
	IF(IMGLINE .GE. NL) GO TO 9999		! EXIT CONDITION
C
	IF(IMGLINE .GE. BOTTOM) GO TO 7500      ! GOING OFF BOTTOM EDGE 
C						! NO MORE INPUT
C
C						! READ AND PAD NEWLINE
	CALL XVREAD(IUNIT, STRIP(65,STRIPINDX),STATUS,' ')
C
	DO I = EDGSTRT, 64			! PAD LEFT EDGE
	  STRIP(I,STRIPINDX) = STRIP(65,STRIPINDX)
	ENDDO
C
	DO I = IMGEND + 1, EDGEND		! PAD RIGHT EDGE
	  STRIP(I,STRIPINDX) = STRIP(IMGEND,STRIPINDX)
	ENDDO
C
	DO I = EDGSTRT, EDGEND			! ENTER NEW SQUARES
	  TEMP = STRIP(I,STRIPINDX)
	  STRIPSQ(I,STRIPINDX) = TEMP**2
	ENDDO
C
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 6100
C
C
C		NO MORE INPUT. REPEAT LAST LINE 
C
 7500	CALL MVL(STRIP(EDGSTRT,LASTINDX),STRIP(EDGSTRT,STRIPINDX),NSX2)
	CALL MVL(STRIPSQ(EDGSTRT,LASTINDX),STRIPSQ(EDGSTRT,STRIPINDX),
     *  NSX4)
	STRIPINDX = STRIPINDX + 1
	IF(STRIPINDX .GT. NLW) STRIPINDX = 1
	GO TO 6100
C

C
C       CLOSE DATA SETS
C
 9999	CALL XVCLOSE(IUNIT,STATUS,' ')
	CALL XVCLOSE(OUNIT,STATUS,' ')
C
	RETURN
 	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create pixstat.pdf
PROCESS help=*
  PARM INP TYPE=(STRING,60)
  PARM OUT TYPE=(STRING,60)
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM SL TYPE=INTEGER DEFAULT=1
  PARM SS TYPE=INTEGER DEFAULT=1
  PARM NL TYPE=INTEGER DEFAULT=0
  PARM NS TYPE=INTEGER DEFAULT=0
  PARM CALC TYPE=KEYWORD COUNT=(0:1) VALID=(MEAN,SDEV,VARIANCE,MOMENT) +
    DEF=SDEV
  PARM NLW TYPE=INTEGER DEF=11 VALID=(3:129)
  PARM NSW TYPE=INTEGER DEF=11 VALID=(3:129)
  PARM SCALE TYPE=REAL DEF=1.0
  PARM OFFSET TYPE=REAL DEF=0.0
END-PROC
.TITLE 
VICAR Program PIXSTAT
.HELP

PURPOSE

	PIXSTAT is a VICAR applications program for calculating statistical
	quantities in local areas surrounding each pixel in an input image.
	The local mean, second moment, variance and standard deviation are
	quantities that may be obtained.

.PAGE
EXECUTION FORMAT

	PIXSTAT IN OUT [NLW,NSW,SCALE,OFFSET,CALC]

	where

	IN	is the input data set (VICAR labeled image).

	OUT	is the output image of the selected statistic.

	NLW	is the number of lines in the local area window.
		This number must be odd to insure a center line.
		The default value is 11. The maximum value is 129

	NSW	is the number of samples in the local area window.
		This number must be odd to insure a center sample.
		The default value is 11. The maximum value is 129

	SCALE	is the scaling factor that each output pixel will be
		multiplied by. The default value is 1

	OFFSET	is the constant which is added to each scaled pixel
		befor it is output. The default value is 0

	CALC    is a keyword selecting the statistic to be calculated.
		'MEAN, 'MOMENT, 'VARIANCE and 'SDEV (standard deviation)
		are the valid selections. 'SDEV is the default selection.

		All numerical parameters are integers.
.PAGE
OPERATION

	PIXSTAT performs a sliding window statistical analysis on an input
	image. An output pixel's position represents the center position of
	the window for the input image and its value represents statistics
	based on data within the window only. The window moves along one sample
	at a time and one line at a time until each pixel in the input has
	been at the center of the window. In other words statistics are 
	compiled for the local area around each and every pixel in the input
	image.

	The edge conditions are handled as follows. Any window positions that
	extend beyond the edge of the image are handled by using data from the
	nearest pixel on the image. This is not a reflection scheme like some
	sliding window algorithms have.

	Half word input results in half word output. Byte input results in
	byte output. This is automatic and requires no specification by the 
	user. Half word values that exceed the half word maximum integer value
	(32767) are set to 32767. Similarly byte values that exceed 255 are
	set to 255. Input images must have less than 4001 samples. There is
	no limit to the number of lines in the input.
.PAGE
EQUATIONS

	MEAN
		The mean is a result of the sum of all DN values in the 
		window divided by the number of pixels in the window.
		(the average of the DNs)
			WINDOWSUM / NPIXWNDO




	SECOND MOMENT
		The second moment is a result of the sum of the squares 
		of all DN values in the window divided by the number of
		pixels in the window. (the average of the squares of the DNs)

			DNSQRSUM / NPIXWNDO

	VARIANCE
		The variance is the difference between the second moment
		and the square of the mean.(average square - average squared)

			SECOND MOMENT - MEAN ** 2

	STANDARD DEVIATION
		The standard deviation is the square root of the variance.

			VARIANCE ** 0.5


Original Programmer:	Leo Bynum    March 1985
Cognizant Programmer:	Leo Bynum 
Revision		3	     May 1988

.LEVEL1
.VARI INP
INPUT IMAGE FILE
.VARI OUT
OUTPUT IMAGE FILE
.VARI NLW
NUMBER OF LINES IN THE WINDOW
OVER WHICH THE CALCULATION
WILL BE PREFORMED (ODD)
.VARI NSW
NUMBER OF SAMPLES IN THE WINDOW
OVER WHICH THE CALCULATION
WILL BE PREFORMED (ODD)
.VARI SCALE
SCALE FACTOR BY WHICH EACH PIXEL
WILL BE MULTIPLIED
.VARI OFFSET
OFFSET ADDED TO EACH PIXEL
TO BE OUTPUT
.VARI CALC 
OUTPUT VALUES WILL REPRESENT
ONE OF THE FOLLOWING:
MEAN, SDEV, MOMENT, VARIANCE
.VARI NL
Number of input lines
.VARI NS
Number of input samples 
(Maximum 4000)
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI SIZE
Standard VICAR size field
.LEVEL2
.VARI	IN
File name to be used as the input
data set (VICAR labeled image).
.VARI   OUT
File name to be used as the output
data set (VICAR labeled image).

.VARI	NLW
The number of lines in the local area window.
This number must be odd to insure a center line.
The default value is 11. The maximum value is 129

.VARI	NSW
The number of samples in the local area window.
This number must be odd to insure a center sample.
The default value is 11. The maximum value is 129

.VARI	SCALE
The scaling factor that each output pixel will be
multiplied by. The default value is 1

.VARI	OFFSET
The constant which is added to each scaled pixel
befor it is output. The default value is 0

.VARI	CALC
A keyword selecting the statistic to be calculated.
'MEAN, 'MOMENT, 'VARIANCE and 'SDEV (standard deviation)
are the valid selections. 'SDEV is the default selection.
.VARI	SIZE
The size parameter determines the boundaries in the input
file on which the statistics are to be gathered.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.VARI NL
Number of input lines
.VARI NS
Number of input samples
.VARI SL
Starting line
.VARI SS
Starting sample
.END
$ Return
$!#############################################################################
$Imake_File:
$ create pixstat.imake
#define  PROGRAM   pixstat

#define MODULE_LIST pixstat.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
