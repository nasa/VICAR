$!****************************************************************************
$!
$! Build proc for MIPL module polyscrb
$! VPACK Version 1.7, Friday, November 25, 1994, 10:04:32
$!
$! Execute by entering:		$ @polyscrb
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
$ write sys$output "*** module polyscrb ***"
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
$ write sys$output "Invalid argument given to polyscrb.com file -- ", primary
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
$   if F$SEARCH("polyscrb.imake") .nes. ""
$   then
$      vimake polyscrb
$      purge polyscrb.bld
$   else
$      if F$SEARCH("polyscrb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polyscrb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polyscrb.bld "STD"
$   else
$      @polyscrb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polyscrb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polyscrb.com -
	-s polyscrb.f -
	-i polyscrb.imake -
	-p polyscrb.pdf -
	-t tstpolyscrb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polyscrb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  POLYSCRB
C#######################################################################
C  NAME OF ROUTINE
C      POLYSCRB  ( POLYgon SCRiBe )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USE FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     AUGUST 1983
C  FOR
C      RON ALLEY and JERRY SOLOMON
C
C  ENVIRONMENT
C      VAX 11/780    VMS 3.0 with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      POLYSCRB INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      POLYSCRB INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      POLYSCRB a b (sl,ss,nl,ns) optional parameters
C            
C       Here 'a' represents the input file name,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name.
C      OUT    - Output file name followed by work file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      HALF   - halfword output flag.
C      DN     - Data number for polygon borders
C      DIM    - Specifies 2 or 3-dimensional graphics-1 file  6/2/88 NDR
C      BDN    - Data number for background areas
C      PDN    - Data number for polygon corners (endpoints of line segments)
C      EXCLOSE- Flag to check for polygons which close exactly
C               when determining the end of sequences of points in the
C               input file.
C      APCLOSE- Distances in checking for polygons which close approximately.
C      OUTSIDE- Flag to expand the polygon border by one pixel
C               around the outside of the polygon.
C      IGNOR  - coordinates of a null point.
C
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The output image can be byte or halfword data.
C      2. Maximum number of samples is 500,000 per line.  The TAE/VICAR 
C         executive may impose further size limitations.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44  (name of top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 takes the data from a standard Graphics-1 vector
C      input file and produces from it an output image file of polygon borders
C      (line segments) that are scribed (drawn) against a uniform
C      background. 
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     AUGUST 1983
C  FOR
C      RON ALLEY and JERRY SOLOMON
C
C  ORIGINAL POLYSCRB PROGRAM BY
C      AL ZOBRIST
C  ENVIRONMENT
C      VAX 11/780    VMS 3.0 with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     8-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C     8-83  SP   INCREASED WORK BUFFER SIZE TO 500000 BYTES.
C     8-83  SP   CORRECTED LABEL PROCESSING OF INPUT FILE BY USING LABELC.
C     8-83  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO INCLUDE CORRECT
C                DATA FORMAT IN LABEL.
C     8-83  SP   IMPLEMENTED BORIS GOKHMAN'S CHANGES TO THE IBM POLYSCRB WHICH
C                ALLOW POLYSCRB TO PRODUCE HALFWORD OUTPUT. ADDED HALF KEYWORD.
C     8-83  SP   CORRECTED INITIALIZATION OF OX2 AND OY2 WHEN IGNOR PARAMETERS
C                ENTERED.
C     8-83  SP   CHANGED REFERENCES TO DN(4), BDN(4), PDN(4) TO  DN(1),
C                BDN(1), PDN(4) BECAUSE VAX STORES LOW-ORDER BYTE IN FIRST
C                BYTE.
C     5-84  SP   USED XVSIZE INSTEAD OF PARAM(2),PARAM(4) FOR SIZE 
C                PARAMETER BECAUSE PARAM DOES NOT KNOW SIZE PARAMETER DOES NOT
C                REFER TO INPUT FILE.
C     6-88  NR   ADDED DIM OPTION FOR SCRIBING 3-DIMENSIONAL FILES
C     1-95  AS   ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM POLYSCRBMAIN.
C      
C  CALLED BY
C      POLYSCRBMAIN
C  SUBROUTINES CALLED
C      The library routines  ABEND, KSCAN, LABELC, OPEN, 
C      PARAM, PRNT, XVMESSAGE, RDCHEK, READ, WCHECK, WRITE.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                 ORIGINAL HEADER COMMENTS.
C
C  IBIS ROUTINE POLYSCRB
C
C  POLYSCRB IS USED TO TRANSFORM A STANDARD POLYGON FILE INTO AN IMAGE
C  FILE OF POLYGON BORDERS.  THE INPUT DATA IN COORDINATE-POINT FORMAT
C  IS USED TO FORM A RASTER BASE CONTAINING POLYGON OUTLINES.  BOTH THE
C  DN VALUES FOR THE POLYGON BORDERS AND THE BACKGROUND AREAS MAY BE
C  SPECIFIED.  PARAMETERS FOR CHAINING AND CLOSURE MAY BE INVOKED.
C
C  USER PARAMETERS:
C
C  DN,N -    SPECIFIES THE DN VALUE WHICH WILL BE USED FOR SCRIBING POLY
C            BOUNDARIES.  THE DEFAULT IS 255.
C  EXCLOSE - INDICATES THAT THE BEGINNING AND ENDING NODES OF EACH POLYG
C            MUST MATCH EXACTLY.
C  APCLOSE,X,Y - THE FLOATING POINT NUMBERS X,Y SPECIFY THE MINIMUM
C            ACCEPTABLE X AND Y DISTANCES IN THE APPROXIMATE POLYGON CLO
C            OPTION.
C  BDN,N -   SPECIFIES THE DN VALUE WHICH WILL BE USED AS BACKGROUND.
C            THE DEFAULT IS ZERO.
C  OUTSIDE - SPECIFIES THAT THE BOUNDARY OF RIGHT HAND CODED POLYGONS AR
C            EXPANDED BY ONE PIXEL.
C  IGNOR,X,Y - CAUSES ALL OCCURENCES OF THE POINT (X,Y) TO BE TREATED AS
C            NULL POINTS.  A NULL POINT INVOKES AUTOMATIC POLYGON CLOSUR
C            THE DEFAULT IS (0.,0.).
C  PDN,P -   SPECIFIES A DN VALUE TO BE PLACED AT THE ENDS OF EACH LINE
C            SEGMENT OR THE CORNERS OF EACH POLYGON.
C
C REVISIONS
C   04 NOV 81   ...ALZ...   FIX KSCAN EXIT FOR NO PARAMS AGAIN
C   10 AUG 81   ...ALZ...   FIX KSCAN EXIT FOR NO PARAMS
C   14 FEB 80   ...ALZ...    REPLACE CALL END BY RETURN
C   2 FEB 79   ...ALZ...    INITIAL RELEASE
C
C                 END OF ORIGINAL HEADER COMMENTS.
C
      IMPLICIT NONE

      INTEGER WORKSIZE_PAR, HWRKSIZE_PAR
      PARAMETER ( WORKSIZE_PAR = 500000 )   ! NUMBER OF BYTES IN WORK BUFFER
      PARAMETER ( HWRKSIZE_PAR = WORKSIZE_PAR/2 )

      REAL ABS,FLOAT,AMIN1,AMAX1,Z
      REAL FXL,FXU,FYL,FYU,LINES,X1,X2,Y1,Y2,X,Y,XD,YD,TST(4),T,T1,T2
      REAL XINC,YINC,TL,TU,XQ,YQ,XFIRST,YFIRST,LEPS,SEPS
      REAL PX,PY,OX2,OY2,SQ,XT,YT,PMJ,PMN,CYCLE,XIGNOR,YIGNOR
      REAL TEMP_REAL(2), maxX, maxY, max
      BYTE WORK,BDN
      LOGICAL INLINE,CONTIN,OUTSID,LPRNT,HALF,NOPRINT,XVPTST,USEZVALUE

      INTEGER   GDIM, IDN, IBDN, IPDN, PTR, WSIZ, COUNT, CLOSR,TEMP_INT
      INTEGER   NUM_VALUES, DEFAULT_USED, ERROR_NUM, SIZE(4), NINT, I
      INTEGER   STATUS, ISL, ISS, NL, NS, SLINE, NLINE, NLINEU, SSAMP
      INTEGER   NSAMP, SPIXEL, NSAMPU, NSAMP4, LU, LD, LL, WSUP, ISC
      INTEGER   JSC, IST, JST, ISZ, JSZ, ISTZ, JSTZ, NSQT, INDEX, LINDEX
      INTEGER   ZINDEX, XBIT, YBIT, F2H_BUF(12), OUT_UNIT
      INTEGER*2 HWORK(HWRKSIZE_PAR),  HDN,  HBDN, HPDN
      LOGICAL   ALL_ZERO_SW, EOF_SW, SAMEPDN
      INTEGER   RDGR, GETGR, SETGR, CLGR

      include 'fortport'

      COMMON /COM/WORK(WORKSIZE_PAR)
      COMMON /COM1/LINES(900)

       ! NOTE THAT ON THE VAX THE FIRST BYTE (NOT THE FOURTH) OF A WORD
       ! IS THE LOW ORDER BYTE.

      DATA XIGNOR,YIGNOR/2*0./,IPDN/255/
      DATA IDN,IBDN/255,0/,CONTIN,OUTSID/2*.FALSE./,CLOSR/-1/
      DATA OX2,OY2,XFIRST,YFIRST/4*0./,SEPS,LEPS/.1,.1/,LPRNT/.TRUE./


C
C=================START OF EXECUTABLE CODE===============================     
C
      CALL IFMESSAGE('POLYSCRB version 2-JAN-95')
      CALL XVEACTION('SA',' ')
C
C  INITIALIZE, GET PARAMETERS, OPEN FILES.
C
      CALL XVTRANS_SET(F2H_BUF,'FULL','HALF',STATUS)
      IF (STATUS.NE.1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')

      WSIZ = WORKSIZE_PAR                ! SIZE OF WORK BUFFER IN PIXELS.
      HALF = .FALSE.

	if (xvptst('HALF')) then
          HALF = .TRUE.
          WSIZ = HWRKSIZE_PAR
        END IF

	CALL XVP ('DIM',GDIM,COUNT)

	call xvparm ('DN', temp_int, num_values, default_used,1)
	USEZVALUE  = (DEFAULT_USED .EQ. 1) .AND. (GDIM .EQ. 3)
	if (.NOT. USEZVALUE) idn = temp_int

	call xvparm ('BDN', temp_int, num_values, default_used,1)
	if (default_used .eq. 0) ibdn = temp_int

        IPDN = IDN
	call xvparm ('PDN', temp_int, num_values, default_used,1)
	SAMEPDN = (DEFAULT_USED .EQ.  1) .AND. (USEZVALUE)
	if (.NOT. SAMEPDN) ipdn = temp_int

	outsid = xvptst('OUTSIDE')

	if (xvptst('EXCLOSE')) closr = 0

	call xvparm ('IGNOR', temp_real, num_values, default_used,2)
	if (default_used .eq. 0) then
	    xignor = temp_real(1)
	    yignor = temp_real(2)
            OX2    = XIGNOR
            OY2    = YIGNOR
        END IF

	call xvparm ('APCLOSE', temp_real, num_values, default_used,2)
	if (default_used .eq. 0) then
	    CLOSR = 1

!	 If one value is supplied for the approximate closure (num_values
!	 = 1), then assign it to the line and sample closure. If a second 
!	 value is supplied, then assign it to the sample closure.
	    if (num_values .ge. 1) then
		leps = temp_real(1)
		seps = temp_real(num_values)
	    endif
	endif

      NOPRINT=XVPTST('NOPRINT')

! Open the input graphics file. We'll open the first output file, call it
! the first graphics file, and we'll use 2 coordinates per point. 
!********************************************************************
!********** UNLESS DIM = 3 ******************************************
!********************************************************************

	status= rdgr(1, 1, GDIM)
        if (status.ne.1) call signalgr(1,status,1)

! Get the SIZE parameters
	call xvparm ('SIZE', size, num_values, default_used,4)
	if (default_used .eq. 0) then
	    isl = size(1)
	    iss = size(2)
	    nl = size(3)
	    ns = size(4)
	else
	    call xvp ('SL', isl, num_values)
	    call xvp ('SS', iss, num_values)
	    call xvp ('NL', nl, num_values)
	    call xvp ('NS', ns, num_values)
	endif

! If number of lines and number of samples are defaulted, and thus have a
! value of 0, then run through the graphics file and get the values that
! will enclose the maximum graphics coordinates. Subtract the starting
! line and starting sample from this maximum to get values for NL and NS
! that will display all of the graphics when SL = SS = 1. Graphics with
! line (x) values less than SL or sample (y) values less than SS will be
! chopped. We'll leave the graphics file after the last coordinate set
! because there's a SETGR call made before the graphics file is re-used. 
	if (nl .le. 0  .or.  ns .le. 0) then
	    maxX = isl
	    maxY = iss
	    eof_sw = .false.
	    do while (.not. eof_sw)		! go thru file until eof
	       	status= getgr(1, all_zero_sw, eof_sw, x, y, Z)
                if (status.ne.1) call signalgr(1,status,1)
		if (.not. all_zero_sw .and. .not. eof_sw) then
		    maxX = max(maxX, x)
		    maxY = max(maxY, y)
		endif
	    enddo

!     Convert from absolute maximums to window widths
	    maxX = maxX - isl + 1
	    maxY = maxY - iss + 1

!     Modify the defaults, rounding the real-valued window widths to integers
	    if (nl .le. 0) nl = nint(maxX)
	    if (ns .le. 0) ns = nint(maxY)
	endif


	sline = isl
	nline = nl
	nlineu = nline + sline - 1

        SSAMP = ISS
        NSAMP = NS

         SPIXEL = SSAMP
         NSAMPU = NSAMP+SSAMP-1
         NSAMP4 = NSAMP+4-MOD(NSAMP,4)  ! NSAMP4 PROVIDES MARGIN FOR SCRIBING 
					! AT BORDERS,ALSO FOR FULLWORD ALIG


! Open the output image file, setting it to BYTE or HALF, depending on 
! whether half is false or true. Use an internal buffer that matches the
! image type so that we can use as big an addressable buffer as possible.

! Set the image type to "IMAGE" to avoid confusing some other programs-NDR.
	call xvunit (out_unit, 'OUT', 1, error_num,' ')
	if (half) then
	    call xvopen (out_unit, error_num, 'U_FORMAT', 'HALF',
     +		 'O_FORMAT', 'HALF', 'U_NL', nline, 'U_NS', nsamp,
     +		 'OP', 'WRITE','TYPE','IMAGE',' ')
	else
	    call xvopen (out_unit, error_num, 'U_FORMAT', 'BYTE',
     +		 'O_FORMAT', 'BYTE', 'U_NL', nline, 'U_NS', nsamp,
     +		 'OP', 'WRITE','TYPE','IMAGE',' ')
	endif

C
C  SET UP ONE 'STRIP' OR 'LAYER' OF IMAGE FOR SCRIBING THE LINE DATA SET.
C  AS MUCH AS CAN BE HELD BY WORK VECTOR.  HERE X VALUES REFER 
C  TO LINE POSITON, AND Y VALUES REFER TO PIXEL POSITION.
C
      LU = SLINE-1
      LD = WSIZ/NSAMP4              ! NUMBER OF LINES IN STRIP.
      WSUP = LD*NSAMP4
      FYL = FLOAT(SPIXEL)-.6        ! LEFT EDGE OF WINDOW.
      FYU = FLOAT(NSAMPU)+.6        ! RIGHT EDGE OF WINDOW.

C  LOOP TO BUILD OUTPUT FILE ONE STRIP AT A TIME.

 100  LL = LU+1                    ! FIRST LINE OF STRIP.
      LU = MIN0(LU+LD,NLINEU)      ! LAST LINE OF STRIP.
      IF (LL.GT.LU)  GOTO  8000    ! ALL DONE IF WE'RE PAST LAST LINE OF IMAGE.

      FXL = FLOAT(LL)-.6           ! TOP OF STRIP.
      FXU = FLOAT(LU)+.6           ! BOTTOM OF STRIP.

      IF ( HALF )   THEN
         CALL XVTRANS(F2H_BUF,IBDN,HBDN,1)
         DO I = 1, HWRKSIZE_PAR    ! FILL BUFFER WITH BACKGROUND PIXELS.
           HWORK(I) = HBDN
         END DO
      ELSE
         BDN=INT2BYTE(IBDN)
         DO I = 1, WORKSIZE_PAR
           WORK(I) = BDN
         END DO
      END IF

! Reposition the input graphics file to the first coordinate set in
! the file
	status= setgr(1, 1)
        if (status.ne.1) call signalgr(1,status,1)

C
C  READ THE ENTIRE LINES DATA SET.  LINES THAT CROSS THE STRIP ARE PROCES
C
      do while (.true.)

      X1 = OX2               ! TERMINATING POINT OF LAST SEGMENT IS
      Y1 = OY2               ! BEGINNING POINT OF THIS SEGMENT.
      status= getgr(1, all_zero_sw, eof_sw, x2, y2, Z) ! coords of next point
      if (status.ne.1) call signalgr(1,status,1)
      if (eof_sw) go to 91

      IF (USEZVALUE) IDN = NINT(Z)
      IF (SAMEPDN) IPDN = IDN

      OX2 = X2	! save coordinates of end of this segment for use as
      OY2 = Y2	! the beginning of the next segment

C  CHECK FOR NULL POINT.

      IF (ABS(X1-XIGNOR)+ABS(Y1-YIGNOR).LE.1.E-6) GO TO 88
      IF (ABS(X2-XIGNOR)+ABS(Y2-YIGNOR).LE.1.E-6) GO TO 88

C   CLOSR = -1   NO CLOSURE OPTION
C            0   EXCLOSE              CHECK FOR POLYGON CLOSURE.
C            1   APCLOSE              CHECK FOR POLYGON CLOSURE.

      IF (CLOSR) 15,8,7
 7    IF (ABS(X1-XFIRST).GT.LEPS) GO TO 9
      IF (ABS(Y1-YFIRST).GT.SEPS) GO TO 9
      GO TO 88
 8    IF (X1.EQ.XFIRST.AND.Y1.EQ.YFIRST) GO TO 88
 9    IF (CONTIN) GO TO 15
      CONTIN = .TRUE.
      XFIRST = X1
      YFIRST = Y1
 15   IF (.NOT.OUTSID) GO TO 16
      PX = Y1-Y2
      PY = X2-X1
      SQ = SQRT(PX*PX+PY*PY)       ! JUST LIKE IN THE ANALYTIC GEOMETRY
      IF (SQ.LE..01) GO TO 16      ! BOOK.
      PX = PX/SQ
      PY = PY/SQ
      X1 = X1+PX                   ! SHIFT LINE SEGMENT ONE PIXEL
      Y1 = Y1+PY                   ! IN PERPINDICULAR DIRECTION.
      X2 = X2+PX		   !        ^ SPELLING -- CHANGE TO "E"
      Y2 = Y2+PY
C  QUICK TEST FOR INTERSECTION
 16   IF ((X1-FXL).LT.0..AND.(X2-FXL).LT.0.) GO TO 89
      IF ((X1-FXU).GT.0..AND.(X2-FXU).GT.0.) GO TO 89
C  MORE COMPLETE TEST FOR INTERSECTION.  ALSO FIND INITIAL SQUARE.

      IF (ABS(X2).GT.1.E6.OR.ABS(Y2).GT.1.E6) GO TO 401  ! CRAZY NUMBERS?

      IF (X1-X2) 19,17,18
 17   IF (Y1-Y2) 19,19,18
 18   X = X1
      X1 = X2                ! SWAP ENDPOINTS SO X2 >= X1
      X2 = X
      Y = Y1
      Y1 = Y2
      Y2 = Y

C  THE STRIP IS A RECTANGLE. FIND WHERE THE LINE THROUGH THE ENDPOINTS
C  INTERSECTS THE LINES THROUGH EACH OF THE FOUR SIDES OF THE STRIP.
C

 19   XD = (X2-X1)+1.141593E-9   ! OBSCURE NUMBERS DESIGNED TO PREVENT
      YD = (Y2-Y1)+1.141593E-9   ! OVERFLOW ON DIVIDE.
      IF ( ABS(YD)  .LE. 1.E-10 )   YD = 1.E-10        ! EXTRA CARE.
      TST(1) = (FXL+.1-X1)/XD
      TST(2) = (FYL+.1-Y1)/YD
      TST(3) = (FXU-.1-X1)/XD
      TST(4) = (FYU-.1-Y1)/YD
      TL = 3.
      TU = -3.
      INLINE = .FALSE.
      DO 4 I=1,4
      X = XD*TST(I)+X1
      Y = YD*TST(I)+Y1
      IF (X.LT.FXL.OR.X.GT.FXU) GO TO 4
      IF (Y.LT.FYL.OR.Y.GT.FYU) GO TO 4
      INLINE = INLINE.OR.(TST(I).GE.0..AND.TST(I).LE.1.)
      TL = AMIN1(TL,TST(I))
      TU = AMAX1(TU,TST(I))
 4    CONTINUE
      IF (TL.GT.2.) GO TO 89
      T = AMAX1(TL,0.)
      TU = AMIN1(TU,1.)
C
C  CHECK FOR LINE SEGMENT BETWEEN POINTS INTERSECTING A SIDE OF THE STRIP
C  (INLINE = .TRUE. ) OR FOR AN ENDPOINT IN THE INTERIOR OF STRIP.
C
      IF (INLINE) GO TO 5
      IF (X1.GE.FXL.AND.X1.LE.FXU.AND.Y1.GE.FYL.AND.Y1.LE.FYU) GO TO 5
      IF (X2.GE.FXL.AND.X2.LE.FXU.AND.Y2.GE.FYL.AND.Y2.LE.FYU) GO TO 5
      GO TO 89
 5    X = XD*T+X1
      Y = YD*T+Y1
      ISC = MAX0(MIN0(INT(X+.5),LU),LL)-LL+1
      JSC = MAX0(MIN0(INT(Y+.5),NSAMPU),SPIXEL)-SPIXEL+1
      XT = XD*TU+X1
      YT = YD*TU+Y1
      IST = MAX0(MIN0(INT(XT+.5),LU),LL)-LL+1
      JST = MAX0(MIN0(INT(YT+.5),NSAMPU),SPIXEL)-SPIXEL+1
      NSQT = MAX0(IABS(IST-ISC),IABS(JST-JSC))
      INDEX = (ISC-1)*NSAMP4+JSC
      LINDEX = (IST-1)*NSAMP4+JST
 
      IF (HALF)  THEN
          CALL XVTRANS(F2H_BUF,IDN,HDN,1)
          CALL XVTRANS(F2H_BUF,IPDN,HPDN,1)
          HWORK(INDEX) = HDN
          IF (T.LT..0001) HWORK(INDEX) = HPDN
      ELSE
          WORK(INDEX) = INT2BYTE(IDN)
          IF (T.LT..0001) WORK(INDEX) = INT2BYTE(IPDN)
      END IF

      IF (NSQT.EQ.0) GO TO 89
      XQ = FLOAT(ISC+LL-1)
      YQ = FLOAT(JSC+SPIXEL-1)
C  FINE ADJUSTMENT OF TRAJECTORY OF LINE.
      XINC = ABS(1./XD)
      YINC = ABS(1./YD)
      XBIT = NSAMP4
      YBIT = 1
      IF (XD.LT.0) XBIT = -XBIT
      IF (YD.LT.0) YBIT = -YBIT
      IF (XINC-YINC) 27,27,28
 27   YBIT = YBIT+XBIT
      YINC = YINC-XINC
      CYCLE = (XINC+YINC)/XINC
      PMN = Y-YQ
      IF (YD.LT.0.) PMN = -PMN
      PMJ = X-XQ
      IF (XD.LT.0.) PMJ = -PMJ
      T1 = XINC*.5
      T2 = YINC*(.5-PMN+PMJ/CYCLE)
      GO TO 29
 28   XBIT = XBIT+YBIT
      XINC = XINC-YINC
      CYCLE = (XINC+YINC)/YINC
      PMN = X-XQ
      IF (XD.LT.0.) PMN = -PMN
      PMJ = Y-YQ
      IF (YD.LT.0.) PMJ = -PMJ
      T1 = XINC*(.5-PMN+PMJ/CYCLE)
      T2 = YINC*.5
 29   CONTINUE
C  THIS LOOP SCRIBES THE LINE
      DO 10 I=1,NSQT
      IF (T2-T1) 11,11,12
 11   INDEX = INDEX+YBIT
      IF (INDEX.GT.WSUP) INDEX = INDEX-NSAMP4

      IF (HALF)  THEN
         CALL XVTRANS(F2H_BUF,IDN,HDN,1)
         HWORK(INDEX) = HDN
      ELSE
         WORK(INDEX) = INT2BYTE(IDN)
      END IF

      T2 = T2+YINC
      GO TO 10
 12   INDEX = INDEX+XBIT
      IF (INDEX.GT.WSUP) INDEX = INDEX-NSAMP4

      IF (HALF)  THEN
         CALL XVTRANS(F2H_BUF,IDN,HDN,1)
         HWORK(INDEX) = HDN
      ELSE
         WORK(INDEX) = INT2BYTE(IDN)
      END IF

      T1 = T1+XINC
 10   CONTINUE
      ISZ = INDEX/NSAMP4+1
      JSZ = INDEX-(ISZ-1)*NSAMP4
      ISTZ = (IST+ISZ)/2
      JSTZ = (JST+JSZ)/2
      ZINDEX = (ISTZ-1)*NSAMP4+JSTZ

      IF (HALF)  THEN
         CALL XVTRANS(F2H_BUF,IDN,HDN,1)
         CALL XVTRANS(F2H_BUF,IPDN,HPDN,1)
         HWORK(ZINDEX) = HDN
         HWORK(LINDEX) = HDN
         IF (TU.GT..9999) HWORK(LINDEX) = HPDN
      ELSE
         WORK(ZINDEX) = INT2BYTE(IDN)
         WORK(LINDEX) = INT2BYTE(IDN)
         IF (TU.GT..9999) WORK(LINDEX) = INT2BYTE(IPDN)
      END IF

      GO TO 89
 88   CONTIN = .FALSE.
      XFIRST = 0.
 89   CONTINUE
 90   CONTINUE
      end do  ! end of loop thorough all line segments in input file
C
C
C  WRITE OUT THE SCRIBED STRIP.
 91   PTR = 1
      DO 20 I=LL,LU

      IF (HALF)  THEN
	  call xvwrit (out_unit, hwork(ptr), error_num,' ')
      ELSE
	  call xvwrit (out_unit, work(ptr), error_num,' ')
      END IF

 20   PTR = PTR+NSAMP4
      GO TO 100

 401  IF(.NOT.NOPRINT) CALL XVMESSAGE('BAD DATA',' ')

! Close the input graphics file and the output image file
	status= clgr(1)
        if (status.ne.1) call signalgr(1,status,1)
	call xvclose (out_unit, error_num,' ')

	CALL ABEND      ! ABNORMAL END. (NO RETURN FROM ABEND.)

C

8000  CONTINUE

! Close the input graphics file and the output image file
	status= clgr(1)
        if (status.ne.1) call signalgr(1,status,1)
	call xvclose (out_unit, error_num,' ')

      IF(.NOT.NOPRINT) CALL XVMESSAGE('POLYSCRB NORMAL TERMINATION',' ')
      RETURN          ! NORMAL END.
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polyscrb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polyscrb

   To Create the build file give the command:

		$ vimake polyscrb			(VMS)
   or
		% vimake polyscrb			(Unix)


************************************************************************/


#define PROGRAM	polyscrb
#define R2LIB

#define MODULE_LIST polyscrb.f

#define FTNINC_LIST fortport

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polyscrb.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM DIM     TYPE=INTEGER  COUNT=1 	 DEFAULT=2	    VALID=(2:3)
!
PARM HALF    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=HALF
!
PARM DN      TYPE=INTEGER  COUNT=1       DEFAULT=255        VALID=(0:255)
PARM BDN     TYPE=INTEGER  COUNT=1       DEFAULT=0          VALID=(0:255)
PARM PDN     TYPE=INTEGER  COUNT=1       DEFAULT=255        VALID=(0:255)
!
PARM EXCLOSE TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=EXCLOSE
PARM APCLOSE TYPE=REAL     COUNT=(1:2)   DEFAULT=0.0
PARM OUTSIDE TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=OUTSIDE
PARM NOPRINT TYPE=KEYWORD  COUNT=(0:1)   DEF= --	    VALID=NOPRINT
PARM IGNOR   TYPE=REAL     COUNT=2       DEFAULT=(0.0,0.0)
!
END-PROC
.TITLE
VICAR/IBIS Program "polyscrb"
.HELP
 PURPOSE:

Program "polyscrb" takes the data from a standard IBIS graphics-1 vector
input file and produces from it an output image file of polygon borders
(line segments) that are scribed (drawn) against a uniform
background.  The input file consists of the coordinates of points in
the output image that are to be connected with line segments.  The output
image that is produced forms a raster base containing polygon outlines.
Program options allow the user to specify both the data number values for the
polygon borders and the background areas.  "polyscrb" is used in conjunction
with programs "polyreg" and "polygeom" to produce a scribed image for input to
program "paint".
.PAGE
 EXECUTION:

The input file consists of the coordinates of sequences of points in
the output image space.  Each sequence of points is to be connected
with line segments in sequential order.  The coordinates of points
are given in floating point (REAL*4) form with the line location first,
followed by the pixel location in the line.  The program rounds 
the coordinate locations to the nearest pixel in the output image space.

The records in the input file consist of an integral number of coordinate
pairs, or in other words, an integral number of points.  Sequences of points
are packed into the records end to end and may cross record boundaries.
The end of a sequence of points can either be marked by a null point or the
end of the input file, or can be implied by means of a polygon closure option.
By default a null point is a point with the coordinates (0.0,0.0).  A null
point can be defined otherwise, though, by means of the IGNOR option.  When
a polygon closure option (EXCLOSE or APCLOSE) is used, the program saves the
coordinates of the first point in a sequence and checks if this point
occurs again exactly (EXCLOSE) or approximately (APCLOSE).  If this point
does occur again, (a closed polygon has been found, and) the sequence
ends with the second occurrence of the point.  When a polygon closure option 
is used, the program interprets either a closed polygon or a null point as
an end of sequence.

In "polyscrb", the size field does not refer to the input file, but instead
determines a window (in the output image space) which will be written
out to the output file.  The program finds the line segments or portions
of line segments that lie within the window and assigns to the pixels along
these segments the data number value for line segments (polygon borders).
All pixels in the window area that are not part of a line segment are
assigned the data number for background areas.

The output image may either be byte or halfword data.  The output
file must be allocated with AL or a similar program before
running "polyscrb".

TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      polyscrb INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      polyscrb INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
      polyscrb a b (sl,ss,nl,ns) optional parameters
      
       Here 'a' represents the input file name,
       'b' represents the output image file name.
.PAGE
EXAMPLES

 1.   polyscrb INP=OLINES OUT=SCRIB SIZE=(600,600,100,360) 'EXCLOSE

      In this example a byte-data image is produced for the window which
      starts at line 600, pixel 600 and which is 100 lines long and 360
      pixels wide.  The program will check for polygons which close
      exactly in determining the end of sequences of points in the
      input file.

 2.   polyscrb INP=OLINES OUT=SCRIBH SIZE=(600,600,100,360) 'EXCLOSE 'HALF

      In this example a halfword-data image is produced for the same window 
      which was used in example 1.
.PAGE

 3.   polyscrb IN.GRA OUT.IMG  DIM=3

      In this example IN.GRA is a 3-dimensional file; the output line
      segments will have the dn values specified by the z-value of the
      initial point.  All other actions are the same.

 4.   polyscrb IN.GRA OUT.IMG  DIM=3  DN = 255

      Specifying the DN value in the case of a 3-dimensional input will
      override the z-value in the graphics file; all polygons will have
      the specified DN value.

RESTRICTIONS
1. The output image can be byte or halfword data.
2. Maximum number of samples is 500,000 per line.  The TAE/VICAR executive
   may impose further size limitations.
.PAGE
 OPERATION:

Because the size of output images can be larger than the size of the
program's work buffer, "polyscrb" builds the output image in strips.
Each strip contains as many (of the remaining) lines of the window area
as will fit in the work buffer.  For each strip, "polyscrb" processes each
line segment one at a time.  When a line segment intersects the strip,
the slope of the segment is used in determining exactly which pixels to
use in scribing the line.

 WRITTEN BY:             A. L. Zobrist                1 Dec 1975

 COGNIZANT PROGRAMMER:   Niles Ritter		      2 Jun 1988

 REVISION:               1                           22 Sep 1978
           AMS (CRI)     Made portable for UNIX       2 Jan 1995

.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name 
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE DIM
2 or 3 dim file?
.VARIABLE HALF
Enter  HALF=HALF (or abbreviate)
for halfword output.
.VARIABLE DN
Data number for polygon borders
.VARIABLE BDN
Data number for background areas
.VARIABLE PDN
Data number for polygon corners
(endpoints of line segments)
.VARIABLE EXCLOSE
Enter to check for
polygons which close exactly
when determining the end of
sequences of points in the
input file.
.VARIABLE APCLOSE
Distances in checking for
polygons which close
approximately.
.VARIABLE OUTSIDE
Enter to expand the polygon
border by one pixel around
the outside of the polygon.
.VARI NOPRINT
Suppresses program messages
.VARIABLE IGNOR
Defines coordinates of a
null point.
.LEVEL2
.VARIABLE HALF
Byte output is the default.
.VARIABLE DN
For 2-D the default data number for polygon borders is 255; for 3-D files
the dn's are set to the z-values, unless DN is explicitly set.
.VARIABLE BDN
The default data number for background areas is 0.
.VARIABLE PDN
The default data number for polygon corners (endpoints of line segments)
is the value for the DN parameter.
.VARIABLE APCLOSE
The APCLOSE distances are entered when the program is to check for
polygons which close approximately (to within the APCLOSE distances)
when determining the end of sequences of points in the input file.
The first value gives the vertical distance in units of lines.  The second
value gives the horizontal distance in units of pixels.  Both values are
real numbers.  The program checks that both the vertical and horizontal
distances are less than or equal to the APCLOSE parameters.  If only one
APCLOSE parameter is entered, the value is used for both horizontal
and vertical distances.
.VARIABLE OUTSIDE
This option is used when the polygon borders specified in the input file
are to be included as part of the interior of the polygons and the actual
borders are to be drawn around the outside of the specified polygon
borders.  If this option is used the points of the polygon must be
listed in the input file in clockwise ('right hand coded') order.
This is because each line segment will be shifted over one pixel to the
left (perpendicularly when looking towards the ending point from the
starting point of the segment).
.VARI NOPRINT
Supresses program messages output to the screen.
.VARIABLE IGNOR
The default coordinates used to denote a null point are (0.0,0.0).
A null point in the input file signifies the end of a sequence of points
to be connected by line segments.  A null point is analogous to a pen-up
command for a plotter.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolyscrb.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM POLYSCRB
!
!      FIRST BUILD A SIMPLE GRAPHICS VECTOR FILE AND SCRIBE IT.
!
ibis-gen POLYA NC=2 NR=5 'ibis-1 'row DATACOL=(1,2)  +
         DATA=(3,1, 3,9, 8,9, 2.5,0, 0,0)
ibis-list POLYA NC=2 NR=5 GR1DIM=2
!
polyscrb INP=POLYA OUT=POLYAO SIZE=(1,1,10,10)
list POLYAO 'ZEROES
!
!   now try out some options
!
ibis-gen POLYB NC=2 NR=11 'ibis-1 'row DATACOL=(1,2)  +
         DATA=(3,11, 3,9, 8,19, 25,19, 0,0, 40,3, 3,40, -1,-2, +
               -1,20, 22.5,19)
ibis-list POLYB NC=2 NR=11 GR1DIM=2
!
polyscrb INP=POLYB OUT=POLYBO SIZE=(5,2,30,19) DN=77 PDN=100 BDN=1   +
                         IGNOR=(-1, -2)
list POLYBO
!
!   try the same polyscrb except build a halfword-format output image.
!
polyscrb INP=POLYB OUT=POLYHO SIZE=(5,2,30,19) DN=77 PDN=100 BDN=1   +
               'HALF    IGNOR=(-1, -2)
list POLYHO
!
!  convert halfword output to byte and compare.
!
cform INP=POLYHO OUT=POLYHBO 
difpic (POLYBO,POLYHBO) POLYHDIF 'MOD
!
!   now try out the EXCLOSE and OUTSIDE options
!
ibis-gen POLYC NC=2 NR=8 'ibis-1 'row DATACOL=(1,2)  +
         DATA=(3,2, 3,9, 8,9, 8,2, 3,2, 4,4.5, 7.6,4.5, 0,0)
ibis-list POLYC NC=2 NR=8 GR1DIM=2
!
ibis-gen POLYD NC=2 NR=8 'ibis-1 'row DATACOL=(1,2)  +
         DATA=(2,1, 2,10, 9,10, 9,1, 2,1, 4,5.5, 7.6,5.5, 0,0)
ibis-list POLYD NC=2 NR=8 GR1DIM=2
!
polyscrb INP=POLYC OUT=POLYCO SIZE=(1,1,10,10) 'OUTSIDE 'EXCLOSE
list POLYCO  'ZEROES
!
polyscrb INP=POLYD OUT=POLYDO SIZE=(1,1,10,10) 'EXCLOSE
list POLYDO  'ZEROES
!
difpic (POLYCO,POLYDO) POLYDIF 'MOD
list POLYDIF
!
!   try the APCLOSE option with a file bigger than the work buffer
!
ibis-gen POLYBIG NC=2 NR=8 'ibis-1 'row DATACOL=(1,2)  +
         DATA=(2,1, 2,510, 1024,510, 1024,1, 2.5,1.5, 4,5.5, +
               7.6,5.5, 0,0)
ibis-list POLYBIG NC=2 NR=8 GR1DIM=2
!
polyscrb INP=POLYBIG OUT=POLYBIGO SIZE=(1,1,1024,512) APCLOSE=.5
list POLYBIGO  SIZE=(1,1,10,10) 'ZEROES
list POLYBIGO  SIZE=(1,503,10,10) 'ZEROES
list POLYBIGO  SIZE=(1015,503,10,10) 'ZEROES
list POLYBIGO  SIZE=(1015,1,10,10) 'ZEROES
!
end-proc
$ Return
$!#############################################################################
