$!****************************************************************************
$!
$! Build proc for MIPL module zinterp
$! VPACK Version 1.8, Wednesday, September 27, 1995, 14:16:37
$!
$! Execute by entering:		$ @zinterp
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
$ write sys$output "*** module zinterp ***"
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
$ write sys$output "Invalid argument given to zinterp.com file -- ", primary
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
$   if F$SEARCH("zinterp.imake") .nes. ""
$   then
$      vimake zinterp
$      purge zinterp.bld
$   else
$      if F$SEARCH("zinterp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zinterp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zinterp.bld "STD"
$   else
$      @zinterp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zinterp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zinterp.com -
	-s zinterp.f -
	-i zinterp.imake -
	-p zinterp.pdf -
	-t tstzinterp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zinterp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM "zinterp"

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM ZINTERP
C  PURPOSE ---
C
C	Interpolate elevation values from random control points
C	into a rectangular grid (A "surface" image).
C
C  31 OCT 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C  INPUT ---
C	NLINES	 Depth of grid/picture
C	NSAMPS	 Width of grid/picture
C	FORMAT	 Specify output format (BYTE,HALF,FULL)
C	EXPONENT Interpolation exponent
C	NUMN	 Sample size (Number of neighbors)
C	RADIUS	 Search radius
C	RADEXP	 Search radius expansion
C	LINECOL	 Column in input that contains LINE values
C	SAMPCOL	 Column in input that contains SAMPLE values
C	ZCOL	 Column in input that contains the Z values
C	LSCALE	 Scale factor applied to LINE before adding LOFFSET
C	SSCALE	 Scale factor applied to SAMPLE before adding SOFFSET
C	ZSCALE	 Scale factor applied to the Z value before adding ZOFFSET
C	LOFFSET	 Offset added to LINE after scale factor LSCALE
C	SOFFSET	 Offset added to SAMPLE after scale factor SSCALE
C	ZOFFSET	 Offset added to the Z value after scale factor ZSCALE
C	TRON	 TRace ON value used for N% or N pixels computed
C	TRONMODE TRON mode [ 1 = % /// 2 = N ]
C
C  OUTPUT ---
C	An output "surface" image is generated
C
C  RESTRICTIONS ---
C	NPTS must be >= sample size
C	Sample size must be >= 3
C	Radius must be >= 0
C	Radius expansion must be > 0 if radius > 0
C
C  SUBROUTINES CALLED ---
C	ABEND		 Stops execution and makes a clean exit
C	GETDIST 	 Computes distance between pixel and control pts
C	IBIS_RECORD_READ Reads row of data from interface file
C	GETZ		 Interpolates Z with distance and Z values
C	NEWPTRS		 Rearrange low/high pointers after each line
C	XVMESSAGE	 Print a message to the user
C	IBIS_FILE_READ	 Opens interface file + misc.
C	INDSRTR		 Sort in an in-core array
C	XLADD		 Add comments to history label
C	IBIS_FILE_CLOSE	 Close an IBIS file from within VICAR/TAE
C	IBIS_FILE_OPEN	 Open an IBIS file from within VICAR/TAE
C	XVP		 Parameter acquisition routine
C	XVUNIT		 Get the unit number
C       XVOPEN           Open a file from within VICAR/TAE
C	XVWRITE 	 Writes an array of data to a file
C
C  COMMENTS ---
C
C	It is recommended that control points not form clusters or be
C	positioned in obvious linear allignments (such as gathering
C	control points close together along a contour line from
C	a topographic map). The control points sould be scattered
C	evenly throughout the region of interest.
C
C	The interpolated values are stored in a local cache. The
C	cache consists of two arrays, NHDIST and NHELEV. The
C	interpolation uses these values for computing the Z value.
C
C	Sorting of elevation values by distance takes increasingly
C	more time as the number of control points increases.
C
C	If a radial search is used, the maximum size of the cache is a
C	function of the desired sample size.  The value for NMAX is set
C	to SFACTR * NUMN.
C
C
C  MODE DECLARATIONS ---
	INTEGER MAXNH, SFACTR, BUFSZ, BYTE, HALF, REAL, FULL, DEFAULT
	PARAMETER (MAXNH = 512)
	PARAMETER (DEFAULT = 1)
	PARAMETER (BYTE = 1)
	PARAMETER (HALF = 2)
	PARAMETER (FULL = 3)
	PARAMETER (REAL = 4)
	PARAMETER (SFACTR = 4)
	PARAMETER (BUFSZ = 5000)
	LOGICAL RADIAL, TRON, EXPAND
	CHARACTER*4 FORMAT
	CHARACTER*72 STRING
	INTEGER TRACE, TOTZ, TRMODE, NBUFF, KEY(BUFSZ), NUMVALID
	INTEGER ROW, NMAX, K, IMAX, I, JK, KJ, NNH
	INTEGER RUNIT, OUTUNIT, COLS(3), STATUS, COUNT, TCLIP
	INTEGER NUMN, LCOL, SCOL, ZCOL, NLINES, NSAMPS
	INTEGER IS, IL, NPTS, NCOL, IBIS, RECORD, KEY2(BUFSZ)
	INTEGER BTM, LOW, HIGH, TOP, OBTM, IP(BUFSZ)
        INTEGER OLOW, OHIGH, OTOP, OUTLIST(MAXNH), DKEY2(MAXNH)
	INTEGER INLIST(MAXNH), DKEY(MAXNH), INSIDE, FRINGE, OUTFORM
	REAL TRCOUNT, TRPERC, TRINC, BTMVAL, LOWVAL, HIGHVAL, TOPVAL
	REAL LS(2), ZMIN, ZMAX, EXPONENT, RADIUS, RADEXP, RDSQR, RDXSQR
	REAL LOFF, SOFF, ZOFF, LSF, SSF, ZSF, CUTOFF, CPL(BUFSZ)
	REAL CPS(BUFSZ), CPZ(BUFSZ), DBUFF(BUFSZ), DBUFF2(BUFSZ)
	REAL Z, CPL2(BUFSZ), BAND(10000), CP(3), ZBUFF(BUFSZ)
	REAL NHDIST(MAXNH), NHELEV(MAXNH), NHDIST2(MAXNH)
C
C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

        CALL IFMESSAGE('ZINTERP version 31-OCT-94')
        CALL XVEACTION('SA',' ')

C		Initializations
        RADIAL=.FALSE.

C		Get the size of the output image
C		We will make a rectangle starting
C		at SL=1 SS=1 and ending with NL NS

	CALL XVP ('NL', NLINES, COUNT)
	CALL XVP ('NS', NSAMPS, COUNT)

C		Extract the interpolation parameters

	CALL XVP ('EXPONENT', EXPONENT, COUNT)
	CALL XVP ('NUMN', NUMN, COUNT)
	CALL XVP ('RADIUS', RADIUS, COUNT)
	CALL XVP ('RADEXP', RADEXP, COUNT)
	EXPONENT = EXPONENT / 2.0
	IF (RADIUS.GT.0.0) RADIAL=.TRUE.

C		Open up the IBIS interface file.

        CALL XVUNIT(RUNIT, 'INP', 1, STATUS, ' ')
	CALL IBIS_FILE_OPEN(RUNIT,IBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',NPTS,1,1)

C		Check for too many control points

	IF (NPTS.GT.BUFSZ) GOTO 9100

C		Check for not enough control points

	IF (NPTS.LT.NUMN) GOTO 9300

C		Set the number of nearest neighbors to 5.
C		Also set a default value for RADEXP if none given.

	IF (NUMN.EQ.0) NUMN = 5
	NMAX = NUMN

C		Set up a default radius search if none
C		has been requested (nearest neighbor).
C		This formula assumes even distribution of points.

	IF (.NOT.RADIAL) THEN
	    RADIUS = SQRT(FLOAT(NUMN)/(FLOAT(NPTS)/(NLINES*NSAMPS)))
	ENDIF
	IF (RADEXP.LE.0.0) RADEXP = RADIUS / 3.14159264

C		There should be	three columns given:
C		LINECOL, SAMPCOL, and ZCOL.

	CALL XVP ('LINECOL', LCOL, COUNT)
	CALL XVP ('SAMPCOL', SCOL, COUNT)
	CALL XVP ('ZCOL', ZCOL, COUNT)

C		Check for valid column numbers

	IF (LCOL.GT.NCOL) GOTO 9200
	IF (SCOL.GT.NCOL) GOTO 9200
	IF (ZCOL.GT.NCOL) GOTO 9200

C		Allow for L, S, and Z offsets

	CALL XVP ('LOFFSET',LOFF,COUNT)
	CALL XVP ('SOFFSET',SOFF,COUNT)
	CALL XVP ('ZOFFSET',ZOFF,COUNT)

C		Also allow for L, S, and Z scaling

	CALL XVP ('LSCALE',LSF,COUNT)
	CALL XVP ('SSCALE',SSF,COUNT)
	CALL XVP ('ZSCALE',ZSF,COUNT)

C		A TRace ON would be nice

	CALL XVP ('TRON',TRACE,COUNT)
	CALL XVP ('TRONMODE',TRMODE,COUNT)
	TRON = TRACE.GE.1

C	Read in the Line, Sample and Z values of the control points

	COLS(1) = LCOL
	COLS(2) = SCOL
	COLS(3) = ZCOL
	ZMAX = -1.0E20
        CALL IBIS_RECORD_OPEN(IBIS,RECORD,'FORMAT:REAL',
     &                        COLS,NCOL,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
	DO ROW = 1, NPTS
	    CALL IBIS_RECORD_READ(RECORD, CP, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(IBIS,STATUS,1)
	    CPL(ROW) = (CP(1) * LSF) + LOFF
	    CPS(ROW) = (CP(2) * SSF) + SOFF
	    CPZ(ROW) = (CP(3) * ZSF) + ZOFF
	    IF(CPZ(ROW).GT.ZMAX) ZMAX = CPZ(ROW)
	ENDDO

C		Output format would be appropriate too

	CALL XVP ('FORMAT', FORMAT, COUNT)
	IF (FORMAT(1:4).EQ.'----') THEN
	    OUTFORM = DEFAULT
	ELSEIF (FORMAT(1:4).EQ.'BYTE') THEN
	    OUTFORM = BYTE
	    ZMAX = 255.
	    CUTOFF = 255.
	ELSEIF (FORMAT(1:4).EQ.'HALF') THEN
	    OUTFORM = HALF
	    ZMAX = 32767.
	    CUTOFF = 32767.
	ELSEIF (FORMAT(1:4).EQ.'FULL') THEN
	    OUTFORM = FULL
	    ZMAX = 2147483647.
	    CUTOFF = ZMAX
	ELSEIF (FORMAT(1:4).EQ.'REAL') THEN
	    OUTFORM = REAL
	    ZMAX = 1.0E30
	    CUTOFF = ZMAX
	ENDIF
C		Open the output image

	CALL XVUNIT (OUTUNIT,'OUT',1,STATUS,' ')

C		BYTE output for MAXZ <= 255
C		HALF > 255 <= 32767
C		FULL > 32767

	IF (ZMAX.LE.255) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','BYTE',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a BYTE image.',' ')
	    CALL XVMESSAGE(' ',' ')
	    GOTO 50
	ENDIF

	IF (ZMAX.LE.32767) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','HALF',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a HALFWORD image.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE IF (ZMAX.EQ.2147483647.0) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','FULL',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a FULLWORD image.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a REAL image.',' ')
	    CALL XVMESSAGE(' ',' ')
	END IF

C		Set up for TRON if active

50	IF (TRON) THEN
	    TRPERC = TRACE
	    TRCOUNT = TRACE
	    COUNT = 0
	    TOTZ = 0
	    IF (TRMODE.EQ.1) THEN	! % complete
		TRCOUNT = ( (NLINES*NSAMPS) * (FLOAT(TRACE)/100.) )
	    ELSE			! N pixels computed
		TRPERC = ( (FLOAT(TRACE) / (NLINES*NSAMPS)) * 100.)
	    ENDIF
	    TRINC = TRPERC
	ENDIF

C ----------------------------------------------------------------
C --------------     M A I N    R O U T I N E     ----------------
C ----------------------------------------------------------------

	RDSQR = RADIUS ** 2
	RDXSQR = (RADIUS + RADEXP) ** 2
	TCLIP = 0
	ZMIN = +1.0E20
	ZMAX = -1.0E20

C		Sort the control points in the line direction

	DO I = 1, NPTS
	    KEY (I) = I
	END DO
        CALL MVE(7,NPTS,CPL,CPL2,1,1)
        CALL MVE(7,NPTS,KEY,KEY2,1,1)
	CALL INDSRTR(CPL,IP,NPTS)
        DO K = 1, NPTS
           CPL(K) = CPL2(IP(K))
           KEY(K) = KEY2(IP(K))
        END DO

C		Set up the upper and lower boundaries of a search swath

	BTM = 1
	LOW = 1
	HIGH = 2
	TOP = 2
	LOWVAL = 1. - RADIUS
	BTMVAL = LOWVAL - RADEXP
	HIGHVAL = 1. + RADIUS
	TOPVAL = HIGHVAL + RADEXP

	CALL NEWPNTRS(CPL,BTM,LOW,HIGH,TOP,
     *                BTMVAL,LOWVAL,HIGHVAL,TOPVAL,NPTS)

C			----------  BTM         -----
C						  ^
C						  |
C						RADEXP (DEFAULT - EXPANDS
C						  |     IF NECESSARY)
C						  V
C			----------  LOW         -----
C						  ^
C						  |
C						  |
C						  |
C			 S W A T H	      RADIUS * 2
C						  |
C						  |
C						  |
C						  V
C			----------  HIGH        -----
C						  ^
C						  |
C						RADEXP (DEFAULT - EXPANDS
C						  |     IF NECESSARY)
C						  V
C			----------  TOP         -----

C			Save pointers for restore later

		OBTM = BTM
		OLOW = LOW
		OHIGH = HIGH
		OTOP = TOP

C		Loop through the grid points (pixels)
C		one band (line) at a time ...
C		(from upper left to lower right)

	DO IL = 1, NLINES
	   DO IS = 1, NSAMPS


C			GET the line and sample value
C			for the current pixel

		LS(1) = FLOAT(IS)
		LS(2) = FLOAT(IL)
		NNH = 0
		IMAX = 1

C			Build up a list of squared distances and elevations.

		DO I = LOW, HIGH
		    CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,CPL,CPS,CPZ,
     *				NHDIST,NHELEV)
		END DO

		NBUFF = (HIGH - LOW) + 1	! # Control pts in swath
		EXPAND = .FALSE.		! (Not # within range !)

C			Check to make sure that enough points were found
C			in the swath. If not, increase and check above
C			and below the strip for more points.

510		IF ((NNH.LT.NMAX).OR.(EXPAND)) THEN
		  IF (BTM.EQ.LOW) GOTO 560  ! Don't go below the bottom

		  DO I = BTM, (LOW - 1)
		      CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,
     *				CPL,CPS,CPZ,NHDIST,NHELEV)
		  END DO

		  IF (EXPAND) THEN
		    IF(BTM.NE.OBTM) NBUFF = NBUFF + 1
		  ELSE
		    NBUFF = NBUFF + (LOW - BTM)
		  END IF

560		  IF (HIGH.EQ.TOP) GOTO 580 ! Don't go over the top

		  DO I = (HIGH + 1), TOP
		      CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,
     *				CPL,CPS,CPZ,NHDIST,NHELEV)
		  END DO

		  IF (EXPAND) THEN
		    IF(TOP.NE.OTOP) NBUFF = NBUFF + 1
		  ELSE
		    NBUFF = NBUFF + (TOP - HIGH)
		  END IF
		ENDIF


C		Find all control points within RADIUS
C		and those within RADEXP

580		INSIDE = 0	! Within radius
		FRINGE = 0	! Within radius expansion
		JK = 0
		KJ = 0

		DO I = 1, NNH
		    IF (NHDIST(I).LE.RDSQR) THEN
			INSIDE = INSIDE + 1
			JK = JK + 1
			INLIST(JK)=I
		    ELSE
			IF(NHDIST(I).LE.RDXSQR) THEN
			    FRINGE = FRINGE + 1
			    KJ = KJ + 1
			    OUTLIST(KJ)=I
			ENDIF
		    END IF
		END DO

C			If we still don't have enough points
C			expand the window into the control
C			points and try again.

		NUMVALID = INSIDE + FRINGE

		IF (((NUMVALID.LT.NMAX).AND.(NBUFF.LT.NPTS)) .OR.
     *		    ((NHDIST(IMAX).LT.RDXSQR).AND.(RADIAL))) THEN
C			! Check for no more points
		    IF((BTM.EQ.1).AND.(TOP.EQ.NPTS)) GOTO 600
		    EXPAND = .FALSE.
		    IF(BTM.GT.1)THEN
			LOW=BTM
			IF(LOW.GT.1) THEN
			    BTM=LOW - 1
			    EXPAND=.TRUE.
			END IF
		    END IF
		    IF(TOP.LT.NPTS)THEN
			HIGH=TOP
			IF(HIGH.LT.NPTS) THEN
			    TOP=HIGH+1
			    EXPAND=.TRUE.
			END IF
		    END IF
		    IF(EXPAND)GOTO 510
		END IF

C -----------------------------------------------------------------------

C		    Sort by distance

600		IF (NUMVALID.LT.NMAX) THEN
		    DO I = 1, NNH
			DKEY(I) = I
		    END DO
                    CALL MVE(7,NNH,NHDIST,NHDIST2,1,1)
                    CALL MVE(7,NNH,DKEY,DKEY2,1,1)
		    CALL INDSRTR(NHDIST,IP,NNH)
                    DO K = 1, NNH
                       NHDIST(K) = NHDIST2(IP(K))
                       DKEY(K) = DKEY2(IP(K))
                    END DO
		    DO I = 1, NNH		    ! Stuff 1 through NNH back
			DBUFF(I) = NHDIST(I)	    ! into DKEY because DBUFF
			ZBUFF(I) = NHELEV(DKEY(I))  ! is already sorted. NOTE:
		    END DO			    ! the ELSE sorts DBUFF and
		    DO I = 1, NNH		    ! later refers to DKEY ...
			DKEY(I) = I		    ! so we have to fill DKEY
		    END DO			    ! the ELSE sorts DBUFF and
		    NUMVALID = NNH		    ! compatiblity later ...
		ELSE
		    IF (JK.GT.0) THEN
			DO I=1, JK
			    DBUFF(I) = NHDIST(INLIST(I))
			    ZBUFF(I) = NHELEV(INLIST(I))
			    DKEY(I) = I
			END DO
			NUMVALID = JK
		    END IF
		    IF((JK.LT.NMAX) .AND. (KJ.NE.0)) THEN  ! We need to include
			DO I = 1, KJ                       ! RADEXP points
			    DBUFF(I+JK) = NHDIST(OUTLIST(I))
			    ZBUFF(I+JK) = NHELEV(OUTLIST(I))
			    DKEY(I+JK) = I + JK
			END DO
			NUMVALID = JK + KJ
		    END IF
                    CALL MVE(7,NUMVALID,DBUFF,DBUFF2,1,1)
                    CALL MVE(7,NUMVALID,DKEY,DKEY2,1,1)
		    CALL INDSRTR(DBUFF,IP,NUMVALID)
                    DO K = 1, NUMVALID
                       DBUFF(K) = DBUFF2(IP(K))
                       DKEY(K) = DKEY2(IP(K))
                    END DO
		END IF

C		Establish upper pointer depending on whether we
C		are using nearest neighbor or a search criteria

		IF ((.NOT.RADIAL).AND.(NUMVALID.GT.NMAX)) NUMVALID=NMAX

C		Also check for enough points within RADIUS excluding
C		those points found within RADEXP(s). (Remember that
C		if RADIAL is true then NMAX = NMAX * 4.)

		IF ((RADIAL).AND.(INSIDE.GE.NMAX)) NUMVALID = INSIDE

C			We are finally ready to interpolate the Z

		CALL GETZ(NUMVALID,DBUFF,ZBUFF,DKEY,EXPONENT,Z)

		IF (Z.LT.ZMIN) ZMIN=Z
		IF (Z.GT.ZMAX) ZMAX=Z

C			Prevent Z roundoffs

		IF (OUTFORM.NE.REAL) Z = ANINT(Z)

C			Check to see if Z will fit in the requested
C			output image data format. Clip if need be.	

		IF (OUTFORM.NE.DEFAULT) THEN
		    IF (Z.GT.CUTOFF) THEN
			Z = CUTOFF
			TCLIP = TCLIP + 1
		    END IF			! add lower cuttoff here later 
		END IF

C			Stuff the Z value into the next pixel in the line

		BAND(IS)=Z
C			TRON fights for the user - tell my user what's up

		IF (TRON) THEN
		    COUNT = COUNT + 1
		    IF (TOTZ.EQ.0) THEN
			CALL XVMESSAGE(' ',' ')
			CALL XVMESSAGE
     *                   ('      ++++++++++  TRace ON  ++++++++++',' ')
			CALL XVMESSAGE(' ',' ')
		    ENDIF
		    TOTZ = TOTZ + 1
		    IF (COUNT.GE.TRCOUNT) THEN
			COUNT = 0
			WRITE (STRING,'(A,I6,A,I6,A,I7,F6.1,A)') 
     *                        'Line: ',IL,' Sample: ',
     *			      IS,' Pixel #',TOTZ, TRPERC, '% done'
			CALL XVMESSAGE(STRING,' ')
			TRPERC = TRPERC + TRINC
		    ENDIF
		ENDIF
	    END DO

C		Write out a line at a time

	    CALL XVWRIT (OUTUNIT,BAND,STATUS,' ')

C		Step to the next line ... add 1 to everybody

	    BTMVAL = BTMVAL + 1.0
	    LOWVAL = LOWVAL + 1.0
	    HIGHVAL = HIGHVAL + 1.0
	    TOPVAL = TOPVAL + 1.0

C		    Restore pointers

	    BTM = OBTM
	    LOW = OLOW
	    HIGH = OHIGH
	    TOP = OTOP

C		Recompute the range strip for searching

	    CALL NEWPNTRS (CPL,BTM,LOW,HIGH,TOP,BTMVAL,
     *		LOWVAL,HIGHVAL,TOPVAL,NPTS)

	    OBTM = BTM
	    OLOW = LOW
	    OHIGH = HIGH
	    OTOP = TOP

	END DO

C -----------------------------------------------------------------
C ------------   E N D    O F    M A I N   ------------------------
C -----------------------------------------------------------------

C		Report all vital program statistics

	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE
     *       ('  Interpolated surface generated successfully.',' ')
	CALL XVMESSAGE(' ',' ')

	IF (TCLIP.GT.0) THEN
	    WRITE (STRING,'(A,F8.0,A,I10)')
     *         'Total number of pixels over ',CUTOFF, ': ', TCLIP
	    CALL XVMESSAGE(STRING,' ')
	END IF

C		Chuck info into the history label

	WRITE (STRING,'(A,F7.2,A,F7.2,A,I5)') 'MINZ:', ZMIN, ' MAXZ:',
     *        ZMAX, '   # Control points:', NPTS
	CALL XLADD(OUTUNIT,'HISTORY','COMMENTS',STRING,STATUS,
     *	      'FORMAT','STRING',' ')
	CALL XVMESSAGE(STRING,' ')
	EXPONENT = EXPONENT * 2.0
	WRITE (STRING,'(A,I4,A,F5.1,A,F5.1,A,F4.1)') 'NUMN:', NMAX, 
     *	' RADIUS:', RADIUS, ' RADEXP:', RADEXP, ' EXPONENT:', EXPONENT
	CALL XLADD(OUTUNIT,'HISTORY','PARAMETER',STRING,
     *	STATUS,'FORMAT','STRING',' ')
	CALL XVMESSAGE(STRING,' ')
	CALL XVMESSAGE(' ',' ')

		GOTO 9900

C	********* E R R O R   T R A P S **********

C		Too may control points !!!

9100	CALL XVMESSAGE('Too many control points !!!',' ')
	CALL XVMESSAGE('Maximum # is 3000',' ')
		GOTO 9800

C		Invalid column number given

9200	CALL XVMESSAGE
     *        ('An invalid column number has been requested',' ')
		GOTO 9800

C		Not enough control points given

9300	CALL XVMESSAGE('Not enough control points given !',' ')
     	WRITE (STRING,'(A,I6,A,I5,A)') 'Must be >= ', NUMN, ' Only ', 
     *			NPTS, ' control points found.'
	CALL XVMESSAGE(STRING,' ')
		GOTO 9800

C		Something terrible has happened ... STOP !!!

9800	CALL ABEND

C		Close things up and go home

9900	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
	CALL XVCLOSE (OUTUNIT,STATUS,' ')

        RETURN
	END

C------------------  G E T D I S T  ----------------------------

	SUBROUTINE GETDIST(I,NMAX,LS,NNH,IMAX,KEY,CPL,CPS,CPZ,
     *                     NHDIST,NHELEV)
	IMPLICIT NONE
	INTEGER NNH, IMAX, INH, KEY(*), NMAX, I
	REAL DIST, CPL(*), CPS(*), CPZ(*), LS(2), NHDIST(*), NHELEV(*) 

	DIST = (CPS(KEY(I))-LS(1))**2 + (CPL(I)-LS(2))**2
	IF(NNH.LT.NMAX) THEN
	    NNH = NNH + 1
	    NHDIST(NNH) = DIST
	    NHELEV(NNH) = CPZ(KEY(I))
	    IF(DIST.GT.NHDIST(IMAX)) IMAX = NNH
	ELSE         !The following line has been changed to use 1.00001 to
	    IF(1.00001*DIST .LT. NHDIST(IMAX)) THEN    !breaks ties
		NHDIST(IMAX) = DIST
 		NHELEV(IMAX) = CPZ(KEY(I))
		DO INH = 1, NNH  ! Find the furthest point away
		    IF(NHDIST(INH).GT.NHDIST(IMAX)) IMAX = INH
		END DO
	    END IF
	END IF
	RETURN
	END
C------------------ G E T Z ----------------------

	SUBROUTINE GETZ (NUMVALID,DBUFF,ZBUFF,DKEY,EXPONENT,Z)

	IMPLICIT NONE
	INTEGER NUMVALID, DKEY(*), I, IATCP
	REAL DBUFF(*), ZBUFF(*), EXPONENT, Z
	REAL SUMNUM, SUMDEN, WTDDIST

C		Compute the sums needed for interpolation

	SUMNUM = 0.0
	SUMDEN = 0.0
	IATCP = 0

	DO 100 I = 1, NUMVALID

C		If the current grid point is at one or more
C		control points, then average the control point's
C		Z values

	    IF (DBUFF(I) .LE. 1.0E-12) GO TO 75
	    WTDDIST = DBUFF(I)**EXPONENT
	    IF (WTDDIST .GT. 1.0E-12) GO TO 80
75	    IF (IATCP.EQ.0) THEN
		IATCP = 1
		SUMNUM = 0.0
		SUMDEN = 0.0
	    END IF
	    SUMNUM = SUMNUM + ZBUFF(DKEY(I))
	    SUMDEN = SUMDEN + 1.0
		GO TO 100
80	    IF(IATCP.NE.0) GO TO 100
	    SUMNUM = SUMNUM + ZBUFF(DKEY(I))/WTDDIST
	    SUMDEN = SUMDEN + 1.0/WTDDIST
100	CONTINUE

C	    Calculate Z the value

	Z = SUMNUM / SUMDEN

	RETURN
	END
C----------------  N E W P N T R S  -----------------------

	SUBROUTINE NEWPNTRS (CPL,BTM,LOW,HIGH,TOP,BTMVAL,
     *	LOWVAL,HIGHVAL,TOPVAL,NPTS)

	IMPLICIT NONE
	INTEGER BTM, LOW, HIGH, TOP, J, NPTS
	REAL CPL(*), BTMVAL, LOWVAL, HIGHVAL, TOPVAL

100	IF (CPL(LOW).LT.LOWVAL) THEN
	    LOW = LOW + 1
	ELSE
	    IF (LOW.GT.1)THEN
		LOW = LOW - 1
		GOTO 100
	    END IF
	END IF
	BTM = LOW

	DO J = (LOW - 1), 1, -1
	    IF (CPL(J).LT.BTMVAL) THEN
		BTM = J + 1
		GOTO 300
	    END IF
	END DO

300	IF (CPL(HIGH).GT.HIGHVAL) THEN
	    HIGH = HIGH - 1
	ELSE
	    IF (HIGH.LT.NPTS)THEN
		HIGH = HIGH + 1
		GOTO 300
	    END IF
	END IF
	TOP = HIGH
	DO J = (HIGH + 1), NPTS
	    IF (CPL(J).GT.TOPVAL) THEN
		TOP = J - 1
		GOTO 5000
	    END IF
	END DO

5000	RETURN
	END

C============ E N D === O F === S O U R C E === C O D E ===========

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zinterp.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM zinterp

   To Create the build file give the command:

		$ vimake zinterp			(VMS)
   or
		% vimake zinterp			(Unix)


************************************************************************/


#define PROGRAM	zinterp
#define R2LIB

#define MODULE_LIST zinterp.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create zinterp.pdf
PROCESS		HELP=*
!
! "zinterp" PDF - SURFACE INTERPOLATION FROM RANDOM 3D CONTROL POINTS
!
PARM INP TYPE=(STRING)
PARM OUT TYPE=(STRING)
PARM NL TYPE=INTEGER
PARM NS TYPE=INTEGER
PARM FORMAT TYPE=(STRING,4) VALID=(----,BYTE,HALF,FULL,REAL) DEFAULT="----"
PARM NUMN TYPE=INTEGER VALID=0:32 DEFAULT=5
PARM RADIUS TYPE=REAL VALID=0.0:200.0 DEFAULT=0.0
PARM RADEXP TYPE=REAL VALID=0.0:200.0 DEFAULT=0.0
PARM EXPONENT TYPE=REAL VALID=.001:10.0 DEFAULT=2.0
PARM LOFFSET TYPE=REAL DEFAULT=0.0
PARM SOFFSET TYPE=REAL DEFAULT=0.0
PARM ZOFFSET TYPE=REAL DEFAULT=0.0
PARM LSCALE TYPE=REAL VALID=.0001:999999. DEFAULT=1.0
PARM SSCALE TYPE=REAL VALID=.0001:999999. DEFAULT=1.0
PARM ZSCALE TYPE=REAL VALID=.0001:999999. DEFAULT=1.0
PARM LINECOL TYPE=INTEGER VALID=1:40 DEFAULT=1
PARM SAMPCOL TYPE=INTEGER VALID=1:40 DEFAULT=2
PARM ZCOL TYPE=INTEGER VALID=1:40 DEFAULT=3
PARM TRON TYPE=INTEGER VALID=0:1000000 DEFAULT=0
PARM TRONMODE TYPE=INTEGER VALID=1:2 DEFAULT=1
END-PROC
.HELP

PURPOSE

	Interpolate elevation values from random control points
	into a rectangular grid (A "surface" image).


TAE COMMAND LINE FORMAT

	zinterp INP=3DCPTS.INT OUT=SURFACE.IMG NL=250 NS=300 +
		FORMAT=BYTE LINECOL=1 SAMPCOL=2 ZCOL=3 RADIUS=15.0 +
		RADEXP=10.0 EXPONENT=2.5 NUMN=5 LOFFSET=250 +
		SOFFSET=-50 ZOFFSET=1000 LSCALE=1.5 SSCALE=.75 +
		ZSCALE=.875 TRON=20 TRONMODE=1

.PAGE
EXAMPLES

	zinterp INP=CPTS OUT=SURF NL=150 NS=150 NUMN=10 ZOFFSET=500

	This will create a surface image 150 square. The default
columns in the interface file for the Line, Sample and Z values
of the control points (1,2,3) are employed. The number of nearest
neighbors (NUMN) option is set to 10 which means that for every
pixel in the surface image the 10 closest control points will
influence the determination of the Z value. A value of 500 is added
to the Z value of each control point. Sinze the Z value will exceed
255, the output data set will automatically be set to HALFWORD.

	zinterp INP=CPTS OUT=SURF NL=200 NS=100 RADIUS=25 RADEXP=10

	The same defaults apply to this example with the exception that
the search mode is set to a radial distance from each pixel instead of
nearest neighbor. This distance is set to 25 units away from each pixel.
If not enough points (there is an internal default of 5 points within
radial search mode) are found within the radius, then the radius is
increased by RADEXP (in this case 10) until enough points are found.

	zinterp INP=CPTS OUT=SURF NL=250 NS=250 FORMAT=BYTE +
		LINECOL=14 SAMPCOL=10 ZCOL=1 ZSCALE=1.5 TRON=20

	This example will create a surface image 250 square using the
default value of 5 nearest neighbors for each pixel. The columns that
contain the values of the control points are given explicitely: column
14 - line value; column 10 - sample value; and column 1 - the Z value.
All Z values in the control points will have a scale factor of 1.5
applied before usage. The TRON (TRace ON) option will notify the user
after every 20% of the output image is created. If any Z values exceed
255, they will be set to 255 in the output image since the FORMAT was
requested to be BYTE.

.PAGE
OPERATION

	It is recommended that control points not form clusters or be
	positioned in obvious linear allignments (such as gathering
	control points close together along a contour line from
	a topographic map). Typically, 7 nearest neighbors is
	sufficient to interpolate a synthetic surface of reasonable
	quality. More than 7 will only modify the surface slightly
	at the expence of CPU cycles. The control points sould be
	scattered evenly throughout the region of interest.

	The interpolated values are stored in two locations:
	BUF and a local cache. The cache consists of two arrays,
	NHDIST and NHELEV.  If sufficient points are found in the 
	cache, then interpolation uses just these values.  If 
	the cache is too small, then the elevations in BUF are
	sorted to generate the values for interpolation.

	The cache is used to improve performance in the most common
	cases (large numbers of control points and neighborhood sizes
	of fewer than 100 points). Sorting of elevation values by
	by distance takes increasingly more time as the number of
	control points increases.

	If a radial search is used, the maximum size of the cache is a
	function of the desired sample size.  The value for NMAX is set
	to SFACTR * NUMN.

.PAGE
RESTRICTIONS



	Number of control points must be >= sample size
	Number of nearest neighbors should be around 7 (+-3)
	Maximum number of nearest neighbors is 32
	In the collection of control points, caution should
	be excersized to avoid clustering of control points.

REFERENCE

	See SYMAP USER'S REFERENCE MANUAL (Harvard University Laboratory
	for Computer Graphics and Spatial Analysis, 1977) Section III,
	pages 33 through 36 - Discussion and documentation on the use of
	the SEARCH RADIUS, NUMBER of DATA POINTS (nearest neighbors), and
	the INTERPOLATION GRID.

	See Mark S. Monmonier, COMPUTER-ASSISTED CARTOGRAPHY Principles
	and Prospects (Englewood Cliffs, N.J.: Prentice Hall, Inc., 1982)
	Chapter 3 "Raster Symbols and Surface Mapping" Section titled "GRID
	INTERPOLATION" pages 58 - 65.

HISTORY

        31 OCT 1994  AMS  (CRI)  Made portable for UNIX
        21 NOV 1994  AMS  (CRI)  Added initialization of RADIAL for ANDES

.PAGE
VALID DATA RANGES / DEFAULT VALUES

	VARIABLE	    VALID VALUES	    DEFAULTS 
	========	    ============            ========

	NUMN 		    0 through 32		 5
	EXPONENT 	 .001 through 10.0 		2.0
	LOFFSET 	       any			0.0
	SOFFSET 	       any			0.0
	ZOFFSET 	       any			0.0
	LSCALE		.0001 through 999999. 		1.0
	SSCALE		.0001 through 999999.		1.0
	ZSCALE		.0001 through 999999.		1.0
	LINECOL 	   1 through 40			 1
	SAMPCOL 	   1 through 40			 2
	ZCOL 		   1 through 40			 3
	TRON		       >=1			 0 (off)
	TRONMODE	      1 or 2			 1  (%)

.PAGE

PRECISION:
  The precision of values in fullword output images is limited to six
significant digits because "zinterp" converts all numeric values to real
format before performing computations.  The variation in fullword values that
can be expected on different MIPS-supported machines shall not differ by
more than 1 in six significant digits.
"zinterp" now has a very slight difference from the unported version to
improve portability and agreement of results on different machines.  The 
reason for this change is to handle a probably infrequent case where "zinterp"
encounters a tie for the last place in the set of NUMN points from the input
file used in interpolating a "z" value.  In such a case there are two points
(with different "z" values) competing for inclusion in the set of points used
in the interpolation.  These two points will be equidistant from the output
pixel currently being handled by the program.  To prevent this tie from being
broken unpredictably by floating point round-off and thus varying from
machine to machine, "zinterp" uses a simple tiebreaking scheme that ignores
all but the first point in a tie for last.  ("zinterp" processes the control
points in order of ascending line value.  The tiebreaking scheme can be found
in the source code by searching for the phrase "break ties".) Since in this
case the set of NUMN closest points is poorly defined, the difference from the
unported version (see the test pdf, for example,) is justified and has a
magnitude on the order of the difference caused by increasing NUMN by 1.
.PAGE

.LEVEL1
.VARIABLE INP
IBIS interface file
.VARIABLE OUT
VICAR image
.VARIABLE NL
Number of lines in the
output image.
.VARIABLE NS
Number of samples in the
output image.
.VARIABLE FORMAT
Output format
.VARIABLE LINECOL
Column number in the interface
file that contains the LINE
value.
.VARIABLE SAMPCOL
Column number in the interface
file that contains the SAMPLE
value.
.VARIABLE ZCOL
Column number in the interface
file that contains the Z value.
.VARIABLE NUMN
Number of nearest neighbor 
control points used for the
calculation of each pixel.
.VARIABLE RADIUS
Search distance from current
pixel for finding control
points.
.VARIABLE RADEXP
Increment to increase search
distance in the case of
not enough control points.
.VARIABLE EXPONENT
Value to be applied as
weighting function for
interpolation.
.VARIABLE LOFFSET
Offset value added to LINE
of each control point after
scaling.
.VARIABLE SOFFSET
Offset value added to SAMPLE
of each control point after
scaling.
.VARIABLE ZOFFSET
Offset value added to Z value
of each control point after
scaling.
.VARIABLE LSCALE
Scale factor applied to LINE
value of control point before
adding LOFFSET.
.VARIABLE SSCALE
Scale factor applied to SAMPLE
value of control point before
adding SOFFSET.
.VARIABLE ZSCALE
Scale factor applied to the Z
value of control point before
adding ZOFFSET.
.VARIABLE TRON
Enables TRace ON which reports
information every N% complete
or N pixels computed.
.VARIABLE TRONMODE
  TRON MODE: 
1 = percent computed
2 = TRON values as N
    pixels computed
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstzinterp.pdf
procedure
refgbl $autousage
refgbl $echo
body
 let $autousage="none"
 let _onfail="continue"
 let $echo="yes"

 ibis-gen points nc=3 nr=60
 mf points fun=("c1=index/3","c2=index%20 + 1","c3=(c1*c1 + c2)/2")
 ibis-list points

 zinterp points image nl=20 ns=20 numn=5 tron=10
 list image

 zinterp points image nl=20 ns=20 numn=5 zoffset=300
 list image

 zinterp points image nl=20 ns=20 numn=5 radius=5 zscale=0.5 radex=2
 list image

end-proc
$ Return
$!#############################################################################
