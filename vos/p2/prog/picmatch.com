$!****************************************************************************
$!
$! Build proc for MIPL module picmatch
$! VPACK Version 1.9, Tuesday, June 02, 1998, 13:52:53
$!
$! Execute by entering:		$ @picmatch
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
$ write sys$output "*** module picmatch ***"
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
$ write sys$output "Invalid argument given to picmatch.com file -- ", primary
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
$   if F$SEARCH("picmatch.imake") .nes. ""
$   then
$      vimake picmatch
$      purge picmatch.bld
$   else
$      if F$SEARCH("picmatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake picmatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @picmatch.bld "STD"
$   else
$      @picmatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create picmatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack picmatch.com -
	-s picmatch.f -
	-i picmatch.imake -
	-p picmatch.pdf -
	-t tstpicmatch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create picmatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C......REVISION HISTORY
C
C   5-95  CRI   MSTP Software Conversion (VICAR PORTING)
C   6-87  KFE   Change output tiepoint format from IBIS graphics-1 file
C		to IBIS interface (tabular) file.
C
C   1-87  SXP   Modified to use LLSQ to solve linear least 
C               squares system of equations.
C   9-86  SXP   Modified to access the array processor if it is present unless
C               'NOAP is specified.  Replaced body of RFIT subroutine with
C               call to CROSSCAP for AP execution or CROSSCORR for non-AP
C               execution.  Replaced NOPHASE parameter with more flexible
C               PHASE parameter.  Added NOAP parameter in pdf to make usage 
C               of AP selectable.  Modified calling sequence for RFIT
C               subroutine for PHASE and USEAP.  Removed POWER from RFITCOM.
C               The handling of the case where the peak of the cross-correlation
C               matrix is close to the edge of the matrix is different now,
C               presumably better.  In this case the previous code could 
C               report a peak where there was none.  (See OFFMAX parameter
C               in help for subroutine CROSSCORR.)  
C               The handling of the HPF is a little different now.  Before
C               some terms in the FFTs were not zeroed because they were
C               already 0 theoretically.  Now they are explicitly zeroed
C               in case they were not zero due to roundoff.  The normalization
C               of the correlation for phase with HPF used to divide by
C               32**2.  It now divides by 31**2 since the axes should not be 
C               counted with HPF.
C               The test pdf has been expanded.

C               USERS WILL PROBABLY NOTICE MORE TIEPOINTS BEING REJECTED WITH A 
C               FAILURE CODE OF 4 (CORRELATION PEAK UNRELIABLE) AND A 
C               CORRELATION VALUE OF 0.  THIS IS DUE TO
C               THE DIFFERENCE INVOLVING THE OFFMAX PARAMETER MENTIONED ABOVE.

C   5-86  KFE   General upgrade:
C		  Put in comments, improve code legibility.
C		  Change items output.
C		  Change contour file input from graphics-2 to graphics-1
C		  Add FILTER parameter.
C		  Add STOP parameter.
C		  Update/improve help file.
C
C--------------------------------------------------------------------------
C
C  IBIS PROGRAM PICMATCH: REGISTERS AN IMAGE PAIR ALONG GRAPHICS LINES OR
C	    ON A GRID.
C       THE REGISTRATION SQUARES ARE SAMPLED FROM THE IMAGES TO GIVE 
C	    THE BEST AFFINE FIT PRIOR TO FFT
C
C 
	IMPLICIT INTEGER*4(A-Q),REAL*4(R-Z)
        INCLUDE 'fortport'

	REAL*4 A(60,3),AA(180),B(60,2),BB(180),X(3,3),AUX(6),CENTR(2)
	REAL*4 PRED(2),CORR(3,3),SHIFT(2),GCNTR(2),AG(3,3),BG(3,2)
        REAL*4 XG(3,2),XID(3,2),RMAG(2),RVEC(10)
	REAL*8 DSUM, SECCOORD(3)
 	REAL*4 FILTER, STOPTHRESH, PHASE

	INTEGER	MAXOUTROWS, MAXOUTCOLS
	PARAMETER (MAXOUTROWS = 5000, MAXOUTCOLS = 14)

	INTEGER	OUTPTR, CLEN, NCOL, COLS(MAXOUTCOLS)
	INTEGER STOPNUM, STOPCOUNT, IBIG
	INTEGER	RUNIT1, RUNIT2, RUNIT3, RUNIT4, STATUS, WUNIT1
	INTEGER SEARCH,SRCHW, II, JJ, RDGR, GETGR, NEXTGR, CLGR

	LOGICAL CONTUR, RETRY, OLDFFT
	LOGICAL XVPTST, NOPRNT, NOCO, NOHPF, SUBPIX
	LOGICAL NOIN/.FALSE./

	CHARACTER*80  PRSTRING

        REAL    OBUF(MAXOUTROWS)
	REAL	OUTBUF(MAXOUTCOLS,MAXOUTROWS)
	REAL	RPARM(3)
	REAL	CONL,CONS, CONL1,CONS1, CONL2,CONS2, CONDL,CONDS
	REAL	CONSEG, CONDELTA, CONLEN, CONMAX

	LOGICAL	EOF, ZERO



	BYTE CHIP1(32,32), ASRCH(128,128)
	BYTE CHIP2(32,32), ASRCH2(128,128) ! for the masks

	REAL	FFTCORR(32,34)
	COMMON /RFITCOM/  CHIP1, ASRCH,  FFTCORR
	COMMON /RFITCOM2/ CHIP2, ASRCH2

        integer inpcount
        common /inputs/ inpcount


	INTEGER GETGRV(4000)
	DIMENSION IPIV(20)
	DATA XID/1.,0.,0.,0.,1.,0./
	DATA AG,BG/1.,1.,2.,1.,2.,2.,5*1.,2.,1.,2.,2./
	DATA RVEC/0.,1.,-1.,1.,-1.,0.,1.,-1.,-1.,1./
	DIMENSION RITIE(20),ROTIE(20),CVEC(3)



C		Get the input parameters
        CALL IFMESSAGE('PICMATCH version 30-SEPT-96')

	CALL XVPARM('ITIE',RITIE,CITIE,PDEF,20)
	CALL XVPARM('OTIE',ROTIE,COTIE,PDEF,20)


        CALL XVP ('MAGNIFY', RMAG, N)
        IF (N.EQ.1) RMAG(2) = RMAG(1)
        CALL XVP ('SEARCH', SEARCH, DUMMY)
        SEARCH = MIN0(128,SEARCH)
        CALL XVP ('MSEARCH', MSRC, DUMMY)
	CALL XVP ('MINCORR', RMCOR, DUMMY)
        CALL XVP ('ZWIND', GETW, DUMMY)
        CALL XVP ('ZREJECT', CVEC, DUMMY)
	GETR = CVEC(1)
	GETZ = CVEC(2)
        NOHPF = XVPTST('NOHPF')
        SUBPIX = .NOT.XVPTST('NOSUBPIX')
        NOCO = XVPTST('NOCORR')
	CALL XVP ('FILTER', FILTER, DUMMY)
	CALL XVP ('PHASE',  PHASE, DUMMY)
        NOPRNT = XVPTST('NOPRINT')
        CALL XVPARM ('CONTOUR', RPARM, DUMMY, CONTDEF, 3)
        CONTUR = CONTDEF .EQ. 0
        CONMAX = RPARM(1)
        IF (NINT(RPARM(2)).EQ.1) THEN
            DO I=1,CITIE
                XT = RITIE(I)
                RITIE(I) = ROTIE(I)
                ROTIE(I) = XT
            ENDDO
	ENDIF
        CALL XVP ('STOP', RPARM, DUMMY)
	STOPNUM = NINT(RPARM(1))
	STOPTHRESH = RPARM(2)
        CALL XVP ('RETRY', RPARM, DUMMY)
	NRETRY = MAX(MIN( NINT(RPARM(1)), 5), 0)
	RETRY = NRETRY .GT. 0
	IF (NRETRY .EQ. 0) NRETRY = 1
	RRETRY = 0.0
        IF (RETRY) THEN
            TRETRY = RPARM(2)
            RRETRY = RPARM(3)
        ENDIF
        IF (.NOT. (CONTUR) ) THEN
            CALL XVP('MINL',XL,DUMMY)
            CALL XVP('MINS',XS,DUMMY)
            CALL XVP('MAXL',YL,DUMMY)
            CALL XVP('MAXS',YS,DUMMY)
            CALL XVP('NAH',NAH,DUMMY)
            CALL XVP('NAV',NAV,DUMMY)
            XLD = (YL-XL)/NAV
            XSD = (YS-XS)/NAH
            LCOUNT = (NAH+1)*(NAV+1)
	ENDIF


c
c       get the count of the input files
c
c       2 = just two images to be correlated
c       3 = two images and a contour file
c       4 = two images + two masks 
c       5 = two images, two masks, and one contour file
c
c
c
        CALL XVPCNT ('INP', INPCOUNT)


C		Open first input file (first image).
        CALL XVUNIT (RUNIT1, 'INP', 1, STATUS,' ')
        CALL XVOPEN (RUNIT1, STATUS, 'OPEN_ACT','SA', 
     &          'IO_ACT','SA',' ')
        CALL XVGET (RUNIT1, STATUS, 'NL',NL1, 'NS',NS1,' ')


        CALL XVUNIT (RUNIT2, 'INP', 2, STATUS,' ')
        CALL XVOPEN (RUNIT2, STATUS, 'OPEN_ACT','SA',
     &               'IO_ACT','SA',' ')
        CALL XVGET (RUNIT2, STATUS, 'NL',NL2, 'NS',NS2,' ')

c
C		Open third input file (optional graphics-1 contour file).
c
	IF (CONTUR) THEN
	    STATUS = RDGR (3, 2, 2)
        ENDIF
c
c       check for only the coutour ibis1 graphics file
c
        if ( inpcount .eq. 3 ) go to 10    
c
c               Optionally open the mask files
c
        if ( inpcount .gt. 2 ) then    ! set up input unit #'s          
            mask1 = 3
            mask2 = 4
            if ( contur ) then
                mask1 = 4
                mask2 = 5
            end if          

            CALL XVUNIT (RUNIT4, 'INP', mask1, STATUS,' ')
            CALL XVOPEN (RUNIT4, STATUS, 'OPEN_ACT','SA', 
     &          'IO_ACT','SA',' ')


            CALL XVUNIT (RUNIT5, 'INP', mask2, STATUS,' ')
            CALL XVOPEN (RUNIT5, STATUS, 'OPEN_ACT','SA',
     &               'IO_ACT','SA',' ')
        end if


C		Process itie-otie to set up the affine transform.

 10     continue
        DO I = 1, 60
            A(I,3) = 1.0
        ENDDO

        COUNT = CITIE/2

        IF (COUNT.NE.COTIE/2) THEN
	    CALL XVMESSAGE
     &        ('NUMBER OF OTIE MUST EQUAL NUMBER OF ITIE',' ')
	    CALL ABEND
	ENDIF

        DO  I=1,COUNT
           A(I,1) = ROTIE(I*2-1)
           A(I,2) = ROTIE(I*2)
           B(I,1) = RITIE(I*2-1)
           B(I,2) = RITIE(I*2)
        ENDDO

        NEQ = MIN0(COUNT,20)
        NEQP = NEQ

        DO I=1,COUNT
           AG(I,1) = ROTIE(I*2-1)
           AG(I,2) = ROTIE(I*2)
           BG(I,1) = RITIE(I*2-1)
           BG(I,2) = RITIE(I*2)
        ENDDO

	EPS = 1.E-7

C		Find the affine transformation from the ITIE-OTIE that
C		    defines how the chips are extracted from the first image
	CALL LLSQ (AG,BG, COUNT,3,2, XG, IPIV,EPS,IER,AUX)
	IF (IER.NE.0) THEN
	    CALL XVMESSAGE
     &	 ('LEAST SQUARES ON ITIE-OTIE FAILED, TRANSFORM NOT SET',' ')
	    CALL ABEND
	ENDIF

C		Find the initial transformation that gives the location
C		    of the estimated points in the first image
	DO I=1,NEQ
	    DO J=1,3
		AA(I+(J-1)*NEQ) = A(I,J)
		IF (J.NE.3) BB(I+(J-1)*NEQ) = B(I,J)
	    ENDDO
	ENDDO

	CALL LLSQ (AA,BB, NEQ,3,2, X, IPIV,EPS,IER,AUX)
	IF (IER.NE.0) THEN
	    CALL XVMESSAGE('ERROR IN GEOMETRICAL MODEL FIT',' ')
	    CALL ABEND
	ENDIF

	SECCOORD(3) = 1.D0

	IF (.NOT. NOPRNT) THEN
	    PRSTRING = 'SEQ     SECOND LOCATION   ESTIMATED LOCATION'
     *		//'   FIRST LOCATION  CORR WIN  RES FN'
	    CALL XVMESSAGE(PRSTRING,' ')
	ENDIF

	SRCHW = SEARCH-32
	MSRCHW = MSRC-32

	IF (CONTUR) THEN
	    STATUS = NEXTGR (2, EOF, CONL2, CONS2)
	    IF (EOF) GOTO 200
	    CONL1 = CONL2
	    CONS1 = CONS2
	    CONSEG = 0.0
	    CONDELTA = 1.0
	    CONDL = 0.0
	    CONDS = 0.0
	ENDIF


	OUTPTR = 1
	STOPCOUNT = 0
	IBIG = 0



C**********************************************
C	MAIN LOOP THROUGH ALL MATCHING POINTS

 100	CONTINUE
	IBIG = IBIG + 1

	DO 1000 JBIG = 1, NRETRY
	VMAX = 0.0

        IF (CONTUR) THEN
C   Find chip location for contour option
	   IF (CONSEG .GE. 1.0) THEN
	      CONL1 = CONL2
	      CONS1 = CONS2
 	      STATUS = GETGR (2, ZERO, EOF, CONL2, CONS2)
	      IF (EOF) GOTO 200
	      IF (ZERO) THEN
		 STATUS = NEXTGR (2, EOF, CONL2, CONS2)
		 IF (EOF) GOTO 200
		 CONL1 = CONL2
		 CONS1 = CONS2
	      ENDIF
	      CONDL = CONL2 - CONL1
	      CONDS = CONS2 - CONS1
	      CONLEN = SQRT( CONDL**2 + CONDS**2)
	      CONDELTA = CONMAX/(CONLEN + 1.0E-4)
	      CONSEG = 0.0
	   ENDIF
	   CONSEG = MIN( CONSEG + CONDELTA, 1.0)
	   CONL = CONL1 + CONSEG*CONDL
	   CONS = CONS1 + CONSEG*CONDS
	   XLT = CONL + RRETRY*RVEC(JBIG)
	   XST = CONS + RRETRY*RVEC(JBIG+5)

	ELSE
C		Find chip location for grid option
	   IF (IBIG .GT. LCOUNT) GOTO 200
	   IHOR = MOD(IBIG-1,NAH+1)
	   IVER = (IBIG-1)/(NAH+1)
	   XLT = XL + FLOAT(IVER)*XLD + RRETRY*RVEC(JBIG)
	   XST = XS + FLOAT(IHOR)*XSD + RRETRY*RVEC(JBIG+5)
	ENDIF
	CENTR(1) = 16.5
	CENTR(2) = 16.5
	GCNTR(1) = INT(XLT)+0.5
	GCNTR(2) = INT(XST)+0.5
	SECCOORD(1) = DBLE(GCNTR(1))
	SECCOORD(2) = DBLE(GCNTR(2))


C		Use the geometric model to estimate the matching point 
C		    location in the first image
	DO I = 1,2
	    DSUM = 0.D0
	    DO J=1,3
		DSUM = DSUM + SECCOORD(J)*DBLE(X(J,I))
	    ENDDO
	    PRED(I) = INT(SNGL(DSUM))+.5
	ENDDO
	WL = PRED(1)
	WS = PRED(2)
	IF (PRED(1) .LT. 1.0 .OR. PRED(1) .GT. FLOAT(NL1) .OR.
     +      PRED(2) .LT. 1.0 .OR. PRED(2) .GT. FLOAT(NS1) ) GOTO 1000


C		Extract grid from the second image.
	CALL GETGRD(RUNIT2,CHIP1,32,GETGRV,8000,32,NL2,NS2,
     *              GCNTR,XID,SHIFT,RMAG,CHOP,NOIN,NOINV,*1000)
	IFAIL = 1
	IF (CHOP .GE. 100) GO TO 1001


C		Extract grid from mask of the second image
        if ( inpcount .gt. 3 ) then
   	    CALL GETGRD(RUNIT3,CHIP2,32,GETGRV,8000,32,NL2,NS2,
     *              GCNTR,XID,SHIFT,RMAG,CHOP,NOIN,NOINV,*1000)
        end if

C		Extract grids in the search window of the first image
	MM = (MIN0(128,SRCHW+40)/2)*2

        do ii = 1, 128       ! clear the search arrays
          do jj = 1, 128
            asrch(ii,jj) = 0
            if ( inpcount .gt. 3 ) asrch2(ii,jj) = 0
          enddo
        enddo

	CALL GETGRD(RUNIT1,ASRCH,128,GETGRV,8000,MM,NL1,NS1,
     *            PRED,XG,SHIFT,RMAG,CHOP,NOIN,NOINV,*1000)

C		Extract grid from mask of the second image
        if ( inpcount .gt. 3 ) then
    	    CALL GETGRD(RUNIT3,ASRCH2,128,GETGRV,8000,MM,NL1,NS1,
     *            PRED,XG,SHIFT,RMAG,CHOP,NOIN,NOINV,*1000)
        end if

c
c       check for no correlation
c
	IF (NOCO) GO TO 99   


C		Iterate over window for best match
	WMAX = -1.E20
	PSRCH = 49-SRCHW/2
	NAHV = (SRCHW+23)/12
	NWID = 1
	IF (NAHV.GE.2) NWID = SRCHW/(NAHV-1)
	OLDFFT = .FALSE.
	DO IH = 1, NAHV
	    ILIN = PSRCH+(IH-1)*NWID
	    DO JV = 1, NAHV
		JSMP = PSRCH+(JV-1)*NWID
		CALL RFIT(ILIN,JSMP, VMAX, VLOFF, VSOFF, CORR,
     *				NOHPF, OLDFFT, PHASE, FILTER )

		OLDFFT = .TRUE.
		IF (VMAX.GT.WMAX) THEN
		    WMAX = VMAX
		    WLMAX = VLOFF+FLOAT(ILIN)
		    WSMAX = VSOFF+FLOAT(JSMP)
		ENDIF
	    ENDDO
	ENDDO

C		Refine the match location at the correlation peak
	IFAIL = 4
	ILIN = INT(WLMAX)
	JSMP = INT(WSMAX)
	IF (ILIN.LE.0.OR.ILIN.GE.98) GO TO 1001
	IF (JSMP.LE.0.OR.JSMP.GE.98) GO TO 1001
        IF ( WMAX .LE. 0 )           GOTO  1001   ! CASE OF NO GOOD PEAKS.
	CALL RFIT(ILIN,JSMP, VMAX, VLOFF, VSOFF, CORR,
     *		NOHPF, OLDFFT, PHASE, FILTER )

        IF ( VMAX .LE. 0 )           GOTO  1001   ! CASE OF NO GOOD PEAK.

C		If subpixel accuracy desired fit the parabola to peak
	IFAIL = 5

	IF (SUBPIX) CALL REFINE(CORR,VLOFF,VSOFF,*1001)
	WLD = FLOAT(ILIN)+VLOFF+CENTR(1)-65.5


	WSD = FLOAT(JSMP)+VSOFF+CENTR(2)-65.5
	WL = PRED(1) + (XG(1,1)*WLD+XG(2,1)*WSD)*RMAG(1)
	WS = PRED(2) + (XG(1,2)*WLD+XG(2,2)*WSD)*RMAG(2)

	IFAIL = 2
	IF (RETRY .AND. (VMAX.LT.TRETRY)) GO TO 1001


 99	CONTINUE


C		Get the average brightnesses at the matching locations
	CALL GETZVL(CHIP1,32,CENTR(1),CENTR(2),GETW,GETR,GETZ,Z2)
	CALL GETZVL(ASRCH,128,64.5+WLD,64.5+WSD,GETW,GETR,GETZ,Z1)

C		Output stuff to first output file
	OUTBUF(1,OUTPTR) = WL
	OUTBUF(2,OUTPTR) = WS
        OUTBUF(3,OUTPTR) = SNGL(SECCOORD(1))
	OUTBUF(4,OUTPTR) = SNGL(SECCOORD(2))
	OUTBUF(5,OUTPTR) = PRED(1)
	OUTBUF(6,OUTPTR) = PRED(2)
	OUTBUF(7,OUTPTR) = Z1
	OUTBUF(8,OUTPTR) = Z2
	OUTBUF(9,OUTPTR) = VMAX
	OUTBUF(10,OUTPTR) = FLOAT(IBIG)
	OUTPTR = OUTPTR + 1
	IF (OUTPTR .GT. MAXOUTROWS)  GOTO 200


C		Calculate the distance between predicted and correlation point
      RES = SQRT( (PRED(1)-WL)**2 + (PRED(2)-WS)**2 )
      IF (.NOT. NOPRNT) THEN
          WRITE (PRSTRING, 
     *		'(I4,3(2X,F8.2,1X,F8.2),1X,F5.3,1X,I3,F6.1)' )
     *			IBIG,  SECCOORD(1), SECCOORD(2), 
     *			PRED(1), PRED(2), WL, WS, VMAX, SRCHW+32, RES
          CALL XVMESSAGE (PRSTRING,' ')
      ENDIF

C		If correlation value is high enough then add point
C		    to geometric model
	IF (VMAX .GE. RMCOR) THEN
	    NEQ = MIN0(NEQ+1,60)
	    NEQP = MOD(NEQP,60)+1
	    A(NEQP,1) = SNGL(SECCOORD(1))
	    A(NEQP,2) = SNGL(SECCOORD(2))
	    B(NEQP,1) = WL
	    B(NEQP,2) = WS
	    DO I=1,NEQ
		DO J=1,3
		    AA(I+(J-1)*NEQ) = A(I,J)
		    IF (J.NE.3) BB(I+(J-1)*NEQ) = B(I,J)
		ENDDO
	    ENDDO

	    CALL LLSQ (AA,BB, NEQ,3,2, X, IPIV,EPS,IER,AUX)
	    IF (IER.NE.0) THEN
		CALL XVMESSAGE('ERROR IN GEOMETRICAL MODEL FIT',' ')
		CALL ABEND
	    ENDIF
C			Adjust the search window based on the last residual
	    SRCHW = MIN0(MAX0(MSRCHW,(4*SRCHW+2*INT(RES))/5),SRCHW)
	ENDIF

	IF (RETRY) GOTO 100   ! RETRY corr was above threshold so stop retries

 1000	CONTINUE
 1010   CONTINUE
C		Stop the program if we have enough good matching points
C		    above the threshold
	IF (VMAX .GE. STOPTHRESH) THEN
	    STOPCOUNT = STOPCOUNT + 1
	    IF (STOPCOUNT .EQ. STOPNUM) GOTO 200
	ENDIF


	GOTO 100
C		End of main loop




  200   CONTINUE


C		Open first output file (IBIS interface (tabular) file
C		    containing correlation results).
C		Output all the tiepoints.
	CLEN = OUTPTR - 1
	NCOL = 10
	DO I = 1, NCOL
	    COLS(I) = I
	ENDDO
        CALL XVUNIT(WUNIT1,'OUT',1,STATUS,' ')
        IF (STATUS.NE.1) THEN
           CALL XVMESSAGE
     &  ('OUTPUT FILE INITIALIZATION ERROR-PROGRAM TERMINATED',' ')
           CALL ABEND
        ENDIF
        CALL IBIS_FILE_OPEN(WUNIT1,OUTIBIS,'WRITE',NCOL,CLEN,' ',
     &     'COLUMN',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(WUNIT1,STATUS,1)

	DO I = 1, NCOL
          DO JJ = 1, CLEN
            OBUF(JJ) = OUTBUF(I,JJ)
          ENDDO
          CALL IBIS_COLUMN_WRITE(OUTIBIS,OBUF,I,1,CLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
C         CALL PUTREC (WUNIT1, NCOL, COLS, OUTBUF(1,I), I, CLEN, IOBUF)
	ENDDO

   	CALL XVCLOSE (WUNIT1,STATUS,' ')


C		Close up the files
	CALL XVCLOSE (RUNIT1,STATUS,' ')
	CALL XVCLOSE (RUNIT2,STATUS,' ')
	IF (CONTUR) STATUS = CLGR (2)

C		Print out the final geometrical model
	IF (.NOT. NOPRNT) THEN
	    WRITE (PRSTRING, '(A,3(1X,F12.5))')
     *			 ' FINAL FIT: LINE   = ', 
     *			X(1,1), X(2,1), X(3,1)
	    CALL XVMESSAGE(PRSTRING,' ')
	    WRITE (PRSTRING, '(A,3(1X,F12.5))')
     *			 ' FINAL FIT: SAMPLE = ', 
     *			X(1,2), X(2,2), X(3,2)
	    CALL XVMESSAGE(PRSTRING,' ')
	ENDIF

	GO TO 9999


 1001	CONTINUE
C		Printout failure report numbers
	IF (.NOT. NOPRNT) THEN
	    WRITE (PRSTRING, 
     *		'(I4,3(2X,F8.2,1X,F8.2),1X,F5.3,1X,I3,7X,I1)' )
     *			IBIG,  SECCOORD(1), SECCOORD(2),
     *			PRED(1), PRED(2), WL, WS, VMAX, SRCHW+32, IFAIL
	    CALL XVMESSAGE(PRSTRING,' ')

	ENDIF
	GOTO 1010

 9999	CONTINUE

	RETURN
	END



      SUBROUTINE REFINE(CORR,VLOFF,VSOFF,*)
      INTEGER*4 IPIV(6)
      REAL*4 CORR(3,3),A(9,6),B(9),S(6),AUX(12)

C
      DO 1 I=1,3
      Y = FLOAT(I)
      DO 1 J=1,3
      X = FLOAT(J)
      IQ = (I-1)*3+J
      A(IQ,1) = X*X
      A(IQ,2) = X*Y
      A(IQ,3) = Y*Y
      A(IQ,4) = X
      A(IQ,5) = Y
      A(IQ,6) = 1.
 1    B(IQ) = CORR(J,I)
      EPS = 1.E-7
      CALL LLSQ(A,B,9,6,1,S,IPIV,EPS,IER,AUX)
      IF (IER.NE.0) RETURN 1
      IF (S(1).EQ.0.) RETURN 1
      B2A = S(2)/(S(1)*2.)
      Y0 = (B2A*S(4)-S(5))/(2.*S(3)-B2A*S(2))-2.
      X0 = -B2A*(Y0+2.)-S(4)/(S(1)*2.)-2.
      IF (X0*X0+Y0*Y0.GE.4.) RETURN

      VLOFF = VLOFF+Y0
      VSOFF = VSOFF+X0
      RETURN
      END



      SUBROUTINE GETZVL(A,N,FL,FS,NW,NR,NZ,Z)
      INCLUDE 'fortport'
      BYTE A(N,N),KLOG(4)
C
      KINT = 0
      ILL = INT(FL)-NW/2-1
      JSL = INT(FS)-NW/2-1
      FLU = FL-INT(FL)
      FSU = FS-INT(FS)
      FLL = 1.-FLU
      FSL = 1.-FSU
      ILU = ILL+NW-1
      JSU = JSL+NW-1
      Z = -999.0
      IF (ILL.LT.1.OR.ILU.GT.N) RETURN
      IF (JSL.LT.1.OR.JSU.GT.N) RETURN
C
      IRE = 0
      SUM = 0.
      DO 10 I=ILL,ILU
      DO 10 J=JSL,JSU
        KLOG(1) = A(J,I)
        KINT = BYTE2INT(KLOG(1))
        IF (KINT.LE.NZ) IRE = IRE+1
        FDATA = FLOAT(KINT)
        IF (I.EQ.ILL) FDATA = FDATA*FLL
        IF (I.EQ.ILU) FDATA = FDATA*FLU
        IF (J.EQ.JSL) FDATA = FDATA*FSL
        IF (J.EQ.JSU) FDATA = FDATA*FSU
        SUM = SUM+FDATA
 10   CONTINUE
      IF (IRE.LE.NR) Z = SUM/FLOAT((NW-1)*(NW-1))
C
      RETURN
      END



      SUBROUTINE GETGRD(RUNIT,GRID,NDIM,BUFIN,LBUFIN,N,LINE,
     *       SAMP,CENTR,TRANS,SHIFT,MAGNIF,CHOP,NOIN,NOINV,*)
C
C---- VICAR SUBROUTINE "GETGRD".
C     PURPOSE:    TO OBTAIN THE DN-VALUES IN THE POINTS
C   OF THE LINEARLY TRANSFORMED GRID.
C   DN-VALUES ARE OBTAINED BY THE BILINEAR
C   INTERPOLATION BETWEEN FOUR PIXELS
C   ADJACENT TO THE GRID POINT.
C     PARAMETERS: RUNIT - UNIT NUMBER OF IMAGE
C   GRID  - OUTPUT L*1 ARRAY OF INTERPOLATED DN-VALUES.
C   NDIM  - DIMENSION OF THE GRID(NDIM MAX=128), EVEN
C   BUFIN - WORK AREA, TWO-DIMENSIONAL L*1 ARRAY
C    BUFIN(LBUFIN,2), LBUFIN>NDIM*MAGN(2)+50
C   LBUFIN- DEFINES THE SIZE OF BUFIN.
C   N     - DEFINES THE PORTION OF THE GRID
C    TO BE PROCESSED (MUST BE EVEN).
C   LINE  - NUMBER OF LINES IN THE IMAGE.
C   SAMP  - NUMBER OF SAMPLES IN THE IMAGE.
C   CENTR - ARRAY, CENTR(1)=Y OF THE GRID CENTER,
C    CENTR(2)=X OF THE GRID CENTER.
C   TRANS - ARRAY OF THE GRID TRANSFORMATION COEFFICIENTS:
C    YNEW=TRANS(1)*YOLD+TRANS(2)*XOLD+TRANS(3)
C    XNEW=TRANS(4)*YOLD+TRANS(5)*XOLD+TRANS(6)
C   SHIFT - VECTOR TO ADD TO THE TRANSFORMED GRID
C    TO BRING ITS CENTER TO "CENTR".
C   MAGNIF- ARRAY CONTAINING MAGNIFICATION OF
C    X- AND Y- DISTANCES IN THE GRID.
C   CHOP  - NUMBER OF GRID POINTS OUTSIDE THE IMAGE.
C    CORRESPONDING DN-VALUES IN THE "GRID"
C    ARE SET TO ZERO.
C     PROGRAMMER:  BORIS GOKHMAN, OCTOBER 1980.
C
      INCLUDE 'fortport'
      LOGICAL NOIN
      BYTE GRID(1)
      BYTE BUFIN(LBUFIN,2)
      BYTE LOGDN(4),LPIX1(4),LPIX2(4),LPIX3(4),LPIX4(4)
      INTEGER*4 DN1,DN2,DN3,DN4
      INTEGER*4 IDN

      DIMENSION TRANS(6),CENTR(2),SHIFT(2)
      REAL*4 MAGNIF(2)
      REAL*4 Y(4),X(4)
      INTEGER*4 CHOP,LINE,SAMP
      INTEGER*4 ISTART(128)
      INTEGER*4 ISGNES(4), JSGNES(4)
      DIMENSION YINC1(128),XINC2(128),YINC4(128),XINC5(128)
      LOGICAL ENDJ(128)


      DATA ISGNES / 1, -1, -1, 1 /
      DATA JSGNES / 1, 1, -1, -1 /
C
C---- PRELIMINARY CALCULATIONS
C
      IDN = 0
      DN1 = 0
      DN2 = 0
      DN3 = 0
      DN4 = 0
      NOFFB = (NDIM-N)*(NDIM+1)/2
      IHI = 1
      ILOW = 2
      RLINE = LINE
      RSAMP = SAMP
      CHOP = 0
      IF(CENTR(1).LT.1..OR.CENTR(1).GT.RLINE) RETURN 1
      IF(CENTR(2).LT.1..OR.CENTR(2).GT.RSAMP) RETURN 1
      GAPX = MAGNIF(2)
      GAPY = MAGNIF(1)
      SHIFT(1) = CENTR(1)-(N+1)/2.*(TRANS(1)+TRANS(2))-TRANS(3)
      SHIFT(2) = CENTR(2)-(N+1)/2.*(TRANS(4)+TRANS(5))-TRANS(6)
      XMIN = (N+1)/2.-GAPX*(N-1)/2.
      YMIN = (N+1)/2.-GAPY*(N-1)/2.
      XMAX = (N+1)/2.+GAPX*(N-1)/2.
      YMAX = (N+1)/2.+GAPY*(N-1)/2.

C---- FIND THE UPPERMOST CORNER OF THE TRANSFORMED QUAD
C     AND THE SIGN OF THE ARRAY INDEXES CHANGE.
C
      Y(1) = TRANS(1)*YMIN+TRANS(2)*XMIN+TRANS(3)+SHIFT(1)
      Y(2) = TRANS(1)*YMAX+TRANS(2)*XMIN+TRANS(3)+SHIFT(1)
      Y(3) = TRANS(1)*YMAX+TRANS(2)*XMAX+TRANS(3)+SHIFT(1)
      Y(4) = TRANS(1)*YMIN+TRANS(2)*XMAX+TRANS(3)+SHIFT(1)

      IMIN = 1
      DO 20 I=2,4
          IF(nint(Y(I)) .LT. nint(Y(IMIN))) IMIN = I
   20 CONTINUE
      ISIG = ISGNES(IMIN)
      JSIG = JSGNES(IMIN)
C
C---- FIND THE SAMPLE COORDINATES OF LEFTMOST AND
C     RIGHTMOST CORNERS OF THE GRID.
C
      X(1) = TRANS(4)*YMIN+TRANS(5)*XMIN+TRANS(6)+SHIFT(2)
      X(2) = TRANS(4)*YMAX+TRANS(5)*XMIN+TRANS(6)+SHIFT(2)
      X(3) = TRANS(4)*YMAX+TRANS(5)*XMAX+TRANS(6)+SHIFT(2)
      X(4) = TRANS(4)*YMIN+TRANS(5)*XMAX+TRANS(6)+SHIFT(2)
      IABBR = X(1)
      IRIGHT = X(1)
      DO 21 I=2,4
          IF(X(I).LT.IABBR) IABBR = X(I)
          IF(X(I).GT.IRIGHT) IRIGHT = X(I)
   21 CONTINUE
      IABBR = MAX0(IABBR-10,0)
      ICOUNT = IRIGHT-IABBR+10
C
C---- SET UP PARAMETERS FOR THE LOOP.
C
      T1GAPY = TRANS(1)*GAPY
      T2GAPX = TRANS(2)*GAPX
      T4GAPY = TRANS(4)*GAPY
      T5GAPX = TRANS(5)*GAPX
      BASEY = TRANS(1)*(YMIN-GAPY)+TRANS(2)*(XMIN-GAPX)+
     * TRANS(3)+SHIFT(1)
      BASEX = TRANS(4)*(YMIN-GAPY)+TRANS(5)*(XMIN-GAPX)+
     * TRANS(6)+SHIFT(2)
      DO 30 I=1,N
          YINC1(I) = T1GAPY*I
          XINC2(I) = T2GAPX*I
          YINC4(I) = T4GAPY*I
          XINC5(I) = T5GAPX*I
   30 ISTART(I) = 1
      I0 = (N+1)*(1-ISIG)/2
      J0 = (N+1)*(1-JSIG)/2

      JFIN = J0+ISIGN(N,JSIG)
   60 IREC = Y(IMIN)
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *  CALL XVREAD(RUNIT,BUFIN(1,1),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
      IREC = IREC+1
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *   CALL XVREAD(RUNIT,BUFIN(1,2),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
C
C---- BEGIN THE LOOP
C
  100 CONTINUE
      DO 300 J1=1,N
      J = J0+ISIGN(J1,JSIG)

      IF(.NOT.ENDJ(J).AND.ISTART(J).EQ.N) GO TO 300
      IST = ISTART(J)
      ENDJ(J) = .FALSE.
      DO 200 I1=IST,N
          I = I0+ISIGN(I1,ISIG)
          Y0 = BASEY+YINC1(I)+XINC2(J)
          X0 = BASEX+YINC4(I)+XINC5(J)
          IF(Y0.GE.IREC.AND.I1.EQ.1) GO TO 400
C CHECK IF THE END OF COLUMN #J IN THE STRIP IS REACHED
          ISTART(J) = I1
          IF(Y0.LT.IREC) GO TO 240
          ENDJ(J) = .TRUE.
          GO TO 300
  240    CONTINUE
          IF(X0.GE.1.0.AND.X0.LE.RSAMP.AND.Y0.GE.1.0.AND.Y0.LE.RLINE)
     *		 GO TO 210
          IDN = 0
          CHOP = CHOP+1
          GO TO 220
  210    CONTINUE
          IX = X0
          JX = IX-IABBR
          JXPLUS = JX+1
          IY = Y0
          LPIX1(1) = BUFIN(JX,IHI)
          DN1 = BYTE2INT(LPIX1(1))
          LPIX2(1) = BUFIN(JXPLUS,IHI)
          DN2 = BYTE2INT(LPIX2(1))
          LPIX3(1) = BUFIN(JX,ILOW)
          DN3 = BYTE2INT(LPIX3(1))
          LPIX4(1) = BUFIN(JXPLUS,ILOW)
          DN4 = BYTE2INT(LPIX4(1))
      IF (NOIN) GO TO 9000
          FACTX = (X0-IX)
          DN12 = DN1+(DN2-DN1)*FACTX
          DN34 = DN3+(DN4-DN3)*FACTX
c          IDN = DN12+(DN34-DN12)*(Y0-IY)+.5
          IDN = nint(DN12+(DN34-DN12)*(Y0-IY))
  220 CONTINUE 
          LOGDN(1) = INT2BYTE(IDN)
          GRID(NDIM*(I-1)+J+NOFFB) = LOGDN(1)


  200 CONTINUE
  300 CONTINUE
C
C---- READ THE NEW LINE AND REPEAT THE LOOP
C     OR EXIT
C
  400 CONTINUE
      IF(.NOT.ENDJ(JFIN).AND.ISTART(JFIN).EQ.N) RETURN
C
C---- READ THE NEW LINE.
C
      I = IHI
      IHI = ILOW
      ILOW = I
      IREC = IREC+1
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *CALL XVREAD(RUNIT,BUFIN(1,ILOW),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
      GO TO 100
C
c 9000 JQ=X0-IABBR+.5
 9000 JQ=nint(X0-IABBR)
c      IQ=Y0+.5
      IQ=nint(Y0)
      JREC = IREC
      IF(JQ.EQ.JX .AND. IQ.EQ.(JREC-1))     LOGDN(1)=LPIX1(1)
      IF(JQ.EQ.JXPLUS .AND. IQ.EQ.(JREC-1)) LOGDN(1)=LPIX2(1)
      IF(JQ.EQ.JX .AND. IQ.EQ.JREC)         LOGDN(1)=LPIX3(1)
      IF(JQ.EQ.JXPLUS .AND. IQ.EQ.JREC)     LOGDN(1)=LPIX4(1)
      IDN = BYTE2INT(LOGDN(1))
      NOINV = IDN
      END




      SUBROUTINE RFIT(IL,JS, VMAX, VLOFF, VSOFF, CORR,
     *			NOHPF, OLDFFT, PHASE, FILTER )
C
C	RFIT performs both regular and phase correlation between 
C	    CHIP1 and a 32x32 part of ASRCH which are in RFITCOM common.
C
C	Parameters:
C	  IL	 input	integer		line position in ASRCH
C	  JS	 input	integer		sample position in ASRCH
C	  VMAX	 output real		maximum correlation value
C	  VLOFF	 output real		line offset of correlation peak
C	  VSOFF	 output real		sample offset of correlation peak
C	  CORR   output real array	3x3 array around corr. peak
C	  NOHPF	 input  logical		.true. to zero first column and row
C	  OLDFFT input	logical		.true. to use old FFT in matrix A
C	  PHASE  input  real		amount of phase correlation
C	  FILTER input  real		power of power law filter in FFT
C         USEAP  input  logical         .true. if array processor is to be used.

      IMPLICIT NONE
      INTEGER	IL, JS
      REAL	VMAX, VLOFF, VSOFF, CORR(3,3), FILTER, PHASE
      LOGICAL	NOHPF, OLDFFT


      INTEGER	I,J, IXMAX, JXMAX, DCODE, M, N, KL, KS, ISTATUS
      REAL   	OFFMAX

      BYTE CHIP1(32,32), ASRCH(128,128)
      BYTE CHIP2(32,32), ASRCH2(128,128)
      REAL*4    C(32,34)

      COMMON /RFITCOM/  CHIP1, ASRCH,  C
      COMMON /RFITCOM2/ CHIP2, ASRCH2

      integer inpcount
      common /inputs/   inpcount

C=======================================================

 
      DCODE = 1             ! BYTE DATA
      M     = 32
      N     = 32
      OFFMAX =  18./32.     ! OLD RFIT USED TO CHECK ABOUT HALF THE CORR. MATRIX



      if ( inpcount .lt. 4 ) then
          CALL CROSSCORR( CHIP1,32, ASRCH(JS,IL),128, M,N, DCODE,
     +                .NOT. OLDFFT, .NOT. NOHPF, PHASE, FILTER, OFFMAX,
     +                 C, KL,KS, VMAX, ISTATUS)
      else
          CALL CROSSCORR4( CHIP1,32,ASRCH(JS,IL),128,
     +                 chip2,asrch2(js,il),
     +                 M,N, DCODE,
     +                .NOT. OLDFFT, .NOT. NOHPF, PHASE, FILTER, OFFMAX,
     +                 C, KL,KS, VMAX, ISTATUS)
      end if

      IF (ISTATUS .LT. 0)  CALL MABEND( ' ERROR IN CROSSCORR' )


      IF (ISTATUS .EQ. 2 )   THEN     ! IGNORE PEAK IF TOO CLOSE TO MATRIX EDGE.
         VMAX =  0.
         VLOFF = 0.
         VSOFF = 0.
         DO I = 1, 3
           DO J = 1, 3
             CORR(J,I) = VMAX
           ENDDO
         ENDDO

      ELSE
C		Calculate the offsets and extract the correlation peak area
        
        VLOFF = FLOAT(KL)
        VSOFF = FLOAT(KS)
        IXMAX = KL + 17
        JXMAX = KS + 17

         DO I = 1, 3
           DO J = 1, 3
             CORR(J,I) = C(JXMAX+J-2,IXMAX+I-2)
           ENDDO
         ENDDO
         
      END IF

      RETURN
      END
C********************************************************************

      SUBROUTINE crosscorr4( A,IA, B,IB, M,N, DCODE, FIRST, HPF, PHASE,
     .                      PFILTER, OFFMAX,
     .                      C, IL,IS, QUAL, ISTATUS )
C#######################################################################
C  NAME OF ROUTINE
C      crosscorr4 (CROSS-CORRelation)
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        6-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11       VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     5-90  SP   INCREASED MAXIMUM FOR M AND N FROM 128 TO 512.
C     4-94  AS (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C     3-95  AS (CRI) Removed LIB_LOCAL as per FR85771
c     9/96  BAM  ADDED 2 MORE ARRAYS FOR 4 BUFFER CROSSCORRELATION
c
c
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      FLOATA, FTCORR, MVE, XVMESSAGE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

C...ARGUMENT DECLARATIONS

      BYTE        A(*),B(*)
      INTEGER*4   IA, IB, M, N, DCODE, IL,IS, ISTATUS
      LOGICAL*4   FIRST, HPF
      REAL*4      PHASE, PFILTER, OFFMAX, C(N,M+2), QUAL

C...LOCAL DECLARATIONS

      INTEGER*4   I,  IPTRA,   IPTRB,   JPTR,   LSIZEA,  LSIZEB,
     .            MCODE,       MAXM_PAR,        MM2_PAR, PIXSIZE(8)

      PARAMETER   (MAXM_PAR = 512)       ! MAX FOR M & N.  COULD BE INCREASED.
      PARAMETER   (MM2_PAR  = MAXM_PAR*(MAXM_PAR+2))

      REAL*4      FA(MM2_PAR), FB(MM2_PAR)
      SAVE        FA                     ! FFT OF A SLEPT HERE.

      DATA        PIXSIZE  /  1, 2, 0, 4, 0, 0, 4, 8  /  ! BYTES PER PIXEL

C
C======================START OF EXECUTABLE CODE======================

      ISTATUS = 0

C...CONVERT IMAGE DATA TO REAL*4 AND MOVE INTO FA & FB.

      LSIZEA = IA * PIXSIZE(DCODE)
      LSIZEB = IB * PIXSIZE(DCODE)
      IPTRA  = 1
      IPTRB  = 1
      JPTR   = 1

      IF ( N .NE. MIN(N,IA,IB))  THEN
        ISTATUS = -1
        CALL XVMESSAGE('ERROR IN CROSSCORR4: INVALID N VALUE',' ')
        
      ELSE IF ( N .GT. MAXM_PAR .OR. M .GT. MAXM_PAR)  THEN
        ISTATUS = -1
        CALL XVMESSAGE('ERROR IN CROSSCORR4: INVALID M OR N VALUE',' ')
        
      ELSE 
        IF (DCODE .EQ. 1 .OR. DCODE .EQ. 2 .OR. DCODE .EQ. 4)  THEN
          DO I = 1, M
            IF (FIRST)  THEN            ! MOVE A LINE AND CONVERT TO REAL*4.
               CALL FLOATA( DCODE, N, A(IPTRA), FA(JPTR) )
               IPTRA = IPTRA + LSIZEA
            END IF
            CALL FLOATA( DCODE, N, B(IPTRB), FB(JPTR) )
            IPTRB = IPTRB + LSIZEB
            JPTR = JPTR + N
          END DO
  
        ELSE IF (DCODE .EQ. 7 .OR. DCODE .EQ. 8)  THEN
          IF (DCODE .EQ. 7)  MCODE = 7
          IF (DCODE .EQ. 8)  MCODE =-9
          DO I = 1, M
            IF (FIRST)  THEN            ! MOVE A LINE.
               CALL MVE( MCODE, N, A(IPTRA), FA(JPTR),1,1 )
               IPTRA = IPTRA + LSIZEA
            END IF
            CALL MVE( MCODE, N, B(IPTRB), FB(JPTR),1,1 )
            IPTRB = IPTRB + LSIZEB
            JPTR = JPTR + N
          END DO
  
        ELSE
          ISTATUS = -1
          CALL XVMESSAGE('ERROR IN CROSSCORR4: INVALID DCODE VALUE',' ')
        END IF
      END IF

      IF (ISTATUS .EQ. 0)
     .  CALL FTCORR( FA,FB, M,N, FIRST, HPF, PHASE, PFILTER, OFFMAX,
     .               C, IL,IS, QUAL, ISTATUS)

      RETURN
      END



C********************************************************************
      SUBROUTINE CROSSCORR( A,IA, B,IB, M,N, DCODE, FIRST, HPF, PHASE,
     .                      PFILTER, OFFMAX,
     .                      C, IL,IS, QUAL, ISTATUS )
C  NAME OF ROUTINE
C      CROSSCORR (CROSS-CORRelation)
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        6-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      FLOATA, FTCORR, MVE, QPRINT

      IMPLICIT NONE

C...ARGUMENT DECLARATIONS

      BYTE        A(*),B(*)
      INTEGER*4   IA, IB, M, N, DCODE, IL,IS, ISTATUS
      LOGICAL*4   FIRST, HPF
      REAL*4      PHASE, PFILTER, OFFMAX, C(N,M+2), QUAL

C...LOCAL DECLARATIONS
                 
      INTEGER*4   I,  IPTRA,   IPTRB,   JPTR,   LSIZEA,  LSIZEB,
     .            MCODE,       MAXM_PAR,        MM2_PAR, PIXSIZE(8)

      PARAMETER   (MAXM_PAR = 256)       ! MAX FOR M & N.  COULD BE INCREASED.
      PARAMETER   (MM2_PAR  = MAXM_PAR*(MAXM_PAR+2))

      REAL*4      FA( MM2_PAR ), FB(MM2_PAR)
      SAVE        FA                     ! FFT OF A SLEPT HERE.

      DATA        PIXSIZE  /  1, 2, 0, 4, 0, 0, 4, 8  /  ! BYTES PER PIXEL

C
C======================START OF EXECUTABLE CODE======================

      ISTATUS = 0

C...CONVERT IMAGE DATA TO REAL*4 AND MOVE INTO FA & FB.

      LSIZEA = IA * PIXSIZE(DCODE)
      LSIZEB = IB * PIXSIZE(DCODE)
      IPTRA  = 1
      IPTRB  = 1
      JPTR   = 1

      IF ( N .NE. MIN(N,IA,IB))  THEN
        ISTATUS = -1
        CALL ifmessage('ERROR IN CROSSCORR: INVALID N VALUE')
        
      ELSE IF ( N .GT. MAXM_PAR .OR. M .GT. MAXM_PAR)  THEN
        ISTATUS = -1
        CALL ifmessage('ERROR IN CROSSCORR: INVALID M OR N VALUE')
        
      ELSE       ! automatch always uses decode = 7 
        IF (DCODE .EQ. 1 .OR. DCODE .EQ. 2 .OR. DCODE .EQ. 4)  THEN
          DO I = 1, M
            IF (FIRST)  THEN            ! MOVE A LINE AND CONVERT TO REAL*4.
               CALL FLOATA( DCODE, N, A(IPTRA), FA(JPTR) )
               IPTRA = IPTRA + LSIZEA
            END IF
            CALL FLOATA( DCODE, N, B(IPTRB), FB(JPTR) )
            IPTRB = IPTRB + LSIZEB
            JPTR = JPTR + N
          END DO
  
        ELSE IF (DCODE .EQ. 7 .OR. DCODE .EQ. 8)  THEN
          IF (DCODE .EQ. 7)  MCODE = 7
          IF (DCODE .EQ. 8)  MCODE =-9   ! automatch always uses decode = 7 
	  DO I=1, M
	      IF (FIRST) THEN                 ! MOVE A LINE
	        CALL MVE( MCODE, N, A(IPTRA), FA(JPTR),1,1 )
          	IPTRA = IPTRA + LSIZEA
              END IF
              CALL MVE( MCODE, N, B(IPTRB), FB(JPTR),1,1 )
              IPTRB = IPTRB + LSIZEB
              JPTR = JPTR + N
          END DO
  
        ELSE
          ISTATUS = -1
          CALL ifmessage('ERROR IN CROSSCORR: INVALID DCODE VALUE')
        END IF
      END IF

      IF (ISTATUS .EQ. 0)
     .  CALL FTCORR( FA,FB, M,N, FIRST, HPF, PHASE, PFILTER, OFFMAX,
     .               C, IL,IS, QUAL, ISTATUS)

      RETURN
      END


C********************************************************************
      SUBROUTINE FTCORR( FA,FB, M,N, FIRST, HPF, PHASE, PFILTER, OFFMAX,
     .                   C, IL,IS, QUAL, ISTATUS)

C#######################################################################
C  NAME OF ROUTINE
C      FTCORR (Fourier Transform cross-CORRelation)
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        6-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  FTCORR descended from routine RFIT by K.F.Evans & routine FTCORR by L.W.Kamp.
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  SUBROUTINES CALLED
C      RFT2, XVMESSAGE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

C...ARGUMENT DECLARATIONS

      INTEGER*4   M, N, IL,IS, ISTATUS, ISTAT2
      LOGICAL*4   FIRST, HPF
      REAL*4      PHASE, PFILTER, OFFMAX, C(N,M+2), QUAL,
     .            FA(N,M+2), FB(N,M+2)

C...LOCAL DECLARATIONS

      INTEGER*4   I,  J,  IXMAX, JXMAX,  MHALF, NHALF

      REAL*4      DNORM,  FILPOW, POWER, POWERA, POWER1A,POWER1B,
     .            POWERB, POWER2A, POWER2B,  FILT, R2, Y2, AMP, AMPP,
     .            CIJR, CIJI, VMAX, T, RV
      SAVE        POWERA
C
C======================START OF EXECUTABLE CODE======================

      ISTATUS = 0
      DNORM   = MAX( M,N )
      DNORM   = 1./DNORM
C...CHECK THAT PARAMETERS ARE VALID.

      IF ( MOD(M,2) .NE. 0 .OR. MOD(N,2) .NE. 0 ) GO TO 8100
      IF ( PHASE .LT. 0 .OR. PHASE .GT. 1.0)      GO TO 8200
      IF ( PFILTER .LT. 0)                        GO TO 8300
      IF ( OFFMAX .LE. 0 .OR. OFFMAX .GT. 1.0)    GO TO 8400

C...FFT the image areas

      IF (FIRST)  THEN
          CALL RFT2(FA, M,N, 1, ISTAT2)
          IF (ISTAT2 .NE. 1) GO TO 8100

C...Do radial filter for A area

	  IF (PFILTER .NE. 0.0) THEN

	    IF (PHASE .EQ. 1.0) THEN 
               FILPOW = PFILTER             ! ALL FOR A AND NONE FOR B.
            ELSE
               FILPOW = PFILTER/2.          ! HALF FOR A AND HALF FOR B.
            END IF

	    DO I = 1, N
	      Y2 = ( MIN(I-1, N-I+1)*DNORM )**2 
	      DO J = 1, M+2, 2
	        R2 = Y2 + ( ((J-1)/2)*DNORM )**2
                IF (FILPOW .EQ. 1.0)  THEN
                   FILT = R2
                ELSE IF (FILPOW .EQ. .5)  THEN
                   FILT = SQRT(R2)
                ELSE
                   FILT = R2**FILPOW
                END IF
	        FA(I,J) = FILT*FA(I,J)
	        FA(I,J+1) = FILT*FA(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C...HANDLE FRACTIONAL PHASE VALUES.

          IF (PHASE .GT. 0.0 .AND. PHASE .LT. 1.0)  THEN
	    DO J = 1, M+2, 2
	      DO I = 1, N
	        AMP = AMAX1( SQRT( FA(I,J)**2 + FA(I,J+1)**2), 1.E-12)
                IF (PHASE .EQ. .5)  THEN
                    AMPP= 1./ SQRT(AMP)
                ELSE
                    AMPP= 1./( AMP**PHASE )
                END IF
	        FA(I,J)   = AMPP*FA(I,J)
	        FA(I,J+1) = AMPP*FA(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C		Zero first row and column if HPF

          IF (HPF) THEN
            CALL ZIA( FA, 2*N)
            DO J = 3,M+2
              FA(1,J) = 0.
            ENDDO
          ENDIF

C		Calculate power for normalization of nophase

	  IF (PHASE .NE. 1.0) THEN
	    FA(1,1) = 0.0               ! ZERO DIRECT CURRENT (DC) TERM.
	    FA(1,2) = 0.0    ! IN CASE NOT 0 BECAUSE OF ROUNDING.
            POWER1A = 0.0
            POWER2A = 0.0
            DO I = 1, N
      	      POWER1A = POWER1A + ( FA(I,1)**2 + FA(I,2)**2 )
      	      POWER1A = POWER1A + ( FA(I,M+1)**2 + FA(I,M+2)**2 )
              DO J = 3, M, 2
	        POWER2A = POWER2A + ( FA(I,J)**2 + FA(I,J+1)**2 )
              ENDDO
	    ENDDO
	    POWERA = POWER1A + 2*POWER2A
	  ENDIF
      ENDIF

C...COMPUTE FFT OF B IMAGE AREA.

      CALL RFT2(FB, M,N, 1, ISTAT2)
         IF (ISTAT2 .NE. 1) GO TO 8100
	
C...If phase corr. multiply FFT's  & divide by the amplitudes.
 
      IF (PHASE .EQ. 1.0)  THEN
        DO J = 1, M+1, 2
          DO I = 1, N
            CIJR = FA(I,J)*FB(I,J) + FA(I,J+1)*FB(I,J+1)
            CIJI = FA(I,J)*FB(I,J+1) - FA(I,J+1)*FB(I,J)
            AMP  = AMAX1( SQRT(CIJR*CIJR+CIJI*CIJI), 1.E-12)
            C(I,J)   = CIJR/AMP
            C(I,J+1) = CIJI/AMP
          ENDDO
        ENDDO

      ELSE   ! OTHERWISE WE FILTER AND COMPUTE NORMALIZATION FACTOR POWERB
             ! AND THEN MULTIPLY FFTS.

C...Do radial filter for B area

	  IF (PFILTER .NE. 0.0) THEN
            FILPOW = PFILTER/2.          ! HALF FOR A AND HALF FOR B.

	    DO I = 1, N
	      Y2 = ( MIN(I-1, N-I+1)*DNORM )**2 
	      DO J = 1, M+2, 2
	        R2 = Y2 + ( ((J-1)/2)*DNORM )**2
                IF (FILPOW .EQ. 1.0)  THEN
                   FILT = R2
                ELSE IF (FILPOW .EQ. .5)  THEN
                   FILT = SQRT(R2)
                ELSE
                   FILT = R2**FILPOW
                END IF
	        FB(I,J) = FILT*FB(I,J)
	        FB(I,J+1) = FILT*FB(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C...HANDLE FRACTIONAL PHASE VALUES.

          IF (PHASE .GT. 0.0 .AND. PHASE .LT. 1.0)  THEN
	    DO J = 1, M+2, 2
	      DO I = 1, N
	        AMP = AMAX1( SQRT( FB(I,J)**2 + FB(I,J+1)**2), 1.E-12)
                IF (PHASE .EQ. .5)  THEN
                    AMPP= 1./ SQRT(AMP)
                ELSE
                    AMPP= 1./( AMP**PHASE )
                END IF
	        FB(I,J)   = AMPP*FB(I,J)
	        FB(I,J+1) = AMPP*FB(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C		Zero first row and column if HPF

          IF (HPF) THEN
            CALL ZIA( FB, 2*N)
            DO J = 3,M+2
              FB(1,J) = 0.
            ENDDO
          ENDIF

C		Calculate power for normalization.

	    FB(1,1) = 0.0               ! ZERO DIRECT CURRENT (DC) TERM.
	    FB(1,2) = 0.0    ! IN CASE NOT 0 BECAUSE OF ROUNDING.
            POWER1B = 0.0
            POWER2B = 0.0
            DO I = 1, N
      	      POWER1B = POWER1B + ( FB(I,1)**2 + FB(I,2)**2 )
      	      POWER1B = POWER1B + ( FB(I,M+1)**2 + FB(I,M+2)**2 )
              DO J = 3, M, 2
	        POWER2B = POWER2B + ( FB(I,J)**2 + FB(I,J+1)**2 )
              ENDDO
	    ENDDO
	    POWERB = POWER1B + 2*POWER2B

        DO J = 1, M+1, 2
          DO I = 1, N
            C(I,J)   = FA(I,J)*FB(I,J) + FA(I,J+1)*FB(I,J+1)
            C(I,J+1) = FA(I,J)*FB(I,J+1) - FA(I,J+1)*FB(I,J)
          ENDDO
        ENDDO

      ENDIF
C		FFT back to the image domain and rearrange 
C			matrix to put the DC in center
      CALL RFT2(C, M,N, -1, ISTAT2)
          IF (ISTAT2 .NE. 1) GO TO 8100

      MHALF = M/2
      NHALF = N/2

      DO J = 1, MHALF
        DO I = 1, NHALF
          T = C(I,J)
          C(I,J) = C(I+NHALF,J+MHALF)
          C(I+NHALF,J+MHALF) = T
          T = C(I+NHALF,J)
          C(I+NHALF,J) = C(I,J+MHALF)
          C(I,J+MHALF) = T
        ENDDO
      ENDDO
C		Search for the correlation peak
      VMAX = C(1+NHALF, 1+MHALF)            ! INITIALIZE.
      IXMAX = 1 + NHALF
      JXMAX = 1 + MHALF
      DO J = 1, M
            DO I = 1, N
               RV = C(I,J)
               IF (VMAX .LT. RV) THEN
                  VMAX = RV
                  IXMAX = I
                  JXMAX = J
               ENDIF
            ENDDO
      ENDDO
C		Normalize the correlation value

      IF ( PHASE .EQ. 1.0 )  THEN
         IF (HPF)  THEN
            QUAL = VMAX/ ( (M-1)*(N-1) )
         ELSE
            QUAL = VMAX/ (M*N)
         END IF

      ELSE
         POWER = SQRT(POWERA*POWERB) 
         IF (POWER .GT. 0.0)  THEN
            QUAL = VMAX/POWER
         ELSE
            QUAL = VMAX
         END IF
      ENDIF
C		Calculate the offsets.

      IL = JXMAX - MHALF - 1
      IS = IXMAX - NHALF - 1

C...Test for correlation peak too near edge of matrix.

      IF ( IABS(IL) .GT. OFFMAX*MHALF .OR. IABS(IS) .GT. OFFMAX*NHALF )
     .     ISTATUS = 2

7000  RETURN

C...ERROR HANDLING

8100  CONTINUE
      ISTATUS = -1
      CALL XVMESSAGE('ERROR IN FTCORR: INVALID M OR N VALUE',' ')
      GOTO 7000

8200  CONTINUE
      ISTATUS = -1
      CALL XVMESSAGE('ERROR IN FTCORR: INVALID PHASE VALUE',' ')
      GOTO 7000

8300  CONTINUE
      ISTATUS = -1
      CALL XVMESSAGE('ERROR IN FTCORR: INVALID PFILTER VALUE',' ')
      GOTO 7000

8400  CONTINUE
      ISTATUS = -1
      CALL XVMESSAGE('ERROR IN FTCORR: INVALID OFFMAX VALUE',' ')
      GOTO 7000

      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create picmatch.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM picmatch

   To Create the build file give the command:

		$ vimake picmatch 			(VMS)
   or
		% vimake picmatch	       		(Unix)


************************************************************************/


#define PROGRAM	picmatch
#define R2LIB

#define MODULE_LIST picmatch.f 

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77  
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create picmatch.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,72) COUNT=(2:5)
PARM OUT      TYPE=(STRING,72) COUNT=(1:2)
PARM ITIE     TYPE=REAL    COUNT=(3:20)
PARM OTIE     TYPE=REAL    COUNT=(3:20)
PARM IGRID    TYPE=REAL    COUNT=(3:20) DEFAULT=(0,0,0)
PARM OGRID    TYPE=REAL    COUNT=(3:20) DEFAULT=(0,0,0)
PARM MAGNIFY  TYPE=REAL    COUNT=(1:2)  DEFAULT=(1,1)
PARM SEARCH   TYPE=INTEGER DEFAULT=128  VALID=32:128
PARM MSEARCH  TYPE=INTEGER DEFAULT=50   VALID=32:128
PARM ZWIND    TYPE=INTEGER DEFAULT=10
PARM ZREJECT  TYPE=INTEGER COUNT=2 DEFAULT=(100000,0)
PARM MINCORR  TYPE=REAL    DEFAULT=0.20
PARM CONTOUR  TYPE=REAL    COUNT=2 DEFAULT=(0,0)
PARM STOP     TYPE=REAL    COUNT=2 DEFAULT=(0,0)
PARM RETRY    TYPE=REAL    COUNT=3 DEFAULT=(0,0,0)
PARM PHASE    TYPE=REAL    COUNT=1 DEFAULT=1 VALID=(0:1)    
PARM FILTER   TYPE=REAL    DEFAULT=0  VALID=0:2
PARM NOPRINT  TYPE=KEYWORD COUNT=1 VALID=(PRINT,NOPRINT)  DEFAULT=PRINT
PARM NOSUBPIX TYPE=KEYWORD COUNT=(0:1) VALID=(NOSUBPIX) DEFAULT=--
PARM NOCORR   TYPE=KEYWORD COUNT=(0:1) VALID=(NOCORR)   DEFAULT=--
PARM NOHPF    TYPE=KEYWORD COUNT=(0:1) VALID=(NOHPF)    DEFAULT=--
PARM MINL     TYPE=REAL    DEFAULT=0
PARM MINS     TYPE=REAL    DEFAULT=0
PARM MAXL     TYPE=REAL    DEFAULT=0
PARM MAXS     TYPE=REAL    DEFAULT=0
PARM NAH      TYPE=INTEGER DEFAULT=0
PARM NAV      TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program "picmatch"
.HELP
PURPOSE

     "picmatch"  is a batch oriented image correlation routine 
     which incorporates the following features.

     1. Correlation  of  image  to image.

     2. Sampling  of one image to match the geometry of  the 
        other  image (e.g.  one image could be at  different 
        scale or rotation).

     3. Regular or phase correlation in the FFT domain.
.PAGE
     4. Specification of a magnification factor to correlate 
        a larger area without increasing computation.

     5. Use  of  a geometric model to estimate the  matching 
        locations  and  feedback of matches  to  refine  the 
        geometric model.

     6. Matching of images along a polygonal contour.

     7. Use  of  standard  data formats for  all  input  and 
        output data sets.
.PAGE

     TAE COMMAND LINE FORMAT

     picmatch INP=(A,B)   OUT=P      PARAMS
     or
     picmatch INP=(A,B,C) OUT=P      PARAMS

     where

     A                   is the first image to be correlated.

     B                   is   the   second   image   to   be 
                         correlated.

     C                   is  an optional file in graphics-1
			 format containing polygonal contours.

     P                   is an output IBIS tabular file that 
                         contains  the  pairs  of   matching  
                         coordinate locations together  with
                         editing information  such as corre-
			 lation value.  


OPERATION

     First,  "picmatch" sets up a geometric model relating the 
     two images using the ITIE-OTIE parameters.   The points 
     originally  given  are supplemented  by  matches  which 
     exceed  the  value given by the parameter MINCORR  or  its 
     default.   The  geometric  model is used to estimate  a 
     search  location  and to specify a  resampling  of  the 
     first  image to match the raster gridding of the second 
     image.   The model is a linear least squares fit to the 
     control  points.   For normal image to image  matching, 
     only  ITIE-OTIE  points  are  needed.   Their  accuracy 
     should be sufficient to yield a match within the search 
     area  specified  by parameters SEARCH and  MSEARCH  taking 
     into account nonlinearities in the data.

     The  second step is to set up the correlation point set 
     in  the second image by one of three methods.   If  the 
     parameters MINL,  MINS, MAXL, MAXS, NAH, NAV are given, 
     then a grid of points will be used.   If a third  input 
     (a contour or point set) and the parameter CONTOUR is used 
     then the correlation point set will be spaced along the 
     contour  (see  the CONTOUR parameter).   By  specifying  a 
     large spacing, the input can be regarded as a point set 
     and  matching  will  only  occur at the  nodes  of  the 
     contour.

     The  iteration  over  the  correlation  point  set  now 
     proceeds.   At the requested point in the second input, 
     a  32x32  sub-image is extracted.   If the point  is  a 
     fractional pixel location,  the center of extraction is 
     moved   to   align   with  pixel   spacing   to   avoid 
     interpolation.   In fact, interpolation will occur only 
     if  the keyword MAGNIFY is used with a nonintegral  value.  
     The  geometric  model  is used to  calculate  a  search 
     location in the first input.   If this is a  fractional 
     pixel  location,  then  it is also aligned  with  pixel 
     spacing  to minimize interpolation near the center.   A 
     sub-image is extracted of size WxW where W is somewhere 
     between  the values given by the parameters SEARCH  and 
     MSEARCH.  This  sub-image  is  extracted  by   bilinear 
     interpolation at a rotation and pixel size to match the 
     second  input  geometry as specified by  the  geometric 
     model discussed earlier.

     Correlation is performed on 32x32 subwindows of the WxW 
     window.   The  32x32 pieces of the two inputs are  each 
     subjected   to  a  complex  valued  DFT  via  the   FFT 
     algorithm.  One of the DFT arrays has its low frequency 
     terms  set  to  zero by zeroing the first row  and  the 
     first  column unless the parameter NOHPF is  specified.  
     Then  the DFT's are multiplied element by element  (one 
     DFT is conjugated).  If PHASE=1 is specified each
     product is divided  by the  absolute value of its multi-
     plicands (to give phase correlation,  see  reference  1).   
     ("picmatch" divides each element of the FFT of the 
     cross-correlation by the amplitude of the element raised 
     to the PHASE power.  See under the PHASE parameter.)
     Except for phase correlation the power in each FFT is calculated
     for use in the normalization.  An inverse  FFT  is applied  
     to  this  result and the peak  and  its  eight neighbors 
     are saved.   The correlation value for both methods is 
     normalized so that the maximum possible correlation value 
     is 1.0 .   The subwindow is moved over  the search  
     area  with an increment of at most  12  pixels.   When  
     the  entire search area is covered,  the  largest peak is 
     taken to be the correlation point.  A final FFT correlation 
     is performed at the correlation peak to get a refined 
     correlation.   Unless NOSUBPIX is specified a subpixel 
     match is  obtained by  fitting a quadratic  to the peak  
     and  its  eight neighbors.     The match point is transformed 
     back to  the original image location  via  the  inverse  
     of   the geometric  model  and  incorporating  alignment  
     shifts performed in both images.  

     The results are written to an IBIS interface (tabular)
     file with one row for each tiepoint.  The following 
     column format is used:

     Column       Description			   Format

        1     First input matching line            real*4
        2     First input matching sample          real*4
        3     Second input line                    real*4
        4     Second input sample                  real*4
	5     First input estimated line           real*4
	6     First input estimated sample         real*4
        7     First input Z value                  real*4
        8     Second input Z value                 real*4
        9     Correlation value                    real*4
       10     Sequence number                      real*4

       11,12  Latitude of chip                     real*8
       13,14  Longitude of chip                    real*8

    The last four columns are only output if the ground control 
    chip set option is invoked; if so the second input line and 
    sample refer to the reference point in the ground control chip.




    Information about each matching point is printed out unless 
    the NOPRINT keyword is specified.   The printed information
    is as follows:

        1   SEQ			Sequence number
	2   SECOND LOCATION	Tiepoint location in second image
	3   ESTIMATED LOCATION	Estimated location in first image
	4   FIRST LOCATION	Matching location in first image
				    (from correlation search)
	5   CORREL		Correlation value (between 0 and 1)
	6   WIN			Current search window size
	7   RESID		Residual distance (between estimated 
				    location and correlation location)
	8   FN			Failure number

     If correlation  fails,  nothing  is written into the  disk 
     file  but  a failure number is given  in  the  printout 
     under the column heading FN.  The failure numbers are:

              1             point on edge of second image 
				(more than ten percent of chip off image)
              2             failure of RETRY option to exceed threshold
              4             correlation peak unreliable
              5             subpixel correlation failure


     Points  completely outside of either image are  ignored 
     completely.




PRECISION

Due to the iterative nature of the subroutine llsq, the programs results will
vary from machine to machine.  When prnt in llsq's test program was temporarily
changed to a write statement, it was observed that the resulting output varied
from machine to machine.  The following table will provide some idea of the
differential between machines.

                PORTED VMS  ALPHA &     SUN        SGI        SOLARIS
                            UNPORT VAX
FINAL FIT: LINE -81.20914   -81.20914   -81.20908  -81.20908  -81.20908
FINAL FIT: SAMP  15.19182    15.19181    15.19191   15.19190   15.19191



RESTRICTIONS

     The ground control chip file option (use of IBIS graphics-2 
     files)  is not currently supported.


REFERENCES

     C.  D.  Kuglin and D.  C. Hines, "The phase correlation 
     image alignment method," Proc.  IEEE 1975 International 
     Conference on Cybernetics and Society, September, 1975, 
     pp. 163-165.

     Original Programmer:  A. L. Zobrist    12 October 1981
     Current Cognizant Programmer: B. A. McGuffie
     Revision:  5	June 1987
                6       May  1995  (CRI) Made portable for UNIX
                7       September 1996

.LEVEL1
.VARIABLE INP
Input images(2) and contour(1)
Optionally, Input files 3,4
are binary masks.
.VARIABLE OUT
Out file (matching coord pairs)
.VARIABLE ITIE
Control points in first image
.VARIABLE OTIE
Control points in second image
.VARIABLE MAGNIFY
Magnification of sampling
.VARIABLE SEARCH
Search area size (s X s)
.VARIABLE MSEARCH
Minimum of the search area
.VARIABLE MINCORR
Minimum acceptable corr value
.VARIABLE ZWIND
Window for brightness calculation
.VARIABLE ZREJECT
Rejection threshold for Z value
.VARIABLE MINL
Set of grid locations for corr
.VARIABLE MINS
Set of grid locations for corr
.VARIABLE MAXL
Set of grid locations for corr
.VARIABLE MAXS
Set of grid locations for corr
.VARIABLE NAH
Set of grid locations for corr
.VARIABLE NAV
Set of grid locations for corr
.VARIABLE CONTOUR
Specifies contour/point data set
.VARIABLE STOP
Parameters for stopping program
.VARIABLE RETRY
Parameters for retrying of corr
.VARIABLE PHASE
REAL - Specifies the amount of
phase correlation. 
.VARIABLE FILTER
The power of the FFT filter
.VARIABLE NOPRINT  
To suppress messages
.VARIABLE NOSUBPIX 
For no subpixel determination
of matching point
.VARIABLE NOCORR   
To not do correlations
.VARIABLE NOHPF    
To turn off high pass filter
in correlations

.LEVEL2
.VARIABLE INP
     INP=(A,B,C,D,E),
     where:

     A                   is the first image to be correlated.

     B                   is   the   second   image   to   be 
                         correlated   [or  a  ground  control 
                         point file in graphics-2 format --
                         NOT CURRENTLY SUPPORTED].

     C                   is  a  file  in  graphics-1  format 
                         containing polygonal contours  (in
                         B, which must be an image).

     D                   is the binary for the first image.
                         (0,1)

     E                   is the binary for the second image.
.VARIABLE OUT
     OUT=(P,Q)
     P                   is an IBIS tabular file that contains 
                         the  pairs of  matching  coordinate 
                         locations   together  with  editing 
                         information  such  as   correlation 
                         value.  See help file for format.

    Q                    is  a ground control point file  in 
                         graphics-2  format  made  from  the 
                         first   image  A  at  its  matching 
                         locations
                          -- NOT CURRENTLY SUPPORTED.
.VARIABLE ITIE
     ITIE=(X1,Y1,...,XN,YN)   This  specifies three or  more 
                              control  points in  the  first
                              input    image   to    control 
                              matching search and resampling 
                              geometry.   
.VARIABLE OTIE
     OTIE=(P1,Q1,...,PN,QN)   This specifies the  correspon
                              ding  control  points  in  the 
                              second  image.   If the second 
                              input  is  a  ground   control 
                              point  file then these  values 
                              are  latitude longitude pairs.  
                              The number of values given  to 
                              OTIE  must be the same as  the 
                              number  to ITIE.   
.VARIABLE MAGNIFY
     MAGNIFY=R                This  specifies that  sampling 
                              from  both images is magnified 
                              by R (default 1.0).   The grid 
                              is  always  32 by 32  so  this 
                              increases the correlation area 
                              without increasing computation.
			      The magnification can be speci-
			      fied separately for line and 
			      sample directions.
.VARIABLE SEARCH
     SEARCH=S                 This specifies that the search 
                              area  in  the first  input  is 
                              initially  an SxS  square  re-
                              gion.   Allow  for uncertainty 
                              and  nonlinearity in the esti-
                              mation of matching location by 
                              ITIE,  OTIE and add 12 more to 
                              S for ineffectiveness of  cor-
                              relation near the window edge.  
                              S  must be less than or  equal 
                              to   128  which  is  also  the 
                              default.
.VARIABLE MSEARCH
     MSEARCH=T                This   specifies   that    the 
                              initial   search   S  can   be 
                              reduced    upon     successful 
                              matching to a minimum value of 
                              T.    Note   that   successful 
                              matches  refine  the image  to 
                              image geometric model allowing 
                              a  reduction in search with  a 
                              consequent     savings      of 
                              computation.     The   default 
                              value  is 50 and miminum value 
                              is 32.
.VARIABLE MINCORR
     MINCORR=V                The  value  V sets  a  minimum 
                              correlation     value      for 
                              acceptance of a new point into 
                              the  image-to-image  geometric 
                              model (default .20).

.VARIABLE ZWIND
     ZWIND=W                  The program calculates and outputs
			      the  average brightness values 
			      at  the matching coordinates.  
			      The average is calculated using
			      a W by W window.  Default is 10.
.VARIABLE ZREJECT
     ZREJECT=(K,Z)            The average brightness is rejected 
			      by setting the output Z value to
			      -999 if more than  K of  the values 
			      are below a DN value of Z.  This
			      does not affect whether the corre-
			      lation is performed.  The default
			      is (100000,0) .

.VARIABLE MINL
     MINL=M1,MINS=M2,         These  specify a set  of  grid 
     MAXL=M3,MAXS=M4,         locations  in the second input 
     NAH=M5,NAV=M6            image  for  correlation.   The 
                              program      will      attempt 
                              (M5+1)*(M6+1) correlations  in 
                              a  uniform grid bounded by the 
                              values M1,...,M4.  This is  an 
                              alternative  to matching along 
                              a set of points specified by a 
                              third input data set.
.VARIABLE CONTOUR
     CONTOUR=(A,B)            This parameter must be given  if 
                              a  third data set containing a 
                              contour   or  point  set   for 
                              matching is given.   The third 
			      data set is in graphics-1 format.
                              The  parameter A  specifies  a 
                              spacing  to be followed  along 
                              the   contour.    The  routine 
                              always   matches  at   contour 
                              nodes  hence a  large  spacing 
                              turns  the third input into  a 
                              simple   set  of  points   for 
                              matching.   The parameter B is 
                              normally 0 (zero) but if it is 
                              set  to 1 then it reverses the 
                              role of ITIE and  OTIE.   This 
                              is   a  convenience  when  two 
                              images  are  correlated  twice 
                              along contours in each of  the 
                              frames.

.VARIABLE STOP
     STOP=(N,T)               This  instructs the program to 
			      stop after achieving N matches
			      with a correlation value above
			      a threshold T.  This is useful
			      for saving CPU time while still
			      assuring a certain number of
			      good matches.

.VARIABLE RETRY
     RETRY=(N,T,R)            This  instructs the program to 
                              attempt  more  tries  near   a 
                              location    if    a    certain 
                              correlation  threshold is  not 
                              achieved.   N is the number of 
                              tries  (1 to 5), with the first
			      try being the at the original point.
			      T is  the correlation threshold 
			      required to stop the retry at each  
			      point.  A failure number is 
 			      printed every time the threshold 
			      test fails.  If all retries for 
			      a point fail then nothing is 
			      written to the output files.
                              The  retry pattern is to  move 
                              to  the corners of a square 
			      i.e. to move to (+/-R, +/-R) from
			      the original point.

.VARIABLE PHASE
       PHASE=P		      P is a REAL*4 value in the 
			      range 0 to 1.  

                              Use          PHASE=1
                              for          phase correlation.

                              Use          PHASE=0
                              for          normal (NOPHASE) correlation.

			      "picmatch" divides each element of the
			      FFT of the cross-correlation 
			      by the amplitude of the element raised 
			      to the P power.  Note that when
			      P is 0, this reduces to 
			      dividing by 1.  (The value of the
			      correlation is still normalized
			      to a maximum possible value of 1.)
                              "picmatch" runs slower for PHASE values 
			      other than 0, .5, and 1.
			      The default for P is 1.
			      (See also the help for subroutine CROSSCORR.)
.VARIABLE FILTER
       FILTER=P		      This parameter specifies the
			      power of the power law filter
			      that can be applied in the FFT
			      domain for the correlations.
			      The filter enhances the high
			      spatial frequencies relative 
			      to the low ones, thus helping
			      the correlation key in on edges.
			      The radial power law filter 
			      multiplies both of the FFT's.
			      The filtering is not recommended 
			      for the phase (PHASE=1) corre-
			      lation.  Default is P=0.
			      
.VARIABLE NOPRINT
        'NOPRINT	      This specifies that the normal 
			      printout is to be suppressed.  
			      Error messages will still be
			      printed. 

.VARIABLE NOHPF
       'NOHPF                 This   specifies   that    the 
                              default  high  pass filter  in 
                              the correlation is to be  shut 
                              off.   The  high  pass  filter 
                              consists  of zeroing the  low-
                              order  row  and column of  the 
                              DFT matrices.

.VARIABLE NOSUBPIX
         'NOSUBPIX            This  specifies that  subpixel 
                              correlation   is  not  to   be 
                              calculated (see operation)

.VARIABLE NOCORR
         'NOCORR              This  turns off correlation so 
                              that the estimated points  are 
                              taken   to  be  the   matching 
                              location.  It  is  useful  for 
                              generating  certain  kinds  of 
                              output data sets.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpicmatch.pdf
procedure
! TO RUN ON VMS, TYPE   TSTPICMATCH
! TO RUN ON UNIX OR AXP, MOVE THE TEST FILE TO THE MACHINE FROM THE VAX
! IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
! tstpicmatch DIR=dirname
! where dirname = pathname of directory containing file with trailing / OR
!               = "" if in current directory.
refgbl $echo
refgbl $autousage
refgbl $syschar
body
local DIR string init="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
LOCAL INPIC TYPE=STRING
LOCAL INREF TYPE=STRING
!let $autousage="none"
let _onfail="continue"
let $echo="no"
if ($syschar(1) = "UNIX")
  let DIR="/project/test_work/testdata/mipl/vgr/"
end-if
let INPIC= "&DIR"//"pic.vgr"
let INREF= "&DIR"//"ref.vgr"
!
! test default parameters:
! (tiepoints are from R + RG)
!
picmatch (&INPIC,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4
!
ibis-list tiept.fil
!
! register an image to itself.  We should get same tiepoints and corr=1.0
!
picmatch (&INREF,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(191,329, 411,219,  631,329) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4
!
ibis-list tiept.fil
!
! test 'NOHPF 
picmatch (&INPIC,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4 'NOHPF 
!
ibis-list tiept.fil
!
! test PHASE=0 option:
picmatch (&INPIC,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4 PHASE=0
!
ibis-list tiept.fil
!
! test PHASE & FILTER options:
picmatch (&INPIC,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4 PHASE=.5 FILTER=1
!
ibis-list tiept.fil
!
! test STOP & RETRY params:
picmatch (&INPIC,&INREF) tiept.fil +
  ITIE=(191,329, 411,219,  631,329) +
  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
  MINL=100 MINS=100 MAXL=700 MAXS=700 NAH=4 NAV=4 +
  STOP=(10,.2) RETRY=(5,.2,20)
!
ibis-list tiept.fil
!
! test graphics-1 input from CORNER:
! (03May2010 - lwk - this test removed from procedure because
! program pcopout is no longer on mipl system.)
!corner &INREF point.tab WIDTH=10 GTHRESH=10
!pcopout point.tab point.gra COL=(1,2)
!picmatch (&INPIC,&INREF,point.tab) tiept.fil +
!  ITIE=(191,329, 411,219,  631,329) +
!  OTIE=(275,309, 495,199,  715,309) SEARCH=64 +
!  CONTOUR=(1000,0) phase=0
!
ibis-list tiept.fil
!
end-proc
$ Return
$!#############################################################################
