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
