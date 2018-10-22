C  PROGRAM CONTOUR

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM CONTOUR
C  PURPOSE ---
C
C	Create a graphics file of vectors representing contours 
C	derived from an input "elevation" image.
C
C  INPUT ---
C
C	DIM	Dimension of graphics file (2D or 3D)
C	ZSTART	Starting elevation for contour generation
C	ZEND	Ending elevation for contour generation
C	CONTINT	Contour interval
C	SMOOTH	Smoothing level
C	ZERO	Switch to INCLude or OMIT contouring next to 0
C		valued pixels
C
C  OUTPUT ---
C	A 2D or 3D graphics file is generated
C
C  RESTRICTIONS ---
C
C
C  SUBROUTINES CALLED ---
C	XVCLOSE		Close a file from within VICAR/TAE
C	XVOPEN		Open a file from within VICAR/TAE
C	XVP		Parameter acquisition routine
C	XVSIZE		Get window size of incomming image
C	XVUNIT		Get the unit number
C
C  COMMENTS ---
C
C  HISTORY:
C	1-95   MAC   CRI   MSTP S/W CONVERSION (VICAR PORTING)
C
C
C  MODE DECLARATIONS ---
	INTEGER LINE, SAMP, COUNT, SL, SS, NL, NS, NUMPTS
	INTEGER DIM, STATUS, UNITIN, I, NLI, NSI
	INTEGER LL(10000), BF(10000), CONTINV, ZMIN, ZMAX
	CHARACTER*4 ZERO
	LOGICAL FRINGE

C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

C		+=================+
C		| INITIALIZATIONS |
C		+=================+

        CALL IFMESSAGE('CONTOUR version 02-JAN-95')
	CALL XVUNIT (UNITIN, 'INP', 1, STATUS,' ')
	CALL XVOPEN(UNITIN, STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     *            'U_FORMAT','FULL',' ')
	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)

C		Extract the contouring parameters

	CALL XVP ('DIM',DIM,COUNT)
	CALL XVP ('ZSTART',ZMIN,COUNT)
	CALL XVP ('ZEND',ZMAX, COUNT)
	CALL XVP ('CONTINT',CONTINV,COUNT)
	CALL XVP ('NUMPTS',NUMPTS,COUNT)
	CALL XVP ('ZERO',ZERO,COUNT)
	IF (ZERO(1:4).EQ.'OMIT') THEN
	    FRINGE = .FALSE.
	ELSE
	    FRINGE = .TRUE.
	END IF

C		Open the output graphics file

	CALL WRGR(1,1,DIM) ! Open a graphics output file


C	   ************ Begin EXECUTION *************

C------------------------ C O N T O U R ----------------------------

C	Program Contour

	CALL XVREAD (UNITIN,LL,STATUS,'LINE',SL,
     *		     'SAMP',SS,'NSAMPS',NS,' ')

	DO LINE = SL+1, NL+SL-1
	    CALL XVREAD (UNITIN,BF,STATUS,'LINE',LINE,'SAMP',SS,
     &                   'NSAMPS',NS,' ')
	    DO SAMP = 1, NS-1
		IF (FRINGE) THEN
		    CALL CNTRGRID (LL(SAMP),LL(SAMP+1),
     *		    BF(SAMP),BF(SAMP+1),CONTINV,ZMIN,ZMAX,
     *		    NUMPTS,FLOAT(LINE-1),FLOAT(SAMP+SS-1))
		ELSE
		    IF (.NOT.((LL(SAMP).EQ.0).OR.(LL(SAMP+1).EQ.0).OR.
     *		    (BF(SAMP).EQ.0).OR.(BF(SAMP+1).EQ.0))) THEN
			CALL CNTRGRID (LL(SAMP),LL(SAMP+1),
     *			BF(SAMP),BF(SAMP+1),CONTINV,ZMIN,ZMAX,
     *			NUMPTS,FLOAT(LINE-1),FLOAT(SAMP+SS-1))
		    END IF
		END IF
	    END DO
	    DO I = 1, NS
		LL(I)=BF(I)
	    END DO
	END DO

	CALL XVCLOSE(UNITIN,STATUS,' ')
	CALL CLGR(1)

	RETURN
	END

C ------------- C O N T O U R G R I D (CNTRGRID) --------------------

	SUBROUTINE CNTRGRID (F00,F01,F10,F11,CONTINV,ZMIN,
     *		ZMAX,NUMPTS,XOFF,YOFF)

	IMPLICIT NONE

	INTEGER F00, F01, F10, F11, NUMPTS
	INTEGER NUMPAIRS, BRANCH, POINT, I, CONTINV, ZMIN, ZMAX
	REAL XOFF, YOFF, K, X0, Y0, R, X, Y, TEMP, PHI, PHI1, PHI2
	REAL A, B, C, D, L, XEDGE(4), YEDGE(4)

	A = F10 - F00
	B = F11 - F10 - F01 + F00
	C = F01 - F00
	D = F00

	L = ZMIN + 0.5

	DO WHILE (L.LE.ZMAX)

	    CALL GETECRDS (NUMPAIRS, XEDGE, YEDGE, F00, F01, F10, F11, L)

	    IF (NUMPAIRS .GT. 0) THEN
		IF (B .EQ. 0) THEN
		    CALL PUTGR (1,XEDGE(1)+XOFF,YEDGE(1)+YOFF,L)
		    CALL PUTGR (1,XEDGE(2)+XOFF,YEDGE(2)+YOFF,L)
		    CALL PUTGR (1,0,0,0)
		ELSE
		    X0 = - C/B
		    Y0 = - A/B
		    K = 2*(A*C + B*(L-D)) / (B*B)
		    IF ((NUMPAIRS .EQ. 2) .AND. (K .GT. 0)) THEN
			TEMP = YEDGE(2)
			YEDGE(2) = YEDGE(4)
			YEDGE(4) = TEMP
			TEMP = XEDGE(2)
			XEDGE(2) = XEDGE(4)
			XEDGE(4) = TEMP
		    END IF

		    DO BRANCH = 1, NUMPAIRS
			I = 2*BRANCH - 1
			PHI1 = ATAN2 (YEDGE(I)-Y0, XEDGE(I)-X0)
			PHI2 = ATAN2 (YEDGE(I+1)-Y0, XEDGE(I+1)-X0)

			DO POINT = 0, NUMPTS-1
			    PHI = ((PHI2-PHI1)/(NUMPTS-1))*POINT + PHI1
			    IF (PHI .EQ. 0) PHI = 1.0E-8
			    R = SQRT(ABS( K/SIN(2*PHI) ))
			    X = R*COS(PHI) + X0
			    Y = R*SIN(PHI) + Y0
			    CALL PUTGR (1,X+XOFF,Y+YOFF,L)
			END DO
			CALL PUTGR (1,0.0,0.0,0.0)
		    END DO
	    	END IF
	    END IF
	    L = L + CONTINV
	END DO
	RETURN
	END

C ------------ G E T E D G E C O O R D S (GETECRDS) ------------------

	SUBROUTINE GETECRDS (NUMPAIRS, X, Y, F00, F01, F10, F11, L)

	IMPLICIT NONE

	INTEGER NUMPAIRS, J, F00, F01, F10, F11
	REAL X(4), Y(4), EDGE, L

	J = 0
	EDGE = (L-F00) / (F10-F00+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = EDGE
	    Y(J) = 0
	END IF
	EDGE = (L-F10) / (F11-F10+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = 1
	    Y(J) = EDGE
	END IF
	EDGE = (L-F01) / (F11-F01+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = EDGE
	    Y(J) = 1
	END IF
	EDGE = (L-F00) / (F01-F00+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = 0
	    Y(J) = EDGE
	END IF
	NUMPAIRS = J/2

	RETURN
	END

C  ------------------ E N D   O F   S O U R C E ----------------------
