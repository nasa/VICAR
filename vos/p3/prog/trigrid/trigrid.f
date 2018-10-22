	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C					    SIZE CONSIDERATIONS
C			Presently, this program will handle up 	to 1000 
C			tiepoints.  To alter this value, subroutines 
C			READ_PICREGTP and READ_ENVITP must be changed to read 
C			the new maximum number of tiepoints, and all COMMON 
C			blocks changed.  Each of the arrays in 	/TIEPTS/ should
C			be the new maximum, while XXX should be	the square of 
C			that value.
C
	COMMON /WORKBUF/ XXX(1000000)
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	CHARACTER*80 PRT
	INTEGER*2 IXXX(2000000)
	EQUIVALENCE (XXX,IXXX)
C						    get parameters and tiepoints
	CALL XVPARM('NLIN',NLIN,ICNT,IDEF,0)
	CALL XVPARM('NSIN',NSIN,ICNT,IDEF,0)
	CALL XVPARM('TOPPTS',NTOP,ICNT,IDEF,0)
	CALL XVPARM('SIDEPTS',NSIDE,ICNT,IDEF,0)
	CALL XVPARM('ENVI',PRT,ICNT,IDEF,0)
	IF (IDEF .EQ. 0) THEN
	    CALL READ_ENVITP(PRT,NPTS)
	ELSE
	    CALL READ_PICREGTP(NPTS)
	END IF
	CALL XVMESSAGE(
     +		'  Point New Line New Samp      Old Line Old Samp',' ')
	DO I=1,NPTS
	    WRITE (PRT,100) I,XNULINE(I),XNUSAMP(I),OLDLINE(I),
     +			    OLDSAMP(I)
  100	    FORMAT(I6,F10.1,F8.1,F14.1,F8.1)
	    CALL XVMESSAGE(PRT,' ')
	END DO
C						   construct synthetic tiepoints
C						   around border
	CALL XVMESSAGE(' The following synthetic tiepoints were added:',
     +			' ')
	DX = (NSIN-1.0)/(NTOP-1.0)
	DY = (NLIN-1.0)/(NSIDE-1.0)
	DO I=1,NTOP						! top & bottom
	    J = NPTS + I
	    OLDLINE(J) = 1.0
	    OLDSAMP(J) = DX*(I-1) + 1.0
	    CALL FIND_NEW_PT(NPTS,J)
	END DO
	DO I=1,NTOP
	    J = NPTS + NTOP + I
	    OLDLINE(J) = NLIN
	    OLDSAMP(J) = DX*(I-1) + 1.0
	    CALL FIND_NEW_PT(NPTS,J)
	END DO
	DO I=2,NSIDE-1						! sides
	    J = NPTS + 2*NTOP + I - 1
	    OLDLINE(J) = DY*(I-1) + 1.0
	    OLDSAMP(J) = 1.0
	    CALL FIND_NEW_PT(NPTS,J)
	END DO
	DO I=2,NSIDE-1
	    J = NPTS + 2*NTOP + NSIDE + I - 3
	    OLDLINE(J) = DY*(I-1) + 1.0
	    OLDSAMP(J) = NSIN
	    CALL FIND_NEW_PT(NPTS,J)
	END DO
C						report the synthetic tiepoints
	NPTS = NPTS + 2*NTOP + 2*(NSIDE-2)
C
	NSEGS = (NPTS*(NPTS-1))/2
	NSEGSAV = NSEGS
	CALL FORM_SEGMENTS(NPTS,NSEGS,IXXX(1),IXXX(NSEGS+1),
     +			   XXX(NSEGS+1))
	CALL RESORT_SEGMENTS(NSEGS,IXXX,IXXX(NSEGS+1),IXXX(2*NSEGS+1),
     +			     IXXX(3*NSEGS+1),IXXX(NSEGSAV+1))
	CALL FORM_TRIANGLES(NSEGS,NTRI,IXXX(1),IXXX(NSEGS+1),
     +			IXXX(2*NSEGS+1),IXXX(4*NSEGS+1),IXXX(5*NSEGS+1),
     +			IXXX(6*NSEGS+1),XXX((7*NSEGS+3)/2))
	CALL WRITE_COEFFS(NTRI,IXXX(4*NSEGS+1),IXXX(5*NSEGS+1),
     +			  IXXX(6*NSEGS+1))
	RETURN
	END
C***************************************************************************
	SUBROUTINE READ_ENVITP(FILENAME,NPTS)
C
C	READ_ENVITP reads in the tiepoints from a tiepoint dataset gererated
C	in ENVI.  That is, it expects an ASCII file of four columns, ordered 
C	(NS NL OS OL).
C
	CHARACTER*80 FILENAME
	COMMON /WORKBUF/ XXX(1000000)
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	OPEN (51,FILE=FILENAME,STATUS='OLD')
	NPTS = 1
  100	CONTINUE
	    READ (51,*,END=900,ERR=100) XNUSAMP(NPTS),XNULINE(NPTS),
     +					OLDSAMP(NPTS),OLDLINE(NPTS)
	    NPTS = NPTS +1
	    GO TO 100
  900	CONTINUE
	NPTS = NPTS - 1
	CLOSE (51)
	RETURN
	END
C******************************************************************************
	SUBROUTINE READ_PICREGTP(NPTS)
C
C	READ_PICREGTP reads in the tiepoints from a tiepoint dataset gererated
C	by PICREG (First output dataset).  That is, it expects records of
C	800 real values in the form of 200 sets of (NL, NS, OL, OS). The
C	string of tiepoints is terminated by NL=NS=0.0 
C
	COMMON /WORKBUF/ XXX(1000000)
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C
	NPTS = 0
	DO I=1,5
	    CALL XVREAD(INUNIT,XXX,ISTAT,' ')
	    DO J=1,800,4
C
		IF (XXX(J) .EQ. 0.0 .AND. XXX(J+1) .EQ. 0.0) RETURN
		NPTS = NPTS+1
		XNULINE(NPTS) = XXX(J)
		XNUSAMP(NPTS) = XXX(J+1)
		OLDLINE(NPTS) = XXX(J+2)
		OLDSAMP(NPTS) = XXX(J+3)
	    END DO
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FIND_NEW_PT(NPTS,NEWPT)
C
C	This routine computes the NewLine and NewSample of a synthetic tiepoint.
C	It does this by finding the three closest real tiepoints to the desired
C	OldLine and OldSample, and using the linear transformation defined by
C	those three points to compute the new tiepoint output location.
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	REAL*4 C(6)
	CHARACTER*80 PRT
C
	DISTSQ(Y1,X1,Y2,X2) = (Y2-Y1)*(Y2-Y1) + (X2-X1)*(X2-X1)
C
C						      find 3 closest tiepoints;
C						      mark them LOC1, LOC2, LOC3
	DX1 = 1E10
	DX2 = 1E10
	DX3 = 1E10
	LOC1 = 0
	LOC2 = 0
	LOC3 = 0
	REFLINE = OLDLINE(NEWPT)
	REFSAMP = OLDSAMP(NEWPT)
C
	DO I=1,NPTS
	    DX = DISTSQ(OLDLINE(I),OLDSAMP(I),REFLINE,REFSAMP)
	    IF (DX.LT.DX1) THEN
		DX3 = DX2
		DX2 = DX1
		DX1 = DX
		LOC3 = LOC2
		LOC2 = LOC1
		LOC1 = I
	    ELSE IF (DX.LT.DX2) THEN
		DX3 = DX2
		DX2 = DX
		LOC3 = LOC2
		LOC2 = I
	    ELSE IF (DX.LT.DX3) THEN
		DX3 = DX
		LOC3 = I
	    END IF
	END DO
C							get coeffs to compute
C							XNULINE and XNUSAMP
  100	CONTINUE
	CALL GET_COEFFS(LOC1,LOC2,LOC3,C)
	DETERM = C(1)*C(5) - C(2)*C(4)
	IF (DETERM .EQ. 0.0) THEN
C				the 3 points were colinear, and the calculation
C				of XNULINE and XNUSAMP will fail.  Drop the most
C				distant of the 3 points and replace with the
C				next closest point.
C
	    DX3A = DX3
	    DX3 = 1E10
	    DO I=1,NPTS
		DX = DISTSQ(OLDLINE(I),OLDSAMP(I),REFLINE,REFSAMP)
		IF (DX.GT.DX3A .AND. DX.LT.DX3) THEN
		    DX3 = DX
		    LOC3 = I
		END IF
	    END DO
	    GO TO 100
	END IF
C							compute XNULINE, XNUSAMP
C
	XNULINE(NEWPT) = (C(5)*OLDLINE(NEWPT) - C(2)*OLDSAMP(NEWPT) +
     +			C(2)*C(6) - C(3)*C(5)) / (C(1)*C(5) - C(2)*C(4))
	IF (C(5) .NE. 0.0) THEN
	    XNUSAMP(NEWPT) = (OLDSAMP(NEWPT)-C(6)-C(4)*XNULINE(NEWPT)) /
     +                        C(5)
	ELSE
	    XNUSAMP(NEWPT) = (OLDLINE(NEWPT)-C(3)-C(1)*XNULINE(NEWPT)) /
     +                        C(2)
	END IF
C							    report the new point
	WRITE (PRT,200) NEWPT,XNULINE(NEWPT),XNUSAMP(NEWPT),
     +			OLDLINE(NEWPT),OLDSAMP(NEWPT),LOC1,LOC2,LOC3
  200	    FORMAT(I6,F10.1,F8.1,F14.1,F8.1,'     from  ',3I5)
	CALL XVMESSAGE(PRT,' ')
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE GET_COEFFS(I1,I2,I3,C)
C
C	This routine computes the coefficients to solve the equations
C		OL = C(1)*NL + C(2)*NS + C(3)
C		OS = C(4)*NL + C(5)*NS + C(6)
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	REAL*4 C(6)
C							    compute coefficients
	IF (XNUSAMP(I1).EQ.XNUSAMP(I2) .AND. 
     +	    XNUSAMP(I1).EQ.XNUSAMP(I3)) THEN
	    C(1) = 0.0
	    C(2) = 0.0
	    C(4) = 0.0
	    C(5) = 0.0
	ELSE IF (XNULINE(I1).EQ.XNULINE(I2) .AND. 
     +	    XNULINE(I1).EQ.XNULINE(I3)) THEN
	    C(1) = 0.0
	    C(2) = 0.0
	    C(4) = 0.0
	    C(5) = 0.0
	ELSE IF (XNUSAMP(I1) .EQ. XNUSAMP(I2)) THEN
	    C(1) = (OLDLINE(I1)-OLDLINE(I2)) / (XNULINE(I1)-XNULINE(I2))
	    C(2) = ((OLDLINE(I1)-OLDLINE(I3))-(XNULINE(I1)-XNULINE(I3))*
     +		   C(1)) / (XNUSAMP(I1)-XNUSAMP(I3))
	    C(4) = (OLDSAMP(I1)-OLDSAMP(I2)) / (XNULINE(I1)-XNULINE(I2))
	    C(5) = ((OLDSAMP(I1)-OLDSAMP(I3))-(XNULINE(I1)-XNULINE(I3))*
     +		   C(4)) / (XNUSAMP(I1)-XNUSAMP(I3))
	ELSE IF (XNUSAMP(I1) .EQ. XNUSAMP(I3)) THEN
	    C(1) = (OLDLINE(I1)-OLDLINE(I3)) / (XNULINE(I1)-XNULINE(I3))
	    C(2) = ((OLDLINE(I1)-OLDLINE(I2))-(XNULINE(I1)-XNULINE(I2))*
     +		   C(1)) / (XNUSAMP(I1)-XNUSAMP(I2))
	    C(4) = (OLDSAMP(I1)-OLDSAMP(I3)) / (XNULINE(I1)-XNULINE(I3))
	    C(5) = ((OLDSAMP(I1)-OLDSAMP(I2))-(XNULINE(I1)-XNULINE(I2))*
     +		   C(4)) / (XNUSAMP(I1)-XNUSAMP(I2))
	ELSE IF (XNULINE(I1) .EQ. XNULINE(I2)) THEN
	    C(2) = (OLDLINE(I1)-OLDLINE(I2)) / (XNUSAMP(I1)-XNUSAMP(I2))
	    C(1) = ((OLDLINE(I1)-OLDLINE(I3))-(XNUSAMP(I1)-XNUSAMP(I3))*
     +		   C(2)) / (XNULINE(I1)-XNULINE(I3))
	    C(5) = (OLDSAMP(I1)-OLDSAMP(I2)) / (XNUSAMP(I1)-XNUSAMP(I2))
	    C(4) = ((OLDSAMP(I1)-OLDSAMP(I3))-(XNUSAMP(I1)-XNUSAMP(I3))*
     +		   C(5)) / (XNULINE(I1)-XNULINE(I3))
	ELSE IF (XNULINE(I1) .EQ. XNULINE(I3)) THEN
	    C(2) = (OLDLINE(I1)-OLDLINE(I3)) / (XNUSAMP(I1)-XNUSAMP(I3))
	    C(1) = ((OLDLINE(I1)-OLDLINE(I2))-(XNUSAMP(I1)-XNUSAMP(I2))*
     +		   C(2)) / (XNULINE(I1)-XNULINE(I2))
	    C(5) = (OLDSAMP(I1)-OLDSAMP(I3)) / (XNUSAMP(I1)-XNUSAMP(I3))
	    C(4) = ((OLDSAMP(I1)-OLDSAMP(I2))-(XNUSAMP(I1)-XNUSAMP(I2))*
     +		   C(5)) / (XNULINE(I1)-XNULINE(I2))
	ELSE
	    C(1) = ((OLDLINE(I1)-OLDLINE(I2))*(XNUSAMP(I1)-XNUSAMP(I3))-
     +		   (OLDLINE(I1)-OLDLINE(I3))*(XNUSAMP(I1)-XNUSAMP(I2)))/
     +		   ((XNULINE(I1)-XNULINE(I2))*(XNUSAMP(I1)-XNUSAMP(I3))-
     +		   (XNULINE(I1)-XNULINE(I3))*(XNUSAMP(I1)-XNUSAMP(I2)))
	    C(2) = ((OLDLINE(I1)-OLDLINE(I2))-(XNULINE(I1)-XNULINE(I2))*
     +		   C(1)) / (XNUSAMP(I1)-XNUSAMP(I2))
	    C(4) = ((OLDSAMP(I1)-OLDSAMP(I2))*(XNUSAMP(I1)-XNUSAMP(I3))-
     +		   (OLDSAMP(I1)-OLDSAMP(I3))*(XNUSAMP(I1)-XNUSAMP(I2)))/
     +		   ((XNULINE(I1)-XNULINE(I2))*(XNUSAMP(I1)-XNUSAMP(I3))-
     +		   (XNULINE(I1)-XNULINE(I3))*(XNUSAMP(I1)-XNUSAMP(I2)))
	    C(5) = ((OLDSAMP(I1)-OLDSAMP(I2))-(XNULINE(I1)-XNULINE(I2))*
     +		   C(4)) / (XNUSAMP(I1)-XNUSAMP(I2))
	END IF
	C(3) = OLDLINE(I1) - C(1)*XNULINE(I1) - C(2)*XNUSAMP(I1)
	C(6) = OLDSAMP(I1) - C(4)*XNULINE(I1) - C(5)*XNUSAMP(I1)
C
	RETURN
	END
C**************************************************************************
	SUBROUTINE FORM_SEGMENTS(NPTS,NSEGS,ISTART,IEND,SQLEN)
C
C	This routine forms the line segments that comprise the grid of triangles
C	that will be used to define each local transformation.  This is done by
C	the following steps:
C		(1) The distance between each pair of tiepoints is computed.
C		    This is the length of the corresponding line segment.
C		(2) The segments are sorted by length.
C		(3) If any pair of line segments cross, the longer segment is
C		    removed.
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	REAL*4 SQLEN(NSEGS)
	LOGICAL CROSS
	INTEGER*2 ISTART(NSEGS),IEND(NSEGS)
	CHARACTER*80 PRT
C							      statement function
	DISTSQ(Y1,X1,Y2,X2) = (Y2-Y1)*(Y2-Y1) + (X2-X1)*(X2-X1)
C						      report number of tiepoints
	WRITE (PRT,100) NPTS
  100	FORMAT(I8,' tiepoints')
	CALL XVMESSAGE(PRT,' ')
C						compute lengths and store ends
C						and length**2
	N = 1
	DO I=1,NPTS-1
	    DO J=I+1,NPTS
		ISTART(N) = I
		IEND(N) = J
		SQLEN(N) = DISTSQ(OLDLINE(I),OLDSAMP(I),
     +				  OLDLINE(J),OLDSAMP(J))
		IF (SQLEN(N) .LT. 1.0) THEN
		    WRITE (PRT,150) I,J
  150		    FORMAT(' Tiepoints',I4,' and',I4,
     +			   ' map from the same location')
		    CALL XVMESSAGE(PRT)
		    CALL ABEND
		END IF
		N = N + 1
	    END DO
	END DO
C					       sort semgents by ascending length
	CALL SORT_SEGMENTS(NSEGS,SQLEN,ISTART,IEND)
	LOC = 1					! remove all segments that cross
	DO I=2,NSEGS				! a shorter segment
	    DO J=LOC,1,-1
		IF (CROSS(OLDLINE,OLDSAMP,ISTART,IEND,I,J)) GO TO 200
	    END DO
	    DO J=LOC,1,-1
		IF (CROSS(XNULINE,XNUSAMP,ISTART,IEND,I,J)) THEN
		    WRITE (PRT,180) ISTART(I),IEND(I),ISTART(J),IEND(J)
  180		    FORMAT('Lines from tiepoints',I3,' -',I3,' &',I3,
     +		   ' -',I3,' cross in new image, but not in raw image')
		    CALL XVMESSAGE(PRT,' ')
		    GO TO 200
		END IF
	    END DO
	    LOC = LOC+1
	    ISTART(LOC) = ISTART(I)
	    IEND(LOC) = IEND(I)
  200	    CONTINUE
	END DO
	NSEGS = LOC
C						report number of segments kept
	WRITE (PRT,300) NSEGS
  300	FORMAT(I8,' line segments formed')
	CALL XVMESSAGE(PRT,' ')
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE SORT_SEGMENTS(NSEGS,SQLEN,ISTART,IEND)
C
C	This routine sorts the NSEGS items in ascending order of SQLEN
C
	REAL*4 SQLEN(NSEGS)
	INTEGER*2 ISTART(NSEGS),IEND(NSEGS)
C
	M = NSEGS/2
	DO WHILE (M.GT.0)
	    K = NSEGS-M
	    J = 1
	    DO WHILE (J.LE.K)
 		I = J
		DO WHILE (I.GE.1)
		    L = I+M
		    IF (SQLEN(I) .GT. SQLEN(L)) THEN
			HOLD = SQLEN(I)
			SQLEN(I) = SQLEN(L)
			SQLEN(L) = HOLD
			IHOLD = ISTART(I)
			ISTART(I) = ISTART(L)
			ISTART(L) = IHOLD
			IHOLD = IEND(I)
			IEND(I) = IEND(L)
			IEND(L) = IHOLD
			I = I-M
		    ELSE
			I = -1
		    END IF
		END DO
		J = J+1
	    END DO
	    M = M/2
	END DO
C
	RETURN
	END
C****************************************************************************
	FUNCTION LEFTRIGHT(X1,Y1,X2,Y2,X3,Y3)
C
C	This function determines whether the third point (X3,Y3) is
C	left (=1) or right of the line formed by the first two points.
C
	REAL PI/3.141593/
C
	ANGLE = ATAN2(Y2-Y1,X2-X1) - ATAN2(Y3-Y1,X3-X1)
	IF (ANGLE.GT.PI) ANGLE=ANGLE-2.0*PI
	IF (ANGLE.LT.-PI) ANGLE=ANGLE+2.0*PI
	IF (ANGLE.GT.0.0) THEN
	    LEFTRIGHT = 1
	ELSE
	    LEFTRIGHT = 2
	END IF
	RETURN
	END
C****************************************************************************
	LOGICAL FUNCTION CROSS(XLINE,SAMP,ISTART,IEND,I1,I2)
C
C	This routine determines whether the line segments I1 and I2 cross
C
	REAL XLINE(*),SAMP(*)
	INTEGER*2 ISTART(*),IEND(*)
C
	CROSS = .FALSE.
C
	X1 = SAMP(ISTART(I1))
	X2 = SAMP(IEND(I1))
	X3 = SAMP(ISTART(I2))
	X4 = SAMP(IEND(I2))
	Y1 = XLINE(ISTART(I1))
	Y2 = XLINE(IEND(I1))
	Y3 = XLINE(ISTART(I2))
	Y4 = XLINE(IEND(I2))
	X12MIN = MIN(X1,X2)
	X12MAX = MAX(X1,X2)
	X34MIN = MIN(X3,X4)
	X34MAX = MAX(X3,X4)
	Y12MIN = MIN(Y1,Y2)
	Y12MAX = MAX(Y1,Y2)
	Y34MIN = MIN(Y3,Y4)
	Y34MAX = MAX(Y3,Y4)
C					if one line is beyond the other, FALSE
	IF (X12MIN .GT. X34MAX) RETURN
	IF (X34MIN .GT. X12MAX) RETURN
	IF (Y12MIN .GT. Y34MAX) RETURN
	IF (Y34MIN .GT. Y12MAX) RETURN
C						prepare to find slopes of lines
	DX1 = X2-X1
	DX2 = X4-X3
	DY1 = Y2-Y1
	DY2 = Y4-Y3
C
	IF (DX1 .EQ. 0.0) THEN				! Seg 1 is vertical
	    IF (DX2 .EQ. 0.0) THEN			!   both segs vertical
		IF (X1.EQ.X3 .AND. Y12MIN.LT.Y34MAX .AND. 
     +		    Y12MAX.GT.Y34MIN) CROSS = .TRUE.
	    ELSE					!   Seg 2 not vertical
		IF ((ISTART(I1).EQ.ISTART(I2)) .OR. 
     +		    (IEND(I1).EQ.IEND(I2)) .OR. 
     +		    (IEND(I1).EQ.ISTART(I2)) .OR.
     +		    (ISTART(I1).EQ.IEND(I2))) RETURN
		Y = Y3 + (DY2/DX2)*(X1-X3)		!     Y is line of cross
		IF ((Y.GE.Y12MIN) .AND. (Y.LE.Y12MAX)) CROSS = .TRUE.
	    END IF
	ELSE						! Seg 1 not vertical
	    IF (DX2 .EQ. 0.0) THEN			!    but Seg 2 vertical
		IF ((ISTART(I1).EQ.ISTART(I2)) .OR. 
     +		    (IEND(I1).EQ.IEND(I2)) .OR. 
     +		    (IEND(I1).EQ.ISTART(I2)) .OR.
     +		    (ISTART(I1).EQ.IEND(I2))) RETURN
		Y = Y1 + (DY1/DX1)*(X3-X1)
		IF ((Y.GE.Y34MIN) .AND. (Y.LE.Y34MAX)) CROSS = .TRUE.
	    ELSE					! Neither seg vertical
		SLOPE1 = DY1/DX1
		SLOPE2 = DY2/DX2
		OFFSET1 = Y1 - SLOPE1*X1
		OFFSET2 = Y3 - SLOPE2*X3
		IF (SLOPE1.EQ.SLOPE2) THEN		! Segs are parallel
		    IF (OFFSET1.EQ.OFFSET2 .AND. X12MIN.LT.X34MAX
     +			.AND. X12MAX.GT.X34MIN) CROSS = .TRUE.
		ELSE					! Segs not parallel
		    IF ((ISTART(I1).EQ.ISTART(I2)) .OR. 
     +			(IEND(I1).EQ.IEND(I2)) .OR. 
     +			(IEND(I1).EQ.ISTART(I2)) .OR.
     +			(ISTART(I1).EQ.IEND(I2))) RETURN
		    X = (OFFSET2-OFFSET1)/(SLOPE1-SLOPE2)  !X is sample of cross
		    IF ((X.GT.X12MIN) .AND. (X.LT.X12MAX) .AND.
     +			(X.GT.X34MIN) .AND. (X.LT.X34MAX)) CROSS =.TRUE.
		END IF
	    END IF
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE RESORT_SEGMENTS(NSEGS,ISTART,IEND,LEFT,IRIGHT,IENDX)
C
C	This routine moves the endpoints of the segments to the front of the
C	work buffer, then sorts by startpoint, then endpoint, in ascending
C	order.
C
	INTEGER*2 ISTART(NSEGS),IEND(NSEGS),LEFT(NSEGS),IRIGHT(NSEGS)
	INTEGER*2 IENDX(NSEGS)
C								move endpoints
	DO I=1,NSEGS
	    IEND(I) = IENDX(I)
	END DO
C							    zero out connections
	DO I=1,NSEGS
	    LEFT(I) = 0
	    IRIGHT(I) = 0
	END DO
C								sort
	M = NSEGS/2
	DO WHILE (M.GT.0)
	    K = NSEGS-M
	    J = 1
	    DO WHILE (J.LE.K)
 		I = J
		DO WHILE (I.GE.1)
		    L = I+M
		    IF (ISTART(I).GT.ISTART(L) .OR. 
     +			(ISTART(I).EQ.ISTART(L) .AND. 
     +			 IEND(I).GT.IEND(L))) 		THEN
			IHOLD = ISTART(I)
			ISTART(I) = ISTART(L)
			ISTART(L) = IHOLD
			IHOLD = IEND(I)
			IEND(I) = IEND(L)
			IEND(L) = IHOLD
			I = I-M
		    ELSE
			I = -1
		    END IF
		END DO
		J = J+1
	    END DO
	    M = M/2
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FORM_TRIANGLES(NSEGS,NTRI,ISTART,IEND,IHAVETRI,
     +				  IVERT1,IVERT2,IVERT3,TOP)
C
C			The segments are now sorted by start vertex number,
C			then end vertex number. The end vertex number is always
C			higher.  Any triangle will consist of two segments with
C			the same start vertex, and a third segment whose start 
C			is the first segment's end, and whose end is the second 
C			segment's end.
C			
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	REAL TOP(NSEGS)
	INTEGER*2 ISTART(NSEGS),IEND(NSEGS),IHAVETRI(2,NSEGS)
	INTEGER*2 IVERT1(NSEGS),IVERT2(NSEGS),IVERT3(NSEGS)
	CHARACTER*80 PRT
C
	NTRI = 0
	DO I=1,NSEGS-2
	    J = I+1
	    DO WHILE (ISTART(I).EQ.ISTART(J) .AND. J.LT.NSEGS)
		K = J+1
		DO WHILE (IEND(I).GT.ISTART(K))
		    K = K+1
		END DO
		DO WHILE (IEND(I).EQ.ISTART(K) .AND. K.LE.NSEGS)
		    IF (IEND(J).EQ.IEND(K)) THEN	  ! a triangle is found;
			I1 = ISTART(I)
			I2 = IEND(I)
			I3 = IEND(J)
			CALL TOPOCHECK(I1,I2,I3,ISIDE)
			JSIDE = 3 - ISIDE
C
C				There can only be one valid triangle on each
C				side of a segment. The following code determines
C				which side of each of the new triangle's 
C				segments the new triangle occupies, checks to
C				see if there is another triangle using that side
C				of each segment, and, if so, WHICH_TRI is called
C				to determine which triangle is to be removed.
C					
			IF (ISIDE .GT. 0) THEN
			    LOC = NTRI+1
			    IF (IHAVETRI(ISIDE,I).NE.0) CALL WHICH_TRI(
     +				NTRI,NSEGS,I1,I2,I3,IHAVETRI(ISIDE,I),
     +				IVERT1,IVERT2,IVERT3,LOC,IHAVETRI)
			    IF (LOC.GT.0 .AND. IHAVETRI(JSIDE,J).NE.0)
     +				CALL WHICH_TRI(NTRI,NSEGS,I1,I3,I2,
     +				IHAVETRI(JSIDE,J),IVERT1,IVERT2,
     +				IVERT3,LOC,IHAVETRI)
			    IF (LOC.GT.0 .AND. IHAVETRI(ISIDE,K).NE.0)
     +				CALL WHICH_TRI(NTRI,NSEGS,I2,I3,I1,
     +				IHAVETRI(ISIDE,K),IVERT1,IVERT2,IVERT3,
     +				LOC,IHAVETRI)
			    IF (LOC.GT.0) THEN
				IVERT1(LOC) = I1
				IVERT2(LOC) = I2
				IVERT3(LOC) = I3
				IHAVETRI(ISIDE,I) = LOC
				IHAVETRI(JSIDE,J) = LOC
				IHAVETRI(ISIDE,K) = LOC
				NTRI = MAX(LOC,NTRI)
			    END IF
			END IF
			K = NSEGS+1
		    ELSE
			K = K+1
		    END IF
		END DO
		J = J+1
	    END DO
	END DO
C					       sort triangles by lowest new line
	DO I=1,NTRI
	    TOP(I)=MIN(XNULINE(IVERT1(I)),XNULINE(IVERT2(I)),
     +		       XNULINE(IVERT3(I)))
	END DO
	CALL SORT_TRIANGLES(NTRI,IVERT1,IVERT2,IVERT3,TOP)
C						      report number of triangles
	WRITE (PRT,100) NTRI
  100	FORMAT(I8,' triangles formed')
	CALL XVMESSAGE(PRT,' ')
	RETURN
	END
C******************************************************************************
	SUBROUTINE WHICH_TRI(NTRI,NSEGS,I1,I2,I3,JOLD,IVERT1,IVERT2,
     +			     IVERT3,LOC,IHAVETRI)
C
	INTEGER*2 JOLD,IHAVETRI(2,*),IVERT1(*),IVERT2(*),IVERT3(*)
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C							statement function
	XLEN(I,J)=SQRT((XNULINE(I)-XNULINE(J))*(XNULINE(I)-XNULINE(J)) +
     +		       (XNUSAMP(I)-XNUSAMP(J))*(XNUSAMP(I)-XNUSAMP(J)))
C						   get old triangle's vertices
	J1 = IVERT1(JOLD)
	J2 = IVERT2(JOLD)
	J3 = IVERT3(JOLD)
C				compute the total lengths of the two sides not
C				held in common by the two triangles.  We want
C				the triangle with the shorter total length.
C				I3 is the vertex not part of the old triangle
C
	XNEW = XLEN(I1,I3) + XLEN(I2,I3)
	IF (J1.NE.I1 .AND. J1.NE.I2) THEN
	    XOLD = XLEN(I1,J1) + XLEN(I2,J1)
	ELSE IF (J2.NE.I1 .AND. J2.NE.I2) THEN
	    XOLD = XLEN(I1,J2) + XLEN(I2,J2)
	ELSE
	    XOLD = XLEN(I1,J3) + XLEN(I2,J3)
	END IF
C
	IF (XNEW.GT.XOLD) THEN				! discard new triangle
	    LOC = -1
	ELSE						! discard old triangle
	    LOC = JOLD					! remove old triangle
	    DO I=1,NSEGS				! segment references
		IF (IHAVETRI(1,I).EQ.JOLD) IHAVETRI(1,I)=0
		IF (IHAVETRI(2,I).EQ.JOLD) IHAVETRI(2,I)=0
	    END DO
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE SORT_TRIANGLES(NTRI,IVERT1,IVERT2,IVERT3,TOP)
C
C	This routine sorts the NTRI items in ascending order of TOP
C
	REAL TOP(NTRI)
	INTEGER*2 IVERT1(NTRI),IVERT2(NTRI),IVERT3(NTRI)
C
	M = NTRI/2
	DO WHILE (M.GT.0)
	    K = NTRI-M
	    J = 1
	    DO WHILE (J.LE.K)
 		I = J
		DO WHILE (I.GE.1)
		    L = I+M
		    IF (TOP(I) .GT. TOP(L)) THEN
			HOLD = TOP(I)
			TOP(I) = TOP(L)
			TOP(L) = HOLD
			IHOLD = IVERT1(I)
			IVERT1(I) = IVERT1(L)
			IVERT1(L) = IHOLD
			IHOLD = IVERT2(I)
			IVERT2(I) = IVERT2(L)
			IVERT2(L) = IHOLD
			IHOLD = IVERT3(I)
			IVERT3(I) = IVERT3(L)
			IVERT3(L) = IHOLD
			I = I-M
		    ELSE
			I = -1
		    END IF
		END DO
		J = J+1
	    END DO
	    M = M/2
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE WRITE_COEFFS(NTRI,IVERT1,IVERT2,IVERT3)
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
C
	REAL OUT(12)
	INTEGER*2 IVERT1(NTRI),IVERT2(NTRI),IVERT3(NTRI)
C								open output
	CALL XVUNIT(IUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     +		    NTRI,'U_NS',12,'U_NB',1,'U_ORG','BSQ','OP','WRITE',
     +		    'O_FORMAT','REAL','U_FORMAT','REAL',' ')
C
C							   for each triangle...
	DO I=1,NTRI
	    N1 = IVERT1(I)
	    N2 = IVERT2(I)
	    N3 = IVERT3(I)
	    CALL SORT_VERT(OUT,XNULINE(N1),XNUSAMP(N1),XNULINE(N2),
     +			   XNUSAMP(N2),XNULINE(N3),XNUSAMP(N3))
	    CALL GET_COEFFS(N1,N2,N3,OUT(7))		 ! get new to old coeffs
	    CALL XVWRIT(IUNIT,OUT,ISTAT,' ')
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE TOPOCHECK(N1,N2,N3,ISIDE)
C
C	This routine compares the new and old triangles, to see if the triangle
C	gets flipped during the transformation. For example, if the path from
C	N1 to N2 to N3 to N1 is clockwise in the new image, it should also be
C	clockwise in the old image. If this is not true, the geometric 
C	transformation will be discontinuous around this triangle. A warning 
C	message is printed whenever a flipped triangle is discovered.  The
C	variable ISIDE returns 1 for left (clockwise) triangles, 2 for right
C	side (counterclockwise) triangles, and -1 for inconsistent triangles.
C
	COMMON /TIEPTS/ XNULINE(1000),XNUSAMP(1000),OLDLINE(1000),
     +			OLDSAMP(1000)
	CHARACTER*80 PRT
C
	ISIDE = LEFTRIGHT(XNULINE(N1),XNUSAMP(N1),XNULINE(N2),
     +			  XNUSAMP(N2),XNULINE(N3),XNUSAMP(N3))
	ISIDEOLD = LEFTRIGHT(OLDLINE(N1),OLDSAMP(N1),OLDLINE(N2),
     +			     OLDSAMP(N2),OLDLINE(N3),OLDSAMP(N3))
	IF (ISIDE .NE. ISIDEOLD) THEN
	    ISIDE = -1
	    CALL XVMESSAGE(
     +		     ' WARNING: Discontinuous rectification around',' ')
	    CALL XVMESSAGE(
     +		    '  Tie  New line  New samp  Old line  Old samp',' ')
	    WRITE (PRT,100) N1,XNULINE(N1),XNUSAMP(N1),OLDLINE(N1),
     +			    OLDSAMP(N1)
	    CALL XVMESSAGE(PRT,' ')
	    WRITE (PRT,100) N2,XNULINE(N2),XNUSAMP(N2),OLDLINE(N2),
     +			    OLDSAMP(N2)
	    CALL XVMESSAGE(PRT,' ')
	    WRITE (PRT,100) N3,XNULINE(N3),XNUSAMP(N3),OLDLINE(N3),
     +			    OLDSAMP(N3)
	    CALL XVMESSAGE(PRT,' ')
  100	    FORMAT(I5,F9.2,3F10.2)
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORT_VERT(OUT,XL1,XS1,XL2,XS2,XL3,XS3)
C
C	This routine puts the line,sample pairs of the vertices into the 
C	array OUT, sorted by ascending line number.
C
	REAL OUT(6)
C
	IF (XL1.LE.XL2 .AND. XL2.LE.XL3) THEN
	    OUT(1) = XL1
	    OUT(2) = XS1
	    OUT(3) = XL2
	    OUT(4) = XS2
	    OUT(5) = XL3
	    OUT(6) = XS3
	ELSE IF (XL1.LE.XL3 .AND. XL3.LE.XL2) THEN
	    OUT(1) = XL1
	    OUT(2) = XS1
	    OUT(3) = XL3
	    OUT(4) = XS3
	    OUT(5) = XL2
	    OUT(6) = XS2
	ELSE IF (XL2.LE.XL1 .AND. XL1.LE.XL3) THEN
	    OUT(1) = XL2
	    OUT(2) = XS2
	    OUT(3) = XL1
	    OUT(4) = XS1
	    OUT(5) = XL3
	    OUT(6) = XS3
	ELSE IF (XL2.LE.XL3 .AND. XL3.LE.XL1) THEN
	    OUT(1) = XL2
	    OUT(2) = XS2
	    OUT(3) = XL3
	    OUT(4) = XS3
	    OUT(5) = XL1
	    OUT(6) = XS1
	ELSE IF (XL3.LE.XL1 .AND. XL1.LE.XL2) THEN
	    OUT(1) = XL3
	    OUT(2) = XS3
	    OUT(3) = XL1
	    OUT(4) = XS1
	    OUT(5) = XL2
	    OUT(6) = XS2
	ELSE
	    OUT(1) = XL3
	    OUT(2) = XS3
	    OUT(3) = XL2
	    OUT(4) = XS2
	    OUT(5) = XL1
	    OUT(6) = XS1
	END IF
	RETURN
	END
