	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C---- VICAR/IBIS PROGRAM OMC
C     6 MAR 95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
	IMPLICIT NONE
	INTEGER	INUNIT, OUTUNIT, STATUS, COUNT, DEF
	INTEGER	CLEN, NCOL, NCOL2, INIBIS, OUTIBIS, INRECORD, OUTRECORD
	INTEGER	MECOL, OMCOL, CCOL, RSCOL, VRCOL
	INTEGER I, J, K, ROW, COL, COLS(15), FULLBUF(1000)
	REAL	REALBUF(1000)
	REAL*8	MEANGLES(3), OMANGLES(3), CANGLES(3)
	REAL*8	ME(3,3), OM(3,3), C(3,3), RS(3), VR(3)
        CHARACTER*16 ORG
        CHARACTER*6 FORMAT(1024)
        CHARACTER*256 CHARBUF(1000)
	LOGICAL	XVPTST, TOPLANET, ERR
	LOGICAL OMCOR
        COMMON/OMC/OMCOR
        EQUIVALENCE (FULLBUF,REALBUF)  !just to save memory, mutually excl.

        CALL IFMESSAGE('OMC version 6-MAR-95')

C	    Get the input parameters

C		First get where everything is in the IBIS columns
	CALL XVP ('RSCOL', RSCOL, COUNT)
	CALL XVP ('OMCOL', OMCOL, COUNT)
	CALL XVP ('MECOL', MECOL, COUNT)
	CALL XVP ('CCOL', CCOL, COUNT)
	CALL XVP ('VRCOL', VRCOL, COUNT)

C -- Copy all of the columns, even those not in the list.
C
C -- Note to those porting this: this trick won't work if NC > 40,
C -- or for IBIS files with non-numeric columns ! You will need
C -- to make the output file have the same format columns as the
C -- input, copy all of the other columns over, and then  process
C -- the columns listed above.
C
C	DO I = 0, 40
C	    COLS(I) = I
C            ROWDATA(I) = 0.0 ! pad with  0's
C	ENDDO

C		Then get the transformation type desired
	TOPLANET = XVPTST('TOPLANET')
	OMCOR = XVPTST('OMCOR')



C	    Multiply the VR vector by the transpose of the ME matrix to 
C	    get the RS vector, and multiply the ME matrix by the C matrix 
C	    to get the OM matrix:
C			__     t __
C			RS = ME *VR      OM = C*ME
C
C	    Multiply the RS vector by the ME matrix to get the VR vector,
C	    and multiply the transpose of the ME matrix by the OM matrix 
C	    to get the C matrix:
C			__      __               t
C			VR = ME*RS      C = OM*ME


        CALL XVUNIT(INUNIT, 'INP', 1, STATUS, ' ')
        CALL XVUNIT(OUTUNIT, 'OUT', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(INUNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
        CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)
        CALL IBIS_FILE_GET(INIBIS,'FORMATS',FORMAT,1,NCOL)
        CALL IBIS_FILE_GET(INIBIS,'ORG',ORG,1,1)


	CALL XVPARM ('NCOL', NCOL2, COUNT, DEF, 1)
	IF (DEF .EQ. 1) NCOL2 = NCOL
	CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL2,CLEN,
     *                      FORMAT,ORG,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        DO COL=1,NCOL
C skip columns that will be updated
           IF (COL.NE.MECOL.AND.COL.NE.OMCOL.AND.COL.NE.CCOL.AND.
     *         COL.NE.RSCOL.AND.COL.NE.VRCOL) THEN
C read into buffer
              IF (FORMAT(COL)(1:1).EQ.'A') THEN
                 CALL IBIS_COLUMN_READ(INIBIS, CHARBUF, COL, 1, CLEN,
     *                                 STATUS)
              ELSE IF (FORMAT(COL)(1:1).EQ.'F') THEN
                 CALL IBIS_COLUMN_READ(INIBIS, FULLBUF, COL, 1, CLEN,
     *                                 STATUS)
              ELSE
                 CALL IBIS_COLUMN_READ(INIBIS, REALBUF, COL, 1, CLEN,
     *                                 STATUS)
              END IF
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
C write to output
              IF (FORMAT(COL)(1:1).EQ.'A') THEN
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, CHARBUF, COL, 1, CLEN,
     *                                  STATUS)
              ELSE IF (FORMAT(COL)(1:1).EQ.'F') THEN
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, FULLBUF, COL, 1, CLEN,
     *                                  STATUS)
              ELSE
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, REALBUF, COL, 1, CLEN,
     *                                  STATUS)
              END IF
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
           END IF
        END DO

C setup which rows go in record
C 1  MECOL       4  OMCOL       7  CCOL       10  RSCOL       13  VRCOL
C 2  MECOL + 1   5  OMCOL + 1   8  CCOL + 1   11  RSCOL + 1   14  VRCOL + 1
C 3  MECOL + 2   6  OMCOL + 2   9  CCOL + 2   12  RSCOL + 2   15  VRCOL + 2

        DO I=1,3
           COLS(I)= MECOL+I-1
           COLS(I+3)= OMCOL+I-1
           COLS(I+6)= CCOL+I-1
           COLS(I+9)= RSCOL+I-1
           COLS(I+12)= VRCOL+I-1
        END DO


        CALL IBIS_RECORD_OPEN(INIBIS,INRECORD,'FORMAT:REAL',
     &                        COLS,15,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INUNIT,STATUS,1)
        CALL IBIS_RECORD_OPEN(OUTIBIS,OUTRECORD,'FORMAT:REAL',
     &                        COLS,15,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTUNIT,STATUS,1)
	DO ROW = 1, CLEN
	    CALL IBIS_RECORD_READ(INRECORD, REALBUF, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    MEANGLES(1) = DBLE(REALBUF(COLS(1)))
	    MEANGLES(2) = DBLE(REALBUF(COLS(2)))
	    MEANGLES(3) = DBLE(REALBUF(COLS(3)))
	    CALL FROMEULER (MEANGLES(1), MEANGLES(2), MEANGLES(3), ME)

	    IF (TOPLANET) THEN
		DO I = 1, 3
		    VR(I) = DBLE(REALBUF(COLS(13+I-1)))
		    CANGLES(I) = DBLE(REALBUF(COLS(7+I-1)))
		ENDDO
		CALL FROMEULER (CANGLES(1), CANGLES(2), CANGLES(3), C)
		DO I = 1, 3
		    RS(I) = 0.0D0
		    DO J = 1, 3
			OM(I,J) = 0.0D0
			RS(I) = RS(I) + ME(J,I)*VR(J)
			DO K = 1, 3
			    OM(I,J) = OM(I,J) + C(I,K)*ME(K,J)
			ENDDO
		    ENDDO
		ENDDO
		CALL TOEULER(OM,OMANGLES(1),OMANGLES(2),OMANGLES(3),ERR)
		IF (ERR)  CALL MABEND ('ILLEGAL ROTATION MATRIX')
		DO I = 1, 3
		    REALBUF(COLS(10+I-1)) = SNGL(RS(I))
		    REALBUF(COLS(4+I-1)) = SNGL(OMANGLES(I))
		ENDDO

	    ELSE

		DO I = 1, 3
		    RS(I) = DBLE(REALBUF(COLS(10+I-1)))
		    OMANGLES(I) = DBLE(REALBUF(COLS(4+I-1)))
		ENDDO
		CALL FROMEULER (OMANGLES(1),OMANGLES(2),OMANGLES(3),OM) 
		DO I = 1, 3
		    VR(I) = 0.0D0
		    DO J = 1, 3
			C(I,J) = 0.0D0
			VR(I) = VR(I) + ME(I,J)*RS(J)
			DO K = 1, 3
			    C(I,J) = C(I,J) + OM(I,K)*ME(J,K)
			ENDDO
		    ENDDO
		ENDDO
		CALL TOEULER (C,CANGLES(1), CANGLES(2), CANGLES(3),ERR)
		IF (ERR)  CALL MABEND ('ILLEGAL ROTATION MATRIX')
		DO I = 1, 3
		    REALBUF(COLS(13+I-1)) = SNGL(VR(I))
		    REALBUF(COLS(7+I-1)) = SNGL(CANGLES(I))
		ENDDO

	    ENDIF
	    CALL IBIS_RECORD_WRITE(OUTRECORD, REALBUF, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	ENDDO

	CALL IBIS_FILE_CLOSE(INIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
	CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

	RETURN
	END



	subroutine FromEuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! Input  - RA of z axis (degrees)
	real*8	delta	    ! Input  - Declination z axis (degrees)
	real*8	kappa	    ! Input  - rotation angle around z axis
 			    !          (3rd Euler angle) (degrees)
	real*8	c(3,3)      ! Output - Derived rotation matrix 
	real*8	cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8	cos_kappa, sin_kappa, d2r
	LOGICAL OMCOR
        COMMON/OMC/OMCOR

c  This routine performs the functional inverse of routine ToEuler.  The
c  three Euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  The 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

        d2r = dacos(-1.0d0)/180.0d0   ! degree to radian conversion PI/180
	sin_alpha = dsin(alpha*d2r)    ! dsin() is dble(sin(radians)
	cos_alpha = dcos(alpha*d2r)
	sin_delta = dsin(delta*d2r)
	cos_delta = dcos(delta*d2r)
	sin_kappa = dsin(kappa*d2r)
	cos_kappa = dcos(kappa*d2r)

	if (OMCOR) then !Use OMCOR euler angles
	c(1,1)= -sin_alpha * cos_kappa - cos_alpha * sin_delta*sin_kappa
	c(1,2)=  cos_alpha * cos_kappa - sin_alpha * sin_delta*sin_kappa
	c(1,3)=  cos_delta * sin_kappa
	c(2,1)=  sin_alpha * sin_kappa - cos_alpha * sin_delta*cos_kappa
	c(2,2)= -cos_alpha * sin_kappa - sin_alpha * sin_delta*cos_kappa
	c(2,3)=  cos_delta * cos_kappa
	c(3,1)=  cos_alpha * cos_delta
	c(3,2)=  sin_alpha * cos_delta
	c(3,3)=  sin_delta
	else
	c(1,1)=  cos_alpha * cos_kappa - sin_alpha * cos_delta*sin_kappa
	c(1,2)=  sin_alpha * cos_kappa + cos_alpha * cos_delta*sin_kappa
	c(1,3)=  sin_delta * sin_kappa
	c(2,1)= -cos_alpha * sin_kappa - sin_alpha * cos_delta*cos_kappa
	c(2,2)= -sin_alpha * sin_kappa + cos_alpha * cos_delta*cos_kappa
	c(2,3)=  sin_delta * cos_kappa
	c(3,1)=  sin_alpha * sin_delta
	c(3,2)= -cos_alpha * sin_delta
	c(3,3)=  cos_delta
	endif

	return
	end




	subroutine ToEuler (c, alpha, delta, kappa, error)
	implicit none
	real*8	c(3,3)      ! Output - Derived rotation matrix 
	real*8	alpha       ! Input  - RA of z axis (degrees)
	real*8	delta	    ! Input  - Declination z axis (degrees)
	real*8	kappa	    ! Input  - rotation angle around z axis
 			    !          (3rd Euler angle) (degrees)
	real*8	collength, rowlength,d2r,r2d
	integer i, j
	logical	error, OMCOR
        COMMON/OMC/OMCOR

c  This routine performs the functional inverse of routine FromEuler.
c  This routine takes an input rotation matrix, and computes the three Euler
c  angles representing the matrix.  (These 3 angles are called Alpha, Delta,
c  and Kappa by Mert Davies etc.)  If the matrix is not a valid rotation
c  matrix (i.e. if the length of the row and column vectors is not within
c  0.0001 of unity) then error is returned true.
c
c  The 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

        d2r = dacos(-1.0d0)/180.0d0   ! degree to radian conversion PI/180
        r2d = 180.0d0/dacos(-1.0d0)   ! degree to radian conversion PI/180

	error = .false.
	do i = 1, 3
	    collength = 0.0d0
	    rowlength = 0.0d0
	    do j = 1, 3
		collength = collength + c(i,j)**2
		rowlength = rowlength + c(j,i)**2
	    enddo
	    if (abs(collength-1.0) .gt. 0.0001) error = .true.
	    if (abs(rowlength-1.0) .gt. 0.0001) error = .true.
	enddo

	if (.not. error) then
	    if (OMCOR) then !Use OMCOR euler angles
		delta = dasin(c(3,3))*r2d
		if (dabs(c(3,1)) .gt. 1e-10) then
			alpha = datan2(c(3,2), c(3,1))*r2d
		else if (dabs(c(3,2)) .lt. 1e-10) then
			alpha = 0.0d0
		else
			alpha = sign(dble(90.0),c(3,2))
		endif
		if (alpha .lt. 0.0) alpha = alpha + 360.0
		if (dabs(c(2,3)) .gt. 1e-10) then
			kappa = datan2(c(1,3), c(2,3))*r2d
		else if (dabs(c(1,3)) .lt. 1e-10) then
			kappa = datan2( -c(1,1), -c(2,1) )*r2d
		else
			kappa = sign(dble(90.0),c(1,3))
		endif
		if (kappa .lt. 0.0) kappa = kappa + 360.0
		if (abs(dcos(delta*d2r)) * dcos(kappa*d2r) - c(2,3) 
     +		   .gt. 0.0001) kappa = 180.0 - kappa
	    else !new method
		delta = dacos(c(3,3))*r2d
		if (dabs(c(3,2)) .lt. 1e-10 .and. 
     +              dabs(c(3,1)) .lt. 1e-10) then
			alpha = 0.0
		else !atan2 well-defined
			alpha = datan2(c(3,1), -c(3,2))*r2d
		endif
		if (alpha .lt. 0.0) alpha = alpha + 360.0
		if (dabs(c(1,3)) .lt. 1e-10 .and. 
     +              dabs(c(2,3)) .lt. 1e-10) then
		    kappa = (datan2(c(1,2), c(1,1))*r2d - alpha)*c(3,3)
		else
			kappa = datan2(c(1,3), c(2,3))*r2d
		endif
		if (kappa .lt. 0.0) kappa = kappa + 360.0
		endif
	endif

	return
	end

