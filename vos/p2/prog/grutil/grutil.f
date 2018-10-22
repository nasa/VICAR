	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE
	INTEGER	COUNT, DEF
	INTEGER	INPCOUNT, OUTCOUNT
	INTEGER	DIM, OUTDIM
	INTEGER	I, GRFILE
	REAL	FIRST, SECOND, THIRD(40)
	REAL	ZEROS(40)/40*0.0/
	REAL	X, Y, X1, Y1, X2, Y2, DX, DY, DIST, INTERVAL, DELTA, T
	LOGICAL	EOF, EOL,   ALLZERO
	CHARACTER*16  OPTION

        
	CALL XVPARM ('OPTION', OPTION, COUNT, DEF, 0)

	CALL XVPARM ('DIM', DIM, COUNT, DEF, 0)
	


	IF (OPTION(1:6) .EQ. 'APPEND') THEN

	    CALL wrgr_grutil (1, 1, DIM)
	    CALL XVPCNT ('INP', INPCOUNT)
	    DO I = 1, INPCOUNT
		CALL rdgr_grutil (I, I+1, DIM)
		EOF = .FALSE.
		DO WHILE (.NOT. EOF)
		    CALL NEXTGR (I+1, EOF, FIRST, SECOND, THIRD)
		    IF (.NOT. EOF) THEN
			EOL = .FALSE.
			DO WHILE (.NOT. EOL)
			    CALL PUTGR (1, FIRST, SECOND, THIRD)
			    CALL GETGR (I+1, EOL, EOF, FIRST, SECOND, THIRD)
			ENDDO
			CALL PUTGR (1, 0.0, 0.0, ZEROS)
		    ENDIF
		ENDDO
		CALL CLGR (I+1)
	    ENDDO
	    CALL CLGR (1)


	ELSE IF (OPTION(1:4) .EQ. 'SWAP') THEN

	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		IF (.NOT. EOF) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (2, SECOND, FIRST, THIRD)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (2, 0.0, 0.0, ZEROS)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    CALL CLGR (2)


	ELSE IF (OPTION(1:6) .EQ. '2DTO3D') THEN

	    CALL wrgr_grutil (1, 1, 3)
	    CALL XVPCNT ('INP', INPCOUNT)
	    DO I = 1, INPCOUNT
		CALL rdgr_grutil (I, I+1, 2)
		THIRD(1) = FLOAT(I)
		EOF = .FALSE.
		DO WHILE (.NOT. EOF)
		    CALL NEXTGR (I+1, EOF, FIRST, SECOND)
		    IF (.NOT. EOF) THEN
			EOL = .FALSE.
			DO WHILE (.NOT. EOL)
			    CALL PUTGR (1, FIRST, SECOND, THIRD)
			    CALL GETGR (I+1, EOL, EOF, FIRST, SECOND)
			ENDDO
			CALL PUTGR (1, 0.0, 0.0, 0.0)
		    ENDIF
		ENDDO
		CALL CLGR (I+1)
	    ENDDO
	    CALL CLGR (1)


	ELSE IF (OPTION(1:6) .EQ. '3DTO2D') THEN

	    CALL rdgr_grutil (1, 1, 3)
	    CALL XVPCNT ('OUT', OUTCOUNT)
	    DO I = 1, OUTCOUNT
		CALL wrgr_grutil (I, I+1, 2)
	    ENDDO
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		GRFILE = INT(THIRD(1))
		IF (.NOT. EOF .AND. 
     +			GRFILE .GE. 1 .AND. GRFILE .LE. OUTCOUNT) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (GRFILE+1, FIRST, SECOND)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (GRFILE+1, 0.0, 0.0)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    DO I = 1, OUTCOUNT
		CALL CLGR (I+1)
	    ENDDO


	ELSE IF (OPTION(1:5) .EQ. 'REDIM') THEN

	    CALL XVPARM ('OUTDIM', OUTDIM, COUNT, DEF, 0)
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, OUTDIM)
	    SECOND = 0.0
	    DO I = 3, OUTDIM
		THIRD(I-2) = 0.0
	    ENDDO
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		IF (.NOT. EOF) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (2, FIRST, SECOND, THIRD)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (2, 0.0, 0.0, ZEROS)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    CALL CLGR (2)


	ELSE IF (OPTION(1:4) .EQ. 'FILL') THEN

	    CALL XVP ('INTERVAL', INTERVAL, COUNT)
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)

	    DO WHILE (.TRUE.)
C			Scan for the beginning of a line string
		X1 = 0.0
		Y1 = 0.0
		DO WHILE (X1 .EQ. 0.0 .AND. Y1 .EQ. 0.0)
		    CALL GETGR (1, ALLZERO, EOF, Y1, X1, THIRD)
		    IF ( EOF ) GO TO 30
		ENDDO
C			Put out the first point in the linestring
		CALL PUTGR (2, Y1, X1, THIRD)
						! Get the next pair
		CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD)
		IF ( EOF ) GO TO 30
		EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
	
		IF (.NOT. EOL ) THEN
		    DO WHILE (.NOT. EOL .AND. .NOT. EOF)
			DY = Y2 - Y1
			DX = X2 - X1
			DIST = SQRT( DY**2 + DX**2 )
			IF (DIST .GT. 0.0) THEN
			    DELTA = INTERVAL/DIST
			ELSE
			    DELTA = 2.0
			ENDIF
				! Fill in the points in between
			T = DELTA
			DO WHILE (T .LT. 1.0)
			    Y = Y1 + T*DY
			    X = X1 + T*DX
			    CALL PUTGR (2, Y, X, THIRD)
			    T = T + DELTA
			ENDDO
			CALL PUTGR (2, Y2, X2, THIRD)   
				! Get the next point in linestring
			Y1 = Y2
			X1 = X2
			CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD)
			EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
			IF ( EOF ) GO TO 30
		    ENDDO
		ENDIF
		CALL PUTGR (2, 0.0, 0.0, ZEROS)
	    ENDDO
   30	    CONTINUE
	    CALL CLGR (1)
	    CALL CLGR (2)



	ELSE IF (OPTION(1:6) .EQ. 'SMOOTH') THEN

	    CALL XVP ('INTERVAL', INTERVAL, COUNT)
	    INTERVAL = INTERVAL**2
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)

	    DO WHILE (.TRUE.)
C			Scan for the beginning of a line string
		X1 = 0.0
		Y1 = 0.0
		DO WHILE (X1 .EQ. 0.0 .AND. Y1 .EQ. 0.0)
		    CALL GETGR (1, ALLZERO, EOF, Y1, X1, THIRD)
		    IF ( EOF ) GO TO 40
		ENDDO
C			Put out the first point in the linestring
		CALL PUTGR (2, Y1, X1, THIRD)

		EOL = .FALSE.
		DO WHILE (.NOT. EOL)
				! Get the next point in linestring
		    CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD )
		    IF ( EOF ) GO TO 40
		    EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
		    IF (.NOT. EOL) THEN
C		       Output if distance from last output point is large enough
			DIST = (Y2-Y1)**2 + (X2-X1)**2
			IF (DIST .GE. INTERVAL) THEN
			    CALL PUTGR (2, Y2, X2, THIRD)
			    Y1 = Y2
			    X1 = X2
			ENDIF
		    ENDIF
		ENDDO
		CALL PUTGR (2, 0.0, 0.0, ZEROS)
	    ENDDO
 40	    CONTINUE
	    CALL CLGR (1)
	    CALL CLGR (2)

	ENDIF

	RETURN
	END



c
c	Code added to check for missing files - fr 088285
c


      subroutine wrgr_grutil(instance,vicarfile,dimension)

      integer instance, vicarfile, dimension
      integer status, wrgr

      status = wrgr(instance,vicarfile,dimension)
      if ( status .ne. 1 ) call signalgr(vicarfile,status,1)

      return
      end

      subroutine rdgr_grutil(instance,vicarfile,dimension)

      integer instance, vicarfile, dimension
      integer status, rdgr

      status = rdgr(instance,vicarfile,dimension)
      if ( status .ne. 1 ) call signalgr(vicarfile,status,1)

      return
      end
