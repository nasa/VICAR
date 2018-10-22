      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44


      IMPLICIT NONE
      include 'mp_for_defs'  !needed for MP software.  This version (March 1996)
                             ! uses CONVEV, but is part way converted to use
                             ! the MP routines, when they are debugged. It would
                             ! need a call to mp_buf2mpo, and then the call to
                             ! convev can be replaced with calls to mp_xy2ll and
                             ! mp_ll2xy.
      INTEGER*4 istat                    !for mp routines
      REAL*8 mp
	INTEGER COUNT, DEF, DEF1, DEF2
	INTEGER SKIP
	REAL	X, Y, EXTRA(40)
	REAL	ERAD, PRAD
	INTEGER	FLAG, MAPTYPE, MODE, STATUS
	REAL	LINE, SAMP, LAT, LONG
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG
	LOGICAL EOF, ZERO,  XVPTST
        INTEGER WRGR,  RDGR, GETGR, PUTGR, CLGR

	CHARACTER*12  PLANET
        REAL RDATA(40)
        INTEGER IDATA(40)
        EQUIVALENCE (IDATA(1),RDATA(1))

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +		        SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG



        CALL XVMESSAGE('POLYPMAP version Mar 8 1996',' ')
        CALL INIT_SPICE

        call mp_init( mp,istat)
        if(istat.ne.mp_success) call mabend('error in mp_init')

C		Get the parameters

	IF (XVPTST('MERCATOR')) THEN
	    MAPTYPE = 6
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'MERCATOR',istat)
	ELSE IF (XVPTST('LAMBERT')) THEN
	    MAPTYPE = 5
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'LAMBERT_CONFORMAL',istat)
	ELSE IF (XVPTST('CYLINDRI')) THEN
	    MAPTYPE = 9
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'NORMAL_CYLINDRICAL',istat)
	ELSE IF (XVPTST('RECTANGU')) THEN
	    MAPTYPE = 10
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'SIMPLE_CYLINDRICAL',istat)
	ELSE IF (XVPTST('POLSTERE')) THEN
	    MAPTYPE = 3
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'POLAR_STEREOGRAPHIC',istat)
	ELSE IF (XVPTST('STEREOGR')) THEN
	    MAPTYPE = 4
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_STEREOGRAPHIC',istat)
	ELSE IF (XVPTST('POLORTHO')) THEN
	    MAPTYPE = 1
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'POLAR_ORTHOGRAPHIC',istat)
	ELSE IF (XVPTST('ORTHOGRA')) THEN
	    MAPTYPE = 2
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_ORTHOGRAPHIC',istat)
	ELSE IF (XVPTST('OBLICYL')) THEN
	    MAPTYPE = 11
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_SIMPLE_CYLINDRICAL',istat)
	ELSE IF (XVPTST('SINUSOID')) THEN
	    MAPTYPE = 12
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'SINUSOIDAL',istat)
	ELSE IF (XVPTST('OBSINUSO')) THEN
	    MAPTYPE = 13
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_SINUSOIDAL',istat)
	ELSE IF (XVPTST('MOLLWEID')) THEN
	    MAPTYPE = 14
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'MOLLWEIDE',istat)
	ELSE IF (XVPTST('TMERCATO')) THEN
	    MAPTYPE = 15
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'TRANSVERSE_MERCATOR',istat)
	ENDIF


	IF (XVPTST('INVERSE')) THEN
	    MODE = 2
	ELSE
	    MODE = 1
	ENDIF

	CALL XVP ('PLANET', PLANET, COUNT)
	CALL GETPLANETSIZE (PLANET, ERAD, PRAD)
	EQUARADIUS = DBLE(ERAD)
	POLRADIUS = DBLE(PRAD)

	CALL XVPARMD ('SCALE', PIXSCALE, COUNT, DEF,  1)
	CALL XVPARMD ('LATITUDE', SPECLAT, COUNT, DEF,  1)
	CALL XVPARMD ('LONGITUD', SPECLONG, COUNT, DEF,  1)
	CALL XVPARMD ('PARAL1', PARALLEL1, COUNT, DEF,  1)
	CALL XVPARMD ('PARAL2', PARALLEL2, COUNT, DEF,  1)
	CALL XVPARMD ('NORTHANG', NORTHANG, COUNT, DEF,  1)
	CALL XVPARMD ('LINE', SPECLINE, COUNT, DEF,  1)
	CALL XVPARMD ('SAMPLE', SPECSAMP, COUNT, DEF,  1)

	IF (XVPTST('SOUTH')) THEN
	    POLEFLAG = -1.0D0
	ELSE
	    POLEFLAG = +1.0D0
	ENDIF
	IF (MAPTYPE .EQ. 5) THEN
	    POLEFLAG = DSIGN (1.0D0, MIN(PARALLEL1,PARALLEL2) )
	ENDIF

	CALL XVP ('SKIP', SKIP, COUNT)


C...Set up the MP BUFFER.

       IDATA(39)=MAPTYPE      ! MAP PROJECTION TYPE
       RDATA(1)=SPECSAMP   ! SPECIAL SAMPLE
       RDATA(2)=SPECLINE   ! SPECIAL LINE
       RDATA(3)=SPECLAT    ! SPECIAL LATITUDE DEGREES
       RDATA(4)=PARALLEL1  ! LAT OF NORTH PARALLEL DEGREES (LAMBERT CASE)
       RDATA(5)=PARALLEL2  ! LAT OF SOUTH PARALLEL DEGREES (LAMBERT CASE)
       RDATA(6)=SPECLONG   ! SPECIAL LONGITUDE DEGREES
       RDATA(7)=PIXSCALE   ! SCALE KM/PXL
       RDATA(8)=POLEFLAG! VISIBLE POLE +1 FOR NORTH -1 FOR SOUTH
C       SPECIAL CASE FOR LAMBERT
       IF(MAPTYPE.EQ.5)THEN
         IF(PARALLEL1/PARALLEL2.LT.0.)  CALL mabend(
     .      'FOR LAMBERT BOTH PARALLELS MUST ON SAME SIDE OF EQUATOR')
       ENDIF
       RDATA(9)=NORTHANG      ! NORTH ANGLE DEGREES
       RDATA(25)=PRAD         ! POLAR RADIUS KM
       RDATA(26)=ERAD         ! EQUATORIAL RADIUS


	IF (XVPTST('AUTOMAT')) THEN
	    CALL AUTOPLACE(RDATA)
	ENDIF

C		Open the input graphics 1 file
	STATUS = RDGR (1, 1, SKIP+2)
        if (status.ne.1) call signalgr(1,status,1)

C		Open the output graphics 1 file
	STATUS = WRGR (1, 2, SKIP+2)
        if (status.ne.1) call signalgr(2,status,1)



C	    Main loop through input graphics file

	DO WHILE (.TRUE.)
	    STATUS = GETGR (1, ZERO, EOF, X, Y, EXTRA)
            if (status.ne.1) call signalgr(1,status,1)
	    IF (EOF) GOTO 90
	    IF (X .NE. 0.0 .OR. Y .NE. 0.0) THEN
                
		IF (MODE .EQ. 1) THEN
		    LAT = X
		    LONG = Y
		ELSE
		    LINE = X
		    SAMP = Y
		ENDIF
		CALL MAPPROJ (LAT, LONG, LINE, SAMP,RDATA)
		IF (MODE .EQ. 1) THEN
		    X = LINE
		    Y = SAMP
		ELSE
		    X = LAT
		    Y = LONG
		ENDIF
	    ENDIF
	    STATUS = PUTGR (2, X, Y, EXTRA)
            if (status.ne.1) call signalgr(2,status,1)
	ENDDO

90	CONTINUE
	STATUS = CLGR (1)
        if (status.ne.1) call signalgr(1,status,1)
	STATUS = CLGR (2)
        if (status.ne.1) call signalgr(2,status,1)

        call mp_free(mp)
	RETURN
	END




	SUBROUTINE MAPPROJ (LAT, LONG, LINE, SAMP,RDATA)
	IMPLICIT NONE
	REAL	LINE, SAMP, LAT, LONG, RDATA(40)
        REAL    DUMMY       ! POLYPMAP DOES NOT SUPPORT IMAGE SPACE
	INTEGER	FLAG, MAPTYPE, MODE
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +			SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG

c==================================================================

        CALL CONVEV(FLAG, RDATA,RDATA, LINE,SAMP, LAT,LONG,MODE,DUMMY)
	IF (FLAG .NE. 0) THEN     ! IF POINT OFF PLANET, SET OUTPUT TO 0.
	    IF (MODE .EQ. 1) THEN
		LINE = 0.0
		SAMP = 0.0
	    ELSE
		LAT = 0.0
		LONG = 0.0
	    ENDIF
	ENDIF

	RETURN
	END




	SUBROUTINE AUTOPLACE(RDATA)
	IMPLICIT NONE
	INTEGER	COUNT, MODE, MAPTYPE
	REAL	LATRANGE(2), LONRANGE(2), SPLINE, SPSAMP
	REAL	MINLINE, MINSAMP, MAXLINE, MAXSAMP
	REAL	LINE, SAMP, LAT, LONG, RDATA(40)
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG
	CHARACTER*72 STRING

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +			SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG

C==================================================================
	CALL XVP ('LATRANGE', LATRANGE, COUNT)
	CALL XVP ('LONRANGE', LONRANGE, COUNT)
	CALL XVP ('LINE', SPLINE, COUNT)
	CALL XVP ('SAMPLE', SPSAMP, COUNT)
	IF (MODE .EQ. 2) THEN
	    CALL XVMESSAGE (
     +          'Auto placement not available in inverse mode.',' ')
	    CALL ABEND
	ENDIF

	SPECLINE = 0.0
	SPECSAMP = 0.0
        RDATA(1)=SPECSAMP   ! Update the values in rdata accordingly.
        RDATA(2)=SPECLINE   ! SPECIAL LINE
	MINLINE = +1.0E30
	MAXLINE = -1.0E30
	MINSAMP = +1.0E30
	MAXSAMP = -1.0E30

	LAT = LATRANGE(1)
	DO WHILE (LAT .LE. LATRANGE(2))
	    CALL MAPPROJ  (LAT, LONRANGE(1), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LAT = LAT + (LATRANGE(2)-LATRANGE(1))/10.
	ENDDO
	LAT = LATRANGE(1)
	DO WHILE (LAT .LE. LATRANGE(2))
	    CALL MAPPROJ  (LAT, LONRANGE(2), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LAT = LAT + (LATRANGE(2)-LATRANGE(1))/10.
	ENDDO

	    CALL MAPPROJ  (LATRANGE(2), LONRANGE(2), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)  ! Make double sure this corner
	    MINSAMP = MIN (MINSAMP, SAMP)  ! is not skipped due to rounding.
	    MAXSAMP = MAX (MAXSAMP, SAMP)

	LONG = LONRANGE(1)
	DO WHILE (LONG .LE. LONRANGE(2))
	    CALL MAPPROJ  (LATRANGE(1), LONG, LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LONG = LONG + (LONRANGE(2)-LONRANGE(1))/10.
	ENDDO
	LONG = LONRANGE(1)
	DO WHILE (LONG .LE. LONRANGE(2))
	    CALL MAPPROJ  (LATRANGE(2), LONG, LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LONG = LONG + (LONRANGE(2)-LONRANGE(1))/10.
	ENDDO

	SPECLINE = DBLE( SPLINE - MINLINE )
	SPECSAMP = DBLE( SPSAMP - MINSAMP )
	MAXLINE = MAXLINE + SPECLINE
	MAXSAMP = MAXSAMP + SPECSAMP
	WRITE (STRING, '(A,F10.2,1X,F10.2)' ) 
     +		'Special line and sample :',	SPECLINE, SPECSAMP
	CALL XVMESSAGE (STRING,' ')
	WRITE (STRING, '(A,F10.2,1X,F10.2)' ) 
     +		'Max line and sample :',	MAXLINE, MAXSAMP
	CALL XVMESSAGE (STRING,' ')

c..Update the values in rdata accordingly.

        RDATA(1)=SPECSAMP   ! SPECIAL SAMPLE
        RDATA(2)=SPECLINE   ! SPECIAL LINE

	RETURN
	END




	SUBROUTINE GETPLANETSIZE (PLANET, EQRADIUS, POLRADIUS)
	IMPLICIT NONE
	CHARACTER*12 PLANET
	REAL	EQRADIUS, POLRADIUS
	INTEGER	I
	REAL	PBBUFFER(20)
C==================================================================
	CALL PBDATA (planet, PBBUFFER, *999)
	EQRADIUS = PBBUFFER(1)
	POLRADIUS = PBBUFFER(3)

	RETURN

999	CONTINUE
	CALL XVMESSAGE (' '//PLANET//
     +                  ' is not an implemented planet.',' ')
	CALL ABEND
	RETURN
	END


