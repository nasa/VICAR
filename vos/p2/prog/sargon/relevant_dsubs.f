c---------------------------------------------------------
c
c
c        open_device
c
c         the devopen call is the first call that must be
c         made to the d routines. it will open the required
c         unit, configure them as required,
c         and activate them so that they can be read to and
c         from. the graphics plane is set to 4 and turned on.
c         a font is read in; character height is specified;
c         and text angle rotation is set to 0.
c
c         calling sequence ( iunit )
c         where :
c                iunit - device logical unit no.
c
          SUBROUTINE OPEN_DEVICE( IUNIT )

          INTEGER  H, W
          INTEGER  CSETUP(4), C, U, IFORM, IBLINK, ICONOFF
          INTEGER  GPLANE, NLUTS, NIMPS, MAXSAMPS, XDSGRAPH
          INTEGER  IGRAPH, NCURS, NINTIO, SECTION, LMAX, SMAX
          INTEGER  PLANE,XDSSECTION
          LOGICAL  CAUTO, FLAG
          INTEGER  XDEACTION, XDDUNIT, XDDOPEN, XDDACTIVATE
          INTEGER  XDDCONFIGURE, XDTFONT, XDTSIZE
          INTEGER  XDTROTATE, XDGON, XDLRAMP, XDLCONNECT
          INTEGER  XDGCONNECT, XDGLINIT
          REAL     S, ANGLE
          
          DATA C / 1 /, CSETUP / 0, 0, 0, 0/
          DATA H / 7 /, S / 1.0 /
          DATA IFORM / 0 /, CAUTO / .true. / 
          DATA IBLINK / 0 /, ICONOFF / 0 / 
          DATA SECTION /1/
          DATA W / 1 / 

          U = IUNIT
c
c         open unit u
c
          IERR = XDEACTION( 2, 2, 3 )
          IERR = XDDUNIT( U )
          IERR = XDDOPEN( U )
c
c         activate the display unit so that we can write on it
c
          FLAG = .TRUE.
          IERR = XDDACTIVATE( U, FLAG )
c
c         now configure the display (csetup is all 0's - default)
c
          IERR = XDDCONFIGURE( U, CSETUP )
c
c         find out what type of device we have
c
          CALL XDDINFO( U, 3, 1, NLUTS )
          CALL XDDINFO( U, 4, 1, NIMPS )
          CALL XDDINFO( U, 5, 1, MAXLINES )
          CALL XDDINFO( U, 6, 1,  MAXSAMPS )
          CALL XDDINFO( U, 30, 1, IGRAPH )
          CALL XDDINFO( U, 34, 1, GPLANE )
          CALL XDDINFO( U, 48, 1, NCURS )
          CALL XDDINFO( U, 60, 1, NINTIO )
          LMAX = MAXLINES 
          SMAX = MAXSAMPS
c
c         read in a font file
c
          IFONT = 1
          IERR  = XDTFONT(IFONT) 
c
c         set the initial size
c
          IF ( LMAX .LE. 512 ) THEN
            H = 7
          ELSE 
            H = 14
          END IF 
          IERR = XDTSIZE(H,S) 
c
c         rotate at 0 degrees 
c
          ANGLE = 0.0
          IERR = XDTROTATE( ANGLE )
c
c         turn on the cursor
c
          IF ( NCURS .GT. 0 ) THEN
           ICONOFF = 1
           IERR = XDCON( U, C, IFORM, IBLINK ) 
          END IF 
c
c         and the ramps
c
          DO N1 = 1, NLUTS 
           NSECTION = XDSSECTION( U, N1)
           IERR = XDGLINIT( U, NSECTION)
           IERR = XDLRAMP ( U, N1, NSECTION )
           PLANE = XDSGRAPH(U)
           IERR = XDLCONNECT ( U, PLANE, N1, NSECTION, .FALSE. )
          END DO
c
c         connect the graphics plane to image plane 
c
          IF ( IGRAPH .GT. 0 ) THEN 
           PLANE = XDSGRAPH (U)
           CALL XDDINFO ( U, 35, 1, SECTION)
           IERR = XDGCONNECT (U, PLANE, SECTION, .FALSE. )
c
c          turn on the graphics overlay plane
c
           IERR = XDGON (U) 
          END IF
c
          IUNIT = U 
          RETURN
          END
c
c------------------------------------------------------------------
c
c	configure_device
c
c	configure the device given a number of 
c	lines and samples.  If the lines and samples are 0,
c	the the default configuration for the device is used
c	and the number of lines and samples are set to the
c	appropriate values.  Otherwise the device will be
c	configured to one of the following if it is available:
c		512x512 when lines=512 and samples=512
c		640x480 when lines=480 and samples=640
c		1024x1024 when lines=1024 and samples=1024
c	If the given lines and samples do not match any of
c	the above, the device's default is used but the values
c	of lines and samples are not changed.
c
      SUBROUTINE CONFIGURE_DEVICE( LINES, SAMPLES, IUNIT )

      CHARACTER*100 MSG
      CHARACTER*30 CTBL0(3)/ ' Video Output = 512x512       ',
     -                       ' Video Output = 1024x1024     ',
     -                       ' Video Output = 640x480       '/
      CHARACTER*30 CTBL1,CTBL2,CTBL3,CTBL4,CTBL5,CTBL6
      CHARACTER*30 CTBL7A/' GRAPHICS PLANE AVAILABLE     '/
      CHARACTER*30 CTBL7B/' GRAPHICS PLANE NOT AVAILABLE '/
      CHARACTER*30 CTBL7C/' GRAPHICS WILL BE PLACED ON   '/
      CHARACTER*30 CTBL7D/'       IMAGE PLANE 1          '/
      INTEGER  H,CSETUP(4), OUTMODES, CURRD3, SECTION,U
      INTEGER  GPLANE, NLUTS, NIMPS, MAXSAMPS  
      INTEGER  IGRAPH, NCURS, NINTIO, XDSGRAPH, PLANE
      INTEGER  RED(256), GREEN(256), BLUE(256)
      REAL     S
      INTEGER XDDCONFIGURE, XDGCONNECT 
      INTEGER XDTSIZE
      LOGICAL BTEST
c
      DATA CSETUP / 0, 0, 0, 0 /
      DATA RED   / 0,255*255 /
      DATA GREEN / 0,255*255 /
      DATA BLUE / 0,255*255 /
      DATA H / 7 /, S / 1.0 /

      U = IUNIT
      CALL XDDINFO( U, 35, 1, SECTION)
      CALL XDDINFO(U, 7, 1, OUTMODES )
      IF ((LINES.EQ.1024).AND.(SAMPLES.EQ.1024)) THEN
       IF ( BTEST(OUTMODES,9) ) THEN
         CSETUP(2) = 2
         CSETUP(3) = 2
       ELSE
         CALL XVMESSAGE(' 1024X1024 Output Not Available ',' ')
       END IF
      ELSE IF ((LINES.EQ.480).AND.(SAMPLES.EQ.640)) THEN
       IF ( BTEST(OUTMODES,10) ) THEN
         CSETUP(2) = 2
         CSETUP(3) = 3
       ELSE
         CALL XVMESSAGE(' 640x480 Output Mode Not Available ',' ')
       END IF
      ELSE IF ((LINES.EQ.512).AND.(SAMPLES.EQ.512)) THEN
       IF ( BTEST(OUTMODES,8) ) THEN
         CSETUP(2) = 1
         CSETUP(3) = 1
       ELSE
         CALL XVMESSAGE(' 512x512 Output Mode Not Available ',' ')
       END IF
      ELSE
       CSETUP(1) = 0
       CSETUP(2) = 0
       CSETUP(3) = 0
       CSETUP(4) = 0
       IF ( (LINES .NE. 0) .AND. (SAMPLES.NE.0) ) THEN 

	CALL XVMESSAGE(
     - ' Unrecognized Output Size, Display Default Used ', ' ')
       END IF  
      END IF 
c
c         now configure the display
c
          IERR = XDDCONFIGURE (U, CSETUP) 
c
c         find out what type of device we have
c
          CALL XDDINFO ( U, 3, 1, NLUTS )
          CALL XDDINFO ( U, 4, 1, NIMPS )
          CALL XDDINFO ( U, 5, 1, MAXLINES )
          CALL XDDINFO ( U, 6, 1,  MAXSAMPS )
          CALL XDDINFO ( U, 12, 1, CURRD3 )
          CALL XDDINFO ( U, 30, 1, IGRAPH )
          CALL XDDINFO ( U, 34, 1, GPLANE )
          CALL XDDINFO ( U, 48, 1, NCURS )
          CALL XDDINFO ( U, 60, 1, NINTIO )
c
c         print out what we have
c
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE(' Display Device Characteristics',' ')
          WRITE(MSG,50) CTBL0(CURRD3)
 50       FORMAT(A30)
          WRITE (CTBL1, 100) NLUTS
100       FORMAT(' No. of LUTs =     ',I2)
          WRITE (CTBL2, 200) NIMPS
200       FORMAT(' No. of IMPs =     ',I2)
          WRITE (CTBL3, 300) MAXLINES
300       FORMAT(' No. of LINES =    ',I4)
          WRITE (CTBL4, 400) MAXSAMPS
400       FORMAT(' No. of SAMPS =    ',I4)
          WRITE (CTBL5, 500) NCURS
500       FORMAT(' No. of CURSORS =  ',I2)
          WRITE (CTBL6, 600) NINTIO
600       FORMAT(' No. of IO DEVS =  ',I2)
          CALL XVMESSAGE(MSG,' ')
          CALL XVMESSAGE(CTBL1,' ')
          CALL XVMESSAGE(CTBL2,' ')
          CALL XVMESSAGE(CTBL3,' ')
          CALL XVMESSAGE(CTBL4,' ')
          CALL XVMESSAGE(CTBL5,' ')
          CALL XVMESSAGE(CTBL6,' ')

          IF (IGRAPH .EQ. 1) THEN 
           CALL XVMESSAGE(CTBL7A,' ')

          ELSE
           CALL XVMESSAGE(CTBL7B,' ')
           CALL XVMESSAGE(' ',' ')
           CALL XVMESSAGE(CTBL7C,' ')
           CALL XVMESSAGE(CTBL7D,' ')

           GPLANE = 1

          END IF 
          CALL XVMESSAGE(' ',' ')


	IF ( (LINES.EQ.0).AND.(SAMPLES.EQ.0) ) THEN 
         LINES = MAXLINES
         SAMPLES = MAXSAMPS
        END IF
c
c         connect the graphics plane to image plane 
c
          IF ( IGRAPH .GT. 0 ) THEN
           PLANE = XDSGRAPH(U)
           IERR = XDGCONNECT(U, PLANE, SECTION, .FALSE.)
          END IF
        LMAX = LINES
        SMAX = SAMPLES
        IF (LMAX.LE.512) THEN
           H = 7
        ELSE
           H = 14
        END IF 
        IERR = XDTSIZE (H,S) 
	RETURN 
        END
c
c-----------------------------------------------------------------
c
c
c        CLOSE_DEVICE
c
c         to deactivate the ability to modify the display unit u
c         and to deallocate it for the next user
c
c
c
c
	 SUBROUTINE CLOSE_DEVICE(IUNIT)
c
c        deactivate the device 
c
         INTEGER U
         INTEGER XDDCLOSE, XDDACTIVATE
	 LOGICAL FLAG

         U = IUNIT
         FLAG = .FALSE. 
         IERR = XDDACTIVATE ( U, FLAG)
c
c        now close unit
c
         IERR = XDDCLOSE(U)
c
         RETURN
         END
c----------------------------------------------------------------
c
c
c        bw_mode
c
c         this is the routine that connects image 1 to lut 1
c         2, and 3 and turns on the linear ramps.
c
          SUBROUTINE BW_MODE(IUNIT)
c
          INTEGER U, N1, SECTION, NLUTS
          INTEGER XDSSECTION,XDSGRAPH,PLANE
          INTEGER XDLCONNECT, XDLRAMP

          U = IUNIT
          PLANE = XDSGRAPH(U)
          CALL XDDINFO ( U, 3, 1, NLUTS )
         
          DO N1 = 1, NLUTS 
           SECTION = XDSSECTION(U,N1)
           ICOLOR = 0
           IERR = XDLCONNECT (U,1,N1,SECTION, .FALSE.)
c
c          and the ramp
c
           IERR = XDLRAMP (U,N1,SECTION) 
          END DO
         RETURN 
         END
c----------------------------------------------------------------
c
c
c        AUTOTRACKING_MODE
c	
c         this is the routine that turns autotracking on
c
          SUBROUTINE AUTOTRACKING_MODE( ON, IUNIT )
          LOGICAL ON
	  INTEGER XDCAUTOTRACK
          INTEGER U, NINTIO, C

          DATA  C /1/
          U = IUNIT
          CALL XDDINFO( U, 60, 1, NINTIO )
c
          IF ( NINTIO .GT. 0 ) THEN
           IF ( ON ) THEN 
              AUTOFLAG = 1
           ELSE
              AUTOFLAG = 0
           END IF
           IERR = XDCAUTOTRACK ( U,C,0,AUTOFLAG)
          END IF 
          RETURN
          END
