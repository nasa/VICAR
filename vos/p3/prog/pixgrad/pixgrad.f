C***************************************************************************
C    PIXGRAD calculates the magnitude of the gradiend of a picture.
C    The absolute value of the difference between the current pixel
C    and its eight adjacent neighbors is calculated.  For the current
C    output pixel location, the largest difference is produced on the
C    primary ourput data set; the secondary output data set will
C    contain the index of the largest difference.  The differences 
C    are labelled 1 through 8 counterclockwise starting with the
C    difference of the current pixel (I,J) and the neighbor on the
C    left (I,J-1).
C
C    VARIABLE INP
C             Input filename 
C    VARIABLE OUT
C             Output filenames - one for dip and the 
C             other for azimuth. Byte data output.
C    VARIABLE SIZE
C             Vicar size field.
C             -----------------
C             Starting line          default - input image
C             Starting sample
C             Number of lines
C             Number of samples
C    VARIABLE NORTH
C             Direction of North in degrees clockwise from 
C                           top of image.
C             Default = 0.0
C    VARIABLE ADIP
C             Scale factor for slope.
C             DN(DIP) = (ADIP * DIP) + BDIP
C             where
C             adip - scale factor
C             dip  - computed dip in degrees
C             bdip - offset factor ( see BDIP )
C             Default = 255./90.
C    VARIABLE BDIP
C             Offset factor for slope.
C             DN(DIP) = (ADIP * DIP) + BDIP
C             where
C             adip - scale factor ( see ADIP )
C             dip  - computed dip in degrees
C             bdip - offset factor 
C             Default = 0.0
C    VARIABLE AAZ
C             Scale factor for azimuth.
C             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
C             where
C             aaz      - scale factor 
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor ( see BAZ )
C             Default = 256./360.
C    VARIABLE BAZ
C             Offset factor for azimuth.
C             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
C             where
C             aaz      - scale factor ( see AAZ )
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor 
C             Default = 0.0
C    VARIABLE LSCALE
C             Line scale in meters per line.
C             Default = 1.0
C    VARIABLE SSCALE
C             Sample scale in meters per sample.
C             Default = 1.0
C    VARIABLE AALT
C             Altitude scale in meters per dn of input.
C             Default = 1.0 meter/dn
C***************************************************************************
C
C     10-10-88  ...SP...   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 
C                          USES ONLY THE FORMAT IN LABEL.
C     08-05-94   (CRI)     MSTP S/W Conversion (VICAR porting) 
C     11-08-94  ..REA...   Changed default scalings, I/O cleanup
C
C    A = INPUT ELEVATION IMAGE
C    B = GRADIENT MAGNITUDE IMAGE (DIP)      BYTE
C    C = GRADIENT DIRECTION IMAGE (AZIMUTH)  BYTE
C
C    SIZE FIELD REFERS TO THE INPUT IMAGE
C
C    PARAMETER  VARIABLE  FORMAT  DEFAULT   DESCRIPTION
C    NORTH      NORTH     FLOAT   0.        DIR OF NRTH DEG CLKWS FRM UP
C    ADIP       ADIP      FLOAT   255./90.  DN(DIP) = ADIP*(DIP+BDIP)
C    BDIP       BDIP      FLOAT   0.        
C    AAZ        AAZ       FLOAT   256./360. DN(AZIM)=AAZ*(AZIMUTH+BAZ)
C    BAZ        BAZ       FLOAT   0.        
C    LSCALE     LSCALE    FLOAT   1.0       LINE SCALE, IN METERS/LINE
C    SSCALE     SSCALE    FLOAT   1.0       SAMPLE SCALE, IN METERS/SAMPLE
C    AALT       AALT      FLOAT   1.0 METERS/DN ALTITUDE=AALT*DN(ALT)
C***************************************************************************
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT NONE

      INCLUDE 'fortport'
      REAL*4  ALT(10002,3)                ! altitude array 
      BYTE  DIP(10000)                    ! gradient magnitude array
      BYTE  AZIM(10000)                   ! gradient direction array

      CHARACTER*72 LBL

      REAL*4  NORTH                  ! DIR OF NRTH, DEG CLKWS FRM UP
      REAL*4  ADIP                   ! DN(DIP) = (ADIP*DIP)+BDIP
      REAL*4  BDIP                   ! DN(DIP) = (ADIP*DIP)+BDIP
      REAL*4  AAZ                    ! DN(AZIM)=(AAZ*AZIMUTH)+BAZ
      REAL*4  BAZ                    ! DN(AZIM)=(AAZ*AZIMUTH)+BAZ
      REAL*4  LSCALE                 ! LINE SCALE, IN MILES/LINE
      REAL*4  SSCALE                 ! SAMPLE SCALE, IN MILES/SAMPLE
      REAL*4  AALT                   ! ALTITUDE=AALT*DN(ALT)+BALT
      REAL*4  PI/3.1415927/          ! pi

      INTEGER*4 SL                   ! starting line
      INTEGER*4 SS                   ! starting sample
      INTEGER*4 NL                   ! number of lines output
      INTEGER*4 NS                   ! number of samples output
      INTEGER*4 NLIN                 ! number of lines input
      INTEGER*4 NSIN                 ! number of samples input
      INTEGER*4 ICOUNT,LAST,NOW,NEXT ! temp variables
      INTEGER*4 LINE,ISAMP,LOC,N     ! temp variables
      REAL*4    LFAC,SFAC,TWOPI      ! temp variables
      REAL*4    X,Y,Z,XDIP,XAZIM     ! temp variables
C
      INTEGER*4 INUNIT               ! unit of input data set
      INTEGER*4 OUTUNIT(2)           ! 2 units for output data sets
      INTEGER*4 STATUS               ! status of xv returns
C
      CALL XVEACTION ('SA',' ')
      CALL XVMESSAGE('PIXGRAD version 09-NOV-94',' ')
C							open datasets
C
      CALL XVUNIT ( INUNIT, 'INP', 1, STATUS, ' ' ) ! get unit for input data
      CALL XVOPEN  ( INUNIT, STATUS, 'U_FORMAT','REAL', ' ')      
      CALL XVUNIT(OUTUNIT(1), 'OUT', 1, STATUS,' ') ! first output data set
      CALL XVOPEN(OUTUNIT(1), STATUS, 'U_FORMAT','BYTE',
     +            'O_FORMAT','BYTE','OP','WRITE',' ')  
      CALL XVUNIT(OUTUNIT(2), 'OUT', 2, STATUS,' ') ! second output data set
      CALL XVOPEN(OUTUNIT(2), STATUS, 'U_FORMAT','BYTE',
     +            'O_FORMAT','BYTE','OP','WRITE',' ')  
      CALL XVSIZE ( SL, SS, NL, NS, NLIN, NSIN )
      IF ( SL + NL .GT. 1 + NLIN ) THEN
	  CALL XVMESSAGE(
     +       '$$$ Requested size field exceeds input image length $$$',
     +	     ' ')
	  CALL ABEND
      ENDIF
      IF ( SS + NS .GT. 1 + NSIN ) THEN
	  CALL XVMESSAGE(
     +         '$$$ Requested size field exceeds input image width $$$',
     +	       ' ')
	  CALL ABEND
      ENDIF
C
C								get parameters
      CALL XVP ( 'NORTH',  NORTH,  ICOUNT )
      CALL XVP ( 'ADIP',   ADIP,   ICOUNT )
      CALL XVP ( 'BDIP',   BDIP,   ICOUNT )
      CALL XVP ( 'AAZ',    AAZ,    ICOUNT )
      CALL XVP ( 'BAZ',    BAZ,    ICOUNT )
      CALL XVP ( 'LSCALE', LSCALE, ICOUNT )
      CALL XVP ( 'SSCALE', SSCALE, ICOUNT )
      CALL XVP ( 'AALT',   AALT,   ICOUNT )
C								print parameters
      WRITE (LBL,100) AALT
  100 FORMAT ('          Altitude Image...    ALT = ',F8.3,
     +        ' * DN meters')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,110) LSCALE
  110 FORMAT ('          Line scale is  ',F10.3,' meters/pixel')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,120) SSCALE
  120 FORMAT ('          Sample scale is',F10.3,' meters/pixel')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,130) ADIP,BDIP
  130 FORMAT ('Gradient Magnitude Image...     DN = ',F8.3,
     +        ' * DIP(deg) ',SP,F8.3)
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,140) AAZ,BAZ
  140 FORMAT ('Gradient Direction Image...     DN = ',F8.3,
     +        ' * AZIMUTH(deg) ',SP,F8.3)
      CALL XVMESSAGE(LBL,' ')
C								update labels
      CALL XLADD(OUTUNIT(1),'HISTORY','IMAGE','Gradient Magnitude',
     +           STATUS,'FORMAT','STRING',' ')
      WRITE (LBL,150) ADIP,BDIP
  150 FORMAT (F8.3,' * DIP(deg) ',SP,F8.3)
      CALL XLADD(OUTUNIT(1),'HISTORY','DN',LBL,STATUS,'FORMAT','STRING',
     +           ' ')
C
      CALL XLADD(OUTUNIT(2),'HISTORY','IMAGE','Gradient Direction',
     +           STATUS,'FORMAT','STRING',' ')
      WRITE (LBL,160) AAZ,BAZ
  160 FORMAT (F8.3,' * AZIMUTH(deg) ',SP,F8.3)
      CALL XLADD(OUTUNIT(2),'HISTORY','DN',LBL,STATUS,'FORMAT','STRING',
     +           ' ')
C					change linear scaling factors to pixels
C					change angular factors to include
C					conversion from radians to degrees.
      LFAC = 0.5 * AALT / LSCALE
      SFAC = 0.5 * AALT / SSCALE
      ADIP = ADIP * 180.0 / PI
      AAZ  = AAZ  * 180.0 / PI
      TWOPI = 2.0 * PI
      NORTH = TWOPI - (NORTH*PI/180.0)
C				fill in initial values into the altitude buffer
      CALL XVREAD(INUNIT,ALT(2,1),STATUS,'LINE',SL,'SAMP',SS,
     +            'NSAMPS',NS,' ')
      ALT(1,1) = ALT(2,1)
      ALT(NS+2,1) = ALT(NS+1,1)
      CALL MVE(7,NS+2,ALT(1,1),ALT(1,2),1,1)
C
      LAST = 1				! index to previous line ALT values
      NOW = 2				! index to this line ALT values
      NEXT = 3				! index to next line ALT values
C
C					loop through each line of the image
      DO LINE = SL+1,SL+NL-1
          CALL XVREAD(INUNIT,ALT(2,NEXT),STATUS,'LINE',LINE,'SAMP',SS,
     +                'NSAMPS',NS,' ')
          ALT(1,NEXT) = ALT(2,NEXT)
          ALT(NS+2,NEXT) = ALT(NS+1,NEXT)
C								sample loop
          DO ISAMP = 1,NS
              LOC = ISAMP + 1
              X = SFAC * (ALT(LOC+1,NOW) - ALT(LOC-1,NOW))
              Y = LFAC * (ALT(LOC,NEXT) - ALT(LOC,LAST))
              Z = X*X + Y*Y
              IF (Z .EQ. 0.0) THEN
                  XDIP = BDIP
                  XAZIM = BAZ
              ELSE
                  XDIP = ADIP * ATAN(SQRT(Z)) + BDIP
                  XAZIM = AAZ * AMOD(ATAN2(-X,Y)+NORTH,TWOPI) + BAZ
              END IF
              N = MAX(0,MIN(255,NINT(XDIP)))
              DIP(ISAMP) = INT2BYTE(N)
              N = MAX(0,MIN(255,NINT(XAZIM)))
              AZIM(ISAMP) = INT2BYTE(N)
          END DO
          CALL XVWRIT(OUTUNIT(1),DIP,STATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT(2),AZIM,STATUS,'NSAMPS',NS,' ')
C						     increment ALT array indices
          LOC = LAST
          LAST = NOW
          NOW = NEXT
          NEXT = LOC
      END DO
C						    last line remains to be done
      CALL MVE(7,NS+2,ALT(1,NOW),ALT(1,NEXT),1,1)
      DO ISAMP = 1,NS
          LOC = ISAMP + 1
          X = SFAC * (ALT(LOC+1,NOW) - ALT(LOC-1,NOW))
          Y = LFAC * (ALT(LOC,NEXT) - ALT(LOC,LAST))
          Z = X*X + Y*Y
          IF (Z .EQ. 0.0) THEN
              XDIP = BDIP
              XAZIM = BAZ
          ELSE
              XDIP = ADIP * ATAN(SQRT(Z)) + BDIP
              XAZIM = AAZ * AMOD(ATAN2(-X,Y)+NORTH,TWOPI) + BAZ
          END IF
          N = MAX(0,MIN(255,NINT(XDIP)))
          DIP(ISAMP) = INT2BYTE(N)
          N = MAX(0,MIN(255,NINT(XAZIM)))
          AZIM(ISAMP) = INT2BYTE(N)
      END DO
      CALL XVWRIT(OUTUNIT(1),DIP,STATUS,'NSAMPS',NS,' ')
      CALL XVWRIT(OUTUNIT(2),AZIM,STATUS,'NSAMPS',NS,' ')
C								close data sets
C
      CALL XVCLOSE(INUNIT, STATUS,' ')
      CALL XVCLOSE(OUTUNIT(1), STATUS,' ')
      CALL XVCLOSE(OUTUNIT(2), STATUS,' ')
      RETURN
      END
