C------------------------------------------------------------------
C If PC or RPC is specified, use them to fix the camera pointing for
C the input or reference image.  The OM matrix is updated.
C If rotation rate is specified, adjust SCLON to match rotation over
C time inverval T-T2.  The OM matrix is updated.
C
      SUBROUTINE FIX_POINTING(SBUF,SBUF2,TARGET,T,T2,SROT)
      IMPLICIT NONE
      REAL*8 SBUF(100),SBUF2(100)
      CHARACTER*12 TARGET	!Target name
      REAL*8 T,T2		!Time (days) for input and ref images
      REAL*8 SROT		!Rotation rate (degrees/day) from SEDR

      COMMON/CINC/DL,DS,INCLUDE
      REAL*8 DL,DS

      COMMON/SCALING/SCALE_FACTOR
      REAL*8 SCALE_FACTOR      !RRP FR89270
      INTEGER*4 INCLUDE

      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      LOGICAL XVPTST
      REAL*8 SCLINE,SCSAMP,SCLAT,SCLON,ANGLN,RANGE
      REAL*8 SCLINE2,SCSAMP2,SCLAT2,SCLON2,ANGLN2,RANGE2
      REAL*8 ROT		!Rotation rate specified by user
      REAL*8 SCLON0		!Spacecraft longitude from SEDR
      REAL*4 PAR(2)
      INTEGER*4 NUM,DEF
      CHARACTER*128 MSG
  110 FORMAT(' OS target center=(',F10.2,',',F10.2,')')
  112 FORMAT(' OS target center for ref image=(',F10.2,',',F10.2,')')
 1004 FORMAT(' SEDR rotation rate is',F9.4,' degrees/day')
 1003 FORMAT(' New rotation rate is',F9.4,' degrees/day')
 1002 FORMAT(' Spacecraft longitude adjusted by',F8.3,' degrees')

      SCLINE = SBUF(69)				!Target center line
      SCSAMP = SBUF(70)				!Target center sample
      SCLAT = SBUF(30)				!Spacecraft latitude
      SCLON = DMOD(SBUF(31)+360.d0,360.d0)	!Spacecraft longitude
      ANGLN = DMOD(SBUF(68)+90.d0,360.d0)	!North angle
      RANGE = SBUF(27)				!Target range (km)

      CALL XVPARM('PC',par,num,def,2)
      IF (DEF.NE.1) THEN			!If PC is specified, then
         SCLINE = PAR(1)			!compute the OM-matrix from it.
         SCSAMP = PAR(2)
         CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,SCALE,FL,SCLON,SCLAT,ANGLN,
     &      RANGE,om,rs)
      ENDIF

      WRITE(MSG,110) SCLINE,SCSAMP
      CALL XVMESSAGE(MSG,' ')

      SCLINE2 = SBUF2(69)			!Target center line
      SCSAMP2 = SBUF2(70)			!Target center sample

      CALL XVPARM('RPC',par,num,def,2)
      IF (DEF.NE.1) THEN			!If RPC is specified, then
         SCLINE2 = PAR(1)
         SCSAMP2 = PAR(2)
         SCLAT2 = SBUF2(30)			!Spacecraft latitude
         SCLON2 = DMOD(SBUF2(31)+360.d0,360.d0)	!Spacecraft longitude
         ANGLN2 = DMOD(SBUF2(68)+90.d0,360.d0)	!North angle
         RANGE2 = SBUF2(27)			!Target range (km)
         CALL MOMATI(OAL2,OAS2,SCLINE2,SCSAMP2,SCALE2,FL2,SCLON2,
     &      SCLAT2,ANGLN2,RANGE2,OM2,RS2)
      ENDIF

      WRITE(MSG,112) SCLINE2,SCSAMP2
      CALL XVMESSAGE(MSG,' ')

      CALL XVPARMD('ROT',rot,num,def,1)
      IF (DEF.NE.1) THEN
         WRITE(MSG,1004) SROT
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,1003) ROT
         CALL XVMESSAGE(MSG,' ')
         SCLON0 = SCLON
         SCLON = SCLON + (T-T2)*(ROT-SROT)
         CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,SCALE,FL,SCLON,SCLAT,ANGLN,
     &      RANGE,om,rs)
         WRITE(MSG,1002) SCLON-SCLON0
         CALL XVMESSAGE(MSG,' ')
      ENDIF

C     ....Compute offset for sky background

      SCALE_FACTOR = SCALE/SCALE2 !RRP FR89270
      IF (XVPTST('INCLUDE')) THEN
         INCLUDE = 1
         DL = SCLINE/SCALE_FACTOR - SCLINE2 !RRP FR89270
         DS = SCSAMP/SCALE_FACTOR - SCSAMP2 !RRP FR89270
      ELSE
         INCLUDE = 0
      ENDIF
      RETURN
      END
