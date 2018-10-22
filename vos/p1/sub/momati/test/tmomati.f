        Include 'VICMAIN_FOR'
c
C-----THIS PROGRAM TESTS SUBROUTINE FORMOM (IBM FARENC)
        Subroutine  Main44
        common/c1/language_flag
	REAL*4 MAP(40)
        Character*10  C
C
C-----SET UP THE TEST INPUT BUFFER
	MAP(25) = 66773.
	MAP(26) = 71400.
	MAP(27) = 1500.1904
	MAP(28) = 500.
	MAP(29) = 500.
	MAP(30) = 84.821431
	MAP(31) = 3.4825339
	MAP(32) = 116.72441
	MAP(33) = -121.600
	MAP(34) = 1662.700
	MAP(35) = 160.8923
	MAP(38) = 14967727.
	I = 5
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(36), 4)
c	CALL MVL(I,MAP(36),4)
	I = 7
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(37), 4)
c	CALL MVL(I,MAP(37),4)
	I = 8
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(39), 4)
c	CALL MVL(I,MAP(39),4)
	MAP(40) = 0.
	CALL PRNT(7,11,MAP(25),' MAP(25-35) = . ')
	CALL PRNT(4,2,MAP(36),' MAP(36-37) = . ')
	CALL PRNT(7,1,MAP(38),' MAP(38) = . ')
	CALL PRNT(4,2,MAP(39),' MAP(39-40) = . ')

        CALL XVMESSAGE('Test of Fortran version',' ')
        language_flag = 1
C-----CALL THE SUBROUTINE TO BE TESTED
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
C-----CALL THE SUBROUTINE TO BE TESTED
        language_flag = 2
        CALL XVMESSAGE('Test of C version via zmomati',' ')
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
C-----CALL THE SUBROUTINE TO BE TESTED
        language_flag = 0
        CALL XVMESSAGE('Test of C version directly',' ')
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
        Return
	END



C     THIS IS IBM SUBROUTINE FARENC   -----NAME CHANGE-----
C     2 FEB 83   ...CCA...     INITIAL RELEASE
c       Sep 92   ...WPL...     Ported for UNIX Conversion
c
      Subroutine  FORMOM(DATA)
c
C Routine to set up CAMERA Pointing Geometry INFO for calculation of 
C the Planet-to-Camera ROTATION MATRIX (OM).
c
      IMPLICIT REAL*8 (A-Z)
      common/c1/language_flag
      integer*4 language_flag
      REAL*4 DATA(1)

C
      PI = 3.141592653589793D0
      RADDEG = 180.D0 / PI
      DEGRAD = PI / 180.D0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      LAT = DATA(31)
      LON = DATA(32)
      LSS = DATA(33)
      SSS = DATA(34)
      NA = DATA(35)
      D = DATA(38)
c
c  Convert from Geodetic to Geocentric Latitude
c
      IF(DABS(LAT).EQ.90.D0) GOTO 10
      RP = DATA(25)
      RE = DATA(26)
      E = RE/RP
      LAT = DATAN(DTAN(LAT*DEGRAD)/E**2)*RADDEG

10    if (language_flag .eq.1) then
          Call MOMATI(OAL,OAS,LSS,SSS,SCALE,FL,LON,LAT,NA,D,DATA,
     &                DATA(19))
      else 
          Call TZMOMATI(language_flag,OAL,OAS,LSS,SSS,SCALE,FL,
     &	   LON,LAT,NA,D,DATA,DATA(19))
      endif
      Return
      END

