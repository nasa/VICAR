C VICAR PROGRAM PTP:  Perspective-to-perspective projection.
C
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      COMMON/CIO/IUNIT,OUNIT,DCODE
      INTEGER*4 IUNIT,OUNIT,DCODE

      COMMON/CINP/SL,SS,NLO,NSO,NLI,NSI,INCo
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,INCo

C     ....Navigation data for input image
      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

C     ....Navigation data for reference image
      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      COMMON/PCONST/RA,RB,RC
      REAL*8 RA,RB,RC

      REAL*8 SBUF(100),SBUF2(100)
      INTEGER*4 IBUF(200),IBUF2(200)
      EQUIVALENCE (SBUF,IBUF),(SBUF2,IBUF2)

      REAL*8 T,T2,SROT
      INTEGER*4 N1,N2
      CHARACTER*12 TARGET
      EXTERNAL ICPROJECT		!For STACKA

      CALL XVMESSAGE(' PTP Version 29-Dec-2011',' ')

C     ...Open input and output images
      CALL OPEN_IMAGES(iunit,ounit,dcode,sl,ss,nlo,nso,nli,nsi)

C     ....Get navigation data for input and reference images
      CALL GET_INP_NAV(IUNIT,sbuf,ibuf,t)		!Store data in C1
      CALL PBNAME(IBUF(9),target,*994)			!Get target name
      CALL GET_RADII(TARGET,ra,rb,rc,srot)		!Get target radii
      CALL GET_REF_NAV(PROJECT,TARGET,ITYPE,sbuf2,ibuf2,t2) !Store data in C2

C     ....Process parameters PC, RPC, and ROT to correct navigation
      CALL FIX_POINTING(SBUF,SBUF2,TARGET,T,T2,SROT)

      RX2(1) = RS2(1)/RA
      RX2(2) = RS2(2)/RB
      RX2(3) = RS2(3)/RC
      RX2MAG = RX2(1)*RX2(1) + RX2(2)*RX2(2) + RX2(3)*RX2(3)

C     ....Compute size of input and output buffers
      INCo = 32			!Size of default grid (bytes)
      N1 = NLI*NSI		!Size of input picture buffer (bytes)
      N2 = INCo*NSO		!Size of output picture buffer (bytes)
      IF (DCODE.EQ.2) THEN	!If input is halfword,
         N1 = 2*N1		!buffers must be twice as large.
         N2 = 2*N2
      ENDIF

C     ....Project the input image
      CALL STACKA(5,ICPROJECT,2,N1,N2,INCo)
      CALL XVMESSAGE(' ***PTP task completed',' ')
      RETURN

  994 CALL XVMESSAGE('***Invalid target name',' ')
      CALL MABEND('***PTP task cancelled')
      END
C------------------------------------------------------------------	  
C Convert SCET into Days
C          T = days since Jan 1, 1950.
C          
      SUBROUTINE GET_TIME(IBUF,t)
      IMPLICIT NONE
      INTEGER*4 IBUF(8)
      REAL*8 T

      INTEGER*4 IDATE,ITIME,IYEAR,IDAY,IHOUR,IMIN,ISEC,JTIME
      REAL*8 HOUR

      IDATE = 1000*MOD(IBUF(3),100) + IBUF(4)	! IDATE = YYDDD
      ITIME = 10000000*IBUF(5) + 100000*IBUF(6)
     &         + 1000*IBUF(7) + IBUF(8)		! ITIME = HHMMSSMMM
C         Time since 1950 (days)
      IYEAR = IBUF(3) - 1900
      IDAY = 365.25*IYEAR - 18262.125 + IBUF(4) !days since 1950
      JTIME = ITIME/1000
      ISEC = MOD(JTIME,100)
      JTIME = JTIME/100
      IMIN = MOD(JTIME,100)
      IHOUR = JTIME/100
      HOUR = IHOUR + (ISEC/60.D0 + IMIN)/60.0D0
      T = IDAY + HOUR/24.0D0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get target radii and rotation rate (deg/day)
C
      SUBROUTINE GET_RADII(TARGET,ra,rb,rc,srot)
      IMPLICIT NONE
      CHARACTER*8 TARGET
      REAL*8 RA,RB,RC,SROT

      REAL*8 R(3)
      REAL*4 DATA(20)
      INTEGER*4 DEF,NUM

      call init_spice

      CALL PBDATA(TARGET,data,*994)
      RA = DATA(1)			!Equatorial radius, long axis
      RB = DATA(2)			!Equatorial radius, short axis
      RC = DATA(3)			!Polar radius
      SROT = 360.D0/DATA(5)		!rotation rate in degrees/day

      CALL XVPARMD('RADII',r,num,def,1)
      IF (DEF.NE.1) THEN
         RA = r(1)			!Equatorial radius, long axis
         RB = r(2)			!Equatorial radius, short axis
         RC = r(3)			!Polar radius
      ENDIF
      RETURN

  994 CALL XVMESSAGE('***Invalid target name',' ')
      CALL XVMESSAGE('***PTP task cancelled',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute approximate transformation:
C    u = a1 + a2*x + a3*y + a4*x*y
C    v = b1 + b2*x + b3*y + b4*x*y
C where (x,y) and (u,v) are the (line,samp) coordinates of the reference
C and input images.  The a's and b's are solved for such that the above
C equation is true for the four corners.
C
      SUBROUTINE FINDT (IX,IY,U,V,a,b,ind)
      IMPLICIT NONE
      INTEGER*4 IX(4),IY(4)	!Location of four corners in ref image
      REAL*8 U(4),V(4)		!Location of four corners in input image
      REAL*4 a(4),b(4)		!Transformation coefficients (output)
      INTEGER*4 IND		!Return status (1=singular matrix)

      REAL*4 X(4),Y(4),M(4,4)
      INTEGER I,N

      DO I=1,4
        X(I) = IX(I)
        Y(I) = IY(I)
        A(I) = U(I)
        B(I) = V(I)
      ENDDO

      DO I=1,4
        M(I,1) = 1
        M(I,2) = X(I)
        M(I,3) = Y(I)
        M(I,4) = X(I)*Y(I)
      ENDDO

      N = 4
      CALL SIMQ(M,a,N,IND)

      DO I=1,4
        M(I,1) = 1
        M(I,2) = X(I)
        M(I,3) = Y(I)
        M(I,4) = X(I)*Y(I)
      ENDDO

      CALL SIMQ(M,b,N,IND)

      RETURN
      END
