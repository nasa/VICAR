CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate ANGLN, ANGLA, and ANGLB from the C-matrix, spin vector,
C and spacecraft vector.
C
C Inputs: CM,N,PSC,SCLON,RLORA
C Outputs: ANGLN,ANGLA,ANGLB
C
      SUBROUTINE GETANGLES(CM,N,PSC,angln,angla,anglb)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3),N(3),PSC(3)
      REAL*8 N0(3),P0(3),P1(3),NX,NZ
      COMMON/CONST/PI,DTR,RTD
C
C        Rotate spin vector N and S/C vector PSC into image space by 
C        multiplying by CM-inverse.
      DO I=1,3
         N0(I) = CM(1,I)*N(1) + CM(2,I)*N(2) + CM(3,I)*N(3)
         P0(I) = CM(1,I)*PSC(1) + CM(2,I)*PSC(2) + CM(3,I)*PSC(3)
      ENDDO
C
C     ....Rotate spin vector thru ANGLN so that north is along X-axis
      ANGLN = DATAN2(N0(2),N0(1))	! Compute angle N
      NX = N0(1)*DCOS(ANGLN) + N0(2)*DSIN(ANGLN)
      NZ = N0(3)

C     ....Rotate S/C vector thru angles N and A
      ANGLA = DATAN2(NX,NZ)             ! Compute angle A
      P1(1) =  P0(1)*DCOS(ANGLN) + P0(2)*DSIN(ANGLN)
      P1(2) = -P0(1)*DSIN(ANGLN) + P0(2)*DCOS(ANGLN)
      P1(3) =  P0(3)
      PX = P1(1)*DCOS(ANGLA) - P1(3)*DSIN(ANGLA)
      PY = P1(2)
      ANGLB = DATAN2(PY,PX)		! Compute angle B
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate THETA_N, THETA_A, and THETA_B for the C-matrix.  The angles
C are analoguous to ANGLN, ANGLA, and ANGLB for the OM-matrix.
C
C Input: CM
C Outputs: THETA_N,THETA_A,THETA_B
C
      SUBROUTINE GETANGLES2(CM,theta_n,theta_a,theta_b)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3),N(3),PSC(3)
      REAL*8 N0(3),P0(3),P1(3),NX,NZ
      COMMON/CONST/PI,DTR,RTD
C
C        In this routine, the EME50 coordinate system takes the role
C        of the planet coordinate system.
      N(1) = 0.		!Spin vector is z-axis
      N(2) = 0.
      N(3) = 1.

      PSC(1) = 1.		!Spacecraft vector is x-axis
      PSC(2) = 0.
      PSC(3) = 0.

C        Rotate spin vector N and S/C vector PSC into image space by 
C        multiplying by CM-inverse.
      DO I=1,3
      N0(I) = CM(1,I)*N(1) + CM(2,I)*N(2) + CM(3,I)*N(3)
      P0(I) = CM(1,I)*PSC(1) + CM(2,I)*PSC(2) + CM(3,I)*PSC(3)
      ENDDO
C
C        Rotate spin vector thru THETA_N so that north is along X-axis
      if (n0(1).eq.0.0) then
	THETA_N = 0.5*pi
      else
	THETA_N = DATAN2(N0(2),N0(1))	! Compute angle N
      endif
      NX = N0(1)*DCOS(THETA_N) + N0(2)*DSIN(THETA_N)
      NZ = N0(3)
C         Rotate S/C vector thru angles N and A
      if (nz.eq.0.0) then
	THETA_A = 0.5*pi
      else
	THETA_A = DATAN2(NX,NZ)             ! Compute angle A
      endif
      P1(1) =  P0(1)*DCOS(THETA_N) + P0(2)*DSIN(THETA_N)
      P1(2) = -P0(1)*DSIN(THETA_N) + P0(2)*DCOS(THETA_N)
      P1(3) =  P0(3)
      PX = DCOS(THETA_A)*P1(1) - DSIN(THETA_A)*P1(3)
      PY = P1(2)
      if (px.eq.0.0) then
	THETA_B = 0.5*pi
      else
	THETA_B = DATAN2(PY,PX)		! Compute angle B
      endif
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This routine takes an input C-matrix (C) and computes the three Euler
C angles (ALPHA,DELTA,KAPPA) representing the matrix.
C
C Outputs: ALPHA,DELTA,KAPPA: Euler angles in radians.
C
C The 9 elements of the C matrix are stored in order of increasing address
C as
C                  |  c1  c4  c7  |
C                  |  c2  c5  c8  |
C                  |  c3  c6  c9  |
C
      SUBROUTINE COMPCM(C,alpha,delta,kappa)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 C(9)	!Input C-matrix
      REAL*8 ALPHA	!Output RA of instrument boresight (degrees)
      REAL*8 DELTA	!Output Declination of instrument boresight (degrees)
      REAL*8 KAPPA	!Output rotation about optical axis (degrees)

      PI = 3.141592653589793D0
      RTD = 180.D0/PI

      if (c(7).eq.0.0) then
	ALPHA = 0.5*pi
      else
	ALPHA = DATAN2(C(8),C(7))
      endif
      DELTA = DASIN(C(9))
      if (c(6).eq.0.0) then
	KAPPA = 0.5*pi
      else
	KAPPA = DATAN2(C(3),C(6))
      endif
      IF (ALPHA.LT.0.D0) ALPHA=ALPHA+2.D0*PI
      IF (KAPPA.LT.0.D0) KAPPA=KAPPA+2.D0*PI
CCC      ALPHA = ALPHA*RTD
CCC      DELTA = DELTA*RTD
CCC      KAPPA = KAPPA*RTD
C Temporary code to test whether this works...
CCC      CALL BUILDCM(ctest,ALPHA,DELTA,KAPPA)
CCC      CALL ORTHOT(CTEST)
CCC      CALL PRNT(8,9,C,' C-matrix=.')
CCC      CALL PRNT(8,9,CTEST,' COMPCM C-matrix=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to calculate the camera-to-planet rotation matrix (OM)
C from angles ANGLN, ANGLA, and ANGLB.
      SUBROUTINE OMMATRIX(ANGLN,ANGLA,ANGLB,OM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3)
C
C        The OM-matrix is initially matrix M1 (north angle rotation about
C        z0-axis).
      OM(1,1) =  DCOS(ANGLN)
      OM(2,1) = -DSIN(ANGLN)
      OM(3,1) =  0.D0
C
      OM(1,2) =  DSIN(ANGLN)
      OM(2,2) =  DCOS(ANGLN)
      OM(3,2) =  0.D0
C
      OM(1,3) =  0.D0
      OM(2,3) =  0.D0
      OM(3,3) =  1.D0
C        OM = M2*M1 (rotate about y1-axis through angle A)
      DO 20 J=1,3
      TEMP    = DCOS(ANGLA)*OM(1,J) - DSIN(ANGLA)*OM(3,J)
      OM(3,J) = DSIN(ANGLA)*OM(1,J) + DCOS(ANGLA)*OM(3,J)
   20 OM(1,J) = TEMP
C        OM = M3*M2*M1 (rotate about z2-axis through angle B)
      DO 30 J=1,3
      TEMP    =  DCOS(ANGLB)*OM(1,J) + DSIN(ANGLB)*OM(2,J)
      OM(2,J) = -DSIN(ANGLB)*OM(1,J) + DCOS(ANGLB)*OM(2,J)
   30 OM(1,J) = TEMP
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute the C-matrix from ME and OM' matrices...
C Output: CM
C
      SUBROUTINE CMATRIX(ME,ANGLN,ANGLA,ANGLB,SCLON,CM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),CM(3,3),OMp(3,3)

C           Compute OM' matrix
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON,OMp)

      DO 150 J=1,3
      DO 150 I=1,3
  150 CM(I,J) = ME(I,1)*OMp(1,J)+ME(I,2)*OMp(2,J)+ME(I,3)*OMp(3,J)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a unit vector in EME50 coordinates, compute the latitude-longitude
C coordinates...
C
C Inputs: ME matrix
C         P = unit vector
C Outputs: RLAT,RLON
C
      SUBROUTINE FROMEME(ME,P,RLAT,RLON)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),P(3),xdot
      COMMON/CONST/PI,DTR,RTD

      RLAT = DASIN(DOT(P,ME(1,3)))			!RLAT = P o N
      xdot = DOT(P,ME(1,1))
      if (xdot.eq.0.0) then
	RLON = 0.5*pi
      else
	RLON = DATAN2(DOT(P,ME(1,2)),xdot)
      endif
      RLON = DMOD(RLON+2.0D0*PI,2.0D0*PI)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the latitude-longitude coordinates, compute a unit vector in
C the EME-coordinate system...
C
C Inputs: ME(3,3) = EME50 conversion matrix
C         RLAT,RLON
C
C Outputs: Q(3) = unit vector in EME50 coordinates
C
      SUBROUTINE TOEME(ME,RLAT,RLON,Q)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),P(3),Q(3)
C             Compute unit vector in target centered coordinates...
      CALL VECTOR3(1.D0,RLAT,RLON,P)
C             Convert to EME50 coordinates
      Q(1) = ME(1,1)*P(1) + ME(1,2)*P(2) + ME(1,3)*P(3)
      Q(2) = ME(2,1)*P(1) + ME(2,2)*P(2) + ME(2,3)*P(3)
      Q(3) = ME(3,1)*P(1) + ME(3,2)*P(2) + ME(3,3)*P(3)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the latitude-longitude coordinates and range of a point, compute
C its vector in target centered coordinates.
C
C Inputs: RANGE,RLAT,RLON
C Outputs: P(3)
C
      SUBROUTINE VECTOR3(RANGE,RLAT,RLON,P)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 P(3)

      P(1) = RANGE*DCOS(RLAT)*DCOS(RLON)
      P(2) = RANGE*DCOS(RLAT)*DSIN(RLON)
      P(3) = RANGE*DSIN(RLAT)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute offsets (DL,DS) which minimize the distance
C between the computed and actual curves:
C             CHISQ2 = SIGMA((DPi - DL*COSLi - DS*COSSi)**2)
C Inputs:  CPTS,APTS,NPTS,IMODE
C     IMODE=1 if goodness of fit measures are to be printed out
C          =0 otherwise
C Outputs: IND,DL,DS
C     IND = 1 if fit was successful
C         = 0 if error (Det=0)
C
      SUBROUTINE CHISQ2(IND,CPTS,APTS,NPTS,DL,DS,IMODE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS)
C
      DP = 0.D0
      A11 = 0.D0
      A12 = 0.D0
      A22 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      N = 0			!Count number of points used in fit

      DO 50 I=1,NPTS
      IF (APTS(1,I).LT.0.0) GOTO 50		!Skip point if flagged as bad
      N = N + 1
      DLi = APTS(1,I) - CPTS(1,I)
      DSi = APTS(2,I) - CPTS(2,I)
      DPi = DLi**2 + DSi**2
      IF (DPi.EQ.0.0) GOTO 50
      DP = DP + DPi
      DPi = DSQRT(DPi)
      COSSi = DSi/DPi
      COSLi = DLi/DPi
      A11 = A11 + COSSi**2
      A12 = A12 + COSSi*COSLi
      A22 = A22 + COSLi**2
      B1 = B1 + DSi
      B2 = B2 + DLi
   50 CONTINUE
C
      Det = A11*A22 - A12**2
      IF (DABS(Det).LT.1.D-6) GOTO 990
      DL = (B2*A11 - B1*A12)/Det
      DS = (B1*A22 - B2*A12)/Det
      IND = 1
      IF (IMODE.EQ.0) RETURN
C
C     ....The remainder of the routine measures "goodness of fit"
C     ....Compute CHISQ2 and Covariance
      CALL PRNT(4,1,N,' Number of points used in fit=.')
      IF (N.LE.2) GOTO 992
      CHISQ = DP + DS**2*A11 + DL**2*A22
     &       - 2*DS*B1 - 2*DL*B2 + 2*DS*DL*A12
      CHISQ = CHISQ/DFLOAT(N-2)/0.5  !Using measurement error of 0.5 in L & S
      CALL PRNT(8,1,CHISQ,' CHISQ2=.')
      CALL COVMT2(A11,A12,A22,2.*CHISQ)
      RETURN

  990 CALL XVMESSAGE('***Error fitting curve',' ')
      CALL PRNT(4,1,N,'***Number of points used in fit=.')
      IND = 0
      RETURN

  992 CALL XVMESSAGE(
     & '***Warning: Insufficient number of points used in fit',' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE COVMT2(A11,A12,A22,VAR)  !Fixed by VRH 5/23/89
      IMPLICIT NONE
      REAL*8 A11,A12,A22,VAR
      REAL*8 CA11,CA12,CA22,DETA
      CHARACTER*80 MSG
C
      DETA = A11*A22 - A12**2
      IF (DETA.LE.0.D0) THEN
          CALL XVMESSAGE('***Warning: Covariance matrix is singular',
     &	   ' ')
          RETURN
      ENDIF
C
      CA11 =  A22*VAR/DETA
      CA12 = -A12*VAR/DETA
      CA22 =  A11*VAR/DETA
C
      CALL XVMESSAGE('Covariant Matrix:  S (pixels), L (pixels)',' ')
      WRITE(MSG,110) CA11,CA12
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA12,CA22
      CALL XVMESSAGE(MSG,' ')
  110 FORMAT('   ',2F20.10)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to Compute offsets (DL,DS) and DT which minimize the distance
C between the computed and actual curves:
C             CHISQ3 = SIGMA((DPi - DL*COSLi - DS*COSSi -DT*RXDi)**2)
C Inputs:  CPTS,APTS,NPTS,OAL,OAS,IMODE
C     IMODE=1 if goodness of fit measures are to be printed out
C          =0 otherwise
C Outputs: IND,DL,DS,DT
C     IND = 1 if fit was successful
C         = 0 if error (Det=0)
C
      SUBROUTINE CHISQ3(IND,CPTS,APTS,NPTS,OAL,OAS,DL,DS,DT,IMODE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS)
      REAL*8 LAi
C
      DP = 0.D0
      A11 = 0.D0
      A22 = 0.D0
      A33 = 0.D0
      A12 = 0.D0
      A13 = 0.D0
      A23 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      B3 = 0.D0
      N = 0			!Count number of points used in fit
C
      DO 50 I=1,NPTS
      LAi = APTS(1,I)
      IF (LAi.LT.0.0) GOTO 50		!Skip point if flagged as bad
      N = N + 1
      SAi = APTS(2,I)
      DLi = LAi - CPTS(1,I)
      DSi = SAi - CPTS(2,I)
      DPi = DLi**2 + DSi**2
      IF (DPi.LE.0.0) GOTO 50
      DP = DP + DPi
      DPi = DSQRT(DPi)
      COSSi = DSi/DPi
      COSLi = DLi/DPi
      RXDi = (SAi-OAS)*COSLi - (LAi-OAL)*COSSi
      A11 = A11 + COSSi**2
      A22 = A22 + COSLi**2
      A33 = A33 + RXDi**2
      A12 = A12 + COSSi*COSLi
      A13 = A13 + COSSi*RXDi
      A23 = A23 + COSLi*RXDi
      B1 = B1 + DSi
      B2 = B2 + DLi
      B3 = B3 + DPi*RXDi
   50 CONTINUE
C
      D11 = A22*A33 - A23**2
      D22 = A11*A33 - A13**2
      D33 = A11*A22 - A12**2
      D12 = A12*A33 - A13*A23
      D13 = A12*A23 - A13*A22
      D23 = A11*A23 - A13*A12
      Det = A11*D11 - A12*D12 + A13*D13
      IF (DABS(Det).LT.1.D-6) THEN
           CALL PRNT(4,1,N,' Number of points used in fit=.')
           CALL XVMESSAGE('***Error fitting curve',' ')
           IND = 0
           RETURN
      ENDIF
      DS =  (B1*D11 - B2*D12 + B3*D13)/Det
      DL = (-B1*D12 + B2*D22 - B3*D23)/Det
      DT =  (B1*D13 - B2*D23 + B3*D33)/Det
      IND = 1
      IF (IMODE.EQ.0) RETURN
C
C          The rest of the routine measures "goodness of fit"
      CALL PRNT(4,1,N,' Number of points used in fit=.')
      IF (N.LE.3) THEN
           CALL XVMESSAGE(
     &      '***Warning: Insufficient number of points used in fit',
     &	    ' ')
           RETURN
      ENDIF
C          Compute RMS and Covariance
      CHISQ = DP + DS**2*A11 + DL**2*A22 + DT**2*A33
     &           + 2*DS*DL*A12 + 2*DS*DT*A13 + 2*DL*DT*A23
     &           - 2*DS*B1     - 2*DL*B2     - 2*DT*B3
      CHISQ = CHISQ/DFLOAT(N-3)/0.5  !Using measurement error of 0.5 in L & S
      CALL PRNT(8,1,CHISQ,' CHISQ3=.')
      CALL COVMT3(A11,A12,A13,A22,A23,A33,2.*CHISQ)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE COVMT3(A11,A12,A13,A22,A23,A33,VAR)  !Fixed by VRH 5/23/89
      IMPLICIT NONE
      REAL*8 A11,A12,A13,A22,A23,A33,VAR
      REAL*8 CA11,CA12,CA13,CA22,CA23,CA33,DETA
      CHARACTER*80 MSG
C
      DETA = A11*(A22*A33-A23*A23) - A12*(A12*A33-A23*A13)
     *     + A13*(A12*A23-A22*A13)
      IF (DETA.LE.0.D0) THEN
        CALL XVMESSAGE('***Warning: Covariance matrix is singular',
     1   ' ')
        RETURN
      ENDIF
C
      CA11 = (A22*A33 - A23*A23)*VAR/DETA
      CA12 = (A23*A13 - A12*A33)*VAR/DETA
      CA13 = (A12*A23 - A22*A13)*VAR/DETA
      CA22 = (A11*A33 - A13*A13)*VAR/DETA
      CA23 = (A12*A13 - A11*A23)*VAR/DETA
      CA33 = (A11*A22 - A12*A12)*VAR/DETA
C
      CALL XVMESSAGE('Covariant Matrix: S, L (pixels), N (radians)',' ')
      WRITE(MSG,110) CA11,CA12,CA13
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA12,CA22,CA23
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA13,CA23,CA33
      CALL XVMESSAGE(MSG,' ')
  110 FORMAT('   ',3F20.10)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given computed star coordinates CPTS and the actual coordinates APTS,
C determine displacements (DL,DS) which register the two.
C
      SUBROUTINE FIT2(APTS,CPTS,NPTS,DL,DS)
      REAL*4 APTS(2,NPTS),CPTS(2,NPTS)
      REAL*8 DL,DS

      DL = 0.D0
      DS = 0.D0

      DO 50 I=1,NPTS
      DL = DL + APTS(1,I) - CPTS(1,I)
      DS = DS + APTS(2,I) - CPTS(2,I)
   50 CONTINUE

      DL = DL/NPTS
      DS = DS/NPTS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the computed star coordinates CPTS and their actual line-sample
C coordinates APTS compute the (line,sample) and north angle displacements
C (DL,DS) and DT to register the two sets.
C RETURN1 on error.
C
      SUBROUTINE FIT3(APTS,CPTS,NPTS,OAL,OAS,dl,ds,dt,*)
      IMPLICIT REAL*8 (A-H,O-Z)		! VRH added for OAL,... Real*8 6/8/89
      REAL*4 APTS(2,NPTS),CPTS(2,NPTS)

      A13 = 0.D0
      A23 = 0.D0
      A33 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      B3 = 0.D0

      DO 50 I=1,NPTS
      DL = APTS(1,I) - CPTS(1,I)
      DS = APTS(2,I) - CPTS(2,I)
      V = OAL - APTS(1,I)
      U = APTS(2,I) - OAS
      A13 = A13 + U
      A23 = A23 + V
      A33 = A33 + U**2 + V**2
      B1 = B1 + DL
      B2 = B2 + DS
      B3 = B3 + U*DL + V*DS
   50 CONTINUE

      N = NPTS
      Det = N*(N*A33-A13**2-A23**2)
      IF (Det.EQ.0.0) THEN
          CALL XVMESSAGE('***Fit3 error.',' ')
          RETURN1
      ENDIF

      DL = (N*(B1*A33-B3*A13) + A23*(B2*A13-B1*A23))/Det
      DS = (N*(B2*A33-B3*A23) + A13*(B1*A23-B2*A13))/Det
      DT = (N*(N*B3-B1*A13-B2*A23))/Det
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a displacement of the optical axis of (DL,DS) in the image plane,
C compute the resulting changes in ANGLA and ANGLB.
C
C Inputs: DL,DS,ANGLN,ZSCALE (where DL and DS are in object-space)
C Updated: ANGLA,ANGLB
C
      SUBROUTINE MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
      DUX = DS/SCALE
      DUY = DL/SCALE
      DA = DUX*DCOS(ANGLN) + DUY*DSIN(ANGLN)
      DB = (DUX*DSIN(ANGLN) - DUY*DCOS(ANGLN))/DSIN(ANGLA)
      ANGLA = ANGLA + DA
      ANGLB = ANGLB + DB
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a displacement of the optical axis of (DL,DS) in the image plane,
C computes the resulting changes in ANGLA and ANGLB.  This version uses
C the old farenc algorithm due to Ingersoll, circa 1980.
C
C Inputs: DL,DS,ANGLN
C Updated: ANGLA,ANGLB
C 95-11-1 -lwk- ANGLA,ANGLB,ANGLN,PSC3,SCLAT,SCLON are in common /CMAP/, 
c		removed from argument list
C
C Note: CMAP must be in planet coordinate system
C
      SUBROUTINE MOVE2(DL,DS,*)
      IMPLICIT REAL*8 (A-H,O-Z)

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD

      REAL*8 OMp(3,3),RS(3)

C            Get current planet center....
      CALL PLAINV(IND,SCLAT,SCLON,SCLINE,SCSAMP,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.EQ.0) THEN
            CALL XVMESSAGE('***Err calculating planet center',' ')
            RETURN1
      ENDIF
C             Update planet center...
      SCLINE = SCLINE + DL
      SCSAMP = SCSAMP + DS
C            Calculate OMp transpose...
      SCLA = SCLAT*RTD
      SCLO = (2.D0*PI-SCLON)*RTD
      ANGN = ANGLN*RTD + 90.D0	!IPL north angle
      CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,PSCALE,FL,SCLO,SCLA,ANGN,
     &       RSC,OMp,RS)
C            Calculate C-matrix...
      DO 20 J=1,3
      DO 20 I=1,3
   20 CM(I,J) = ME(I,1)*OMp(J,1)+ME(I,2)*OMp(J,2)+ME(I,3)*OMp(J,3)
C            Compute angles N, A, and B...
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update C-matrix and all OM-matrices
C
      SUBROUTINE UPDATENAV
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD

      real*4 scl4,scs4
      REAL*8 CM0(3,3)
      CHARACTER*80 MSG
  112 FORMAT('Object Space PC (LINE,SAMP)=(',F10.2,',',F10.2,')  ANGLN='
     &,F8.3)
  114 FORMAT(' Image Space PC (LINE,SAMP)=(',F10.2,',',F10.2,')')

      CALL CMATRIX(ME,ANGLN,ANGLA,ANGLB,SCLON,CM0)  !Compute C-matrix
C           Update the pointing for the planet...
      CALL GETNAV
      CALL MVE(7,18,CM0,CM,1,1)
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
      CALL PUTNAV
      CALL PLAINV(ind,SCLAT,SCLON,scline,scsamp,      !Compute planet center
     &		OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      WRITE(MSG,112,ERR=10) SCLINE,SCSAMP,ANGLN*RTD   !Print object-space PC
   10 CALL XVMESSAGE(MSG,' ')
      IF (ITYPE.EQ.7) THEN
         CALL CONVISOS(PROJECT,ICAM,scl4,scs4,SNGL(SCLINE),
     &   SNGL(SCSAMP),0,CONV,NPH,NPV,ind)
         SCLINE=SCL4
         SCSAMP=SCS4
         WRITE(MSG,114,ERR=12) SCLINE,SCSAMP	      !Print image-space PC
   12    CALL XVMESSAGE(MSG,' ')
      ENDIF
C          Now update pointing for all the rings...
      DO IRING=1,NRINGS
          CALL GETRING(IRING)
          CALL GETANGLES(CM0,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
          CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
          CALL PUTRING(IRING)
      ENDDO

      CALL GETNAV	!Restore the planet's reference frame
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update each of the ring geometries as a result of changes to PSC, RSC,
C PSUN,RSUN,RLORA, or CM.
C
      SUBROUTINE UPDTRING
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      CALL PUTNAV			! First save the planet

      DO IRING=1,NRINGS
          CALL GETRING(IRING)
          CALL FROMEME(ME,PSC,SCLAT,SCLON)	       ! Compute SCLAT,SCLON
          CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)     ! Compute PSC3
          CALL FROMEME(ME,PSUN,SUNLAT,SUNLON)	       ! Compute SUNLAT,SUNLON
          CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Compute PSUN3
          CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
          CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM
          CALL PUTRING(IRING)
      ENDDO

      CALL GETNAV			! Restore the planet
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routines to save and retrieve planet and ring navigation data..
C Navigation data is saved in three buffers: (1) ORIG, (2) LAST, and
C (3) CURR.  ORIG contains the original data retrieved from the SEDR.
C The version of the camera pointing contained in ORIG is controlled
C by the SEDRSRC parameter.  LAST contains
C the last saved version of the data.  CURR contains the current version
C of the data.   Each of these three save areas contain all the navigation
C data for the planet and each of the rings.  The buffer CMAP contains either
C the planet geometry, or one of the ring geometries, and is the buffer
C which all the navigation routines use.
C
C  GETNAV moves the planet geometry from CURR to CMAP (inverse is PUTNAV)
C  GETRING moves a ring geometry from RSAV to CMAP (inverse is PUTRING)
C  GETLAST moves LAST to CURR (inverse is SAVELAST).  GETNAV is then called.
C  GETSEDR moves ORIG to CURR (inverse is SAVESEDR).  GETNAV is then called.
C
      SUBROUTINE GETNAV
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2

      COMMON/CURR/CUR(71),PCCUR(13)
      COMMON/LAST/LAS(71),PCLAS(13)
      COMMON/ORIG/ORG(71),PCORG(13)
      COMMON/RSAV/RSAVE(38,15)
      REAL*8 LAS

      IRING = 1
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN
C
      ENTRY PUTNAV
      CALL MVE(8,71,FL,CUR,1,1)		!Put planet geometry
      CALL MVE(8,13,AI2,PCCUR,1,1)
      RETURN

      ENTRY GETRING(JRING)
      CALL MVE(8,38,RSAVE(1,JRING),ME,1,1)	!Get orbital data for ring
      RETURN

      ENTRY PUTRING(JRING)
      CALL MVE(8,38,ME,RSAVE(1,JRING),1,1)	!Put orbital data for ring
      RETURN

      ENTRY GETSEDR
      IRING = 1
      CALL MVE(8,84,ORG,CUR,1,1)	!Get nominal planet geometry
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry, CM etc.
      CALL MVE(8,13,PCCUR,AI2,1,1)
      CALL ERING0(PLANET_ID,T,T0,1)	!Navigate rings 7/27/89 ",1" added
      CALL MVE(8,71,CUR,FL,1,1)		!Restore planet geometry 
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN

      ENTRY SAVESEDR
      CALL MVE(8,84,CUR,ORG,1,1)	!Save nominal planet geometry
      RETURN

      ENTRY GETLAST
      IRING = 1
      CALL MVE(8,84,LAS,CUR,1,1)	!Get last planet geometry
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry, CM etc.
      CALL MVE(8,13,PCCUR,AI2,1,1)
      CALL ERING0(PLANET_ID,T,T0,1)	!Navigate rings 7/27/89 ",1" added
      CALL MVE(8,71,CUR,FL,1,1)		!Restore planet geometry
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN

      ENTRY SAVELAST
      CALL MVE(8,84,CUR,LAS,1,1)	!Save last planet geometry
      RETURN
      END
