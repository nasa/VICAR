CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from (line,sample) to (latitude,longitude) for a
C perspective projection camera system.  An oblate spheroid model of the
C planet (RE,RP) is used.  All angles are in radians.  Planetocentric
C latitudes and east-longitudes are used.
C
C Inputs:
C     RLINE,RSAMP   :  input image coordinates
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)*DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)*DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     A2B2,B2C2,etc :  constants derived from planet radii & s/c vector
C     RLORA         :  longitude of planet's major equatorial radius
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :	IND=1 if resulting (RLAT,RLON) are valid
C		    :	IND=0 if point is off the planet
C     RLAT,RLON     :  computed planet coordinates.
C
      SUBROUTINE PLANET(IND,RLINE,RSAMP,RLAT,RLON,OM,PSC3,RLORA,
     &                OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 OM(3,3),PSC3(3)
      REAL*8 PI/3.141592653589793D0/
C
C         Vector from spacecraft to point on planet in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C         Convert vector to planet coordinate system (x3,y3,z3)
      xc = OM(1,1)*x0 + OM(1,2)*y0 + OM(1,3)*z0
      yc = OM(2,1)*x0 + OM(2,2)*y0 + OM(2,3)*z0
      zc = OM(3,1)*x0 + OM(3,2)*y0 + OM(3,3)*z0
C          Find where vector intersects planet
      A = AI2*xc**2 + BI2*yc**2 + CI2*zc**2
      B = AI2XS*xc  + BI2YS*yc  + CI2ZS*zc
      D = B*B - A*CTERM
      IF(D.LT.0.) THEN
          IND = 0		!Point off the planet
          RETURN
      ENDIF
      r = (-B-DSQRT(D))/A	!Choose smaller root for point in front
C
      x3 = r*xc + PSC3(1)
      y3 = r*yc + PSC3(2)
      z3 = r*zc + PSC3(3)
      D = DSQRT(x3**2+y3**2)
      RLAT = DATAN2(z3,D)
      RLON = DMOD(DATAN2(y3,x3)+RLORA+2.D0*PI,2.D0*PI)
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from (latitude,longitude) to (line,sample) in a
C perspective projection camera system.  An oblate spheroid model of the
C planet (RE,RP) is used.  All angles are in radians.  Planetocentric
C latitudes and east-longitudes are used.
C
C Inputs:
C     RLAT,RLON     :  input planet coordinates.
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLAT-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLAT-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     RLORA         :  longitude of planet's major equatorial radius
C     ABC,A2B2,etc  :  constants derived from planet radii & s/c vector
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  IND=1 if resulting (RLINE,RSAMP) are valid
C                   :  IND=0 if point is behind the planet
C     RLINE,RSAMP   :  computed image coordinates
C
      SUBROUTINE PLAINV(IND,RLAT,RLON,RLINE,RSAMP,OM,PSC3,RLORA,
     &                    OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 OM(3,3),PSC3(3)
C
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
      CLON = DCOS(RLON-RLORA)
      SLON = DSIN(RLON-RLORA)
C     ...Compute planetocentric radius
      R = 1.D0/DSQRT(AI2*(CLAT*CLON)**2+BI2*(CLAT*SLON)**2+CI2*SLAT**2)
C     ...Convert (lat,lon) point on planet to rectangular coordinates
      xp = R*CLAT*CLON
      yp = R*CLAT*SLON
      zp = R*SLAT
C          Compute vector from camera to point on planet
      xc =  xp - PSC3(1)
      yc =  yp - PSC3(2)
      zc =  zp - PSC3(3)
C          Vance Haemmerle's back-of-planet test
      DOTPROD = xc*xp + yc*yp + zc*zp
      IF (DOTPROD.GT.0) THEN	!If the angle between the two vectors
           IND = 0		!is less than 90 degrees, then point
           RETURN		!is behind the planet.
      ENDIF
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transform from camera coordinates (line,sample) to ring plane
C coordinates (radius,longitude) for a perspective projection camera system.  
C All angles are in radians.  Planetocentric latitudes and east-longitudes
C are used.
C
C Inputs:
C     RLINE,RSAMP   :  input image coordinates
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     SCLON         :  spacecraft longitude
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  return indicator.  Upon return,
C		    :	    IND=1 if resulting (RADIUS,RLON) are valid
C                   : 	    IND=0 if the ring plane is not visible
C     RADIUS,RLON   :  computed ring coordinates.
C
      SUBROUTINE RING(IND,RLINE,RSAMP,RADIUS,RLON,OM,PSC3,RLORA,
     &       OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3)
      REAL*8 PI/3.141592653589793D0/
C
C         Vector from spacecraft to point on ring in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C         Convert vector to planet centered coordinates
      xc = OM(1,1)*x0 + OM(1,2)*y0 + OM(1,3)*z0
      yc = OM(2,1)*x0 + OM(2,2)*y0 + OM(2,3)*z0
      zc = OM(3,1)*x0 + OM(3,2)*y0 + OM(3,3)*z0
C
      S = -PSC3(3)/zc
      IF (S.LT.0.) THEN
	IND = 0
        RETURN
      ENDIF
C
      X = S*xc + PSC3(1)
      Y = S*yc + PSC3(2)
      RADIUS = DSQRT(X**2+Y**2)
      RLON = DMOD(DATAN2(Y,X)+RLORA+2.D0*PI,2.D0*PI)
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from ring plane coordinates (radius,longitude) to
C camera coordinates (line,sample) in a perspective projection camera system.
C All angles are in radians.  Planetocentric latitudes and east-longitudes
C are used.
C
C Inputs:
C     RADIUS,RLON   :  input ring coordinates.
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     SCLON         :  spacecraft longitude.
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  IND=1 always.  Output (line,sample) are always valid.
C     RLINE,RSAMP   :  computed image coordinates
C
      SUBROUTINE RINV(IND,RADIUS,RLON,RLINE,RSAMP,
     &      OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3)
C          Compute vector from camera to point on ring
      xc =  RADIUS*DCOS(RLON-RLORA) - PSC3(1)
      yc =  RADIUS*DSIN(RLON-RLORA) - PSC3(2)
      zc =                          - PSC3(3)
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the line-sample image coordinates of a star, calculate
C its right-ascension and declination.
C
C Outputs: RA,DEC
C
      SUBROUTINE STAR(RLINE,RSAMP,ra,dec,CM,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3)
      REAL*8 PI/3.141592653589793D0/

C     ....Compute vector in camera coordinates
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C     ....Rotate vector into EME50 coordinate system using C-matrix
      x = CM(1,1)*x0 + CM(1,2)*y0 + CM(1,3)*z0
      y = CM(2,1)*x0 + CM(2,2)*y0 + CM(2,3)*z0
      z = CM(3,1)*x0 + CM(3,2)*y0 + CM(3,3)*z0
C     ....Convert from rectangular to polar coordinates
      D = DSQRT(x**2 + y**2)
      DEC = DATAN2(z,D)
      RA = DMOD(DATAN2(y,x)+2.D0*PI,2.D0*PI)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the right-ascension and declination of a star, calculate
C its line-sample coordinates in the image.
C
C Outputs: RLINE,RSAMP
C
      SUBROUTINE STARINV(RA,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3)

C     ....Convert RA,DEC to unit vector (x,y,z) in EME50 coordinates.
      CD = DCOS(DEC)
      x = DCOS(RA)*CD
      y = DSIN(RA)*CD
      z = DSIN(DEC)
C     ....Rotate vector into camera coordinates using inverse C-matrix
      x0 = CM(1,1)*x + CM(2,1)*y + CM(3,1)*z
      y0 = CM(1,2)*x + CM(2,2)*y + CM(3,2)*z
      z0 = CM(1,3)*x + CM(2,3)*y + CM(3,3)*z
C     ....Project onto focal-plane and scale vector into pixels
      S = ZSCALE/z0		!ZSCALE=FL*PICSCALE
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert from (line,sample) to (lat,lon) or (radius,lon) 
C
      SUBROUTINE LATLON(IND,RLINE,RSAMP,rlat,rlon)
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
      real*4 rl4,rs4

      IF (ITYPE.EQ.7) THEN  !If image-space, convert to object=space.
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE),sngl(RSAMP),rl4,rs4,1,
     &   CONV,NPH,NPV,ind)
	rl=rl4
	rs=rs4
      ELSE		    !Else, no conversion necessary.
	RL = RLINE
	RS = RSAMP
      ENDIF

      IF (MODEL.LE.3) THEN	!Convert from (line,samp) to (lat,lon)
          CALL PLANET(IND,RL,RS,rlat,rlon,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
          IF (IND.EQ.0) CALL XVMESSAGE('***Point off planet',' ')
          RETURN
      ENDIF

      IF (MODEL.EQ.4) THEN	!Convert from (line,samp) to (radius,lon)
        CALL RING(IND,RL,RS,radius,rlon,
     &   OM,PSC3,RLORA,OAL,OAS,ZSCALE)
        RLAT = RADIUS
        IF (IND.EQ.0) CALL XVMESSAGE('***Ring plane is not visible',
     &   ' ')
        RETURN
      ENDIF

      IF (MODEL.LE.5) THEN	!Convert from (line,samp) to (RA,DEC)
          CALL STAR(RL,RS,ras,dec,CM,OAL,OAS,ZSCALE)
          RLAT = RAS
          RLON = DEC
          IND = 1
          RETURN
      ENDIF

      CALL PRNT(4,1,MODEL,' ***Invalid target model=.')
      RETURN 
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert from (lat,lon) or (radius,lon) to (line,sample) 
C
      SUBROUTINE LINSAM(IND,RLAT,RLON,rline,rsamp)
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
      real*4 rl4,rs4

      IF (MODEL.LT.3.OR.MODEL.GT.5) THEN
          CALL PRNT(4,1,MODEL,' ***Invalid target model=.')
          IND = 0
          RETURN
      ENDIF

      IF (MODEL.LE.3) THEN	!Convert from (lat,lon) to (line,samp)
          CALL PLAINV(IND,RLAT,RLON,rline,rsamp,
     &           OM,PSC3,RLORA,OAL,OAS,ZSCALE)
          IF (IND.EQ.0) THEN
             CALL XVMESSAGE('***Point behind the planet',' ')
             RETURN
          ENDIF
      ENDIF

      IF (MODEL.EQ.4) THEN	!Convert from (radius,lon) to (line,samp)
          RADIUS = RLAT
          CALL RINV(IND,RADIUS,RLON,rline,rsamp,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      ENDIF

      IF (MODEL.EQ.5) THEN	!Convert from (RA,DEC) to (line,samp)
          RAS = RLAT
          DEC = RLON
          CALL STARINV(RAS,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
      ENDIF

      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	rline=rl4
	rsamp=rs4
      endif
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to calculate the phase angle at a particular point (radius,longitude)
C for the ring and (latitude,longitude) for the planet or satellite
C
      SUBROUTINE PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 add arg
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

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
C
C  All vectors are planet centered co-ordinates.  Convert (radius,longitude) 
C  or (latitude,longitude) of intercept point to X,Y,Z
      IF (MODEL.EQ.3) THEN
        CLAT = DCOS(RADIUS)			!RLAT = RADIUS
        SLAT = DSIN(RADIUS)
        CLON = DCOS(RLON-RLORA) ! Fixed by VRH 5/26/89 (RLORA replaces
        SLON = DSIN(RLON-RLORA) ! Fixed by VRH 5/26/89      SCLON)
CCC        EPSLN = (RA/RC)**2
CCC        R = RE/DSQRT((1-EPSLN)*CLAT**2+EPSLN)	!geocentric radius
        R = 1.D0/DSQRT(AI2*(CLAT*CLON)**2	!Planetocentric radius
     &		 + BI2*(CLAT*SLON)**2 + CI2*SLAT**2)
        XP3 = R*CLAT*CLON
        YP3 = R*CLAT*SLON
        ZP3 = R*SLAT
      ELSE IF (MODEL.EQ.4) THEN
        XP3 = RADIUS*DCOS(RLON-RLORA) !Fixed VRH 5/26/89  (RLORA replaces
        YP3 = RADIUS*DSIN(RLON-RLORA) !Fixed VRH 5/26/89       SCLON)
        ZP3 = 0.D0
      ENDIF
C  Get components of Spacecraft from intercept point, vector DX,DY,DZ
      DX = PSC3(1) - XP3
      DY = PSC3(2) - YP3
      DZ = PSC3(3) - ZP3
      RANGE = DSQRT(DX*DX + DY*DY + DZ*DZ)
c
c VRH added code to calculate particular info (one time deal) 3/94
c      sin_delta = DZ/RANGE
c      delta = dasind(sin_delta)
c      print*,'S/C is',delta,' deg above ring plane as seen from pt'
c
C  Get components of Sun from intercept point, vector DXSUN,DYSUN,DZSUN
      DXSUN = PSUN3(1) - XP3
      DYSUN = PSUN3(2) - YP3
      DZSUN = PSUN3(3) - ZP3
      SUNRAD = DSQRT(DXSUN*DXSUN + DYSUN*DYSUN + DZSUN*DZSUN)
C  Calculate Cos(phase)
      COSPHA = (DX*DXSUN + DY*DYSUN + DZ*DZSUN)/RANGE/SUNRAD
      PHA = DACOS(COSPHA)
C  Calculate Image scale (km/pixel)  ! VRH 3/1/94 add
      SCALE = RANGE/ZSCALE
C  Calculate Radial image scale (km/pixel) !VRH 3/1/94 add
      IF(MODEL.EQ.4) THEN
        COSPHI = -(DX*XP3 + DY*YP3)/RADIUS/RANGE
        PHI = DACOS(COSPHI)
        RSCALE = SCALE/DSIN(PHI)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to convert input latitude (RLAT) from planetodetic to planetocentric
C if IGEO=1.  If IGEO=0, the latitude is returned unchanged.
C
      FUNCTION GEOCEN(RLAT,RLON)
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

      IF (DABS(RLAT).GE.PI/2.D0) THEN
           GEOCEN = DSIGN(PI/2.D0,RLAT)
           RETURN
      ENDIF

      IF (IGEO.NE.1) THEN
           GEOCEN = RLAT
      ELSE
           EPS = (RC/(RA*RB))**2*
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)
           GEOCEN = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to convert input latitude (RLAT) from planetocentric to
C planetodetic if IGEO=1.  If IGEO=0, the latitude is returned
C unchanged. ! VRH changed description from GEOFLAG to IGEO 9/6/91
C
      FUNCTION GEODET(RLAT,RLON)
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

      IF (DABS(RLAT).GE.PI/2.D0) THEN
         GEODET = DSIGN(PI/2.D0,RLAT)
         RETURN
      ENDIF

      IF (IGEO.NE.1) THEN
         GEODET = RLAT
      ELSE
         EPS = ((RA*RB)/RC)**2/
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)
         GEODET = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert (line,sample) to camera unit vector
C
      SUBROUTINE IMG2OBJ(RLINE,RSAMP,vout)
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

      DOUBLEPRECISION VIN(3),VOUT(3),VLCL(3)
      REAL*4 RL,RS

      IF (ITYPE.EQ.7) THEN  !If image space, convert to object space.
         CALL CONVISOS(PROJECT,ICAM,SNGL(RLINE),SNGL(RSAMP),rl,rs,1,
     &                CONV,NPH,NPV,ind)
      ELSE		    !Else, no conversion necessary.
         RL = RLINE
         RS = RSAMP
      ENDIF

      VOUT(1) = RS - OAS
      VOUT(2) = RL - OAL
      VOUT(3) = ZSCALE
      CALL VHAT(VOUT,vout)	!Make it a unit vector
      RETURN

C********************************************************************
C Convert camera vector to (line,sample)
C
      ENTRY OBJ2IMG(VIN,rline,rsamp)

C     ....Test for vector pointing along or behind image plane
      IF (VIN(3).LE.0D0) THEN
        RLINE = -999D0
        RSAMP = RLINE
        RETURN
      ENDIF
C
C     ....Convert to vector with Z = ZSCALE
      CALL VSCL(ZSCALE/VIN(3),VIN,VLCL)
      RSAMP = VLCL(1) + OAS
      RLINE = VLCL(2) + OAL

      IF (ITYPE.EQ.7) THEN  !Convert object-space to image-space,
         CALL CONVISOS(PROJECT,ICAM,rl,rs,SNGL(RLINE),SNGL(RSAMP),0,
     &                CONV,NPH,NPV,ind)
	RLINE=RL
	RSAMP=RS
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert (line,sample) <=> object l,s <=> camera unit vec
C rotate one vector around another
C
      SUBROUTINE ROTVEC(ROT,VECIN,AXIVEC,VECOUT)
      IMPLICIT NONE
      DOUBLEPRECISION ROT,VECIN(3),AXIVEC(3),VECOUT(3)
      DOUBLEPRECISION QUART(0:3),QMTX(3,3),OLDROT,SINROT2,ROT2
      DATA QUART(0) / 1D0 /
      DATA OLDROT / 0D0 /
      DATA SINROT2 / 0D0 /
      SAVE

      IF (ROT .EQ. -OLDROT) THEN
         SINROT2 = -SINROT2
         OLDROT = ROT
      ELSE IF (ROT .NE. OLDROT) THEN
         ROT2 = ROT / 2D0
         QUART(0) = DCOS(ROT2)
         SINROT2 = DSIN(ROT2)
         OLDROT = ROT
      ENDIF
      IF (QUART(0) .EQ. 1D0) THEN
        CALL VEQU(VECIN,VECOUT)
      ELSE
        CALL VSCL(SINROT2,AXIVEC,QUART(1))
        CALL Q2M(QUART,QMTX)
        CALL MXV(QMTX,VECIN,VECOUT)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CBTC
CBTC END - rotvec
CBTC START + & - rotate vector around axis,convert to image space,update
CBTC       array of image space line,samp,count how many are within
CBTC       image space limits
CBTC SUBROUTINE ROTVECS2IMG
CBTC Inputs:
CBTC   VECIN(3)    DP   Vector to rotate
CBTC   AXIVEC(3)   DP   Axis of rotation
CBTC   ROT         DP   Rotation,radians
CBTC   SL,SS,EL,ES DP   Image space limits - determines which results cause
CBTC                      NPTS to be updated
CBTC Outputs:
CBTC   NPTS        INT  Counter to be incremented for each line,samp 
CBTC                      within limits
CBTC   PTS(2,2)    R4   Array of line,samp pairs for positive & negative rot
CBTC
      subroutine rotvecs2img(vecin,axivec,rot,sl,ss,el,es
     &                      ,npts,pts)
      implicitnone
      doubleprecision vecin(3),axivec(3),rot,sl,ss,el,es
      real*4 pts(2,2)
      integer npts
CBTC
      doubleprecision vec1(3),rline,rsamp
      integer i
CBTC********************************************************************
CBTC rotate negative
      call rotvec(-rot,vecin,axivec,vec1)
      call obj2img(vec1,rline,rsamp)
CBTC
CBTC copy rline,rsamp to pts(,1) if both are not outside limits & update npts,
CBTC otherwise copy to pts(,2)
      if (.not. (rline.lt.sl .or. rline.gt.el 
     &        .or. rsamp.lt.ss .or. rsamp.gt.es)) then
        pts(1,1) = rline
        pts(2,1) = rsamp
        i = 2
        npts = npts + 1
      else
        pts(1,2) = rline
        pts(2,2) = rsamp
        i = 1
      endif
CBTC
CBTC rotate positive 
      call rotvec(rot,vecin,axivec,vec1)
      call obj2img(vec1,rline,rsamp)
CBTC
CBTC copy rline,rsamp to pts(,i) & update npts if necessary
      pts(1,i) = rline
      pts(2,i) = rsamp
      if (.not. (rline.lt.sl .or. rline.gt.el 
     &        .or. rsamp.lt.ss .or. rsamp.gt.es)) npts = npts + 1
      return
      end
