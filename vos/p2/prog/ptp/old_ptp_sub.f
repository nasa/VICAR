C------------------------------------------------------------------	  
C   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE: 
C
C    None of the code below is called anymore, but it preserved
C    in case someone needs to run PTP on a very large image that
C    cannot be read into memory. The routine PROJECT is supposed
C    to read in 32 x 32 blocks into IBUF, and process only parts
C    of the image at a time. The current implementation just
C    reads in the whole file and is no better than the ICPROJECT
C    code (in fact, it is slower). These routines will need to
C    be hacked for 32 x 32 block buffering to work. See LGEOM for
C    a guide on how it is done there.
C
C     -- Niles Ritter.
C------------------------------------------------------------------	  

	  
C------------------------------------------------------------------	  
C ROUTINE PROJECT - part of PTP program
C  Project input image onto output image
C------------------------------------------------------------------	  

      SUBROUTINE PROJECT(PIC,NPIX2,IBUF,NBI2,BUF,NSO2,
     &     NLI,NSI,NLO,NLW,NSW,NSB,NR,NC,NBLK,DL,DS,INCLUDE)
      IMPLICIT NONE
      
      INTEGER I, I0, I1, I2, I3, I4, ICAMERA, ICAMERA2, IFDSC
      INTEGER IFDSC2, II, IIP, IL, IMOD, INCLUDE, IS, IUNIT, IVAL
      INTEGER IX, J, JMOD, L, NBI, NBI2, NBLK, NC, NLI, NLO, NLW
      INTEGER NPIX, NPIX2, NR, NSB, NSI, NSO, NSO2, NSW
      INTEGER*2 PIC(NSI,NLW),IBUF(NSB,NBLK),BUF(NSO2/2)
      INTEGER*4 OUNIT
      INTEGER S

      CHARACTER*80 MSG      
      
      REAL*8 A, ANGLN, ANGLN2, B, D, DEGRAD, DL, DS, DSQRT, EPSLN
      REAL*8 EPSLN2, EPSLN22, FL, FL2		, OAL, OAL2, OAS, OAS2
      REAL*8 OM, OM2, PI, R, RADDEG, RANGE, RANGE2, RE, RLINE, RP
      REAL*8 RS, RS2, RSAMP, SCALE, SCALE2, SCLAT, SCLAT2, SCLINE
      REAL*8 SCLINE2, SCLON, SCLON2, SCSAMP, SCSAMP2, STATUS, UX0
      REAL*8 UX3, UY0, UY3, UZ0, UZ3, X, Y, Z, ZSCALE, ZSCALE2

      COMMON/CIO/OUNIT,IUNIT

      COMMON/CONST/PI,DEGRAD,RADDEG,RE,RP,EPSLN

      COMMON/CMAP/IFDSC,ICAMERA,FL,OAL,OAS,SCALE,ZSCALE
      COMMON/CMAP/SCLINE,SCSAMP,SCLAT,SCLON,ANGLN,RANGE
      COMMON/CMAP/OM(3,3),RS(3),EPSLN2      
      COMMON/CMAP/IFDSC2,ICAMERA2,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/CMAP/SCLINE2,SCSAMP2,SCLAT2,SCLON2,ANGLN2,RANGE2
      COMMON/CMAP/OM2(3,3),RS2(3),EPSLN22

 1000 FORMAT(' ***Increase virtual memory size by',I4,'K')


      call init_spice
      npix=npix2/2
      nbi=nbi2/2
      nso=nso2/2

      IF (NBI .LT. (NBLK*NSB)) THEN		!IF not enough core...
	    CALL xvmessage(' ***Virtual memory too small',' ')
	    WRITE(MSG,1000) (NBLK*NSB-NBI)/1024+1
        CALL xvmessage(MSG,' ')
	    CALL ABEND
      ENDIF
C           Read in input image and segment into IBUF
      CALL PICSEG(IUNIT,PIC,IBUF,NLI,NSI,NBLK,NSB,NLW,NSW)


      DO L=1,NLO			!Image line loop

          DO  S=1,NSO			!Pixel loop
              IVAL = 0				!clear output dn
C         Vector from S/C to point on planet in camera coordinates
          ux0 = S - OAS2
          uy0 = L - OAL2
          uz0 = ZSCALE2
C         Convert vector to planet coordinate system (x3,y3,z3)
               ux3 = OM2(1,1)*ux0 + OM2(2,1)*uy0 + OM2(3,1)*uz0
               uy3 = OM2(1,2)*ux0 + OM2(2,2)*uy0 + OM2(3,2)*uz0
               uz3 = OM2(1,3)*ux0 + OM2(2,3)*uy0 + OM2(3,3)*uz0
C          Find where vector intersects planet
               A = ux3**2     + uy3**2     + EPSLN*uz3**2
               B = ux3*RS2(1) + uy3*RS2(2) + EPSLN*uz3*RS2(3)
               D = B*B - A*EPSLN22

               IF (D.LT.0.) THEN		!Point off the planet
                   IF (INCLUDE.EQ.0) CONTINUE
                   RLINE = L + DL	!If sky is to be included
                   RSAMP = S + DS        !just use constant offset
                   GOTO 60
               ENDIF

               r = (-B-DSQRT(D))/A	!Choose smaller root for point in front

               X = r*ux3 + RS2(1)		!(X,Y,Z) is point on planet
               Y = r*uy3 + RS2(2)
               Z = r*uz3 + RS2(3)

               ux3 = X - RS(1)		!vector from S/C to point on planet
               uy3 = Y - RS(2)
               uz3 = Z - RS(3)
               IF (INCLUDE.EQ.1) GOTO 50	!Skip test if sky is included
C          Back-of-planet test
               A = ux3**2    + uy3**2    + EPSLN*uz3**2
               B = ux3*RS(1) + uy3*RS(2) + EPSLN*uz3*RS(3)
               D = B*B - A*EPSLN2
               IF(D.GT.0.) THEN
                   r = (-B-DSQRT(D))/A
                   IF (r.LT.0.99999D0) CONTINUE		!Point behind the planet
               ENDIF
C          Rotate vector into camera coordinates
   50          ux0 = OM(1,1)*ux3 + OM(1,2)*uy3 + OM(1,3)*uz3
               uy0 = OM(2,1)*ux3 + OM(2,2)*uy3 + OM(2,3)*uz3
               uz0 = OM(3,1)*ux3 + OM(3,2)*uy3 + OM(3,3)*uz3
C          Scale vector into pixels
               SCALE = ZSCALE/uz0
               RLINE = SCALE*uy0 + OAL
               RSAMP = SCALE*ux0 + OAS

   60          IS = RSAMP
               IL = RLINE
               IF (IL.LT.1.OR.IS.LT.1) CONTINUE		!Point off image
               IF (IL.GE.NLI.OR.IS.GE.NSI) CONTINUE

C           Determine which block contains (IS,IL)
               I = (IS-1)/32
               J = (IL-1)/32
               IX = NC*J + I + 1
               IMOD = IS - 32*I
               JMOD = IL - 32*J
               I0 = 32*(JMOD-1)
               II = I0 + IMOD
               i1 = IBUF(II,IX)			!upper left pixel

               IF (IMOD .EQ. 32) THEN		!right margin of block
                    IIP = I0 + 1

                    i2 = IBUF(IIP,IX+1)		!upper right pixel

                    IF (JMOD .EQ. 32) THEN       !lower right corner of block
                         IX = IX + NC
                         i3 = IBUF(32,IX)	!lower left pixel
                         i4 = IBUF(1,IX+1)	!lower right pixel
                     ELSE
                         i3 = IBUF(II+32,IX)
                         i4 = IBUF(IIP+32,IX+1)
                     ENDIF

               ELSE				!not on right margin
                     i2 = IBUF(II+1,IX)
                     IF (JMOD .EQ. 32) THEN	!lower margin
                         IX = IX + NC
                         i3 = IBUF(IMOD,IX)
                         i4 = IBUF(IMOD+1,IX)
                     ELSE			!interior pixel
                         II = II + 32
                         i3 = IBUF(II,IX)
                         i4 = IBUF(II+1,IX)
                     ENDIF
               ENDIF

               X = RSAMP - IS
               Y = RLINE - IL
               IVAL = I1 + (I2-I1)*X + (I3-I1)*Y + (I1-I2-I3+I4)*X*Y
               BUF(S) = IVAL
          ENDDO  !Pixel Loop

          CALL xvwrit(OUNIT,BUF,status,' ')	!Write completed line to output image
          IF (status.ne.1) call xvsignal(ounit,status,1) 
      ENDDO   !Line Loop
      RETURN
      END
C------------------------------------------------------------------	  
C Segments input image into 32x32 areas and store in PIC
C
      SUBROUTINE PICSEG(IUNIT,PIC,IBUF,NLI,NSI,NBLK,NSB,NLW,NSW)
      integer*2 PIC(NSI,NLW),IBUF(NSB,NBLK)

      LINE = 1
      IBLK = 1

      DO 100 L=1,NLI,NLW

      DO LL=1,NLW			!Read in next NLW lines...
          IF (LINE .GT. NLI) THEN
               CALL MVE(2,NSI,0,PIC(1,LL),0,1)  !all zeroes
          ELSE
               CALL XVREAD(IUNIT,PIC(1,LL),status,' ')
	           IF (status.ne.1) call xvsignal(iunit,status,1) 
          ENDIF
          LINE = LINE + 1
      ENDDO

      DO I=1,NSI,NSW			!and carve them up into
          J = 1				!NSWxNLW areas

          DO LL=1,NLW
               CALL MVE(2,nsw,PIC(I,LL),IBUF(J,IBLK),1,1)
               J = J + NSW
          ENDDO
          IBLK = IBLK + 1
      ENDDO

  100 CONTINUE

      CALL XVCLOSE(IUNIT,status,' ')
      RETURN
      END
