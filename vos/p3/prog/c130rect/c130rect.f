      INCLUDE 'VICMAIN_FOR'
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 24 AUGUST 1983
C     MODIFIED TO VICAR2 BY RICH WALKER, 28 AUGUST 1985
C     FIXED BUG WHEN TILT > DEFL
C     MODIFIED FOR NON-BYTE DATA
C     CONVERTED TO UNIX VICAR  4/18/91 Ron Alley
C     Modified to handle multichannel (BSQ, BIL) datasets  1/24/97  Ron Alley
C**********************************************************************
      SUBROUTINE MAIN44
      LOGICAL XVPTST
      REAL PI/3.1415927/
      REAL RBUF(8000),OBUFR(32000),FRAC(32000)
      INTEGER ADDR(32000)
      CHARACTER*80 PRT
      CHARACTER*4 FORMAT
      CHARACTER *3 ORG
C
      CALL XVMESSAGE(' C130RECT VERSION 2.1',' ')
C							 OPEN THE INPUT DATA SET
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		      'U_FORMAT','REAL',' ')
      CALL XVGET(INP,ISTAT,'FORMAT',FORMAT,' ')
      CALL XVGET(INP,ISTAT,'ORG',ORG,' ')
      CALL XVSIZE(ISL,ISS,NLO,NS,NLI,NSI)
      CALL XVBANDS(ISB,NB,NBIN)
C
      IF (ORG .EQ. 'BIP') THEN
	 CALL XVMESSAGE(' BIP files are not currently supported',' ')
	 CALL ABEND
      ENDIF
C
      IF(ISS.NE.1) THEN
       CALL XVMESSAGE(' Starting sample must be 1',' ')
       CALL XVMESSAGE(' Resetting starting sample to 1',' ')
       ISS = 1
      ENDIF
      IF(NS.NE.0.AND.NS.NE.NSI) THEN
       CALL XVMESSAGE(' Number of samples must be entire input image',
     +			' ')
       CALL XVMESSAGE(' Resetting number of samples to entire input',
     +			' ')
       NS = NSI
      ENDIF
C
      IEL = ISL + NLO - 1
C
      IF(IEL.GT.NLI) THEN
	 CALL XVMESSAGE(
     +	     ' Size field NL specified overruns file dimensions',' ')
	 CALL ABEND
      ENDIF
C
C						        GET THE PARAMETERS
      CALL XVPARM('DEFL',DEFL,ICNT,IDEF,0)
      CALL XVPARM('TILT',TILT,ICNT,IDEF,0)
      CALL XVPARM('SCALE',SCALE,ICNT,IDEF,0)
C						        VERIFY THE PARAMETERS
      IF(DEFL.GT.85.)  DEFL= 85.
      WRITE (PRT,100) DEFL+0.0001
  100 FORMAT('.... DEFL=',F5.2,' DEG.')
      CALL XVMESSAGE(PRT,' ')
      WRITE (PRT,200) SCALE+0.0001
  200 FORMAT('.... SCALE=',F5.2)
      CALL XVMESSAGE(PRT,' ')
      IF (TILT.NE.0.) THEN
          WRITE (PRT,300) TILT+0.0001
  300     FORMAT('.... TILT=',F5.2,' DEG.')
          CALL XVMESSAGE(PRT,' ')
      END IF
      DEFL= DEFL*PI/180.0
      TILT = TILT*PI/180.0
C
      IF (TILT.NE.0.) THEN
          SMAX = NS
          H = (SMAX/(DEFL*2))*SCALE
          HH = H*COS(TILT)
          XMAX1 = HH*TAN(DEFL+TILT)
          XMAX2 = HH*TAN(DEFL-TILT)
          IMAX1 = XMAX1 + 0.5
          THETA = DEFL+TILT
          XMAX = XMAX1 + XMAX2
          IMAX = XMAX + 0.5
          NSO = XMAX+1.5
      ELSE
          R= NS
          SMAX= (R-1.)/2.
          H = (SMAX/DEFL)*SCALE
          XMAX= H*TAN(DEFL)
          NSO= 2.*XMAX+1.5
      END IF
C
      WRITE (PRT,400) NLO,NSO
  400 FORMAT('.... OUTPUT    NL=',I5,'   NS=',I5)
      CALL XVMESSAGE(PRT,' ')
      IF(NSO.GT.32000) THEN
	 CALL XVMESSAGE(' $$$ TOO MANY OUTPUT SAMPLES $$$',' ')
	 CALL ABEND
      ENDIF
C						        OPEN THE OUTPUT DATA SET
      CALL XVUNIT(OUT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA','LAB_ACT',
     +   'SA','U_NL',NLO,'U_NS',NSO,'OP','WRITE','U_FORMAT','REAL',' ')
C					      LOAD THE RESAMPLING ADDRESS BUFFER
      IF (TILT.NE.0) THEN
	  KK = MIN(NSO,IMAX1)
          DO J=1,KK
              R = J - 1
              S = H*ATAN((R+XMAX2-XMAX)/HH)+THETA*H
              IF (S.LT.0) S = 0.
              S = S/SCALE
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
	  END DO
          KK = MAX(IMAX1+1,1)
          DO J = KK,IMAX
              R=J-1
              S=H*ATAN((R-XMAX1)/HH)+THETA*H
              S = S/SCALE
              IF (S.LT.0) S=0.
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
 	  END DO
      ELSE
          DO J=1,NSO
              R= J-1
              X= R-XMAX
              THETA= ATAN(X/H)
              S = THETA*H/SCALE+SMAX
              IF(S.LT.0.)  S= 0.
              ADDR(J) = S
	      FRAC(J) = S - ADDR(J)
              ADDR(J) = ADDR(J) + 1
	  END DO
      END IF
C						 DUMP THE RESAMPLING ADDR BUFFER
      IF(XVPTST('DEBUG')) THEN
          CALL XVMESSAGE(' OUTPUT    INPUT',' ')
          DO J=1,NSO
              WRITE (PRT,500) J, ADDR(J)+FRAC(J)
  500         FORMAT(I5,F12.4)
              CALL XVMESSAGE(PRT,' ')
	  END DO
      END IF
C				    RESAMPLE EACH LINE TO CORRECT THE DISTORTION
      IF (ORG .EQ. 'BSQ') THEN
        IF (FORMAT.EQ.'REAL') THEN
          DO IB=1,NB
	    DO ILINE=ISL,IEL
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = (1.0-FRAC(I))*RBUF(ADDR(I)) +
     +                        FRAC(I)*RBUF(ADDR(I)+1)
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
            END DO
	  END DO
        ELSE
          DO IB=1,NB
	    DO ILINE=ISL,IEL
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = NINT( (1.0-FRAC(I))*RBUF(ADDR(I))
     +                            + FRAC(I)*RBUF(ADDR(I)+1))
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
            END DO
          END DO
	END IF
      ELSE
        IF (FORMAT.EQ.'REAL') THEN
	  DO ILINE=ISL,IEL
            DO IB=1,NB
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = (1.0-FRAC(I))*RBUF(ADDR(I)) +
     +                        FRAC(I)*RBUF(ADDR(I)+1)
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
	    END DO
          END DO
        ELSE
	  DO ILINE=ISL,IEL
            DO IB=1,NB
              CALL XVREAD(INP,RBUF,ISTAT,'LINE',ILINE,'BAND',IB,
     +                    'NSAMPS',NS,' ')
              RBUF(NS+1) = RBUF(NS)
              DO I=1,NSO
                OBUFR(I) = NINT( (1.0-FRAC(I))*RBUF(ADDR(I))
     +                            + FRAC(I)*RBUF(ADDR(I)+1))
              END DO
	      CALL XVWRIT(OUT,OBUFR,ISTAT,'NSAMPS',NSO,' ')
	    END DO
          END DO
        END IF
      END IF
      RETURN
      END
