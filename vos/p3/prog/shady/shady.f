      INCLUDE 'VICMAIN_FOR'
C VICAR program SHADY - Introduces shading and contour lines into an image
C        SHADY  IN  OUT  user-parameters...
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      EXTERNAL SHADY
      LOGICAL XVPTST,NOSHAD,NOCONT,DBUG
	INTEGER*2 CTBL(256),STBL(512)
      INTEGER C(3),S(2)
      COMMON/C1/SL,SS,NL,NS,J1,J2,CTBL,STBL,DBUG,NOSHAD,NOCONT
      COMMON/C2/INUNIT,OUTUNIT

      CALL XVEACTION('SA',' ')
      CALL XVMESSAGE('SHADY version  16-JUL-1993',' ')

      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE',
     +'O_FORMAT','BYTE','U_FORMAT','HALF',' ')

C     ....Begin parameter processing
      CALL XVPARM('SHADE',N,COUNT,DEF,0)		!N=direction of sun

      CALL XVPARM('CONTOUR',C,COUNT,DEF,0)
      NSTART = C(1)	!Lowest DN of 2nd contour region
      NREG = C(2)	!Number of regions
      INT = C(3)	!DN spacing between regions

      CALL XVPARM('SCALE',S,COUNT,DEF,0)
      A = S(1)		!Shading scale
      B = S(2)		!Shading offset

      NOSHAD = XVPTST('NOSHADE')	!Suppress shading?
      NOCONT = XVPTST('NOCONTOU')	!Suppress contours?
      DBUG = XVPTST('DBUG')		!Print diagnostics?

C     ....Generate contour look-up table
      IF(NOCONT) GO TO 30
      IVAL = 0
      IF(NSTART.GT.0) CALL MVE(-6,NSTART,0,CTBL,0,1)
      LOOP = NREG - 2
      J = NSTART + 1
C
      DO  L=1,LOOP
            IVAL = IVAL + 1
        DO  I=1,INT
              IF(J.GT.256) GO TO 30
              CTBL(J) = IVAL
              J = J + 1
        END DO
      END DO
C
      IF(J .LT. 256) CALL MVE(-6,257-J,IVAL+1,CTBL(J),0,1)
C
C     ....Set shade indices
   30 NSP = NS + 2
      IF (NOSHAD) THEN
          IF (NOCONT) THEN
	      CALL XVMESSAGE(
     +	      'Both shading and contours suppressed, task cancelled',' ')
	      CALL ABEND
	  ELSE
	      GO TO 37
	  END IF
      END IF
      M = N
      IF(M.GT.3) M = M - 4
      IF(M.NE.0) GO TO 301
      J1 = NSP + 3
      J2 = NSP + 1
      GO TO 303
  301 J1 = 3
      J2 = 2*NSP + 1
      DO I=1,3
           IF(M.EQ.I) GO TO 303
           J1 = J1 - 1
           J2 = J2 + 1
      END DO
  303 IF(M.EQ.N) GO TO 304
      M = J1
      J1 = J2
      J2 = M
  304 CONTINUE

C************************************
C                                   *
C     GENERATE SCALE LOOK-UP TABLE  *
C                                   *
C************************************         
      DO  I=1,512
            IVAL = (I-256)*A + B
            IF(IVAL.LT.0) IVAL = 0
            IF(IVAL.GT.255) IVAL = 255
            STBL(I) = IVAL
      END DO
   37 NSQ = NSP
      IF(NOCONT) NSQ = 2
      CALL STACKA(5,SHADY,3,2*NS,6*NSQ,6*NSP)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE SHADY(PIC,NXX,CBUF,NXY,BUF,NXZ)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 BUF(*),CBUF(*),PIC(*)
      LOGICAL DBUG,NOCONT,NOSHAD
	INTEGER*2 STBL(512),CTBL(256)
      CHARACTER*128 MSG
      COMMON/C1/SL,SS,NL,NS,J1,J2,CTBL,STBL,DBUG,NOSHAD,NOCONT
      COMMON/C2/INUNIT,OUTUNIT
C
      NSP = NS + 2
      NSP3 = 3*NSP
      IF(NXZ.LT.6*NSP) THEN
	 CALL XVMESSAGE('*** STACKA Failure ***',' ')
	 CALL ABEND
      ENDIF
      I1 = 2
      I2 = I1 + NSP
      I3 = I2 + NSP
      NS2 = NS*2
      IF (DBUG) THEN
	  CALL XVMESSAGE('CTBL',' ')
	  DO I=1,256,16
	      WRITE (MSG,38) (CTBL(J),J=I,I+15)
	      CALL XVMESSAGE(MSG,' ')
	  END DO
	  CALL XVMESSAGE('STBL',' ')
	  DO I=1,256,16
	      WRITE (MSG,38) (STBL(J),J=I,I+15)
	      CALL XVMESSAGE(MSG,' ')
	  END DO
      END IF
   38 FORMAT(16I8)

C***************************************************
C                                                  * 
C     READ IN FIRST LINE AND REFLECT AT MARGINS    *
C                                                  *
C***************************************************

      CALL XVREAD(INUNIT,BUF(I2),STATUS,'SAMP',SS,
     +            'NSAMPS',NS,' ')
      BUF(I2-1) = BUF(I2)
      BUF(I2+NS) = BUF(I2+NS-1)
      CALL MVE(2,NS+2,BUF(I2-1),BUF(I1-1),1,1)
      IF(.NOT.NOCONT) CALL CTAB(BUF(I2),CBUF(I2),CTBL,NS)

C*********************
C                    * 
C     MAIN LOOP      *
C                    *
C*********************

      DO  L=1,NL
            IF(L.NE.NL) GO TO 391
            CALL MVE(2,NS+2,BUF(I2-1),BUF(I3-1),1,1)
            IF(.NOT.NOCONT) CALL MVE(2,NS,CBUF(I2),CBUF(I3),1,1)
            GO TO 40
  391       CALL XVREAD(INUNIT,BUF(I3),STATUS,'SAMP',SS,
     +                  'NSAMPS',NS,' ')
   40       IF(NOSHAD) GO TO 50
            BUF(I3-1) = BUF(I3)
            BUF(I3+NS) = BUF(I3+NS-1)
            CALL SHADER(BUF(J2),BUF(J1),PIC,STBL,NS)
   50       IF(NOCONT) GO TO 60
            IF(NOSHAD) CALL MVE(2,NS,BUF(I2),PIC,1,1)
            CALL CTAB(BUF(I3),CBUF(I3),CTBL,NS)
            K2 = I2
            K3 = I3
            C1 = CBUF(I2)
            DO I=1,NS
                    C0 = CBUF(K2)
                    IF(C0.NE.C1.OR.C0.NE.CBUF(K3)) PIC(I) = 255
                    K2 = K2 + 1
                    K3 = K3 + 1
                    C1 = C0
            END DO  
   60       ISAVE = I1
            I1 = I2
            I2 = I3
            I3 = ISAVE
            J1 = J1 + NSP
            J2 = J2 + NSP
            IF(J1.GT.NSP3) J1 = J1 - NSP3
            IF(J2.GT.NSP3) J2 = J2 - NSP3
            CALL XVWRIT(OUTUNIT,PIC,STATUS,'NSAMPS',NS,' ')
      END DO
C
      RETURN
      END

	SUBROUTINE SHADER(BUF1,BUF2,PIC,STBL,NS)

C***************************************************
C                                                  *
C-----CONVERTED FROM THE IBM ASSEMBLER BY GMY 1-84 * 
C                                                  *
C***************************************************
	INTEGER*2 BUF1(*),BUF2(*),PIC(*),STBL(512)
C
	DO I=1,NS
      	     J = BUF1(I) - BUF2(I) + 256
  	     PIC(I) = STBL(J)
        END DO
C
	RETURN
	END

	SUBROUTINE CTAB(BUF,CBUF,CTBL,NS)

C***************************************************
C                                                  *
C-----CONVERTED FROM THE IBM ASSEMBLER BY GMY 1-84 * 
C                                                  *
C***************************************************
	INTEGER*2 BUF(*),CBUF(*),CTBL(*)
C
	DO  I=1,NS
              J = BUF(I) + 1
     	      CBUF(I) = CTBL(J)
        END DO
C
	RETURN
	END
