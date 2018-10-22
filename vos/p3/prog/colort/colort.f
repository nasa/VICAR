      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      EXTERNAL WORK
C
C   parameters...
C
C     radiance space (acquired data)
C        DN     = DN color space
C
C     two-dimensional chromaticity spaces
C        TRIS   = tristimulus chromaticity space
C                 G and R are scaled so that 1. = 255 DN
C        CIE    = CIE chromaticity space
C                 X and Y are scaled so that 1. = 255 DN
C        UCS    = UCS chromaticity space (1960)
C                 U and V are scaled so that 1. = 400 DN
C
C     three-dimensional chromaticity spaces
C        CUBE   = cube root color space - approx. to Munsell space 
C                 the cube root space is defined in Wyszecki and Stiles,
C                 Color Science - pp. 459,460.
C
C     spherical coordinate spaces
C        SPH    = spherical coordinate system (hue,colatitude,radiance)
C        HSR    = hue, saturation, radiance space
C        HSI    = hue, saturation, intensity space
C                 'hue' = longitude
C                 'saturation' = colatitude normalized to the maximum
C                 colatitude at a given hue
C                 'intensity' = radiance normalized to the maximum
C                 radiance at a given hue and colatitude
C                 100 percent = DN 255
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
      CALL XVMESSAGE(' *** This is version 10.0 ***',' ')
C
C     Open input datasaets
C
      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
      CALL XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
      CALL XVUNIT(INUNIT3,'INP',3,ISTATUS,' ')
      CALL XVOPEN(INUNIT3,ISTATUS,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
c
C     Process size field
C
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      IEL = ISL+NL-1
C
C     Open output datasets
C
      CALL XVUNIT(OUTUNIT1,'OUT',1,ISTATUS,' ')
      CALL XVOPEN(OUTUNIT1,ISTATUS,'OP','WRITE','OPEN_ACT','SA',
     +		  'IO_ACT','SA','O_FORMAT','BYTE','U_FORMAT','HALF',' ')
      CALL XVUNIT(OUTUNIT2,'OUT',2,ISTATUS,' ')
      CALL XVOPEN(OUTUNIT2,ISTATUS,'OP','WRITE','OPEN_ACT','SA',
     +		  'IO_ACT','SA','O_FORMAT','BYTE','U_FORMAT','HALF',' ')
      CALL XVUNIT(OUTUNIT3,'OUT',3,ISTATUS,' ')
      CALL XVOPEN(OUTUNIT3,ISTATUS,'OP','WRITE','OPEN_ACT','SA',
     +		  'IO_ACT','SA','O_FORMAT','BYTE','U_FORMAT','HALF',' ')
C
      CALL GETMODE
C
C     Set up for and call STACKA
C                               N1-N6 are I*2 arrays for inputs & outputs
C                               M1 is for lookup table I*2 (256,768)
C                               M2 is for a 1-D lookup tabel I*2 (768)
C 
      N1 = 2*NS
      N2 = N1
      N3 = N1
      N4 = N1
      N5 = N1
      N6 = N1
      M1 = 393216
      M2 = 1536
      CALL STACKA(10,WORK,8,N1,N2,N3,N4,N5,N6,M1,M2)
      RETURN
      END
C*************************************************************************
	SUBROUTINE WORK(IN1,NN1,IN2,NN2,IN3,NN3,IOUT1,NN4,IOUT2,NN5,
     +                IOUT3,NN6,LUT,MM1,LUT1,MM2)
C
	INTEGER*2 IN1(*),IN2(*),IN3(*),IOUT1(*),IOUT2(*),IOUT3(*)
	INTEGER*2 LUT(256,768),LUT1(768)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The function of this subroutine is to divide up the buffer space
C     and to select the appropriate functional routine. 
C
	IF (MODE .EQ. 1) THEN
	    CALL BGR_TRIS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT,LUT1)
	ELSE IF (MODE .EQ. 2) THEN
	    CALL TRIS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT)
	ELSE IF (MODE .EQ. 3) THEN
	    CALL BGR_CIE(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,3),LUT(1,5),LUT(1,7),LUT(1,9),LUT(1,11),
     +			 LUT(1,13),LUT(1,15),LUT(1,17))
	ELSE IF (MODE .EQ. 4) THEN
	    CALL CIE_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,3),LUT(1,5),LUT(1,7),LUT(1,9),LUT(1,11))
	ELSE IF (MODE .EQ. 5) THEN
	    CALL BGR_UCS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,3),LUT(1,5),LUT(1,7),LUT(1,9),LUT(1,11),
     +			 LUT(1,13),LUT(1,15),LUT(1,17),LUT(1,257),
     +			 LUT(1,513))
	ELSE IF (MODE .EQ. 6) THEN
	    CALL UCS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,3),LUT(1,5),LUT(1,7),LUT(1,9),LUT(1,11),
     +			 LUT(1,257),LUT(1,513))
	ELSE IF (MODE .EQ. 7) THEN
	    CALL BGR_HSI(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257))
	ELSE IF (MODE .EQ. 8) THEN
	    CALL HSI_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257),LUT(1,513))
	ELSE IF (MODE .EQ. 9) THEN
	    CALL BGR_SPH(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257))
	ELSE IF (MODE .EQ. 10) THEN
	    CALL SPH_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257),LUT(1,513))
	ELSE IF (MODE .EQ. 11) THEN
	    CALL BGR_HSR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257),LUT(1,513))
	ELSE IF (MODE .EQ. 12) THEN
	    CALL HSR_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1),
     +			 LUT(1,257),LUT(1,513))
	ELSE IF (MODE .EQ. 13) THEN
	    CALL BGR_CUB(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT(1,1))
	ELSE IF (MODE .EQ. 14) THEN
	    CALL CUB_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3)
	ELSE
	    CALL XVMESSAGE(' Mode error',' ')
	    CALL ABEND
	END IF
	RETURN
	END
C*************************************************************************
      SUBROUTINE BGR_TRIS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT,LUT1) 
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 LUT(256,768),LUT1(768)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C
C     set up lookup tables
C          LUT1 is used for intensity
C          LUT is used for both g and r.
C             sum = bl+gr+rd
C               g = 255*gr/sum
C               r = 255*rd/sum
C             int = sum/3
C            LUT1(sum+1) = int
C            LUT(gr+1,sum+1) = g
C            LUT(rd+1,sum+1) = r
C
      LUT1(1) = 0
      LUT1(2) = 0
      DO I=1,256
          LUT(I,1) = 0
          LUT(I,2) = 255
      END DO
      LUT(1,2) = 0
      DO K = 2,767
          J = K+1
          X = 255.0/K
          LUT1(J) = K/3.0 + 0.5
          DO I=1,256
              LUT(I,J) = X*(I-1) + 0.5
          END DO
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              N = IN1(I)+IN2(I)+IN3(I)+1
              IOUT1(I) = LUT(IN2(I)+1,N)
              IOUT2(I) = LUT(IN3(I)+1,N)
              IOUT3(I) = LUT1(N)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE TRIS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,LUT) 
      INTEGER*2 IN1(*),IN2(*),IN3(*),IOUT1(*),IOUT2(*),IOUT3(*)
      INTEGER*2 LUT(256,256)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     set up lookup table
C          LUT is used for rd and gr
C             gr = 3*int*g/255
C             rd = 3*int*r/255
C             bl = 3*int-gr-rd
C          LUT(i,j) = 3*(i-1)*(j-1)/255
C    
      DO J=1,256
          DO I=1,256
              LUT(I,J) = (I-1)*(J-1)/85.0 + 0.5
              IF(LUT(I,J).GT.255) LUT(I,J)=255
          END DO
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IOUT2(I) = LUT(IN1(I)+1,IN3(I)+1)
              IOUT3(I) = LUT(IN2(I)+1,IN3(I)+1)
              IOUT1(I) = 3*IN3(I)-IOUT2(I)-IOUT3(I)
              IF(IOUT1(I).LT.0) IOUT1(I)=0
              IF(IOUT1(I).GT.255) IOUT1(I)=255
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_CIE(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6,TAB7,TAB8,TAB9)
      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      REAL TAB7(256),TAB8(256),TAB9(256)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL BY/0.01063/,GY/0.81240/,RY/0.17697/
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
C
C     set up lookup tables
C          norm =  1.20063*bl + 1.13240*gr + 0.66697*rd
C             x = (0.20000*bl + 0.31000*gr + 0.49000*rd)/norm
C             y = (0.01063*bl + 0.81240*gr + 0.17697*rd)/norm
C          TAB1-TAB9 are used to do the multiplications
C
      DO I=1,256
          J = I-1
          TAB1(I) = BD*J
          TAB2(I) = GD*J
          TAB3(I) = RD*J
          TAB4(I) = BX*J
          TAB5(I) = GX*J
          TAB6(I) = RX*J
          TAB7(I) = BY*J
          TAB8(I) = GY*J
          TAB9(I) = RY*J
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              XNORM = TAB1(IN1(I)+1)+TAB2(IN2(I)+1)+TAB3(IN3(I)+1)
              IF(XNORM.EQ.0.0) THEN
                  IOUT1(I) = 85
                  IOUT2(I) = 85
                  IOUT3(I) = 0
              ELSE
                  SCALE = 255.0/XNORM
                  IOUT1(I) = NINT(SCALE*(TAB4(IN1(I)+1)+
     +				  TAB5(IN2(I)+1)+TAB6(IN3(I)+1)))
                  IOUT2(I) = NINT(SCALE*(TAB7(IN1(I)+1)+
     +				  TAB8(IN2(I)+1)+TAB9(IN3(I)+1)))
                  IOUT3(I) = NINT(XNORM/3.0)
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE CIE_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6)
      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C                 
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL BY/0.01063/,GY/0.81240/,RY/0.17697/
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)
C
C     all this is to perform the inverse of the equations in BGR_CIE
C
      A = GD*BX-BD*GX
      B = RD*BX-BD*RX
      C = GD*BY-BD*GY
      D = RD*BY-BD*RY
      Q = B*C-A*D
      FAC1 = 3.0*C*BD/(255.0*Q)
      FAC2 = 3.0*A*BD/(255.0*Q)
      FAC3 = 3.0*BD/(255.0*A)
      DO I=1,256
          J = I-1
          TAB1(I) = J*3.0*(C*BX-A*BY)/Q
          TAB2(I) = J*3.0*BX/A
          TAB3(I) = J*B/A
          TAB4(I) = J*3.0/BD
          TAB5(I) = J*GD/BD
          TAB6(I) = J*RD/BD
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IOUT3(I) = TRUNC(TAB1(IN3(I)+1)-FAC1*IN1(I)*IN3(I)+
     +                   FAC2*IN2(I)*IN3(I)+0.5)
              IOUT2(I) = TRUNC(TAB2(IN3(I)+1)-FAC3*IN1(I)*IN3(I)-
     +                   TAB3(IOUT3(I)+1)+0.5)
              IOUT1(I) = TRUNC(TAB4(IN3(I)+1)-TAB5(IOUT2(I)+1)-
     +                   TAB6(IOUT3(I)+1)+0.5)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_UCS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,TAB1,TAB2,TAB3,
     +			 TAB4,TAB5,TAB6,TAB7,TAB8,TAB9,LUT1,LUT2)
      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      REAL TAB7(256),TAB8(256),TAB9(256)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL BY/0.01063/,GY/0.81240/,RY/0.17697/
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 LUT1(256,256),LUT2(256,256)
C
C     set up lookup tables
C          norm =  1.20063*bl + 1.13240*gr + 0.66697*rd
C             x = (0.20000*bl + 0.31000*gr + 0.49000*rd)/norm
C             y = (0.01063*bl + 0.81240*gr + 0.17697*rd)/norm
C             u = 4x/(-2x+12y+3)
C             v = 6y/(-2x+12y+3)
C          TAB1-TAB9 are used to do the multiplications
C          U = LUT1(x,y)
C          V = LUT2(x,y)
C 
      DO I=1,256
          J = I-1
          TAB1(I) = BD*J
          TAB2(I) = GD*J
          TAB3(I) = RD*J
          TAB4(I) = BX*J
          TAB5(I) = GX*J
          TAB6(I) = RX*J
          TAB7(I) = BY*J
          TAB8(I) = GY*J
          TAB9(I) = RY*J
          DO II=1,256
              JJ = II-1
              D = 12*J-2*JJ+765.01 
              LUT1(II,I) = MIN1(1600.0*JJ/D+0.5,255.0)
              LUT2(II,I) = MIN1(2400*J/D+0.5,255.0)
          END DO
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              XNORM = TAB1(IN1(I)+1)+TAB2(IN2(I)+1)+TAB3(IN3(I)+1)
              IF(XNORM.EQ.0.0) THEN
                  IOUT1(I) = 84
                  IOUT2(I) = 126
                  IOUT3(I) = 0
              ELSE
                  SCALE = 255.0/XNORM
                  IX = SCALE*(TAB4(IN1(I)+1)+TAB5(IN2(I)+1)+
     +                        TAB6(IN3(I)+1))+1.5
                  IY = SCALE*(TAB7(IN1(I)+1)+TAB8(IN2(I)+1)+
     +                        TAB9(IN3(I)+1))+1.5
                  IOUT1(I) = LUT1(IX,IY)
                  IOUT2(I) = LUT2(IX,IY)
                  IOUT3(I) = XNORM/3.0 + 0.5
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE UCS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,
     +                   TAB1,TAB2,TAB3,TAB4,TAB5,TAB6,LUT1,LUT2)
      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
C                 
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL BY/0.01063/,GY/0.81240/,RY/0.17697/
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 LUT1(256,256),LUT2(256,256)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)+0.5
C
C     all this is to perform the inverse of the equations in BGR_UCS
C
      A = GD*BX-BD*GX
      B = RD*BX-BD*RX
      C = GD*BY-BD*GY
      D = RD*BY-BD*RY
      Q = B*C-A*D
      FAC1 = 3.0*C*BD/(255.0*Q)
      FAC2 = 3.0*A*BD/(255.0*Q)
      FAC3 = 3.0*BD/(255.0*A)
      DO I=1,256
          J = I-1
          TAB1(I) = J*3.0*(C*BX-A*BY)/Q
          TAB2(I) = J*3.0*BX/A
          TAB3(I) = J*B/A
          TAB4(I) = J*3.0/BD
          TAB5(I) = J*GD/BD
          TAB6(I) = J*RD/BD
          DO II=1,256
              JJ = II-1
              D = 1600.01+2*JJ-8*J
              LUT1(II,I) = TRUNC(765.0*JJ/D)
              LUT2(II,I) = TRUNC(510.0*J/D)
          END DO 
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IX = LUT1(IN1(I)+1,IN2(I)+1)
              IY = LUT2(IN1(I)+1,IN2(I)+1)
              IOUT3(I) = TRUNC(TAB1(IN3(I)+1)-FAC1*IX*IN3(I)+
     +                   FAC2*IY*IN3(I)+0.5)
              IOUT2(I) = TRUNC(TAB2(IN3(I)+1)-FAC3*IX*IN3(I)-
     +                   TAB3(IOUT3(I)+1)+0.5)
              IOUT1(I) = TRUNC(TAB4(IN3(I)+1)-TAB5(IOUT2(I)+1)-
     +                   TAB6(IOUT3(I)+1)+0.5)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_HSI(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ARCSINE,SATLIMIT)
C
      REAL ARCSINE(10001),SATLIMIT(256)
      INTEGER*2 IN1(*),IN2(*),IN3(*),IOUT1(*),IOUT2(*),IOUT3(*)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The relevant equations are:
C          intensity = maximum(blue,green,red)
C
C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C                  I = sqrt(blue**2 + green**2 + red**2)
C
C                hue = arctan(y/x)     Note: This is a different definition
C                                            than in previous versions.
C                                            Magenta is now 0 DN, blue=43,
C                                            green=128, red=213.
C               sat = arcsin(d/I)/max possible arcsin(d/I) for this hue
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SCALE = 256.0/360.0
      CALL SATMAX(SATLIMIT)
      DO I=1,10001
          X = (I-1)/10000.0
          ARCSINE(I) = 255.0*ASIN(SQRT(X)*PI/180)
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
              IOUT3(I) = MAX0(IBLUE,IGREEN,IRED)
              IF(X.EQ.0.0) THEN
                  IF(Y.GT.0.0) THEN
                      IOUT1(I) = 64
                  ELSE
                      IOUT1(I) = 192
                  END IF
              ELSE
                  IOUT1(I) = SCALE*ATAN2(Y,X)*180/PI+0.5
                  IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+256
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                  ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                            (IBLUE*IBLUE+IGREEN*IGREEN+IRED*IRED)
                  IOUT2(I) = AMIN1(ARCSINE(ISINESQ)/
     +                       SATLIMIT(IOUT1(I)+1),255.0)+0.5
              ELSE
                  IOUT2(I) = 0
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE HSI_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,IDIST,ISQROOT,
     +			 HUEFUNC)
C
      REAL HUEFUNC(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 IDIST(256,256),ISQROOT(65536)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The relevant equations are:
C              d = intensity*sin(saturation/satlimit(hue))
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(intensity**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/256.0
      CALL SATMAX(HUEFUNC)
      DO J=1,256
          DO I=1,256
              IDIST(I,J) = 10000.0*SIN((HUEFUNC(I)*(J-1)/255.0)*PI/180)
     +				+0.5
          END DO
      END DO
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
          IF(I.NE.65.AND.I.NE.193) THEN 
              HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)*PI/180)**2))
          ELSE
              HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INT = IN3(I)
              D = INT*IDIST(IHUE,ISAT)/10000.0
              DD = D*D
              X = D/HUEFUNC(IHUE)
              Y = ISQROOT(DD-X*X+1.5)/100.0
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              RED = SQRT32*Y-X/SQRT2+ISQROOT(INT*INT-DD+1.5)/100.0
              GREEN = RED+3.0*X/SQRT2-SQRT32*Y
              BLUE = RED-SQRT6*Y
C                                            Since intensity is scaled to
C                                            fraction of max possible,
C                                            rescaling is needed.
              FACTOR = INT/AMAX1(BLUE,GREEN,RED,1.0)
              IOUT1(I) = FACTOR*BLUE+0.5
              IOUT2(I) = FACTOR*GREEN+0.5
              IOUT3(I) = FACTOR*RED+0.5
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_SPH(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,INVSINE,ISQROOT)
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536),INVSINE(10001) 
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The relevant equations are:
C
C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C           radiance = sqrt((blue**2 + green**2 + red**2)/3)
C
C          longitude = arctan(y/x)     Note: This is a different definition
C                                            than in previous versions.
C                                            Magenta is now 0 DN, blue=43,
C                                            green=128, red=213.
C         colatitude = arcsin(d/radiance)/arctan(sqrt(2))
C
C                                      Note: Arctan(sqrt)) is the largest
C                                            possible colatitude for mapping
C                                            positive values of bgr.
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SCALE = 256.0/360.0
      SATLIMIT = ATAN(SQRT2)*180/PI/255.0
      DO I=1,65536       
          ISQROOT(I) = SQRT(I-1.0)+0.5
      END DO
      DO I=1,10001
          X = (I-1)/10000.0
          INVSINE(I) = AMIN1(ASIN(SQRT(X))*180/PI/SATLIMIT+0.5,255.0)
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              IOUT3(I) = ISQROOT((IBLUE*IBLUE+IGREEN*IGREEN+
     +                            IRED*IRED+4.5)/3.0)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
              IF(X.EQ.0.0) THEN
                  IF(Y.GT.0.0) THEN
                      IOUT1(I) = 64
                  ELSE
                      IOUT1(I) = 192
                  END IF
              ELSE
                  IOUT1(I) = SCALE*ATAN2(Y,X)*180/PI+0.5
                  IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+255
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                  ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                      (3.0*IOUT3(I)*IOUT3(I))
                  IOUT2(I) = INVSINE(ISINESQ)
              ELSE
                  IOUT2(I) = 0
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE SPH_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,DIST,ISQROOT,
     +			 HUEFUNC)
C
      REAL HUEFUNC(256),DIST(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)+0.5
C
C     The relevant equations are:
C              d = radiance*sin(saturation)
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(radiance**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/256.0
      SATSCALE = ATAN(SQRT2)*180/PI/255.0
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
          DIST(I) = SIN((I-1)*SATSCALE*PI/180.)
          IF(I.NE.65.AND.I.NE.193) THEN 
              HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)*PI/180)**2))
          ELSE
              HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INT = IN3(I)
              D = INT*DIST(ISAT)
              DD = D*D
              X = D/HUEFUNC(IHUE)
              Y = ISQROOT(DD-X*X+1.5)/100.0
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              IOUT3(I) = TRUNC(ISQROOT(INT*INT-DD+1.5)/100.0+
     +                         SQRT32*Y-X/SQRT2)
              IOUT2(I) = TRUNC(IOUT3(I)+3.0*X/SQRT2-SQRT32*Y)
              IOUT1(I) = TRUNC(IOUT3(I)-SQRT6*Y)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_HSR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ARCSINE,SATLIMIT,
     +			 ISQROOT)
C
      REAL ARCSINE(10001),SATLIMIT(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536) 
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The relevant equations are:
C
C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C           radiance = sqrt((blue**2 + green**2 + red**2)/3)
C
C                hue = arctan(y/x)     Note: This is a different definition
C                                            than in previous versions.
C                                            Magenta is now 0 DN, blue=43,
C                                            green=128, red=213.
C               sat = arcsin(d/I)/max possible arcsin(d/I) for this hue
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SCALE = 256.0/360.0
      CALL SATMAX(SATLIMIT)
      DO I=1,65536       
          ISQROOT(I) = SQRT(I-1.0)+0.5
      END DO
      DO I=1,10001
          X = (I-1)/10000.0
          ARCSINE(I) = 255.0*ASIN(SQRT(X))*180/PI
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              IOUT3(I) = ISQROOT((IBLUE*IBLUE+IGREEN*IGREEN+
     +                            IRED*IRED+4.5)/3.0)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
              IF(X.EQ.0.0) THEN
                  IF(Y.GT.0.0) THEN
                      IOUT1(I) = 64
                  ELSE
                      IOUT1(I) = 192
                  END IF
              ELSE
                  IOUT1(I) = SCALE*ATAN2(Y,X)*180/PI+0.5
                  IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+255
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                  ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                            (3.0*IOUT3(I)*IOUT3(I))
                  IOUT2(I) = AMIN1(ARCSINE(ISINESQ)/
     +                             SATLIMIT(IOUT1(I)+1),255.0)+0.5
              ELSE
                  IOUT2(I) = 0
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE HSR_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,IDIST,ISQROOT,
     +			 HUEFUNC)
C
      REAL HUEFUNC(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 IDIST(256,256),ISQROOT(65536)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)+0.5
C
C     The relevant equations are:
C              d = radiance*sin(saturation/satlimit(hue))
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(radiance**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/256.0
      CALL SATMAX(HUEFUNC)
      DO J=1,256
          DO I=1,256
              IDIST(I,J) = 10000.0*SIN((HUEFUNC(I)*(J-1)/255.0)*PI/180)
     +				+0.5
          END DO
      END DO
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
          IF(I.NE.65.AND.I.NE.193) THEN 
              HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)*PI/180)**2))
          ELSE
              HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INT = IN3(I)
              D = INT*IDIST(IHUE,ISAT)/10000.0
              DD = D*D
              X = D/HUEFUNC(IHUE)
              Y = ISQROOT(DD-X*X+1.5)/100.0
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              IOUT3(I) = TRUNC(ISQROOT(INT*INT-DD+1.5)/100.0+
     +                         SQRT32*Y-X/SQRT2)
              IOUT2(I) = TRUNC(IOUT3(I)+3.0*X/SQRT2-SQRT32*Y)
              IOUT1(I) = TRUNC(IOUT3(I)-SQRT6*Y)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_CUB(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,CUBEROOT)
C
      REAL CUBEROOT(12000)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
C     The relevant equations are taken from Wyszecki and Stiles, 
C          Color Science, pp 269,459-460.
C   
C          First, R, G, and B are redefined in terms of the 1931 CIE
C     standard observer.
C               R = 1.02*X = .7494*red + .2791*green + .1699*blue
C               G =      Y = .2653*red + .7171*green + .0089*blue
C               B =                    + .0075*green + .6984*blue
C          Then these are related to the cube-root color system
C     coordinates of L*, a*, and b*.
C              L* = 25.29*cuberoot(G)-18.38
C              a* = Ka*(cuberoot(R)-cuberoot(G))
C              b* = Kb*(cuberoot(G)-cuberoot(B))
C                      Ka=105.0 for R<G; Ka=125.0 for R>G
C                      Kb=30.5  for B<G; Kb=53.6  for B>G
C          The data are rescaled such that 4 DN = 1 unit in cuberoot
C     space, and offset such that:
C              L*=0  implies  DN=4*(18.38)
C              a*=0  implies  DN=100
C              b*=0  implies  DN=150  
C
C     set up lookup tables and constants
C
C                            These values are the color adjustment
C                            coefficients*10000/255.
      C11 = 29.387
      C12 = 10.950
      C13 =  6.663
      C21 = 10.405
      C22 = 28.134
      C23 =  0.347
      C32 =  0.293
      C33 = 27.388
C
      THIRD = 1.0/3.0
      DO I=1,12000
          X = (I-1)/10000.0
          CUBEROOT(I) = X**THIRD
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              B = IN1(I)
              G = IN2(I)
              R = IN3(I)
              RED = CUBEROOT(C11*R+C12*G+C13*B+1.5)
              GREEN = CUBEROOT(C21*R+C22*G+C23*B+1.5)
              BLUE = CUBEROOT(C32*G+C33*B+1.5)
C
              IF(GREEN.GT.RED) THEN
                  IOUT1(I) = 420.0*(RED-GREEN)+100.5
              ELSE
                  IOUT1(I) = AMIN1(500.0*(RED-GREEN)+100.5,255.0)
              END IF
C
              IF(GREEN.GT.BLUE) THEN
                  IOUT2(I) = 122.0*(GREEN-BLUE)+150.5
              ELSE
                  IOUT2(I) = 214.4*(GREEN-BLUE)+150.5
              END IF
C
              IOUT3(I) = 101.16*GREEN+0.5
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE CUB_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3)
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
C
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C
      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)+0.5
C
C     The relevant equations are the inverse of those given in the
C     subroutine BGR_CUB
C
C     set up constants
C
C                            These values are the color adjustment
C                            coefficients/255.
      C11 = .0029387
      C12 = .0010950
      C13 = .0006663
      C21 = .0010405
      C22 = .0028134
      C23 = .0000347
      C32 = .0000293*0.8
      C33 = .0027388
      C12A = C12/255.0
      C13A = C13/255.0
      C23A = C23/255.0
C
C
C     process images
C
      DO LINE=ISL,IEL
          CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                'NSAMPS',NS,' ')
          DO I=1,NS
              GREEN = IN3(I)/101.16
              IF(IN1(I).LT.100) THEN
                  RED = (IN1(I)-100)/420.0+GREEN
              ELSE
                  RED = (IN1(I)-100)/500.0+GREEN
              END IF
              IF(IN2(I).GT.150) THEN
                  BLUE = GREEN-(IN2(I)-150)/122.0
              ELSE
                  BLUE = GREEN-(IN2(I)-150)/214.4
              END IF
              BLUE = BLUE*BLUE*BLUE
              GREEN = GREEN*GREEN*GREEN
              RED = RED*RED*RED
              IF(BLUE.LE.0.0) THEN
                  IOUT1(I) = 0
              ELSE
                  IOUT1(I) = TRUNC(BLUE/(C33+C32*GREEN/BLUE))
              END IF
              IOUT2(I) = TRUNC((C21*(RED-C13*IOUT1(I))
     +                         -C11*(GREEN-C23*IOUT1(I)))/
     +                         (C12*C21-C11*C22))
              IOUT3(I) = TRUNC((RED-C12*IOUT2(I)-C13*IOUT1(I))/C11)
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C**************************************************************************
      SUBROUTINE SATMAX(SATLIMIT)
C
C     This routine computes a table of maximum possible angular deflection
C     from the achromatic line, as a function of hue. The values returned
C     are in hundredths of degrees.
C
      REAL SATLIMIT(256)
      SQRT2 = SQRT(2.0)
      DNTODEGREE = 360.0/256.0
      X = 90.0
      DO I=1,256
          SATLIMIT(I) = ATAN(1.0/(SQRT2*SIN(X*PI/180)))*180/PI
          X = X-DNTODEGREE
          IF(X.LT.30.0) X=X+120.0
      END DO
      RETURN
      END
C*****************************************************************************
      SUBROUTINE GETMODE
C     This subroutine finds the mode of operation, stores its code in the
C     COMMON variable MODE, reports it to the terminal (and/or log), and
C     updates the labels.
C
	CHARACTER*44 PRT
	CHARACTER*9 FROM,TO
	CHARACTER*4 DIM(3,8)/'blue','grn ','red ','g   ','r   ','int ',
     +			     'x   ','y   ','int ','u   ','v   ','int ',
     +			     'hue ','sat ','int ','long','clat','rad ',
     +			     'hue ','sat ','rad ','a*  ','b*  ','l*  '/
      COMMON /QQ/ INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3,
     +		  MODE,ISL,ISS,IEL,NS
C								Parameter "FROM"
	INMODE = 0
	CALL XVPARM('FROM',FROM,ICNT,IDEF,0)
	IF (FROM(1:3).EQ.'TRI') INMODE=1
	IF (FROM(1:3).EQ.'CIE') INMODE=2
	IF (FROM(1:3).EQ.'UCS') INMODE=3
	IF (FROM(1:3).EQ.'HSI') INMODE=4
	IF (FROM(1:3).EQ.'SPH') INMODE=5
	IF (FROM(1:3).EQ.'HSR') INMODE=6
	IF (FROM(1:3).EQ.'CUB') INMODE=7
C								Parameter "TO
	IOUTMODE = 0
	CALL XVPARM('TO',TO,ICNT,IDEF,0)
	IF (TO(1:3).EQ.'TRI') IOUTMODE=1
	IF (TO(1:3).EQ.'CIE') IOUTMODE=2
	IF (TO(1:3).EQ.'UCS') IOUTMODE=3
	IF (TO(1:3).EQ.'HSI') IOUTMODE=4
	IF (TO(1:3).EQ.'SPH') IOUTMODE=5
	IF (TO(1:3).EQ.'HSR') IOUTMODE=6
	IF (TO(1:3).EQ.'CUB') IOUTMODE=7
C								Check for errors
	IF (INMODE .EQ. IOUTMODE) THEN
	    CALL XVMESSAGE(' TO and FROM modes are the same',' ')
	    CALL ABEND
	ENDIF
	IF (INMODE.NE.0 .AND. IOUTMODE.NE.0) THEN
	    CALL XVMESSAGE(' Either TO or FROM must be BGR',' ')
	    CALL ABEND
	ENDIF
	MODE = 2*(INMODE+IOUTMOE)
	IF (INMODE .EQ. 0) MODE=MODE-1
C
	WRITE (PRT,10) FROM,TO
   10	FORMAT(' Transformation from ',A9,' to ',A9)
	CALL XVMESSAGE(PRT,' ')
C
C		       Update outputs' labels with the space and dimension names
C
	CALL XLADD(OUTUNIT1,'HISTORY','SPACE',TO,ISTATUS,
     +		  'FORMAT','STRING',' ')
	CALL XLADD(OUTUNIT1,'HISTORY','DIMENSION',DIM(IOUTMODE+1,1),
     +		   ISTATUS,'FORMAT','STRING',' ')
	CALL XLADD(OUTUNIT2,'HISTORY','SPACE',TO,ISTATUS,
     +		  'FORMAT','STRING',' ')
	CALL XLADD(OUTUNIT2,'HISTORY','DIMENSION',DIM(IOUTMODE+1,2),
     +		   ISTATUS,'FORMAT','STRING',' ')
	CALL XLADD(OUTUNIT3,'HISTORY','SPACE',TO,ISTATUS,
     +		  'FORMAT','STRING',' ')
	CALL XLADD(OUTUNIT3,'HISTORY','DIMENSION',DIM(IOUTMODE+1,3),
     +		   ISTATUS,'FORMAT','STRING',' ')
C
	RETURN
	END
