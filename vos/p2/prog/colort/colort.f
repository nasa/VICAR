c
c
C  REVISION HISTORY
C	7-94 CRI MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      EXTERNAL WORK
      INCLUDE 'fortport'
      INTEGER*4 I,INUNIT1,INUNIT2,INUNIT3,ISTATUS,MODE,MODEOUT
      INTEGER*4 OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*8 N1,N2,N3,N4,N5,N6
      INTEGER*8 M1,M2,M3,CC1,T1,A1,A2
      INTEGER*4 ISL,ISS,IEL,NL,NS,NLI,NSI
      CHARACTER*4 DIM(3,8)
      CHARACTER*58 BUF

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
      Common /c2/ buf
      common /c3/ mode,isl,iss,iel,ns

      DATA DIM/'blue','grn ','red ','g   ','r   ','int ',
     +                 'x   ','y   ','int ','u   ','v   ','int ',
     +                 'hue ','sat ','int ','long','clat','rad ',
     +                 'hue ','sat ','rad ','a*  ','b*  ','l*  '/

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

      buf(1:20) = '                    '
      buf(20:40)= '                    '
      buf(41:58)= '                 '

      I   = 0
      INUNIT1 = 0
      INUNIT2 = 0
      INUNIT3 = 0
      ISTATUS = 0
      MODE = 0
      MODEOUT = 0
      OUTUNIT1 = 0
      OUTUNIT2 = 0
      OUTUNIT3 = 0
      A1 = 0
      A2 = 0
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      N5 = 0
      N6 = 0
      M1 = 0
      M2 = 0
      M3 = 0
      CC1 = 0
      ISL = 0
      ISS = 0
      IEL = 0
       NL = 0
       NS = 0
      NLI = 0
      NSI = 0
C
      CALL IFMESSAGE('** COLORT version 2017-05-31')
      CALL XVEACTION ('SA',' ')

C
C     Open input datasaets
C
      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','HALF',' ')
      call XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      call XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','HALF',' ')
      call XVUNIT(INUNIT3,'INP',3,ISTATUS,' ')
      call XVOPEN(INUNIT3,ISTATUS,'U_FORMAT','HALF',' ')

c
C     Process size field
C
      call XVSIZE(isl,iss,nl,ns,nli,nsi)
      iel = isl+nl-1

C
      call GETMODE(mode,modeout)

      call XVMESSAGE(buf,' ')

C     Open output datasets
C
      call XVUNIT(OUTUNIT1,'OUT',1,ISTATUS,' ')
      call XVOPEN(OUTUNIT1,ISTATUS,'OP','WRITE',
     +  'O_FORMAT','BYTE','U_FORMAT','HALF',' ')
      call XVUNIT(OUTUNIT2,'OUT',2,ISTATUS,' ')
      call XVOPEN(OUTUNIT2,ISTATUS,'OP','WRITE',
     +  'O_FORMAT','BYTE','U_FORMAT','HALF',' ')
      call XVUNIT(OUTUNIT3,'OUT',3,ISTATUS,' ')
      call XVOPEN(OUTUNIT3,ISTATUS,'OP','WRITE',
     +  'O_FORMAT','BYTE','U_FORMAT','HALF',' ')
C
C     Update outputs' labels with the space and dimension names 
C
      buf(42:53) = ' dimension ='

      buf(55:58) = dim(1,modeout)
      call XLADD(OUTUNIT1,'HISTORY','LABEL',BUF,ISTATUS,
     +           'FORMAT','STRING',' ') 
      buf(55:58) = dim(2,modeout)
      call XLADD(OUTUNIT2,'HISTORY','LABEL',BUF,ISTATUS,
     +           'FORMAT','STRING',' ') 
      buf(55:58) = dim(3,modeout)
      call XLADD(OUTUNIT3,'HISTORY','LABEL',BUF,ISTATUS,
     +           'FORMAT','STRING',' ') 
C
C     Set up for and call STACKA
C                               N1-N6 are I*2 arrays for inputs & outputs
C                               M1 is for lookup table I*2 (256,768)
C                               M2 is for a 1-D lookup tabel I*2 (768)
C 
      N1 = int8(2*NS)
      N2 = N1
      N3 = N1
      N4 = N1
      N5 = N1
      N6 = N1
      M1 = int8(256*768*2)         !393216
      M2 = int8(768*2)             !1536
      M3 = int8(256*256*2)         !
      T1 = int8(256*4)             !1024
      CC1 = int8(12000*4)          !48000
      A1 = int8(10001*4)           !40004
      A2 = int8(10001*2)           !20002

      call STACKA(25,work,23,n1,n2,n3,n4,n5,n6,m1,m2,cc1,
     &            t1,t1,t1,t1,t1,t1,t1,t1,t1,m3,m3,m3,a1,a2)
      return
      end
C*************************************************************************
      SUBROUTINE WORK(IN1,NN1,IN2,NN2,IN3,NN3,IOUT1,NN4,IOUT2,NN5,
     +             IOUT3,NN6,LUT,MM1,LUT1,MM2,CTAB,CC1,
     +             TAB1,TT1,TAB2,TT2,TAB3,TT3,TAB4,TT4,TAB5,TT5,TAB6,TT6,
     +             TAB7,TT7,TAB8,TT8,TAB9,TT9,LUTA,MM3,LUTB,MM4,LUTC,MM5,
     +             ARC1,AA1,ARC2,AA2)
      IMPLICIT NONE
      INTEGER*8 NN1,NN2,NN3,NN4,NN5,NN6,MM1,MM2,MM3,MM4,MM5,CC1
      INTEGER*8 TT1,TT2,TT3,TT4,TT5,TT6,TT7,TT8,TT9,AA1,AA2
      INTEGER*4 MODE,ISL,ISS,IEL,NS
      Integer*2 in1(2*ns),in2(2*ns),in3(2*ns),iout1(2*ns),iout2(2*ns),
     +          iout3(2*ns)
      Integer*2 lut(256,768),lut1(768),arc2(10001)
      integer*2 luta(256,256),lutb(256,256),lutc(65536)
      real*4 ctab(12000),arc1(10001)
      real*4 tab1(256),tab2(256),tab3(256),tab4(256),tab5(256),tab6(256)
      real*4 tab7(256),tab8(256),tab9(256)
c
      common /c3/ mode,isl,iss,iel,ns
c
c      If(mm2.ne.1536) then
c          call XVMESSAGE('Not enough space for stacka buffers',' ')
c          call ABEND
c      end if
c
C     The function of this subroutine is to divide up the buffer space
C     and to select the appropriate functional routine. 
C
cc      go to (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,
cc     +       1400) mode
C
C     To reach here, MODE must be incorrect
C
      if (mode .eq. 0) then
        call XVMESSAGE('??E - Mode = 0 error - identity transfomation',' ')
        call ABEND
      endif
C
C*****************************************************bgr to tristimulus
C
      if (mode .eq. 1) then 
          call BGR_TRIS(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,lut,
     +              lut1)
C
C*****************************************************tristimulus to bgr
C
      else if (mode .eq. 2) then  
          call TRIS_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,lut,
     +              lut1)
C
C******************************************bgr to 1931 CIE X,Y,Z system
C
      else if (mode .eq. 3) then 
          call BGR_CIE(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +             tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9)
c     +             lut(1,1),lut(1,3),lut(1,5),lut(1,7),lut(1,9),
c     +             lut(1,11),lut(1,13),lut(1,15),lut(1,17))
C
C******************************************1931 CIE X,Y,Z system to bgr
C
 
      else if (mode .eq. 4) then 
          call CIE_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +             tab1,tab2,tab3,tab4,tab5,tab6)
c     +             lut(1,1),lut(1,3),lut(1,5),lut(1,7),lut(1,9),
c     +             lut(1,11))
C
C******************************************bgr to MacAdam UCS space
C                                                 aka 1960 CIE-UCS space
      else if (mode .eq. 5) then 
          call BGR_UCS(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +             tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,
     +             luta,lutb)
c     +             lut(1,1),lut(1,3),lut(1,5),lut(1,7),lut(1,9),
c     +             lut(1,11),lut(1,13),lut(1,15),lut(1,17),lut(1,257),
c     +             lut(1,513))
C
C******************************************MacAdam UCS space to bgr
C
      else if (mode .eq. 6) then 
          call UCS_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +              tab1,tab2,tab3,tab4,tab5,tab6, luta,lutb)
C     +             lut(1,1),lut(1,3),lut(1,5),lut(1,7),lut(1,9),
C     +             lut(1,11),lut(1,257),lut(1,513))
C
C*************************************************************bgr to hsi
C
      else if (mode .eq. 7) then 
          call BGR_HSI(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +             arc1,tab1)
c     +             lut(1,1),lut(1,257))
C
C***************************************************************hsi to bgr
C
      else if (mode .eq. 8) then
          call HSI_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +             luta,lutc,tab1)
c     +             lut(1,1),lut(1,257),lut(1,513))
C
C*********************************************bgr to spherical coordinates
C
      else if (mode .eq. 9) then
          call BGR_SPH(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +              arc2,lutc)
c     +             lut(1,1),lut(1,257))
C
C***************************************spherical coordinates to bgr
C
      else if (mode .eq. 10) then
          call SPH_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +              tab1,lutc,tab2)
c     +             lut(1,1),lut(1,257),lut(1,513))
c
C*************************************************************bgr to hsr
C
      else if (mode .eq. 11) then
          call BGR_HSR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +              arc1,tab1,lutc)
c     +             lut(1,1),lut(1,257),lut(1,513))
C
C*************************************************************hsr to bgr
C
      else if (mode .eq. 12) then
          call HSR_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns,
     +              luta,lutc,tab1)
c     +             lut(1,1),lut(1,257),lut(1,513))
C
C********************************************************bgr to cuberoot
C
      else if (mode .eq. 13) then
          call BGR_CUB(in1,in2,in3,iout1,iout2,iout3,ctab,isl,iss,iel,ns
     +        )

C*********************************************************cuberoot to bgr
C
      else 
          call CUB_BGR(in1,in2,in3,iout1,iout2,iout3,isl,iss,iel,ns)
      endif

      return
      end
C*************************************************************************
      SUBROUTINE BGR_TRIS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                    LUT,LUT1) 
      IMPLICIT NONE
      INTEGER*4 N,I,K,J,LINE,ISL,IEL,NS,ISS,INUNIT1,INUNIT2,INUNIT3
      INTEGER*4 ISTATUS,OUTUNIT1,OUTUNIT2,OUTUNIT3
      REAL*4 X
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 LUT(256,768),LUT1(768)

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
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
      SUBROUTINE TRIS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                    LUT,LUT1) 
      IMPLICIT NONE
      INTEGER*4 NS,J,I,LINE,ISL,IEL,ISS,ISTATUS
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3

      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 LUT(256,768),LUT1(768)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
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
      SUBROUTINE BGR_CIE(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6,TAB7,TAB8,TAB9)
      IMPLICIT NONE

      INTEGER*4 NS,I,J,LINE,ISL,IEL,ISS,ISTATUS
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      REAL*4 XNORM,SCALE
      REAL*4 TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      REAL*4 TAB7(256),TAB8(256),TAB9(256)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
C                 
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL*4 BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL*4 BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL*4 BY/0.01063/,GY/0.81240/,RY/0.17697/
C
C      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              XNORM = TAB1(IN1(I)+1)+TAB2(IN2(I)+1)+TAB3(IN3(I)+1)
              IF(XNORM.EQ.0.0) THEN
                      IOUT1(I) = 85
                      IOUT2(I) = 85
                      IOUT3(I) = 0
                  ELSE
                      SCALE = 255.0/XNORM
                      IOUT1(I) = SCALE*(TAB4(IN1(I)+1)+TAB5(IN2(I)+1)+
     +                                  TAB6(IN3(I)+1))+0.5
                      IOUT2(I) = SCALE*(TAB7(IN1(I)+1)+TAB8(IN2(I)+1)+
     +                                  TAB9(IN3(I)+1))+0.5
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
      SUBROUTINE CIE_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,I,J,LINE,ISL,IEL,ISTATUS,ISS
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)

      REAL*4 A,B,C,D,Q,FAC1,FAC2,FAC3,X,TRUNC

      REAL*4 TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
C                 
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL*4 BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL*4 BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL*4 BY/0.01063/,GY/0.81240/,RY/0.17697/
C
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
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
      SUBROUTINE BGR_UCS(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6,TAB7,TAB8,TAB9,LUT1,
     +               LUT2)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER I,J,II,JJ,LINE,ISL,IEL,ISTATUS,ISS,NS,IX,IY
      REAL XNORM,SCALE,D

      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      REAL TAB7(256),TAB8(256),TAB9(256)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
C      TRUNC(X) = AMIN1(AMAX1(X,0.0),255.0)+0.5
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
C - jaw - MIN1 used to be IMIN1.  Changed for Linux platform.
              LUT1(II,I) = AMIN1(1600.0*JJ/D+0.5,255.0)
              LUT2(II,I) = AMIN1(2400*J/D+0.5,255.0)
          END DO
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              XNORM = TAB1(IN1(I)+1)+TAB2(IN2(I)+1)+TAB3(IN3(I)+1)
              IF(XNORM.EQ.0.0) THEN
                      IOUT1(I) = 84
                      IOUT2(I) = 126
                      IOUT3(I) = 0
                  ELSE
                      SCALE = 255.0/XNORM
                      IX = SCALE*(TAB4(IN1(I)+1)+TAB5(IN2(I)+1)+
     +                                  TAB6(IN3(I)+1))+1.5
                      IY = SCALE*(TAB7(IN1(I)+1)+TAB8(IN2(I)+1)+
     +                                  TAB9(IN3(I)+1))+1.5
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
      SUBROUTINE UCS_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +               TAB1,TAB2,TAB3,TAB4,TAB5,TAB6,LUT1,LUT2)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,J,II,JJ,LINE,ISL,IEL,ISTATUS,ISS,IX,IY
      REAL A,B,C,D,Q,FAC1,FAC2,FAC3,X,TRUNC

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
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IX = LUT1(IN1(I)+1,IN2(I)+1)
              IY = LUT2(IN1(I)+1,IN2(I)+1)
              IOUT3(I) = TRUNC(TAB1(IN3(I)+1)-FAC1*IX*IN3(I)+
     +                   FAC2*IY*IN3(I))
              IOUT2(I) = TRUNC(TAB2(IN3(I)+1)-FAC3*IX*IN3(I)-
     +                   TAB3(IOUT3(I)+1))
              IOUT1(I) = TRUNC(TAB4(IN3(I)+1)-TAB5(IOUT2(I)+1)-
     +                   TAB6(IOUT3(I)+1))
          END DO
          CALL XVWRIT(OUTUNIT1,IOUT1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,IOUT2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,IOUT3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_HSI(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   ARCSINE,SATLIMIT)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,LINE,ISL,IEL,ISS,ISTATUS,IBLUE,IGREEN,IRED
      INTEGER ISINESQ
      REAL X,Y,SCALE,SQRT6,SQRT2
C
      REAL*4 ARCSINE(10001),SATLIMIT(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
          ARCSINE(I) = 255.0*ASIN(SQRT(X))*180/3.14159
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
C - jaw - MAX0 used to be JMAX0.  Changed for Linux platform.
              IOUT3(I) = MAX0(IBLUE,IGREEN,IRED)
              IF(X.EQ.0.0) THEN
                      IF(Y.GT.0.0) THEN
                              IOUT1(I) = 64
                          ELSE
                              IOUT1(I) = 192
                      END IF
                  ELSE
C - jaw - 57.297 = 180/3.1415
                      IOUT1(I) = SCALE*ATAN2(Y,X)*57.297+0.5
                      IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+255
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                      ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                          (IBLUE*IBLUE+IGREEN*IGREEN+IRED*IRED)
                      IOUT2(I) = AMIN1(ARCSINE(ISINESQ)/
     +                                 SATLIMIT(IOUT1(I)+1),255.0)+0.5
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
      SUBROUTINE HSI_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   IDIST,ISQROOT,HUEFUNC)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,J,I,LINE,ISL,IEL,ISTATUS,ISS,IHUE,ISAT,INTEN
      INTEGER*4 CINDEX,RINDEX 
      REAL*4 SQRT6,SQRT2,SQRT32,X,Y,D,DD,RED,GREEN,BLUE,FACTOR,SCALE
C
      REAL*4 HUEFUNC(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 IDIST(256,256),ISQROOT(65536)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
C - jaw - 255*180/3.14159 = 14610.4
              IDIST(I,J) = 10000.0*SIN(HUEFUNC(I)*(J-1)/14610.4)+0.5
          END DO
      END DO
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
          IF(I.NE.65.AND.I.NE.193) THEN 
C - jaw - 180/3.14159 = 57.296
                  HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)/57.296)**2))
              ELSE
                  HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INTEN = IN3(I)
              D = INTEN*IDIST(IHUE,ISAT)/10000.0
              DD = D*D
              X = D/HUEFUNC(IHUE)
              CINDEX = INT(DD-X*X+1.5)
C              Y = ISQROOT(DD-X*X+1.5)/100.0  !17-Jul-2016 - remove Legacy Extension: REAL array index 
              Y = ISQROOT(CINDEX)/100.0
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              RINDEX = INT(INTEN*INTEN-DD+1.5)
C              RED = SQRT32*Y-X/SQRT2+ISQROOT(INTEN*INTEN-DD+1.5)/100.0
              RED = SQRT32*Y-X/SQRT2+ISQROOT(RINDEX)/100.0
              GREEN = RED+3.0*X/SQRT2-SQRT32*Y
              BLUE = RED-SQRT6*Y
C                                            Since intensity is scaled to
C                                            fraction of max possible,
C                                            rescaling is needed.
              FACTOR = INTEN/AMAX1(BLUE,GREEN,RED,1.0)
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
      SUBROUTINE BGR_SPH(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   INVSINE,ISQROOT)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,I,LINE,ISL,IEL,ISTATUS,ISS,IBLUE,IGREEN,IRED
      INTEGER*4 ISINESQ,CINDEX
      REAL*4 SQRT6,SQRT2,SCALE,X,Y,SATLIMIT
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536),INVSINE(10001) 
C
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
C - jaw - 255*3.14159/180 = 4.45048
      SATLIMIT = ATAN(SQRT2)/4.45048
      DO I=1,65536       
          ISQROOT(I) = SQRT(I-1.0)+0.5
      END DO
      DO I=1,10001
          X = (I-1)/10000.0
C - jaw - 180/3.14159 = 57.2958
          INVSINE(I) = AMIN1(ASIN(SQRT(X))*57.2958/SATLIMIT+0.5,255.0)
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              CINDEX = INT((IBLUE*IBLUE+IGREEN*IGREEN+IRED*IRED+4.5)/3.0)
c              IOUT3(I) = ISQROOT((IBLUE*IBLUE+IGREEN*IGREEN+
c     +                            IRED*IRED+4.5)/3.0)
              IOUT3(I) = ISQROOT(CINDEX)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
              IF(X.EQ.0.0) THEN
                      IF(Y.GT.0.0) THEN
                              IOUT1(I) = 64
                          ELSE
                              IOUT1(I) = 192
                      END IF
                  ELSE
C - jaw - 180/3.14159 = 57.2958
                      IOUT1(I) = SCALE*ATAN2(Y,X)*57.2958+0.5
                      IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+255
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                      ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                          (3.0*IOUT3(I)*IOUT3(I))
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
      SUBROUTINE SPH_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   DIST,ISQROOT,HUEFUNC)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,I,LINE,ISL,IEL,ISTATUS,ISS,IHUE,ISAT,INTEN
      INTEGER*4  CINDEX
      REAL*4 SCALE,SQRT6,SQRT2,SQRT32,D,DD,X,Y,SATSCALE,TRUNC
C
      REAL*4 HUEFUNC(256),DIST(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536)

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

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
C - jaw - 255*3.14159/180 = 4.45058
      SATSCALE = ATAN(SQRT2)/4.45058
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
C - jaw - 180/3.14159 = 57.2958
          DIST(I) = SIN((I-1)*SATSCALE/57.2958)
          IF(I.NE.65.AND.I.NE.193) THEN 
                  HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)/57.2958)**2))
              ELSE
                  HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INTEN = IN3(I)
              D = INTEN*DIST(ISAT)
              DD = D*D
              X = D/HUEFUNC(IHUE)
              CINDEX = INT(DD-X*X+1.5)
C              Y = ISQROOT(DD-X*X+1.5)/100.0
               Y = ISQROOT(CINDEX)/100.
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              CINDEX = INT(INTEN*INTEN-DD+1.5)
              IOUT3(I) = TRUNC(ISQROOT(CINDEX)/100.0+SQRT32*Y-X/SQRT2)
C              IOUT3(I) = TRUNC(ISQROOT(INTEN*INTEN-DD+1.5)/100.0+
C     +                         SQRT32*Y-X/SQRT2)
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
      SUBROUTINE BGR_HSR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   ARCSINE,SATLIMIT,ISQROOT)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,I,LINE,ISL,IEL,ISTATUS,ISS,IBLUE,IGREEN,IRED
      INTEGER*4 ISINESQ,CINDEX
      REAL*4 SQRT2,SQRT6,SCALE,X,Y
C
      REAL*4 ARCSINE(10001),SATLIMIT(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 ISQROOT(65536) 

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
          ARCSINE(I) = 255.0*ASIN(SQRT(X))*57.2958
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IBLUE = IN1(I)
              IGREEN = IN2(I)
              IRED = IN3(I)
              CINDEX = INT((IBLUE*IBLUE+IGREEN*IGREEN+IRED*IRED+4.5)/3.0)
C              IOUT3(I) = ISQROOT((IBLUE*IBLUE+IGREEN*IGREEN+
C     +                            IRED*IRED+4.5)/3.0)
              IOUT3(I) = ISQROOT(CINDEX)
              X = (IBLUE+IRED-2*IGREEN)/SQRT6
              Y = (IBLUE-IRED)/SQRT2
              IF(X.EQ.0.0) THEN
                      IF(Y.GT.0.0) THEN
                              IOUT1(I) = 64
                          ELSE
                              IOUT1(I) = 192
                      END IF
                  ELSE
C - jaw - 180/3.14159 = 57.2958
                      IOUT1(I) = SCALE*ATAN2(Y,X)*57.2958+0.5
                      IF(IOUT1(I).LT.0) IOUT1(I)=IOUT1(I)+255
              END IF
              IF(X.NE.0.0.OR.Y.NE.0.0) THEN
                      ISINESQ = 1.5 + 10000.0*(X*X+Y*Y)/
     +                          (3.0*IOUT3(I)*IOUT3(I))
                      IOUT2(I) = AMIN1(ARCSINE(ISINESQ)/
     +                                 SATLIMIT(IOUT1(I)+1),255.0)+0.5
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
      SUBROUTINE HSR_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS,
     +                   IDIST,ISQROOT,HUEFUNC)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,J,I,LINE,ISL,IEL,ISTATUS,ISS,IHUE,ISAT,INTEN
      INTEGER*4 CINDEX
      REAL*4 X,Y,SQRT2,SQRT6,SQRT32,SCALE,D,DD,TRUNC
C
      REAL*4 HUEFUNC(256)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*2 IDIST(256,256),ISQROOT(65536)

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

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
C - jaw - 255*180/3.14159 = 14610.4
              IDIST(I,J) = 10000.0*SIN(HUEFUNC(I)*(J-1)/14610.4)+0.5
          END DO
      END DO
      DO I=1,65536
          ISQROOT(I) = 100.0*SQRT(I-1.0)+0.5
      END DO
      DO I=1,256
          IF(I.NE.65.AND.I.NE.193) THEN 
                  HUEFUNC(I) = SQRT(1.0+(TAN(SCALE*(I-1)/57.2958)**2))
              ELSE
                  HUEFUNC(I) = 1.0E10
          END IF
      END DO
C
C     process images
C
      DO LINE=ISL,IEL
              CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              IHUE = IN1(I)+1
              ISAT = IN2(I)+1
              INTEN = IN3(I)
              D = INTEN*IDIST(IHUE,ISAT)/10000.0
              DD = D*D
              X = D/HUEFUNC(IHUE)
              CINDEX = INT(DD-X*X+1.5)
C              Y = ISQROOT(DD-X*X+1.5)/100.0
              Y = ISQROOT(CINDEX)/100.0
              IF(IHUE.LE.128) Y=-Y
              IF(IHUE.LE.64.OR.IHUE.GT.192) X=-X
              CINDEX = INT(INTEN*INTEN-DD+1.5)
c              IOUT3(I) = TRUNC(ISQROOT(INTEN*INTEN-DD+1.5)/100.0+
c     +                         SQRT32*Y-X/SQRT2)
              IOUT3(I) = TRUNC(ISQROOT(CINDEX)/100.0+SQRT32*Y-X/SQRT2)
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
      SUBROUTINE BGR_CUB(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,CUBEROOT,
     +         ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,I,LINE,ISL,IEL,ISTATUS,ISS
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)
      INTEGER*4 RINDEX,GINDEX,BINDEX
      REAL*4 C11,C12,C13,C21,C22,C23,C32,C33
      REAL*4 B,G,R,BLUE,GREEN,RED,X,THIRD
C
      REAL*4 CUBEROOT(12000)

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
          DO I=1,NS
              B = IN1(I)
              G = IN2(I)
              R = IN3(I)
              RINDEX = INT(C11*R+C12*G+C13*B+1.5)
C              RED = CUBEROOT(C11*R+C12*G+C13*B+1.5)
              RED = CUBEROOT(RINDEX)
              GINDEX = INT(C21*R+C22*G+C23*B+1.5)
C              GREEN = CUBEROOT(C21*R+C22*G+C23*B+1.5)
              GREEN = CUBEROOT(GINDEX)
              BINDEX = INT(C32*G+C33*B+1.5)
C              BLUE = CUBEROOT(C32*G+C33*B+1.5)
              BLUE = CUBEROOT(BINDEX)
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
      SUBROUTINE CUB_BGR(IN1,IN2,IN3,IOUT1,IOUT2,IOUT3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER*4 INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER*4 NS,LINE,ISL,IEL,ISTATUS,ISS,I
      REAL*4 C11,C12,C13,C21,C22,C23,C32,C33
      REAL*4 C12A,C13A,C23A,X,RED,GREEN,BLUE,TRUNC
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),IOUT1(NS),IOUT2(NS),IOUT3(NS)

      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

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
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
              CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +                    'NSAMPS',NS,' ')
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
      IMPLICIT NONE
      INTEGER I
      REAL SQRT2,DNTODEGREE,X
      REAL SATLIMIT(256)
      SQRT2 = SQRT(2.0)
      DNTODEGREE = 360.0/256.0
      X = 90.0
      DO I=1,256
C - jaw - 180/3.14159 = 57.2958
          SATLIMIT(I) = ATAN(1.0/(SQRT2*SIN(X/57.2958)))*57.2958
          X = X-DNTODEGREE
          IF(X.LT.30.0) X=X+120.0
      END DO
      RETURN
      END
C*****************************************************************************
      SUBROUTINE GETMODE(MODE,MODEOUT)
C     This subroutine returns in the first argument the corresponding mode code.
C     The second argument outputs the code for the output space alone.
C     The third argument is a print buffer, return to the calling
C     routine for label updates.
C 
      IMPLICIT NONE
      INTEGER*4 MODECODE,MODE,MODEOUT

      Integer*4 m(2),icount,idef,n
      character*12  from,to
      CHARACTER*58 BUF

      COMMON /C2/ BUF

      BUF(1:58) = '                                                          '
      from = '            '
      to   = '            '
      buf(1:20) = 'Transformation from '

      m(1)=0
      m(2)=0
C     Parameter processing
C     Parameter "FROM"
C     N is the length of the string. Default='BGR'
      n=3
      modecode=0               !no 'from' given

      Call XVPARM('FROM',from,icount,idef,12)

      If (idef .eq. 0) n = 8

      buf(30:31) = 'to'
      buf(21:21+n) = from

      if(from.eq.'TRISTIM ') modecode=1
      if(from.eq.'CIE     ') modecode=2
      if(from.eq.'UCS     ') modecode=3
      if(from.eq.'HSI     ') modecode=4
      if(from.eq.'SPHERIC ') modecode=5
      if(from.eq.'HSR     ') modecode=6
      if(from.eq.'CUBEROOT') modecode=7
      if(from.eq.'BGR     ') modecode=0   !17-jul-2016
      if(from.eq.'DN      ') modecode=0   !17-jul-2016
      m(1)=modecode
C
C     parameter "TO"
      n=3
      modecode=0                !no "to" given
      call XVPARM('TO',to,icount,idef,12)
      If (idef .eq. 0) n = 8

      buf(33:33+n) = to

      if(to .eq.'TRISTIM ') modecode=1
      if(to .eq.'CIE     ') modecode=2
      if(to .eq.'UCS     ') modecode=3
      if(to .eq.'HSI     ') modecode=4
      if(to .eq.'SPHERIC ') modecode=5
      if(to .eq.'HSR     ') modecode=6
      if(to .eq.'CUBEROOT') modecode=7
      if(to .eq.'BGR     ') modecode=0   !17-jul-2016
      if(to .eq.'DN      ') modecode=0   !17-jul-2016

      m(2)=modecode
C
C    Check for error
      If ((m(1).eq.0.and.m(2).eq.0) .or. (m(1).ne.0.and.m(2).ne.0)) then
         call XVMESSAGE('??E - Either "FROM" or "TO" must be specified',
     +             ' ')
         call ABEND
      else
          mode= 2*(m(1)+m(2))
          if(m(1).eq.0) mode=mode-1
          modeout = m(2)+1
      end if
      return
      end 
C*********************************************************************** 
      INTEGER FUNCTION MODECODE(I)
      MODECODE = 0
      RETURN
      END
