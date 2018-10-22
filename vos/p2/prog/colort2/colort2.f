C  REVISION HISTORY
c   2012-Dec-02 -lwk- revision of COLORT extending support up to real*4 data
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      EXTERNAL WORK
      INCLUDE 'fortport'
      INTEGER I,INUNIT1,INUNIT2,INUNIT3,ISTATUS,MODE,MODEOUT
      INTEGER OUTUNIT1,OUTUNIT2,OUTUNIT3,N1,N2,N3,N4,N5,N6
      INTEGER M1,M2
      INTEGER ISL,ISS,IEL,NL,NS,NLI,NSI,icount,idef
      real*4 maxval
      CHARACTER*4 DIM(3,8)
      CHARACTER*58 BUF
      character*4 fmt,fmt1
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
      Common /c2/ buf

      DATA DIM/'blue','grn ','red ','g   ','r   ','int ',
     +  'x   ','y   ','int ','u   ','v   ','int ','hue ',
     +  'sat ','int ','long','clat','rad ','hue ','sat ',
     +  'rad ','a*  ','b*  ','l*  '/

C   parameters...

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
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      N5 = 0
      N6 = 0
      M1 = 0
      M2 = 0

      ISL = 0
      ISS = 0
      IEL = 0
       NL = 0
       NS = 0
      NLI = 0
      NSI = 0

      CALL IFMESSAGE('COLORT2 version 2017-08-03')
      CALL XVEACTION ('SA',' ')

C     Open input datasets
      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','REAL',' ')
      call XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      call XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','REAL',' ')
      call XVUNIT(INUNIT3,'INP',3,ISTATUS,' ')
      call XVOPEN(INUNIT3,ISTATUS,'U_FORMAT','REAL',' ')

c     determine data format:
      call xvget(inunit1,istatus,'FORMAT',fmt,' ')
      if (fmt.ne.'BYTE' .and. fmt.ne.'HALF' .and. fmt.ne.'WORD'
     & .and. fmt.ne.'FULL' .and. fmt.ne.'REAL') call mabend(
     & 'unsuported data format')
      call xvget(inunit2,istatus,'FORMAT',fmt1,' ')
      if (fmt.ne.fmt1) call mabend('all inputs must have same format')
      call xvget(inunit3,istatus,'FORMAT',fmt1,' ')
      if (fmt.ne.fmt1) call mabend('all inputs must have same format')
      if (fmt.eq.'WORD') fmt = 'HALF'

C     Process size field
      call XVSIZE(isl,iss,nl,ns,nli,nsi)
      iel = isl+nl-1

      call GETMODE(mode,modeout)
c     if mode>14, then input is RGB -- transform to BGR:
      if (mode.gt.14) then
	mode = mode-14
	i = inunit1
	inunit1 = inunit3
	inunit3 = i
      endif
c  temporary limitations:
      if (mode.lt.7 .or. mode.gt.12) call mabend(
     & 'currently only HSI/HSR/SPH supported ...')

      call XVMESSAGE(buf,' ')

C     Open output datasets

      call XVUNIT(OUTUNIT1,'OUT',1,ISTATUS,' ')
      call XVOPEN(OUTUNIT1,ISTATUS,'OP','WRITE',
     +'O_FORMAT',fmt,'U_FORMAT','REAL',' ')
      call XVUNIT(OUTUNIT2,'OUT',2,ISTATUS,' ')
      call XVOPEN(OUTUNIT2,ISTATUS,'OP','WRITE',
     +'O_FORMAT',fmt,'U_FORMAT','REAL',' ')
      call XVUNIT(OUTUNIT3,'OUT',3,ISTATUS,' ')
      call XVOPEN(OUTUNIT3,ISTATUS,'OP','WRITE',
     +'O_FORMAT',fmt,'U_FORMAT','REAL',' ')

c     if modeout=9, then output is RGB; transform from BGR:
      if (modeout.eq.9) then
	modeout = 1
	i = outunit1
	outunit1 = outunit3
	outunit3 = i
      endif

C     Update outputs' labels with the space and dimension names 

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

c     When going from BGR/RGB, MAXVAL is required input, but
c     when going to BGR/RGB, it is taken from the label (if possible)

      if (mode/2 .ne. (mode+1)/2) then	! from BGR

        if (fmt.eq.'BYTE') then
          maxval = 255
        elseif (fmt.eq.'HALF') then
          call xvparm('MAXVAL',maxval,icount,idef,1)
          if (icount.eq.0) then
	    maxval = 4095
	    call xvmessage(' MAXVAL = 4095 assumed',' ')
          endif
        else
          call xvparm('MAXVAL',maxval,icount,idef,1)
          if (icount.eq.0) call mabend(
     &     'param. MAXVAL required for FULL or REAL data')
	endif

	call xladd(outunit1,'HISTORY','MAXVAL',maxval,istatus,
     &   'FORMAT','REAL',' ')
	call xladd(outunit2,'HISTORY','MAXVAL',maxval,istatus,
     &   'FORMAT','REAL',' ')
	call xladd(outunit3,'HISTORY','MAXVAL`',maxval,istatus,
     &   'FORMAT','REAL',' ')

      else		! to BGR

	call xlget(inunit1,'HISTORY','MAXVAL',maxval,istatus,
     &   'FORMAT','REAL',' ')
	if (istatus.ne.1) then
	  call xvmessage('MAXVAL not found in label',' ')
          if (fmt.eq.'BYTE') then
            maxval = 255
          else
            call xvparm('MAXVAL',maxval,icount,idef,1)
            if (icount.eq.0) call mabend(
     &       'parameter MAXVAL must be specified')
          endif
        endif

      endif

C     Set up for and call STACKA
C     N1-N6 are I*2 arrays for inputs & outputs

      N1 = 4*NS
      N2 = N1
      N3 = N1
      N4 = N1
      N5 = N1
      N6 = N1
      call STACKA(14,work,6,n1,n2,n3,n4,n5,n6,
     & mode,maxval+1,isl,iss,iel,ns)
      return
      end
C*************************************************************************
      SUBROUTINE WORK(IN1,NN1,IN2,NN2,IN3,NN3,out1,NN4,out2,NN5,
     + out3,NN6,MODE,maxval,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER NN1,NN2,NN3,NN4,NN5,NN6,MM1,MM2
      INTEGER MODE,ISL,ISS,IEL,NS,maxval
      real*4 in1(nn1),in2(nn2),in3(nn3),out1(nn4),out2(nn5),
     +          out3(nn6)

C     The function of this subroutine is to select the appropriate functional routine. 

      go to (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,
     + 1400) mode

C     To reach here, MODE must be incorrect
      call XVMESSAGE('Mode error',' ')
      call ABEND

C*****************************************************bgr to tristimulus

  100 continue
      call BGR_TRIS(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C*****************************************************tristimulus to bgr
C
  200 continue
      call TRIS_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C******************************************bgr to 1931 CIE X,Y,Z system
C
  300 continue
      call BGR_CIE(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C******************************************1931 CIE X,Y,Z system to bgr
C
  400 continue
      call CIE_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return 
C
C******************************************bgr to MacAdam UCS space
C                                                 aka 1960 CIE-UCS space
C
  500 continue
      call BGR_UCS(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C******************************************MacAdam UCS space to bgr
C
  600 continue
      call UCS_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C*************************************************************bgr to hsi
C
  700 continue
      call BGR_HSI(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
C
C***************************************************************hsi to bgr
C
  800 continue
      call HSI_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
C
C*********************************************bgr to spherical coordinates
C
  900 continue
      call BGR_SPH(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
C
C***************************************spherical coordinates to bgr
C
 1000 continue
      call SPH_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
c
C*************************************************************bgr to hsr
C
 1100 continue
      call BGR_HSR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
C
C*************************************************************hsr to bgr
C
 1200 continue
      call HSR_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns,
     + maxval)
      return
C
C********************************************************bgr to cuberoot
C
 1300 continue
      call BGR_CUB(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
C
C*********************************************************cuberoot to bgr
C
 1400 continue
      call CUB_BGR(in1,in2,in3,out1,out2,out3,isl,iss,iel,ns)
      return
      end
C*************************************************************************
      SUBROUTINE BGR_TRIS(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER N,I,K,J,LINE,ISL,IEL,NS,ISS,INUNIT1,INUNIT2,INUNIT3
      INTEGER ISTATUS,OUTUNIT1,OUTUNIT2,OUTUNIT3
      REAL X
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              out1(I) = LUT(IN2(I)+1,N)
              out2(I) = LUT(IN3(I)+1,N)
              out3(I) = LUT1(N)
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE TRIS_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER NS,J,I,LINE,ISL,IEL,ISS,ISTATUS
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3

      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              if (LUT(I,J).GT.255) LUT(I,J)=255
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
              out2(I) = LUT(IN1(I)+1,IN3(I)+1)
              out3(I) = LUT(IN2(I)+1,IN3(I)+1)
              out1(I) = 3*IN3(I)-out2(I)-out3(I)
              if (out1(I).LT.0) out1(I)=0
              if (out1(I).GT.255) out1(I)=255
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_CIE(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER NS,I,J,LINE,ISL,IEL,ISS,ISTATUS
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      REAL XNORM,SCALE
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
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              if (XNORM.EQ.0.0) THEN
                      out1(I) = 85
                      out2(I) = 85
                      out3(I) = 0
                  ELSE
                      SCALE = 255.0/XNORM
                      out1(I) = SCALE*(TAB4(IN1(I)+1)+TAB5(IN2(I)+1)+
     +                                  TAB6(IN3(I)+1))+0.5
                      out2(I) = SCALE*(TAB7(IN1(I)+1)+TAB8(IN2(I)+1)+
     +                                  TAB9(IN3(I)+1))+0.5
                      out3(I) = XNORM/3.0 + 0.5
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE CIE_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,J,LINE,ISL,IEL,ISTATUS,ISS
      REAL A,B,C,D,Q,FAC1,FAC2,FAC3,X,TRUNC

      REAL TAB1(256),TAB2(256),TAB3(256),TAB4(256),TAB5(256),TAB6(256)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
C                 
C     The following values were taken from COLOR SCIENCE by Wyszecki and
C     Stiles page 269.
C
      REAL BD/1.20063/,GD/1.13240/,RD/0.66697/
      REAL BX/0.20000/,GX/0.31000/,RX/0.49000/
      REAL BY/0.01063/,GY/0.81240/,RY/0.17697/
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              out3(I) = TRUNC(TAB1(IN3(I)+1)-FAC1*IN1(I)*IN3(I)+
     +                   FAC2*IN2(I)*IN3(I)+0.5)
              out2(I) = TRUNC(TAB2(IN3(I)+1)-FAC3*IN1(I)*IN3(I)-
     +                   TAB3(out3(I)+1)+0.5)
              out1(I) = TRUNC(TAB4(IN3(I)+1)-TAB5(out2(I)+1)-
     +                   TAB6(out3(I)+1)+0.5)
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_UCS(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
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
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              if (XNORM.EQ.0.0) THEN
                      out1(I) = 84
                      out2(I) = 126
                      out3(I) = 0
                  ELSE
                      SCALE = 255.0/XNORM
                      IX = SCALE*(TAB4(IN1(I)+1)+TAB5(IN2(I)+1)+
     +                                  TAB6(IN3(I)+1))+1.5
                      IY = SCALE*(TAB7(IN1(I)+1)+TAB8(IN2(I)+1)+
     +                                  TAB9(IN3(I)+1))+1.5
                      out1(I) = LUT1(IX,IY)
                      out2(I) = LUT2(IX,IY)
                      out3(I) = XNORM/3.0 + 0.5
              END IF
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE UCS_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
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
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              out3(I) = TRUNC(TAB1(IN3(I)+1)-FAC1*IX*IN3(I)+
     +                   FAC2*IY*IN3(I))
              out2(I) = TRUNC(TAB2(IN3(I)+1)-FAC3*IX*IN3(I)-
     +                   TAB3(out3(I)+1))
              out1(I) = TRUNC(TAB4(IN3(I)+1)-TAB5(out2(I)+1)-
     +                   TAB6(out3(I)+1))
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_HSI(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,LINE,ISL,IEL,ISS,ISTATUS
      real*4 BLUE,GREEN,RED
      REAL*4 X,Y,SCALE,SQRT6,SQRT2,SINESQ,xout1,satlimit
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3
C
C     The relevant equations are:
C          intensity = maximum(blue,green,red)
C
C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C                  R = sqrt(blue**2 + green**2 + red**2)
C
C                hue = arctan(y/x)     Note: This is a different definition
C                                            than in previous versions.
C                                            Magenta is now 0 DN, blue=43,
C                                            green=128, red=213.
C               sat = arcsin(d/R)/max possible arcsin(d/R) for this hue
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SCALE = maxval/360.0

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          BLUE = IN1(I)
          GREEN = IN2(I)
          RED = IN3(I)
          X = (BLUE+RED-2*GREEN)/SQRT6
          Y = (BLUE-RED)/SQRT2
          out3(i) = AMAX1(BLUE,GREEN,RED)
          out1(i) = SCALE*ATAN2(Y,X)*57.2957795
          if (out1(i).LT.0) out1(I)=out1(I)+maxval-1
          if (X.NE.0.0 .OR. Y.NE.0.0) THEN
            SINESQ = maxval*asin(sqrt((X*X+Y*Y)/
     +       (BLUE*BLUE+GREEN*GREEN+RED*RED)))*57.2957795
	    xout1 = 90.0-out1(i)/scale
	    do while (xout1.lt.30.0)
	      xout1 = xout1+120.0
	    enddo
            satlimit = ATAN(1.0/(SQRT2*SIN(xout1/57.2957795)))
     &       *57.2957795
            out2(i) = AMIN1(SINESQ/SATLIMIT,maxval-1.0)
          ELSE
            out2(i) = 0.0
          END IF
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE HSI_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,J,I,LINE,ISL,IEL,ISTATUS,ISS
      REAL SQRT6,SQRT2,SQRT32,X,Y,D,DD,RED,GREEN,BLUE,FACTOR,SCALE
      REAL root,hue,sat,xint,xdist,xhue,huefunc
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

C     The relevant equations are:
C              d = intensity*sin(saturation/satlimit(hue))
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(intensity**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y

      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/maxval

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          HUE = IN1(I)
          SAT = IN2(I)
          XINT = IN3(I)
	  xhue = 90.0-hue*scale
	  do while (xhue.lt.30.0)
	    xhue = xhue+120.0
	  enddo
          huefunc = ATAN(1.0/(SQRT2*SIN(xhue/57.2957795)))
     &     *57.2957795
          XDIST = SIN(HUEFUNC*sat/(57.2957795*(maxval-1.0)))
          D = XINT*xDIST
          DD = D*D
	  xhue = hue*scale
          if (abs(xhue-90.0).gt.0.000001.AND.
     &     abs(xhue-270.0).gt.0.000001) THEN 
            HUEFUNC = SQRT(1.0+(TAN(xhue/57.2957795)**2))
          ELSE
            HUEFUNC = 1.0E10
          END IF
          X = D/HUEFUNC
          root = dd-x*x
          if (root.lt.0.0) root = 0.0
          Y = SQRT(root)
          if (xHUE.LE.180.0) Y=-Y
          if (xHUE.LE.90.0.OR.xHUE.GT.270.0) X=-X
          root = xint*xint-dd
          if (root.lt.0.0) root = 0.0
          RED = SQRT32*Y-X/SQRT2+SQRT(root)
          GREEN = RED+3.0*X/SQRT2-SQRT32*Y
          BLUE = RED-SQRT6*Y
C                                            Since intensity is scaled to
C                                            fraction of max possible,
C                                            rescaling is needed.
c         FACTOR = xINT/AMAX1(BLUE,GREEN,RED,1.0)
c         above does not work well with real*4 data, replace with a smaller
c         number -- can't hurt for integers, since we just need to avoid 0:
          FACTOR = xINT/AMAX1(BLUE,GREEN,RED,0.000001)
          out1(I) = amin1(maxval,FACTOR*BLUE)
          out2(I) = amin1(maxval,FACTOR*GREEN)
          out3(I) = amin1(maxval,FACTOR*RED)
	  if (maxval.eq.256) then	! assume this is byte data
	    if (out1(i).lt.0) out1(i) = 0
	    if (out2(i).lt.0) out2(i) = 0
	    if (out3(i).lt.0) out3(i) = 0
	  endif
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_SPH(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,LINE,ISL,IEL,ISS,ISTATUS
      real*4 BLUE,GREEN,RED
      REAL*4 X,Y,SCALE,SQRT6,SQRT2,SINESQ,xout1,satlimit
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

C     The relevant equations are:

C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C           radiance = sqrt(blue**2 + green**2 + red**2)
C
C          longitude = hue = arctan(y/x)     Note: This is a different definition
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
      SCALE = maxval/360.0
      SATLIMIT = ATAN(SQRT2)*57.2957795/(maxval-1.0)

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          BLUE = IN1(I)
          GREEN = IN2(I)
          RED = IN3(I)
          out3(i) = sqrt((BLUE*blue+GREEN*green+RED*red)/3.0)
          X = (BLUE+RED-2*GREEN)/SQRT6
          Y = (BLUE-RED)/SQRT2
          out1(i) = SCALE*ATAN2(Y,X)*57.2957795
          if (out1(i).LT.0) out1(I)=out1(I)+maxval-1
          if (X.NE.0.0 .OR. Y.NE.0.0) THEN
            SINESQ = asin(sqrt((X*X+Y*Y)/(3.0*out3(i)*out3(i))))
     &       *57.2957795
            out2(i) = AMIN1(SINESQ/SATLIMIT,maxval-1.0)
          ELSE
            out2(i) = 0.0
          END IF
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE SPH_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,J,I,LINE,ISL,IEL,ISTATUS,ISS
      REAL SQRT6,SQRT2,SQRT32,X,Y,D,DD,RED,GREEN,BLUE,FACTOR,SCALE
      REAL root,hue,sat,xint,xdist,xhue,huefunc,satscale
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

C     The relevant equations are:
C              d = radiance*sin(saturation)
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(radiance**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y

      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/maxval
      satscale = atan(sqrt2)*57.2957795/(maxval-1.0)

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          HUE = IN1(I)+1
          SAT = IN2(I)+1
          XINT = IN3(I)		! actually radiance
          XDIST = SIN(sat*satscale/57.2957795)
          D = XINT*xDIST
          DD = D*D
	  xhue = hue*scale
          if (xhue.NE.90.0.AND.xhue.NE.270.0) THEN 
            HUEFUNC = SQRT(1.0+(TAN(xhue/57.2957795)**2))
          ELSE
            HUEFUNC = 1.0E10
          END IF
          X = D/HUEFUNC
          root = dd-x*x
          if (root.lt.0.0) root = 0.0
          Y = SQRT(root)
          if (xHUE.LE.180.0) Y=-Y
          if (xHUE.LE.90.0.OR.xHUE.GT.270.0) X=-X
          root = xint*xint-dd
          if (root.lt.0.0) root = 0.0
          out3(i) = SQRT32*Y-X/SQRT2+SQRT(root)
          out2(i) = out3(i)+3.0*X/SQRT2-SQRT32*Y
          out1(i) = out3(i)-SQRT6*Y
	  if (maxval.eq.256) then	! assume this is byte data
	    if (out1(i).lt.0) out1(i) = 0
	    if (out2(i).lt.0) out2(i) = 0
	    if (out3(i).lt.0) out3(i) = 0
	  endif
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_HSR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,LINE,ISL,IEL,ISS,ISTATUS
      real*4 BLUE,GREEN,RED
      REAL*4 X,Y,SCALE,SQRT6,SQRT2,SINESQ,xout1,satlimit
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      Common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

C     The relevant equations are:

C                  x = (blue+red-2*green)/sqrt(6)
C                  y = (blue-red)/sqrt(2)
C                  d = sqrt(x*x+y*y)
C           radiance = sqrt(blue**2 + green**2 + red**2)
C
C                hue = arctan(y/x)     Note: This is a different definition
C                                            than in previous versions.
C                                            Magenta is now 0 DN, blue=43,
C                                            green=128, red=213.
C               sat = arcsin(d/I)/max possible arcsin(d/I) for this hue
C
      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SCALE = maxval/360.0

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          BLUE = IN1(I)
          GREEN = IN2(I)
          RED = IN3(I)
          X = (BLUE+RED-2*GREEN)/SQRT6
          Y = (BLUE-RED)/SQRT2
          out3(i) = sqrt((BLUE*blue+GREEN*green+RED*red)/3.0)
          out1(i) = SCALE*ATAN2(Y,X)*57.2957795
          if (out1(i).LT.0) out1(I)=out1(I)+maxval-1
          if (X.NE.0.0 .OR. Y.NE.0.0) THEN
            SINESQ = maxval*asin(sqrt((X*X+Y*Y)/
     +       (BLUE*BLUE+GREEN*GREEN+RED*RED)))*57.2957795
	    xout1 = 90.0-out1(i)/scale
	    do while (xout1.lt.30.0)
	      xout1 = xout1+120.0
	    enddo
            satlimit = ATAN(1.0/(SQRT2*SIN(xout1/57.2957795)))
     &       *57.2957795
            out2(i) = AMIN1(SINESQ/SATLIMIT,maxval-1.0)
          ELSE
            out2(i) = 0.0
          END IF
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE HSR_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS,
     + maxval)
      IMPLICIT NONE
      real*4 maxval
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,J,I,LINE,ISL,IEL,ISTATUS,ISS
      REAL SQRT6,SQRT2,SQRT32,X,Y,D,DD,RED,GREEN,BLUE,FACTOR,SCALE
      REAL root,hue,sat,xint,xdist,xhue,huefunc
      real*4 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
      common /c1/ inunit1,inunit2,inunit3,outunit1,outunit2,outunit3

C     The relevant equations are:
C              d = radiance*sin(saturation/satlimit(hue))
C              x = d/sqrt(1+tan(hue)**2)
C              y = sqrt(d*d-x*x)
C                             Note: The sign of y is negative if hue>128
C                                   The sign of x is negative if 64<hue<192
C            red = -sqrt(2)*x+sqrt(6)*y+2*sqrt(radiance**2-d*d)
C           blue = red-sqrt(6)*y
C          green = red+3*x/sqrt(2)-sqrt(3/2)*y

      SQRT6 = SQRT(6.0)
      SQRT2 = SQRT(2.0)
      SQRT32 = SQRT(3.0/2.0)
      SCALE = 360.0/maxval

C     process images

      DO LINE=ISL,IEL
        CALL XVREAD(INUNIT1,IN1,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT2,IN2,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        CALL XVREAD(INUNIT3,IN3,ISTATUS,'LINE',LINE,'SAMP',ISS,
     +   'NSAMPS',NS,' ')
        DO I=1,NS
          HUE = IN1(I)+1
          SAT = IN2(I)+1
          XINT = IN3(I)		! actually radiance
	  xhue = 90.0-hue*scale
	  do while (xhue.lt.30.0)
	    xhue = xhue+120.0
	  enddo
          huefunc = ATAN(1.0/(SQRT2*SIN(xhue/57.2957795)))
     &     *57.2957795
          XDIST = SIN(HUEFUNC*sat/(57.2957795*(maxval-1.0)))
          D = XINT*xDIST
          DD = D*D
	  xhue = hue*scale
          if (xhue.NE.90.0.AND.xhue.NE.270.0) THEN 
            HUEFUNC = SQRT(1.0+(TAN(xhue/57.2957795)**2))
          ELSE
            HUEFUNC = 1.0E10
          END IF
          X = D/HUEFUNC
          root = dd-x*x
          if (root.lt.0.0) root = 0.0
          Y = SQRT(root)
          if (xHUE.LE.180.0) Y=-Y
          if (xHUE.LE.90.0.OR.xHUE.GT.270.0) X=-X
          root = xint*xint-dd
          if (root.lt.0.0) root = 0.0
          out3(i) = SQRT32*Y-X/SQRT2+SQRT(root)
          out2(i) = out3(i)+3.0*X/SQRT2-SQRT32*Y
          out1(i) = out3(i)-SQRT6*Y
	  if (maxval.eq.256) then	! assume this is byte data
	    if (out1(i).lt.0) out1(i) = 0
	    if (out2(i).lt.0) out2(i) = 0
	    if (out3(i).lt.0) out3(i) = 0
	  endif
        END DO
        CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
        CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE BGR_CUB(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,I,LINE,ISL,IEL,ISTATUS,ISS
      REAL C11,C12,C13,C21,C22,C23,C32,C33
      REAL B,G,R,BLUE,GREEN,RED,X,THIRD
C
      REAL CUBEROOT(12000)
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              RED = CUBEROOT(C11*R+C12*G+C13*B+1.5)
              GREEN = CUBEROOT(C21*R+C22*G+C23*B+1.5)
              BLUE = CUBEROOT(C32*G+C33*B+1.5)
C
              if (GREEN.GT.RED) THEN
                      out1(I) = 420.0*(RED-GREEN)+100.5
                  ELSE
                      out1(I) = AMIN1(500.0*(RED-GREEN)+100.5,255.0)
              END IF
C
              if (GREEN.GT.BLUE) THEN
                      out2(I) = 122.0*(GREEN-BLUE)+150.5
                  ELSE
                      out2(I) = 214.4*(GREEN-BLUE)+150.5
              END IF
C
              out3(I) = 101.16*GREEN+0.5
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C*************************************************************************
      SUBROUTINE CUB_BGR(IN1,IN2,IN3,out1,out2,out3,ISL,ISS,IEL,NS)
      IMPLICIT NONE
      INTEGER INUNIT1,INUNIT2,INUNIT3,OUTUNIT1,OUTUNIT2,OUTUNIT3
      INTEGER NS,LINE,ISL,IEL,ISTATUS,ISS,I
      REAL C11,C12,C13,C21,C22,C23,C32,C33
      REAL C12A,C13A,C23A,X,RED,GREEN,BLUE,TRUNC
C
      INTEGER*2 IN1(NS),IN2(NS),IN3(NS),out1(NS),out2(NS),out3(NS)
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
              if (IN1(I).LT.100) THEN
                      RED = (IN1(I)-100)/420.0+GREEN
                  ELSE
                      RED = (IN1(I)-100)/500.0+GREEN
              END IF
              if (IN2(I).GT.150) THEN
                      BLUE = GREEN-(IN2(I)-150)/122.0
                  ELSE
                      BLUE = GREEN-(IN2(I)-150)/214.4
              END IF
              BLUE = BLUE*BLUE*BLUE
              GREEN = GREEN*GREEN*GREEN
              RED = RED*RED*RED
              if (BLUE.LE.0.0) THEN
                      out1(I) = 0
                  ELSE
                      out1(I) = TRUNC(BLUE/(C33+C32*GREEN/BLUE))
              END IF
              out2(I) = TRUNC((C21*(RED-C13*out1(I))
     +                         -C11*(GREEN-C23*out1(I)))/
     +                         (C12*C21-C11*C22))
              out3(I) = TRUNC((RED-C12*out2(I)-C13*out1(I))/C11)
          END DO
          CALL XVWRIT(OUTUNIT1,out1,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT2,out2,ISTATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT3,out3,ISTATUS,'NSAMPS',NS,' ')
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
      INTEGER MODECODE,MODE,MODEOUT

      Integer m(2),icount,idef,n,m1,m2
      character*12  from,to
      CHARACTER*58 BUF

      COMMON /C2/ BUF

      from = '            '
      to   = '            '
      buf(1:20) = 'Transformation from '

      m(1)=0
      m(2)=0
C     Parameter processing
C     Parameter "FROM"
C     N is the length of the string. Default='BGR'
      n=3
      modecode=0

      Call XVPARM('FROM',from,icount,idef,12)

      If (idef .eq. 0) n = 8

      buf(30:31) = 'to'
      buf(21:21+n) = from

      if (from.eq.'TRISTIM ') modecode=1
      if (from.eq.'CIE     ') modecode=2
      if (from.eq.'UCS     ') modecode=3
      if (from.eq.'HSI     ') modecode=4
      if (from.eq.'SPHERIC ') modecode=5
      if (from.eq.'HSR     ') modecode=6
      if (from.eq.'CUBEROOT') modecode=7
      if (from.eq.'RGB     ') modecode=8
      m(1)=modecode
C
C     parameter "TO"
      n=3
      modecode=0
      call XVPARM('TO',to,icount,idef,12)
      If (idef .eq. 0) n = 8

      buf(33:33+n) = to

      if (to.eq.'TRISTIM ') modecode=1
      if (to.eq.'CIE     ') modecode=2
      if (to.eq.'UCS     ') modecode=3
      if (to.eq.'HSI     ') modecode=4
      if (to.eq.'SPHERIC ') modecode=5
      if (to.eq.'HSR     ') modecode=6
      if (to.eq.'CUBEROOT') modecode=7
      if (to.eq.'RGB     ') modecode=8
      m(2)=modecode
C
C    Check for error
      if (m(1).eq.0.and.m(2).eq.0) then
        call mabend('Either "FROM" or "TO" must be specified')
      elseif ((m(1).eq.0.and.m(2).eq.8).or.
     + (m(1).eq.8.and.m(2).eq.0)) then
	call mabend('No transformation between BGR and RGB!')
      elseif (m(1).ne.0.and.m(1).ne.8.and.
     + m(2).ne.0.and.m(2).ne.8) then
	call mabend('Either FROM or TO must be RGB/BGR')
      else
	m1 = m(1)
	if (m1.eq.8) m1 = 0
	m2 = m(2)
	if (m2.eq.8) m2 = 0
        mode= 2*(m1+m2)
        if (m1.eq.0) mode=mode-1
        modeout = m2+1
	if (m(1).eq.8) mode = 14+mode
	if (m(2).eq.8) modeout = 9
      end if
      return
      end 
C*********************************************************************** 
      INTEGER FUNCTION MODECODE(I)
      MODECODE = 0
      RETURN
      END
