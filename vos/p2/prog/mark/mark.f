      INCLUDE 'VICMAIN_FOR'
C VICAR program MARK
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      COMMON/C1/LOCS(2,5000),PIX(2048,32)
      REAL*4 LOCS
      INTEGER*2 PIX		!10 consecutive image lines
      INTEGER NBUF
      PARAMETER (NBUF=32)	!# of image lines in PIX (should be even)
      
      INTEGER NLI,NSI		!Input picture is NLI X NSI
      INTEGER NLO,NSO		!Output picture is NLO X NSO
      INTEGER SLI,SSI		!Starting line and sample of input image
      INTEGER ELI
      INTEGER MIDDN,MAXDN,DN,ISUM,MEAN
      INTEGER NLOCS,N,N1,N2,NB,I,K,JX,KX
      INTEGER OUNI,IMG,STAT,CNT
      INTEGER LINE,L,S
      INTEGER SL,SS,EL,ES	!Boundaries of rectangular area
      REAL*4 SKIP
      CHARACTER*4 FMT

      CALL IFMESSAGE('MARK version March 5, 2001')
      CALL GET_LOCS(PIX,locs,nlocs)

      CALL XVUNIT(IMG,'INP',1,STAT,' ')
      CALL XVOPEN(IMG,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     *		'U_FORMAT','HALF',' ')
      CALL XVGET(IMG,STAT,'FORMAT',FMT,' ')
      IF (FMT.EQ.'BYTE') THEN
         MAXDN = 255
      ELSEIF (FMT.EQ.'HALF') THEN
         MAXDN = 511
      ELSE
         CALL MABEND('***Invalid data format for input image')
      ENDIF
      CALL XVP('MAXDN',N,CNT)
      IF (CNT.EQ.1) MAXDN=N
      MIDDN = (MAXDN+1)/2

      CALL XVSIZE(SLI,SSI,NLO,NSO,NLI,NSI)
      NLO = MIN0(NLI-SLI+1,NLO)
      NSO = MIN0(NSI-SSI+1,NSO)
      ELI = SLI + NLO - 1

C     ...If (SLI,SSI) is not (1,1), redefine origin of coordinates
      IF (SLI.GT.1) THEN
         SKIP = FLOAT(SLI-1)
         DO I=1,NLOCS
            LOCS(1,I)=LOCS(1,I)-SKIP
         ENDDO
      ENDIF
      IF (SSI.GT.1) THEN
         SKIP = FLOAT(SSI-1)
         DO I=1,NLOCS
            LOCS(2,I)=LOCS(2,I)-SKIP
         ENDDO
      ENDIF

      CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     *		'OP','WRITE','U_NL',NLO,'U_NS',NSO,' ')

      CALL XVP('BOXSIZE',N,CNT)
      N1 = N/2
      N2 = (N+1)/2
      NB = NBUF/2

      KX = 0	              !Image buffer index

      DO 100 LINE=SLI,ELI+9
      KX = (MOD(KX,NBUF)) + 1
      IF (LINE.LE.NLO) THEN
         CALL XVREAD(IMG,PIX(1,KX),STAT,'LINE',LINE,'SAMP',SSI,
     *		'NSAMPS',NSO,' ')
         IF (LINE.EQ.SLI) THEN
            DO JX=2,10
               CALL MVE(2,NSO,PIX,PIX(1,JX),1,1)
            ENDDO
         ENDIF
      ENDIF

      DO 90 I=1,NLOCS
      L = LOCS(1,I) - .25
      IF (LINE-SLI+1.NE.L+NB) GOTO 90  
      IF (LOCS(1,I)-L.LT..75) THEN	!Here if entire rectangle is in memory
         SL = NB - N1		!Compute bounding coordinates of
         EL = NB + N1 + 1	!rectangle:  (SL,SS) to (EL,ES)
      ELSE
         SL = NB - N2 + 1
         EL = NB + N2 + 1
      ENDIF
      S = LOCS(2,I) - .25
      IF (LOCS(2,I)-S.LT..75) THEN
         SS = S - N1
         ES = S + N1 + 1
      ELSE
         SS = S - N2 + 1
         ES = S + N2 + 1
      ENDIF

      ISUM = 0			!Compute sum of DNs on perimeter of rectangle
      DO 50 L=SL,EL
      JX = MOD(KX+L-1,NBUF) + 1
      DO 50 S=SS,ES
      IF (L.NE.SL .AND. L.NE.EL .AND. S.NE.SS .AND. S.NE.ES) GOTO 50
      ISUM = ISUM + PIX(S,JX)
   50 CONTINUE

      K  = 2*(EL-SL+ES-SS)	!Compute # of pixels on perimeter
      MEAN = ISUM/K		!Average DN in area
      DN = 0
      IF (MEAN.LT.MIDDN) DN=MAXDN

      DO 60 L=SL,EL
      JX = MOD(KX+L-1,NBUF) + 1
      DO 60 S=SS,ES
      IF (L.NE.SL .AND. L.NE.EL .AND. S.NE.SS .AND.S.NE.ES) GOTO 60
      PIX(S,JX) = DN
   60 CONTINUE
   90 CONTINUE

      IF (LINE.LT.SLI+NBUF-1) GOTO 100
      JX = MOD(KX,NBUF) + 1
      CALL XVWRIT(OUNI,PIX(1,JX),STAT,' ')
  100 CONTINUE

      CALL XVCLOSE(OUNI,STAT,' ')
      CALL XVCLOSE(IMG,STAT,' ')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get the pixel locations from paramters or secondary input file.
C
      SUBROUTINE GET_LOCS(BUF,LOCS,NLOCS)
      IMPLICIT NONE
      REAL*4 BUF(2,5000)	!Temporary buffer
      REAL*4 LOCS(2,1)		!Output coordinates SL,s1,EL,s2,...
      INTEGER NLOCS		!number of coordinate pairs

      INTEGER*4 IUNIT,NI,NL,NS,NSAMPS,I,L,N,INC,ISTART,STAT
      LOGICAL XVPTST

      CALL XVP('DATA',LOCS,NS)
      NLOCS = NS/2
      IF (2*NLOCS.NE.NS) CALL MABEND('***Pixel locations not in pairs')

      CALL XVPCNT('INP',NI)	!Get number of input files
      IF (NI.NE.2) RETURN

C     ...If EVEN is specified, start with the second pixel location
      ISTART = 1
      IF (XVPTST('EVENONLY')) ISTART=2
C     ....If ODD or EVEN is specified, skip every other pixel location
      INC = 1
      IF (XVPTST('ODD_ONLY').OR. XVPTST('EVENONLY')) INC=2
  
      CALL XVUNIT(IUNIT,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,STAT,'NL',NL,'NS',NS,' ')

      NSAMPS = NS/2
      NLOCS = NL*NSAMPS/INC
      IF (NLOCS.GT.5000) CALL MABEND('***Pixel locations exceed 5000')
      N = 0

      DO L=0,NL-1
        CALL XVREAD(IUNIT,BUF(1,L*NSAMPS+1),STAT,'NSAMPS',NS,' ')
        DO I=ISTART,NSAMPS,INC
           N = N + 1
           LOCS(1,N) = BUF(1,I)
           LOCS(2,N) = BUF(2,I)
        ENDDO
      ENDDO
      CALL XVCLOSE(IUNIT,STAT,' ')
      RETURN
      END
