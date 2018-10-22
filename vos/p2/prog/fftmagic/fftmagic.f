      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ASM, OCT 1983
C**********************************************************************
C
      SUBROUTINE MAIN44
      EXTERNAL WORK
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &   BETA,NPSF,LOOP,NRESET
C        
      CALL IFMESSAGE('FFTMAGIC version 31-OCT-94')
C
      CALL PARAMP(INUNIT1,INUNIT2)
      CALL STACKA(6,WORK,2,4*NL2*NPIX,4*NL2*NPIX,INUNIT1,INUNIT2)
      RETURN
C100   CALL MABEND('NOT ENOUGH CORE FOR STACKA')
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE WORK(PIX,LPIX,REF,LREF,INUNIT1,INUNIT2)
      COMMON /MBUF/ MBUF
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER PSF
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      COMMON /PSF/ PSF(7,100)
      
      CHARACTER*132 MBUF
C      CALL MVE(1,38,
C     &    ' PIXELS CHANGED   SUM NEGATIVES   BETA',
C     &    MBUF(1),1,1) 
      MBUF(1:38)=' PIXELS CHANGED   SUM NEGATIVES   BETA'
C
      IF (LPIX.LT.4*NPIX*NL2 .OR. LREF.LT.4*NL2*NPIX) THEN
          CALL MABEND('NOT ENOUGH CORE FOR STACKA')
      END IF
C        
C       READ IN IMAGE ESTIMATE DSRN2
C        
      CALL LDIMAG(INUNIT1,PIX) 
      CALL RFT2(PIX,NX,NPIX,1,STATUS)   
C        
C       READ IN REFERENCE IMAGE(BLURRED)  DSRN3  
C        
      CALL LDIMAG(INUNIT2,REF) 
      IF (NPSF.GT.0) CALL FNDSTR(REF)
      CALL RFT2(REF,NX,NPIX,1,STATUS)
      CALL XVMESSAGE(MBUF,' ') 
C      DO KK=1,100
C          MBUF(KK:KK)=' '
C      END DO
      MBUF = ' '
C        
      LOOP=0 
      DO WHILE (LOOP.EQ.0 .OR. 
     &        (NRESET.NE.0 .AND. LOOP.LT.ITER)) 
C
          LOOP=LOOP+1 
C        
          IF (MODE.EQ.1) THEN 
              CALL MAMPL(PIX,REF)
          ELSE 
              CALL MPHASE(PIX,REF)
          END IF
C        
C       TAKE INVERSE FFT OF PIX  
C        
          CALL RFT2(PIX,NX,NPIX,-1,STATUS)
          CALL PICBOU(PIX)
          IF (NRESET.NE.0 .AND. LOOP.LT.ITER) THEN
              IF (NPSF.GT.0) CALL SETSTR(PIX)
C        
C	TAKE DIRECT FFT OF PIX
C        
              CALL RFT2(PIX,NX,NPIX,1,STATUS)
          END IF
C       
      END DO
C        
      CALL OUTLST(PIX) 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE PARAMP(INUNIT1,INUNIT2)
      INTEGER IPAR(600)
      LOGICAL XVPTST
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /PSF/ PSF(7,100)

      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVSIZE(ISL,ISS,NX,NPIX,NLI,NSI)
      NL2 = NX + 2

      CALL XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','REAL',' ')

      IF (XVPTST('PHASE')) THEN
          MODE=2
      ELSE 
          MODE=1
      END IF

      IF (XVPTST('PRINT')) THEN
          IPRINT=1
      ELSE 
          IPRINT=0
      END IF

      CALL XVP ('ITER',ITER,ICNT)
      CALL XVP ('BETA',BETA,ICNT)

      CALL XVP ('PSF',IPAR,ICNT)
      IF ( ICNT .NE. 0 ) THEN
          NPSF=IPAR(1)
          I=2
          DO N=1,NPSF 
              PSF(5,N) = IPAR(I)
              PSF(6,N) = IPAR(I+1)
              PSF(7,N) = IPAR(I+2)
              I=I+3
          END DO
      ELSE
          NPSF=0
      END IF
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE LDIMAG(INUNIT,BUF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL BUF(NPIX,NL2)

      L=0
      DO LINE=ISL,ISL+NX-1 
              L=L+1 
              CALL XVREAD(INUNIT,BUF(1,L),ISTATUS,'LINE',LINE, 
     +                    'SAMP',ISS,'NSAMPS',NPIX,' ')
      END DO
      CALL XVCLOSE(INUNIT,ISTATUS,' ') 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MAMPL(PIX,REF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      DO L=1,NL2,2 
          DO J=1,NPIX 
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (PA.GT.0.0) THEN
                  S=SQRT(RA/PA) 
                  PIX(J,L)=PIX(J,L)*S 
                  PIX(J,L+1)=PIX(J,L+1)*S 
              END IF
          END DO
      END DO
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MPHASE(PIX,REF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      DO L=1,NL2,2 
          DO J=1,NPIX
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (RA.GT.0.0) THEN
                  S=SQRT(PA/RA) 
                  PIX(J,L)=S*REF(J,L) 
                  PIX(J,L+1)=S*REF(J,L+1) 
              ELSE 
                  PIX(J,L)=0.0 
                  PIX(J,L+1)=0.0 
              END IF
          END DO
      END DO
C        
      PIX(1,1)=REF(1,1) 
      PIX(1,2)=REF(1,2) 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE PICBOU(PIX)
      CHARACTER*132 MBUF
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /MBUF/ MBUF
      REAL PIX(NPIX,NL2)
      NRESET=0
      SUMNEG=0.E0
C        
      IF (LOOP.EQ.(LOOP/2)*2) THEN
          SCALE_NEG = -BETA 
      ELSE 
          SCALE_NEG = 0.E0 
      END IF        
      S=NX*NPIX*2 
      DO L=1,NX 
          DO J=1,NPIX 
              PIX(J,L)=PIX(J,L)/S          
              IF (PIX(J,L).LT.0.0) THEN
                  SUMNEG=SUMNEG+PIX(J,L) 
                  PIX(J,L)=PIX(J,L)*SCALE_NEG 
                  NRESET=NRESET+1 
              END IF
          END DO
      END DO
      IF (IPRINT.GT.0) THEN
          WRITE (MBUF,9900) NRESET,SUMNEG,SCALE_NEG
9900  FORMAT ('     ',I5,'          ',F10.1,'  ',F6.2)
          CALL XVMESSAGE(MBUF(2:50),' ')
      END IF
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE OUTLST(PIX)
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      LOGICAL XVPTST
      REAL PIX(NPIX,NL2)
C        
C        
      CALL XVUNIT(IOUTUNIT,'OUT',1,ISTATUS,' ')
      IF (XVPTST('OUTREAL')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','REAL',' ')
      ELSE IF (XVPTST('OUTINTEG')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','FULL',' ')
      ELSE IF (XVPTST('OUTHALF')) THEN
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.32767) PIX(J,L)=32767
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','HALF',' ')
      ELSE
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.255) PIX(J,L)=255 
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','BYTE',' ')
      END IF      

      DO L=1,NX
           CALL XVWRIT(IOUTUNIT,PIX(1,L),ISTATUS,'NSAMPS',
     +                 NPIX,' ')
      END DO
C        
      CALL PRNT(4,1,LOOP,' ITERATION ENDED ON PASS .') 
      CALL XVCLOSE(IOUTUNIT,ISTATUS,' ')
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE FNDSTR(REF)
C        
      REAL REF(NPIX,NL2)
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER PSF
      COMMON /PSF/ PSF(7,100)
      INTEGER LINE,SAMP,LEFT,RIGHT,TOP,BOTTOM
      REAL MAXDN,SUMDN
C        
      DO N=1,NPSF  
C        
          LEFT=PSF(6,N)-PSF(7,N)   
          RIGHT=PSF(6,N)+PSF(7,N) 
          TOP=PSF(5,N)-PSF(7,N) 
          BOTTOM=PSF(5,N)+PSF(7,N) 
          IF (LEFT.LT.1) LEFT=1 
          IF (RIGHT.GT.NPIX) RIGHT=NPIX 
          IF (TOP.LT.1) TOP = 1 
          IF (BOTTOM.GT.NX) BOTTOM=NX 
          PSF(1,N)=TOP
          PSF(2,N)=LEFT
          PSF(3,N)=BOTTOM
          PSF(4,N)=RIGHT
C        
          LINE=TOP  
          SAMP=LEFT
          MAXDN=REF(SAMP,LINE) 
          DO L= TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  IF (REF(J,L).GT.MAXDN) THEN
                      MAXDN=REF(J,L) 
                      LINE=L  
                      SAMP=J
                  END IF
              END DO
          END DO
          PSF(5,N)=LINE
          PSF(6,N)=SAMP
C        
          MAXDN=0     
          DO J=LEFT,RIGHT 
              MAXDN=MAXDN+REF(J,TOP)+REF(J,BOTTOM)  
          END DO
          DO L=TOP,BOTTOM 
              MAXDN=MAXDN+REF(LEFT,L)+REF(RIGHT,L)  
          END DO
          MAXDN=MAXDN/(2*(BOTTOM-TOP+1+RIGHT-LEFT+1))   
C        
          SUMDN=0.E0      
          DO L=TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  SUMDN=SUMDN+REF(J,L)-MAXDN 
              END DO
          END DO
          PSF(7,N)=SUMDN 
          CALL PRNT(4,7,PSF(1,N),' PSF BUF=.') 
C        
      END DO
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE SETSTR(PIX)
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2)
      INTEGER PSF
      COMMON /PSF/ PSF(7,100)
      REAL MEANDN
C        
      DO N=1,NPSF
C        
          MEANDN=0.E0
          DO L=PSF(1,N),PSF(3,N) 
              MEANDN=MEANDN+PIX(PSF(2,N),L)+PIX(PSF(4,N),L)
          END DO
          DO J=PSF(2,N),PSF(4,N) 
              MEANDN=MEANDN+PIX(J,PSF(1,N))+PIX(J,PSF(3,N))
          END DO
          MEANDN=MEANDN/(2*(PSF(3,N)-PSF(1,N)+1+PSF(4,N)-PSF(2,N)+1)) 
C        
          DO L=PSF(1,N),PSF(3,N) 
              DO J=PSF(2,N),PSF(4,N) 
                  PIX(J,L)=MEANDN 
              END DO
          END DO
          PIX(PSF(6,N),PSF(5,N))=PSF(7,N)  
      END DO
      RETURN
      END
