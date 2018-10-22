      INCLUDE 'VICMAIN_FOR'
C*********************************************************************
C  PROGRAM FFTFLIP:  TRANSPOSE QUADRANTS OF BYTE/HALFWORD FFT 
C
C   JAN.'84  ...JJL...  ORIGINAL VERSION
C   MAY '84  ...ASM...  CONVERTED TO VAX
C   AUG.'85  ...LWK...  CONVERTED TO VICAR2, FIXED BUGS
C   SEP.'94  ...AMS...  CRI MSTP S/W CONVERSION (VICAR PORTING)
C
C**********************************************************************
C
      SUBROUTINE MAIN44
	implicit none
      EXTERNAL WORK
C 
C
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1,IST
      INTEGER*4 OUT1,icode       !BUF(90)
      real*4 A 
      LOGICAL*4 FLIP
        character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      character*8 format
C
C PARAMETERS...
C       NLW =FFT SIZE,DEFAULT =32
C       LINES =LINE AND SAMPLE WIDTH,BE INTERPOLATED
C       (PSUEDO APODIZE),DEFAULT =1
C       DC =NUMBER OF PIXELS SURROUNDING THE DC PIXEL THAT
C       ARE,BE EXCLUDED FROM 'LINES'INTERPOLATION.
C       DEFAULT =1 (DC PIXEL ONLY)
C       FLIP =TRANSPOSES OUTPUT FFT ALONG THE DIAGONAL AXIS
C       FOR USE WHEN INPUT FFT WAS CREATED BY FFT22.
C
C      LOGICAL*4 FMT,MODE
C 
      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     & NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1

      DATA FLIP/.FALSE./

      CALL IFMESSAGE('FFTFLIP version 2016-06-08')
      CALL XVEACTION('SA',' ')
C 
C DEFINE EXTERNAL SUBROUTINE CALLING SEQUENCES 
C 
      CALL XVUNIT( IN1, 'INP', 1, IST,' ')
      CALL XVOPEN( IN1, IST, 'U_FORMAT', 'REAL', ' ')
C
      CALL XVGET( IN1, IST, 'PIX_SIZE', KEY,'FORMAT',format,' ')

      call xvclose(in1,ist,' ')
        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
             call xvmessage('??E - Unknown data format for input image',' ')
             call abend
        endif

        call xvopen(in1,ist,'OPEN_ACT','SA','IO_ACT','SA',
     1          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')            !FMT(INCODE),' ')


        KEY = 2-KEY
C
      CALL PPARM	!PROCESS PARAMETERS
C
      CALL XVUNIT( OUT1, 'OUT', 1, IST,' ')
      CALL XVOPEN( OUT1, IST, 'OP', 'WRITE', 'U_FORMAT',fmt(4),
     &     'O_FORMAT',fmt(icode),'U_NL', NLO, 'U_NS', NSO,' ')
C 
      CALL STACKA(5,WORK,3,NLW*NLW*4,NLW/2*4,NLW*NPIX*4)
      RETURN
      END
C 
C
C
C**********************************************************************
C
      SUBROUTINE WORK(SAV2,LSAV2,SAVE,LSAVE,IN,LIN)
c
      implicit none
      INTEGER*4 BLOCK,FFT,OUT1,LSAV2,LSAVE,LIN
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1,IST
      integer*4 J,L,LINE,N10,N11,N3,N4,N9
      real*4 IN(NPIX,NLW),SAV2(NLW,NLW),SAVE(NLW/2)
      real*4 A,A1,OF,SL
      LOGICAL*4 FLIP

      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     +    NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1
c
      IF (LSAV2.LT.NLW*NLW*2 .OR. LSAVE.LT.NLW/2*2 .OR.
     +    LIN.LT.NLW*NPIX*2) THEN
          CALL XVMESSAGE('??E - STACKA ERROR',' ')
          CALL ABEND
      END IF
C
C FFT ROW LOOP
C 
      DO BLOCK=1,NROWS
C
C READ IN A ROW OF FFT'S
C
          DO LINE=1,NLW
              CALL XVREAD( IN1, IN(1,LINE), IST, 'NSAMPS', NSO,' ')
          END DO
C 
C FFT COLUMN LOOP
C
          DO FFT=1,NCOLS
              N3 =FFT*NLW-N2+1
              N4 =(FFT-1)*NLW+1
              N9 =N4-1
              N10 =N9+N5
              N11 =N9+N6
C
C FLIP FFT QUADRANTS
C
              DO J =1,N2
                  CALL MVE(7,N2,IN(N3,N2+J),SAVE,1,1)
                  CALL MVE(7,N2,IN(N4,J),IN(N3,N2+J),1,1)
                  CALL MVE(7,N2,SAVE,IN(N4,J),1,1)
C 
                  CALL MVE(7,N2,IN(N4,N2+J),SAVE,1,1)
                  CALL MVE(7,N2,IN(N3,J),IN(N4,N2+J),1,1)
                  CALL MVE(7,N2,SAVE,IN(N3,J),1,1)
              END DO
              IF (FLIP) THEN
                  DO J=1,NLW
                      DO L =1,NLW
                          SAV2(L,J) =IN(L+N9,J)
                      END DO
                  END DO
                  DO J =1,NLW
                      DO L =1,NLW
                          IN(J+N9,L) =SAV2(L,J)
                      END DO
                  END DO
              END IF
C 
C HORIZONTAL INTERPOLATION
C
              IF (LINES.GT.0) THEN
                  DO L=1,NLW
                      IF (L.LE.N7 .OR. L.GE.N8) THEN
                          A1=IN(N10,L)
                          SL =(IN(N11,L) -A1)/A
                          OF =A1 -SL*N10 +0.5
                          DO J =N10+1,N11-1
                              IN(J,L) =SL*J+OF
                          END DO
                      END IF
                  END DO
C
C VERTICAL INTERPOLATION
C
                  DO J =N4,N9+NLW
                      IF (J.LE.N9+N7 .OR. J.GE.N9+N8) THEN
                          A1 =IN(J,N5)
                          SL =(IN(J,N6)-A1)/A
                          OF =A1 -SL*N5+0.5
                          DO L =N12,N13
                              IN(J,L) =SL*L+OF
                          END DO
                      END IF
                  END DO
              END IF
          END DO
          DO LINE=1,NLW
C
C WRITE OUT ROW OF FFT'S
C
              CALL XVWRIT( OUT1, IN(1,LINE), IST, 'NSAMPS', NSO,' ')
          END DO
      END DO
      RETURN
      END
C 
C
C
C**********************************************************************
C
      SUBROUTINE PPARM
C 
C      INTEGER IPAR(100)
      implicit none
      INTEGER*4 OUT1
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1,IST
      integer*4 IC,ID
      real*4 A
      LOGICAL*4 FLIP,XVPTST
C      REAL RPAR(100)
C      LOGICAL*4 KPAR(100)
C      EQUIVALENCE (IPAR,RPAR)
C      EQUIVALENCE (IPAR,KPAR)
      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     +    NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1
      
C
      CALL XVSIZE( SLIN, SSAM, NLO, NSO, NX, NS)
C
      CALL XVPARM( 'NLW', NLW, IC, ID, 1)
      IF (ID.EQ.1) NLW = MIN0(NLO,NSO)
C
      CALL XVPARM( 'LINE', LINES, IC, ID, 1)
C
      CALL XVPARM( 'DC', NLDC, IC, ID, 1)
C
      FLIP = XVPTST( 'FLIP')
C
      NROWS =NLO/NLW
      NPIX=NSO
      NCOLS=NPIX/NLW
      N2=NLW/2
      N5 =N2-LINES/2
      N6 =N2+(LINES+1)/2+1
      N7 =N2-NLDC/2
      N8 =N2+(NLDC+1)/2+1
      A =LINES+1
      N12=N5+1
      N13=N6-1
C 
      RETURN
      END
