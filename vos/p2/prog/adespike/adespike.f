      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM ADESPIKE -- Detect and remove single-pixel errors.fmt
C          ADESPIKE  INP  OUT  user-parameters...
C    REVISION HISTORY
C       7-95    CRI    fixed xvtrans_inb incorrect variable as per FR87387
C	6-94    CRI    MSTP S/W CONVERSION (VICAR PORTING)
c  10-Jan-2013 -lwk- changed write statements for VTOTAL/HTOTAL due to
c                    new compiler flag on Solaris
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      CHARACTER*8  LMT
      INTEGER*4 INSO,NXX,NYY,NSO,NLO,NBB
      INTEGER I,ISTAT

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      LOGICAL STDIO,BINARY
      EXTERNAL WORKA	!  Called by STACKA
      EXTERNAL WORKB	!  Called by STACKA

      CHARACTER*132 VTOTAL
      CHARACTER*132 HTOTAL

      HTOTAL = ' '
      I   = 0
      ISTAT = 0
      LMT = ' '
      NLI = 0
      NSI = 0
      VTOTAL = ' '


      CALL IFMESSAGE ('ADESPIKE version 21-JUL-95')
      CALL XVEACTION ('SA',' ')


c
c   First, Open assuming NON-BINARY
c
      CALL XVUNIT(IUN,'INP',1,I,' ')
      CALL XVOPEN(IUN,I, 'U_FORMAT', 'HALF',' ')
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      CALL XVGET(IUN,I,'NLB',NLB,'NBB',NBB,'FORMAT',LMT,' ')
c
c   This portion will be repeated in Subroutine APARAM
c
      IF (LMT.EQ.'BYTE') DCODE = 1  
      IF (LMT.EQ.'HALF') DCODE = 2
      IF ((LMT.NE.'HALF') .AND. (LMT.NE.'BYTE'))  Then
         CALL XVMESSAGE(
     &        '***Illegal format, only byte/half allowed.',' ')
         GOto 999
      ENDIF

      CALL APARAM(*999)

C	Check for binary header and image size .. Max is 800x800
c
      BINARY = .FALSE.

      IF (NLB .NE. 0 .OR. NBB .NE. 0) THEN

        BINARY = .TRUE.
        IF (SLO .NE. 1 .OR. SSO .NE. 1) THEN
            CALL XVMESSAGE 
     &         ('Windowing binary labelled image not allowed',' ')
            GOTO 999
        ENDIF
        IF (NLO .GT. 800 .OR. NSO .GT. 800) THEN
            CALL XVMESSAGE
     &         ('Image too large to process binary label',' ')
            GOTO 999
        ENDIF
        CALL XVCLOSE(IUN,ISTAT,' ')
 
c
c    Open BINARY  INPUT  after CLOSE !     
c
         CALL XVUNIT(IUN,'INP',1,I,' ')

         IF (LMT .EQ. 'BYTE') THEN
         CALL XVOPEN(IUN,I,'COND','BINARY','U_FORMAT','BYTE',' ')
         ENDIF

         IF (LMT .EQ. 'HALF') THEN
         CALL XVOPEN(IUN,I,'COND','BINARY','U_FORMAT','HALF',' ')
         ENDIF

      ENDIF                       !  If (BINARY)
c
c    Open OUTPUT now !
c

      CALL XVUNIT(OUN,'OUT',1,I,' ')

      IF (BINARY) THEN
        IF (LMT .EQ. 'BYTE') THEN
	CALL XVOPEN(OUN,I,'O_FORMAT',
     &	    'BYTE','OP','WRITE','U_NS',NSO,'U_NLB',NLB,'U_NBB',NBB,
     &      'U_FORMAT','BYTE',' ')
        ENDIF
        IF (LMT .EQ. 'HALF') THEN
	CALL XVOPEN(OUN,I,'O_FORMAT',
     &	    'HALF','OP','WRITE','U_NS',NSO,'U_NLB',NLB,'U_NBB',NBB,
     &      'U_FORMAT','HALF',' ')
        ENDIF

      ELSE
	CALL XVOPEN(OUN,I,
     &	 'OP','WRITE','U_NS',NSO,'U_FORMAT','HALF',' ')
      ENDIF

      CALL AUTSET(NSO,ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,
     &		DCODE,IBITS,IDN)

      IF (BINARY) THEN
        NXX = (NLO+2) * (2*NSO + NBB)    ! (800+2) * (2*800 + 200) 
        NYY = NSO + NBB                  ! 800 + 200 
        CALL STACKA(7,WORKB,2,NXX,NYY,NSO,NLO,NBB)
      ELSE
        INSO = 8*NSO
        CALL STACKA(3,WORKA,1,INSO)
      ENDIF

      WRITE (VTOTAL,9900) TOT
9900  FORMAT (
     +'     TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST       ',I6)
      CALL XVMESSAGE(VTOTAL(2:80),' ')
      WRITE (HTOTAL,9910) HTOT
9910  FORMAT (
     +'     TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST          ',I6)
      CALL XVMESSAGE(HTOTAL(2:80),' ')
c      WRITE (LAB,9920) TOT,HTOT
c9920  FORMAT (
c     +'     ADJACENT LINE PIXELS CHANGED ',I6,'. SAME LINE PIXELS
c     +CHANGED ',I5)
C     CALL XVMESSAGE(LAB(2:80),' ')


CCCC	CALL XLADD(OUN,'HISTORY','PGM_LAB',LAB,ISTAT,
CCCC     &	 'FORMAT','STRING','ULEN',68)
      CALL XLADD(OUN,'HISTORY','ADJ_LINE',TOT,ISTAT,
     &	 'FORMAT','INT',' ')
      CALL XLADD(OUN,'HISTORY','SAM_LINE',HTOT,ISTAT,
     &	 'FORMAT','INT',' ')
      RETURN
CCCCCCCCC
C          ERROR CONDITIONS
  999 CALL XVMESSAGE('***ADESPIKE task cancelled',' ')
      CALL ABEND
      END


C Process the image.
C
      SUBROUTINE WORKA(BUF,NBYT)
      IMPLICIT INTEGER (A-Z)
      INTEGER ISTAT
      INTEGER*4 NBYT
      BYTE BUF(NBYT/4,4)

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      LOGICAL STDIO,BINARY

      ISTAT = 0
      ELO = SLO + NLO - 1
      I1 = 1		!Index to upper adjacent line
      I2 = 2		!Index to current line
      I3 = 3		!Index to lower adjacent line
      CALL XVREAD(IUN,BUF(1,I2),ISTAT,'SAMP',SSO,'NSAMPS',
     &	   NSO,'LINE',SLO,' ')	!Read in first line

      DO 100 L=SLO,ELO

      IF (L.LT.ELO) THEN	!Read next image line
         IF (STDIO) THEN
            CALL XVREAD(IUN,BUF(1,I3),ISTAT,' ')
         ELSE
            CALL XVREAD(IUN,BUF(1,I3),ISTAT,'SAMP',SSO,
     &           'NSAMPS',NSO,' ')
         ENDIF
      ENDIF

      IF (IDN.GE.0) THEN	!If replacement DN is specified,
         I0 = 4			!we need a separate output buffer.
         CALL MVE(2,NSO,BUF(1,I2),BUF(1,I0),1,1)
      ELSE			!Otherwise,set output buffer
         I0 = I2		!equal to input buffer.
      ENDIF

      IF (L.EQ.SLO.OR.L.EQ.ELO) THEN
         CALL HUTSAR(BUF(1,I2),BUF(1,I0),HTOT,1,NSO)
      ELSE
         CALL AUTSAR(BUF(1,I1),BUF(1,I2),BUF(1,I3),BUF(1,I0),HTOT,TOT,
     &               1,NSO)
      ENDIF

      CALL XVWRIT(OUN,BUF(1,I0),ISTAT,' ')
      I1 = I2
      I2 = I3
  100 I3 = MOD(I3,3) + 1

      RETURN
      END

c
C Process the binary-header image.
C
      SUBROUTINE WORKB(BUF,BUF_SIZE,BBUF,BBSIZE,NSAMPS,NLINES,NPIX)
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 BUF_SIZE,BBSIZE,NSAMPS,NLINES,NPIX
c
      BYTE  BUF(2*NSAMPS+NPIX,NLINES+2)   ! for HALF 
      BYTE  BBUF(NSAMPS+NPIX)             ! for BYTE

      COMMON/CC/LINE,REC_CNT,BDV_REC
      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI
      
      LOGICAL STDIO,BINARY

      INTEGER INBUF(12)
      INTEGER LASTPIX(800),ISTAT,II
      INTEGER*2 HREC_H(1800),HREC_B(1000)      ! For HALF and BYTE 
      INTEGER*2 HREC_HOUT(1800),HREC_BOUT(1000)
      INTEGER*2 BDV_REC(1000)   ! 2000 BYTES, more than 1800+ or 1000+ !

      II = 0
      ISTAT = 0
      DO 2 II = 1,1800
        HREC_H(II) = 0
2     CONTINUE
      DO 4 II = 1,1000
        HREC_B(II) = 0
4     CONTINUE

C        Zero array with the 4-byte count
      CALL ZIA(BDV_REC,500)  ! Zero out Bad data value buffer.

      REC_CNT = NLB	!BDV record counter
      BDV_REC(1) = 6	!Bad data type (single pixel spikes)
      BDV_REC(2) = 1	!Object type (single pixel)
      BDV_REC(3) = 0	!Counter
      ELO = NLO
      I0 = 0
      I1 = 2		!Index to upper adjacent line
      I2 = 3		!Index to current line
      I3 = 4		!Index to lower adjacent line

      DO LINE=1, 800
         LASTPIX(LINE) = NSO
      ENDDO

      DO BHDR=1, NLB
       IF (DCODE .EQ. 2)   then               ! Half
         CALL XVREAD(IUN,HREC_H(1),ISTAT,' ') !Read in binary labels
         CALL XVWRIT(OUN,HREC_H(1),ISTAT,' ') ! 1800+ Bytes/line
         CALL XVTRANS_INB(INBUF,'HALF','HALF',IUN,ISTAT)
         IF (ISTAT .NE. 1) THEN
            CALL XVMESSAGE(' INBUF SETUP UNSUCCESSFUL',' ')
            CALL ABEND
         ENDIF
         CALL XVTRANS(INBUF,HREC_H,HREC_HOUT,1800)
         IF (HREC_HOUT(1) .EQ. 3) THEN	      ! Data-Drop-Out record
            DO BDVCNT=1, HREC_HOUT(3)
               LASTPIX(HREC_HOUT(BDVCNT*3+1)) = 
     &                 HREC_HOUT(BDVCNT*3+2) - 1
            ENDDO
         ENDIF
       ENDIF

       IF (DCODE .EQ. 1)   then               ! BYTE Format
         CALL XVREAD(IUN,HREC_B(1),ISTAT,' ') !Read in binary labels
         CALL XVWRIT(OUN,HREC_B(1),ISTAT,' ') ! 1000+ Bytes/line
         CALL XVTRANS_INB(INBUF,'HALF','HALF',IUN,ISTAT)
         IF (ISTAT .NE. 1) THEN
            CALL XVMESSAGE(' INBUF SETUP UNSUCCESSFUL',' ')
            CALL ABEND
         ENDIF
         CALL XVTRANS(INBUF,HREC_B,HREC_BOUT,1000)
         IF (HREC_BOUT(1) .EQ. 3) THEN	      ! Data-Drop-Out record
            DO BDVCNT=1, HREC_BOUT(3)
               LASTPIX(HREC_BOUT(BDVCNT*3+1)) =
     &                 HREC_BOUT(BDVCNT*3+2) - 1
            ENDDO
         ENDIF
       ENDIF

      ENDDO              !  End of BINARY-HEADER COPYing

      IDX = NBB+1
      RECLTH = 2 * NSO + NBB    ! 1800+ 
      IF (DCODE.EQ.2) THEN                !  HALF
	CALL XVREAD(IUN,BUF(1,I2),ISTAT,' ')  !  READing in 1800+ 
      ELSE
	CALL XVREAD(IUN,BBUF,ISTAT,' ')       ! BYTE, Read in 1000+
c             MOVE first 200, BYTE to BYTE
        Call MVE(1,NBB,BBUF(1),BUF(1,I2),1,1)   
C             Move NEXT 800, BYTE TO HALF 
	CALL MVE(3,NSO,BBUF(IDX),BUF(IDX,I2),1,1) 
      ENDIF

      DO 100 LINE=3,ELO
	IF (LINE.LT.ELO) THEN
	  IF (DCODE.EQ.2) THEN
	    CALL XVREAD(IUN,BUF(1,I3),ISTAT,' ')
	  ELSE
	   CALL XVREAD(IUN,BBUF,ISTAT,' ')
C             Move first 200, BYTE to BYTE 
           Call MVE(1,NBB,BBUF(1),BUF(1,I3),1,1)   
C             Move NEXT 800, BYTE TO HALF
	   CALL MVE(3,NSO,BBUF(IDX),BUF(IDX,I3),1,1) 
	  ENDIF
	ENDIF

        IF (IDN.GE.0) THEN	!If replacement DN is specified,
           I0 = I0 + 1
           CALL MVE(1,RECLTH,BUF(1,I2),BUF(1,I0),1,1)
        ELSE			!Otherwise,set output buffer
           I0 = I2		!equal to input buffer.
        ENDIF

c
CCC	SET LINE LENGTH BASED ON LAST VALID PIX FROM EDR LINE HEADER
c
         IF (LINE .EQ. 3 .OR. LINE .EQ. ELO) THEN

            CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,1,LASTPIX(I2))
         ELSE
            IF (LASTPIX(I2) .LE. LASTPIX(I1) .AND.
     &          LASTPIX(I2) .LE. LASTPIX(I3)) THEN
               CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &             BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I2))
            ELSE
               IF (LASTPIX(I1) .LE. LASTPIX(I3)) THEN
                  CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &             BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I1))
                  CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,
     &                 LASTPIX(I1)+1,LASTPIX(I2))
               ELSE
                  CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &           BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I3))
                  CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,
     &                 LASTPIX(I3)+1,LASTPIX(I2))
               ENDIF
            ENDIF
         ENDIF

         I1 = I2
         I2 = I3
         I3 = I3 + 1
100   CONTINUE

      CALL DUMP_BDV(0)
      I0 = 2
      IF (IDN .GE. 0) I0 = 0
      DO I2=I0+1, ELO+I0
	IF (DCODE.EQ.2) THEN
	  CALL XVWRIT(OUN,BUF(1,I2),ISTAT,' ')
	ELSE                                    
C            Move first 200 BYTEs
          Call MVE(1,NBB,BUF(1,I2),BBUF(1),1,1)      
C            Move the REst 800 Bytes
	  CALL MVE(-3,NSO,BUF(IDX,I2),BBUF(IDX),1,1) 
	  CALL XVWRIT(OUN,BBUF,ISTAT,' ')           
	ENDIF
      ENDDO
c
C	UPDATE NLB WITH XLDEL & XLADD
c
      CALL XLDEL(OUN,'SYSTEM','NLB',ISTAT,' ')
      CALL XLADD(OUN,'SYSTEM','NLB',REC_CNT,ISTAT,'FORMAT',
     &     'INT',' ')


      RETURN
      END
C
C
C
      SUBROUTINE DUMP_BDV(SAMP)
      IMPLICIT INTEGER(A-Z)

      COMMON/CC/LINE,REC_CNT,BDV_REC
      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      INTEGER outbuf(12)
      INTEGER ISTAT
      INTEGER*2 BDV_REC(1000),TRANS_OUT(1000)
      LOGICAL STDIO,BINARY

      call xvtrans_out(outbuf,'half','half','low','vax',istat)
      IF (ISTAT .NE. 1) THEN
         CALL XVMESSAGE(' OUTBUF SETUP UNSUCCESSFUL',' ')
         CALL ABEND
      ENDIF
      ISTAT = 0
      IF (.NOT. BINARY) RETURN

      IF (SAMP .LE. 0) THEN
         IF (BDV_REC(3) .GT. 0) THEN
            call xvtrans(outbuf,bdv_rec,trans_out,1000)
            CALL XVWRIT(OUN,TRANS_OUT,ISTAT,' ')
            REC_CNT = REC_CNT + 1
         ENDIF
      ELSE
         IF (BDV_REC(3) .GE. ((NBB+DCODE*NSO)-6)/4) THEN
            call xvtrans(outbuf,bdv_rec,trans_out,1000)
            CALL XVWRIT(OUN,TRANS_OUT,ISTAT,' ')
            REC_CNT = REC_CNT + 1
C            BDV_REC(3) = 0
            CALL ZIA(BDV_REC(3),499)
         ENDIF
         BDV_REC(BDV_REC(3)*2+4) = LINE
         BDV_REC(BDV_REC(3)*2+5) = SAMP
         BDV_REC(3) = BDV_REC(3) + 1
      ENDIF

      RETURN
      END
C Process ADESPIKE user-parameters...
C All values in common areas CA and CB are set.
C
      SUBROUTINE APARAM(*)
      IMPLICIT INTEGER (A-Z)
      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      LOGICAL STDIO,BINARY,PRINT,XVPTST
      CHARACTER*132 TOL1
      CHARACTER*132 TOL2
      CHARACTER*8 FMT
      INTEGER I,ICNT,IDEF,J,N

      INTEGER POWERS(17)

      DATA POWERS/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +		8192,16384,32768,65536/

      I = 0
      ICNT = 0
      IDEF = 0
      J = 0
      FMT  = ' '
      HTOT = 0		!Total number of horizontal spikes
      N = 0
      TOL1 = ' '
      TOL2 = ' '
      TOT = 0		!Total number of vertical spikes

      IF (SLO.NE.1.OR.SSO.NE.1.OR.NSO.NE.NSI) THEN
         STDIO = .FALSE.
      ELSE
         STDIO = .TRUE.
      ENDIF

      CALL XVGET(IUN,I,'FORMAT',FMT,' ')
      IF (FMT.EQ.'BYTE') THEN
         DCODE = 1
      ELSE IF (FMT.EQ.'HALF') THEN
         DCODE = 2
      ELSE
         CALL XVMESSAGE
     &        ('***Illegal format, only byte/half allowed.',' ')
         RETURN1
      ENDIF

      CALL XVPARM('TOL',N,ICNT,IDEF,1)

      IF (IDEF.EQ.0) THEN
          CALL MVE(4,8,N,ISUTO,0,1)
      ELSE
         CALL XVPARM('SUTOL',ISUTO,I,J,1)
         CALL XVPARM('SLTOL',ISLTO,I,J,1)
         CALL XVPARM('CUTOL',ICUTO,I,J,1)
         CALL XVPARM('CLTOL',ICLTO,I,J,1)
         CALL XVPARM('DSUTOL',IDSUT,I,J,1)
         CALL XVPARM('DSLTOL',IDSLT,I,J,1)
         CALL XVPARM('DCUTOL',IDCUT,I,J,1)
         CALL XVPARM('DCLTOL',IDCLT,I,J,1)
      END IF

      PRINT = XVPTST('PRINT')
      if (PRINT) then
        WRITE (TOL1,9920) ISLTO,ISUTO,IDSLT,IDSUT
9920  FORMAT ('     SLTOL=',I6,'    SUTOL=',I6,'    DSLTOL=',I6,
     +'    DSUTOL=',I6)
        CALL XVMESSAGE(TOL1(2:67),' ')
        WRITE (TOL2,9930) ICLTO,ICUTO,IDCLT,IDCUT
9930  FORMAT ('     CLTOL=',I6,'    CUTOL=',I6,'    DCLTOL=',I6,
     +'    DCUTOL=',I6)
        CALL XVMESSAGE(TOL2(2:67),' ')
        CALL XVMESSAGE(' ',' ')
      end if

      CALL XVPARM('DN',IDN,I,J,1)

      CALL XVPARM('BITS',IBITS,I,J,1)
      IF (IBITS.EQ.0) THEN
         IAVG = (ISUTO+ISLTO+ICUTO+ICLTO+IDSUT+IDSLT+IDCUT+IDCLT)/8
         DO I=1,16
            IF (IAVG.GE.POWERS(I).AND.IAVG.LT.POWERS(I+1)) THEN
               IF (DCODE.EQ.1) IBITS=8-I
               IF (DCODE.EQ.2) IBITS=16-I
               GOTO 6
            ENDIF
         ENDDO
         RETURN1
      ELSEIF (IBITS.LT.0) THEN
         IBITS=0
      ENDIF

    6 IF (DCODE.EQ.1) THEN
         IF (IBITS.EQ.8) THEN
            IBITS = 0
         ELSEIF (IBITS.GT.8) THEN
            CALL XVMESSAGE
     &         ('***Invalid BITS for BYTE data ignored',' ')
            IBITS = 0
         ELSEIF (IBITS.NE.0) THEN
            IBITS = IBITS+8		!for conversion to halfword
         ENDIF
      ENDIF
	
      RETURN
      END
C Original FORTRAN version is saved here for conversion to other machines.
C Search and delete spikes on a line.
C 83-4-12  ...LWK...  Modified ucl routine: create 3 entry points:
C 94-6-2   ...CRI(MAC)... replaced assembly language for UNIX portability
C
C   AUTSET:  Set constants for AUTSAR and HUTSAR
C   AUTSAR:  Perform horizontal and vertical tests to detect and delete
C            spikes.
C   HUTSAR:  Perform horizontal test only.
C
      SUBROUTINE AUTSET (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12)
      IMPLICIT INTEGER(A - Z)
      INTEGER*2 BUF1(*),BUF2(*),BUF3(*),OBUF(*),HMASK,LMASK
      SAVE NSO,ESU,ESL,ECU,ECL,DSU,DSL,DCU,DCL,DCODE,IBITS,IDN
      SAVE HMASK,LMASK


      NSO = P1
      ESU = P2
      ESL = P3
      ECU = P4
      ECL = P5
      DSU = P6
      DSL = P7
      DCU = P8
      DCL = P9
      DCODE = P10
      IBITS = P11
      IDN = P12
      IF (IBITS .GT. 0) THEN
         LMASK = 2**(16-IBITS)-1
         HMASK = NOT(LMASK)
      ENDIF
      RETURN
C AUTO-SAR (Perform hor.+vert. despiking)
C Inputs: 3 consecutive input lines BUF1,BUF2,and BUF3.
C Outpts: OBUF = despiked version of BUF2.
C         HTOT = running count of horizontal spikes.
C         TOT  = running count of vertical spikes.
C
      ENTRY AUTSAR(BUF1,BUF2,BUF3,obuf,
     & htot,tot,ssamp,nsamp)
      I = ssamp

C-----------------------------------------------------------------C
C   Added the following statement (6/4/94) so that the FORTRAN    C
C      code yields same result as the assembly code that it is    C
C      replacing.                                                 C
C-----------------------------------------------------------------C
      nso = nsamp

      X0 = BUF2(I)
      X2 = BUF2(1+I)
      GOTO 30		!Skip horizontal test at left margin.
C X0 is the current pixel.  It is compared against its four neighbors:
C           X3                      BUF1(I)
C        X1 X0 X2        BUF2(I-1)  BUF2(I)  BUF2(I+1)
C           X4                      BUF3(I)
C     ....Horizontal bit-error test.
   10 X2 = BUF2(I+1)
      AVG = (X1+X2)/2
      IF (X0.LT.AVG-ESL) GOTO 25
C     ....Here if AVG-ESL .LE. X0
      IF (X0.LE.AVG+ESU) GOTO 30
      IF (X0.LE.X1+DSU) GOTO 30
      IF (X0.LE.X2+DSU) GOTO 30
      HTOT = HTOT + 1			!Horizontal bit error.
      GOTO 40

   25 IF (X0.GE.X1-DSL) GOTO 30
      IF (X0.GE.X2-DSL) GOTO 30
      HTOT = HTOT + 1			!Horizontal bit error.
      GOTO 40
C
C     ....Vertical bit-error test.
   30 X3 = BUF1(I)
      X4 = BUF3(I)
      AVG = (X3+X4)/2

      IF (X0.LT.AVG-ECL) GOTO 35
C     ....Here if AVG-ECL .LE. X0.
      IF (X0.LE.AVG+ECU) GOTO 50
      IF (X0.LE.X3+DCU) GOTO 50
      IF (X0.LE.X4+DCU) GOTO 50
      TOT = TOT + 1			!Vertical bit error
      GOTO 40
C     ....Here if X0.LT.AVG-ECL
   35 IF (X0.GE.X3-DCL) GOTO 50
      IF (X0.GE.X4-DCL) GOTO 50
      TOT = TOT + 1			!Vertical bit error.
C     ....Replace pixel.
   40 IF (IBITS.GT.0) THEN	!If BITS is specified,replace
         X0 = IOR(IAND(LMASK,X0),IAND(HMASK,AVG)) !high-order bits.
      ELSE
         X0 = AVG		!Otherwise,replace all bits
      ENDIF
      IF (IDN.GT.0) THEN	!If user has specified replacement
         OBUF(I) = IDN		!pixel,then use it.
      ELSE			!Otherwise,
         OBUF(I) = X0		!replace pixel with average.
      ENDIF

C-----------------------------------------------------------------C
C   Added the following statement (6/4/94) so that the FORTRAN    C
C      code yields same result as the assembly code that it is    C
C      replacing.                                                 C
C-----------------------------------------------------------------C
      call dump_bdv(i)

   50 X1 = X0			!Update to next pixel
      X0 = X2
      I = I + 1
      IF (I.LT.NSO) GOTO 10
      IF (I.EQ.NSO) GOTO 30
      RETURN
C Horizontal de-spiking only.
C
      ENTRY HUTSAR(BUF2,obuf,htot,ssamp,nsamp)
      NSO = NSAMP	! Redundant due to change requiring BDV processing
      NS = NSO - 1
      X0 = BUF2(SSAMP)
      X2 = BUF2(SSAMP+1)
      DO 60 I=SSAMP+1,NS
      X1 = X0
      X0 = X2
      X2 = BUF2(I+1)
      AVG = (X1+X2)/2
      IF (X0.LT.AVG-ESL) GOTO 55
C     ....Here if AVG-ESL .LE. X0
      IF (X0.LE.AVG+ESU) GOTO 60
      IF (X0.LE.X1+DSU) GOTO 60
      IF (X0.LE.X2+DSU) GOTO 60


      GOTO 59
C     ....Here if X0.LT.AVG-ESL
   55 IF (X0.GE.X1-DSL) GOTO 60
      IF (X0.GE.X2-DSL) GOTO 60

C
   59 CONTINUE
      HTOT = HTOT + 1			!Horizontal bit error
      IF (IBITS.GT.0) THEN
         X0 = IOR(IAND(LMASK,X0),IAND(HMASK,AVG))
      ELSE
         X0 = AVG
      ENDIF
      IF (IDN.GT.0) THEN
         OBUF(I) = IDN
      ELSE
         OBUF(I) = X0
      ENDIF

      CALL DUMP_BDV(I)


   60 CONTINUE

      RETURN
      END
c$!
c$! create adespike.mar
c	.psect autset
c	.title autset
c;                     4   8   12  16  20  24  28  32  36   40    44   48
c;  subroutine autset(nso,esu,esl,ecu,ecl,dsu,dsl,dcu,dcl,dcode,ibits,idn)
c	.entry 	autset,^m<>
c	movl	@4(ap),nso
c	movl	@8(ap),esu
c	movl	@12(ap),esl
c	movl	@16(ap),ecu
c	movl	@20(ap),ecl
c	movl	@24(ap),dsu
c	movl	@28(ap),dsl
c	movl	@32(ap),dcu
c	movl	@36(ap),dcl
c	movl	@40(ap),dcode
c	movl	@44(ap),ibits
c	movl	@48(ap),idn
c	movl	ibits,r0
c	bleq	900$
c	subl3	r0,#16,r0
c	movl	#1,r1
c	ashl	r0,r1,r1
c	decl	r1
c	movw	r1,lmask
c	mcomw	r1,hmask
c;masks are complemented to work with "bic"
c	mcomw	lmask,lmask
c	mcomw	hmask,hmask
c900$:	ret
c;
cnso:	.blkl	1
cesu:	.blkl	1
cesl:	.blkl	1
cecu:	.blkl	1
cecl:	.blkl	1
cdsu:	.blkl	1
cdsl:	.blkl	1
cdcu:	.blkl	1
cdcl:	.blkl	1
cdcode:	.blkl	1
cibits:	.blkl	1
cidn:	.blkl	1
clmask:	.blkw	1
chmask:	.blkw	1
ctmp1:	.blkw	1
ctmp2:	.blkw	1
carg_list:
c	.long	1
c	.address  samp
csamp:	.blkl	1
c;                      4    8   12   16   20   24
c;  subroutine autsar(buf1,buf2,buf3,obuf,htot,tot,ssamp,nsamp)
c	.entry	autsar,^m<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>
c	movl	4(ap),r5	;address of buf1
c	movl	8(ap),r6	;address of buf2
c	movl	12(ap),r7	;address of buf3
c	movl	16(ap),r8	;address of obuf
c	movl	@28(ap),r9	; i = ssamp
c	decl	r9		
c	movl	@32(ap),nso	; nso = nsamp
c	decl	nso		; *** fixed 15oct90
c	cvtwl	(r6)[r9],r0	; x0 = buf2(1)
c	cvtwl	2(r6)[r9],r2	; x2 = buf2(2)
c	brb	30$		; goto 30
c;	....Horizontal bit-error test
c10$:	cvtwl	2(r6)[r9],r2	;10 x2 = buf2(i+1)
c	addl3	r1,r2,r10
c	ashl	#-1,r10,r10	;   avg = (x1+x2)/2
c	subl3	esl,r10,r11
c	cmpl	r0,r11
c	blss	25$		;   if (x0.lt.avg-esl) goto 25
c	addl3	esu,r10,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.avg+esu) goto 30
c	addl3	dsu,r1,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.x1+dsu) goto 30
c	addl3	dsu,r2,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.x2+dsu) goto 30
c	incl	@20(ap)		;   htot = htot + 1
c	brb	40$		;   goto 40
c25$:
c	subl3	dsl,r1,r11
c	cmpl	r0,r11
c	bgeq	30$		;25 if (x0.ge.x1-dsl) goto 30
c	subl3	dsl,r2,r11
c	cmpl	r0,r11
c	bgeq	30$		;   if (x0.ge.x2-dsl) goto 30
c	incl	@20(ap)		;   htot = htot + 1
c        brb	40$		;   goto 40
c;       ....Vertical bit-error test
c30$:	cvtwl	(r5)[r9],r3	;   x3 = buf1(i)
c	cvtwl	(r7)[r9],r4	;   x4 = buf3(i)
c	addl3	r3,r4,r10
c	ashl	#-1,r10,r10	;   avg = (x3+x4)/2
c	subl3	ecl,r10,r11
c	cmpl	r0,r11
c	blss	35$		;   if (x0.lt.avg-ecl) goto 35
c	addl3	ecu,r10,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.avg+ecu) goto 50
c	addl3	dcu,r3,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.x3+dcu) goto 50
c	addl3	dcu,r4,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.x4+dcu) goto 50
c	incl	@24(ap)		;   tot = tot + 1
c	brb	40$		;   goto 40
c;
c35$:	subl3	dcl,r3,r11
c	cmpl	r0,r11
c	bgeq	50$		;35 if (x0.ge.x3-dcl) goto 50
c	subl3	dcl,r4,r11
c	cmpl	r0,r11
c	bgeq	50$		;   if (x0.ge.x4-dcl) goto 50
c	incl	@24(ap)		;   tot = tot + 1
c;
c40$:	cmpl	ibits,#0
c	bleq	42$		;40 if (ibits.le.0) goto 42
c	bicw3	lmask,r0,tmp1
c	bicw3	hmask,r10,tmp2  ;   x0 = ior(iand(lmask,x0),
c	bisw3	tmp1,tmp2,r0    ;          iand(hmask,avg))
c       brb     43$		;   goto 43
c42$:	movl	r10,r0		;42 x0 = avg
c;
c43$:	cmpl	idn,#0		;
c	bleq	45$		;43 if (idn.le.0) goto 45
c	cvtlw	idn,(r8)[r9]	;   obuf(i) = idn
C-----------------------------------------------------------------C
C   Changed the following statement (6/4/94) so that the ASSMBLY  C
C      code is correct                                            C
C-----------------------------------------------------------------C
c	brb	50$		;   goto 47
c45$:	cvtlw	r0,(r8)[r9]	;45 obuf(i) = x0
c;
C-----------------------------------------------------------------C
C   Registers must be saved and restored before and after the     C
C      call to dump_bdv                                           C
C-----------------------------------------------------------------C
c47$:	movl	r9,samp		; call dump_bdv(i)
c	incl	samp
c	callg	arg_list,dump_bdv
c;
c50$:	movl	r0,r1		; x1 = x0
c	movl	r2,r0		; x0 = x2
c	incl	r9		; i = i + 1
c	cmpl	r9,nso
c	blss	1010$		; if (i.lt.nso) goto 10
c	beql	3030$		; if (i.eq.nso) goto 30
c	ret			; return
c1010$:	jmp	10$
c3030$:	jmp	30$
c;                      4    8   12
c;  subroutine hutsar(buf2,obuf,htot,ssamp,nsamp)
c	.entry	hutsar,^m<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>
c	movl	4(ap),r6	;address of buf2
c	movl	8(ap),r8	;address of obuf
c	movl	@16(ap),r9	; i = ssamp
c	decl	r9		;
c	movl	@20(ap),nso	; nso = nsamp
c	cvtwl	(r6)[r9],r0	; x0 = buf2(ssamp)
c	cvtwl	2(r6)[r9],r2	; x2 = buf2(ssamp+1)
c	incl	r9		; i = i + 1
c	decl	nso		; ns = nso - 1
c;
c54$:				; do i=2,ns
c	movl 	r0,r1		; x1 = x0
c	movl	r2,r0		; x2 = x0
c	cvtwl	2(r6)[r9],r2
c	addl3	r1,r2,r10
c	ashl	#-1,r10,r10	; avg = (x1+x2)/2
c	subl3	esl,r10,r11
c	cmpl	r0,r11
c	blss	55$		; if (x0.lt.avg-esl) goto 55
c	addl3	esu,r10,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.avg+esu) goto 90
c	addl3	dsu,r1,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.x1+dsu) goto 90
c	addl3	dsu,r2,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.x2+dsu) goto 90
c	brb	59$		; goto 59
c;
c55$:	subl3	dsl,r1,r11
c	cmpl	r0,r11
c	bgeq	90$		; if (x0.ge.x1-dsl) goto 90
c	subl3	dsl,r2,r11
c	cmpl	r0,r11
c	bgeq	90$		; if (x0.ge.x2-dsl) goto 90
c;
c59$:	incl	@12(ap)		;59 htot = htot + 1
c	cmpl	ibits,#0
c	bleq	60$		;   if (ibits.le.0) goto 60
c	bicw3	lmask,r0,tmp1
c	bicw3	hmask,r10,tmp2  ;   x0 = ior(iand(lmask,x0),
c	bisw3	tmp1,tmp2,r0    ;          iand(hmask,avg))
c        brb     62$		;   goto 62
c60$:	movl	r10,r0		;60 x0 = avg
c;
c62$:	cmpl	idn,#0		;
c	bleq	65$		;62 if (idn.le.0) goto 65
c	cvtlw	idn,(r8)[r9]	;   obuf(i) = idn
c	brb	70$		;   goto 70
c65$:	cvtlw	r0,(r8)[r9]	;65 obuf(i) = x0
c
C-----------------------------------------------------------------C
C   Registers must be saved and restored before and after the     C
C      call to dump_bdv                                           C
C-----------------------------------------------------------------C
c70$:	movl	r9,samp		; call dump_bdv(i)
c	incl	samp
c	callg	arg_list,dump_bdv
c
c90$:	aoblss	nso,r9,5654$	; enddo
c	ret			; return
c5654$:	jmp	54$
c	.end
