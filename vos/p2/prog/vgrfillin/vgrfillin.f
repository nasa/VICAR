      INCLUDE 'VICMAIN_FOR'
C  PROGRAM VGRFILLIN:  VOYAGER line-fill program.....
C           VGRFILLIN  IN  OUT
C where IN is a Disk EDR (with engineering data attached) and OUT
C is a standard VICAR image.
C
      SUBROUTINE MAIN44
      COMMON/C1/HDR(248),HIST(1024)
      COMMON/C1/BUF(512,22),MFP(10,22),VPP(2,22),SS,NS
      COMMON/LCOUNT/ NLFILL, NLTRUNC, NLGORE
      byte	HDR
      INTEGER*2 BUF,MFP,VPP
      INTEGER*2 SS(10),NS(10)
      INTEGER	IMCTEMP,IMCODE,SCAN_RATE,SLT,ELT,MAXBUF/22/
      CHARACTER*5	FORMAT
      LOGICAL	TRUNC    
      CHARACTER*80  PMSG
C
      CALL IFMESSAGE('VGRFILLIN Version 6-Mar-95')
      NL     = 800
      NLFILL = 0
      TRUNC  = .FALSE.
C
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &        'COND','BINARY',' ')
      CALL XVGET(IUNIT,IND,'NL',NLI,'NS',NSI,'NLB',
     &                   NLB,'NBB',NBB,' ')
      IF (NLI.NE.800.OR.NSI.NE.800.OR.NLB.NE.2.OR.NBB.NE.224)
     &                    GOTO 980

      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &        'OP','WRITE',' ')
      CALL XVREAD(IUNIT,HDR,IND,'NSAMPS',248,' ')
c      CALL BITS(HDR(60),1,6,IMCODE)
      if (hdr(119).LT.0) then
        imctemp =((256)+HDR(119))/2
      else 
        imctemp = HDR(119)/2
      endif
      imcode = mod(imctemp,64)
      CALL VGRIMFMT(IMCODE,FORMAT,SCAN_RATE,SS,NS,IND)
      IF (IND.NE.1) THEN
            CALL XVMESSAGE('***it1it1Invalid Image Format',' ')
            GOTO 990
      ENDIF
      CALL XVREAD(IUNIT,HIST,IND,' ')

C     ....First check for some easy cases:
      IF (IMCODE.EQ.15 .OR. IMCODE.EQ.17) THEN
	CALL XVMESSAGE('PWS/PRA DATA: NO FILLIN DONE',' ')
	CALL VGCOPY(IUNIT,OUNIT,BUF,NL)
	GO TO 100
      ELSEIF (IMCODE.EQ.2) THEN		!IMS
	CALL XVMESSAGE('FILLIN HAS BEEN DONE BY IMBUILD',' ')
	CALL VGCOPY(IUNIT,OUNIT,BUF,NL)
	GO TO 100
      ENDIF
C
C     ....Unless old minor-frame format, no partial lines.
      IF (IMCODE.NE.9 .AND. IMCODE.NE.11
     &   .AND. IMCODE.NE.20 .AND. IMCODE.NE.21 .AND. IMCODE.NE.22
     &   .AND. IMCODE.NE.27 .AND. IMCODE.NE.31) SCAN_RATE=1
C
C     ....For alternate pixel editing, fix NS
      IF (IMCODE.EQ.5) NS(1)=740	!IM-21
      IF (IMCODE.EQ.7) NS(1)=800	!IM-22
      IF (IMCODE.EQ.13) NS(1)=762	!IM-23
      IF (IMCODE.EQ.19) NS(1)=87	!OC-3
      IF (IMCODE.EQ.26) NS(1)=646	!IM-2D
C
C     ....Editing of top and bottom of frame
      IF (IMCODE.EQ.6) THEN		!IM-Q
         SLT = 1			!Starting-line transmitted
         NLT = 480 			!Number-of-lines transmitted
      ELSE IF (IMCODE.EQ.14) THEN	!IM-24
         SLT = 301
         NLT = 195
      ELSE IF (IMCODE.EQ.25) THEN	!IM-25
         SLT = 101
         NLT = 585
      ELSE IF (IMCODE.EQ.23) THEN	!IM-26
         SLT = 271
         NLT = 266
      ELSE IF (IMCODE.EQ.16) THEN	!IM-2W
         SLT = 1
         NLT = 720
      ELSE
         SLT = 1
         NLT = 800
      ENDIF
C
C     ....Data compression modes IMO,IMQ,IMK,IM2c,IM26
      IF (IMCODE.EQ.4 .OR. IMCODE.EQ.6 .OR. IMCODE.EQ.8 .OR.
     & IMCODE.EQ.12 .OR. IMCODE.EQ.23) TRUNC=.TRUE.

      IF (SLT.GT.1) CALL VGCOPY(IUNIT,OUNIT,BUF,SLT-1)

      I1 = 1
      I2 = 2
      NLFILL = 0
      NLTRUNC = 0
      NLGORE = 0
      ELT = SLT + NLT - 1
      NLX = ELT + MAXBUF - 2
      N = 0
      DO L=SLT,NLX
         N = N + 1
         I3 = I1
         I1 = I2
         I2 = MOD(I2,MAXBUF) + 1
         IF (L.LE.ELT) THEN
            CALL XVREAD(IUNIT,BUF(1,I3),ind,' ')
            CALL GETMFP(BUF(3,I3),mfp(1,I3),SCAN_RATE,vpp(1,i3))
         ELSE
            CALL ZIA(mfp(1,i3),5)
         ENDIF
         IF (N.GE.MAXBUF-1) THEN
           CALL FILLIN(buf(113,1),I1,I2,SCAN_RATE,TRUNC)
           CALL XVWRIT(OUNIT,BUF(113,I2),ind,' ')
         ENDIF
      ENDDO
      IF (ELT.NE.800) CALL VGCOPY(IUNIT,OUNIT,BUF,800-ELT)

C     ....Write filled line count to label
  100 CALL XLADD( OUNIT, 'HISTORY', 'LIN_CNT', NLFILL, STAT, 
     . 'FORMAT', 'INT', 'ERR_ACT', 'SA',' ')
	PMSG='# MISSING LINES = '
	WRITE(PMSG(20:25), '(I6)' )  NLFILL
	CALL XVMESSAGE(PMSG,' ')
      IF (TRUNC) THEN
	PMSG='# TRUNCATED LINES = '
	WRITE(PMSG(22:27), '(I6)' )  NLTRUNC
	CALL XVMESSAGE(PMSG,' ')
	PMSG='# LINES WITH GORES = '
	WRITE(PMSG(22:27), '(I6)' )  NLGORE
	CALL XVMESSAGE(PMSG,' ')
      ENDIF

      CALL XVCLOSE(IUNIT,IND,' ')
      CALL XVCLOSE(OUNIT,IND,' ')
      RETURN
  980 CALL XVMESSAGE('***Input image is not a valid DEDR',' ')
      CALL ABEND
  990 CALL XVMESSAGE('***VGRFILLIN task cancelled',' ')
      CALL ABEND
      END
C
C Determine if minor-frame is present by examining EDR line-header (LHDR).
C Outputs: MFP(I)=1  if minor-frame is present  (I=1 to SCAN_RATE)
C                =0  if missing
C          VPP(1)=first valid pixel
C          VPP(2)=last valid pixel
C VPP is for edited frames where the left and right margins of the picture are
C not transmitted.
C
      SUBROUTINE GETMFP(LHDR,mfp,SCAN_RATE,vpp)
      INTEGER*2 MFP(10),VPP(2),ITP1,ITP2
      INTEGER SCAN_RATE
      byte    LHDR(220)
C
       itp1= LHDR(217)
       itp2= LHDR(218)
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       VPP(1) = 256*itp2 + itp1
       itp1= LHDR(219)
       itp2= LHDR(220)
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       VPP(2) = 256*itp2 + itp1
           DO I=1,SCAN_RATE
       itp1= LHDR(2*(I+85)-1)
       itp2= LHDR(2*(I+85))
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       itp1 = 256*itp2 + itp1
c              IF (LHDR(I+85).GT.0) THEN    
               if (itp1.gt.0) then
                  MFP(I) = 1		!DATA PRESENT
              ELSE
                  MFP(I) = 0		!DATA MISSING
              ENDIF
           ENDDO
C     ...Check for possible bad data from IMBUILD:
      IF (VPP(1).LT.1) VPP(1)=1
      IF (VPP(2).GT.800) VPP(2)=800
      RETURN
      END
C Fill in missing lines and missing pixels on a line.
C
      SUBROUTINE FILLIN(IN,I1,I2,SCAN_RATE,TRUNC)
      COMMON/C1/HDR(248),HIST(1024)
      COMMON/C1/BUF(512,22),MFP(10,22),VPP(2,22),SS,NS
      COMMON/LCOUNT/ NLFILL, NLTRUNC, NLGORE
      byte      HDR
      INTEGER*2 BUF,MFP,VPP
      INTEGER*2 SS(10),NS(10)
      INTEGER	SCAN_RATE
      INTEGER	MAXBUF/22/	!MUST BE EVEN FOR TRUNCATION-FILL ALGORITHM
      BYTE	IN(1024,22)
      LOGICAL	FILLED,GORE,TRUNC

C     ....First do truncation-fill if necessary:
      IF (.NOT.TRUNC) GOTO 20  !Skip if no truncation
      IF (VPP(1,I2).EQ.1 .AND. VPP(2,I2).EQ.800) GOTO 20  !Skip if no trunc
      IF (MFP(1,I2).EQ.0 .OR. MFP(1,I1).EQ.0) GOTO 20     !Skip if no data
      GORE = .FALSE.
      K = 0
      J = I2 - 1
C     ....Search for next even/odd line
   10 J = MOD(J,MAXBUF) + 1
      J = MOD(J,MAXBUF) + 1	!Every other line
      IF (J.EQ.I1) GOTO 20
      K = K + 2
      IF (MFP(1,J).EQ.0) GOTO 10

      IF (VPP(1,I2).NE.1) THEN	!LINE IS RIGHT-ALIGNED (EVEN)
         ISS = 1
         INS = VPP(1,I2)-1
         IF (VPP(2,I1).LT.INS .OR. VPP(2,J).LT.INS) THEN
            GORE = .TRUE.
            INS = MIN(VPP(2,I1),VPP(2,J))
            JSS = INS
            JNS = VPP(1,I2)-JSS+1
         ENDIF
         VPP(1,I2) = 1
      ELSE			!LINE IS LEFT-ALIGNED (ODD)
         ISS = VPP(2,I2)+1
         INS = 800-VPP(2,I2)
         IF (VPP(1,I1).GT.ISS .OR. VPP(1,J).GT.ISS) THEN
            GORE = .TRUE.
            ISS = MAX(VPP(1,I1),VPP(1,J))
            INS = 801-ISS
            JSS = VPP(2,I2)
            JNS = ISS-JSS+1
         ENDIF
         VPP(2,I2) = 800
      ENDIF
      NLTRUNC = NLTRUNC+1
      CALL INTERP(IN(ISS,I1),IN(ISS,I2),IN(ISS,J),K,INS)
      IF (GORE) THEN
         CALL SINTERP(IN(JSS,I2),JNS)
         NLGORE = NLGORE+1
      ENDIF

C     ....Now do line-fill:
   20 FILLED = .FALSE.
      GORE = .FALSE.
C
      DO 60 I=1,SCAN_RATE	!Loop through each minor-frame on line I2
      IF (MFP(I,I2).EQ.1.OR.MFP(I,I1).EQ.0) GOTO 60
      K = 1			!Here if minor-frame for line I2 is missing
      J = I2
C     ....Scan down for next valid minor-frame (line=J)
   30 J = MOD(J,MAXBUF) + 1
      IF (J.EQ.I1) GOTO 60	!Give up if no data
      K = K + 1
      IF (MFP(I,J).EQ.0) GOTO 30
C
C     ....Make a one-shot attempt to perform truncation-fill on line j.
      IF (.NOT.TRUNC) GOTO 40	!Skip if no truncation
      IF (VPP(1,J).EQ.1 .AND. VPP(2,J).EQ.800) GOTO 40  !Skip if no trunc
      IF (J.NE.I1-1 .AND. MFP(I,J+1).NE.0) THEN
         IF (VPP(1,J).NE.1) THEN
            ISS = 1
            INS = VPP(1,J)-1
            IF (VPP(2,I1).LT.INS .OR. VPP(2,J).LT.INS) THEN
               GORE = .TRUE.
               INS = MIN(VPP(2,I1),VPP(2,J))
               JSS = INS
               JNS = VPP(1,I2)-JSS+1
            ENDIF
            VPP(1,J) = 1
         ELSE
            ISS = VPP(2,J)+1
            INS = 800-VPP(2,J)
            IF (VPP(1,I1).GT.ISS .OR. VPP(1,J).GT.ISS) THEN
               GORE = .TRUE.
               ISS = MAX(VPP(1,I1),VPP(1,J))
               INS = 801-ISS
               JSS = VPP(2,I2)
               JNS = ISS-JSS+1
            ENDIF
            VPP(2,J) = 800
         ENDIF
         CALL INTERP(IN(ISS,J+1),IN(ISS,J),IN(ISS,I1),K+1,INS)
         IF (GORE) THEN
            CALL SINTERP(IN(JSS,I2),JNS)
            NLGORE = NLGORE+1
         ENDIF
      ELSE		!Can't interpolate: copy data from line I1
         IF (VPP(1,J).NE.1) THEN
            ISS = VPP(1,J)-1
            INS = ISS
            VPP(1,J) = 1
            CALL MVE(1,INS,IN(ISS,I1),IN(ISS,J),1,1)
         ELSE
            ISS = VPP(2,J)+1
            INS = 800-VPP(2,J)
            VPP(2,J) = 800
            CALL MVE(1,INS,IN(ISS,I1),IN(ISS,J))
         ENDIF
      ENDIF
      NLTRUNC = NLTRUNC+1

   40 ISS = SS(I)
      INS = NS(I)
      IF (.NOT.FILLED) THEN		!ONLY 1 COUNT PER LINE
         NLFILL = NLFILL+1
         FILLED = .TRUE.
      ENDIF
      CALL INTERP(IN(ISS,I1),IN(ISS,I2),IN(ISS,J),K,INS)
      MFP(I,I2) = 1
   60 CONTINUE
C
      RETURN
      END
C Fill-in missing line B2 by interpolating between two existing lines
C B1 and B3.  Line B1 is assumed to be adjacent to B2.  K is the number
C of lines between B1 and B3 (e.g: if B1,B2,B3 are consecutive lines,
C then K=2).
C
      SUBROUTINE INTERP(B1,B2,B3,K,NS)
      BYTE	B1(NS),B2(NS),B3(NS)
      INTEGER   I,NS
      INTEGER*4	IVAL

      INCLUDE 'fortport'

      K1 = K - 1
C
      DO I=1,NS
           IVAL = (K1*BYTE2INT(B1(I)) + BYTE2INT(B3(I)))/K
           IF (IVAL.GE.128) THEN
              B2(I) = (255-IVAL)*(-1)
           ELSE
               B2(I) = IVAL
           ENDIF
      ENDDO
C
      RETURN
      END
C Fill-in missing pixels on a line by interpolating between existing
C pixels B(1) and B(N).
C
      SUBROUTINE SINTERP(BUF,N)
      BYTE	BUF(N)
      INTEGER   IVAL

      INCLUDE 'fortport'

      IVAL = 0
      D1 = BYTE2INT(BUF(1))
      D2 = BYTE2INT(BUF(N))
      DB = (D2-D1)/(N-1)
      DN = D1 + 0.5

      DO I=2,N-1
         DN = DN + DB
         IVAL = DN
	 IVAL = MOD (IVAL,256)
         IF (IVAL.GE.128) THEN
            BUF(I)= -(256-IVAL)
         ELSE   
            BUF(I) = IVAL
         ENDIF    
      ENDDO
      RETURN
      END
C Copy pixel data to output.  No fill-in is performed.
C
      SUBROUTINE VGCOPY( IN, OUT, BUF, NL)
      INTEGER*2 BUF(*)

      DO L=1,NL
	CALL XVREAD( IN, BUF, IND,' ')
	CALL XVWRIT( OUT, BUF(113), IND,' ')
      ENDDO
      RETURN
      END
