      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C   1 JULY  94    R. SCHENK    PORTED TO UNIX 
c  88-2-11     ...MAG      converted to VICAR2 with modifications by SXP.
c  84-4-20     ...LWK...   replaced SYS000 with XVP calls, removed INP,
c                          OUT, SIZE, & FORMAT params.
C  21 NOV   83    ...CCA...    CONVERT TO VAX (RENAME FIL2) 
C  16 APRIL 79    ...JJL...    REMOVE IBCOM
C  27 JUNE  75    ...DAH...    CHANGES FOR CONVERSION TO 360/OS
C  10 MAR   75    ...JJL... 
C    FIL2     *****PREVIOUSLY FILTER2*******
      CHARACTER*4  SCAL/'SCAL'/,SHIF/'SHIF'/,T/'T   '/,BLANK/'    '/,
     1  DIVI/'DIVI'/, DE/'DE  '/,NLWX/'NLW '/,NSWX/'NSW '/,WEIG/'WEIG'/,
     2  HTS/'HTS '/,RANG/'RANG'/,E/'E   '/,HALF/'HALF'/,
     3  AMPL/'AMPL'/
      CHARACTER*4 DNMAX/'DNMA'/,DNMIN/'DNMI'/
      COMPLEX*8 C(64,64),CC(64)
      INTEGER*4  PARMS(1000),RANGE(2,10),OUTW(500)
      REAL*4  RPARM(1000),F1(33,10),MTF1(33,10),SN(10),F(33),MTF(33,2)
      REAL*4 F2(33,10),MTF2(33,10)
      INTEGER*4 DIVIDX(10)/10*0/
      INTEGER*4 IPSF(10),INLW(10),INSW(10),ISCAL(2,10)
      INTEGER*4 RECIP(10)
      COMPLEX*8 COM(1024)
      REAL*4 WT(513),FRE(513),OTF(513)
      EQUIVALENCE (C(1,1),COM(1))

      COMMON/C1/C,CC,PARMS,F1,MTF1,F,MTF,F2,MTF2
      COMMON/FT/NX(496)
      DATA ISCAL/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1/
      DATA RANGE/0,255,0,255,0,255,0,255,0,255,0,255,0,255,0,255,
     . 0,255,0,255/           
      DATA IPSF,RECIP,SN/10*0,10*0,10*0.0/
      LOGICAL XVPTST
      INTEGER*4 UNITNO
C
      IPRINT=0
      NR=0
      IPOLY=0
      WTMAX=32000.
      NV=0
      IRECT=0
      NCF=0
      NVF=0
      INLWX=0
      INSWX=0
      NSCALX=0
      NLW=11
      NP=11
      NC=0
      NSW=11
      NSN=0
      IDIVX=0
      THRESH=1000.

      CALL IFMESSAGE('fil2 version 1-JULY-94')
C
C  PARAMETER PROCESSOR
C
100   CONTINUE
C
      IF (XVPTST('RECT')) IRECT = 1
C
      INCF = NCF + 1
      CALL XVPARM ( 'PSF', MTF1(1,INCF), NUM, IDEF, 33 )
      IF ( NUM .NE. 0 ) THEN
C
         NCF = NCF + 1
         NC = NC + 1
         IPSF(NC) = 1
         CALL ZIA(MTF1(NUM+1,NC),33-NUM)
C
      ENDIF
C
C
      IF (XVPTST('PRINT')) IPRINT = 1
C
C
      INVF = NVF + 1
      CALL XVPARM ( 'VIF', F2(1,INVF), NUM, IDEF, 33 )
      IF ( NUM .NE. 0 ) THEN
C
         NVF = NVF + 1
C
      ENDIF
C
C
C
      INCF = NCF + 1
      CALL XVPARM ( 'HIF', F1(1,INCF), NUM, IDEF, 33 )
      IF ( NUM .NE. 0 ) THEN
C
         NCF = NCF + 1
C
      ENDIF
C
C
      INV = NV + 1
      CALL XVPARM ( 'VIA', MTF2(1,INV), NUM, IDEF, 33 )
      IF ( NUM .NE. 0 ) THEN
C
         NV = NV + 1
C
      ENDIF
C
C
C
      INC = NC + 1
      CALL XVPARM ( 'HIA', MTF1(1,INC), NUM, IDEF, 33 )
      IF ( NUM .NE. 0 ) THEN
C
         NC = NC + 1
         CALL ZIA(MTF1(NUM+1,NC),33-NUM)
C
      ENDIF
C
C
      CALL XVPARM ( 'WTMAX', WTMAX, INUM, IDEF, 0 )
C
C
      NSCALX = 1
      CALL XVPARM ( 'SCALE', ISCAL, INUM, IDEF, 2)
C
C
      IIDIVX = IDIVX + 1
      CALL XVPARM ( 'DIVIDE', DIVIDX, INUM, IDEF, 0 )
      IF ( NUM .NE. 0 ) THEN
C
         IDIVX = IDIVX + 1
C
      ENDIF
C
C
      CALL XVPARM ( 'NSW', NSW, INUM, IDEF, 1)
      IF ( NSW .EQ. (NSW/2)*2) NSW=NSW-1
      INSWX = INSWX + 1
      INSW(INSWX) = NSW
C
C
      CALL XVPARM ( 'NLW', NLW, INUM, IDEF, 1)
      IF ( NLW .EQ. (NLW/2)*2) NLW=NLW-1
      INLWX = INLWX + 1
      INLW(INLWX) = NLW
C
C
      CALL XVPARM ( 'RANGE', RANGE, INUM, IDEF, 2)
      NR = 1
C
C
      CALL XVPCNT ( 'MTF', NUM )
      IF ( NUM.NE.0) THEN
C
         NC = NC+1
         NCF = NCF + 1
         CALL XVPARM ( 'MTF', RPARM, INUM, IDEF, 66 )
         JJ=1
         NUM = NUM/2
         DO 111 J=1,NUM
C
            MTF1(JJ,NC) = RPARM(2*(J-1)+1)
            F1(JJ,NC) = RPARM(2*J)
            JJ = JJ + 1
C
111     CONTINUE
      ENDIF
C
C
      CALL XVPCNT('SN', INUM)
      IF ( INUM.NE.0 ) THEN
C
         NSN = NSN + 1
         CALL XVPARM ( 'SN', SN(NSN), INUM, IDEF, 1)
         RECIP(NSN) = 1
         SN(NSN) = 1./SN(NSN)**2
C
      ENDIF
C
C
C  END OF PARAMETER PROCESSOR
C
      NLW1 = (NLW+1)/2
      NSW1 = (NSW+1)/2
C
      npo = 6
      if (ipoly.eq.0) npo = 5
      call xvpopen( i, npo, 2100, 'filtparms', 'SA', UNITNO )

      IF(NC.NE.NCF .OR. NV.NE.NVF .OR. (NC.EQ.0.AND.NV.EQ.0)) GO TO 997
C
      NUMTOT=NC
      IF(NV.GT.NC) NUMTOT=NV
C  THIS LOOP CONTROLS THE MTF COMPUTATIONS
      DO 200 N=1,NUMTOT
      NLW=INLW(N)
      NSW=INSW(N)
      IF(NLW.EQ.1.OR.NSW.EQ.1) GO TO 304
C
C  TWO DIMENSIONAL FREQUENCIES
      DO 120 J=1,33
120   F(J)=(J-1)/64.
      CALL PRNT(7,33,F,'FREQUENCIES')
304   IF(NLW.GT.1.AND.NSW.GT.1) GO TO 300
C
C  ONE DIMENSIONAL FREQUENCIES
      MAX1=(NLW*NSW+1)/2
      IPOW=6
      MAX=NLW*NSW
302   IPOW=IPOW+1
      IF(2**IPOW.LT.MAX) GO TO 302
      IF(IPOW.GT.10) IPOW=10
      MAX=2**IPOW
      MAX2=MAX/2+1
      DO 303 J=1,MAX
303   FRE(J)=(J-1.0)/MAX
      CALL PRNT(7,MAX2,FRE,'FREQUENCIES')
      NUMTOT=NC
300   CONTINUE
      IF(NLW.GT.1.AND.NSW.GT.1) GO TO 301
C
C  COMPUTE ONE DIMENSIONAL WEIGHTS IF DESIRED
      IF(IPSF(N).EQ.1) GO TO 30
      J=2
      I=1
      SLOPE=(MTF1(2,N)-MTF1(1,N))/(F1(2,N)-F1(1,N))
      CONST=MTF1(1,N)-SLOPE*F1(1,N)
      OTF(1)=MTF1(1,N)
311   I=I+1
312   IF(FRE(I).GE.F1(J,N).AND.I.LT.MAX2) GO TO 313
      OTF(I)=SLOPE*FRE(I)+CONST
      IF(I.EQ.MAX2) GO TO 314
      GO TO 311
313   J=J+1
      SLOPE=(MTF1(J,N)-MTF1(J-1,N))/(F1(J,N)-F1(J-1,N))
      CONST=MTF1(J-1,N)-SLOPE*F1(J-1,N)
      GO TO 312
314   CONTINUE
      IF(IPSF(N).EQ.0) GO TO 31
30    SUM=0.0
      DO 33 J=2,33
33    SUM=SUM+MTF1(J,N)*2.0
      SUM=SUM+MTF1(1,N)
      DO 34 J=1,33
34    COM(J)=CMPLX(MTF1(J,N)/SUM,0.0)
      DO 35 J=34,MAX2
35    COM(J)=CMPLX(0.0,0.0)
      J1=0
      MAX3=MAX2+1
      DO 36 J=MAX3,MAX
      J1=J1+2
36    COM(J)=COM(J-J1)
      CALL FFTT(IPOW,-1,COM)
      DO 37 J=1,MAX2
37    OTF(J)=REAL(COM(J))
31    CONTINUE
      IF(RECIP(N).EQ.0) GO TO 320
      DO 318 J=2,MAX2
318   OTF(J)=OTF(J)/(OTF(J)**2+SN(N))
      OTF(1)=1.0/OTF(1)
320   CONTINUE
      CALL PRNT(7,MAX2,OTF,'HORIZONTAL OTF')
      DO 321 J=1,MAX2
321   COM(J)=CMPLX(OTF(J),0.0)
      J1=0
      MAX3=MAX2+1
      DO 322 J=MAX3,MAX
      J1=J1+2
322   COM(J)=COM(J-J1)
      CALL FFTT(IPOW  ,-1,COM)
      SUM=0.0
      DO 323 J=2,MAX1
      J1=MAX1-J+2
      WT(J-1)=REAL(COM(J1))
323   SUM=SUM+2.0*WT(J-1)
      WT(MAX1)=REAL(COM(1))
      SUM=SUM+WT(MAX1)
      COM(1)=COM(1)+CMPLX(OTF(1)*MAX-SUM,0.0)
      WT(MAX1)=WT(MAX1)-SUM+OTF(1)*MAX
      RMAX=ABS(WT(1))
      DO 324 J=1,MAX1
      IF(RMAX.LT.ABS(WT(J)))RMAX=ABS(WT(J))
324   CONTINUE
      RNORM=WTMAX/(RMAX/MAX)
      call xvpout( i, 'NLW', nlw, 'INT',1)
      call xvpout( i, 'NSW', nsw, 'INT',1)
      IF (IPOLY .NE. 0) call xvpout( i, 'RANGE', range(1,n), 'INT', 2)
      call xvpout( i, 'SCALE', iscal(1,n), 'INT', 2)
      IF(RNORM.GE.0.0) NORM = RNORM+0.5
      IF(RNORM.LT.0.0) NORM = RNORM-0.5
      IF (DIVIDX(N).GT.0) NORM = DIVIDX(N)
      call xvpout( i, 'DIVIDE', norm, 'INT', 1)
      K=0
      RNORM=RNORM/MAX
      DO J=1,MAX1
	ROUT=WT(J)*RNORM
	IF (ROUT.LT.0.0) OUTW(J) = ROUT-0.5
	IF (ROUT.GE.0.0) OUTW(J) = ROUT+0.5
      ENDDO
      call xvpout( i, 'WEIGHTS', outw, 'INT', max1)
      CALL PRNT(4,MAX1,OUTW,'TOP LEFT WEIGHT QUADRANT')
      L1=MAX1+1
      L2=MAX-MAX1+1
      DO 326 J=L1,L2
326   COM(J)=CMPLX(0.0,0.0)
      CALL FFTT(IPOW,+1,COM)
      DO 327 J=1,MAX2
327   OTF(J)=REAL(COM(J)) / (2.*MAX)         ! scaling due to rft2ch
      CALL PRNT(7,MAX2,OTF,'HORIZONTAL OTF RECOMPUTED FROM WEIGHTS')
      GO TO 200

C  2-D WEIGHTS:

301   CONTINUE
      IF(NC.LT.N) GO TO 133
      IF(IPSF(N).EQ.1) GO TO 20
C
C  INTERPOLATE THE HORIZONTAL MTF
      J=2
      I=1
      SLOPE=(MTF1(2,N)-MTF1(1,N))/(F1(2,N)-F1(1,N))
      CONST=MTF1(1,N)-SLOPE*F1(1,N)
      MTF(1,1)=MTF1(1,N)
201   I=I+1
202   IF(F(I).GE.F1(J,N).AND.I.LT.33) GO TO 203
      MTF(I,1)=SLOPE*F(I)+CONST
      IF(I.EQ.33) GO TO 204
      GO TO 201
203   J=J+1
      SLOPE=(MTF1(J,N)-MTF1(J-1,N))/(F1(J,N)-F1(J-1,N))
      CONST=MTF1(J-1,N)-SLOPE*F1(J-1,N)
      GO TO 202
204   CONTINUE
C
C  COMPUTE THE OTF FROM THE PSF IF DESIRED
      IF(IPSF(N).EQ.0) GO TO 21
20    SUM=0.
      DO 22 J=2,33
22    SUM=SUM+MTF1(J,N)*2.0
      SUM=SUM+MTF1(1,N)
      DO 23 J=1,33
23    COM(J)=CMPLX(MTF1(J,N)/SUM,0.0)
      J1=0
      DO 24 J=34,64
      J1=J1+2
24    COM(J)=COM(J-J1)
      CALL FFTT(6,-1,COM)
      DO 25 J=1,33
25    MTF(J,1)=REAL(COM(J))
21    CONTINUE
      IF(NV.LT.N) GO TO 134
133   CONTINUE
C
C  INTERPOLATE THE VERTIVAL MTF
      J=2
      I=1
      SLOPE=(MTF2(2,N)-MTF2(1,N))/(F2(2,N)-F2(1,N))
      CONST=MTF2(1,N)-SLOPE*F2(1,N)
      MTF(1,2)=MTF2(1,N)
205   I=I+1
206   IF(F(I).GE.F2(J,N).AND.I.LT.33) GO TO 207
      MTF(I,2)=SLOPE*F(I)+CONST
      IF(I.EQ.33) GO TO 208
      GO TO 205
207   J=J+1
      SLOPE=(MTF2(J,N)-MTF2(J-1,N))/(F2(J,N)-F2(J-1,N))
      CONST=MTF2(J-1,N)-SLOPE*F2(J-1,N)
      GO TO 206
208   CONTINUE
134   CONTINUE
C
C  TAKE RECIPROCAL IF DESIRED
      IF(RECIP(N).EQ.0) GO TO 211
      I1=1
      I2=2
      IF(NC.LT.N) I1=2
      IF(NV.LT.N) I2=1
      DO 212  I=I1,I2
      DO 210 J=2,33
210   MTF(J,I)=MTF(J,I)/(MTF(J,I)**2+SN(N))
      MTF(1,I)=1./MTF(1,I)
212   CONTINUE
211   CONTINUE
      IF(NC.LT.N) GO TO 236
      CALL PRNT(7,33,MTF(1,1),'HORIZONTAL INPUT OTF')
236   IF(NV.LT.N) GO TO 237
      CALL PRNT(7,33,MTF(1,2),'VERTICAL INPUT OTF')
237   CONTINUE
C
C  CONVERT MTF INTO A 2-D ARRAY
C  J ACROSS   I DOWN
C
C  SYMMETRICAL CASE
      IF(NV.GE.N.AND.NC.GE.N) GO TO 238
      L1=1
      IF(NC.LT.N) L1=2
      BIG=MTF(1,L1)
      DO 220 J=1,33
      DO 220 I=1,33
      R=SQRT(FLOAT((J-1)**2+(I-1)**2))+1.0
      L=R
      IF(L.LT.1) L=1
      DEL1=R-L
      DEL2=1.-DEL1
      IF(L.GE.33) GO TO 223
      VAL=MTF(L,L1)*DEL2+MTF(L+1,L1)*DEL1
      GO TO 220
223   VAL=MTF(33,L1)
220   C(I,J)=CMPLX(VAL,0.0)
C
C  ASYMMETRICAL CASE
238   IF(NV.LT.N.OR.NC.LT.N) GO TO 239
      BIG=(MTF(1,1)+MTF(1,2))/2.0
      IF(IRECT.EQ.0) GO TO 44
C  IF MTF .LT.1 CASE
43    DO 45 J=1,33
      DO 45 I=1,33
      VAL1=MTF(I,2)
      VAL2=MTF(J,1)
      VAL=AMIN1(VAL1,VAL2)
      IF(AMAX1(VAL1,VAL2).GT.THRESH)VAL=AMAX1(VAL1,VAL2)
45    C(I,J)=CMPLX(VAL,0.0)
      C(1,1)=CMPLX(BIG,0.0)
      GO TO 239
44    CONTINUE
C  IF MTF .GT.1 CASE
      DO 231 J=1,33
      DO 231 I=1,33
      R=SQRT(FLOAT((J-1)**2+(I-1)**2))+1.0
      L=R
      IF(L.LT.1) L=1
      DEL1=R-L
      DEL2=1.-DEL1
      AI=(R-I)**2+(J-1)**2
      AJ=(R-J)**2+(I-1)**2
      SUM=AI+AJ
      IF(L.GE.33) GO TO 230
      VALJ=MTF(L,1)*DEL2+MTF(L+1,1)*DEL1
      VALI=MTF(L,2)*DEL2+MTF(L+1,2)*DEL1
      IF(I.EQ.1.AND.J.EQ.1) GO TO 53
      VAL=VALJ*AI/SUM+VALI*AJ/SUM
      GO TO 231
53    VAL=BIG
      GO TO 231
230   VAL=MTF(33,1)*AI/SUM+MTF(33,2)*AJ/SUM
231   C(I,J)=CMPLX(VAL,0.0)
239   CONTINUE
C
C  REFLECT MTF QUADRANT
      DO 221 I=1,33
      J1=0
      DO 221 J=34,64
      J1=J1+2
221   C(I,J)=C(I,J-J1)
      I1=33
      DO 222 I=34,64
      I1=I1-1
      DO 222 J=1,64
222   C(I,J)=C(  I1,J)
C
	call fft2(c,64,64,-1)       ! compute 2-dim transform
C  ADJUST CENTRAL WEIGHT SUCH THAT MTF IS CORRECT AT D.C.
C
      ICNT=0
      SUM=0.0
      DO 251 I=1,NLW1
      I1=NLW1-I+1
      PROD=4.0
      IF(I1.EQ.1) PROD=2.0
      DO 251 J=1,NSW1
      ICNT=ICNT+1
      J1=NSW1-J+1
      RPARM(ICNT)=REAL(C(I1,J1))
      IF(J1.EQ.1) PROD=2.0
      IF(I1.EQ.1.AND.J1.EQ.1) PROD=1.0
251   SUM=SUM+RPARM(ICNT)*PROD
      RPARM(ICNT)=RPARM(ICNT)-SUM+BIG*4096.
      C(1,1)=C(1,1)+CMPLX(BIG*4096.-SUM,0.0)
C
C  FIND MAX WEIGHT
      RMAX=ABS(RPARM(1))
      NTOT=NSW1*NLW1
      DO 252 J=2,NTOT
      IF(ABS(RPARM(J)).GT.RMAX)  RMAX=ABS(RPARM(J))
252   CONTINUE
      RNORM=WTMAX/(RMAX/4096.)
C  WRITE TO SYS000
328   CONTINUE
      call xvpout( i, 'NLW', nlw, 'INT',1)
      call xvpout( i, 'NSW', nsw, 'INT',1)
      IF (IPOLY .NE. 0) call xvpout( i, 'RANGE', range(1,n), 'INT', 2)
      call xvpout( i, 'SCALE', iscal(1,n), 'INT', 2)
      IF(RNORM.GE.0.0) NORM = RNORM+0.5
      IF(RNORM.LT.0.0) NORM = RNORM-0.5
      IF (DIVIDX(N).GT.0) NORM = DIVIDX(N)
      call xvpout( i, 'DIVIDE', norm, 'INT', 1)
      CALL PRNT(4,1,NORM,'DIVIDE')
      K=0
      RNORM=RNORM/4096.
      DO J=1,NTOT
	ROUT=RPARM(J)*RNORM
	IF (ROUT.LT.0.0) OUTW(J) = ROUT-0.5
	IF (ROUT.GE.0.0) OUTW(J) = ROUT+0.5
      ENDDO
      call xvpout( i, 'WEIGHTS', outw, 'INT', ntot)
      CALL XVMESSAGE('TOP LEFT WEIGHT QUADRANT',' ')
      IST = 1-NSW1
      DO 254 I=1,NLW1
      IST=IST+NSW1
      LAST=IST+NSW1-1
254   CALL PRNT(4,LAST-IST+1,OUTW(IST),' ')
      NP=NP+NTOT
C
C  RECOMPUTE THE MTF FROM THE WEIGHTS
      I1=NLW1+1
      I2=64-NLW1+1
      DO 260 I=I1,I2
      DO 260 J=1,64
260   C(I,J)=CMPLX(0.0,0.0)
      I1=NSW1+1
      I2=64-NSW1+1
      DO 261 J=I1,I2
      DO 261 I=1,64
261   C(I,J)=CMPLX(0.0,0.0)
c
      call fft2(c,64,64,1)        ! compute 2-dim transform
      IF(IPRINT.EQ.1) GO TO 268
      DO 266 I=1,33
266   MTF(I,1)=REAL(C(I,1)) / 8192.  ! scaling due to rft2
      CALL PRNT(7,33,MTF(1,1),'VERTICAL OTF RECOMPUTED FROM WEIGHTS')
      DO 267 J=1,33
267   MTF(J,1)=REAL(C(1,J)) / 8192.  ! scaling due to rft2
      CALL PRNT(7,33,MTF(1,1),'HORIZONTAL OTF RECOMPUTED FROM WEIGHTS')
      GO TO 200
268   CALL XVMESSAGE('2-D OTF,DC AT UPPER LEFT',' ')
      DO 270 I=1,33
      DO 271  J=1,33
271   MTF(J,1)=REAL(C(I,J))
      CALL PRNT(7,33,MTF(I1,1),' ')
270   CONTINUE
200   CONTINUE
C  END OF MTF COMPUTATIONS LOOP
C
      call xvpclose( i)
      RETURN
C
997     CALL XVMESSAGE('*** A SET OF FREQ. OR MTFS IS MISSING',' ')
        CALL ABEND
998	CALL XVMESSAGE('BAD PARAM2 KEY',' ')
	CALL ABEND
      END
	SUBROUTINE FFTT(IPOW,MODE,COM)
C-----THIS SUBROUTINE computes 1-dim ffts using RFT2CH instead of
c-----FFTT.   it will accept and return the assumed formats.
	REAL*4 BUF(1026),COM(2050)
C
	M=1
	N= 2**IPOW
	IF(MODE .EQ. -1) GO TO 10
C
C------------------------------------
C-----FORWARD
	CALL MVE(7,N,COM,BUF,2,1)    !STRIP REALS OUT OF COMPLEX
	CALL RFT2CH(BUF,M,N,MODE)    !RETURNED AS COMPLEX
	CALL MVE(7,N+2,BUF,COM,1,1)
	RETURN
C
C-------------------------------------
C-----INVERSE
10	CALL MVE(7,N+2,COM,BUF,1,1)
	CALL RFT2CH(BUF,M,N,MODE)    !PROCESS COMPLEX DATA
	CALL ZIA(COM,2*N)
	CALL MVE(7,N,BUF,COM,1,2)    !STICK REALS INTO COMPLEX 
C
	RETURN
C
	END
	subroutine fft2(c,m,n,mode)
c-----this routine will do 2-dim fft using RFT2 instead of
c-----FFTT . It accepts and returns data formated as before.
	real*4 buf(64,66)
	complex*8 c(m,n)
c
	if(mode .eq. 1) go to 20
c==============================================
c-----inverse transform
	do 10 i=1,m/2+1
	do 10 j=1,n
	k=2*i
	buf(j,k-1) =  real(c(j,i))
	buf(j,k)   = aimag(c(j,i))
10 	continue
c
	call rft2(buf,m,n,-1,RETSTATUS)
c
	do 30 i=1,m
	do 30 j=1,n
30	c(j,i) = cmplx(buf(j,i),0.0)
	return
c==============================================
c-----forward transform
20	continue
	do 40 i=1,m
	do 40 j=1,n
40	buf(j,i) = real(c(j,i))
c
	call rft2(buf,m,n,1,RETSTATUS)
	if(RETSTATUS.lt.0) GOTO 15
c
	do 50 i=1,m/2+1
	k = i*2
	do 50 j=1,n
50	c(j,i) = cmplx(buf(j,k-1),buf(j,k))
	return
c
15	call XVMESSAGE('RFT2 ERROR',' ')
	call abend
	end
