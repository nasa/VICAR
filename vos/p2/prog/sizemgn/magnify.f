CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify an image.  The vertical direction is magnified.  The
C horizontal dimension may be magnified or compressed.
C Input and output images may be byte, halfword, fullword, or real*4.
C Mixed data types (e.g. byte input, halfword output) are permitted.
C
      SUBROUTINE MAGNIFY(SCALE,N,SAMP,WGHT,ib,buf,rbuf,obuf)
      IMPLICIT NONE
      INTEGER*4 N,IB,SAMP(N)
	REAL*4 BUF(*),OBUF(N)
      REAL*4 SCALE,RBUF(N,2),WGHT(N)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      INTEGER*4 ELI,IND,L
      INTEGER*4 I1,I2,J1,J2,ITEMP,JSAVE
      REAL*4 REC,R,Y0,D,C
      
      R = 1.0/ZOOML
      REC = SLI - .5
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      JSAVE = 0
      J2 = 0

      DO 50 L=1,NLO
      Y0 = R*(L-.5) + REC
      J1 = Y0
      J1 = MAX0(J1,SLI)
      J1 = MIN0(J1,ELI-1)
      IF (J1.EQ.JSAVE) GOTO 40	!Skip read (image lines already in memory)
      JSAVE = J1
      ITEMP = I1
      I1 = I2
      I2 = ITEMP
      IF (J1.GT.J2) CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i1))
      J2 = J1 + 1
      CALL SREAD(IUNIT,J2,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
   40 D = Y0 - J1		!Compute vertical weights
      C = (1.-D)*SCALE
      D = D*SCALE
      CALL INTRPV(OCODE,NSO,ILO,IHI,C,D,RBUF(1,I1),RBUF(1,I2),
     & obuf(sso))
   50 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')
         
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute an output image line by iterpolating between two image
C lines BUF1 and BUF2.
C If OCODE=4, the output line is REAL*4.  Otherwise it is INTEGER*4.
C
      SUBROUTINE INTRPV(OCODE,NSO,ILO,IHI,C,D,BUF1,BUF2,obuf)
      IMPLICIT NONE
      INTEGER*4 OCODE,NSO,ILO,IHI
      REAL*4 C,D,BUF1(NSO),BUF2(NSO),OBUF(NSO)

      INTEGER*4 I,DN

      IF (OCODE.EQ.4) GOTO 50
      DO I=1,NSO
         DN = NINT(BUF1(I)*C + BUF2(I)*D)
         IF (DN.GT.IHI) THEN
            DN = IHI
         ELSE IF (DN.LT.ILO) THEN
            DN = ILO
 	 ENDIF
         OBUF(I) = REAL(DN)
      ENDDO
      RETURN
C
C    ....Here for real*4 output
   50 DO I=1,NSO
         OBUF(I) = BUF1(I)*C + BUF2(I)*D
      ENDDO
      RETURN
      END
