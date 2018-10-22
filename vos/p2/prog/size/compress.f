CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image.  The vertical direction is compressed.  The
C horizontal direction may be magnified or compressed.
C 
C
      SUBROUTINE COMPRESS(SCALE,NS,SAMP,WGHT,ib,buf,rbuf,obuf)
      IMPLICIT NONE
      INTEGER*4 NS,IB,SAMP(NS)
      REAL*8 SCALE
      REAL*8 BUF(1),RBUF(NS,2),WGHT(NS),OBUF(NS)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*8 ZOOML,ZOOMS
      REAL*8 GSCALE

      INTEGER*4 I1,I2,J1,J2
      INTEGER*4 J,L,N,ELI,LINE,IND,INC
      REAL*4 R,Y2
      REAL*8 C,D

      R = 1.0/ZOOML
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      IF (LZOOM.LT.0) GOTO 40
C
C     ....Here to compress an image using a real zoom in the vertical
C     ....direction.  The horizontal zoom may be real or integral valued.
      J1 = SLI
      C = ZOOML*SCALE
      D = 0.0D0
      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
C

      DO 20 L=1,NLO
      CALL SMUL(NSO,RBUF(1,I2),RBUF,1.0D0-D)

      Y2 = L*R + SLI
      J2 = Y2
      J2 = MIN0(J2,ELI)
      D = Y2 - J2
      J1 = MIN0(J1+1,ELI)
      DO WHILE (J1.NE.J2)
         CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &    SAMP,WGHT,ib,buf,rbuf(1,i2))
         CALL ADDV(8,NSO,RBUF(1,I2),RBUF,1,1)
         J1 = MIN0(J1+1,ELI)
      ENDDO

      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
      CALL INTRPV(OCODE,NSO,ILO,IHI,C,C*D,RBUF,RBUF(1,I2),
     & obuf(sso))

   20 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')
      RETURN

C     ....Here to compress vertical scale by an integral lzoom
   40 INC = -LZOOM
      N = INC - 1
      LINE = SLI
      C = ZOOML*SCALE

      DO 50 L=1,NLO
      CALL SREAD(IUNIT,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))

      DO J=1,N
         CALL SREAD(IUNIT,LINE+J,SSI,NSI,NSO,IZOOM,ZOOMS,
     &    SAMP,WGHT,ib,buf,rbuf)

         CALL ADDV(8,NSO,RBUF,RBUF(1,I2),1,1)
      ENDDO
      LINE = LINE + INC

      CALL OUT_SCALE(OCODE,C,ILO,IHI,NSO,RBUF(1,I2),obuf(sso))

   50 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Multiply a line by a constant C
C
      SUBROUTINE SMUL(NSO,RIN,ROUT,C)
	IMPLICIT NONE
	INTEGER*4 NSO,I
      REAL*8 RIN(NSO),ROUT(NSO)
      REAL*8 C
C
      DO I=1,NSO
         ROUT(I) = C*RIN(I)
      ENDDO
      RETURN
      END
