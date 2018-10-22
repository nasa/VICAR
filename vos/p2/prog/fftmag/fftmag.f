C**********************************************************************
C       PROGRAM FFTMAG
C**********************************************************************
C
      INCLUDE 'VICMAIN_FOR'
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 13 OCT 1983
C     CONVERTED TO VICAR2 I/O BY FLORANCE MOSS, 28 AUG 1987
C     MSTP S/W CONVERSION (VICAR PORTING) A. Scop (CRI) 1 JUL 1994
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      EXTERNAL WORK
      COMMON INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
C
      CALL IFMESSAGE('FFTMAG version 27-APR-98')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')

      CALL XVSIZE(SL,SS,NXO,NSO,NX,NS)


      CALL XVPARM('BYS',MULTS,COUNT,DEF,1)
      CALL XVPARM('BYL',MULTL,COUNT,DEF,1)

      CALL XVPARM('BY',MULT,COUNT,DEF,1)
      IF (DEF .EQ. 0 ) THEN
          MULTL=MULT
          MULTS=MULT
      END IF      

C       
      IF (MULTL.EQ.0) MULTL=NXO/NX
      N=LOG(FLOAT(MULTL))/LOG(2.) + 1.0E-5
      MULTL = MAX(1.,2.**N)

      IF (MULTS.EQ.0) MULTS=NSO/NS
      N=LOG(FLOAT(MULTS))/LOG(2.) + 1.0E-5
      MULTS = MAX(1.,2.**N)

      NXO=MULTL*NX
      NSO=MULTS*NS

      CALL XVMESSAGE(' ',' ')
      CALL PRNT(4,1,MULTL,' LINE EXPANSION FACTOR = ')
      CALL PRNT(4,1,MULTS,' SAMPLE EXPANSION FACTOR = ')
      CALL PRNT(4,1,NXO,' OUTPUT LINES = ')
      CALL PRNT(4,1,NSO,' OUTPUT SAMPLES = ')

      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE',
     +       'U_NL',NXO,'U_NS',NSO,'U_FORMAT','COMP',' ')

      NBYTES = NSO * 8
      CALL STACKA(3,WORK,1,NBYTES)
      RETURN

      END
C
C
C
C
C**********************************************************************
C
      SUBROUTINE WORK(C,LENC)

      COMMON INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
      INTEGER STATUS
      COMPLEX C(LENC/8)

      IF (LENC.LT.NBYTES) THEN
          CALL MABEND('INSUFFICIENT MEMORY')
      END IF

      SCALE=MULTL*MULTS
      DO I=1,NSO
          C(I) = (0,0)
      END DO
      DO L=1,NXO
          IF (L.LE.NX/2+1 .OR. L.GE.NXO-NX/2+2) THEN
              K=-1
              CALL XVREAD(INUNIT,C,STATUS,'NSAMPS',NS,' ')
              IF (MULTS.GT.1) THEN 
                  DO J=NSO,NSO-NS/2+2,-1
                      K=K+1
                      C(J)=C(NS-K)
                      C(NS-K)=(0,0)
                  END DO
              END IF
          ELSE IF (L.EQ.NX/2+2) THEN
              DO I=1,NSO
                  C(I) = (0,0)
              END DO
          END IF
C       
          DO I=1,NSO
              C(I) = C(I)*SCALE
          END DO
          CALL XVWRIT(OUTUNIT,C,STATUS,'NSAMPS',NSO,' ')
C       

      END DO

      CALL XLDEL(OUTUNIT,'SYSTEM','NL',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','NL',NXO,STATUS,'FORMAT','INT',' ')

      CALL XLDEL(OUTUNIT,'SYSTEM','FORMAT',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','FORMAT','COMP',STATUS,'FORMAT',
     &'STRING',' ')

      CALL XLDEL(OUTUNIT,'SYSTEM','NS',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','NS',NSO,STATUS,'FORMAT','INT',' ')

      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')

      RETURN
      END
