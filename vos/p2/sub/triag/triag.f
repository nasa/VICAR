C     11 JAN 78   ...JJL...   INITIAL RELEASE
c     14 Dec 92   ...WPL...   Ported for UNIX Conversion
c     
	SUBROUTINE TRIAG (LINRES,ISR,OSR,GEOMAB)
C	----------------------------------------
c
C    PACKS POINTS INTO GEOMA ORDER
c
      Real*4   ISR(2,1),OSR(2,1),GEOMAB(4,1)
      INTEGER*4   NRES/12/

      M = 0
      If (LINRES .EQ. (LINRES/2)*2) M=1
      K = 0
9     Do  10  J =1, NRES
      Do  11  L =1, 2
      M = M + 1
      K = K + 1
      N = (M + 1) / 2
      GEOMAB(1,K) = OSR(1,N)
      GEOMAB(2,K) = OSR(2,N)
      GEOMAB(3,K) = ISR(1,N)
11    GEOMAB(4,K) = ISR(2,N)
10    Continue 
c
      Return
      End
