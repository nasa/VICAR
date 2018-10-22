c
      Subroutine  FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
c
C     11 JAN 78   ...JJL...       INITIAL RELEASE
c     15 Jan 93   ...W.P. Lee...  Ported for UNIX Conversion
C
C     SET UP RETICLE DESCRIPTIVE ARRAY A AND CALL FOMCAL
C
C     AA CONTAINS NPOINT ENTRYS OF
C          AA(1,J) LINE VALUE OF JTH RETICLE
C          AA(2,J) SAMPLE VALUE
C          AA(3,J) LATITUDE
C          AA(4,J) LONGITUDE
C
      Real*4  AA(4,20),A(5,20),B(6)
      Real*8  OM(3,3)
      Real*8  RS(3)
      Real*4  CL, CS
c
      Do 100 I = 1, NPOINT
       A(1,I) = AA(1,I)
       A(2,I) = AA(2,I)
       A(3,I) =  B(3)
       A(4,I) = AA(3,I)
       A(5,I) = AA(4,I)
100   Continue
C
      Call FOMCAV(IND,NPOINT,A,B,OM,RS,CL,CS)
c
      Return
      End
