c******************************************************************
C 
c Subroutine   GEOMVO(CONV,ICAM,LOCS)
c 
c Viking Orbiter routine to generate GEOMA parameters.
c
c ARGUMENTS
c
c  Input:   
c
c    ICAM  - Viking Orbiter camera serial number.  Valid are:
c        	7=VO-1A		8 = VO-2A
c	        4=VO-1B		6 = VO-2B
c    RES   - Image space line-sample coordinates for the 103 reseau
c	    marks of a Viking Orbiter frame.
c  Output:
c
c    CONV  - GEOMA parameters in the following format:
c
c		 CONV(1) = 'NAH '
c		 CONV(2) = '    '
c		 CONV(3) = 21		(INTEGER*4, int)
c		 CONV(4) = 'NAV '
c		 CONV(5) = '    '
c		 CONV(6) = 8		(INTEGER*4, int)
c		 CONV(7) = 'TIEP'
c		 CONV(8) = '    '
c		 CONV(9) = beginning of tiepoints in REAL*4, float format
c
c HISTORY
c
c  Original Programmer: Gary Yagi, 1 June 1990
c  Current Cognizant Programmer: G. Yagi
c  Source Language: Fortran
c  Revisions: 
c		25-Aug-93  ...TLT...  Ported to Unix
C
c******************************************************************
c
      SUBROUTINE GEOMVO(CONV,ICAM,LOCS)
c
      IMPLICIT INTEGER (A-Z)
      REAL*4 LOCS(2,103)	!Input image-space reseau locations
      REAL*4 CONV(*)		!Output GEOMA parameters
      REAL*4 OSLOCS(2,103)	!Object-space reseau locations
      CHARACTER*4 NAH,NAV,TIEP,BLANK
      DATA NAH/'NAH '/
      DATA NAV/'NAV '/
      DATA TIEP/'TIEP'/
      DATA BLANK/'    '/

      T21 = 21
      T8 = 8
      CALL MVCL(NAH,CONV(1),4)
      CALL MVCL(BLANK,CONV(2),4)
      CALL MVE(-4,1,T21,CONV(3),1,1)
      CALL MVCL(NAV,CONV(4),4)
      CALL MVCL(BLANK,CONV(5),4)
      CALL MVE(-4,1,T8,CONV(6),1,1)
      CALL MVCL(TIEP,CONV(7),4)
      CALL MVCL(BLANK,CONV(8),4)
      CALL VOOS(ICAM,OSLOCS)	!Get OS reseau locations
      J = 8
C
      DO 100 N1=1,103,23
      N2 = N1 + 10
      M1 = N1 + 11
      M2 = N1 +21
C
      DO 10 I=N1,N2
      CONV(J+1)=OSLOCS(1,I)
      CONV(J+2)=OSLOCS(2,I)
      CONV(J+3)=LOCS(1,I)
      CONV(J+4)=LOCS(2,I)
      CONV(J+5)=OSLOCS(1,I)
      CONV(J+6)=OSLOCS(2,I)
      CONV(J+7)=LOCS(1,I)
      CONV(J+8)=LOCS(2,I)
   10 J = J + 8
C
      DO 20 I=M1,M2
      CONV(J+1)=OSLOCS(1,I)
      CONV(J+2)=OSLOCS(2,I)
      CONV(J+3)=LOCS(1,I)
      CONV(J+4)=LOCS(2,I)
      CONV(J+5)=OSLOCS(1,I+1)
      CONV(J+6)=OSLOCS(2,I+1)
      CONV(J+7)=LOCS(1,I+1)
      CONV(J+8)=LOCS(2,I+1)
   20 J = J + 8
C
  100 CONTINUE
C
      CALL PRNT(4,1,J,' CONV size=.')
      RETURN
      END
