C
C  REVISION HISTORY
c    1-97 ...cca... added distortion center as parameters cl,cs
C    4-94 ...CCA... INITIAL RELEASE
C    6-97 ...RRD... MADE PORTABLE FOR UNIX

        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44 
      IMPLICIT INTEGER(A-Z)
      REAL*4 OBUF(1800),A,RADSQ,DSC,DLC,D,LC,SC
      INTEGER*4 GSL,GSS,GSS0

	CALL XVMESSAGE('RADDIST VERSION 1.15.97',' ')
C-------GRID SIZE FROM PARAMETERS
	CALL XVPARM('NROW',NROW,ICNT,IDEF,0)
	CALL XVPARM('NCOL',NCOL,ICNT,IDEF,0)
	CALL XVPARM('INCR',INC,ICNT,IDEF,0)
	CALL XVPARM('CONSTANT',A,ICNT,IDEF,0)

C-------LINE AND SAMPLE CENTER ADDED ON 1/97
	CALL XVPARM('CL',LC,ICNT,IDEF,0)
	CALL XVPARM('CS',SC,ICNT,IDEF,0)	
C
	NPOINTS = NROW*NCOL
	IF (NPOINTS .EQ. 0) GO TO 994
	IF (NPOINTS .GT. 900) GO TO 998

C-------STARTING VALUES
	GSL = INC
	GSS = INC
	GSS0 = GSS
C
c*******old code****
C-------FIND CENTER OF GRID
C	LINE NUMBER OF FIRST GRID = GSL
C	LINE NUMBER OF LAST GRID  = GSL + ((NROW-1)*INC)/2
C	CENTER = (FIRST + LAST)/2
CCCCCCC	LC = GSL + (NROW-1)*INC/2
CCCCCCC	SC = GSS + (NCOL-1)*INC/2
c*******
C
C-------DERIVATION:   (r = Rold)
C	Assumed distortion model:  Rnew = r + A*r**3
c	deltaInRadius = A*r**3 
c	triangle with sides: deltaInRadius, deltaInLine, deltaInSamp is
c       similar to the triangle with sides: r, L-LC, S-SC
c	So, deltaInLine = A*r**3*(L-LC)/r = A*r**2*(L-LC)
c	So, deltaInSamp = A*r**3*(S-SC)/r = A*r**2*(S-SC)
c
	K = 1

	DO J=1,NROW		!ROWS
          DO I=1,NCOL   	!COLUMNS
	      LINE = GSL
	      SAMP = GSS

	      DLC = (LINE - LC)
	      DSC = (SAMP - SC)

	      RADSQ = DLC*DLC + DSC*DSC

	      D = DLC*A*RADSQ
              OBUF(K) = GSL + D

	      D = DSC*A*RADSQ
              OBUF(K+1) = GSS + D
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
	ENDDO

	CALL XVUNIT(IO,'OUT',1,IST,' ')

	CALL XVOPEN(IO,IST,'U_NL',1,'U_NS',NPOINTS*2,'O_FORMAT','REAL',
     1             'U_FORMAT','REAL','OP','WRITE','OPEN_ACT','SA',
     2             'IO_ACT','SA',' ')
	CALL XVWRIT(IO,OBUF,IST,' ')
	CALL XLADD(IO,'HISTORY','GRID_NROW',NROW,STAT,'FORMAT','INT',
     1             ' ')
	CALL XLADD(IO,'HISTORY','GRID_NCOL',NCOL,STAT,'FORMAT','INT',
     1             ' ')
	CALL XLADD(IO,'HISTORY','CONSTANT',A,STAT,'FORMAT','REAL',' ')
	CALL XVCLOSE(IO,IST,' ')

	RETURN
C
994	CALL XVMESSAGE('GRID SIZE MUST BE GT ZERO',' ')
	CALL ABEND
998	CALL XVMESSAGE('NUMBER OF COORDINATES MUST NOT EXCEED 900',' ')
	CALL ABEND
	END
