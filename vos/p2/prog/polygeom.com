$!****************************************************************************
$!
$! Build proc for MIPL module polygeom
$! VPACK Version 1.8, Wednesday, February 07, 1996, 17:42:02
$!
$! Execute by entering:		$ @polygeom
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module polygeom ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to polygeom.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("polygeom.imake") .nes. ""
$   then
$      vimake polygeom
$      purge polygeom.bld
$   else
$      if F$SEARCH("polygeom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polygeom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polygeom.bld "STD"
$   else
$      @polygeom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polygeom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polygeom.com -
	-s polygeom.f -
	-i polygeom.imake -
	-p polygeom.pdf -
	-t tstpolygeom.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polygeom.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

C  IBIS ROUTINE POLYGEOM
C
C  POLYGEOM PERFORMS GEOMETRIC TRANSFORMATIONS TO CORRECT FOR DISTORTION
C  INCREASE OR DECREASE THE SIZE OF A POINT POLYGON DATA SET.  THE
C  TRANSFORMATION IS SPECIFIED BY CONTROL POINTS WHICH ARE GIVEN IN LINE
C  SAMPLE COORDINATES IN THE INPUT AND OUTPUT PICTURE.   THE OPERATION A
C  COMPUTATIONS OF POLYGEOM ARE SIMILAR TO GEOMA EXCEPT THAT POLYGON DAT
C  CAN BE TRANSFORMED MUCH MORE RAPIDLY THAN IMAGES.
C
C  USER PARAMETERS:
C
C  NAH,NH -  NUMBER OF AREAS HORIZONTALLY.
C  NAV,NV -  NUMBER OF AREAS VERTICALLY.
C  TIEPOINTS,NL1,NS1,OL1,OS1,... - THIS KEYWORD AND ASSOCIATED NUMBERS S
C            THE MAPPING OF CONTROL POINTS BETWEEN OUTPUT AND INPUT PICT
C            ONE MULTIPLE OF FOUR INTEGERS IS GIVEN FOR EACH TIEPOINT.
C            THE ORDERING OF TIEPOINTS IS TOP ROW FIRST, AND LEFT TO RIG
C            WITHIN EACH ROW.
C  INVERSE - THIS KEYWORD CALLS A REVERSE TRANSFORMATION PROCEDURE TO
C            CALCULATE POINTS IN THE ORIGINAL IMAGE BASED ON THE OUTPUT
C  IGNORE,X,Y - THIS CAUSES ALL OCCURRENCES OF THE POINT (X,Y) TO BE LEF
C            AS IS.  THE DEFAULT IS TO IGNORE THE POINT (0.,0.).
C
	IMPLICIT INTEGER(A-Z)
	REAL*8	TAB(8), WORK(64), XX,YY, FX,FY
	REAL	X,Y, MDIST,DIST, MAG
	REAL	XL,XU,YL,YU
        REAL	TIEPOINT(8400)
	REAL	QX(4,2100), QY(4,2100), COEFF(9,2100)
        COMMON	/COM2/ NAH,NAV, QX,QY, COEFF
        INTEGER	PPTR(4)
        integer status,rdgr,wrgr,getgr,putgr,clgr
	REAL	IGNORE(2), XIGNOR,YIGNOR
	REAL	EXTRA(40), COORD1, COORD2
	LOGICAL INVERS,INSECT, NOPRINT, EOF, ZERO, XVPTST
	CHARACTER*60 STRING

      call ifmessage('POLYGEOM version 06-MAR-95')

C---- GET PARAMETERS

        CALL XVP ('TIEPOINT', TIEPOINT, NTIEPP)
        CALL XVP ('NAH', NAH, DUMMY)
        CALL XVP ('NAV', NAV, DUMMY)
	IF (NTIEPP .NE. 4*(NAH+1)*(NAV+1) ) THEN
	    CALL XVMESSAGE ('NUMBER OF TIEPOINTS DOES NOT ' //
     + 'MATCH NAH AND NAV',' ')
	    CALL ABEND
	ENDIF
        CALL XVP ('IGNORE', IGNORE, DUMMY)
        XIGNOR = IGNORE(1)
        YIGNOR = IGNORE(2)
        CALL XVP ('SKIP', SKIP, DUMMY)
        INSECT = XVPTST ('INSECT')
        INVERS = XVPTST ('INVERSE')
        NOPRINT = XVPTST ('NOPRINT')

C---- OPEN INPUT AND OUTPUT FILES.

	STATUS = RDGR (1, 1, 2+SKIP)
        IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	STATUS = WRGR (1, 2, 2+SKIP)
        IF (STATUS.NE.1) CALL SIGNALGR(2,STATUS,1)



        PPM = 0
        REC1 = 0
        REC2 = 0
C
C  SET UP GRID AREAS FOR SOLUTION BY SOLVE ROUTINE.  SOLUTIONS ARE
C  SAVED IN COEFF.
      XL = 1.E35
      YL = 1.E35
      XU = -1.E35
      YU = -1.E35
      DO 20 IX=1,NAH
      DO 20 JX=1,NAV
      NSEQ = IX+(JX-1)*NAH
      PPTR(1) = ((IX-1)+(JX-1)*(NAH+1))*4+PPM+1
      PPTR(4) = PPTR(1)+(NAH+1)*4
      PPTR(2) = PPTR(1)+4
      PPTR(3) = PPTR(4)+4
      DO 12 I=1,4
      P = PPTR(I)
      IF (.NOT.INVERS) P = P+2
      IF (IX.EQ.1) XL = AMIN1(XL,TIEPOINT(P))
      IF (JX.EQ.1) YL = AMIN1(YL,TIEPOINT(P+1))
      IF (IX.EQ.NAH) XU = AMAX1(XU,TIEPOINT(P))
      IF (JX.EQ.NAV) YU = AMAX1(YU,TIEPOINT(P+1))
      QX(I,NSEQ) = TIEPOINT(P)
 12   QY(I,NSEQ) = TIEPOINT(P+1)
      NRANK = 8
      DELPT = 0
      DO 15 I=1,64
 15   WORK(I) = 0.D+0
      IF (QX(1,NSEQ).EQ.QX(2,NSEQ).AND.QY(1,NSEQ).EQ.QY(2,NSEQ)) DELPT=1
      IF (QX(3,NSEQ).EQ.QX(4,NSEQ).AND.QY(3,NSEQ).EQ.QY(4,NSEQ)) DELPT=3
      IF (DELPT.NE.0) NRANK = 6
      NHALF = NRANK/2
      NONES = NRANK*6-24
      BLOCK = NRANK*NHALF+NHALF
      PTR = 1
      DO 16 I=1,4
      P = PPTR(I)
      TAB(PTR) = TIEPOINT(P)
      TAB(PTR+NHALF) = TIEPOINT(P+1)
      WORK(PTR) = TIEPOINT(P+2)
      WORK(PTR+NRANK) = TIEPOINT(P+3)
      IF (NRANK.EQ.8) WORK(PTR+16) = TIEPOINT(P+2)*TIEPOINT(P+3)
      WORK(PTR+NONES) = 1.D+0
      IF (DELPT.NE.I) PTR = PTR+1
 16   CONTINUE
      DO 17 I=1,NHALF
      DO 17 J=1,NHALF
      I1 = I+(J-1)*NRANK
 17   WORK(I1+BLOCK) = WORK(I1)
      CALL SOLVE(TAB,WORK,NRANK,1,1.E-14,IER)
      IF (IER.NE.0) CALL MABEND(2)
      PTR = 1
      DO 18 I=1,4
      COEFF(I,NSEQ) = TAB(PTR)
      COEFF(I+4,NSEQ) = TAB(PTR+NHALF)
      IF (I.NE.NHALF) PTR = PTR+1
 18   CONTINUE
      COEFF(9,NSEQ) = 1./(COEFF(1,NSEQ)*COEFF(6,NSEQ)-COEFF(2,NSEQ)*
     .COEFF(5,NSEQ))
      IF (NRANK.EQ.8) GO TO 20
      COEFF(3,NSEQ) = 0.
      COEFF(7,NSEQ) = 0.
 20   CONTINUE




C  TRANSLATE THE POINTS BY THE CORRESPONDING SOLVE SOLUTION.

	PRINTCOUNT = 0
	IF (.NOT. NOPRINT) THEN
	    WRITE (STRING, '(A)' ) 
     *	'        INPUT COORDINATES           OUTPUT COORDINATES'
	    CALL XVMESSAGE (STRING,' ')
	ENDIF

	IQ = 1
	JQ = 1
	QQ = 1

	DO WHILE (.TRUE.)
	    STATUS = GETGR (1, ZERO, EOF, COORD1, COORD2, EXTRA)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    IF (EOF) GOTO 890
	    X = COORD1
	    Y = COORD2
	    IF (ABS(X-XIGNOR)+ABS(Y-YIGNOR) .GT. 1.0E-6) THEN
		IF (INSECT) THEN
		    IF (X.LT.XL.OR.X.GT.XU) GO TO 100
		    IF (Y.LT.YL.OR.Y.GT.YU) GO TO 100
		ENDIF
		MDIST = 2.E20
		IQNAH = IQ+NAH-1
		JQNAV = JQ+NAV-1
C		  FIND GRID AREA CONTAINING POINT.  SEARCH IN 'NEAREST' ORDER.
		DO I = 1, NAH
		    IS = (2*MOD(I,2)-1)*(I/2)
		    IT = MOD(IQNAH+IS,NAH)+1
		    DO J = 1, NAV
			JS = (2*MOD(J,2)-1)*(J/2)
			JT = MOD(JQNAV+JS,NAV)+1
			QT = IT+(JT-1)*NAH
			CALL INSIDE(X,Y,IT,JT,QT,DIST)
			IF (DIST.LE.0.) GO TO 150
			IF (DIST-MDIST) 139,138,140
 138			IF (QT.GE.MQT) GO TO 140
 139			MDIST = DIST
			MIT = IT
			MJT = JT
			MQT = QT
 140			CONTINUE
		    ENDDO
		ENDDO
		IT = MIT
		JT = MJT
		QT = MQT
 150		IQ = IT
		JQ = JT
		QQ = QT
		IF (.NOT. INVERS) THEN
		    XX = COEFF(1,QQ)*X+COEFF(2,QQ)*Y+
     *				COEFF(3,QQ)*X*Y+COEFF(4,QQ)
		    YY = COEFF(5,QQ)*X+COEFF(6,QQ)*Y+
     *				COEFF(7,QQ)*X*Y+COEFF(8,QQ)
		ELSE
 		    MAG = X*X+Y*Y+1.E-5
		    XX = X-COEFF(4,QQ)
		    YY = Y-COEFF(8,QQ)
		    DO I = 1, 10
			FX = COEFF(1,QQ)*XX+COEFF(2,QQ)*YY+
     *				COEFF(3,QQ)*XX*YY+COEFF(4,QQ)-X
			FY = COEFF(5,QQ)*XX+COEFF(6,QQ)*YY+
     *				COEFF(7,QQ)*XX*YY+COEFF(8,QQ)-Y
			XX = XX-(COEFF(6,QQ)*FX-COEFF(2,QQ)*FY)*COEFF(9,QQ)
			YY = YY-(COEFF(1,QQ)*FY-COEFF(5,QQ)*FX)*COEFF(9,QQ)
			IF ((FX*FX+FY*FY)/MAG .LT. 1.0D-10) GO TO 160
		    ENDDO
		    CALL XVMESSAGE ('INVERSE DID NOT CONVERGE',' ')
		    CALL ABEND
		ENDIF

 160		CONTINUE
		COORD1 = SNGL(XX)
		COORD2 = SNGL(YY)
 100		CONTINUE

	    ENDIF
	    STATUS = PUTGR (2, COORD1, COORD2, EXTRA)
            IF (STATUS.NE.1) CALL SIGNALGR(2,STATUS,1)

	    IF (.NOT. NOPRINT .AND. PRINTCOUNT .LT. 5) THEN
		WRITE (STRING, '(F12.3,1X,F12.3,4X,F12.3,1X,F12.3)' )
     *				 X,Y, COORD1, COORD2
		CALL XVMESSAGE (STRING,' ')
		PRINTCOUNT = PRINTCOUNT + 1
	    ENDIF
	ENDDO

890	CONTINUE
	STATUS = CLGR (1)
        IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	STATUS = CLGR (2)
        IF (STATUS.NE.1) CALL SIGNALGR(2,STATUS,1)

	RETURN
	END



C*****************************************************************
      SUBROUTINE INSIDE(X,Y,IQ,JQ,QQ,DIST)
C
C  USES DOT PRODUCTS FOR POINT IN POLYGON SOLUTION.  BEST METHOD FOR CON
C  POLYGONS.
C
      IMPLICIT INTEGER(A-Z)
      REAL X,Y,DOT,QX,QY,COEFF,DIST,DX,DY
      COMMON /COM2/NAH,NAV,QX(4,2100),QY(4,2100),COEFF(9,2100)
C
      DIST = -1.
      DO 10 I=1,4
      IU = MOD(I,4)+1
      GO TO (1,2,3,4),I
 1    IF (JQ.EQ.1) GO TO 7
      GO TO 5
 2    IF (IQ.EQ.NAH) GO TO 7
      GO TO 5
 3    IF (JQ.EQ.NAV) GO TO 7
      GO TO 5
 4    IF (IQ.EQ.1) GO TO 7
 5    DOT = (X-QX(I,QQ))*(QY(IU,QQ)-QY(I,QQ))
     .     -(Y-QY(I,QQ))*(QX(IU,QQ)-QX(I,QQ))
      IF (DOT+.01) 99,99,10
 7    DX = X-(QX(I,QQ)+QX(IU,QQ))/2.
      DY = Y-(QY(I,QQ)+QY(IU,QQ))/2.
      DIST = DX*DX+DY*DY
 10   CONTINUE
      RETURN
 99   DIST = 1.E20
      RETURN
      END



      SUBROUTINE SOLVE(R,A,M,N,EPS,IER)
C
      DIMENSION A(1),R(1)
      DOUBLE PRECISION R,A,PIV,TB,TOL,PIVI
      IF(M)23,23,1
C
C     SEARCH FOR GREATEST ELEMENT IN MATRIX A
    1 IER=0
      PIV=0.D0
      MM=M*M
      NM=N*M
      DO 3 L=1,MM
      TB=DABS(A(L))
      IF(TB-PIV)3,3,2
    2 PIV=TB
      I=L
    3 CONTINUE
      TOL=EPS*PIV
C     A(I) IS PIVOT ELEMENT. PIV CONTAINS THE ABSOLUTE VALUE OF A(I).
C
C
C     START ELIMINATION LOOP
      LST=1
      DO 17 K=1,M
C
C     TEST ON SINGULARITY
      IF(PIV)23,23,4
    4 IF(IER)7,5,7
    5 IF(PIV-TOL)6,6,7
    6 IER=K-1
    7 PIVI=1.D0/A(I)
      J=(I-1)/M
      I=I-J*M-K
      J=J+1-K
C     I+K IS ROW-INDEX, J+K COLUMN-INDEX OF PIVOT ELEMENT
C
C     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R
      DO 8 L=K,NM,M
      LL=L+I
      TB=PIVI*R(LL)
      R(LL)=R(L)
    8 R(L)=TB
C
C     IS ELIMINATION TERMINATED
      IF(K-M)9,18,18
C
C     COLUMN INTERCHANGE IN MATRIX A
    9 LEND=LST+M-K
      IF(J)12,12,10
   10 II=J*M
      DO 11 L=LST,LEND
      TB=A(L)
      LL=L+II
      A(L)=A(LL)
   11 A(LL)=TB
C
C     ROW INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A
   12 DO 13 L=LST,MM,M
      LL=L+I
      TB=PIVI*A(LL)
      A(LL)=A(L)
   13 A(L)=TB
C
C     SAVE COLUMN INTERCHANGE INFORMATION
      A(LST)=J
C
C     ELEMENT REDUCTION AND NEXT PIVOT SEARCH
      PIV=0.D0
      LST=LST+1
      J=0
      DO 16 II=LST,LEND
      PIVI=-A(II)
      IST=II+M
      J=J+1
      DO 15 L=IST,MM,M
      LL=L-J
      A(L)=A(L)+PIVI*A(LL)
      TB=DABS(A(L))
      IF(TB-PIV)15,15,14
   14 PIV=TB
      I=L
   15 CONTINUE
      DO 16 L=K,NM,M
      LL=L+J
   16 R(LL)=R(LL)+PIVI*R(L)
   17 LST=LST+M
C     END OF ELIMINATION LOOP
C
C
C     BACK SUBSTITUTION AND BACK INTERCHANGE
   18 IF(M-1)23,22,19
   19 IST=MM+M
      LST=M+1
      DO 21 I=2,M
      II=LST-I
      IST=IST-LST
      L=IST-M
      L=A(L)+.5D0
      DO 21 J=II,NM,M
      TB=R(J)
      LL=J
      DO 20 K=IST,MM,M
      LL=LL+1
   20 TB=TB-A(K)*R(LL)
      K=J+L
      R(J)=R(K)
   21 R(K)=TB
   22 RETURN
C
C
C     ERROR RETURN
   23 IER=-1
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polygeom.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polygeom

   To Create the build file give the command:

		$ vimake polygeom		(VMS)
   or
		% vimake polygeom		(Unix)


************************************************************************/


#define PROGRAM	polygeom
#define R2LIB

#define MODULE_LIST polygeom.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polygeom.pdf
PROCESS      HELP=*
PARM INP      TYPE=(STRING,72)
PARM OUT      TYPE=(STRING,72)
PARM PARMS    TYPE=(STRING,72) COUNT=(0:1) DEFAULT=--
PARM NAH      TYPE=INTEGER DEFAULT=0
PARM NAV      TYPE=INTEGER DEFAULT=0
PARM TIEPOINT TYPE=REAL COUNT=4:200 DEFAULT=(0,0,0,0)
PARM IGNORE   TYPE=REAL COUNT=2 DEFAULT=(0,0)
PARM SKIP     TYPE=INTEGER  DEFAULT=0
PARM INVERSE  TYPE=KEYWORD COUNT=(0:1)   VALID=(INVERSE)   DEFAULT=--
PARM INSECT   TYPE=KEYWORD COUNT=(0:1)   VALID=(INSECT)   DEFAULT=--
PARM NOPRINT  TYPE=KEYWORD COUNT=(0:1)   VALID=(NOPRINT)   DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program POLYGEOM
.HELP
PURPOSE

   polygeom  performs geometric transformations to correct  for 
   distortion  and  increase or decrease the size  of  a  point 
   polygon  data  set.   The inverse transformation allows  the 
   polygon  data  set to be transformed back  to  its  original 
   state.  polygeom is similar in format and function to GEOMA.  
   The transformed image is subdivided into a series of quadri-
   laterals.   As  in  GEOMA,  the user specifies a mapping  of 
   points  from an input image to an output  image.   Within  a   
   quadrilateral,  the  program transforms polygon points  from    
   the input image to an output image.   The control points are   
   specified  arbitrarily  without  restriction  to  the  image 
   borders.   As  in  GEOMA,  two  of the four  control  points 
   forming  corners  of any given quadrilateral  may  coincide, 
   causing the quadrilateral to degenerate into a triangle.
   Control  points are specified by line-sample coordinates  in 
   the  input  and output picture.   The internal algorithm  of 
   polygeom  is similar to GEOMA except that polygon data  sets 
   can be transfromed much more rapidly than images.   As  with 
   GEOMA, polygeom will accept its control parameters on a data 
   set  instead  of  or in addition to the  conventional  VICAR 
   parameters.   The main differences between polygeom and  the 
   other  transformative routines is that it basically operates 
   on  line defining polygons and not photo  images.   Thus  no 
   interpolation of data values is required.  polygeom also has 
   the  added capability of inverse transfrormation.   This  is 
   discussed further in the Operation section.   Since polygeom 
   follows the format of GEOMA,  much of the document is simply 
   a repeat of earlier documentation of GEOMA by J.  B. Seidman 
   dated May 16, 1974.
.PAGE
EXECUTION
TAE COMMAND LINE FORMAT

    polygeom INP=file1 OUT=file2 PARAMS or
    polygeom INP=file1 PARMS=parfile OUT=file2 PARAMS

    where

    file1                   is  the input data set containing a 
                            picture   to  be   transformed   in 
                            standard VICAR format.

    file2                   is  the output data set into  which 
                            the  transformed  picture is to  be 
                            written.
.PAGE
    parfile                 is  an optional parameter data  set 
                            containing    control   parameters.  
                            This data set must be created by the
                            routine XVPOUT. It contains keywords
                            and data for TIEPOINT, NAH and NAV
                            and can be used instead of specifying
                            these keywords in the TAE COMMAND LINE.

    PARAMS                  is   a  standard  VICAR   parameter 
                            field.   Legal parameters are given 
                            in Parameters.
.PAGE
EXAMPLES

     polygeom INP=A OUT=B NAH=1 NAV=1 TIEPOINT=(1,1,1,1,+
                                                1,101,1,11,+
                                                101,1,11,1,+
                                                101,101,11,11)

   This  example demonstrates the use of polygeom to  expand  a 
   polygon  data set by a factor of 10,  keeping the upper left 
   corner in the same place.
   
   R73 INP=RESLOC73 OUT=GEOMAPAR RESPAR=(...)
   polygeom INP=A PARMS=GEOMAPAR OUT=B IGNORE=(-99.,-99.)

   In   this  example,   the  program  RES73  writes   polygeom 
   parameters  on  a data set GEOMAPAR.   These parameters  are 
   passed  to  polygeom,  and are  concatenated  following  the 
   parameters which were entered in the VICAR parameter field.

RESTRICTIONS

   The maximum number of tiepoints is 2100.


.PAGE
OPERATION

   polygeom  transforms the coordinate definitions of  polygons 
   using three or four coefficients of transformation depending 
   on the description in the parameter file.  First the program 
   must  decide  which  coefficient to  apply.   For  this  the 
   program  must decide which quadrilateral area in the  output 
   picture the pixel lies.   Using the subroutine  INSIDE,  the 
   containing  quadrilateral is selected.   The  transformation 
   equation  is formed in the following manner.   Let the coor-   
   dinates  of  the four control points defining  that  quadri-     
   lateral     in     the    output    picture    be     called 
   [x(j),y(j),j=1,2,3,4],  and in the input picture let them be 
   called  [x'(j),y'(j),j=1,2,3,4].   (x may be considered  the 
   line  coordinate and y the sample,  although they  could  as 
   well be reversed for this discussion.)  The values of all 16 
   of  these  numbers  are part of the control  parameters  for 
   polygeom.
   Define a transformation by

                        x' = ax+by+cxy+d
                                                         (1)
                        y' = ex+fy+gxy+h

   where  the values of the coefficients a,b,...,h  are  deter-    
   mined by requiring that

                 x'(j) = ax(j)+by(j)+cx(j)y(j)+d
                                                         (2)
                 y'(j) = ex(j)+fy(j)+gx(j)y(j)+h

                 for j = 1,2,3,4

   This  is  the  condition  that  the  defined  transformation 
   exactly  maps the control point coordinates as specified  by 
   the parameters.
   The  transformation  is  used to transform  the  line-sample 
   coordinates  of the polygon point being processed  into  its 
   corresponding  coordinates in the output picture.  The input 
   and  output  coordinates are defined as four  byte  floating 
   numbers defining a polygon boundary.  In this case the pixel 
   position  is not interpreted,  the value is either there  or 
   not.

   In  order  for  polygeom to  work  properly,  the  tiepoint 
   coordinates  in  the  output picture  must  satisfy  certain 
   constraints.   They  must be organizable into a  rectangular 
   matrix of points.  That is, each tiepoint must belong to one 
   row  and one column of points,  each row must have the  same 
   number  of points as every other row,  and each column  must 
   have  the same number as every other column.   It should  be 
   possible to number the rows sequentially from top to bottom, 
   and the columns from left to right.   Quadrilaterals used by 
   the program for performing the geometric transformation  are 
   formed by connecting each tiepoint to the adjacent tiepoints 
   in the same column and in the same row.
     These  quadrilaterals  should all have the  property  that 
   they are not concave (no interior angle of any quadrilateral 
   should be greater than 180o). The number of areas vertically 
   is the number of rows of tiepoints less 1, and the number of 
   areas  horizontally  is the number of columns  of  tiepoints 
   less 1.
   Although  the  transformation in equation (1) is  very  well 
   behaved within the quadrilateral in which it applies,  there 
   is  no  assurance that it is continuous across the  boundary 
   between  adjacent  quadrilaterals;  unless the  boundary  is 
   precisely  vertical or horizontal.   The degree  of  discon-     
   tinuity   depends   on   the   details   of   the   tiepoint 
   specifications.   In some cases the discontinuity may be  so 
   small  that  the transformed picture has no visible  defect, 
   but in others it may be quite visible.

   The user has two choices to eliminate the discontinuity.  He  
   may  specify a rectilinear grid of output tiepoints,  or  he 
   may  specify tiepoints in such a way that the quadrilaterals 
   degenerate into triangles.  A quadrilateral degenerates into 
   a  triangle if two adjacent tiepoints defining it  have  the 
   same coordinates.  By suitable choice of tiepoints, the user 
   can   specify  the  coordinates  in  such  a  way  that  all 
   quadrilaterals degenerate into triangles.  Figure 2  shows a 
   sample array of tiepoints satisfying this  condition.   When 
   an  area is a triangle,  the cross terms in equation (1) are 
   eliminated and the transformation becomes

                        x' = ax+by+d
                                                         (3)
                        y' = ex+fy+h

   where  the  values  of the coefficients  are  determined  by 
   requiring that

                    x'(j) = ax(j)+by(j)+d

                    y'(j) = ex(j)+fy(j)+h                (4)

                    for j = 1,2,3

   The  values  of  j designate the  three  distinct  tiepoints 
   defining the triangle.   Because equation (4) is linear, the 
   transformation  is  guaranteed  to  be  continuous  at   the 
   triangle   boundaries  and  discontinuities  in  the  output 
   picture cannot occur.

   There  is a question about how to transform picture  samples 
   which  fall  outside  all  the  quadrilaterals  defined   by 
   parameters.   One  answer  is  to avoid  this  situation  by 
   specifying  the control point locations so that every sample 
   in the output picture falls within some quadrilateral.
   This  requires  that some control points fall exactly on  or 
   outside  the border of the output picture.   The program  is 
   not affected in any way by using control points outside  the 
   picture.

   If   some  picture  samples  do  fall  outside  the  defined 
   quadrilaterals, the program will process each such sample by 
   assigning  it to a nearby quadrilateral.   This is  done  by 
   "extending"  the boundaries of all "edge" quadrilaterals  to 
   the  picture  borders.   If the point falls in  two  polygon 
   extensions,  the  closest quadrilateral is selected based on 
   the distance of the point to the nearest center of an edge.
.PAGE
   The  user  should understand that the algorithm can lead  to 
   discontinuities  in  the processed picture  in  the  regions 
   outside  the defined quadrilaterals,  even when the  control 
   areas are triangles.
   The  method of calculating the inverse transformation is  as 
   follows.   the  forward  transformation F is assumed  to  be 
   nearly  linear.   A  linear  approximation  A  is  found  by 
   dropping the XY term from F.  Given a point p,

                      p' = Fp = Ap + C

   then
                         -1      -1
                    p = F  p' = A  (p'-C)

.PAGE
   Given  an  input  point q (in the output image  raster)  its 
   inverse point is calculated by iteration.

                          p1 = q-C

                              -1
                 P    = P  - A  (Fp -q)
                  n+1    n         n

   Convergence  is  tested  by examining  the  Fp -q  term  and 
                                                 n
   usually occurs within three iterations.

   The  method relies upon the fact that Fp -q is the error and 
                                           n
.PAGE
          -1                                 
   since F   is nearly linear, the correction is:
                    -1            -1
                  -F  (Fp -q) = -A  (Fp -q)
                         n             n

TIMING

   A   polygon   data  set  consisting  of  38000  points   was 
   transformed   in   about   four   minutes.     An    inverse 
   transformation might be several times slower.

   WRITTEN BY:             A. L. Zobrist

   COGNIZANT PROGRAMMER:   K. F. Evans

   REVISION:               1                 10 Oct 1980
   Made portable for UNIX   ...CRI...        06 MAR 95


.LEVEL1
.VARIABLE INP
Input image
.VARIABLE OUT
Output image
.VARIABLE PARMS
Optional parameter data set
.VARIABLE NAH
Number of areas horizontally
.VARIABLE NAV
Number of areas vertically
.VARIABLE TIEPOINT
Tiepoint coordinates
.VARIABLE SKIP
For skipping nominal data
.VARIABLE IGNORE
To leave (x,y) untransformed
.VARIABLE INVERSE
Inverse transformation
.VARIABLE INSECT
Trans inside param grid
.VARIABLE NOPRINT
Keyword to suppress messages
.LEVEL2
.VARIABLE INP
    INP=file1               is  the input data set containing a 
                            picture   to  be   transformed   in 
                            standard VICAR format.
.VARIABLE PARMS
    PARMS=parfile           is  an optional parameter data  set 
                            containing    control   parameters.  
                            This data set must be created by the
                            routine XVPOUT. It contains keywords
                            and data for TIEPOINT, NAH and NAV
                            and can be used instead of specifying
                            these keywords in the TAE COMMAND LINE.
.VARIABLE OUT
    OUT=file2               is  the output data set into  which 
                            the  transformed  picture is to  be 
                            written.
.VARIABLE     NAH
              NAH=nh        This keyword and associated  number 
                            specify   the   number   of   areas 
                            horizontally.    Its   meaning   is 
                            explained  in  the Method  section.  
                            nh must be an integer.
                            If defaulted, NAH is read from a 
                            tiepoint data set.
.VARIABLE     NAV
              NAV=nv        This keyword and associated  number 
                            specify   the   number   of   areas 
                            vertically.     Its    meaning   is 
                            explained  in the  Method  section.  
                            nv must be an integer.
                            If defaulted, NAV is read from a 
                            tiepoint data set.
.VARIABLE     TIEPOINT
              TIEPOINT=(NEWLINE(1),NEWSAMP(1),
                        OLDLINE(1),OLDSAMP(1),
                        NEWLINE(2), ...       )

                            This keyword and associated numbers 
                            specify   the  mapping  of  control 
                            points  between  output  and  input 
                            pictures.  This keyword must follow 
                            the  NAH  and  NAV  keywords.   The 
                            numbers  which follow  the  keyword 
                            are   in  mutiples  of  four,   one 
                            multiple of four for each tiepoint.  
                            The  numbers may be either  integer 
                            or real type.   The total number of 
                            tiepoint numbers must be

                                      4*(nh+1)*(nv+1)

                            Within  each group of four  numbers 
                            describing  a tiepoint,  the  first 
                            number  specifies  the  line  coor-         
                            dinate  of  that  tiepoint  in  the 
                            output (transformed)  picture,  the 
                            second  number specifies the sample 
                            coordinate of that tiepoint in  the 
                            output  picture,  the third  number 
                            specifies  the  line coordinate  of 
                            the tiepoint in the input  picture, 
                            and the fourth number specifies the 
                            sample  coordinate of the  tiepoint 
                            in the input picture.  The order in 
                            which  the tiepoints are  specified 
                            is   left   to   right   within   a 
                            horizontal  row of tiepoints.   The 
                            horizontal  rows of  tiepoints  are 
                            specified      in     top-to-bottom 
                            sequence. Tiepoint specification is 
                            further clarified in the  Operation 
                            section.



.VARIABLE     SKIP
         SKIP=n             This is used for skipping nominal 
                            data.  Two words of data are read
			    and then n words are skipped, etc.
			    The skipped data is carried along
			    unchanged to the output file.

.VARIABLE     IGNORE
              IGNORE=(x,y)  This  causes all occurrences of the 
                            point (x,y) to be left as is.   The 
                            default  is  to  ignore  the  point 
                            (0,0).   
.VARIABLE     INVERSE
              'INVERSE      This   keyword   calls  a   reverse 
                            transformation     procedure     to 
                            calculate  points in  the  original 
                            image  based  on the output  image.  
                            Since the input to output relation-   
                            ship  is not  exactly  linear,  the 
                            equation  is found in an  iterative 
                            process   which  approximates   the 
                            transformation   equation  used  to 
                            create the output file,  applies it 
                            to the original points,  and  tests 
                            the approximated points against the 
                            output  control points.   The  dif-     
                            ference  is  transformed  backwards 
                            and  subtracted  from the  previous 
                            point.   Through  this process  the 
                            error is reduced.   When the  error 
                            is   reduced  to  nearly  zero  the 
                            solution   is    accepted.     This 
                            iterative   process  is  relatively 
                            efficient,   converging  the  three 
                            steps for most cases.  (For further 
                            explanation   see   the   Operation 
                            section.)

.VARIABLE     INSECT
 	      'INSECT       This  keyword specifies that points 
                            are  to  be  moved  if  inside  the 
                            parameter  grid but points  outside 
                            are left as is.

.VARIABLE     NOPRINT
 	      'NOPRINT      This  keyword specifies that the
			    usual messages output to the user
			    are to be suppressed.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolygeom.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"

ibis-gen a nc=2 nr=9 datacol=(1,2) +
   data=(1,1,1,2,1,3,2,1,0,0,2,2,2,3,3,3,7,8) 'ibis-1 'row

!Try a simple doubling/tripling:
polygeom a b nah=1 nav=1 +
   tiepoint=(1,1,1,1,+
             1,3,1,2,+
	     4,1,2,1,+
	     4,3,2,2 )
ibis-list b gr1dim=2 nr=9

!..and its inverse
polygeom b c nah=1 nav=1 +
   tiepoint=(1,1,1,1,+
             1,3,1,2,+
	     4,1,2,1,+
	     4,3,2,2 )  'inverse
ibis-list c gr1dim=2 nr=9

!Same, but leave (2,2) alone
polygeom a b nah=1 nav=1 +
   tiepoint=(1,1,1,1,+
             1,3,1,2,+
	     4,1,2,1,+
	     4,3,2,2 ) ignore=(2,2)
ibis-list b gr1dim=2 nr=9

!Same, but only transform areas between 1 and 2.
polygeom a b nah=1 nav=1 +
   tiepoint=(1,1,1,1,+
             1,3,1,2,+
	     4,1,2,1,+
	     4,3,2,2 )  'insect
ibis-list b gr1dim=2 nr=9


!Try a rotation:
polygeom a b nah=1 nav=1 +
   tiepoint=(3,1,1,1,+
             1,1,1,3,+
	     3,3,3,1,+
	     1,3,3,3 )
ibis-list b gr1dim=2 nr=9

!...and its inverse
polygeom b c nah=1 nav=1 +
   tiepoint=(3,1,1,1,+
             1,1,1,3,+
	     3,3,3,1,+
	     1,3,3,3 )  'inverse
ibis-list c gr1dim=2 nr=9

end-proc
$ Return
$!#############################################################################
