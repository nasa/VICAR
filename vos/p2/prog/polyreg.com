$!****************************************************************************
$!
$! Build proc for MIPL module polyreg
$! VPACK Version 1.8, Thursday, March 02, 1995, 15:59:01
$!
$! Execute by entering:		$ @polyreg
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
$ write sys$output "*** module polyreg ***"
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
$ write sys$output "Invalid argument given to polyreg.com file -- ", primary
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
$   if F$SEARCH("polyreg.imake") .nes. ""
$   then
$      vimake polyreg
$      purge polyreg.bld
$   else
$      if F$SEARCH("polyreg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polyreg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polyreg.bld "STD"
$   else
$      @polyreg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polyreg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polyreg.com -
	-s polyreg.f -
	-i polyreg.imake -
	-p polyreg.pdf -
	-t tstpolyreg.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polyreg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  IBIS ROUTINE POLYREG
C
C  PURPOSE:  POLYREG PERFORMS A RIGID TRANSFORMATION OF A POLYGON 
C  CORRECT FOR DIFFERENCES IN SIZE, ROTATION, ASPECT, OR SKEW.  TH
C  IS MOST EASILY SPECIFIED BY GIVING THREE INPUT TIEPOINTS AND TH
C  PONDING OUTPUT TIEPOINTS.  ALTERNATIVELY, THE USER CAN SPECIFY 
C  PAIRS AND GIVE ADDITIONAL INFORMATION ON ASPECT RATIOS, AND COO
C  REVERSAL.
C  USER PARAMETERS:
C
C
C  ITIE,X1,Y1,X2,Y2 - SPECIFIES TWO TIEPOINTS IN THE INPUT PICTURE
C		NUMBERS MUST BE IN FLOATING POINT FORMAT.
C  ITIE,X1,Y1,X2,Y2,X3,Y3 - SPECIFIES THREE POINTS IN THE INPUT PI
C  OTIE,X1,Y1,X2,Y2 - SPECIFIES TWO TIEPOINTS IN THE OUTPUT PICTUR
C  OTIE,X1,Y1,X2,Y2,X3,Y3 - SPECIFIES THREE POINTS IN THE OUTPUT P
C  IUSIZE,R,S - GIVES THE ASPECT RATIO (R/S) OF THE INPUT COORDINA
C		R IS THE SIZE OF ONE UNIT IN THE FIRST COORDINATE, S 
C		OF ONE UNIT IN THE SECOND COORDINATE.  THIS PARAMETER
C		WHEN ONLY TWO TIEPOINTS ARE PROVIDED.
C  OUSIZE,R,S - GIVES THE ASPECT RATIO OF THE OUTPUT COORDINATE SY
C  FLIP -	USED FOR COORDINATE REVERSAL IN THE TWO POINT CASE.
C  IGNORE,X,Y - CAUSES ALL POINTS WITH THE VALUE X,Y TO BE IGNORED
C		TRANSFORMATION.  THE DEFAULT IS TO IGNORE 0.,0.
C
C   1-87  SXP   Modified to use DGELG to solve linear system of equations.
C   3-95  AS    (CRI) MSTP S/W CONVERSION (VICAR PORTING)

	IMPLICIT NONE
	LOGICAL FLIP, XVPTST, NOPRINT, ZERO, EOF
	REAL*8 TAB(6),WORK(6,6),IH,IW,OH,OW,L,S,R1,R2,THETA1,THETA2
        REAL*8 RATIO,V51,V52,ALPHA,R,XIGNOR,YIGNOR,T,L1,S1,THETA
	REAL*4 ITIARR(6),OTIARR(6),IUSARR(2),OUSARR(2),IGNARR(2)
	REAL*4 COORD1, COORD2, EXTRA(40)
        INTEGER NITIE,NOTIE,ITIEDF,OTIEDF,COUNT,IUSIDF,OUSIDF,SKIP
        INTEGER I,J,IER,STATUS,RDGR,WRGR,GETGR,PUTGR,CLGR,PRINTCOUNT
	CHARACTER*60	STRING

      DATA IH,IW,OH,OW/4*1.D0/

C
C     GET PARAMETERS
C
        CALL IFMESSAGE('POLYREG version 6-MAR-95')

	CALL XVPARM ('ITIE',ITIARR,NITIE,ITIEDF,6)
	CALL XVPARM ('OTIE',OTIARR,NOTIE,OTIEDF,6)
	CALL XVPARM ('IUSIZE',IUSARR,COUNT,IUSIDF,2)
	CALL XVPARM ('OUSIZE',OUSARR,COUNT,OUSIDF,2)
	CALL XVP ('SKIP', SKIP, COUNT)
	CALL XVP ('IGNORE', IGNARR, COUNT)
	NOPRINT = XVPTST('NOPRINT')
	FLIP = XVPTST ('FLIP')
	XIGNOR = DBLE(IGNARR(1))
	YIGNOR = DBLE(IGNARR(2))


C		 SET UP LINEAR EQUATIONS FOR SOLUTION

	IF (NITIE .NE. NOTIE) THEN
	    CALL MABEND('NUMBER OF ITIE MUST EQUAL NUMBER OF OTIE')
	ENDIF

	IF (IUSIDF.EQ.0) THEN
	    IH = DBLE(IUSARR(1))
	    IW = DBLE(IUSARR(2))
	ENDIF
	IF (OUSIDF.EQ.0) THEN
	    OH = DBLE(OUSARR(1))
	    OW = DBLE(OUSARR(2))
	ENDIF

	DO I=1,4
	    TAB(I) = DBLE(OTIARR(I))
	ENDDO
	IF (NOTIE.EQ.6) THEN
	    TAB(5) = DBLE(OTIARR(5))
	    TAB(6) = DBLE(OTIARR(6))
	ENDIF
	DO I=1,6
	    DO J=1,6
	        WORK(I,J) = 0.D0
	    ENDDO
	ENDDO
	WORK(1,1) = DBLE(ITIARR(1))
	WORK(1,2) = DBLE(ITIARR(2))
	WORK(3,1) = DBLE(ITIARR(3))
	WORK(3,2) = DBLE(ITIARR(4))
	WORK(2,3) = WORK(1,1)
	WORK(2,4) = WORK(1,2)
	WORK(4,3) = WORK(3,1)
	WORK(4,4) = WORK(3,2)
	WORK(1,5) = 1.D0
	WORK(2,6) = 1.D0
	WORK(3,5) = 1.D0
	WORK(4,6) = 1.D0
	IF (NITIE.EQ.6) THEN
	    WORK(5,5) = 1.D0
	    WORK(6,6) = 1.D0
	    WORK(5,1) = DBLE(ITIARR(5))
	    WORK(5,2) = DBLE(ITIARR(6))
	ENDIF
	WORK(6,3) = WORK(5,1)
	WORK(6,4) = WORK(5,2)
C  
C  SPECIAL CASE FOR TWO POINT POLYREG
C
	IF (NITIE .NE. 6) THEN
	    L = (WORK(3,1)-WORK(1,1))*IH
	    S = (WORK(3,2)-WORK(1,2))*IW
	    IF (FLIP) THEN
	      T = L
	      L = S
	      S = T
	    ENDIF
	    R1 = DSQRT(L*L+S*S)
	    THETA1 = DATAN2(L,S)
	    L = (TAB(3)-TAB(1))*OH
	    S = (TAB(4)-TAB(2))*OW
	    R2 = DSQRT(L*L+S*S)
	    THETA2 = DATAN2(L,S)
	    THETA = THETA2-THETA1
	    RATIO = R2/R1
	    V51 = WORK(1,1)-WORK(3,2)+WORK(1,2)
	    V52 = WORK(1,2)+WORK(3,1)-WORK(1,1)
	    ALPHA = DATAN2(V51,V52)+THETA
	    R = DSQRT(V51*V51+V52*V52)*RATIO
	    TAB(5) = R*DSIN(ALPHA)/OH
	    TAB(6) = R*DCOS(ALPHA)/OW
	    IF (FLIP) THEN
	      T = V51
	      V51 = V52
	      V52 = T
	    ENDIF
	    WORK(5,1) = V51/IH
	    WORK(5,2) = V52/IW
	    WORK(6,3) = WORK(5,1)
	    WORK(6,4) = WORK(5,2)
	ENDIF


	CALL DGELG (TAB,WORK,6,1,1.E-14,IER)
	IF (IER .NE. 0) THEN
	    CALL MABEND('SOLUTION FAILURE')
	ENDIF


C  OPEN THE INPUT AND OUTPUT GRAPHICS FILES

	STATUS = RDGR (1, 1, 2+SKIP)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	STATUS = WRGR (1, 2, 2+SKIP)
        IF (STATUS .NE. 1) CALL SIGNALGR(2,STATUS,1)


	PRINTCOUNT = 0
	IF (.NOT. NOPRINT) CALL XVMESSAGE 
     *     ('       INPUT COORDINATES           OUTPUT COORDINATES',' ')

C  TRANSFORM POINTS ACCORDING TO THE SOLUTION OF THE LINEAR EQUATION

	DO WHILE (.TRUE.)
	    STATUS = GETGR (1, ZERO, EOF, COORD1, COORD2, EXTRA)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    IF (EOF) GOTO 290
	    L = DBLE(COORD1)
	    S = DBLE(COORD2)
	    IF (DABS(L-XIGNOR)+DABS(S-YIGNOR).GT.1.D-6) THEN
		L1 = L*TAB(1)+S*TAB(2)+TAB(5)
		S1 = L*TAB(3)+S*TAB(4)+TAB(6)
		COORD1 = SNGL(L1)
		COORD2 = SNGL(S1)
	    ENDIF
	    STATUS = PUTGR (2, COORD1, COORD2, EXTRA)
            IF (STATUS .NE. 1) CALL SIGNALGR(2,STATUS,1)

	    IF (.NOT. NOPRINT .AND. PRINTCOUNT .LT. 5) THEN
		WRITE (STRING, '(F12.3,1X,F12.3,4X,F12.3,1X,F12.3)')
     *				 L,S, COORD1, COORD2
		CALL XVMESSAGE (STRING,' ')
		PRINTCOUNT = PRINTCOUNT + 1
	    ENDIF
	ENDDO

290	CONTINUE
	STATUS = CLGR (1)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	STATUS = CLGR (2)
        IF (STATUS .NE. 1) CALL SIGNALGR(2,STATUS,1)

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polyreg.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polyreg

   To Create the build file give the command:

		$ vimake polyreg			(VMS)
   or
		% vimake polyreg			(Unix)


************************************************************************/


#define PROGRAM	polyreg
#define R2LIB

#define MODULE_LIST polyreg.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polyreg.pdf
PROCESS      HELP=*
PARM INP TYPE=(STRING)
PARM OUT TYPE=(STRING)
PARM ITIE TYPE=REAL COUNT=(4:6)
PARM OTIE TYPE=REAL COUNT=(4:6)
PARM IUSIZE TYPE=REAL COUNT=2 DEFAULT=(0.,0.)
PARM OUSIZE TYPE=REAL COUNT=2 DEFAULT=(0.,0.)
PARM SKIP TYPE=INTEGER DEFAULT=0
PARM IGNORE TYPE=REAL COUNT=2 DEFAULT=(0.,0.)
PARM FLIP TYPE=KEYWORD COUNT=(0:1) VALID=(FLIP) DEFAULT=--
PARM NOPRINT TYPE=KEYWORD COUNT=(0:1) VALID=(NOPRINT) DEFAULT=--
PARM NOARCLEN  TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--      VALID=NOARCLEN
END-PROC
.TITLE
VICAR/IBIS Program "polyreg"
.HELP
PURPOSE

     "polyreg"  performs a rigid transformation of  a  polygon 
     data set to correct for differences in size,  rotation, 
     aspect,   or  skew.   The  correction  is  most  easily 
     specified  by  giving three input tiepoints  and  their 
     corresponding  output  tiepoints.   Alternatively,  the 
     user can specify two tiepoint pairs and give additional 
     information on aspect ratios,  and whether the polygons 
     are to be flipped over.
.PAGE
EXEC STATEMENT FORMAT

     polyreg INP=A OUT=B PARAMS

     where

     INP                 is  the  input data set  containing 
                         the  polygons to be transformed  in 
                         standard VICAR format.
     OUT                 is  the output data set into  which 
                         the  transformed data set is to  be 
                         written.
     PARAMS              is   a  standard  VICAR   parameter 
                         field.   
.PAGE
OPERATION

     "polyreg"  performs  a  linear  transformation  which is then
     applied to the input data set to yield the output data set.

EXAMPLES
       
  1. polyreg INP=LINES OUT=OUT ITIE=(1.,1.,1.,100.,100.,100.)+
                               OTIE=(1.,100.,100.,100.,100.,1.)
                                               o
     This  rotates the polygon file LINES by 90  and  writes 
     the result in OUT.
.PAGE
Original Programmer:  A. L. Zobrist, 10 June 1976

Current Cognizant Programmer:  K. F. Evans

Revision:  3               		    April 1986
Revision:  4        Made portable for UNIX  March 1995   A. Scop (CRI)

.LEVEL1
.VARIABLE INP
Input IBIS graphics-1 file
.VARIABLE OUT
Output IBIS graphics-1 file
.VARIABLE ITIE
Input tiepoints
.VARIABLE OTIE
Output tiepoints
.VARIABLE IUSIZE
Input aspect ratio
.VARIABLE OUSIZE
Output aspect ratio
.VARIABLE SKIP
To skip nominal data
.VARIABLE IGNORE
Ignore specified data
.VARIABLE FLIP
Indicates coordinate reversal
.VARIABLE NOPRINT
Supress program messages

.LEVEL2
.VARIABLE INP
   INP=A                      Input IBIS graphics-1 file
.VARIABLE OUT
   OUT=B                      Output IBIS graphics-1 file
.VARIABLE ITIE
   ITIE=(x1,y1,x2,y2)         specifies two tiepoints in the 
                              input.

   ITIE=(x1,y1,x2,y2,x3,y3)   specifies  three tiepoints  in 
                              the input.
.VARIABLE OTIE
   OTIE=(x1,y1,x2,y2)         specifies two tiepoints in the 
                              output.

   OTIE=(x1,y1,x2,y2,x3,y3)   specifies  three tiepoints  in 
                              the output.
.VARIABLE IUSIZE
   IUSIZE=(r,s)               gives  the aspect ratio  (r/s) 
                              of  the input coordinate  sys-
                              tem.   r  is  the size of  one 
                              unit in the first  coordinate, 
                              s  is the size of one unit  in 
                              the  second coordinate.   This 
                              parameter  is used  when  only 
                              two tiepoints are provided.
.VARIABLE OUSIZE
   OUSIZE=(r,s)               gives  the aspect ratio  (r/s) 
                              of  the output coordinate sys
                              tem.   Note:   r  and  s  must 
                              correspond  to r and s  following
                              the  IUSIZE   parameter.  
                              This   parameter  is  used  in 
                              conjunction with IUSIZE and  a 
                              twopoint "polyreg".
.VARIABLE SKIP
     SKIP=n                   Used for skipping nominal data.
			      Two data words are read and then
			      n words are skipped, etc.
			      The skipped data is carried along
			      unchanged to the output file.

.VARIABLE IGNORE
   IGNORE=(x,y)               causes  all  points  with  the 
                              value  (x,y) to be ignored  by 
                              the  transformation.  The  de-
                              fault is to ignore (0.,0.).   
.VARIABLE FLIP
     'FLIP                    Used  for coordinate  reversal 
                              (if  necessary) when two  tie
                              points are given.  For example 
                              LONG-LAT to LINE-SAMPLE or y-x 
                              to LINE-SAMPLE.

.VARIABLE NOPRINT
     'NOPRINT		      Suppresses program messages to
			      the screen.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolyreg.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!   TEST FOR POLYREG
!
ibis-gen  A NC=2 NR=15
mf        A FUNCTION=("C1=INDEX","C2=C1*C1")
ibis-list  A NC=2 NR=15
polyreg A  B    ITIE=(1,1, 1,100, 100,100)  OTIE=(100,-50, 50,150, 220,100)
ibis-list  B NC=2 NR=15 GR1DIM=2
!
end-proc
$ Return
$!#############################################################################
