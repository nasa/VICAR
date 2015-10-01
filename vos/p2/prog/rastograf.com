$!****************************************************************************
$!
$! Build proc for MIPL module rastograf
$! VPACK Version 1.8, Friday, December 23, 1994, 13:02:52
$!
$! Execute by entering:		$ @rastograf
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
$ write sys$output "*** module rastograf ***"
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
$ write sys$output "Invalid argument given to rastograf.com file -- ", primary
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
$   if F$SEARCH("rastograf.imake") .nes. ""
$   then
$      vimake rastograf
$      purge rastograf.bld
$   else
$      if F$SEARCH("rastograf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rastograf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rastograf.bld "STD"
$   else
$      @rastograf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rastograf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rastograf.com -
	-s rastograf.f -
	-i rastograf.imake -
	-p rastograf.pdf -
	-t tstrastograf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rastograf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 2 JAN 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
	INCLUDE 'VICMAIN_FOR'
C
	SUBROUTINE MAIN44
        IMPLICIT NONE
	INTEGER*2 BUF(1000000)
        INTEGER WRGR, STATUS, INUNIT, ISTAT, ISL, ISS, NL, NS, NLI, NSI
        INTEGER IDIM, ICNT, IDEF, IBG, NLPERBLK, NBLKS,LINOFFSET,CLGR,I
C
        CALL IFMESSAGE('RASTOGRAF version 2-JAN-95')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')	! open input dataset
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     *              'U_FORMAT','HALF',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLI,NSI)		! get size field
	CALL XVPARM('DIM',IDIM,ICNT,IDEF,1)	! get dimension of output
	CALL XVPARM('BACK',IBG,ICNT,IDEF,1)	! get background value
	STATUS = WRGR (1,1,IDIM)		! open ibis graphics-1 file
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C
	IF (ISL.NE.1) CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',ISL-1,' ')
	NLPERBLK = MIN(1000000/NS,NL)
	NBLKS = NL/NLPERBLK
C						! loop thru blocks of lines
	DO I=1,NBLKS
	    LINOFFSET = (I-1)*NLPERBLK
	    CALL TOVECTOR(INUNIT,BUF,NLPERBLK,NS,IBG,LINOFFSET,IDIM,
     +			  ISS)
	END DO
	NLPERBLK = NL-NBLKS*NLPERBLK		! check for extra lines
	IF (NLPERBLK .NE. 0) CALL TOVECTOR(INUNIT,BUF,NLPERBLK,NS,IBG,
     +					   LINOFFSET,IDIM,ISS)
C
	STATUS = CLGR (1)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	RETURN
	END
C***************************************************************************
	SUBROUTINE TOVECTOR(INUNIT,BUF,NL,NS,IBG,LINOFFSET,IDIM,ISS)
        IMPLICIT NONE
C
        INTEGER INUNIT, ISTAT, ISS, NL, NS, IDIM, IBG, LINOFFSET, I, J
	INTEGER*2 BUF(NS,NL)
C
	DO I=1,NL
	   CALL XVREAD(INUNIT,BUF(1,I),ISTAT,'SAMP',ISS,'NSAMPS',NS,' ')
	END DO
C					loop through all pixels, searching for
C					vector
	DO I=1,NL
	    DO J=1,NS
		IF (BUF(J,I).NE.IBG) CALL BUILDVEC(BUF,NL,NS,I,J,IBG,
     +						    LINOFFSET,IDIM)
	    END DO
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE BUILDVEC(BUF,NL,NS,LINE,ISAMP,IBG,LINOFFSET,IDIM)
C
        IMPLICIT NONE
        INTEGER STATUS, NL, NS, IDIM, IBG, LINOFFSET, LINE, ISAMP, L, M
        INTEGER PUTGR, IDN
	INTEGER*2 BUF(NS,NL)
        REAL A,B,C
	LOGICAL QFIRST
C
	QFIRST = .TRUE.
	L = LINE
	M = ISAMP
	IF (IDIM.EQ.3) GO TO 200
C					*************************************
C					Processing for 2-D output starts here
C					*************************************
  100	CONTINUE
	A = L+LINOFFSET			! Output the current vertex
	B = M
	STATUS = PUTGR(1,A,B,0.0)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C					The following tests look at each of
C					the neighboring 8 pixels, trying to
C					find a new vector to follow. If it
C					finds one, it follows it, erasing the
C					vector as it goes.
	IF (     M.LT.NS .AND.               BUF(M+1,L).NE. IBG) THEN
	    DO WHILE (M.LT.NS .AND. BUF(M+1,L).NE.IBG)
		BUF(M+1,L) = IBG
		M = M+1
	    END DO
	ELSE IF (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).NE.IBG) THEN
	    DO WHILE (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).NE.IBG)
		BUF(M+1,L+1) = IBG
		M = M+1
		L = L+1
	    END DO
	ELSE IF (	    L.LT.NL .AND. BUF(M  ,L+1).NE.IBG) THEN
	    DO WHILE (L.LT.NL .AND. BUF(M,L+1).NE.IBG)
		BUF(M,L+1) = IBG
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).NE.IBG) THEN
	    DO WHILE (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).NE.IBG)
		BUF(M-1,L+1) = IBG
		M = M-1
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. 		     BUF(M-1,L  ).NE.IBG) THEN
	    DO WHILE (M.GT.1 .AND. BUF(M-1,L).NE.IBG)
		BUF(M-1,L) = IBG
		M = M-1
	    END DO
	ELSE IF (M.GT.1  .AND. L.GT.1  .AND. BUF(M-1,L-1).NE.IBG) THEN
	    DO WHILE (M.GT.1 .AND. L.GT.1  .AND. BUF(M-1,L-1).NE.IBG)
		BUF(M-1,L-1) = IBG
		M = M-1
		L = L-1
	    END DO
	ELSE IF (	       L.GT.1  .AND. BUF(M  ,L-1).NE.IBG) THEN
	    DO WHILE (L.GT.1 .AND. BUF(M,L-1).NE.IBG)
		BUF(M,L-1) = IBG
		L = L-1
	    END DO
	ELSE IF (M.LT.NS .AND. L.GT.1  .AND. BUF(M+1,L-1).NE.IBG) THEN
	    DO WHILE (M.LT.NS .AND. L.GT.1 .AND. BUF(M+1,L-1).NE.IBG)
		BUF(M+1,L-1) = IBG
		M = M+1
		L = L-1
	    END DO
	ELSE				! finished
	    IF (QFIRST) THEN
		A = L+LINOFFSET			! Output the first vertex again
		B = M
		STATUS = PUTGR(1,A,B,0.0)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    END IF
	    STATUS = PUTGR(1,0.0,0.0,0.0)	! end of vector set; send
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    RETURN				! terminator and return
	END IF
C
	QFIRST = .FALSE.
	GO TO 100
C
C
C					*************************************
C					Processing for 3-D output starts here
C					*************************************
  200	CONTINUE
	IDN = BUF(M,L)
	C = IDN
  300	CONTINUE
	A = L+LINOFFSET			! Output the current vertex
	B = M
	STATUS = PUTGR(1,A,B,C)
        IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
C					The following tests look at each of
C					the neighboring 8 pixels, trying to
C					find a new vector to follow. If it
C					finds one, it follows it, erasing the
C					vector as it goes.
	IF      (M.LT.NS .AND. 		     BUF(M+1,L  ).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. BUF(M+1,L).EQ.IDN)
		BUF(M+1,L) = IBG
		M = M+1
	    END DO
	ELSE IF (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. L.LT.NL .AND. BUF(M+1,L+1).EQ.IDN)
		BUF(M+1,L+1) = IBG
		M = M+1
		L = L+1
	    END DO
	ELSE IF (	       L.LT.NL .AND. BUF(M  ,L+1).EQ.IDN) THEN
	    DO WHILE (L.LT.NL .AND. BUF(M,L+1).EQ.IDN)
		BUF(M,L+1) = IBG
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).EQ.IDN) THEN
	    DO WHILE (M.GT.1  .AND. L.LT.NL .AND. BUF(M-1,L+1).EQ.IDN)
		BUF(M-1,L+1) = IBG
		M = M-1
		L = L+1
	    END DO
	ELSE IF (M.GT.1  .AND. 		     BUF(M-1,L  ).EQ.IDN) THEN
	    DO WHILE (M.GT.1 .AND. BUF(M-1,L).EQ.IDN)
		BUF(M-1,L) = IBG
		M = M-1
	    END DO
	ELSE IF (M.GT.1  .AND. L.GT.1  .AND. BUF(M-1,L-1).EQ.IDN) THEN
	    DO WHILE (M.GT.1 .AND. L.GT.1  .AND. BUF(M-1,L-1).EQ.IDN)
		BUF(M-1,L-1) = IBG
		M = M-1
		L = L-1
	    END DO
	ELSE IF (	       L.GT.1  .AND. BUF(M  ,L-1).EQ.IDN) THEN
	    DO WHILE (L.GT.1 .AND. BUF(M,L-1).EQ.IDN)
		BUF(M,L-1) = IBG
		L = L-1
	    END DO
	ELSE IF (M.LT.NS .AND. L.GT.1  .AND. BUF(M+1,L-1).EQ.IDN) THEN
	    DO WHILE (M.LT.NS .AND. L.GT.1 .AND. BUF(M+1,L-1).EQ.IDN)
		BUF(M+1,L-1) = IBG
		M = M+1
		L = L-1
	    END DO
	ELSE				! finished
	    IF (QFIRST) THEN
		A = L+LINOFFSET			! Output the first vertex again
		B = M
		STATUS = PUTGR(1,A,B,C)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    END IF
	    STATUS = PUTGR(1,0.0,0.0,0.0)	! end of vector set; send
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    RETURN				! terminator and return
	END IF
C
	QFIRST = .FALSE.
	GO TO 300
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rastograf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM rastograf

   To Create the build file give the command:

		$ vimake rastograf			(VMS)
   or
		% vimake rastograf			(Unix)


************************************************************************/


#define PROGRAM	rastograf
#define R2LIB

#define MODULE_LIST rastograf.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create rastograf.pdf
PROCESS    HELP=*
PARM INP 	TYPE=(STRING,72)
PARM OUT 	TYPE=(STRING,72)
PARM SIZE 	INTEGER DEFAULT=--	COUNT=0:4 
PARM SL 	INTEGER DEFAULT=1
PARM SS		INTEGER DEFAULT=1
PARM NL 	INTEGER DEFAULT=0
PARM NS 	INTEGER DEFAULT=0
PARM DIM	INTEGER DEFAULT=2 VALID=(2:3)
PARM BACK	INTEGER DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program RASTOGRAF
.HELP
PURPOSE
     RASTOGRAF is a program used to convert from a line image in raster format
into an IBIS Graphics I file consisting of line segment information. All pixels 
not equal to the BACKground value are considered as parts of lines and are 
included as vectors. For 3-D files, the third value is the DN of the line.

WRITTEN BY:                     Ron Alley, 10 May, 1988
CURRENT COGNIZANT PROGRAMMER:   Ron Alley
REVISION:                       Original
       Made portable for UNIX   AMS (CRI)   2 Jan, 1995
.LEVEL1
.VARIABLE INP
Input raster image
.VARIABLE OUT
Output Graphics I file
.VARIABLE SIZE
VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE DIM
Dimension of the graphics file
.VARIABLE BACK
DN value of background pixels
.LEVEL2
.VARIABLE INP
The input image, which is a standard VICAR raster image. In the image, pixel
values not equal to the BACKground value are considered lines, and are 
converted into vectors. 
.VARIABLE OUT
Output Graphics I file.
.VARIABLE SIZE
The size parameter determines the boundaries in the input
file from which the copy is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.VARIABLE SL
Starting line, the first image line to be examined for output vectors.
.VARIABLE SS
Starting sample, the leftmost image pixel to be examined for output vectors.
.VARIABLE NL
Number of lines to be examined for output vectors.
.VARIABLE NS
Number of pixels per line to be examined for output vectors.
.VARIABLE DIM
DIM specifies the number of dimensions of the output graphics file, either
2 or 3.  If DIM=3, the third dimension is used to store the DN of the line.
.VARIABLE BACK
BACK specifies the DN of background pixels. All pixels that are not of this
DN are included as part of an output vector.
$ Return
$!#############################################################################
$Test_File:
$ create tstrastograf.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! Prepare test file
gen rtgtest1 15 15 ival=0 linc=0 sinc=0
! Note this test heavily uses the program ADL.  As of 13 July 1994,
! ADL has not yet been ported.
adl rtgtest1 rtgtest2 add=(3,2,10,10,10, 3,8,3,11,6, 1,14,2,14,10)
adl rtgtest2 rtgtest3 add=(1,2,3,8,7, 2,7,5,7,12, 2,6,3,3,6)
adl rtgtest3 rtgtest4 add=(-1,7,6,7,6, -3,7,10,7,10)
!
! Display test file
!
list rtgtest4
! Make another copy of test file with EVERY element just offset by +1
f2 inp=rtgtest4 out=rtgtest5 function="IN1+1"
!
! Run RASTOGRAF  with 2 dimensions, then list the resulting IBIS file
! uses size parameter to look only at segments in upper left quadrant
! Not including DIM should make it default to 2.  Not including BACK
! should default it to 0.
rastograf rtgtest4 testgraf size=(1,1,7,7)
ibis-list testgraf gr1dim=2 nr=15
let $echo="no"
write "You should have seen:"
write "Rows: 1:15"
write " ------------- --------------"
write " C:1           C:2          "
write " ------------- --------------"
write " 2             3             "
write " 4             5             "
write " 5             5             "
write " 7             7             "
write " 7             5             "
write " 0             0             "
write " 3             6             "
write " 3             6             "
write " 0             0             "
write " 4             4             "
write " "
write " 5             4             "
write " 6             3             "
write " 0             0             "
write " 0             0             "
write " 0             0             "
write ""
let $echo="yes"
!
! Run RASTOGRAF with 3 dimensions, then list the resulting IBIS file
! This time, I will specify the window to contain all of my line
! segments using the sl/ss nl/ns parameters.  Also, I am testing the
! BACK(ground) parameter by using a non-zero value.
rastograf rtgtest5 testgraf sl=2 ss=1 nl=11 ns=13 dim=3 back=1
ibis-list testgraf gr1dim=3 nr=25
let $echo="no"
write "You should have seen:"
write "Rows: 1:25"
write " ------------- ------------- --------------"
write " C:1           C:2           C:3          "
write " ------------- ------------- --------------"
write " 1             3             2             "
write " 2             4             2             "
write " 3             4             2             "
write " 5             6             2             "
write " 0             0             0             "
write " 1             10            4             "
write " 5             10            4             "
write " 0             0             0             "
write " 2             6             3             "
write " 5             3             3             "
write " "
write " 0             0             0             "
write " 6             5             3             "
write " 6             12            3             "
write " 0             0             0             "
write " 7             3             4             "
write " 10            6             4             "
write " 0             0             0             "
write " 7             7             2             "
write " 7             7             2             "
write " 0             0             0             "
write " "
write " 7             10            4             "
write " 9             10            4             "
write " 0             0             0             "
write " 0             0             0             "
write " 0             0             0             "

!
end-proc
$ Return
$!#############################################################################
