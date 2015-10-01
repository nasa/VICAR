$!****************************************************************************
$!
$! Build proc for MIPL module polygen
$! VPACK Version 1.8, Tuesday, May 21, 1996, 12:01:04
$!
$! Execute by entering:		$ @polygen
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
$ write sys$output "*** module polygen ***"
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
$ write sys$output "Invalid argument given to polygen.com file -- ", primary
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
$   if F$SEARCH("polygen.imake") .nes. ""
$   then
$      vimake polygen
$      purge polygen.bld
$   else
$      if F$SEARCH("polygen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polygen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polygen.bld "STD"
$   else
$      @polygen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polygen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polygen.com -
	-s polygen.f -
	-p polygen.pdf -
	-i polygen.imake -
	-t tstpolygen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polygen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C  IBIS ROUTINE POLYGEN
C  PURPOSE:  GENERATE IBIS POLYGON FILE FROM DATA COMMANDS
C  USER PARAMETERS:
C  DATA,X1,Y1,...,XN,YN  THE POINTS XI,YI ARE INSERTED IN ORDER INTO THE
C                OUTPUT FILE.
C  MDATA,X1,Y1,A1,B1,K1,...,XM,YM,AM,BM,KM   FOR EACH FIVE-TUPLE KI POIN
C                GENERATED IN A UNIFORM STRAIGHT LINE BETWEEN XI,YI AND
C                AND INSERTED IN ORDER INTO THE OUTPUT FILE.
C
C	Revision 2:  March 1986		Frank Evans
C		Put in calls to graphics routines to fix xvopen bug
C
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      REAL	DATAARRAY(500), MDATAARRAY(500)
      REAL	DL,DS,SL,SS 
C
C---- READ PARAMETERS. OPEN FILE.
C
      CALL XVPARM ('DATA', DATAARRAY, NDATA, DATADEF,0)
      CALL XVPARM ('MDATA', MDATAARRAY, NMDATA, MDATADEF,0)
      IF (DATADEF .EQ. 1 .AND. MDATADEF .EQ. 1) GO TO 999
C
C
C--	OPEN IBIS GRAPHICS-1 FILE
      CALL WRGR (1, 1, 2)
C
C
      IF (DATADEF .EQ. 1) GO TO 5
C
C---- PROCESS "DATA".
C
      DO I = 1, NDATA, 2
	 CALL PUTGR (1, DATAARRAY(I), DATAARRAY(I+1) )
      ENDDO
C
C---- PROCESS "MDATA".
C
    5 IF (MDATADEF .EQ. 1) GO TO 6
      DO I = 1, NMDATA, 5
         NP1 = MDATAARRAY(I+4)
         DL = (MDATAARRAY(I+2)-MDATAARRAY(I))/(NP1-1)
         DS = (MDATAARRAY(I+3)-MDATAARRAY(I+1))/(NP1-1)
         SL = MDATAARRAY(I)
         SS = MDATAARRAY(I+1)
         DO J = 1, NP1
            CALL PUTGR (1, SL+(J-1)*DL, SS+(J-1)*DS)
         ENDDO
         CALL PUTGR (1, 0.0, 0.0)
      ENDDO
C
    6 CALL CLGR (1)
C
      RETURN
  999 CALL ABEND
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create polygen.pdf
PROCESS    HELP=*
PARM OUT TYPE=(STRING,72)
PARM DATA  TYPE=REAL COUNT=(1:250) DEFAULT=(0,0)
PARM MDATA TYPE=REAL COUNT=(1:250) DEFAULT=(0,0)
END-PROC
.TITLE
VICAR/IBIS Program POLYGEN
.HELP
PURPOSE
     POLYGEN is an IBIS applications program used to  create 
     an  IBIS  graphics  file  consisting  of  line  segment 
     information.   Data  input  is free format and 
     may be specified as real or integer values.  The output 
     data  file  will  consist of several  line  and  sample 
     coordinates suitable for input to any of the IBIS POLY-
     line  programs.   The size of the output file  will  be 
     512 samples by n lines.

TAE COMMAND LINE FORMAT
     POLYGEN OUT=graph PARAMS
     where
     OUT                 is an IBIS graphics file.
     PARAMS              is a standard VICAR parameter field.
.PAGE
METHOD                             

     Data  points are processed as they are  entered.   When 
     the   MDATA   parameter  is  used   data   values   are 
     interpolated  and  the line string is  terminated  with 
     0.,0.  When the DATA parameter is used, the data points 
     are transferred directly to the output file.   No 0.,0. 
     point is added.  Both parameter keywords and associated 
     data  may  be  used in a single operation  of  POLYGEN, 
     however  the keywords DATA and MDATA may  be  specified 
     only  once each.   If both keywords are specified,  the 
     data following the DATA keyword is written on the  IBIS 
     graphics file first.
.PAGE
EXAMPLE 1

     POLYGEN OUT=graph DATA=(20,30, 32,33, 40,33, 0,0,+
                     26,35, 27,35, 0,0,+
                     16,22, 22,25, 32,25, 32,22, 16,22, 0,0)

     Data  points  are  transferred directly to  the  output 
     file,  OUT,  in the order in which they are input.   In 
     this example points are set up as line segment data for 
     the IBIS POLY-line routines.
.PAGE
EXAMPLE 2

     POLYGEN OUT=graph MDATA=(35,70, 46,150,  10,
                              62,10, 65,36,   18)

     Two  line strings are to be generated in this  example.  
     The  first line string beginning at line 35 and  sample 
     70 and ending at line 46 and sample 150 will consist of 
     10 points.   The second line will consist of 18 points.  
     Both line strings will be terminated with a 0.,0.  line 
     and sample point.
.PAGE
EXAMPLE 3

     POLYGEN OUT=graph MDATA=(16,12, 38,120, 63),+
                 DATA=(22,35, 36,75, 0,0, 25,37, 26,82, 0,0)

     In  this  example both keywords  are  specified.   Data 
     following the DATA keyword are transferred directly  to 
     the  output file first.   Then a line string consisting 
     of 62 data points interpolated between line and  sample 
     points  16,12  and  18,120 follows.   Since  the  MDATA 
     parameter  has  been  specified,   the  file  will   be 
     terminated with a 0,0 point.
.PAGE

WRITTEN BY:                     H. Wilczynski, 15 March 1978
CURRENT COGNIZANT PROGRAMMER:   Frank Evans
REVISION:                       2             March 1986
.LEVEL1
.VARIABLE OUT
Output graphics file
.VARIABLE DATA
Specifies pairs (L,S) to output
.VARIABLE MDATA
Line segment filling option
.LEVEL2
.VARIABLE DATA
     DATA=(L1,S1,L2,S2,    The DATA parameter specifies that 
           ...,Ln,Sn)      a series of pixel coordinates are 
                           to be transferred directly to the 
                           graphics  file.   Any  number  of 
                           line and sample points may follow 
                           the DATA parameters.
.VARIABLE MDATA
     MDATA=(SL,SS,EL,ES,N) MDATA   specifies  that  a   line 
                           segment  filling option is to  be 
                           invoked.   A  line having N total 
                           points  is interpolated  for  the 
                           line    segment   beginning    at 
                           starting  line,  starting  sample 
                           (SL,SS)   and  ending  at  ending 
                           line,   ending  sample   (EL,ES).  
                           SL,SS and EL,ES are the endpoints 
                           of  the  output  line,   and  N-2 
                           points  are interpolated between.  
                           The   line  segment   terminating 
                           value (0.,0.) is added at the end 
                           of the line segment, consequently 
                           n+1  pairs of values are  output.  
                           Several   line  strings  may   be 
                           processed     subsequently     by 
                           repetitions  of  the  five   data 
                           values.

$ Return
$!#############################################################################
$Imake_File:
$ create polygen.imake
#define PROGRAM polygen

#define MODULE_LIST polygen.f

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$Test_File:
$ create tstpolygen.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
POLYGEN OUT=A MDATA=(10. 10. 40. 40. 10 10. 40. 40. 10. 10)
ibis-list A gr1dim=2
end-proc
$ Return
$!#############################################################################
