$!****************************************************************************
$!
$! Build proc for MIPL module xlocus
$! VPACK Version 1.8, Friday, March 24, 1995, 16:09:15
$!
$! Execute by entering:		$ @xlocus
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
$ write sys$output "*** module xlocus ***"
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
$ write sys$output "Invalid argument given to xlocus.com file -- ", primary
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
$   if F$SEARCH("xlocus.imake") .nes. ""
$   then
$      vimake xlocus
$      purge xlocus.bld
$   else
$      if F$SEARCH("xlocus.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xlocus
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xlocus.bld "STD"
$   else
$      @xlocus.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xlocus.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xlocus.com -
	-s xlocus.f -
	-i xlocus.imake -
	-p xlocus.pdf -
	-t tstxlocus.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xlocus.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 8 MAY 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
      INCLUDE 'VICMAIN_FOR'
       SUBROUTINE MAIN44
       IMPLICIT INTEGER(A-Z)
      COMMON/C1/ IN,OUT,FPAR
      REAL*4 R(2,2),O(2),IN(2,1500),OUT(2,1500),FPAR(50)
C********************
C     PARAMETER PROCESSOR HERE
C********************
      CALL IFMESSAGE('XLOCUS VERSION 8-MAY-95')
      CALL XVP('T',FPAR,CNT)
      I = 1
      R(1,1)=FPAR(I)
      R(2,1)=FPAR(I+1)
      R(1,2)=FPAR(I+2)
      R(2,2)=FPAR(I+3)
      O(1)=FPAR(I+4)
      O(2)=FPAR(I+5)
      CALL PRNT(7,1,R(1,1),'0 R(1,1)= .')
      CALL PRNT(7,1,R(2,1),'0 R(2,1)= .')
      CALL PRNT(7,1,R(1,2),'0 R(1,2)= .')
      CALL PRNT(7,1,R(2,2),'0 R(2,2)= .')
      CALL PRNT(7,1,O(1),'0 O(1)= .')
      CALL PRNT(7,1,O(2),'0 O(2)= .')
      CALL XVUNIT(IUNI,'INP',1,STAT,' ')
      CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      NP=NS/2
      CALL XVREAD(IUNI,IN,STAT,'NSAMPS',NS,' ')
      CALL PRNT(7,NP,IN,' DUMP IN.')
C********************
C     NOW APPLY MATRIX TRANSFORMATION TO IN
C********************
      NPTS=NS/2
      DO 11 I=1,NPTS
      OUT(1,I)=R(1,1)*IN(1,I)+R(1,2)*IN(2,I)+O(1)
      OUT(2,I)=R(2,1)*IN(1,I)+R(2,2)*IN(2,I)+O(2)
11    CONTINUE
      CALL PRNT(7,NP,OUT,' DUMP OUT.')
C*********************
C     OK WRITE IT OUT
C*********************
      CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
      IF(STAT.NE.1)GO TO 12
      NLO=1
      NSO=NS
      CALL XVOPEN(OUNI,STAT,'U_NL',NLO,'U_NS',NSO,
     *'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      CALL XVWRIT(OUNI,OUT,STAT,' ')
      CALL XVCLOSE(OUNI,STAT,' ')
12    CONTINUE
      CALL XVCLOSE(IUNI,STAT,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xlocus.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM xlocus

   To Create the build file give the command:

		$ vimake xlocus		(VMS)
   or
		% vimake xlocus		(Unix)


************************************************************************/


#define PROGRAM	xlocus
#define R2LIB

#define MODULE_LIST xlocus.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create xlocus.pdf
process help=*
PARM INP  TYPE=STRING
PARM OUT  TYPE=STRING
PARM T    TYPE=REAL COUNT=6
END-PROC
.TITLE
        "xlocus"
.HELP
PURPOSE:
   
     "xlocus" is a new program to apply the LOCUS rotation and
     offset transformation matrix to the GALILEO Theodolite
     data allowing one to difference (DIFFPIC) the transformed
     data with the edited STARCAT version of the CCD image of
     the GALILEO Geometric grid targat. Parameters are the
     R(11) to R(22) and O(1), O(2) elements from the locus
     output listing. It is intended primarily for use with
     the GALILEO Geometric Grid images.

EXECUTION:

        xlocus INP=ETHEO OUT=TRANS T=(R11,R21,R12,R22,O1,O2)

        where
               |Y1|  |R11 R12| |X1| |O1|
               |  |= |       |*|  |+|  |
               |Y2|  |R21 R22| |X2| |O2|

 The SIZE field is ignored.
.PAGE
OPERATION:
   
   "xlocus" applies the rotation and offset matrix as determined by LOCUS
   to the Theodolite data set which consists of the measured grid 
   intersections for the GALILEO Grid Target. The transformed grid data
   can then be differenced (DIFPIC) with a reference set as determined
   by "starcat" or "gridloca"/"gridlocb" to locate those intersections which
   are badly distorting the "locus" matrix. In this way the poorly located
   intersections can be remove from the fitting procedure which will 
   greatly reduce the global errors.

PROGRAM HISTORY:

   24 MAR  1995   A.  SCOP (CRI) Made portable for UNIX
   22 MAR  1995   S.  POHORSKY   CORRECTED PARAMETER PROCESSING; ADDED TEST.
   06 FEB  1985...M.E.MORRILL....CLEAN UP
   24 OCT  1984...M.E.MORRILL....CONVERSION TO VAX-VICAR*2
   10 MAY  1983...M.E.MORRILL....REVISIONS
   15 OCT  1982...M.E.MORRILL....INITIAL RELEASE  

CURRENT COGNIZANT PROGRAMMER:  GARY YAGI
.LEVEL1
.VARIABLE INP
 A Edited THEO file.
.VARIABLE OUT
 The Transformed
 THEO file.
.VARIABLE T
 REAL--COUNT=6
 T=(R11,R21,R12,R22,O1,O2)
        where
  |Y1|  |R11 R12| |X1| |O1|
  |  |= |       |*|  |+|  |
  |Y2|  |R21 R22| |X2| |O2|
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstxlocus.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen th 1 100 'real4
! TRY IDENTITY OPERATION ON FAKE DATA
xlocus th thought T=(1 0 0 1   0 0)
difpic (th thought)
! TRY SCALE PLUS OFFSET
xlocus th thought T=(2 0 0 2   3 3)
f2 th th2 func="2*IN1 + 3"
difpic (th2 thought)
!
end-proc
$ Return
$!#############################################################################
