$!****************************************************************************
$!
$! Build proc for MIPL module tritra
$! VPACK Version 1.9, Monday, December 07, 2009, 16:40:06
$!
$! Execute by entering:		$ @tritra
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module tritra ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tritra.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tritra.imake") .nes. ""
$   then
$      vimake tritra
$      purge tritra.bld
$   else
$      if F$SEARCH("tritra.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tritra
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tritra.bld "STD"
$   else
$      @tritra.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tritra.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tritra.com -mixed -
	-s tritra.f ztritra.c -
	-i tritra.imake -
	-t ttritra.f tztritra.c ttritra.imake ttritra.pdf tsttritra.pdf -
	-o tritra.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tritra.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE TRITRA(IND,CONV,NPH,NPV,LINE,SAMP,MODE)
C	THIS IS THE UCL VERSION OF TRITRA
C  TRANSFORM COORDINATES USING GEOMA PARAMETERS
C  CONV=GEOMA PARAMETERS STARTING AFTER 'TIEP' KEYWORD
C  NPH,NPV NUMBER POINTS HORIZONTAL,VERTICAL
C  LINE,SAMP  R*4 REPLACED BY OUTPUT VALUES
C  MODE=0 NL,NS TO OL,OS  MODE=1 OL,OS TO NL,NS
      REAL LINE,CONV(4,NPH,NPV),CORNER(2,4),POINT(2),A(16),B(4)
      CHARACTER*80 MSG1
      LOGICAL   INSIDE
C==================================================================
      II=2
      LL=1
      IITO=4
      LLTO=3
      IF(MODE.EQ.0) GO TO 5
      II=4
      LL=3
      IITO=2
      LLTO=1
5     CONTINUE
      POINT(1)=SAMP
      POINT(2)=LINE
      IND=0
      NAH=NPH-1
      NAV=NPV-1
C
C  FIND CLOSEST TIEPOINT TO LINE,SAMP
C  LOOP EACH ROW POINTS
      RMAX=1.0E+20
      SAMOLD=RMAX
      DO 90 L=1,NPV
C  LOOP EACH POINT IN ROW L
      DO 91 I=1,NPH
      SAMNEW=CONV(II,I,L)
      IF(SAMNEW.EQ.SAMOLD) GO TO 91
      R=(LINE-CONV(LL,I,L))**2+(SAMP-SAMNEW)**2
      IF(R.GT.RMAX) GO TO 92
      RMAX=R
      ISAV=I
      LSAV=L
92    SAMOLD=SAMNEW
91    CONTINUE
90    CONTINUE
C
C  SEARCH TWENTY QUADRILATERALS AROUND CLOSEST POINT
      ILEFT=ISAV-2
      IF(ILEFT.LT.1) ILEFT=1
      IRIGHT=ISAV+2
      IF(IRIGHT.GT.NAH) IRIGHT=NAH
      LTOP=LSAV-2
      IF(LTOP.LT.1) LTOP=1
      LBOT=LSAV+1
      IF(LBOT.GT.NAV) LBOT=NAV
C  AREA ROW LOOP
      DO 10 L=LTOP,LBOT
C  AREA COLUMN LOOP
      DO 20 I=ILEFT,IRIGHT
      N=1
C  LOAD UPPER LEFT CORNER
      CORNER(1,N)=CONV(II,I,L)
      CORNER(2,N)=CONV(LL,I,L)
C  CHECK IF POINTS ARE SAME
      IF(CORNER(1,N).EQ.CONV(II,I+1,L)) GO TO 40
      N=N+1
      CORNER(1,N)=CONV(II,I+1,L)
      CORNER(2,N)=CONV(LL,I+1,L)
40    CONTINUE
C  LOAD LOWER RIGHT POINT
      N=N+1
      CORNER(1,N)=CONV(II,I+1,L+1)
      CORNER(2,N)=CONV(LL,I+1,L+1)
C  CHECK IF POINTS ARE SAME
      IF(CONV(II,I,L+1).EQ.CORNER(1,N)) GO TO 41
      N=N+1
      CORNER(1,N)=CONV(II,I,L+1)
      CORNER(2,N)=CONV(LL,I,L+1)
41    CONTINUE
C  CHECK FOR DEGENERACY
      IF(N.GT.2) GO TO 43
      IND=1
      write (MSG1, 9000) I, L
9000  FORMAT('DEGENERATE QUADRILATERAL AT AH=',I3,'    AV=',I3)
      CALL XVMESSAGE(MSG1,' ')
      RETURN
43    CONTINUE
C  CHECK IF INSIDE CURRENT QUADRILATERAL
      IF(INSIDE(POINT,CORNER,N)) GO TO 50
20    CONTINUE
10    CONTINUE
C
C  POINT OUTSIDE QUADRILATERAL ARRAY - SET TO ONE OF 4 QUADS.
      I=ISAV
      L=LSAV
      IF(LINE.GT.CONV(LL,I,L)) GO TO 85
      IF(SAMP.GT.CONV(II,I,L)) GO TO 83
      I=I-1
      L=L-1
      IF(L.LT.1) L=1
      IF(I.LT.1) I=1
      GO TO 89
C  SAMP IS TO RIGHT OF ISAV
83    L=L-1
      IF(L.LT.1) L=1
      IF(I.EQ.NPH) GO TO 89
      IF(CONV(II,I,L).EQ.CONV(II,I+1,L)) I=I+1
      GO TO 89
C  LINE IS BELOW LSAV
85    IF(SAMP.GT.CONV(II,I,L)) GO TO 87
      I=I-1
      IF(I.LT.1) I=1
      GO TO 89
87    IF(I.EQ.NPH) GO TO 89
      IF(CONV(II,I,L).EQ.CONV(II,I+1,L)) I=I+1
89    IF(I.GT.NAH) I=NAH
      IF(L.GT.NAV) L=NAV
C  CHECK IF IS QUADRILATERAL OR REALLY A TRIANGLE (WHAT A PAIN)
      N=3
      IF(CONV(II,I,L).EQ.CONV(II,I+1,L)) GO TO 82
      IF(CONV(II,I,L+1).EQ.CONV(II,I+1,L+1)) GO TO 82
      N=4
82    CONTINUE
C
C  SOLVE SIMULTANEOUS EQUATIONS X,Y=A+BX+CY+DXY
50    IF(N.LT.4) GO TO 60
C  QUADRILATERALS ONLY
      DO 62 M=LLTO,IITO
      DO 61 J=1,4
61    A(J)=1.0
      A(5)=CONV(II,I,L)
      A(6)=CONV(II,I+1,L)
      A(7)=CONV(II,I,L+1)
      A(8)=CONV(II,I+1,L+1)
      A(9)=CONV(LL,I,L)
      A(10)=CONV(LL,I+1,L)
      A(11)=CONV(LL,I,L+1)
      A(12)=CONV(LL,I+1,L+1)
      A(13)=A(5)*A(9)
      A(14)=A(6)*A(10)
      A(15)=A(7)*A(11)
      A(16)=A(8)*A(12)
      B(1)=CONV(M,I,L)
      B(2)=CONV(M,I+1,L)
      B(3)=CONV(M,I,L+1)
      B(4)=CONV(M,I+1,L+1)
      CALL SIMQ(A,B,N,IND)
      IF(IND.NE.0) CALL XVMESSAGE('SIMQ MATRIX IS SINGULAR',' ')
      IF(IND.NE.0) RETURN
      IF(M.EQ.IITO) GO TO 63
      XLINE=B(1)+B(2)*SAMP+B(3)*LINE+B(4)*LINE*SAMP
      GO TO 62
63    SAMP=B(1)+B(2)*SAMP+B(3)*LINE+B(4)*LINE*SAMP
      LINE=XLINE
62    CONTINUE
      RETURN
C
C  TRIANGLES ONLY
60    DO 65 M=LLTO,IITO
      DO 66 J=1,3
66    A(J)=1.0
      A(4)=CONV(II,I,L)
      A(7)=CONV(LL,I,L)
      B(1)=CONV(M,I,L)
      IF(CONV(II,I+1,L).EQ.A(4)) GO TO 67
      A(5)=CONV(II,I+1,L)
      A(8)=CONV(LL,I+1,L)
      B(2)=CONV(M,I+1,L)
67    A(6)=CONV(II,I,L+1)
      A(9)=CONV(LL,I,L+1)
      B(3)=CONV(M,I,L+1)
      IF(CONV(II,I+1,L+1).EQ.A(6)) GO TO 68
      A(5)=CONV(II,I+1,L+1)
      A(8)=CONV(LL,I+1,L+1)
      B(2)=CONV(M,I+1,L+1)
68    CALL SIMQ(A,B,N,IND)
      IF(IND.NE.0) CALL XVMESSAGE('SIMQ MATRIX IS SINGULAR',' ')
      IF(IND.NE.0) RETURN
      IF(M.EQ.IITO) GO TO 69
      XLINE=B(1)+B(2)*SAMP+B(3)*LINE
      GO TO 65
69    SAMP=B(1)+B(2)*SAMP+B(3)*LINE
      LINE=XLINE
65    CONTINUE
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztritra.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztritra - TRANSFORM COORDINATES USING GEOMA PARAMS*/
/************************************************************************/

void ztritra(int* ind,float* conv, int nph,int npv,float* line,
	     float* samp,int mode)
{
FTN_NAME(tritra)( ind,conv,&nph,&npv,line,samp,&mode);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tritra.imake
/* Imake file for VICAR subroutine TRITRA */

#define SUBROUTINE tritra

#define MODULE_LIST tritra.f ztritra.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create ttritra.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INTEGER MODE,NPV,NPH,IND,I,J,PAR(100),SLO,SSO
      REAL LINE,SAMPLE
      REAL CONV(5000)

C  TEST1 FOR TRITRA
      NPH = 24
      NPV = 23
      IND = 0
      IPTR=0
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT', 'SA',
     .      'IO_ACT', 'SA', ' ' )
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      NL=NLI
      DO I=1,NL
         CALL XVREAD(IUNIT,CONV(I*900-900+1),STAT,'LINE',I,
     &               'NSAMPS',3600,' ')
      ENDDO
      LINE = 400.
      SAMP = 400.
      MODE = 1
      CALL PRNT(7,1,LINE,'LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL TRITRA(IND,CONV(10),NPH,NPV,LINE,SAMP,MODE)
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')

C  TEST2 FOR TRITRA

      LINE = 498.56
      SAMP = 498.59
      MODE = 0
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL TRITRA(IND,CONV(10),NPH,NPV,LINE,SAMP,MODE)
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL XVCLOSE(IUNIT,STAT,' ')

      CALL XVMESSAGE(
     . 'Repeat test cases in C to test C interface: ztritra', ' ')

      call tztritra(CONV(10) )

      return
      END

$!-----------------------------------------------------------------------------
$ create tztritra.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tztritra)(conv) 
float *conv;
{
      int nph, npv, ind, mode;
      float line, samp;
/*  ==================================================================  */

      nph = 24;
      npv = 23;
      ind = 0;
      line = 400.;
      samp = 400.;
      mode = 1;
      zprnt(7,1,&line,"LINE =.");
      zprnt(7,1,&samp," SAMP =.");
      ztritra(&ind,conv,nph,npv,&line,&samp,mode);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");

/*      TEST2 FOR TRITRA*/

      line = 498.56;
      samp = 498.59;
      mode = 0;
      zprnt(7,1,&line,"LINE =.");
      zprnt(7,1,&samp," SAMP =.");
      ztritra(&ind,conv,nph,npv,&line,&samp,mode);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");
}
$!-----------------------------------------------------------------------------
$ create ttritra.imake
/* Imake file for Test of VICAR subroutine TRITRA */

#define PROGRAM ttritra

#define MODULE_LIST ttritra.f tztritra.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create ttritra.pdf
PROCESS
PARM INP TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tsttritra.pdf
procedure
!  TO RUN ON VMS, TYPE   TSTTITRA
!  TO RUN ON UNIX MACHINE, MOVE THE TEST FILE TO THE MACHINE FROM THE VAX
!    IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
!     tsttritra DIR=dirname
!       where dirname = pathname of directory containing file with trailing / OR
!                     = "" if in current directory.
refgbl $echo
PARM  DIR TYPE=STRING DEFAULT="MIPLDISK:[MIPL.VGR]"
LOCAL INPIC   TYPE=STRING
body
let _onfail="continue"
let $echo="yes"
LET INPIC = "&DIR"//"f1636832.gpr"

TTRITRA &INPIC
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create tritra.hlp
1 TRITRA

    To convert a line, sample pair using GEOMA parameters.
    
2  CALLING SEQUENCE

  FORTRAN Calling Sequence:  CALL TRITRA(IND,CONV,NPH,NPV,LINE,SAMP,MODE)
  C Calling Sequence:        ztritra(&ind,conv,nph,npv,&line,&samp,mode);


2  ARGUMENTS

      IND         0 = normal return, 1 = abnormal condition sensed.
                  If IND is equal to 1 an explanatory message will be
                  produced.      
      CONV array  The GEOMA parameters, beginning with the first word
                  following the parameter keywords (word #9 if GEOMAV
                  was used to create them).  The declaration is
                  REAL CONV(4,NPH,NPV) or for ztritra
                  float *conv;

      NPH         The number of horizontal vertices/row       I*4
                  (nph, npv, and mode are passed by value
                  for ztritra.)

      NPV         The number of vertical vertices/column      I*4

      LINE        Line number (replaced on return)            R*4 (or float *)

      SAMP        Sample number (replaced on return)          R*4 (or float *)

      MODE        0 = New line, new sample to old line, old sample.
                  1 = Old line, old sample to new line, new sample.

      Note:       Either quadrilaterals or triangles (degenerate quadri-
                  laterals) can be processed.

2  HISTORY

      Original Programmer: J. J. Lorre, 16 June 1977
      Current Cognizant Programmer: J. J. Lorre
      Source Language: Fortran
      Ported to UNIX: Steve Pohorsky

 Revision History
  92-12-29 ...SP.... Made portable.  Added ztritra for calls from C.

2  OPERATION

     This routine will convert a line, sample pair using GEOMA parameters.
     The coordinates can be converted in either direction.


$ Return
$!#############################################################################
