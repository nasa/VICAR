$!****************************************************************************
$!
$! Build proc for MIPL module pgrid
$! VPACK Version 1.7, Friday, April 29, 1994, 11:59:08
$!
$! Execute by entering:		$ @pgrid
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
$ write sys$output "*** module pgrid ***"
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
$ write sys$output "Invalid argument given to pgrid.com file -- ", primary
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
$   if F$SEARCH("pgrid.imake") .nes. ""
$   then
$      vimake pgrid
$      purge pgrid.bld
$   else
$      if F$SEARCH("pgrid.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pgrid
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pgrid.bld "STD"
$   else
$      @pgrid.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pgrid.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pgrid.com -
	-s pgrid.f zpgrid.c -
	-i pgrid.imake -
	-t tpgrid.f tzpgrid.c tpgrid.imake tpgrid.pdf tstpgrid.pdf -
	-o pgrid.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pgrid.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE PGRID(X,NR,NC,TRIX,MODE)

C          ROUTINE TO LIST GRID COORDINATES

      REAL*4 X(2,1)
      INTEGER*2 TRIX(*)
      CHARACTER*132 PBUF,QBUF,RBUF,SBUF
C
C
      JJJ = 0
C          PAGE LOOP
    5 JJ = JJJ
      IF (JJ .NE. 0) CALL XVMESSAGE(' ',' ')
      NCO = MIN0(NC-JJ,20)
      SBUF(1:132) = ' '
      PBUF(1:132) = ' '
      QBUF(1:132) = ' '
      RBUF(1:132) = ' '
C
      DO 20 M = 1, NR
         I = 3
         J = JJ
C
         CALL XVMESSAGE(' ',' ')
         DO 10 N = 1, NCO
            I = I + 6
            J = J + 1
            WRITE (SBUF(I-1-3:I-1),'(I4)') J
            WRITE (PBUF(I-5:I),'(F6.1)') X(1,J)
            WRITE (QBUF(I-5:I),'(F6.1)') X(2,J)
            IF (MODE.EQ.1) WRITE (RBUF(I-1-2:I-1),'(I3)') TRIX(J)
   10    CONTINUE
C
         CALL XVMESSAGE(SBUF,' ')
         CALL XVMESSAGE(PBUF,' ')
         CALL XVMESSAGE(QBUF,' ')
         IF(MODE.EQ.1) CALL XVMESSAGE(RBUF,' ')
   20 JJ = JJ + NC
C
      JJJ = JJJ + 20
      IF (JJJ .LT. NC) GOTO 5
C
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpgrid.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the C-Callable portion for the PGRID subroutine  */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zpgrid(loc,nr,nc,trix,mode)
int nr, nc, mode;
short int trix;
float *loc;
{
     FTN_NAME(pgrid)(loc,&nr,&nc,&trix,&mode);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pgrid.imake
/* Imake file for VICAR subroutine PGRID */

#define SUBROUTINE pgrid

#define MODULE_LIST pgrid.f zpgrid.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tpgrid.f
C  THIS IS A PROGRAM THAT WILL TEST THE TPGRID SUBROUINE
C

      SUBROUTINE TPGRID()

      IMPLICIT INTEGER(A-Z)
      REAL LOC(2,1000)
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')
      CALL XVSIZE(S1,SS,NL,NS,NLI,NSI,' ')
      NLOC = MAX0(NSI,NS)/2
      DO 10 I=1,NLI
      CALL XVREAD(INUNIT,LOC(1,I),STATUS,'NSAMPS',NSI,' ')
   10 CONTINUE
      NC = 10
      NR = 2
      CALL PGRID(LOC,NR,NC,TRIX,0)
      CALL XVMESSAGE(' ',' ')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzpgrid.c
/*  This is a program that will test the C-Callable PGRID  */
/*  Subroutine.                                            */


#include "vicmain_c"
#include "ftnbridge.h"

main44()
{

     int inunit, status, s1, ss, nl, nli, nsi;
     int i,nloc, nc, nr, ns, mode, max;
     short int trix;
     float loc[1000][2];

     zveaction("SA"," ");
     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     status = zvunit(&inunit,"INP",1,0);
     status = zvopen(inunit,"OP", "READ","OPEN_ACT","SA",0);

     zvsize(&s1,&ss,&nl,&ns,&nli,&nsi,0);
    
     if (nli >= nl) 
          max = nli;
     else
          max = nl;
     nloc=max/2;

     for (i=0; i<nli; i++)
     {
          status = zvread(inunit,&loc[i][0],"nsamps",nsi,0);          
     }
     nr = 2;
     nc = 10;

     trix = 0;

     zpgrid(loc,nr,nc,trix,0);

     status = zvclose(inunit,0);
     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tpgrid)();  
}
$!-----------------------------------------------------------------------------
$ create tpgrid.imake
/* Imake file for Test of VICAR subroutine PGRID */

#define PROGRAM tpgrid

#define MODULE_LIST tpgrid.f tzpgrid.c

#define MAIN_LANG_C 
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tpgrid.pdf
process
parm inp string
end-proc
$!-----------------------------------------------------------------------------
$ create tstpgrid.pdf
procedure help=*
refgbl $echo
refgbl $syschar
parm inp string count=0:1 default=--
body
let _onfail="continue"
let $echo="yes"

if ($count(inp)=0) let inp = "testimage.pgrid"
gen out=&inp nl=20 ns=2 ival=2 sinc=3 linc=2 format=real

tpgrid inp=&inp

if ($syschar(1)="UNIX")
   ush rm testimage.pgrid 
else
   dcl delete/log testimage.pgrid;* 
end-if
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create pgrid.hlp
1 PGRID

        Subroutine to print out grid coordinate information.

  Calling Sequence: 
		CALL PGRID(X,NR,NC,TRIX,MODE)

  Arguments:
		X  (input): Array containing grid coordinate information.
		NR (input): Number of rows to be written out.
		NC (input): Number of coordinates to be written in a row.
		TRIX (input): Array containing halfword data to be printed.
		MODE (input): 0 or 1, determines whether TRIX is to be printed.
		
2 History

  Original Programmer: ...<NAME & DATE>
  Current Cognizant Programmer: Damon D. Knight		SEP 3, 1993 
  Source Language: Fortran
  Ported to Unix: Damon D. Knight SEP 3, 1993

3 Operation

	PGRID takes the  coordiate points in X and out puts them by the row
    and column specified, with 20 being the max possible per row.

$ Return
$!#############################################################################
