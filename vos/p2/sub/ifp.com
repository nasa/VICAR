$!****************************************************************************
$!
$! Build proc for MIPL module ifp
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:39
$!
$! Execute by entering:		$ @ifp
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
$ write sys$output "*** module ifp ***"
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
$ write sys$output "Invalid argument given to ifp.com file -- ", primary
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
$   if F$SEARCH("ifp.imake") .nes. ""
$   then
$      vimake ifp
$      purge ifp.bld
$   else
$      if F$SEARCH("ifp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ifp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ifp.bld "STD"
$   else
$      @ifp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ifp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ifp.com -mixed -
	-s ifp.f zifp.c -
	-i ifp.imake -
	-t tifp.f tzifp.c tifp.imake tifp.pdf tstifp.pdf -
	-o ifp.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ifp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	FUNCTION IFP(X,Y,M,N,BUF,INTERP,IHALF)

!       IFP: A four point interpolation function

!       IFP passed parameters
        REAL     X, Y
        INTEGER  M, N, INTERP, IHALF
	BYTE     BUF(1)

!       IFP local parameters
        REAL*8  E0,E2,E4,E6,E,F,G,H,R7,R8,R9,R10
	INTEGER HALF, status
        INTEGER byte_to_half (12), half_to_half (12)
        INTEGER half_to_doub (12), byte_to_doub (12)

	INTEGER*2 IOUT, ifpout
	BYTE      IOUTEQV(2)

!       IFP local parameter initialization
        DATA E0 /0.0/, E2 /0.0/, E4 /0.0/, E6  /0.0/
        DATA E  /0.0/, F  /0.0/, G  /0.0/, H   /0.0/
        DATA R7 /0.0/, R8 /0.0/, R9 /0.0/, R10 /0.0/
	DATA HALF /0/, status /0/
	DATA half_to_doub /12*0/, byte_to_doub /12*0/
	DATA IOUTEQV(1) /0/, IOUTEQV(2) /0/
        DATA IOUT /0/, IFPOUT /0/

C    -------------------------------------------------------------      
C      07 Mar 95    ...CRI...   Removed LIB_LOCAL from imake
C      01 Jul 94    ...CRI...   Made portable for UNIX
C      FEB 1985     ...BZB...   CONVERTED TO VAX FORTRAN 
C      21 AUG 80    ...JBS...   CORRECT ERROR WHEN X=M OR Y=N
C      28 DEC 78    ...WDB...   INITIAL RELEASE
C      TITLE 'FOUR POINT INTERPOLATION SUBROUTINE'
C              OUT=IFP(X,Y,M,N,BUF,INTERP,HALF)
C              WHERE X & Y ARE THE REAL VALUES OF THE THE DESIRED VALUE
C              BUF(M,N) IS THE INPUT BUFFER OF DIMENSION M,N
C              INTERP IS AN INTERPOLATION FLAG. 0 IF INTERPOLATION,
C                 ELSE 1 FOR NO INTERPOLATION                           
C              HALF IS A PARM SPECIFYING WHETHER THE
C                 INPUT IS HALFWORD OR BYTE. 0 IF BYTE, 1 IF HALF.
C
C
C        (PX,PY)              (PX+1,PY)
C        O----------------------------O
C        I*                          *I
C        I *                        * I
C        I  *                      *  I
C        I   *                    *   I
C        I    *                  *    I
C        I     *                *     I
C        I      E              G      I
C        I        *           *       I
C        I         *         *        I
C        I          *       *         I
C        I           *     *          I
C        I            *   *           I
C        I             * *            I
C        I              $-(X,Y)       I
C        I             * *            I
C        I            *   *           I
C        I           *     *          I
C        I          *       *         I
C        I         *         *        I
C        I        *           *       I
C        I      F              H      I
C        I     *                *     I
C        I    *                  *    I
C        I   *                    *   I
C        I  *                      *  I
C        I *                        * I
C        I*                          *I
C        O----------------------------O
C        (PX,PY+1)          (PX+1,PY+1)
C
C    -------------------------------------------------------------
C
        HALF = IHALF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (byte_to_half, 'BYTE',
     &                   'HALF', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (half_to_half, 'HALF',
     &                   'HALF', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to integer
        call xvtrans_set (half_to_doub, 'HALF',
     &                   'DOUB', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

!       Initialize translation buffer for byte to real*8 
        call xvtrans_set (byte_to_doub, 'BYTE',
     &                   'DOUB', status)

!       If translation setup did not function properly then
        IF (status .NE. 1) THEN
           IFP = -1
           return
        ENDIF

	IF (INTERP.NE.0) GO TO 8000
	IPX=X
	E0=X-IPX
	IPY=Y
	E2=Y-IPY
	IF ((E0.EQ.0.).AND.(E2.EQ.0.)) GO TO 8000
	IF (E0.NE.0.) GO TO 10
9	CONTINUE
	IF (IPX.NE.M) GO TO 10
	IPX=IPX-1
	E0=.999
10	CONTINUE
	IF (E2.NE.0.) GO TO 20
19	CONTINUE
	IF (IPY.NE.N) GO TO 20
	IPY=IPY-1
	E2=.999

20	CONTINUE
	E4=1.-E0
	E6=1.-E2
	E0=E0*E0
	E2=E2*E2
	E4=E4*E4
	E6=E6*E6
	E=1./(E0+E2)
	H=1./(E4+E6)
	F=1./(E0+E6)
	G=1./(E2+E4)

	IF (HALF.EQ.1) GO TO 200
        J1 = (IPY-1)*M+IPX
        J2 = J1+M

        CALL xvtrans (byte_to_doub, buf(J1),   R7, 1)
        CALL xvtrans (byte_to_doub, buf(J1+1), R8, 1)
        CALL xvtrans (byte_to_doub, buf(J2),   R9, 1)
        CALL xvtrans (byte_to_doub, buf(J2+1), R10, 1)

	IFP = (R7*E+R8*G+R9*F+R10*H)/(E+G+F+H)+.5

	RETURN

200	CONTINUE
	IOUT=0
        J1 = 2*(IPY-1)*M+2*IPX-1
        J2 = J1+2*M

	IOUTEQV(1)=BUF(J1)
	IOUTEQV(2)=BUF(J1+1)
        CALL xvtrans (half_to_doub, IOUTEQV(1), R7, 1)

	IOUTEQV(1)=BUF(J1+2)
	IOUTEQV(2)=BUF(J1+3)
        CALL xvtrans (half_to_doub, iouteqv(1),   R8, 1)

	IOUTEQV(1)=BUF(J2)
	IOUTEQV(2)=BUF(J2+1)
        CALL xvtrans (half_to_doub, iouteqv(1),   R9, 1)

	IOUTEQV(1)=BUF(J2+2)
	IOUTEQV(2)=BUF(J2+3)
        CALL xvtrans (half_to_doub, iouteqv(1),   R10, 1)

	IFP=(R7*E+R8*G+R9*F+R10*H)/(E+G+F+H)+.5

	RETURN

8000	CONTINUE
C	NO INTERPOLATION, 
C	JUST NEAREST NEIGHBOR
	IPX=X+.5
	IPY=Y+.5
	IF (HALF.EQ.1) GO TO 8200
	IOUTEQV(1)=BUF( (IPY-1)*M+IPX )
        CALL xvtrans (byte_to_half, 
     &                IOUTEQV(1), ifpout, 1)
	IFP = ifpout
        RETURN

8200	CONTINUE
	IOUT=0
	IOUTEQV(1)=BUF( 2*(IPY-1)*M+2*IPX-1 )
	IOUTEQV(2)=BUF( 2*(IPY-1)*M+2*IPX )
        CALL xvtrans (half_to_half, 
     &                IOUTEQV(1), ifpout, 1)
	IFP = ifpout
	RETURN
	END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zifp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for ifp  -  Four point interpolation                        */
/************************************************************************/

int zifp (x, y, M, N, A, interp, ihalf)
void   *A;
float   x, y;
int     M, N, interp, ihalf;
{
int        i, j, status;
void      *ptr; 
unsigned char *ch_from, *ch_to, *ch_old_A, *ch_new_A;
short    int  *si_from, *si_to, *si_old_A, *si_new_A;

   /* If 'byte' input is specified then */
   if (ihalf == 0) {
      ch_old_A = (unsigned char *)A;     
      ch_new_A = (unsigned char *)malloc (sizeof(unsigned char)*M*N);
      ptr      = (void *)ch_new_A;
      /* Invert matrix A[M][N] */
      for (i = 0; i < M; i++) {
         for (j = 0; j < N; j++) {
            ch_from  = (ch_old_A + (i*N) + j);
            ch_to    = (ch_new_A + (j*M) + i);
           *ch_to    = *ch_from;
         }
      }
   } else {
      /* Else then 'integer*2 is the specified format */
      si_old_A = (short int *)A;     
      si_new_A = (short int *)malloc (sizeof(short int)*M*N);
      ptr      = (void *)si_new_A;
      for (i = 0; i < M; i++) {
         for (j = 0; j < N; j++) {
            si_from = (si_old_A + (i*N) + j);
            si_to   = (si_new_A + (j*M) + i);
           *si_to   = *si_from;
         }
      }
   }

   status = FTN_NAME2(ifp, IFP)( &x, &y, &M, &N, ptr, &interp, &ihalf);

   free (ptr);
   return status;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ifp.imake
/* Imake file for VICAR function ifp */

#define SUBROUTINE ifp

#define MODULE_LIST ifp.f zifp.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tifp.f
	include 'VICMAIN_FOR'

	subroutine main44

!	implicit none
!       Local variables
	real x(2)
	byte bbuf(10,10), bx
	integer*2 hbuf(5,5), hx
        integer i, j
        character*132 buffer

!       Local variable initialization
        DATA   i /0/, j /0/
	DATA   x /2*0/

!       Initiate call to 'C' version of test program to test bridge
        call tzifp

	call xvmessage ('byte input buffer:', ' ')
	do i=1,10
	  do j=1,10
	    bbuf(j,i) = 5*(i+j-1)	! construct a ramp
	  enddo
          write (buffer, 100) (bbuf(j,i),j=1,10)
100       format ('     ',10I4)
          CALL xvmessage (buffer, ' ')
	enddo

	x(1) = 5.1
	x(2) = 4.3

        write (buffer, 101) x(1), x(2)
101     format ('At X/Y=    ',2E11.4)
        CALL xvmessage (buffer, ' ')

	bx = ifp( x(1), x(2), 10, 10, bbuf, 0, 0)

        write (buffer, 103) bx
103     format ('IFP =  ', I6)
        CALL xvmessage (buffer, ' ')
c
	call xvmessage (' ', ' ')
	call xvmessage ('halfword input buffer:', ' ')
	do i=1,5
	  do j=1,5
	    hbuf(j,i) = 100*(i+j-1)	! construct a ramp
	  enddo
          write (buffer, 201) (hbuf(j,i),j=1,5)
201       format ('     ',5I6)
          CALL xvmessage (buffer, ' ')
	enddo

	x(1) = 2.1
	x(2) = 3.3

        write (buffer, 101) x(1), x(2)
        CALL xvmessage (buffer, ' ')

	hx = ifp( x(1), x(2), 5, 5, hbuf, 0, 1)

        write (buffer, 203) hx
203     format ('IFP =     ', I5)
        CALL xvmessage (buffer, ' ')
c
	return
	end
$!-----------------------------------------------------------------------------
$ create tzifp.c
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
void FTN_NAME(tzifp)()
{
   float         x[2];
   unsigned char bbuf[10][10];
   unsigned char bx;
   short    int  hbuf[5][5];
   short    int  hx;
   short    int  i, j;
   char  buffer [80];
   char *ptr;

   memset (buffer, ' ', sizeof (buffer));
   buffer [sizeof(buffer)-1] = '\0';

   zvmessage ("byte input buffer:", 0);
   for (i = 0; i < 10; i++) {
      for (j = 0; j < 10; j++) {
          bbuf[j][i] = 5 * (i+j+1);      /* construct a ramp */
         (void)sprintf (buffer+5+j*4, "%4d", bbuf [j][i]);
      }
      zvmessage (buffer,0);
   }

   x[0] = 5.1;
   x[1]	= 4.3;

   (void)sprintf (buffer, "At X/Y=     %1.4E %1.4E", x[0], x[1]);
   zvmessage (buffer, 0);

   bx = zifp( x[0], x[1], 10, 10, bbuf, 0, 0);
   (void)sprintf (buffer, "IFP =      %d\n", bx); 
   zvmessage (buffer, 0);

   memset (buffer, ' ', sizeof (buffer));
   buffer [sizeof(buffer)-1] = '\0';

   zvmessage ("halfword input buffer:", 0);
   for (i = 0; i < 5; i++) {
      for (j = 0; j < 5; j++) {
         hbuf[j][i] = 100 * (i+j+1);    /* construct a ramp */
         (void)sprintf (buffer+5+j*6, "%6d", hbuf [j][i]);
      }
      zvmessage (buffer,0);
   }

   x[0] = 2.1;
   x[1] = 3.3;

   (void)sprintf (buffer, "At X/Y=     %1.4E %1.4E", x[0], x[1]);
   zvmessage (buffer, 0);

   hx = zifp ( x[0], x[1], 5, 5, hbuf, 0, 1);
   (void)sprintf (buffer, "IFP =       %d\n", hx);
   zvmessage (buffer, 0);

   return;
}
$!-----------------------------------------------------------------------------
$ create tifp.imake
/* Imake file for Test of VICAR function IFP */

#define PROGRAM tifp

#define MODULE_LIST tifp.f tzifp.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
#define P2_SUBLIB
$!-----------------------------------------------------------------------------
$ create tifp.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstifp.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage= "none"
let _onfail="continue"
let $echo="yes"
tifp
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ifp.hlp
1 IFP

  IFP - four point interpolation function

  Calling sequence:

      Fortran interface:

            OUT = IFP (X, Y, M, N, BUF, INTERP, HALF)

      C interface:

            out = zifp (x, y, m, n, buf, interp, half)

  Arguments:  

	X,Y        Coordinates of the desired value 
                   REAL*4 when using Fortran
                   REAL   when using C

	BUF(M,N)   Input buffer of dimension M,N

                   Note: When using 'C', the memory layout of BUF[M][N]
                   as provided by the 'C' compiler is different than that
                   which is provided by the 'FORTRAN' compiler.  Therefore,
                   the 'C' bridge interface to the 'FORTRTAN' function IFP
                   translates the memory layout of BUF[M][N] to that which
                   is compatible with the 'FORTRAN' compiler and which is
                   required by the 'FORTRAN' function IFP.

	INTERP     = 0: interpolation, = 1: no interpolation
	HALF       = 0: byte data,     = 1: halfword


2 History

  Current Cognizant Programmer: L.W.Kamp

  Removed LIB_LOCAL as per FR85770     (CRI) 7 Mar  1995
  Made portable for UNIX by: J. Turner (CRI) 1 July 1994
  Converted to VAX by:       B.Z.Barkan,  Feb. 1985
  Original Programmer:       W.D.Benton,  28 Dec.1978

  Source Language: Fortran
  Bridge Language: C   

2 Operation

  Given an input buffer BUF(M,N) and two points X,Y, where X
  lies in the range (1,...,M) and Y lies in the range (1,...,N),
  then IFP returns a value interpolated between the values of
  BUF at the four points in the M/N plane that are closest to
  X and Y.  Thus, if IX and IY are the integer truncations of
  X and Y, then the interpolation is between:

   BUF(IX), BUF(IX+1), BUF(IY), BUF(IY+1).

  An inverse-squared-distance weighted interpolation is performed,
  unless NOIN=1, in which case the value of BUF at the closest of
  these points is returned.

  The data type of BUF and IFP must be the same and correspond to
  that specified by HALF.
$ Return
$!#############################################################################
