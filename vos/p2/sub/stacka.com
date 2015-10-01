$!****************************************************************************
$!
$! Build proc for MIPL module stacka
$! VPACK Version 1.9, Monday, December 07, 2009, 16:37:03
$!
$! Execute by entering:		$ @stacka
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
$ write sys$output "*** module stacka ***"
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
$ write sys$output "Invalid argument given to stacka.com file -- ", primary
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
$   if F$SEARCH("stacka.imake") .nes. ""
$   then
$      vimake stacka
$      purge stacka.bld
$   else
$      if F$SEARCH("stacka.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stacka
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stacka.bld "STD"
$   else
$      @stacka.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stacka.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stacka.com -mixed -
	-s stacka.c -
	-i stacka.imake -
	-t tstacka.f tstacka.imake tstacka.pdf tststacka.pdf -
	-o stacka.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stacka.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*------------------------------------------------------------------
 * C version of STACKA
 *
 * this subroutine is to be called by fortran using:
 *     CALL STACKA(numargs,subname,n,L1,...,Ln,A1,...,Am)
 * subname is declared external in the calling program.
 * Its effect is to execute the equivalent of the following fortran call:
 *      CALL subname(ARRAY1,L1,...,ARRAYn,Ln,A1,...,Am)
 *
 * method:
 *  1. allocate space for n arrays of length L1,...,Ln.
 *  2. Call subname(arglist) as shown above.
 *  3. Free allocated memory upon return from subname.
 *----------------------------------------------------------------
 * USAGE NOTES: 
 *    Any routines that may be called by stacka must be modified to
 *    accept "numargs" as the first parameter. numargs = total number of
 *    arguments other than numargs.
 *    The maximum number of dynamic arrays or arguments is arbitrarily 
 *    set to 25 in this version (L1..Ln and A1..Am).  Thus 1 <= n+m <= 25.
 *
 *    The calls "calloc" and "free" are used to allocate
 *    and free the dynamic arrays.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(stacka, STACKA) (numargs, subr, n,
	arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
	arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
	arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25)
int *numargs;
char **subr;
int *n;
int  *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8,
     *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16,
     *arg17,*arg18,*arg19,*arg20,*arg21,*arg22,*arg23,*arg24,*arg25;
{

#if MAC_MPW_ARCH
  /* functions are passed as pointers TO pointers ! */
  zstacka(*numargs, *subr, n,
	 arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
	 arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
	 arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
#else
  zstacka(*numargs, subr, n,
	 arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
	 arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
	 arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
#endif

}


/*------------------------------------------------------------------*/


zstacka(numargs, subr, n,
       arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
       arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
       arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25)

int numargs, *n;
char (*subr)();
int  *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8,
     *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16,
     *arg17,*arg18,*arg19,*arg20,*arg21,*arg22,*arg23,*arg24,*arg25;
{
  int i;
  int byte_size = 1;       /* size of one byte used for calloc    */
  int nm;                  /* N+M = number of buffers + number of other args*/

  int len[25];             /* temporary array to hold lengths     */
  char *aary[25];          /* array of pointers for new arg. list */

  nm = numargs - 2; 

  if (*n < 1) {
    zvmessage( "STACKA: no buffers to allocate", "");
    zabend();
  }
  else if (nm > 25) {
    zvmessage( "STACKA: too many arguments", "");
    zabend();
  }
  else if (*n > nm) {		/*  if N > N + M  */
    zvmessage( "STACKA: incorrect numargs argument", "");
    zabend();
  }

/*  load the n lengths into an array.  Note that any lengths < 1 are  */
/*  set to 1 to avoid adjustable array dimension errors.  */

  switch (*n)              /*  NOTE THAT THIS SWITCH HAS NO break STATEMENTS  */
    {			   /*  Thus, there is cascading through the desired  */
			   /*  number of assignment statements.		  */
    case 25: if (*arg25 < 1)  *arg25 = 1;
             len[24] = *arg25;
    case 24: if (*arg24 < 1)  *arg24 = 1;
             len[23] = *arg24;
    case 23: if (*arg23 < 1)  *arg23 = 1;
             len[22] = *arg23;
    case 22: if (*arg22 < 1)  *arg22 = 1;
             len[21] = *arg22;
    case 21: if (*arg21 < 1)  *arg21 = 1;
             len[20] = *arg21;
    case 20: if (*arg20 < 1)  *arg20 = 1;
             len[19] = *arg20;
    case 19: if (*arg19 < 1)  *arg19 = 1;
             len[18] = *arg19;
    case 18: if (*arg18 < 1)  *arg18 = 1;
             len[17] = *arg18;
    case 17: if (*arg17 < 1)  *arg17 = 1;
             len[16] = *arg17;
    case 16: if (*arg16 < 1)  *arg16 = 1;
             len[15] = *arg16;
    case 15: if (*arg15 < 1)  *arg15 = 1;
             len[14] = *arg15;
    case 14: if (*arg14 < 1)  *arg14 = 1;
             len[13] = *arg14;
    case 13: if (*arg13 < 1)  *arg13 = 1;
             len[12] = *arg13;
    case 12: if (*arg12 < 1)  *arg12 = 1;
             len[11] = *arg12;
    case 11: if (*arg11 < 1)  *arg11 = 1;
             len[10] = *arg11;
    case 10: if (*arg10 < 1)  *arg10 = 1;
             len[ 9] = *arg10;
    case  9: if (*arg9  < 1)  *arg9  = 1;
             len[ 8] = *arg9;
    case  8: if (*arg8  < 1)  *arg8  = 1;
             len[ 7] = *arg8;
    case  7: if (*arg7  < 1)  *arg7  = 1;
             len[ 6] = *arg7;
    case  6: if (*arg6  < 1)  *arg6  = 1;
             len[ 5] = *arg6;
    case  5: if (*arg5  < 1)  *arg5  = 1;
             len[ 4] = *arg5;
    case  4: if (*arg4  < 1)  *arg4  = 1;
             len[ 3] = *arg4;
    case  3: if (*arg3  < 1)  *arg3  = 1;
             len[ 2] = *arg3;
    case  2: if (*arg2  < 1)  *arg2  = 1;
             len[ 1] = *arg2;
    case  1: if (*arg1 < 1)  *arg1 = 1;
             len[ 0] = *arg1;
    }

  for(i=0;i<*n;i++) {
    aary[i] = (char *)calloc( len[i], byte_size );
    if (aary[i] == '\0') {
      zvmessage("STACKA: calloc failed - insufficient memory", "");
      zabend();
    }
  }

  switch(*n)
    {
    case 1: (*subr)(aary[0],arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
		    arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 2: (*subr)(aary[0],arg1,aary[1],arg2,arg3,arg4,arg5,arg6,arg7,
		    arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 3: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,arg4,arg5,arg6,
		    arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 4: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,
		    arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 5: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    aary[4],arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,
		    arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 6: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    aary[4],arg5,aary[5],arg6,arg7,arg8,arg9,arg10,arg11,
		    arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 7: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    aary[4],arg5,aary[5],arg6,aary[6],arg7,arg8,arg9,arg10,
		    arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 8: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		    arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 9: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		    aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		    aary[8],arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
                    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;

    case 10: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,arg11,arg12,arg13,arg14,
		     arg15,arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 11: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,arg12,arg13,
		     arg14,arg15,arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 12: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,arg13,arg14,arg15,arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 13: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,arg14,arg15,arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 14: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,arg15, arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 15: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 16: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,
                     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 17: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,
                     arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 18: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,
                     arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 19: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 20: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,arg21,arg22,arg23,arg24,arg25);
	    break;
    case 21: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,aary[20],arg21,
                     arg22,arg23,arg24,arg25);
	    break;
    case 22: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,aary[20],arg21,aary[21],
                     arg22,arg23,arg24,arg25);
	    break;
    case 23: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,aary[20],arg21,aary[21],arg22,
                     aary[22],arg23,arg24,arg25);
	    break;
    case 24: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,aary[20],arg21,aary[21],arg22,
                     aary[22],arg23,aary[23],arg24,arg25);
	    break;
    case 25: (*subr)(aary[0],arg1,aary[1],arg2,aary[2],arg3,aary[3],arg4,
		     aary[4],arg5,aary[5],arg6,aary[6],arg7,aary[7],arg8,
		     aary[8],arg9,aary[9],arg10,aary[10],arg11,aary[11],
		     arg12,aary[12],arg13,aary[13],arg14,aary[14],arg15,
		     aary[15],arg16,aary[16],arg17,aary[17],arg18,aary[18],
                     arg19,aary[19],arg20,aary[20],arg21,aary[21],arg22,
                     aary[22],arg23,aary[23],arg24,aary[24],arg25);
	    break;
    }

    for(i=0;i<*n;i++)
      free(aary[i]);
}
  
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stacka.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY stacka

   To Create the build file give the command:

	$ vimake stacka                     (VMS)
   or
	% vimake stacka                     (Unix)


*************************************************************************/

#define SUBROUTINE stacka

#define MODULE_LIST stacka.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tstacka.f
C TEST SUBROUTINE STACKA
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      EXTERNAL T2STCK,T25STCK
C
      CALL XVMESSAGE(
     . 'CALL T2STCK(A,N,B,L,X): PRINT BYTE ARRAY A(N),  CONSTANT X',' ')
      L=0
      N=10
      X=100
      CALL STACKA(5,T2STCK,2,N,L,X)  ! 5 other parameters.
C
      CALL STACKA(27,T25STCK,1,N,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     .               16,17,18,19,20,21,22,23,24,25)
      RETURN
      END
C
      SUBROUTINE T2STCK(A,N,B,N1,X)
C
C TEST SUBROUTINE STACKA
C
      IMPLICIT INTEGER (A-Z)
      BYTE A(N),B(N1)
C
      DO 1 I=1,10
    1 A(I)=I
      CALL PRNT(1,N,A,' ARRAY A:.')
      CALL PRNT(4,1,N1,' 0 DIMENSION CHANGED TO:.')
      CALL PRNT(4,1,X,' INTEGER X:.')
C
      RETURN
      END
      SUBROUTINE T25STCK(A,N, AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,
     .                        AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX)
C
C TEST SUBROUTINE STACKA
C
      IMPLICIT INTEGER (A-Z)
      BYTE A(N)
C
      DO 1 I=1,10
    1 A(I)=I
      CALL PRNT(1,N,A,' ARRAY A:.')
      CALL PRNT(4,1,AX,' INTEGER AX:.')
C
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tstacka.imake
/* Imake file for Test of VICAR subroutine stacka */

#define PROGRAM tstacka

#define MODULE_LIST tstacka.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tstacka.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tststacka.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tstacka
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create stacka.hlp
1 STACKA

  Subroutine to provide run-time buffer allocation (dynamic memory
  allocation) for Fortran-type subroutine calls.

  Calling Sequence:

    CALL STACKA(numargs,subname,N,L1,...,LN,A1,...,AM)

   Its effect is to execute the equivalent of the following Fortran call:

    CALL subname(ARRAY1,L1,...,ARRAYN,LN,A1,...,AM)

  Arguments: 
    numargs = total number of arguments other than numargs = N+M+2.  This 
              must be <= 27.
    subname = Subroutine name; declared EXTERNAL in the calling program.
    N = Number of arrays to create (dynamically allocate).  1 <= N <= 25.
    L1, ..., LN = N array lengths in bytes.  If the 
          value of any LN is less 1, then it is changed to 1, and therefore
          it must be passed via a variable rather than as a constant.
          Therefore, do not attempt to have the constant 0 as a length
          argument in the call to STACKA.  (This feature is there to
          prevent adjustable array dimension errors.)
    A1, ...,AM  = M variables to pass along to subname routine.  Can be
                  scalars or arrays of any type but CHARACTER.
   
    numargs, N, and L1,..., LN are INTEGER*4.

2 History

  Original VICAR Programmer:  J. B. Seidman,  24 June 1974
  Rewritten for VAX by:  L. W. Kamp,  June 1981
  Rewritten in C by ASU with modifications by Ron Alley and Steve Pohorsky
  Current Cognizant Programmer:  S. Pohorsky
  Source Language: C

2 Operation

  The effect of executing the above call to STACKA is equivalent to calling
  the following Fortran subroutine:

    SUBROUTINE subname( ARRAY1, L1, ..., ARRAYN, LN, A1, ..., AM )
    INTEGER*4 L1, ...,L1
    BYTE ARRAY1(L1), ..., ARRAYN(LN)

    .... [rest of subroutine]

  This subroutine has 2N+M arguments, of which the first 2N are variable-
  length array names and their lengths.

  NOTE: This version will ALWAYS allocate the requested amount of VIRTUAL
  memory, unless the requested amount is not available, in which case it
  calls ABEND.  Thus its only purpose is to allow program buffer sizes to be 
  determined at run time, to avoid hard-coding length limitations into a
  program.  The memory will be freed automatically when routine subname
  finishes.

2 Method

  First, STACKA allocates space for the arrays requested
  using the C routine "calloc".  When the memory has been allocated, the
  requested subroutine is called.

  Upon return from the called subroutine, STACKA deallocates the virtual
  memory it had previously allocated, using the C routine "free".

$ Return
$!#############################################################################
