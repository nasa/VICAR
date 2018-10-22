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
  
