/*------------------------------------------------------------------
 * C version - STACKA_big
 *
 * This routine is called by vicar fortran programs to dynamically
 * allocate memory. It is a modification of the older stacka.c 
 * routine which was limited to data sets less than 2GB (integer*4)
 *
 * This routine removes that limitation so images larger than 2 GB
 * can be used. It is presumed that this routine will ultimately
 * replace stacka.c. It currently keeps in place the limitation
 * of 25 arguments in the call. However, this routine can be modified
 * to expand that number if required in the future. 
 *
 * this subroutine is to be called in fortran using:
 *
 *     CALL STACKA_big(numargs,subname,n,L1,...,Ln,A1,...,Am)
 * where,
 *     numargs is the total number of arugments in the staca_big call
 *     subname is the name of the subroutine to be called
 *     n is the number of immediately following consecutive arguments
 *       to reserve memory for, i.e., L1,L2...,Ln. (These will vary
 *       according to the program).
 *     L1 thru Ln are the total number of bytes in each array to be 
 *       dimensioned
 *     A1 through Am are other non-dimensioned arguments to pe passed
 *       to the subroutine. (all formats, allowed).
 *
 *  NOTE: subname must be declared EXTERNAL in the calling program.
 *
 * Its effect is to execute the equivalent of the following fortran call:
 *      CALL subname(ARRAY1,L1,...,ARRAYn,Ln,A1,...,Am)
 *
 * Internal method:
 *  1. allocate space (in bytes) for n arrays of length L1,...,Ln.
 *  2. Execute subname(arglist) as shown in the fortran call above.
 *  3. Free allocated memory upon return from subname.
 *
 *----------------------------------------------------------------
 * USAGE NOTES: 
 *    Any routines that may be called by stacka must be modified to
 *    accept "numargs" as the first parameter. numargs = total number of
 *    arguments other than numargs.
 *    The maximum number of dynamic arrays or arguments is arbitrarily 
 *    set to 25 in this version (L1..Ln and A1..Am).  Thus 1 <= n+m <= 25.
 *
 *    The calls "malloc" and "free" are used to allocate
 *    and free the dynamic arrays.
 *    Note: the older stacka.c routine used calloc instead of malloc
 *
 *
 * Do not bother to compile this routine with the switches 
 *  CFLAGS += -Wmissing-prototypes -Wstrict-prototypes -Wmissing-declarations
 *
 * If compiled with -Wall then you will get warnings, ignore them if they are:
 *
 *  warning: implicit declaration of function 'zstacka_big'
 *  warning: conflicting types for 'zstacka_big' 
 * 
 *  A c trick is being used to bypass strict prototypes.
 *  That trick is being used in the second argument (subname) 
 *  which is really a pointer to a function. We do not
 *  know in advance the number and types of arguments that
 *  will be passed as part of subname. We are dynamically
 *  discovering that in the switch statements below. To
 *  do this under strict control would require many more
 *  switch statements for every possibility.
 *  
 *----------------------------------------------------------------    
 *    History:
 *    Oct 27, 2011 - Ray Bambery - initial release 
 *                  based on stacka.c of Feb 19, 2009
 *                  gcc 4.4.4
 *    Nov 10, 2011 - Ray Bambery - improved documentation
 *
 */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"

/* prototypes */
void FTN_NAME(stacka_big)(int *numargs,char **subr,int *n,
    int64_t  *arg1,int64_t  *arg2,int64_t  *arg3,int64_t  *arg4,int64_t  *arg5,
    int64_t  *arg6,int64_t  *arg7,int64_t  *arg8,int64_t  *arg9,int64_t  *arg10,
    int64_t  *arg11,int64_t  *arg12,int64_t  *arg13,int64_t  *arg14,int64_t  *arg15,
    int64_t  *arg16,int64_t  *arg17,int64_t  *arg18,int64_t  *arg19,int64_t  *arg20,
    int64_t  *arg21,int64_t  *arg22,int64_t  *arg23,int64_t  *arg24,int64_t  *arg25);
/* commented out because of c trick */
/*void zstacka_big(int *numargs,char *subr, int *n,
    int64_t  *arg1, int64_t  *arg2,int64_t  *arg3,int64_t  *arg4,int64_t  *arg5,
    int64_t  *arg6,int64_t  *arg7,int64_t  *arg8,int64_t  *arg9,int64_t  *arg10,
    int64_t  *arg11,int64_t  *arg12,int64_t  *arg13,int64_t  *arg14,int64_t  *arg15,
    int64_t  *arg16,int64_t  *arg17,int64_t  *arg18,int64_t  *arg19,int64_t  *arg20,
    int64_t  *arg21,int64_t  *arg22,int64_t  *arg23,int64_t  *arg24,int64_t  *arg25);
*/

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void zstacka_big(int* numargs, char (*subr)(), int* n,
		 int64_t *arg1, int64_t *arg2, int64_t *arg3, int64_t *arg4,
		 int64_t *arg5, int64_t *arg6, int64_t *arg7, int64_t *arg8,
		 int64_t *arg9, int64_t *arg10, int64_t *arg11, int64_t *arg12,
		 int64_t *arg13, int64_t *arg14, int64_t *arg15, int64_t *arg16,
		 int64_t *arg17, int64_t *arg18, int64_t *arg19, int64_t *arg20,
		 int64_t *arg21, int64_t *arg22, int64_t *arg23, int64_t *arg24,
		 int64_t *arg25);

int *numargs;
int *n;
char (*subr)();
int64_t  *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8,
     *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16,
     *arg17,*arg18,*arg19,*arg20,*arg21,*arg22,*arg23,*arg24,*arg25;

void FTN_NAME(stacka_big)(numargs, subr, n,
    arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
    arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
    arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25)
int *numargs;
char **subr;
int *n;
int64_t  *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8,
     *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16,
     *arg17,*arg18,*arg19,*arg20,*arg21,*arg22,*arg23,*arg24,*arg25;
{
  zstacka_big(numargs, subr, n,
     arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
     arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
     arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25);
}

/*------------------------------------------------------------------*/
void zstacka_big(numargs, subr, n,
       arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
       arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,
       arg17,arg18,arg19,arg20,arg21,arg22,arg23,arg24,arg25)

int *numargs;
int *n;
char (*subr)();
int64_t  *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8,
     *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16,
     *arg17,*arg18,*arg19,*arg20,*arg21,*arg22,*arg23,*arg24,*arg25;
{
  int i;
//  int byte_size = 1;       /* size of one byte used for calloc    */
  int nm;                  /* N+M = number of buffers + number of other args*/

  int64_t len[25];             /* temporary array to hold lengths     */
  char *aary[25];          /* array of pointers for new arg. list */

  nm = *numargs - 2; 
//   printf ("nm = %d   n = %d\n",nm,*n);
  if (*n < 1) {
    zvmessage( "STACKA_big: no buffers to allocate", "");
    zabend();
  }
  else if (nm > 25) {
    printf ("nm = %d\n",nm);
    zvmessage( "STACKA_big: too many arguments", "");
    zabend();
  }
  else if (*n > nm) {       /*  if N > N + M  */
    printf ("n,nm = %d %d\n",*n,nm);
    zvmessage( "STACKA_big: incorrect numargs argument", "");
    zabend();
  }

/*  load the n lengths into an array.  Note that any lengths < 1 are  */
/*  set to 1 to avoid adjustable array dimension errors.  */

  switch (*n)              /*  NOTE THAT THIS SWITCH HAS NO break STATEMENTS  */
    {              /*  Thus, there is cascading through the desired  */
               /*  number of assignment statements.       */
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

//    printf ("len[0] = %ld  %ld   %ld\n",len[0],len[1],len[2] );
  for(i=0;i<*n;i++) {
//    aary[i] = (char *)calloc((size_t) len[i], (size_t)byte_size );
    aary[i] = malloc ((size_t) len[i]);
//    printf ("i, len[i], aary[i] = %d    %ld    %s\n",i,len[i],aary[i]);
    if (aary[i] == NULL) {
      zvmessage("STACKA_big: malloc failed - insufficient memory", "");
      zabend();
    }
  }
/* Here is where you jump into the routine  */
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
/* ON return - free memory */
    for(i=0;i<*n;i++)
      free(aary[i]);
}

