static char sccsNewmve[] = "@(#) newmve.c 2.2 2/3/87 PDS Vicar2";

#include <stdio.h>


/****************************************************************************
 *   NEWMVE  - an addition to the MVE bridge subroutines that allows for more
 *             dcodes than given in the original mve.c.
 *
 *   NOTE :    The C and FORTRAN bridges for mve are in this file along with
 *             the internal version of the code.  The interface is the  same
 *             as the VMS version for both fortran and C.  The FORTRAN
 *             bridges pass along the number of arguments by value in numarg.
 *
 ***************************************************************************
 *   Format of call :
 *  
 *   CALL MVE(numarg, DCODE, N, A, B, AINC, BINC)     FORTRAN version
 *
 *   Where :
 *
 *     numarg    is the number of arguments to follow. (passed by value)
 *
 *     dcode     is the transfer mode.  This addition to the mve.c routines
 *               only takes care of the following cases :
 *               21  : double to byte
 *               22  : double to halfword
 *               23  : fullword to double
 *               24  : fullword to real
 *               25  : real to byte
 *               26  : real to halfword
 *
 *     N         is the number of elements to transfer
 *     A         Source vector to be transfered
 *     B         Destination vector to be transferred *     AINC      Source address increment - default 1 if ommitted
 *     BINC      Destination address increment - default 1 if ommitted
 *
 **************************************************************************/

/* FORTRAN bridge to newmve.c  */
newmve_(numarg,dcode,_n,a,b,_ainc,_binc, /* plus dummy f77 lengths */ l1, l2)
	int             numarg, *dcode, *_n, *_ainc, *_binc;
	unsigned char   *a, *b;
{
	newmve(numarg, dcode, _n, a, b, _ainc, _binc);
}

/* internal version of newmve, called from bridges above. */

newmve(numarg, dcode, len, a, b, _ainc, _binc)
	int             numarg, *dcode, *len, *_ainc, *_binc;
	unsigned char   *a, *b;

{
	register        i, n, ainc, binc;

	unsigned char  *byte;	/* 8 bit                    */
	short          *word;	/* 16 bit, same as halfword */
	unsigned       *full;	/* 32 bit, same as fullword */
	double         *comp;	/* 64 bit, same as double   */
	float          *real;	/* 32 bit                   */

	n = *len;
	if (numarg < 4) {
		printf("newmve: error %d are insufficient arguments to call\n",
		       numarg);
		zabend();
	}
	if (numarg < 5)
		ainc = 1;
	else
		ainc = *_ainc;
	if (numarg < 6)
		binc = 1;
	else
		binc = *_binc;

	switch (*dcode) {
	case -21:		/* byte to comp */
		comp = (double *)b;
		for (i = 0; i <= n; i++, a += ainc, comp += binc)
			*comp = *a;
		break;
	case -22:		/* word to comp */
		word = (short *)a, comp = (double *)b;
		for (i = 0; i <= n; i++, word += ainc, comp += binc)
			*comp = *word;
		break;
	case -23:		/* comp to full */
		comp = (double *)a, full = (unsigned *)b;
		for (i = 0; i <= n; i++, comp += ainc, full += binc)
			*full = *comp;
		break;
	case -24:		/* real to full */
		real = (float *)a, full = (unsigned *)b;
		for (i = 0; i <= n; i++, real += ainc, full += binc)
			*full = *real;
		break;
	case -25:		/* byte to real */
		real = (float *)b;
		for (i = 0; i <= n; i++, a += ainc, real += binc)
			*real = *a;
		break;
	case -26:		/* word to real */
		word = (short *)a, real = (float *)b;
		for (i = 0; i <= n; i++, word += ainc, real += binc)
			*real = *word;
		break;
	case 21:		/* comp to byte */
		comp = (double *)a;
		for (i = 0; i <= n; i++, comp += ainc, b += binc)
			*b = *comp;
		break;
	case 22:		/* comp to word */
		comp = (double *)a, word = (short *)b;
		for (i = 0; i <= n; i++, comp += ainc, word += binc)
			*word = *comp;
		break;
	case 23:		/* full to comp */
		full = (unsigned *)a, comp = (double *)b;
		for (i = 0; i <= n; i++, full += ainc, comp += binc)
			*comp = *full;
		break;
	case 24:		/* full to real */
		full = (unsigned *)a, real = (float *)b;
		for (i = 0; i <= n; i++, full += ainc, real += binc)
			*real = *full;
		break;
	case 25:		/* real to byte */
		real = (float *)a;
		for (i = 0; i <= n; i++, real += ainc, b += binc)
			*b = *real;
		break;
	case 26:		/* real to word */
		real = (float *)a, word = (short *)b;
		for (i = 0; i <= n; i++, real += ainc, word += binc)
			*word = *real;
		break;
	default:		/* otherwise invalid data */
		printf("  newmve : **** invalid value for dcode ****\n");
		zabend();
	      }
      }
