/************************************************************************/
/*									*/
/*				bits.c					*/
/*		Originally changed to C by Steve Hwan			*/
/*									*/
/*			CREATION DATE: 15 October 1993			*/
/*			LAST MODIFIED: 19 October 1993			*/
/*									*/
/*	These routines extract bits lowb through highb from IN and	*/
/* put them into OUT.  One routine is written for full words		*/
/* (long ints) and one is written for half words (short ints).		*/
/*									*/
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>

#define HIGH_BIT_L	(sizeof(long int) * 8 -1)
#define HIGH_BIT_S	(sizeof(short int) * 8 -1)

void zbits(),zhbits();

void FTN_NAME2(bits, BITS) (in,lowb,highb,out)
long int *in;
int *lowb,*highb;
long int *out;
{
  zbits(*in,*lowb,*highb,out);
}

void FTN_NAME2(hbits, HBITS) (in,lowb,highb,out)
short int *in;
int *lowb,*highb;
long int *out;
{
  zhbits(*in,*lowb,*highb,out);
}


void zbits(in,lowb,highb,out)
long int in;
int lowb,highb;				/* uses 0 to N-1 */
long int *out;
{
  unsigned long int mask= 0;

  mask = ~mask;				/* Initialize mask with all 1's */

  if ((lowb<0) || (highb<0)		/* negative bit numbers specified */
      ||  (highb<lowb)			/* highb lower than lowb */
      ||  (lowb>HIGH_BIT_L)		/* bit numbers beyond top */
      || (highb>HIGH_BIT_L))  {		/* of word. */
    zvmessage("Routine bits called with invalid parameters.","");
    zabend();
  }

  mask = mask >>  (HIGH_BIT_L-highb );
  *out = ((unsigned long int)in & mask) >> lowb;

 return;
}


void zhbits(in,lowb,highb,out)
short int in;
int lowb,highb;				/* uses 0 to N-1 */
long int *out;
{
  unsigned short int mask= 0;

  mask = ~mask;				/* Initialize mask with all 1's */

  if ((lowb<0) || (highb<0)		/* negative bit numbers specified */
      ||  (highb<lowb)			/* highb lower than lowb */
      ||  (lowb>HIGH_BIT_S)		/* bit numbers beyond top */
      || (highb>HIGH_BIT_S)) {		/* of word. */
    zvmessage("Routine bits called with invalid parameters.","");
    zabend();
  }

  mask = mask >> (HIGH_BIT_S-highb );
  *out = ( (unsigned short int) ((unsigned short int)in & mask) >> lowb);

 return;
}

	
