/*---------------------------  nibble     ------------------------
 * NIBBLE  (Nibbles to Bytes)
 *
 *  REVISION HISTORY
 *    5-94 CRI MSTP S/W Conversion (VICAR Porting), changed to use C
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                           nibble

	General routine for converting an array of nibbles
          into an array of bytes

	Fortran format of call:

          CALL NIBBLE( IN,OUT,NBYTES_OUT)

        "C" format of call:

          znibble(in, out, nbytes_out);
 
	Parameters:-

	IN - byte declared array containing the nibbles
        OUT - byte declared array to hold the unpacked bytes
        NBYTES_OUT - is the number of resultant bytes

--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#define NIB_MASK 15      /* mask for right nibble */
/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(nibble, NIBBLE) (in, out, nbytes_out)
     unsigned char *in, *out;
     int *nbytes_out;
{
   znibble( in, out, *nbytes_out);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

znibble( in, out, nbytes_out)
   unsigned char *in, *out;
   register int nbytes_out;
{
  register unsigned char nibb;
  register int i, j;

  for (i=0; i < nbytes_out/2; i++)         /* two nibbles to a byte */
    {
      j = 2 * i;
      nibb = *(in + i);                    /* get byte with nibbles */
      *(out + (j + 1)) = nibb & NIB_MASK;  /* mask and store second nibble */ 
      *(out + j) = nibb >> 4;              /* shift and store first nibble */ 
    }
  if (nbytes_out%2)                        /* if odd number of bytes */     
    {
      nibb = *(in + i);                    /* get last byte with nibble */  
      *(out + (2 * i)) = nibb >> 4;        /* shift and store last nibble */
    }
  return;
}
