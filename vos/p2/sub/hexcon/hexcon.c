/*===========================================================================*
 |  HEXCON.C -- Routine to convert a hexadecimal array into ASCII (for       |
 |	printed output)							     |
 *===========================================================================*/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

void zhexcon(unsigned char[], unsigned char *, int *);

/*===========================================================================*
 |  Fortran Callable Subroutine                                              |
 *===========================================================================*/

void FTN_NAME2(hexcon, HEXCON) (unsigned char ibuf[], char *obuf, int *n,
								ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     unsigned char *c_string;
     c_string = (unsigned char *) malloc(2*(*n));
     zhexcon(ibuf,c_string, n);
     zsc2for((char *)c_string,2*(*n),obuf,c_string,3,2,1, n); 
     free(c_string);
     return;
}


/*===========================================================================*
 |  C-Callable Subroutine                                                    |
 *===========================================================================*/

void zhexcon(ibuf,obuf,n)
unsigned char ibuf[];		/* Input hexadecimal array */
unsigned char *obuf;		/* Output ASCII string */
int *n;				/* Number of bytes in ibuf */
{
	static char ascii[16]={'0','1','2','3','4','5','6','7',
			       '8','9','A','B','C','D','E','F' };
	int i,j,msbits,lsbits;

	for (i=0,j=0; i<*n; i++)
		{
 		msbits = ibuf[i]/16;
		lsbits = ibuf[i]%16;
		*obuf = ascii[msbits];
                obuf++;
                *obuf = ascii[lsbits];
                obuf++;
                }
}		
