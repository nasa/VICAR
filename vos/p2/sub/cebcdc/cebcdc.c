/*---------------------------  cebcdc     ------------------------
 * CEBCDC  (EBCDIC to ASCII)
 *
 *  REVISION HISTORY
 *    10-94 CRI MSTP S/W Conversion (VICAR Porting), changed to use C
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                           nibble

	General routine for converting an array of EBCDIC characters
          into an array of ASCII characters

	Fortran format of call:

          CALL CEBCDC( BUF,NCHAR)

        "C" format of call:

          zcebcdc(buf, nchar);
 
	Parameters:-

	BUF - character declared array containing the EBCDIC
        NCHAR - is the number of characters

--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"

void zcebcdc(unsigned char *buf, int nchar);
/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(cebcdc, CEBCDC) (buf, nchar)
     unsigned char *buf;
     int *nchar;
{
   zcebcdc( buf, *nchar);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zcebcdc(unsigned char *buf, int nchar)
{
  unsigned char ebcdic_table[256] = {
/*                          ROW    Conversion results are in decimal
   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   A,  B,  C,  D,  E,   F
                                                                            */
  00,  01,  02,  03,  92,   9,  92, 127,  92,  92,  92, 11, 12, 13, 14,  15,
  16,  17,  18,  19,  92,  92,   8,  92,  24,  25,  92, 92, 28, 29, 30,  31,
  92,  92,  92,  92,  92,  10,  23,  27,  92,  92,  92, 92, 92, 05, 06,  07,
  92,  92,  22,  92,  92,  92,  92,  04,  92,  92,  92, 92, 20, 21, 92,  26,
  32,  92,  92,  92,  92,  92,  92,  92,  92,  92,  91, 46, 60, 40, 43,  33,
  38,  92,  92,  92,  92,  92,  92,  92,  92,  92,  93, 36, 42, 41, 59,  94,
  45,  47,  92,  92,  92,  92,  92,  92,  92,  92, 124, 44, 37, 95, 62,  63,
  92,  92,  92,  92,  92,  92,  92,  92,  92,  96,  58, 35, 64, 39, 61,  34,
  92,  97,  98,  99, 100, 101, 102, 103, 104, 105,  92, 92, 92, 92, 92,  92,
  92, 106, 107, 108, 109, 110, 111, 112, 113, 114,  92, 92, 92, 92, 92,  92,
  92, 126, 115, 116, 117, 118, 119, 120, 121, 122,  92, 92, 92, 92, 92,  92,
  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, 92, 92, 92, 92,  92,
 123,  65,  66,  67,  68,  69,  70,  71,  72,  73,  92, 92, 92, 92, 92,  92,
 125,  74,  75,  76,  77,  78,  79,  80,  81,  82,  92, 92, 92, 92, 92,  92,
  92,  92,  83,  84,  85,  86,  87,  88,  89,  90,  92, 92, 92, 92, 92,  92,
  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  92, 92, 92, 92, 92, 255};

  register int i;

  for (i=0; i < nchar; i++)         /* for each character */
    {
      buf[i] = ebcdic_table[buf[i]];   /* get conversion and replace */
    }
  return;
}
