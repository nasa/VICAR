/*******************************************************************************

Title:    endianness.c
Author:   Mike Burl
Date:     Mar 27, 2001
Function: Determine whether computer is using a big endian or little endian format.

History:  2001/08/16 (MCB) - Realized that change on 2001/06/05 broke shuffle_bytes
          behavior for fmt BYTE (because in previous version we just returned without
          doing anything). Since we're no longer shuffling in place, we need to do
          a straight copy in the BYTE fmt case.

          2001/06/05 (MCB) - Realized that there was an error in the logic of 
          shuffle_bytes. It was shufffling in place, which ruined the array
          for further use.
*******************************************************************************/
#include <stdio.h>
#include "endianness.h"
#include "qmalloc.h"
#include "utils_return_values.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/
union { long Long; char Char[sizeof(long)]; } u;

/**************************************/
/* endianness                         */
/**************************************/

int endianness(void)

{
  /*  char          infunc[] = "endianness"; */

/*--------------------------------------------------------------*/
  u.Long = 0x12345678;
  /*  for (i = 0; i < sizeof(long); i++) {
        printf("Char[%d] = %x\n", i, u.Char[i]);
      }
  */
  if ((u.Char[0]==0x12)&&(u.Char[1]==0x34)&&(u.Char[2]==0x56)&&(u.Char[3]==0x78)) {
    return(ENDIAN_BIG);
  }
  else if ((u.Char[0]==0x78)&&(u.Char[1]==0x56)&&(u.Char[2]==0x34)&&(u.Char[3]==0x12)) {
    return(ENDIAN_LITTLE);
  }
  else {
    return(ERR);
  }
}


/**************************************/
/* shuffle_bytes                      */
/**************************************/

int shuffle_bytes(int n_numbers, int bytes_per_num, unsigned char *ptr_in, unsigned char *ptr_out)

{

  int           i, j, jj;
  /*  char          infunc[] = "shuffle_bytes"; */

/*--------------------------------------------------------------*/
  if (bytes_per_num != 1) { /* only process multibyte data */
    jj = (bytes_per_num-1)/2;
    for (i = 0; i < n_numbers; i++) {
      /* Copy each number to buff */
      for (j = 0; j < bytes_per_num; j++) {
        ptr_out[i*bytes_per_num + j] = ptr_in[i*bytes_per_num + bytes_per_num-j-1];
      }
    }
  }
  else {
    /* Straight copy if bytes_per_num == 1; somewhat wasteful, calling 
    program should check and not call shuffle_bytes if format is BYTE */
    for (i = 0; i < n_numbers; i++) {
      ptr_out[i] = ptr_in[i];
    }
  }

  return(OK);
}
