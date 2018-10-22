/*****************************************************************************
  Title:    endianness.h
  Author:   M.C. Burl  
  Date:     Mar 27, 2001
  Function: Used in determining endianness of platform.

  History:  Changed API of shuffle_bytes to include a ptr_out. Previous API
            was shuffling in place which led to scrambled data
*****************************************************************************/
#define ENDIAN_BIG 1
#define ENDIAN_LITTLE 0

int endianness(void);
int shuffle_bytes(int n_numbers, int bytes_per_num, unsigned char *ptr_in, unsigned char *ptr_out);
