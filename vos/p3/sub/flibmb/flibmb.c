#include <stdio.h>

#define C_MASK     0x7f000000
#define F_MASK     0x00ffffff
#define S_MASK     0x80000000
#define NORMALIZED 0x00800000

float flibmb_ (ibm_in)
float *ibm_in;
{
  unsigned long  c2_part, f_part, sign_bit;
  unsigned final = 0; /* Prepared for ORing */
  long c_part;
  float swapped;
  
  /* First, reverse byte order */
  swapped = intibm(ibm_in);

  /* Now, extract fields */

  c2_part = ( (unsigned long) swapped & C_MASK );
  f_part = ( (unsigned long) swapped & F_MASK );
  sign_bit = ( (unsigned long) swapped & S_MASK );

  c_part = c2_part;

  c_part >>= 24;           /* Make it usable */
  c_part -= 64;

  /* Normalize mantissa */

  while ( (f_part & NORMALIZED) != NORMALIZED ) {
    c_part--;
    f_part <<= 1; /* Shift one left and check again */
  }

  c_part +=  128;
  c_part <<= 23;
  
  final |= (sign_bit | c_part | f_part);
  
}
