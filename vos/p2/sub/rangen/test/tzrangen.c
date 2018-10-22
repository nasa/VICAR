#include <stdio.h>
#include "vicmain_c"

/* "C" test routine to test the rangen subroutine. */
void main44()
{
 unsigned long seed;  /* input and returned seed */ 
 float rand_num;        /* returned random number */
 char string[80];
 int i;
 seed = 1073741969;
 for (i=1; i<25 ; i++ )
 { 
    zrangen(&seed,&rand_num);
    sprintf(string,"%8d    %15.14f\n",seed,rand_num);
    if ( ( i< 51) || (i> 49974) )
      { 
        sprintf(string, "%8d    %15.14f", seed,rand_num);
        zvmessage(string,"");
      } 
 } 
}
