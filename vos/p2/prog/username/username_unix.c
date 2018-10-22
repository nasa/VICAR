/* 02 JAN 1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING) */
#include  <stdio.h>
#include  <stdlib.h>
#include  "vicmain_c"
#include  "pgminc.inc"
#define XPRDIM	(5300/4)	/* Size of the parblock in ints */

void main44(void)
{
   char  *cuserid();
   static char name_desc[5] ;
   char  *p_userid, *s;
   char   vblock[XPRDIM];
   int   xprdim, p_abort, count, status;

   zifmessage ("USERNAME version 02-JAN-95");
   xprdim = XPRDIM;
   p_abort = P_ABORT;
   count = 1;

/* Get the user name */

   p_userid = getenv("USER");

/* Do the TAE stuff to pass the name back to the TM process */

     q_init(vblock, &xprdim, &p_abort);
     status = q_string(vblock, "NAME", count, &p_userid, P_ADD);
     status = zvq_out(vblock); 
}
