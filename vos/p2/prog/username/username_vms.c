/* 02 JAN 1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING) */
#include  "vicmain_c"
#include  <descrip.h>
#include  <stdio.h>
/* #undef I_PGMINC */		/* TEMP KLUDGE to allow .INC instead of .CIN */
#include  "pgminc.inc"
#define XPRDIM	(5300/4)	/* Size of the parblock in ints */

void main44(void)
{
   char  *cuserid();
   static char string[L_cuserid] = "";
   char  *p_userid;
   int   vblock[XPRDIM];
   int   status;
   struct dsc$descriptor_s  userid_desc;

   zifmessage ("USERNAME version 02-JAN-95");

/* Get the user name */

   p_userid = cuserid(string);

/* Create a descriptior for the user id */

   userid_desc.dsc$w_length = strlen(p_userid);
   userid_desc.dsc$a_pointer = p_userid;
   userid_desc.dsc$b_class = DSC$K_CLASS_S;
   userid_desc.dsc$b_dtype = DSC$K_DTYPE_T;

/* Do the TAE stuff to pass the name back to the TM process */

     q_init(vblock, &XPRDIM, &P_ABORT);
     status = q_string(vblock, "NAME", 1, &p_userid, P_ADD);
     status = zvq_out(vblock); 
}
