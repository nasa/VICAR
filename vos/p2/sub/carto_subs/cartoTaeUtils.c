#include <stdio.h> /* necessary for taeextproto.h */
#include <string.h>

#include "zmabend.h"
#include "zvproto.h"
#include "taeconf.inp"
#include "taeextproto.h"
#include "parblk.inc"

//#include "carto/cartoTaeUtils.h"
//#include "carto/cartoVicarProtos.h"

/*======================================================

mq_out_int

output integer value to parameter block

arguments :

      1. pname: input, char *pname;
         Parameter name.

      2. val: output, int val;
         Value output to parameter block.

mq_out_int returns integer variables to the parameter block.
*/

void mq_out_int (char *pname, int val)
{
   int i_vec[1];
   struct PARBLK parblk;        /* TAE parameter block */

   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   i_vec[0] = val;
   q_intg(&parblk, pname, (FUNINT)1, i_vec, (FUNINT)P_ADD);
   zvq_out(&parblk);
}

/*======================================================

mq_out_real

output real value to parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, double val;
	 Value output to parameter block.

mq_out_real returns integer variables to the parameter block.  The name
and value are output to the print log.
*/

void mq_out_real (char *pname, double val)
{
   struct VARIABLE *p_find();
   double r_vec[1];
   struct PARBLK parblk;	/* TAE parameter block */
   
   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   r_vec[0] = val;
   q_real(&parblk, pname, (FUNINT)1, r_vec, (FUNINT)P_ADD);
   zvq_out(&parblk);
}

/*======================================================

mq_out_string

returns character string to the parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, char *val;
	 String returned to parameter block.

      3. maxlen: input, int maxlen;
	 max length to put to output parameter block

mq_out_string returns character strings to the
parameter block.
*/

void mq_out_string (char *pname, char *val, int maxlen)
{
   char *t_vec[1];
   int k;
   struct PARBLK parblk;	/* TAE parameter block */
   
   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   if (maxlen+1 >= 132) zmabend("error in mq_out_string");
   k = strlen(val);
   if (k>maxlen)
      {
      printf("string truncated\n");
      k = maxlen;
      }

   t_vec[0] = (char *)malloc(sizeof(char)*(k+1));
   strncpy(t_vec[0],val,k+1);
   t_vec[0][k] = '\0';
   q_string(&parblk,pname,(FUNINT)1,t_vec,(FUNINT)P_ADD);
   zvq_out(&parblk);
}
