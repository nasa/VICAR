#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Byte-swapping host format conversion routines.  They are currently	*/
/* used for all integer format conversions, and for real and doub	*/
/* between ieee and rieee only.						*/
/* It is a good idea to re-write at least some of these in assembler	*/
/* code, in a host-dependent file (like "int_convert_vms.mar"), then	*/
/* use #if's to remove the functions here that are not needed.		*/

int v2_trans_swap2(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   short int *to = (short int *) toi;
   register int i;
   register unsigned char temp;
   union short_u {
      short int i;
      unsigned char b[2];
   } u;

   for (i=0; i<len; i++) {
      memcpy(&u.i, from++, sizeof(short int));	/* in case an odd address */
      temp = u.b[0];
      u.b[0] = u.b[1];
      u.b[1] = temp;
      memcpy(to++, &u.i, sizeof(short int));
   }
   return SUCCESS;
}

int v2_trans_swap4(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   int *to = (int *) toi;
   register int i;
   register unsigned char temp;
   union long_u {
      int i;
      unsigned char b[4];
   } u;

   for (i=0; i<len; i++) {
      memcpy(&u.i, from++, sizeof(int));	/* in case an odd address */
      temp = u.b[0];
      u.b[0] = u.b[3];
      u.b[3] = temp;
      temp = u.b[1];
      u.b[1] = u.b[2];
      u.b[2] = temp;
      memcpy(to++, &u.i, sizeof(int));
   }
   return SUCCESS;
}

int v2_trans_swap8(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   double *to = (double *) toi;
   register int i;
   register unsigned char temp;
   union long_u {
      double i;
      unsigned char b[8];
   } u;

   for (i=0; i<len; i++) {
      memcpy(&u.i, from++, sizeof(double));	/* in case an odd address */
      temp = u.b[0];
      u.b[0] = u.b[7];
      u.b[7] = temp;
      temp = u.b[1];
      u.b[1] = u.b[6];
      u.b[6] = temp;
      temp = u.b[2];
      u.b[2] = u.b[5];
      u.b[5] = temp;
      temp = u.b[3];
      u.b[3] = u.b[4];
      u.b[4] = temp;
      memcpy(to++, &u.i, sizeof(double));
   }
   return SUCCESS;
}

