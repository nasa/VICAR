#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Data-type translation functions.  Functions are supplied to translate*/
/* between all data types:  byte==unsigned char, half==short int,	*/
/* full==int, real==float, doub==double, comp==struct v2_complex.	*/
/* It is a good idea to re-write at least some of these in assembler	*/
/* code, in a host-dependent file (like "convert_vms.mar"), then use	*/
/* #if's to remove the functions here that are not needed.		*/

/************************************************************************/
/************************************************************************/

#if !VAX_ARCH

int v2_byte2half(void *fromi, void *toi, int len, 
	      struct trans *UNUSED(trans))
{
   unsigned char *from = (unsigned char *) fromi;
   short int *to = (short int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (short int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_byte2full(void *fromi, void* toi, int len, 
	      struct trans *UNUSED(trans))
{
   unsigned char *from = (unsigned char *) fromi;
   int *to = (int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_byte2real(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   unsigned char *from = (unsigned char *) fromi;
   float *to = (float *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (float)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_byte2doub(void *fromi, void* toi, int len, struct trans* UNUSED(trans))
{
   unsigned char *from = (unsigned char *) fromi;
   double *to = (double *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (double)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_byte2comp(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   unsigned char *from = (unsigned char *) fromi;
   struct v2_complex *to = (struct v2_complex *) toi;
   register int i;

   for (i=0; i<len; i++) {
      to->r = (float)(*from++);
      to->i = 0.0;
      to++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

int v2_half2byte(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   unsigned char *to = (unsigned char *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (unsigned char)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_half2full(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   int *to = (int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_half2real(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   float *to = (float *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (float)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_half2doub(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   double *to = (double *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (double)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_half2comp(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   short int *from = (short int *) fromi;
   struct v2_complex *to = (struct v2_complex *) toi;
   register int i;

   for (i=0; i<len; i++) {
      to->r = (float)(*from++);
      to->i = 0.0;
      to++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

int v2_full2byte(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   unsigned char *to = (unsigned char *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (unsigned char)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_full2half(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   short int *to = (short int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (short int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_full2real(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   float *to = (float *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (float)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_full2doub(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   double *to = (double *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (double)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_full2comp(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   int *from = (int *) fromi;
   struct v2_complex *to = (struct v2_complex *) toi;
   register int i;

   for (i=0; i<len; i++) {
      to->r = (float)(*from++);
      to->i = 0.0;
      to++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

int v2_real2byte(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   float *from = (float *) fromi;
   unsigned char *to = (unsigned char *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (unsigned char)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_real2half(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   float *from = (float *) fromi;
   short int *to = (short int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (short int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_real2full(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   float *from = (float *) fromi;
   int *to = (int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_real2doub(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   float *from = (float *) fromi;
   double *to = (double *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (double)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_real2comp(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   float *from = (float *) fromi;
   struct v2_complex *to = (struct v2_complex *) toi;
   register int i;

   for (i=0; i<len; i++) {
      to->r = (float)(*from++);
      to->i = 0.0;
      to++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

int v2_doub2byte(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   unsigned char *to = (unsigned char *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (unsigned char)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_doub2half(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   short int *to = (short int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (short int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_doub2full(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   int *to = (int *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (int)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_doub2real(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   float *to = (float *) toi;
   register int i;

   for (i=0; i<len; i++)
      (*to++) = (float)(*from++);
   return SUCCESS;
}

/************************************************************************/

int v2_doub2comp(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   double *from = (double *) fromi;
   struct v2_complex *to = (struct v2_complex *) toi;
   register int i;

   for (i=0; i<len; i++) {
      to->r = (float)(*from++);
      to->i = 0.0;
      to++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

int v2_comp2byte(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   struct v2_complex *from = (struct v2_complex *) fromi;
   unsigned char *to = (unsigned char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      (*to++) = (unsigned char)from->r;
      from++;
   }
   return SUCCESS;
}

/************************************************************************/

int v2_comp2half(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   struct v2_complex *from = (struct v2_complex *) fromi;
   short int *to = (short int *) toi;
   register int i;

   for (i=0; i<len; i++) {
      (*to++) = (short int)from->r;
      from++;
   }
   return SUCCESS;
}

/************************************************************************/

int v2_comp2full(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   struct v2_complex *from = (struct v2_complex *) fromi;
   int *to = (int *) toi;
   register int i;

   for (i=0; i<len; i++) {
      (*to++) = (int)from->r;
      from++;
   }
   return SUCCESS;
}

/************************************************************************/

int v2_comp2real(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   struct v2_complex *from = (struct v2_complex *) fromi;
   float *to = (float *) toi;
   register int i;

   for (i=0; i<len; i++) {
      (*to++) = (float)from->r;
      from++;
   }
   return SUCCESS;
}

/************************************************************************/

int v2_comp2doub(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   struct v2_complex *from = (struct v2_complex *) fromi;
   double *to = (double *) toi;
   register int i;

   for (i=0; i<len; i++) {
      (*to++) = (double)from->r;
      from++;
   }
   return SUCCESS;
}

/************************************************************************/
/************************************************************************/

#endif /* !VAX_ARCH */

