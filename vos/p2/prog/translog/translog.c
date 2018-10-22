#include  "xvmaininc.h"
#include  "taeconf.inp"
#include  "pgminc.inc"
#include  "symtab.inc"
#include  "parblk.inc"
#include <string.h>

/* This program uses TAE directly instead of the RTL for no	*/
/* particularly good reason.  Historical value, I guess.	*/

main()
{
   struct PARBLK           vblock;

   int                     status, i;
   char                    *inptr, *outptr;
   char                    output[128];
   char                    input[128];
   int lines, cols, type;

   struct VARIABLE *var, *p_find();

   t_pinit(&lines, &cols, &type);	/* avoid screen clear on startup */
   p_inim(&vblock, P_BYTES, P_ABORT);

   /* Fetch logical name from parameter block */

   var = p_find(&vblock, "INP");
   inptr = SVAL(*var, 0);

   /* Convert string to upper case (VMS only) */

#if VMS_OS
   for (i=0; i<strlen(inptr); i++)
      input[i] = toupper(inptr[i]);
#else
   strcpy(input, inptr);
#endif

   /* Get translation */

   do_trans(input, output);

   /* Do the TAE stuff to pass the name back to the TM process */

   /* Note:  This is incompatible with shell-VICAR, due to the direct	*/
   /* use of TAE.  zvq_out() requires initialization which is done by	*/
   /* vicmain_c.  Since this program does TAE directly, zvq_out() is	*/
   /* unusable and q_out() must be used instead.  I could recode the	*/
   /* startup to be standard... but since all scripting languages have	*/
   /* better facilities than this for translating env vars/log names,	*/
   /* it hardly seems worth it.  Small loss. (rgd 11/97)		*/

   q_init(&vblock, P_BYTES, P_ABORT);
   outptr = output;
   q_string(&vblock, "TRANS", 1, &outptr, P_ADD);
   q_out(&vblock);
}


/* This is the routine that actually does the logical/env var translation */

#if VMS_OS

#include <descrip.h>

do_trans(input,output)
char *input, *output;
{
   struct dsc$descriptor_s input_desc;
   struct dsc$descriptor_s output_desc;
   int                     output_length;
   int                     status;

/* Set up Descriptor of input logical name */

   input_desc.dsc$w_length   = strlen(input);
   input_desc.dsc$a_pointer  = input;
   input_desc.dsc$b_class    = DSC$K_CLASS_S;
   input_desc.dsc$b_dtype    = DSC$K_DTYPE_T;

/* Set up Descriptor of output equivalence string */

   output_desc.dsc$w_length  = 128;
   output_desc.dsc$a_pointer = output;
   output_desc.dsc$b_class   = DSC$K_CLASS_S;
   output_desc.dsc$b_dtype   = DSC$K_DTYPE_T;

/* Get translation */

   status = sys$trnlog(&input_desc, &output_length, &output_desc, 0, 0, 0);
   output_desc.dsc$w_length = output_length;

/* Check for error */

   if (status == 1)
      output[output_length] = '\0';
   else
      output[0] = '\0';

   return;
}

#else		/* Unix version */

#include <stdlib.h>

do_trans(input,output)
char *input, *output;
{
   char *p;

   p = (char *)getenv(input);
   if (p == NULL)
      output[0] = '\0';
   else
      strcpy(output, p);
}

#endif

