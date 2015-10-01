#include "xvmaininc.h"
#include "rtlintproto.h"

#if RTL_USE_TAPE

#if RTL_USE_TAE
#else			/* force a compile error */
	!!! ERROR:  if RTL_USE_TAPE is set, RTL_USE_TAE must also be set !!!
#endif

#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "taeextproto.h"

/* Routines to handle the TAE global tape variables. */

/* The variables $TAPES, $TFILE, and $TREC are TAE global variables	*/
/* that control tape mounts, allocates, and tape position.  Local	*/
/* copies of these are kept for every application in the C global	*/
/* variables i_tape, i_file, and i_rec, respectively.  The TAE		*/
/* variables must be copied to the i_* variables on program start, and	*/
/* sent back out on program exit.					*/

/************************************************************************/
/* Copy the TAE globals into the C globals at program startup.		*/
/************************************************************************/

void v2_i_init(
   struct PARBLK *parblk		/* In: TM parameter block */
)

{
   int i;
   struct VARIABLE *v;

   for (i=0; i < MAXTAPES; i++) {
      i_open[i] = FALSE;		/* mark all tapes as not open */
      i_tape[i] = NULL;
   }

   i_count = 0;

   v = p_fvar(parblk, "$TFILE");
   if (v == NULL)
      return;

   for (i=0; i < v->v_count; i++)
      i_file[i] = IVAL(*v, i);

   v = p_fvar(parblk, "$TREC");
   if (v == NULL)
      return;

   for (i=0; i < v->v_count; i++)
      i_rec[i] = IVAL(*v, i);

   v = p_fvar(parblk, "$TAPES");
   if (v == NULL)
      return;

   for (i=0; i < v->v_count; i++) {
      i_tape[i] = (char *)malloc(strlen(SVAL(*v,i))+1);
      if (i_tape[i] == NULL) {
         for (i=0; i<MAXTAPES; i++)		/* clean up */
            if (i_tape[i] != NULL) free(i_tape[i]);
         return;
      }
      strcpy(i_tape[i], SVAL(*v, i));
   }
   i_count = v->v_count;

   for (i = 0; i < i_count; i++) {		/* define "next" files */
      if (i_rec[i] == 1)			/* if at beginning of a file */
         i_nxt_file[i] = i_file[i];
      else
         i_nxt_file[i] = i_file[i] + 1;
   }

   v2_exit_handler (v2_i_exit);		/* declare exit handler	to send it all back */

   return;
}

/************************************************************************/
/* Exit handler.  Here we make sure that the tape housekeeping globals	*/
/* get returned to TAE.   This function is called before image exit.	*/
/************************************************************************/

void v2_i_exit(int UNUSED(a), void * UNUSED(b))

{
   int i;
   struct PARBLK parblk;

   for (i=0; i < i_count; i++) {
      if (i_open[i]) {
         if (i_rec[i] == 0)	/* don't know if OS writes the double eof */
            i_file[i] = 0;
         else if (i_rec[i] > 1) { /* if at least one record written, */
            i_file[i]++;	  /* close writes double eof and backspaces */
            i_rec[i] = 1;
         }
      }
   }

   q_init(&parblk, P_BYTES, P_ABORT);
   q_intg(&parblk, "$TFILE", i_count, i_file, P_ADD);
   q_intg(&parblk, "$TREC",  i_count, i_rec, P_ADD);
   q_string(&parblk, "$TAPES", i_count, i_tape, P_ADD);
   zvq_out (&parblk);

   return;
}

#else

void v2_i_init(struct PARBLK * UNUSED(parblk))
{
   return;
}

void v2_i_exit(int UNUSED(a), void * UNUSED(b))
{
   return;
}

#endif

