#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* If an output file's label has been modified, then it has been retained */
/* in memory; at close time, this routine is called to write the modified */
/* label back out to the file.  The label is modified by this routine,	  */
/* then the label memory is deallocated.				  */

int v2_check_out_lbl_on_close(int unit)
{
   int i, status;
   int lblsize, new_lblsize, eol_size;
   int eol_rec, recsize, nrecs, left;
   char *labels, *eol_labels, *p;

   recsize = CURRENT_I_VALUE(RECSIZE);

   eol_labels = NULL;

   lblsize = CURRENT_I_VALUE(LBLSIZE);
   labels = CURRENT_S_VALUE(LABELS);

   new_lblsize = strlen(labels);

   if (new_lblsize >= lblsize) {	/* Must split off an EOL group */

      p = v2_place_cut(labels, lblsize);	/* Get start of EOL section */

      if (strlen(p) != 0) {		/* make sure there's something there! */

         eol_labels = malloc(strlen(p) + LABEL_SIZE_ITEM + LABEL_BUFFER_EXTRA +
				recsize);
         if (eol_labels == NULL)		/* Create EOL label space */
            return NO_MEMORY_FOR_LABEL_PROCESS;

         memset(eol_labels, 0, LABEL_SIZE_ITEM);
         strcpy(eol_labels + LABEL_SIZE_ITEM, p); /* put labels there */
         v2_add_label_size_item(eol_labels, 0);	/* put placeholder in */
         strcat(eol_labels, "  ");		/* leave space after last one */

         memset(p, 0, strlen(p));     /* blank out EOL labels in main section */
      }
   }

   v2_add_label_size_item(labels, lblsize);

   if (eol_labels != NULL) 
      v2_set_eol(labels);
   else
      v2_clear_eol(labels);	/* make sure it's unset if the label shrunk! */

   new_lblsize = strlen(labels);

   /* Write out main labels */

   left = lblsize - new_lblsize;
   if (left > 0)    /* Zero out the part of the label that contains no data */
      memset(labels+new_lblsize, 0, left);

   nrecs = lblsize / recsize;

   for (i=0; i<nrecs; i++) {		/* write out any full records first */
      status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE),
						labels, i,0,0, &no_trans);
      labels += recsize;
      if (status != SUCCESS) {
         if (eol_labels != NULL)
            free(eol_labels);
         return status;
      }
   }

   left = lblsize % recsize;		/* write out remaining partial rec */
   if (left != 0) {			/* (probably shouldn't happen) */
      status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE),
					labels, nrecs, 0, left, &no_trans);
      if (status != SUCCESS) {
         if (eol_labels != NULL)
            free(eol_labels);
         return status;
      }
   }

   if (eol_labels != NULL) {	/* A second set of labels exists */

      nrecs = (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3))
		+ CURRENT_I_VALUE(NLB);		/* number of recs */

      eol_rec = lblsize / recsize + nrecs;

      eol_size = strlen(eol_labels);	/* make the size a mult of recsize */
      lblsize = CEIL(eol_size, recsize) * recsize;
      CURRENT_I_VALUE(EOL_SIZE) = lblsize;

      v2_add_label_size_item(eol_labels, lblsize);

      left = lblsize - eol_size;
      if (left > 0)   /* Zero out the part of the label that contains no data */
         memset(eol_labels + eol_size, 0, left);

      nrecs = lblsize / recsize;
      labels = eol_labels;

      for (i=0; i<nrecs; i++) {		/* write out any full records first */
         status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE),
					labels, eol_rec+i, 0, 0, &no_trans);
         labels += recsize;
         if (status != SUCCESS) {
            if (eol_labels != NULL)
               free(eol_labels);
            return status;
         }
      }
   }

   if (eol_labels != NULL)
      free(eol_labels);

   if (CURRENT_S_VALUE(LABELS) != NULL) {
      free(CURRENT_S_VALUE(LABELS));
      CURRENT_S_VALUE(LABELS) = NULL;
      CURRENT_I_VALUE(LBLALLOC) = 0;
   }

   return SUCCESS;
}
