#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#define BSQ 1	/* define some integer values for the file organization to */
#define BIL 2	/* simplify the checks.					   */
#define BIP 3

/* Open an input file */

int v2_process_input_file(int unit)
{
   int status;
   int org_int;		/* integer value describing ORG; BSQ, BIL, or BIP */
   char *org_value;
   int n1,n2,n3,u_n1,u_n2,u_n3;		/* local versions for unlabeled files */
   int u_n1_present;
   struct bufstate *bufstate;

   bufstate = (struct bufstate *)malloc(sizeof(struct bufstate));
   if (bufstate == NULL)
      return INSUFFICIENT_MEMORY;
   CURRENT_IP_VALUE(BUFSTATE) = (int *)bufstate;

   memset(bufstate, 0, sizeof(struct bufstate));

   status = v2_open_input_file(unit);	/* open the file for input */
   if (status != SUCCESS) {
      v2_close_down(unit);
      return status;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & NO_LABELS)) {

      /* Initialize current unit table from the input label. */

      status = v2_initialize_from_label(unit);
      if (status != SUCCESS) {
         v2_close_down(unit);
	 if (status == FAILURE)
	    return BAD_INPUT_LABEL;		/* For unknown failure */
	 else
            return status;
      }

      /* Compute the bytes per pixel */

      CURRENT_I_VALUE(PIX_SIZE) = v2_bytes_per_pixel(CURRENT_S_VALUE(FORMAT),
		   CURRENT_S_VALUE(INTFMT), CURRENT_S_VALUE(REALFMT), &status);
      if (status != SUCCESS)
         return status;

/* If the ORG string was given with the U_ORG optional, we must */
/* guarantee that ORG from the label matches U_ORG.		*/

      if (strlen(CURRENT_S_VALUE(U_ORG)) != 0) {	/* If U_ORG given... */
	 if (!EQUAL(CURRENT_S_VALUE(U_ORG),CURRENT_S_VALUE(ORG)))
	    return ORG_MISMATCH;
      }
      if (EQUAL(CURRENT_S_VALUE(ORG), "BSQ"))
         org_int = BSQ;
      else if (EQUAL(CURRENT_S_VALUE(ORG), "BIL"))
         org_int = BIL;
      else if (EQUAL(CURRENT_S_VALUE(ORG), "BIP"))
         org_int = BIP;
      else
         org_int = 0;		/* bad value, will be trapped below */
   }
   else {		/* COND NOLABELS was given, so treat as unlabeled */

      /* If I_FORMAT is not null it means that the programmer	*/
      /* wants to override the input FORMAT			*/

      if (strlen(CURRENT_S_VALUE(I_FORMAT)) != 0)
         v2_add_str_current_table(CURRENT_S_VALUE(I_FORMAT), FORMAT,
				     current_table[unit], default_table);

      CURRENT_I_VALUE(PIX_SIZE) = v2_bytes_per_pixel(CURRENT_S_VALUE(FORMAT),
		   CURRENT_S_VALUE(INTFMT), CURRENT_S_VALUE(REALFMT), &status);
      if (status != SUCCESS)
         return status;

/* Add the proper value for the file organization to the current table   */
/* If the user gave U_ORG, then use that value, otherwise default to BSQ */

      org_value = "BSQ";
      if (strlen(CURRENT_S_VALUE(U_ORG)) != 0)
         org_value = CURRENT_S_VALUE(U_ORG);
      status = v2_add_str_current_table(org_value, ORG, current_table[unit],
							   default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;

      if (EQUAL(CURRENT_S_VALUE(ORG), "BSQ")) {
         org_int = BSQ;
	 u_n1 = CURRENT_I_VALUE(U_NS);
	 u_n2 = CURRENT_I_VALUE(U_NL);
	 u_n3 = CURRENT_I_VALUE(U_NB);
      }
      else if (EQUAL(CURRENT_S_VALUE(ORG), "BIL")) {
         org_int = BIL;
	 u_n1 = CURRENT_I_VALUE(U_NS);
	 u_n2 = CURRENT_I_VALUE(U_NB);
	 u_n3 = CURRENT_I_VALUE(U_NL);
      }
      else if (EQUAL(CURRENT_S_VALUE(ORG), "BIP")) {
         org_int = BIP;
	 u_n1 = CURRENT_I_VALUE(U_NB);
	 u_n2 = CURRENT_I_VALUE(U_NS);
	 u_n3 = CURRENT_I_VALUE(U_NL);
      }
      else {
         return BAD_ORG;
      }

      if (u_n1 != 0)		/* Ignore device recsize if u_n1 given */
         CURRENT_I_VALUE(RECSIZE) =
	    u_n1 * CURRENT_I_VALUE(PIX_SIZE) + CURRENT_I_VALUE(NBB);
      if (CURRENT_I_VALUE(RECSIZE) == 0)
         return IMAGE_SIZE_REQUIRED;

      if (u_n1 != 0)
         n1 = u_n1;
      else
         n1 = (CURRENT_I_VALUE(RECSIZE) - CURRENT_I_VALUE(NBB)) /
	       CURRENT_I_VALUE(PIX_SIZE);

      if (u_n3 <= 0)
         u_n3 = 1;
      n3 = u_n3;

      if (u_n2 != 0)
         n2 = u_n2;
      else    /* eof_record is in bytes until v2_initialize_buffer is called */
         n2 = (((struct bufstate *)CURRENT_IP_VALUE(BUFSTATE))->eof_record) /
              (CURRENT_I_VALUE(PIX_SIZE) * n1 * n3);

/* Now copy the n1 and n3 values back to the proper places */
      switch (org_int) {
	 case BSQ : CURRENT_I_VALUE(NS) = n1;
		    CURRENT_I_VALUE(NL) = n2;
		    CURRENT_I_VALUE(U_NB) = u_n3;
		    CURRENT_I_VALUE(NB) = n3;
		    break;
	 case BIL : CURRENT_I_VALUE(NS) = n1;
		    CURRENT_I_VALUE(NB) = n2;
		    CURRENT_I_VALUE(U_NL) = u_n3;
		    CURRENT_I_VALUE(NL) = n3;
		    break;
	 case BIP : CURRENT_I_VALUE(NB) = n1;
		    CURRENT_I_VALUE(NS) = n2;
		    CURRENT_I_VALUE(U_NL) = u_n3;
		    CURRENT_I_VALUE(NL) = n3;
      }
   }		/* End of unlabeled section */

/* Now copy the image sizes to N1..N3 in order to not have to worry	*/
/* about the file organization.  N1 is the fastest changing, etc.  From	*/
/* this point on, N1..N3 ONLY are used.					*/
/* Also, set up pointers to the values needed in xvread and xvwrit so	*/
/* that the org does not need to be checked each time.			*/
/* The three orgs allowed are BSQ (band sequential), BIL (band		*/
/* interleaved by line), and BIP (band interleaved by pixel).		*/

   if (org_int == BSQ) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NL);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NB);

      CURRENT_IP_VALUE(SLICE1) = & CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = & CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(SLICE3) = & CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(NSLICE1) = & CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = & CURRENT_I_VALUE(NLINES);
      CURRENT_IP_VALUE(NSLICE3) = & CURRENT_I_VALUE(NBANDS);
   }

   else if (org_int == BIL) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NB);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NL);

      CURRENT_IP_VALUE(SLICE1) = & CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = & CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(SLICE3) = & CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(NSLICE1) = & CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = & CURRENT_I_VALUE(NBANDS);
      CURRENT_IP_VALUE(NSLICE3) = & CURRENT_I_VALUE(NLINES);
   }
   else if (org_int == BIP) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NB);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NL);

      CURRENT_IP_VALUE(SLICE1) = & CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(SLICE2) = & CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE3) = & CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(NSLICE1) = & CURRENT_I_VALUE(NBANDS);
      CURRENT_IP_VALUE(NSLICE2) = & CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE3) = & CURRENT_I_VALUE(NLINES);
   }
   else {				/* here if we do not recognize ORG */
      if (CURRENT_I_VALUE(DIM) != 2)   /* For 2D files let ORG default to BSQ */
         return BAD_ORG;

      status = v2_add_str_current_table("BSQ", ORG, current_table[unit],
					   default_table);
      if (status != SUCCESS) 
         return UNABLE_TO_STORE_OPTIONAL;

      /* Force sizes to be correct for a 2D file */

      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NB) = 1;
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NL);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NB) = 1;

      CURRENT_IP_VALUE(SLICE1) = & CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = & CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(SLICE3) = & CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(NSLICE1) = & CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = & CURRENT_I_VALUE(NLINES);
      CURRENT_IP_VALUE(NSLICE3) = & CURRENT_I_VALUE(NBANDS);
   }

   u_n1_present = (CURRENT_I_VALUE(U_N1) > 0);

   /* If no label processing desired then create necessary items from	*/
   /* the CURRENT_VALUE table.						*/

   if ((CURRENT_I_VALUE(U_N1)==0) && (CURRENT_I_VALUE(RECSIZE)==0))
      CURRENT_I_VALUE(RECSIZE) =
		((struct bufstate *)CURRENT_IP_VALUE(BUFSTATE))->blocksize;
   else {
      if (CURRENT_I_VALUE(U_N1) == 0) {
         CURRENT_I_VALUE(N1) = (CURRENT_I_VALUE(RECSIZE)-CURRENT_I_VALUE(NBB)) /
				CURRENT_I_VALUE(PIX_SIZE);
         CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(N1);
      }
      else {
         if (CURRENT_I_VALUE(RECSIZE) == 0) {
            CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(U_N1);
            CURRENT_I_VALUE(RECSIZE) = CURRENT_I_VALUE(U_N1) *
					CURRENT_I_VALUE(PIX_SIZE) +
					CURRENT_I_VALUE(NBB);
         }
      }
   }

   /* The following statement ensures that unblocked, unlabeled files	*/
   /* will have equal block and record sizes.				*/

   if ((CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
       (CURRENT_I_VALUE(FLAGS) & NOBLOCK) &&
       u_n1_present) {

      CURRENT_I_VALUE(RECSIZE) = 
			CURRENT_I_VALUE(U_N1) * CURRENT_I_VALUE(PIX_SIZE) +
			CURRENT_I_VALUE(NBB);

      CURRENT_I_VALUE(BUFSIZE) = CURRENT_I_VALUE(RECSIZE);

      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(U_N1);
   }

   /* If U_FORMAT has not been entered by the programmer, then default it */

   if (strlen(CURRENT_S_VALUE(U_FORMAT))==0)
      v2_add_str_current_table(CURRENT_S_VALUE(FORMAT), U_FORMAT,
				  current_table[unit], default_table);

   v2_initialize_buffer(unit);
   if (status != SUCCESS) {
      v2_close_down(unit);
      return status;
   }

   v2_collect_history_info();       /* Collect the information that   */
                                    /* will become the history label. */

   if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE")) {
      v2_get_eol_size(unit);        /* Get size of EOL labels for EOF mark */
   }

   /* See if we are in UPDATE mode and if so, also check if the		*/
   /* UPD_HIST flag is set.  If so, add the history label to the	*/
   /* current unit.							*/
   if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE") &&
                EQUAL(CURRENT_S_VALUE(UPD_HIST), "ON")) {
      v2_add_hist_task(unit);      /* Add history label to current unit */
      if (status != SUCCESS) {
         v2_close_down(unit);
         return status;
      }
      CURRENT_I_VALUE(FLAGS) |= LABELS_MODIFIED;
   }

   /* See if this unit is the primary input and if so then establish    */
   /* the primary input environment.                                    */

   status=v2_check_primary_input(unit);
   if (status != SUCCESS) {
      v2_close_down(unit);
      return status;
   }

   CURRENT_I_VALUE(FLAGS) |= OPEN;      /* Mark the file as being open. */

   return SUCCESS;
}

