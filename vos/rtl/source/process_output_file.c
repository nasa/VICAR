#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Open an output file.  Should be called only by xvopen. */

int v2_process_output_file(int unit)
{
   int status;
   int save_current;				/* To fool error_handler */
   char save_act[MAX_SHORT_STRING_SIZE];	/* To fool error_handler */
   struct bufstate *bufstate;
   int i;

  static int from_item[]={U_NL,U_NS,U_NB,U_N1,U_N2,U_N3,U_N4,U_DIM,U_NBB,U_NLB};
  static int to_item[]  ={  NL,  NS,  NB,  N1,  N2,  N3,  N4,  DIM,  NBB,  NLB};

   save_current = current_call;		/* Clear default error handling for  */
   strcpy(save_act, def_err_act);	/* xv routines so we can trap errors */
   strcpy(def_err_act, "");

/* The size of an output file can come from the following three places,	*/
/* in priority order (i.e., #2 is used only if #1 is not there)		*/
/* 1) from parameters to 'xvopen' (handled by 'process_optionals')	*/
/* 2) from special command line parameters, NL, NS, and SIZE		*/
/* 3) from the size of the primary input, if present (the routine	*/
/*    'est_primary_input' does that)					*/

   v2_get_out_size_from_parm(unit);

/* If there is a primary input, and the PI should be used, read in the	*/
/* attributes of the primary input and copy them into the output's	*/
/* value table.								*/

   status = v2_est_primary_input();
   if (status == SUCCESS)
      v2_copy_primary_input_val(unit);
   if (status == BAD_INPUT_LABEL)     /* if primary input is unlabeled or     */
      status = SUCCESS;		      /* disabled then continue without error */

   current_call = save_current;		/* Restore default error handling */
   strcpy(def_err_act,save_act);

   if (status != SUCCESS)
      return status;

/* Copy items from U_NL, etc. to NL, etc. */

   for (i=0; i < sizeof(from_item)/sizeof(int); i++) {
      if (CURRENT_I_VALUE(from_item[i]) != default_table[from_item[i]].ivalue)
         CURRENT_I_VALUE(to_item[i]) = CURRENT_I_VALUE(from_item[i]);
   }

   if (CURRENT_I_VALUE(NB) <= 0)	/* Guarantee a valid value for NB */
      CURRENT_I_VALUE(NB) = 1;

   if (strlen(CURRENT_S_VALUE(U_ORG)) != 0) {
      status = v2_add_str_current_table(CURRENT_S_VALUE(U_ORG), ORG,
					   current_table[unit], default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;
   }

/* Now copy the image sizes to N1...N3 in order to not have to worry	*/
/* about the file organization.  N1 is the fastest changing, etc.  From	*/
/* this point on, N1...N3 ONLY are used.				*/
/* The three organizations allowed are BSQ (Band SeQuential), BIL (Band	*/
/* Interleaved by Line), and BIP (Band Interleaved by Pixel).		*/

   if (EQUAL(CURRENT_S_VALUE(ORG),"BSQ")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NL);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NB);

      /* Save addresses of values needed in xvread and xvwrit so that	*/
      /* the org does not need to be checked each time.			*/

      CURRENT_IP_VALUE(SLICE1) = &CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = &CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(SLICE3) = &CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(NSLICE1) = &CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = &CURRENT_I_VALUE(NLINES);
      CURRENT_IP_VALUE(NSLICE3) = &CURRENT_I_VALUE(NBANDS);
   }
   else if (EQUAL(CURRENT_S_VALUE(ORG),"BIL")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NB);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NL);

      CURRENT_IP_VALUE(SLICE1) = &CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = &CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(SLICE3) = &CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(NSLICE1) = &CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = &CURRENT_I_VALUE(NBANDS);
      CURRENT_IP_VALUE(NSLICE3) = &CURRENT_I_VALUE(NLINES);
   }
   else if (EQUAL(CURRENT_S_VALUE(ORG),"BIP")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NB);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NL);

      CURRENT_IP_VALUE(SLICE1) = &CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(SLICE2) = &CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE3) = &CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(NSLICE1) = &CURRENT_I_VALUE(NBANDS);
      CURRENT_IP_VALUE(NSLICE2) = &CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE3) = &CURRENT_I_VALUE(NLINES);
   }
   else {		/* If we are here, then the input file has a bad ORG */

      if ((CURRENT_I_VALUE(DIM) != 2) || (CURRENT_I_VALUE(NB) != 1))
         return BAD_ORG;

      /* For values from an old 2D file, set to BSQ and set size accordingly */

      status = v2_add_str_current_table("BSQ", ORG, current_table[unit],
						    default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;

      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NB) = 1;
      CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(U_NS);
      CURRENT_I_VALUE(U_N2) = CURRENT_I_VALUE(U_NL);
      CURRENT_I_VALUE(U_N3) = CURRENT_I_VALUE(U_NB) = 1;

      CURRENT_IP_VALUE(SLICE1) = &CURRENT_I_VALUE(SAMP);
      CURRENT_IP_VALUE(SLICE2) = &CURRENT_I_VALUE(LINE);
      CURRENT_IP_VALUE(SLICE3) = &CURRENT_I_VALUE(BAND);
      CURRENT_IP_VALUE(NSLICE1) = &CURRENT_I_VALUE(NSAMPS);
      CURRENT_IP_VALUE(NSLICE2) = &CURRENT_I_VALUE(NLINES);
      CURRENT_IP_VALUE(NSLICE3) = &CURRENT_I_VALUE(NBANDS);
   }

/* If the U_NLB or U_NBB optionals have been entered, then the	*/
/* user wants to have binary access.				*/

   if ((CURRENT_I_VALUE(NLB) != default_table[NLB].ivalue) ||
       (CURRENT_I_VALUE(NBB) != default_table[NBB].ivalue))
      CURRENT_I_VALUE(FLAGS) |= BINARY;

/* If BIN_CVT is ON, then we must use native binary data formats, described */
/* by BHOST, BINTFMT, and BREALFMT.  If it is OFF, the binary data formats  */
/* are already set above (in priority order: optional arguments, primary    */
/* input, VAX-VMS format).						    */

   if (EQUAL(CURRENT_S_VALUE(BIN_CVT), "ON")) {
      status = v2_add_str_current_table(NATIVE_HOST_LABEL, BHOST,
				current_table[unit], default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;
      status = v2_add_str_current_table(NATIVE_INTFMT, BINTFMT,
				current_table[unit], default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;
      status = v2_add_str_current_table(NATIVE_REALFMT, BREALFMT,
				current_table[unit], default_table);
      if (status != SUCCESS)
         return UNABLE_TO_STORE_OPTIONAL;
   }

/* If O_FORMAT is not null, then the programmer has entered a desired	*/
/* output format, so set FORMAT.					*/

   if (strlen(CURRENT_S_VALUE(O_FORMAT)) != 0)
      status = v2_add_str_current_table(CURRENT_S_VALUE(O_FORMAT),FORMAT,
					current_table[unit], default_table);

/* U_FORMAT must be defaulted if not entered.  The earlier routine,	*/
/* v2_copy_primary_input_val, will ensure that U_FORMAT will equal the	*/
/* primary U_FORMAT if there is a primary; this code here defaults	*/
/* U_FORMAT if no primary exists.					*/

   if (strlen(CURRENT_S_VALUE(U_FORMAT)) == 0) {
      status = v2_add_str_current_table(CURRENT_S_VALUE(FORMAT),U_FORMAT,
					current_table[unit], default_table);
   }

   CURRENT_I_VALUE(PIX_SIZE) = v2_bytes_per_pixel(CURRENT_S_VALUE(FORMAT),
		   CURRENT_S_VALUE(INTFMT), CURRENT_S_VALUE(REALFMT), &status);
   if (status != SUCCESS)
      return status;

/* Reconcile U_N1, N1, and RECSIZE */

   if ((CURRENT_I_VALUE(U_N1)==0) && (CURRENT_I_VALUE(RECSIZE)==0))
      return IMAGE_SIZE_REQUIRED;
   else {
      if (CURRENT_I_VALUE(N1) == 0) {
         CURRENT_I_VALUE(N1)=(CURRENT_I_VALUE(RECSIZE) - CURRENT_I_VALUE(NBB)) /
				CURRENT_I_VALUE(PIX_SIZE);
         CURRENT_I_VALUE(U_N1) = CURRENT_I_VALUE(N1);
      }
      else {
         CURRENT_I_VALUE(RECSIZE) =
		CURRENT_I_VALUE(N1) * CURRENT_I_VALUE(PIX_SIZE) +
		CURRENT_I_VALUE(NBB);
      }
   }

/* Allocate the buffer */

   bufstate = (struct bufstate *)malloc(sizeof(struct bufstate));
   if (bufstate == NULL)
      return INSUFFICIENT_MEMORY;
   CURRENT_IP_VALUE(BUFSTATE) = (int *)bufstate;

   memset(bufstate, 0, sizeof(struct bufstate));

/* Finally, actually open the file */

   status = v2_open_output_file(unit);
   if (status != SUCCESS) {
      v2_close_down(unit);
      return status;
   }

   status = v2_initialize_buffer(unit);
   if (status != SUCCESS) {
      v2_close_down(unit);
      return status;
   }

   v2_collect_history_info();

   if (!(CURRENT_I_VALUE(FLAGS) & NO_LABELS)) {
      status = v2_create_output_label(unit);
      if (status != SUCCESS) {
         v2_close_down(unit);
         return status;
      }
   }

   CURRENT_I_VALUE(FLAGS) |= OPEN;

   return SUCCESS;

}
