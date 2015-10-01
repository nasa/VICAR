#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/* Returns image size values from command line and primary	*/
/* input label.  The first 4 values are determined from the	*/
/* user parameters SIZE, SL, SS, NL, and NS.  SIZE is looked	*/
/* for first; if found, the others are ignored.  The last two	*/
/* values are determined from the primary input file.		*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvsize, XVSIZE) (
   int *sl,		/* Out: starting line (default=1) */
   int *ss,		/* Out: starting sample (default=1) */
   int *nl,		/* Out: number of output lines (default=nli) */
   int *ns,		/* Out: number of output samples (default=nsi) */
   int *nli,		/* Out: number of input lines (default=0) */
   int *nsi		/* Out: number of input lines (default=0) */
)

{

#if VMS_OS		/* must allow nli & nsi to be optional for VMS.. grrr */
   int dum1, dum2;
   if (n_args() < 5)	/* hopefully this is temporary only */
      zvsize(sl, ss, nl, ns, &dum1, &dum2);
   else if (n_args() < 6)
      zvsize(sl, ss, nl, ns, nli, &dum2);
   else
      zvsize(sl, ss, nl, ns, nli, nsi);
#else
   zvsize(sl, ss, nl, ns, nli, nsi);
#endif

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvsize(
   int *sl,		/* Out: starting line (default=1) */
   int *ss,		/* Out: starting sample (default=1) */
   int *nl,		/* Out: number of output lines (default=nli) */
   int *ns,		/* Out: number of output samples (default=nsi) */
   int *nli,		/* Out: number of input lines (default=0) */
   int *nsi		/* Out: number of input lines (default=0) */
)
{
   int status;
   int tempnl, tempns, temp;

   /* Set the defaults */

   *ss = 1;
   *sl = 1;
   *ns = 0;
   *nl = 0;
   *nli = 0;
   *nsi = 0;

   /* If there is a primary input, read in its attributes */

   status = v2_est_primary_input();	/* Ignore any errors */
   if (status == SUCCESS) {
         *nl = PRIMARY_I_VALUE(NL);
         *nli = *nl;
         *ns = PRIMARY_I_VALUE(NS);
         *nsi = *ns;
   }

   /* Get values from user parameters */

   status = v2_get_one_int_parm("SIZE", 2, &tempnl);
   if (status != SUCCESS)
      tempnl = 0;
   status = v2_get_one_int_parm("SIZE", 3, &tempns);
   if (status != SUCCESS)
      tempns = 0;
   if (tempnl != 0 && tempns != 0) {
      v2_get_one_int_parm("SIZE", 0, sl);
      v2_get_one_int_parm("SIZE", 1, ss);
      *nl = tempnl;
      *ns = tempns;
   }
   else {		/* If NL or NS == 0 in SIZE, assume it is defaulted */
      status = v2_get_one_int_parm("SL", 0, &temp);
      if (temp > 0 && status == SUCCESS)
         *sl = temp;
      status = v2_get_one_int_parm("SS", 0, &temp);
      if (temp > 0 && status == SUCCESS)
         *ss = temp;
      status = v2_get_one_int_parm("NL", 0, &temp);
      if (temp > 0 && status == SUCCESS)
         *nl = temp;
      status = v2_get_one_int_parm("NS", 0, &temp);
      if (temp > 0 && status == SUCCESS)
         *ns = temp;
   }

   return;
}
