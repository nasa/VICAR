#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/* Returns band size values from command line and primary	*/
/* input label.  The first 2 values are determined from the	*/
/* user parameters BANDS, SB, and NB.  BANDS is looked for	*/
/* first; if found, the others are ignored.  The last value	*/
/* is determined from the primary input file.			*/


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvbands, XVBANDS) (int *sb, int *nb, int *nbi)
{

#if VMS_OS		/* must allow nbi to be optional for VMS... grrr */
   if (n_args() < 3)	/* hopefully this is temporary only */
      zvbands(sb, nb, &dum);
   else
      zvbands(sb, nb, nbi);
#else
   zvbands(sb, nb, nbi);
#endif

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvbands(
   int *sb,		/* Out: starting band (default=1) */
   int *nb,		/* Out: number of output bands (default=nbi) */
   int *nbi		/* Out: number of input bands (default=0) */
)

{
   int status;
   int tempnb;

   /* Set the defaults */

   *sb = 1;
   *nb = 0;
   *nbi = 0;

   /* If there is a primary input, read in its attributes */

   status = v2_est_primary_input();	/* Ignore any errors */
   if (status == SUCCESS) {
      *nb = PRIMARY_I_VALUE(NB);
      *nbi = *nb;
   }

   /* Get values from user parameters */

   status = v2_get_one_int_parm("BANDS", 1, &tempnb);
   if (status != SUCCESS)
      tempnb = 0;
   if (tempnb != 0) {
      v2_get_one_int_parm("BANDS", 0, sb);
      *nb = tempnb;
   }
   else {		/* If NB == 0 in BANDS, assume it is defaulted */
      status = v2_get_one_int_parm("SB", 0, &tempnb);
      if (tempnb > 0 && status == SUCCESS)
         *sb = tempnb;
      status = v2_get_one_int_parm("NB", 0, &tempnb);
      if (tempnb > 0 && status == SUCCESS)
         *nb = tempnb;
   }

   return;
}
