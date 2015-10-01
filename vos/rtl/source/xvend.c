#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

PUBLICREF int in_vicar;

/* Close all open files and exit the application.		*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvend, XVEND) (int *status)	/* 1 is success, 0 failure */
{
   zvend(*status);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvend(int status)		/* 1 is success, 0 failure */
{
   int i;
   
   for (i=0; i<N_ACTIVE_UNITS_ALLOWED; i++)
      if ((active_units[i].unit != V2_INACTIVE) &&/* in case it's not alloc'd */
	  (current_table[i][FLAGS].ivalue & OPEN))
         zvclose(i, NULL);

   if (status == 0)
      status = -1;

   if (status != SUCCESS)	/* Always return -1 on error to fix _ONFAIL */
      status = -1;	/* NOTE: this destroys batch job completion error msg */

#if RTL_USE_TAE
   if (in_vicar)
      z_exit(status, "");
   else
      exit(status);
#else
   exit(status);
#endif
}
