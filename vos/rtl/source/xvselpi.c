#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/************************************************************************/
/* Select which instance of "INP" is to be the primary input.  If	*/
/* instance 0 is selected, then no primary input is used.		*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvselpi, XVSELPI) (int *instance)
{
   zvselpi(*instance);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvselpi(int instance)
{

   /* If a different PI is already open, then get rid of it */

   if (primary_input_open && primary_instance != instance) {
      if (PRIMARY_VALUE(LABELS) != 0)
         free(PRIMARY_VALUE(LABELS));
      v2_close_unit(primary_input_unit);
      primary_input_open = FALSE;
   }

   /* Set the new primary input instance.  It will be used on the next	*/
   /* xvopen/zvopen call.						*/

   primary_instance = instance;

   return SUCCESS;
}



/************************************************************************/
/* Select a unit number to be used for the primary input, instead of	*/
/* an INP instance.  A unit of 0 is valid; use x/zvselpi() to turn off	*/
/* primary input processing.						*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvselpiu, XVSELPIU) (int *unit)
{
   zvselpiu(*unit);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvselpiu(int unit)
{

   /* If a different PI is already open, then get rid of it */

   if (primary_input_open &&
		(primary_instance > 0 || primary_unit_requested != unit)) {
      if (PRIMARY_VALUE(LABELS) != 0)
         free(PRIMARY_VALUE(LABELS));
      v2_close_unit(primary_input_unit);
      primary_input_open = FALSE;
   }

   /* Set the new primary input unit.  It will be used on the next	*/
   /* xvopen/zvopen call.						*/

   primary_instance = -1;		/* flag to say "use unit not instance */
   primary_unit_requested = unit;

   return SUCCESS;
}

