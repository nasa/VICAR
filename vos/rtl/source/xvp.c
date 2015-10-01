#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine replaces XVPARM for most cases, and is slightly	*/
/* simpler to handle.  The arguments that are present are the	*/
/* same as the corresponding arguments in xvparm, except if the	*/
/* parameter was defaulted, then count is returned as 0 IN XVP	*/
/* ONLY!  This is a design flaw that must be maintained for	*/
/* historical reasons.  For ZVP, XVIP, and ZVIP, the COUNT	*/
/* argument returns the actual count of the parameter.  This	*/
/* difference may cause some confusion.  It is a design flaw	*/
/* for two reasons:  1) With the count returned as 0, there is	*/
/* no way to know if there is anything valid in the VALUE	*/
/* parameter, so it becomes useless.  2) The default flag	*/
/* should never be used for any reason.				*/
/*								*/
/* Maxcnt and length (for zvparm) are both treated as 0.  For	*/
/* this reason, you should *NOT* use this routine for		*/
/* multivalued string parameters, since (in C at least) you	*/
/* get the XVSPTR packed format.  If you are getting more than	*/
/* one string, please use XVPARM (ZVPARM) or XVPONE instead.	*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvp, XVP) (char *name, char *value, int *count, ...)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *count;		Out: # of values.  0 if not found or default. */
{
   va_list param;
   int which = 0;
   int def;
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */

   current_call = VP;

   va_start(param, count);
   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 3, 1, 1, &param, &which);

   v2_get_parm_for((struct PARBLK*) &parb, c_name, count, &def, 0, value, 
		&name, 3, 2, 2, &param, &which, FALSE);

   if (def)	/* defaulted */	/* DESIGN FLAW, retained for compat only */
      *count = 0;		/* See comments above for details	 */

   va_end(param);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvp(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count		/* Out: # of values.  0 if not found or defaulted. */
)

{
   int def, status;

   current_call = VP;

   status = v2_get_parm_c((struct PARBLK*) &parb, name, value, count, &def, 
		       0, 0, FALSE);

   return status;
}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvip, XVIP) (char *name, char *value, int *count, ...)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *count;		Out: # of values.  0 if not found or default. */
{
   va_list param;
   int which = 0;
   int def;
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */

   current_call = VIP;

   va_start(param, count);
   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 3, 1, 1, &param, &which);

   v2_get_parm_for((struct PARBLK*) &iparb, c_name, count, &def, 0, value, 
		&name, 3, 2, 2, &param, &which, FALSE);

   va_end(param);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvip(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count		/* Out: # of values.  0 if not found or defaulted. */
)

{
   int def, status;

   current_call = VIP;

   status = v2_get_parm_c((struct PARBLK*) &iparb, name, value, count, &def, 
		       0, 0, FALSE);

   return status;
}

