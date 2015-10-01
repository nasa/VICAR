#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/****************************************************************/
/* *vparmd and *viparmd are identical to *vparm and *viparm	*/
/* with the exception that, if the value is floating point, it	*/
/* is returned as double precision instead of single.		*/
/*								*/
/* Returns the value, count, and default flag for the given	*/
/* parameter.  *vparmd tests the initial parameter parblk;	*/
/* *viparmd tests the interactive parblk.			*/
/* Value is returned as a single value or a standard array if	*/
/* the parameter type is integer or real.  If it is string, and	*/
/* there is only one value, it is returned as a normal string.	*/
/* If there's more than one value, and the length is given (via	*/
/* the length parameter in zvparmd, or the fortran string length*/
/* in xvparmd), the strings are returned as a 2D character array*/
/* just like xlget.  If the length is not available, the	*/
/* strings are returned in a weird packed format that requires	*/
/* calling XVSPTR.  Note that use of this packed format is	*/
/* HIGHLY discouraged.						*/
/****************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvparmd, XVPARMD) (char *name, char *value, int *count, int *def,
					int *maxcnt, ...)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *count;		Out: # of values returned.  0 if param not found. */
/* int *def;		Out: default flag, 0 if user-given, 1 if default */
/* int *maxcnt;		In: max # of values to return.  0 means no limit. */
{
   va_list param;
   int which = 0;
   int max_count;
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */

   current_call = VPARMD;

   va_start(param, maxcnt);
   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 5, 1, 1, &param, &which);

   max_count = *maxcnt;

   v2_get_parm_for((struct PARBLK*) &parb, c_name, count, def, max_count,value, 
		&name, 5, 2, 2, &param, &which, TRUE);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvparmd(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count,		/* Out: # of values returned.  0 if param not found. */
   int *def,		/* Out: default flag, 0 if user-given, 1 if default */
   int maxcnt,		/* In: max # of values to return.  0 means no limit. */
   int length		/* In: len of each string in value if string array */
)

{

   current_call = VPARMD;

   return v2_get_parm_c((struct PARBLK*) &parb, name, value, count, def, 
		     maxcnt, length, TRUE);

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xviparmd, XVIPARMD) (char *name, char *value, int *count,
				int *def, int *maxcnt, ...)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *count;		Out: # of values returned.  0 if param not found. */
/* int *def;		Out: default flag, 0 if user-given, 1 if default */
/* int *maxcnt;		In: max # of values to return.  0 means no limit. */
{
   va_list param;
   int which = 0;
   int max_count;
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */

   current_call = VIPARMD;

   va_start(param, maxcnt);
   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 5, 1, 1, &param, &which);

   max_count = *maxcnt;

   v2_get_parm_for((struct PARBLK*) &iparb, c_name, count, def, max_count, 
		value, &name, 5, 2, 2, &param, &which, TRUE);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zviparmd(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count,		/* Out: # of values returned.  0 if param not found. */
   int *def,		/* Out: default flag, 0 if user-given, 1 if default */
   int maxcnt,		/* In: max # of values to return.  0 means no limit. */
   int length		/* In: len of each string in value if string array */
)

{

   current_call = VIPARMD;

   return v2_get_parm_c((struct PARBLK*) &iparb, name, value, count, 
		     def, maxcnt, length, TRUE);

}
