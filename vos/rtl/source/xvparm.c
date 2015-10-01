#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/****************************************************************/
/* Returns the value, count, and default flag for the given	*/
/* parameter.  *vparm tests the initial parameter parblk;	*/
/* *viparm tests the interactive parblk.			*/
/* Value is returned as a single value or a standard array if	*/
/* the parameter type is integer or real.  If it is string, and	*/
/* there is only one value, it is returned as a normal string.	*/
/* If there's more than one value, and the length is given (via	*/
/* the length parameter in zvparm, or the fortran string length	*/
/* in xvparm), the strings are returned as a 2D character array	*/
/* just like xlget.  If the length is not available, the	*/
/* strings are returned in a weird packed format that requires	*/
/* calling XVSPTR.  Note that use of this packed format is	*/
/* HIGHLY discouraged.						*/
/****************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

#if VMS_OS
void FTN_NAME2(xvparm, XVPARM) (char *name, char *value, int *count, int *def,
				int *maxcnt, int *r8flag, ...)
/* int *r8flag;	   in: dbl prec flag ***VMS OLD COMPAT ONLY!!  DON'T USE!!*** */
#else
void FTN_NAME2(xvparm, XVPARM) (char *name, char *value, int *count, int *def,
				int *maxcnt, ...)
#endif
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
   int dbl;

   current_call = VPARM;
   dbl = FALSE;

#if VMS_OS
   va_start(param, r8flag);
#else
   va_start(param, maxcnt);
#endif

   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 5, 1, 1, &param, &which);

#if VMS_OS
   if (n_args() > 4)	/* Must allow 'maxcnt' to be optional for VMS... grrr */
      max_count = *maxcnt;	/* hopefully this is temporary only */
   else
      max_count = 0;
   if (n_args() > 5)	/* Double grrr... r8flag for old VMS compatibility */
      dbl = *r8flag;	/* Use xvparmd() instead */
/* The 6th arg should mess up sfor2c but VMS doesn't use # of args so it's ok */
#else
   max_count = *maxcnt;
#endif

   v2_get_parm_for((struct PARBLK*) &parb, c_name, count, def, max_count, 
		value, &name, 5, 2, 2, &param, &which, dbl);

   va_end(param);

return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvparm(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count,		/* Out: # of values returned.  0 if param not found. */
   int *def,		/* Out: default flag, 0 if user-given, 1 if default */
   int maxcnt,		/* In: max # of values to return.  0 means no limit. */
   int length		/* In: len of each string in value if string array */
)

{

   current_call = VPARM;

   return v2_get_parm_c((struct PARBLK*) &parb, name, value, count, def, 
		     maxcnt, length, FALSE);

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

#if VMS_OS
void FTN_NAME2(xviparm, XVIPARM) (char *name, char *value, int *count, int *def,
				int *maxcnt, int *r8flag, ...)
/* int *r8flag;	in: dbl prec flag ***VMS OLD COMPAT ONLY!!  DON'T USE!!*** */
#else
void FTN_NAME2(xviparm, XVIPARM) (char *name, char *value, int *count, int *def,
				int *maxcnt, ...)
#endif
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
   int dbl;

   current_call = VIPARM;
   dbl = FALSE;

#if VMS_OS
   va_start(param, r8flag);
#else
   va_start(param, maxcnt);
#endif

   v2_sfor2c(c_name, 2*NAMESIZE+1, name, &name, 5, 1, 1, &param, &which);

#if VMS_OS
   if (n_args() > 4)	/* Must allow 'maxcnt' to be optional for VMS... grrr */
      max_count = *maxcnt;	/* hopefully this is temporary only */
   else
      max_count = 0;
   if (n_args() > 5)	/* Double grrr... r8flag for old VMS compatibility */
      dbl = *r8flag;	/* Use xviparmd() instead */
/* The 6th arg should mess up sfor2c but VMS doesn't use # of args so it's ok */
#else
   max_count = *maxcnt;
#endif

   v2_get_parm_for((struct PARBLK*) &iparb, c_name, count, def, max_count, 
		value, &name, 5, 2, 2, &param, &which, dbl);

   va_end(param);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zviparm(
   char *name,		/* In: Name of parameter to get */
   void *value,		/* Out: returned value (may be int,real,or string) */
   int *count,		/* Out: # of values returned.  0 if param not found. */
   int *def,		/* Out: default flag, 0 if user-given, 1 if default */
   int maxcnt,		/* In: max # of values to return.  0 means no limit. */
   int length		/* In: len of each string in value if string array */
)

{

   current_call = VIPARM;

   return v2_get_parm_c((struct PARBLK*) &iparb, name, value, count, def, 
		     maxcnt, length, FALSE);

}
