#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include "rtlintproto.h"

#if FORSTR_METHOD_A
#include <descrip>
#endif

#if FORSTR_METHOD_D
#include <fortran.h>
#endif

/************************************************************************/
/* sfor2c and v2_sfor2c transform an input Fortran string to a standard	*/
/* C null-terminated string.  The Fortran string can be one of several	*/
/* types, defined in ftnbridge.h.  These methods are the archetype for	*/
/* the related family of methods: sc2for, sfor2c_array, sc2for_array,	*/
/* sfor2len, sfor2ptr, and their v2_* versions.				*/
/*									*/
/* There are three API's.  The older one (sfor2c) requires use of K&R	*/
/* style arguments (types in separate declarations after the function	*/
/* itself).  Variadic functions must use varargs.h (which is now	*/
/* obsolete).  It also breaks a lot of rules and is very brittle.	*/
/*									*/
/* The new one (v2_sfor2c) requires the use of ANSI-style arguments	*/
/* (types in the formal argument list) and stdarg conventions (variadic	*/
/* functions must use stdarg.h), and for most platforms, can be		*/
/* completely legal code.  These platforms must pass string lengths as	*/
/* extra pass-by-value arguments after the normal arguments, one per	*/
/* string.  This criteria is met by most modern	platforms and		*/
/* compilers.  If not, the new API resorts to calling the older one so	*/
/* nothing breaks.  This API should ONLY be used by variadic functions	*/
/* (those with truly variable argument lists, beyond just the fortran	*/
/* string lengths).							*/
/*									*/
/* The third API (zsfor2c) is intended for the bulk of applications	*/
/* to use; those which otherwise have constant argument lists.  It is	*/
/* implemented as a set of macros that call the v2_* methods.  These	*/
/* macros relieve the application programmer from having to mess around	*/
/* with variable argument lists and stdarg.				*/
/*									*/
/* Implementation note:  all the nastiness (for non-stdarg cases) has	*/
/* been moved to sfor2len and sfor2ptr, so as to avoid being repeated	*/
/* 5 times!  Why I didn't think of this sooner, I'll never know.	*/
/*									*/
/************************************************************************/
/*									*/
/* OLD (sfor2c) API:							*/
/*									*/
/* This API is for compatibility with old code only, and should not be	*/
/* used any more.  It is valid only for K&R-style argument lists, and	*/
/* users of varargs.h.							*/
/*									*/
/* sfor2c(c_string, max_length, for_string, argptr, nargs, argno, strno)*/
/*									*/
/* c_string: Output string in C format.  Sufficient space must be	*/
/*   provided by the caller (as array or malloc'd area).		*/
/* max_length: Maximum size of c_string buffer (NOT including		*/
/*   terminator byte, i.e. char buffer[max_length+1];)			*/
/* for_string: The Fortran string argument in the formal arg list	*/
/* argptr: Pointer to the first argument in the routine that is		*/
/*   directly called by Fortran, so if the routine is defined		*/
/*   "sub(a,b,string)", then argptr should be "&a".			*/
/* nargs: Total number of arguments					*/
/* argno: Argument number of this string (start at 1)			*/
/* strno: String number of this string (start at 1).  Like argno but	*/
/*   you count only string arguments.					*/
/* 									*/
/* Not all arguments are used by every string type.  They all use	*/
/* c_string and max_length, of course.  Specifically:			*/
/*   FORSTR_METHOD_A: (VAX)	for_string				*/
/*   FORSTR_METHOD_B: (Sun,etc)	for_string, argptr, nargs, strno	*/
/*   FORSTR_METHOD_C: (Alliant)	for_string, argptr, nargs, argno	*/
/*   FORSTR_METHOD_D: (Cray)	for_string				*/
/*   FORSTR_METHOD_E: (Mac-MPW)	for_string, argptr, nargs, argno	*/
/*   FORSTR_METHOD_F: (HP700)   for_string, argptr, nargs, strno	*/
/*   FORSTR_METHOD_G: (AXP-Unx) for_string, argptr, nargs, strno	*/
/*   FORSTR_METHOD_H: (sgi-n32)	for_string, argptr, nargs, strno	*/
/*   FORSTR_METHOD_I: (ppc-lnx)	for_string, argptr, nargs, strno	*/
/*									*/
/* IMPORTANT:  In order for this routine to work on all machines, the	*/
/* FORSTR_PARAM, FORSTR_DEF, and FORSTR_BLOCK macros must be used, and	*/
/* ftnbridge.h must be included.					*/
/*									*/
/* The FORSTR_PARAM macro should be the last parameter of the parameter	*/
/* list in the routine that is *directly* called by Fortran (unless	*/
/* that routine uses <varargs.h>).  For example, Fortran calls routine	*/
/* A, which calls B, which calls sfor2c.  Routine A must have the	*/
/* FORSTR_PARAM macro, and routine B should not (it probably won't	*/
/* hurt, but it shouldn't be there).  If the direct called routine uses	*/
/* <varargs.h>, then it does *NOT* need	the FORSTR_PARAM macro, and it	*/
/* must not be present.  Also, the argptr, nargs, argno, and strno all	*/
/* refer to the argument list of the routine that is directly called by	*/
/* Fortran, not any intermediate routines.  FORSTR_PARAM is *NOT*	*/
/* included in the nargs count.						*/
/*									*/
/* The FORSTR_DEF macro is needed in the same cases the FORSTR_PARAM	*/
/* macro is.  It goes at the end of the formal parameter declaration	*/
/* list, just before the opening brace of the procedure.  It should	*/
/* not have a semicolon after it (just like va_dcl).			*/
/*									*/
/* The FORSTR_BLOCK macro is also needed in the same cases.  It goes	*/
/* just after the opening brace of the procedure, with no semicolon.	*/
/*									*/
/* Examples:								*/
/*   int constargs(a, s1, s2, b, FORSTR_PARAM)				*/
/*     int *a, *b;							*/
/*     char *s1, *s2;							*/
/*     FORSTR_DEF							*/
/*   { FORSTR_BLOCK							*/
/*     char cs1[11], cs2[20];   ...					*/
/*     sfor2c(cs1, 10, s1, &a, 4, 2, 1);				*/
/*     sfor2c(cs2, 19, s2, &a, 4, 3, 2);				*/
/*   }									*/
/*									*/
/*   int varargs(va_alist)						*/
/*     va_dcl								*/
/*   { char cs[11];   ...						*/
/*     forstr = va_arg(ap, char *);					*/
/*     sfor2c(cs, 10, forstr, &va_alist, nargs, argno, strno);		*/
/*   }									*/
/*									*/
/************************************************************************/
/*									*/
/* VARIADIC (v2_sfor2c) API (stdarg-based variable arg lists only):	*/
/*									*/
/* This API should be used only by code requiring variable arguments	*/
/* beyond Fortran string lengths.  Must use stdarg.			*/
/*									*/
/* v2_sfor2c(c_string, max_length, for_string, argptr, nargs, argno,	*/
/*		strno, valist, which)					*/
/*									*/
/* All arguments except the last two are the same as sfor2c (see above)	*/
/* valist: a pointer to the argument list as created by va_start.	*/
/*   It must be "primed" to point at the first argument beyond the end	*/
/*   of the argument list (i.e. the first "hidden" string length	*/
/*   argument), so that the next va_arg() call will get this hidden	*/
/*   string length (but see "which", below).  Note that this routine	*/
/*   supports both constant args as well as those in the ... list;	*/
/*   valist must point past the end of the ... arguments as well.	*/
/* which: ptr to an in/out integer that says which string length is	*/
/*   currently being pointed to by valist.  Normally this should be	*/
/*   initialized to 0, in which case valist points to the first string	*/
/*   length as described above.  However, "which" is incremented by the	*/
/*   routines as strings are read in. Thus, it represents which string	*/
/*   length argument is currently being pointed at by valist.  This	*/
/*   allows consecutive calls to the string routines without resetting	*/
/*   valist.  The only restriction is that the string lengths must be	*/
/*   obtained in order (no backtracking).  Simply preserve "which"	*/
/*   between calls and it will be automatically maintained.  Of course,	*/
/*   if valist is reset, then "which must be reset as well.		*/
/*									*/
/* If the new, stdarg-based mechanism is used, then only the c_string,	*/
/* max_length, for_string, strno, valist, and which arguments are used.	*/
/* The others are used only if the old sfor2c must be called.  This	*/
/* happens if the platform requires it, or if valist is NULL.		*/
/*									*/
/* The FORSTR_* macros are not needed for this method (but are for the	*/
/* third API, see below).						*/
/*									*/
/* This API should ONLY be used by routines that have true variable	*/
/* arguments beyond just the Fortran strings.  Most applications should	*/
/* use the third (zsfor2c) API below.					*/
/*									*/
/* Example:								*/
/*		caller:  x = varargs(1, 'abc', 2, 'def')		*/
/*   int varargs(int a, char *b, ...)					*/
/*   { char cs1[11], cs2[11];						*/
/*     va_list va;							*/
/*     va_start(va, b);							*/
/*     // determine # of args, etc.					*/
/*     int xxx = va_arg(va, int);					*/
/*     char *forstr = va_arg(ap, char *); // end of varargs		*/
/*     int which = 0;							*/
/*     v2_sfor2c(cs1, 10, b, &a, nargs, 2, 1, va, &which);		*/
/*     v2_sfor2c(cs2, 10, forstr, &a, nargs, 4, 2, va, &which);		*/
/*   }									*/
/*									*/
/* You can also use va_copy and pass in 0 for which.  Note that va_copy	*/
/* is not always available from the system, but it is defined by	*/
/* ftnbridge.h if not.							*/
/*									*/
/************************************************************************/
/*									*/
/* CONSTANT-ARGUMENT (zsfor2c) API:					*/
/*									*/
/* This API is intended for use by all new constant-argument routines.	*/
/* Note that ANSI-style declarations must be used.			*/
/*									*/
/* zsfor2c(c_string, max_length, for_string, argptr, nargs, argno,	*/
/*		strno, last_arg)					*/
/*									*/
/* All arguments except the last are the same as for sfor2c (see above)	*/
/* last_arg: The name of the last argument in the list.			*/
/* 									*/
/* Unlike sfor2c, this routine may ONLY be called by the function being	*/
/* directly called by Fortran; i.e. the one with ZFORSTR_PARAM et al in	*/
/* it.  Extract the strings and call subroutines with the C strings.	*/
/* 									*/
/* If the new, stdarg-based mechanism is used, then only the c_string,	*/
/* max_length, for_string, strno, and last_arg arguments are used.  The	*/
/* others are used only if the old sfor2c must be called.  This happens	*/
/* if the platform does not support the new method.			*/
/*									*/
/* IMPORTANT:  In order for this routine to work on all machines, the	*/
/* ZFORSTR_PARAM and ZFORSTR_BLOCK macros must be used, and ftnbridge.h	*/
/* must be included.							*/
/*									*/
/* The ZFORSTR_PARAM macro must be the last parameter of the parameter	*/
/* list.  ZFORSTR_PARAM is *NOT* included in the nargs count.		*/
/*									*/
/* The ZFORSTR_BLOCK macro goes just after the opening brace of the	*/
/* procedure, with no semicolon.					*/
/*									*/
/* Example:								*/
/*   int constargs(int a, char *s1, char *s2, int b, ZFORSTR_PARAM)	*/
/*   { ZFORSTR_BLOCK							*/
/*     char cs1[11], cs2[20];						*/
/*     zsfor2c(cs1, 10, s1, &a, 4, 2, 1, b);				*/
/*     zsfor2c(cs2, 19, s2, &a, 4, 3, 2, b);				*/
/*   }									*/
/*									*/
/* MOVING FROM sfor2c TO zsfor2c					*/
/*									*/
/* In order to move from the old to new API, the following steps are	*/
/* needed.  (variadic functions are not considered here)		*/
/* 1) Switch to ANSI-style parameters (delete FORSTR_DEF)		*/
/* 2) Change FORSTR_PARAM to ZFORSTR_PARAM				*/
/* 3) Change FORSTR_BLOCK to ZFORSTR_BLOCK				*/
/* 3) Change sfor2c et al to zsfor2c et al				*/
/* 4) Add last_arg argument to the end of all zsfor2c et al calls	*/
/* 5) Fix zsfor2len calls (it's no longer len = sfor2len(); see that	*/
/*    method)								*/
/*									*/
/************************************************************************/

/* OLD API */

void sfor2c(
   char *c_string,		/* Out: string in C format		*/
   int max_length,		/* In: maximum size of c_string buffer	*/
				/*     (NOT including terminator byte,	*/
				/*     i.e. char buffer[max_length+1];)	*/
   char *for_string,		/* In: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno			/* In: this string number (start at 1)	*/
)

{
   int which = 0;
   v2_sfor2c(c_string, max_length, for_string,
				argptr, nargs, argno, strno, NULL, &which);
}

/* NEW API */

void v2_sfor2c(
   char *c_string,		/* Out: string in C format		*/
   int max_length,		/* In: maximum size of c_string buffer	*/
				/*     (NOT including terminator byte,	*/
				/*     i.e. char buffer[max_length+1];)	*/
   char *for_string,		/* In: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno,			/* In: this string number (start at 1)	*/
   va_list *valist,		/* In/Out: stdarg ptr past arg list (see*/
				/*     above). Modified by this routine	*/
   int *which			/* In/Out: ptr to int indicating which	*/
				/*     strlen is pointed at by valist	*/
)

{
   int i;
   int no_nonblanks_found = TRUE;
   char *input_string;		/* the actual text address */
   int fortran_length;		/* the length Fortran claims */

   /* It's so simple this way: relegate all the crap to ptr/len.  Sigh. */

   input_string = v2_sfor2ptr(for_string);
   fortran_length=v2_sfor2len(for_string, argptr, nargs, argno, strno,
				valist, which);

/************************************************************************/

/* Now that we have the string, scan backwards to chop trailing blanks	*/
/* and copy the string to the destination.				*/

   if (fortran_length <= 0 || fortran_length > max_length)
      fortran_length = max_length;	/* sanity checks on the length */

   for (i = fortran_length-1; i >= 0; i--) {   

      if ((input_string[i] == ' ') && no_nonblanks_found)
         continue;					/* a trailing blank */

      else if (input_string[i] == '\0') {
         strcpy(c_string,input_string);		/* wow, it's a C string! */
         return;
      }
      else {					/* in the string itself */
         c_string[i] = input_string[i];
         if (no_nonblanks_found)		/* last nonblank char */
            c_string[i+1] = '\0';		/* so add a terminator */
         no_nonblanks_found = FALSE;
      }
   }

   if (no_nonblanks_found)			/* string was all blank */
      c_string[0] = '\0';

   return;

}

