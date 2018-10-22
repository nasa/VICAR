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
/* sc2for transforms an standard C null-terminated string to an output	*/
/* Fortran string.  It is the inverse of sfor2c.			*/
/*									*/
/* See sfor2c.c for comments on how the various Fortran string types	*/
/* are handled, the three API's, and detailed descriptions of the	*/
/* parameters.  The same rules apply for macros, etc.			*/
/*									*/
/* Max_length is optional.  If it's not used, it should be passed in	*/
/* as 0.  It is an alternate maximum length if one should happen to be	*/
/* passed in as another parameter.  The actual maximum Fortran length	*/
/* is the minimum of the passed in max_length (if present) and the	*/
/* length automatically passed with the Fortran string.			*/
/************************************************************************/

/* OLD API */

void sc2for(
   char *c_string,		/* In: string in C format		*/
   int max_length,		/* In: optional max size of for_string	*/
				/*     buffer (length used is min of 	*/
				/*     this and the Fortran string len	*/
   char *for_string,		/* Out: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno			/* In: this string number (start at 1)	*/
)

{
   int which = 0;
   v2_sc2for(c_string, max_length, for_string, argptr, nargs, argno, strno,
							NULL, &which);
}


/* NEW API */

void v2_sc2for(
   char *c_string,		/* In: string in C format		*/
   int max_length,		/* In: optional max size of for_string	*/
				/*     buffer (length used is min of 	*/
				/*     this and the Fortran string len	*/
   char *for_string,		/* Out: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno,			/* In: this string number (start at 1)	*/
   va_list *valist,		/* In/Out: stdarg ptr past arg list (see*/
				/*     description in sfor2c.c).	*/
				/*     Modified by this routine.	*/
   int *which			/* In/Out: ptr to int indicating which  */
				/*     strlen is pointed at by valist   */
)

{

   int i, len;
   int null_term = FALSE;
   char *output_string;		/* the actual text address */
   int fortran_length;		/* the length Fortran claims */

   /* It's so simple this way: relegate all the crap to ptr/len.  Sigh. */

   output_string = v2_sfor2ptr(for_string);
   fortran_length=v2_sfor2len(for_string, argptr, nargs, argno, strno,
					valist, which);

/************************************************************************/

   if (fortran_length == 0)		/* whoops */
      fortran_length = max_length;

   if (max_length > 0)
      fortran_length = MIN(fortran_length, max_length);

   if (fortran_length <= 0)		/* sanity check on the length */
      fortran_length = strlen(c_string)+1;

/* Now that we have the string pointer and the length, copy the string	*/
/* over to the destination, using either blank fill or null termination	*/

   len = MIN((int) strlen(c_string), fortran_length);	/* don't copy too much! */

   strncpy(output_string, c_string, len);

   if (null_term) {			/* Null-terminate if there's room */
      if (len < fortran_length)
         output_string[len] = '\0';
   }
   else {				/* Blank-fill the string */
      for (i=len; i<fortran_length; i++)
         output_string[i] = ' ';
   }

   return;
}

