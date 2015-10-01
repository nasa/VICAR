#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "ftnbridge.h"

#if FORSTR_METHOD_A
#include <descrip>
#endif

#if FORSTR_METHOD_D
#include <fortran.h>
#endif

/************************************************************************/
/* sfor2c_array transforms an input Fortran string array to a standard	*/
/* C null-terminated two-dimensional array of characters.  The Fortran	*/
/* string can be one of several types, defined in ftnbridge.h.  The len	*/
/* parameter is the best guess for the length of each array on input,	*/
/* (used in case the length isn't present), and the true length (of the	*/
/* returned C string) on output.  Nelements is the number of array	*/
/* elements.  The string array is copied from for_string to c_string,	*/
/* which is malloc'ed.  You should free c_string when you're done.  You	*/
/* may pass the same pointer in for both for_string and c_string (with	*/
/* an ampersand for c_string, of course).				*/
/*									*/
/* A status code is returned by the function.				*/
/*									*/
/* See sfor2c.c for comments on how the various Fortran string types	*/
/* are handled, the three API's, and detailed descriptions of the	*/
/* parameters.  The same rules apply for macros, etc.			*/
/************************************************************************/

/* OLD API */

int sfor2c_array(
   char **c_string,		/* Out: string array in C format (malloc'ed)  */
   int *len,			/* In/out: length guess, C length per element */
   int nelements,		/* In: # of elements in array		*/
   char *for_string,		/* In: the Fortran string array		*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno			/* In: this string number (start at 1)	*/
)

{
   int which = 0;
   return v2_sfor2c_array(c_string, len, nelements, for_string, argptr, nargs,
				argno, strno, NULL, &which);
}

/* NEW API */

int v2_sfor2c_array(
   char **c_string,		/* Out: string array in C format (malloc'ed)  */
   int *len,			/* In/out: length guess, C length per element */
   int nelements,		/* In: # of elements in array		*/
   char *for_string,		/* In: the Fortran string array		*/
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
   int i, j;
   int no_nonblanks_found;
   char *input_string;			/* the actual text address */
   int fortran_length;			/* the length Fortran claims */
   char *c_temp;

   /* It's so simple this way: relegate all the crap to ptr/len.  Sigh.	*/

   input_string = v2_sfor2ptr(for_string);
   fortran_length=v2_sfor2len(for_string, argptr, nargs, argno, strno,
					valist, which);

/************************************************************************/

/* Now that we have the length, copy the input to the C string.	*/
/* For each string, scan backwards to chop trailing blanks.	*/

   if (fortran_length <= 0)
      fortran_length = *len;		/* sanity checks on the length */
   if (fortran_length < 0)
      return IMPROPER_LENGTH;		/* 0 length is legal! (empty string) */

   *len = fortran_length + 1;		/* +1 for null terminator */

   *c_string = malloc(*len * nelements);
   if (*c_string == NULL)
      return INSUFFICIENT_MEMORY;
   c_temp = *c_string;

   for (j = 0; j < nelements; j++) {
      no_nonblanks_found = TRUE;
      for (i = fortran_length-1; i >= 0; i--) {   

         if ((input_string[i] == ' ') && no_nonblanks_found)
            continue;				/* a trailing blank */

         else if (input_string[i] == '\0') {
            strcpy(c_temp,input_string);	/* wow, it's a C string! */
            no_nonblanks_found = FALSE;
            break;
         }
         else {					/* in the string itself */
            c_temp[i] = input_string[i];
            if (no_nonblanks_found)		/* last nonblank char */
               c_temp[i+1] = '\0';		/* so add a terminator */
            no_nonblanks_found = FALSE;
         }
      }
      if (no_nonblanks_found)			/* string was all blank */
         c_temp[0] = '\0';
      input_string += fortran_length;
      c_temp += *len;
   }

   return SUCCESS;
}

