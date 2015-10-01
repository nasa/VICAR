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
/* sfor2c_array transforms an input standard C null-terminated two-	*/
/* dimensional array of characters into an output Fortran string array.	*/
/* The Fortran string can be one of several types, defined in		*/
/* ftnbridge.h  The len parameter is the best guess for the length of	*/
/* the Fortran strings on input (used in case the length isn't present),*/
/* and the actual Fortran string length on output.  Maxlen is the true	*/
/* length of the input C string.  Nelements is the number of array	*/
/* elements.  The string array is copied from c_value to for_value.	*/
/* See the routine sc2for for explanations and warnings about the rest	*/
/* of the arguments (FORSTR_PARM, FORSTR_DEF, and FORSTR_BLOCK are	*/
/* still required, as in sc2for().					*/
/*									*/
/* A status code is no longer returned by this function (since there	*/
/* are no errors to return, and nobody looked anyway).			*/
/*									*/
/* See sfor2c.c for comments on how the various Fortran string types	*/
/* are handled, the three API's, and detailed descriptions of the	*/
/* parameters.  The same rules apply for macros, etc.			*/
/************************************************************************/

/* OLD API */

void sc2for_array(
   char *c_string,		/* In: string array in C format		*/
   int maxlen,			/* In: size of c_string inner dimension	*/
   int nelements,		/* In: # of elements in array		*/
   char *for_string,		/* Out: the Fortran string array	*/
   int *len,			/* In/Out: length guess, Ftn length per elem */
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno			/* In: this string number (start at 1)	*/
)

{
   int which = 0;
   v2_sc2for_array(c_string, maxlen, nelements, for_string, len,
		argptr, nargs, argno, strno, NULL, &which);
}

/* NEW API */

void v2_sc2for_array(
   char *c_string,		/* In: string array in C format		*/
   int maxlen,			/* In: size of c_string inner dimension	*/
   int nelements,		/* In: # of elements in array		*/
   char *for_string,		/* Out: the Fortran string array	*/
   int *len,			/* In/Out: length guess, Ftn length per elem */
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
   int i, j, outlen;
   int null_term = FALSE;
   char *output_string;		/* the actual text address */
   int fortran_length;		/* the length Fortran claims */

   /* It's so simple this way: relegate all the crap to ptr/len.  Sigh.	*/

   output_string = v2_sfor2ptr(for_string);
   fortran_length=v2_sfor2len(for_string, argptr, nargs, argno, strno,
					valist, which);

/************************************************************************/

   if (fortran_length == 0)		/* whoops */
      fortran_length = *len;

   if (*len > 0)
      fortran_length = MIN(fortran_length, *len);

   if (fortran_length <= 0)		/* sanity check on the length */
      fortran_length = strlen(c_string)+1;

   *len = fortran_length;

/* Now that we have the length, copy the C string to the output. */

   for (j = 0; j < nelements; j++) {

      outlen = MIN((int) strlen(c_string), fortran_length); /* don't copy too much! */

      strncpy(output_string, c_string, outlen);

      if (null_term) {		/* Null-terminate if there's room */
         if (outlen < fortran_length)
            output_string[outlen] = '\0';
      }
      else {			/* Blank-fill the string */
         for (i=outlen; i<fortran_length; i++)
            output_string[i] = ' ';
      }

      output_string += fortran_length;
      c_string += maxlen;
   }
 
   /* return SUCCESS; */

}
