#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/************************************************************************/
/* This routine takes the optional argument list, and figures out how	*/
/* many optional arguments there are by searching for a blank keyword.	*/
/* Note that this is the number of optional *arguments*, not the number	*/
/* of keyword-value pairs.  This routine accepts only Fortran-style	*/
/* string arguments, as it is not needed for C calls.			*/
/*									*/
/* On input, *params is ready to retrieve the first vararg (optional);	*/
/* on output, it is ready to retrieve the first string length.		*/
/*									*/
/* This could, and maybe should, make use of NARGS_AVAIL_OS.  However,	*/
/* since that is so rarely available (and not on any machines of	*/
/* current interest), it is dispensed with.  Likewise with		*/
/* FORSTR_NARGS; since the stdarg mechanism needs it, no need to go	*/
/* without.								*/
/************************************************************************/

int v2_get_nopts(
   int *nopts,				/* Returned # of opt args	*/
   int *nargs,				/* Returned # of args overall	*/
   int nconst,				/* Input # of constant args	*/
   va_list *params			/* optional arguments pointer	*/
)

{
   char *value;
   char *forstr;
   char *s;

   *nopts = 0;
   *nargs = nconst;

   do {
      forstr = va_arg(*params, char *);		/* get the keyword */
      (*nopts)++;
      (*nargs)++;
      s = v2_sfor2ptr(forstr);			/* pointer to it */

      if (*s != ' ' && *s != '\0') {
         value = va_arg(*params, char *);	/* Assume 4-byte optionals */
         (*nopts)++;
         (*nargs)++;
      }
			/* end main optionals loop */
   } while (*s != ' ' && *s != '\0');

   return SUCCESS;
}

