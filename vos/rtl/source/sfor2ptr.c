#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"

#if FORSTR_METHOD_A
#include <descrip>
#endif

#if FORSTR_METHOD_D
#include <fortran.h>
#endif

/************************************************************************/
/* sfor2ptr returns a pointer to the characters of an input Fortran	*/
/* string.  It is somewhat similar to sfor2c; however, it does NOT get	*/
/* the Fortran string length, nor does it copy the string to an output	*/
/* C string.  It merely returns a pointer to the characters.  This	*/
/* routine is mainly intended for checking strings in variable-length	*/
/* argument lists for the end-of-list marker, in order to get the total	*/
/* number of arguments, which is required by sfor2c.  It is also used	*/
/* internally by sfor2c.						*/
/*									*/
/* See sfor2c.c for comments on how the various Fortran string types	*/
/* are handled, the three API's, and detailed descriptions of the	*/
/* parameters.  The same rules apply for macros, etc.			*/
/*									*/
/* For this routine, there is no difference in API between sfor2ptr,	*/
/* v2_sfor2ptr, and zsfor2ptr.  However, all three routines are		*/
/* available for symmetry.						*/
/*									*/
/* NOTE:  The characters returned are not necessarily (and probably are	*/
/* not) null terminated.  Use sfor2c to get a null-terminated string.	*/
/************************************************************************/

/* OLD API */

char *sfor2ptr(
   char *for_string		/* the Fortran string argument	*/
)

{
   return v2_sfor2ptr(for_string);
}

/* NEW API */

char *v2_sfor2ptr(
   char *for_string		/* the Fortran string argument	*/
)

{

/************************************************************************/

#if FORSTR_METHOD_STDARG

   return for_string;

#else

/************************************************************************/

#if FORSTR_METHOD_A

/* Check if a descriptor is present.  If so, get the info from it.	*/
/* If not, assume it's passed by reference.				*/

   struct dsc$descriptor *possible_descriptor;

   possible_descriptor=(struct dsc$descriptor *)for_string; /* Check descrip */

   if ((possible_descriptor->dsc$b_dtype == DSC$K_DTYPE_T) &&
       ((possible_descriptor->dsc$b_class == DSC$K_CLASS_S) ||
        (possible_descriptor->dsc$b_class == DSC$K_CLASS_NCA) ||
        (possible_descriptor->dsc$b_class == DSC$K_CLASS_A)))

      return possible_descriptor->dsc$a_pointer;	/* descriptor */
   else
      return for_string;				/* reference */

#endif /* FORSTR_METHOD_A */

/************************************************************************/

#if FORSTR_METHOD_B

   return for_string;

#endif /* FORSTR_METHOD_B */

/************************************************************************/

#if FORSTR_METHOD_C

   return for_string;

#endif /* FORSTR_METHOD_C */

/************************************************************************/

#if FORSTR_METHOD_D

   return _fcdtocp(for_string);

#endif /* FORSTR_METHOD_D */

/************************************************************************/

#if FORSTR_METHOD_E

   return for_string;

#endif /* FORSTR_METHOD_E */

/************************************************************************/

#if FORSTR_METHOD_F

   return for_string;

#endif /* FORSTR_METHOD_F */

/************************************************************************/

#if FORSTR_METHOD_G

   return for_string;

#endif /* FORSTR_METHOD_G */

/************************************************************************/

#if FORSTR_METHOD_H

   return for_string;

#endif /* FORSTR_METHOD_H */

/************************************************************************/

#if FORSTR_METHOD_I

   return for_string;

#endif /* FORSTR_METHOD_I */

/************************************************************************/

#endif	/* FORSTR_METHOD_STDARG */


}

