#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvsptr, XVSPTR) (char *value, int *count, int offsets[],
				int lengths[], ZFORSTR_PARAM)
/* char *value;		In: packed string from xvparm, etc. */
/* int *count;		In: number of strings in value */
/* int offsets[];	Out: array of 1-based indexes into value */
/* int lengths[];	Out: array of lengths of strings */
{
   int i;
   int off, len;
   char *c_value;

   off = 0;
   c_value = zsfor2ptr(value);	/* get pointer to the actual string */

   for (i=0; i<*count; i++) {
      len = strlen(c_value+off);
      offsets[i] = off + 1;		/* 1-based offsets */

#if VMS_OS		/* Must allow 'lengths' to be optional for VMS.. grrr */
      if (n_args() > 3)		/* hopefully this is temporary only */
         lengths[i] = len;
#else
      lengths[i] = len;
#endif

      off += len + 1;
   }

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvsptr(
   char *value,		/* In: packed string from zvparm, etc. */
   int count,		/* In: number of strings in value */
   int offsets[],	/* Out: array of 1-based indexes into value */
   int lengths[]	/* Out: array of lengths of strings */
)

{
   int i;
   int off, len;

   off = 0;

   for (i=0; i<count; i++) {
      len = strlen(value+off);
      offsets[i] = off + 1;		/* 1-based offsets */
      lengths[i] = len;
      off += len + 1;
   }

   return;
}
