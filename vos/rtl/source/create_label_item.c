#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Construct the ASCII form of the label described by the parameters.	*/
/* NOTE: This routine mallocs memory for *text.  The caller *MUST* do	*/
/* a free(text) when done!  If an error occurs, the buffer is freed, so	*/
/* the caller should NOT free it on error!				*/

int v2_create_label_item(
   char *key,			/* in: key for label */
   char *valarray,		/* in: array of values for label */
   int maxlen,			/* in: first dimension of values */
   int nelements,		/* in: # of elements */
   char *format,		/* in: format of elements */
   int level,			/* in: level number */
   char **text			/* out: ASCII version of label */
)

{
   int status, i, size;
   int textsize;
   char *element;
   char level_prefix[MAX_LABEL_LEVELS+3],
        level_suffix[MAX_LABEL_LEVELS+3];

   if (EQUAL(format, "REAL"))
      textsize = 20;		/* Plenty of room for -1.234567890e+03 */
   else if (EQUAL(format, "DOUB"))
      textsize = 30;		/* -1.2345678901234567890e+123 */
   else if (EQUAL(format, "INT"))
      textsize = 15;		/* Largest is -2147483648 */
   else if (EQUAL(format, "STRING"))
      textsize = maxlen;
   else
      return ILLEGAL_FORMAT_REQUEST;

   size = MAX_LABEL_KEY_SIZE + 2*MAX_LABEL_LEVELS + (textsize+3)*nelements + 10;
   *text = malloc(size);
   if (*text == NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;

   strcpy(level_prefix, "");
   strcpy(level_suffix, "");

   if (EQUAL(format,"BIN")) {	/* if binary, attach prefix and suffix */
      strcat(level_prefix, "{");	/* NOT IMPLEMENTED */
      strcat(level_suffix, "}");
   }

   level = MIN(level,MAX_LABEL_LEVELS);

   for (i=0; i<level; i++) {		/* add level symbols */
      strcat(level_prefix, "[");	/* NOT IMPLEMENTED */
      strcat(level_suffix, "]");
   }

   if (nelements > 1) {		/* If this is a multivalued label item */
      strcat(level_prefix, "(");	/* add parentheses */
      strcat(level_suffix, ")");
   }

   strcpy(*text, key);			/* Start with the key */
   strcat(*text, "=");
   strcat(*text, level_prefix);

   for (i=0; i<nelements; i++) {	/* build up the elements */
      if (i != 0)			/* add comma except for first */
         strcat(*text, ",");
      status = v2_convert_to_text(&element, valarray, format);
      if (status != SUCCESS) {
         free(*text);
         return status;
      }
      strcat(*text, element);
      valarray += maxlen;
   }

   strcat(*text, level_suffix);

   return SUCCESS;
}
