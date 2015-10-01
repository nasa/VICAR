#include "xvmaininc.h"
#include "defines.h"
#include <stdio.h>
#include "rtlintproto.h"

/* Transform a label element to its ASCII form.			*/
/* NOTE:  The ASCII value is kept in an internal static buffer	*/
/* (*text is set to it), so it better be used or copied		*/
/* elsewuere before this routine is called again.		*/

int v2_convert_to_text(text, value, format)
char **text;		/* out: ptr to ASCII value */
char *value;		/* in: value to convert */
char *format;		/* in: format of item */
{
   int j, status;
   static char temp[MAX_LABEL_VALUE_SIZE+1];
   int i;

   *text = temp;

   if (EQUAL(format, "REAL"))
      status = sprintf(temp, "%g", *(float *)value);
   else if (EQUAL(format, "DOUB"))
      status = sprintf(temp, "%.16g", *(double *)value);
   else if (EQUAL(format, "INT"))
      status = sprintf(temp, "%d", *(int *)value);
   else if (EQUAL(format, "STRING")) {	/* No sprintf cuz need to double ' */
      j=0;
      temp[j++] = '\'';
      for (i=0; i<strlen(value) && j<MAX_LABEL_VALUE_SIZE-2; i++) {
         temp[j++] = *(value+i);
         if (*(value+i) == '\'')	/* Quote in input, so double it */
            temp[j++] = '\'';
      }
      temp[j++] = '\'';
      temp[j] = '\0';
      status = SUCCESS;
   }
   else
      return ILLEGAL_FORMAT_REQUEST;

   if (status == -1)
      return CONVERSION_ERROR;

   /* Make sure that real items have a decimal point or		*/
   /* exponent.  The C "%g" format may not.			*/
   if (EQUAL(format, "REAL") || EQUAL(format, "DOUB"))
      if (strcspn(temp, ".eEdD") == strlen(temp))
         strcat(temp, ".0");

   return SUCCESS;
}
