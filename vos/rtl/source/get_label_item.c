#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Return the first (or only) label element from 'value'.	*/
/* 'Value' is incremented to point at the next item, so this	*/
/* routine can be called multiple times to get all the elements	*/
/* of a label item.  Returns TRUE if an element is returned,	*/
/* FALSE if there are no more elements.				*/
/* NOTE: The element returned is kept in an internal static	*/
/* buffer (*element is set to it), so it better be used or	*/
/* copied elsewhere before this routine is called again.	*/

int v2_get_label_item(
   char **value,	/* in/out: pointer to this (next) element in value */
   int *vallen,		/* in/out: length (remaining) in value */
   char **element,	/* out: pointer to this element's value (STATIC!) */
   int *level		/* in/out: level number */
)

{
   static char temp[MAX_LABEL_VALUE_SIZE+1];
   char *p, *start, *end;
   int len;
   int done;

   temp[0] = '\0';
   *element = temp;

   if (*vallen <= 0 || strlen(*value) == 0)
      return FALSE;			/* no more elements */

   p = *value;
   end = *value + *vallen;

   done = FALSE;
   while (!done) {
      switch (*p++) {
         case '\0':
            p--;		/* point at last valid char */
            done = TRUE;
            break;
         case '[':			/* bump level */
            (*level)++;
            break;
         case '(':			/* skip parens */
         case ',':			/* and commas */
            break;
         case ')':			/* close paren - end of value */
            *vallen -= (p - *value);
            *value = p;
            return FALSE;
         case '\'':		/* string item */
            start = p;
            while (p < end) {
               if (*p == '\'') {	/* end quote? */
                  if (*(p+1) != '\'')
                     break;	/* yep */
                  p++;		/* nope, just a doubled quote */
               }
               p++;
            }
            if (p == end)	/* whoops, no close quote! */
               p--;		/* make the best of it... */
            len = MIN(p-start+1, MAX_LABEL_VALUE_SIZE);
            strncpy(temp, start, len);
            temp[len] = '\0';
            done = TRUE;
            break;
         default:
            start = p-1;
            while (p < end) {
               if (*p == ',' || *p == ')' || *p == '\0' || *p == ' ')
                  break;			/* end of value */
               p++;
            }
            p--;		/* point at last valid char */
            len = MIN(p-start+1, MAX_LABEL_VALUE_SIZE);
            strncpy(temp, start, len);
            temp[len] = '\0';
            done = TRUE;
            break;
      }
   }

   p++;				/* point to first char of next element */
   *vallen -= (p - *value);
   *value = p;

   if (strlen(temp) == 0)
      return FALSE;		/* no items found */
   return TRUE;

}
