#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* parse_label will parse out the next label item in the passed-in	*/
/* string.  It looks in a string beginning at 'label' of length 'len',	*/
/* assuming that the next non-white entry is the beginning of a		*/
/* keyword.  The keyword is followed by an '=', and then by the value	*/
/* associated with the keyword.  The starting and ending positions of	*/
/* the keyword and value are returned in 'sk','ek', and 'sv','ev'	*/
/* respectively.							*/
/* The label is assumed to have a valid 0 terminator, even if len is	*/
/* shorter then the length of the string.  Several places use routines	*/
/* that search to the end of the string, for efficiency.  If any of the	*/
/* pointers go beyond the given length, however, FAILURE is returned.	*/

int v2_parse_label(
   char *label,		/* in: pointer to beginning of string to parse  */
   int len,		/* in:  length of string to be searched		*/
   char **sk,		/* out: pointers to start and end of label key	*/
   char **ek,
   char **sv,		/* out: ptrs to start end end of value field	*/
   char **ev
)

{
   int in_quote;
   char *s, *end, *start_key, *end_key, *start_value;

   /* The following was commented out to increase label processing	*/
   /* efficiency.  This check should be done, but it is very expensive.	*/
   /* A careful analysis of all callers determined that it is impossible*/
   /* for this case to occur, therefore the check was removed.  All	*/
   /* callers should make sure that this criteria is in fact not	*/
   /* violated.								*/
#if 0
   if (strlen(label) < len)
      len = strlen(label);
#endif

   end = label + len;

   *sk = *ek = *sv = *ev = end;		/* in case of error */

   start_key = label + strspn(label, " \t"); /* skip white space at beginning */

   if (start_key >= end) {
      return FAILURE;			/* fail if only white space */
   }

   end_key = start_key + strcspn(start_key, " \t=") - 1;  /* Find end of key */
   if (end_key >= end)			/* No terminating '=' or white space */
      return FAILURE;

   start_value = end_key+1 + strspn(end_key+1," \t="); /* Skip to value field */
   if (start_value >= end)
      return FAILURE;			/* No value field */

/* We are now pointing at the first non-white character in the value field. */
/* Search for the end of the value.					    */

   s = start_value;
   switch (*s++) {
      case '[':			/* skip over level (not yet implemented) */
         while (s < end && *s != ']')
            s++;
	 break;
      case '{':			/* skip over binary (not yet implemented) */
         while (s < end && *s != '}')
            s++;
	 break;
      case '(':				/* Multi-valued item */
         in_quote = FALSE;
         while (s < end) {
            if (*s == ')' && !in_quote)
               break;			/* end of multi-valued item */
            if (*s == '\'')
               in_quote = !in_quote;	/* found a quote, toggle the flag */
            s++;
         }
         break;
      case '\'':			/* String item */
         while (s < end) {
            if (*s == '\'') {		/* end quote? */
               if (*(s+1) != '\'')
                  break;		/* yep */
		s++;			/* nope, just a doubled quote */
	    }
            s++;
	 }
	 break;
      default:				/* Int or Real item */
         s += strcspn(s, " \t");	/* find end (whitespace) */
	 s--;			/* want to point TO last char, not past it */
	 break;
   }
   if (s >= end)
      return FAILURE;		/* We reached the end of string prematurely */

   *sk = start_key;		/* Set the return values */
   *ek = end_key;
   *sv = start_value;
   *ev = s;

   return SUCCESS;
}
