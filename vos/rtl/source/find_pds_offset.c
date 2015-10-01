#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include <ctype.h>
#include "strncasecmp.h"

/* Parses the (partial) PDS label in the string argument, and returns	*/
/* the location of the VICAR label as a 0-based offset.  If we are	*/
/* unable to determine the proper offset, return 0.			*/
/*									*/
/* Only Unix disk files are currently supported; see set_file_offset.c.	*/
/*									*/
/* The rules for finding the offset in the PDS header are:		*/
/*									*/
/* - Keywords are "KEY = VALUE\r\n" where there can be any number of	*/
/*   spaces (including 0) around the = and before KEY and after VALUE.	*/
/*   Both KEY and VALUE are case-insensitive.  Tabs are not allowed.	*/
/* - Search for "^IMAGE_HEADER" keyword.  Value is an integer,		*/
/*   optionally followed by any number of spaces and a unit.		*/
/* - Units are in angle brackets and may have spaces, e.g. "< BYTES  >".*/
/* - If the unit is "<BYTES>", subtract 1 from the number and return it	*/
/*   (the number is 1-based in the file).				*/
/* - If the unit is "<RECORDS>", or no unit, then look for the		*/
/*   "RECORD_BYTES keyword.  Its value is an integer.			*/
/* - Return (image_header - 1) * record_bytes.				*/
/*									*/
/* If any parsing check fails, return 0.				*/

V2_OFFSET v2_find_pds_offset(char *label)
{
   char *p;
   V2_OFFSET image_header;
   V2_OFFSET record_bytes;
   int unit_present;
   char *unit_str;

   /* Get ^IMAGE_HEADER keyword */

   p = v2_find_pds_keyword(label, "^IMAGE_HEADER");
   if (p == NULL)
      return 0;

   image_header = (V2_OFFSET)atoi(p);
   if (image_header == 0)
      return 0;

   /* See if there's a unit */

   while (isdigit(*p))		/* Skip the number */
      p++;
   while (*p == ' ')		/* Skip any spaces */
      p++;

   /* Look for a unit, which must be in < >. */

   unit_present = FALSE;

   if (*p == '<') {			/* found a unit */
      p++;
      while (*p == ' ')
         p++;
      unit_str = p;

      while (isalpha(*p))		/* skip past the unit */
         p++;
      while (*p == ' ')
         p++;
      if (*p != '>')			/* No closing bracket, error */
         return 0;
      p++;
      unit_present = TRUE;
   }

   if (unit_present && (strncasecmp(unit_str, "BYTES", 5) == 0))
      return image_header - 1;			/* Done! */

   /* Now it's gotta be <RECORDS>, or nothing */

   if (unit_present && (strncasecmp(unit_str, "RECORDS", 7) != 0))
      return 0;					/* Unknown unit */

   /* We could check for EOL here but there could be comments so it's	*/
   /* not worth the trouble.						*/

   /* Find the RECORD_BYTES keyword */

   p = v2_find_pds_keyword(label, "RECORD_BYTES");
   if (p == NULL)
      return 0;

   record_bytes = (V2_OFFSET)atoi(p);
   if (record_bytes <= 0)
      return 0;

   return (image_header - 1) * record_bytes;

}

