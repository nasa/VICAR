/************************************************************************/
/* v2param_subs - Support routines for accessing shell-VICAR output	*/
/* parameter files.  Used mostly by v2param.c, but can be used in	*/
/* other applications which call VICAR programs as well.		*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#if defined(vms) || defined(__VMS)
#include <unixlib.h>
#endif

#include "zvproto.h"

#if CUSERID_AVAIL_OS == 0
#include <pwd.h>
#endif

#define MAX_PARAM_SIZE 16384

#include "strncasecmp.h"

/************************************************************************/
/* v2param_get_file - Determine the name of the current parameter file	*/
/* and return it as a string.  If the environment variable/logical name	*/
/* is not set, a message is printed to stdout and a default is used.	*/
/* NULL is returned if even the default doesn't work for some reason	*/
/* (should not be possible, currently).					*/
/*									*/
/* The returned string should be considered to be in a static buffer,	*/
/* so the caller must use it before calling this routine again, and	*/
/* should not free it.							*/
/*									*/
/* The name is contained in the V2PARAM_FILE environment variable/log	*/
/* name.  It defaults to /tmp/shvic_tmp.XXX or				*/
/* sys$login:shvic_tmp.XXX, where XXX is the current username.		*/
/************************************************************************/

char *v2param_get_file(void)
{
   char *env, *userid;
   static char default_name[100];
#if CUSERID_AVAIL_OS
   char* cuserid(char*);
#else
   struct passwd *pw;
#endif

   env = getenv("V2PARAM_FILE");

   if (env == NULL) {
#if CUSERID_AVAIL_OS
      userid = cuserid(NULL);
#else
      pw = getpwuid(getuid());
      userid = pw->pw_name;
#endif
#if VMS_OS
      if (userid)
         sprintf(default_name, "sys$login:shvic_tmp.%s", userid);
      else
         sprintf(default_name, "sys$login:shvic_tmp");
#else
      if (userid)
         sprintf(default_name, "/tmp/shvic_tmp.%s", userid);
      else
         sprintf(default_name, "/tmp/shvic_tmp");
#endif
      env = default_name;

      fprintf(stderr, "Unable to find output parameter filename.  Check $V2PARAM_FILE.\nDefault name \"%s\" used.\n", default_name);
   }

   return env;
}


/************************************************************************/
/* v2param_find_entry - Find the entry for the given parameter in the	*/
/* param file.  Returns a pointer to the value, or NULL if not found.	*/
/* The returned string should be considered in a static buffer, so the	*/
/* caller must use it before calling this routine again, and should not	*/
/* free it.								*/
/************************************************************************/

char *v2param_find_entry(filename, param_name)
char *filename;
char *param_name;
{
   static char buffer[MAX_PARAM_SIZE+1];

   FILE *file;
   char *eof;		/* NULL at eof */
   char *p;
   int len;
   int size_left;
   int line = 0;

   file = fopen(filename, "r");
   if (file == NULL) {
      fprintf(stderr, "Unable to open parameter file.\n");
      return NULL;
   }

   p = buffer;
   size_left = MAX_PARAM_SIZE;

   /* Read each line */

   while ((eof = fgets(p, size_left, file)) != NULL) {
      len = strlen(p);
      if (*(p+len-1) == '\n') {		/* trim off newlines */
         *(p+len-1) = '\0';
         len--;
      }
      line++;
      if (len == 0)
         continue;		/* skip blank lines */
      size_left -= len;

      /* Check for continuation lines */

      while ((*(p+len-1) == '\\' || *(p+len-1) == '+') &&
			size_left > 0 && eof != NULL) {
         p = p+len-1;
         *p = '\0';
         eof = fgets(p, size_left, file);
         len = strlen(p);
         if (*(p+len-1) == '\n') {	/* trim off newlines */
            *(p+len-1) = '\0';
            len--;
         }
         line++;
         size_left -= len;
      }

      if (size_left == 0) {		/* buffer overflow! */
         fprintf(stderr, "Line %d too long for internal buffer.\n", line);
         continue;			/* skip this line */
      }

      /* See if this is the line we want */

      p = buffer;
      while (isspace(*p)) p++;

      if (strncasecmp(p, param_name, strlen(param_name)) == 0) {

         /* Found it!! */

         p += strlen(param_name);
         while (isspace(*p)) p++;
         if (*p == '=') p++;		/* skip the equals */
         else {
            fprintf(stderr, "Syntax error on line %d: no = sign.\n", line);
            continue;
         }
         while (isspace(*p)) p++;

         fclose(file);

         return p;
      }
   }

   return NULL;			/* Not found */

}

/************************************************************************/
/* v2param_count_elements - Return the number of elements in the given	*/
/* param value.  param_name is needed only for error messages.		*/
/************************************************************************/

int v2param_count_elements(param_value, param_name)
char *param_value;
char *param_name;
{
   int element_count = 0;
   char *p;
   char *element;

   if (*param_value != '(')	/* Not multi-valued */
      return 1;

   p = param_value+1;		/* skip the paren */
   element_count = 0;

   do {
      while (isspace(*p)) p++;
      element = v2param_get_one_value(&p, param_name);
      if (element)
         element_count++;
      while (isspace(*p)) p++;
      if (*p != ',' && *p != ')') {
         fprintf(stderr, "Syntax error on param %s: missing , or )\n",
						param_name);
         return element_count;
      }
      if (*p == ',') p++;		/* skip comma */
   } while (*p != ')' && *p != '\0');
   if (*p != ')') {
      fprintf(stderr, "Syntax error on param %s: missing )\n", param_name);
      return element_count;
   }
   p++;
   while (isspace(*p)) p++;
   if (*p != '\0') {
      fprintf(stderr, "Syntax error on param %s: extra data at end\n",
						param_name);
      return element_count;
   }

   return element_count;

}

/************************************************************************/
/* v2param_get_one_value - Return the next value from the value string.	*/
/* Value has quotes (if any) removed, and doubled internal quotes are	*/
/* converted back to singles.  Value ends with end quote for quoted	*/
/* strings, or with ",", ")", or white space for unquoted values.	*/
/* The returned string should be considered in a static buffer, so the	*/
/* caller must use it before calling this routine again, and should not	*/
/* free it.  The pointer is modified to point just past the value.	*/
/* param_name is used only for error messages.				*/
/************************************************************************/

char *v2param_get_one_value(ptr, param_name)
char **ptr;
char *param_name;
{
   static char buffer[MAX_PARAM_SIZE+1];

   char *p, *out;

   p = *ptr;
   out = buffer;

   while (isspace(*p)) p++;

   /* Check for quoted string, because they're handled quite differently */

   if (*p == '"') {
      p++;
      while (*p != '\0' && ! (*p == '"' && *(p+1) != '"')) {
         if (*p == '"')
            p++;		/* skip doubled quote */
         *out++ = *p++;
      }
      *out = '\0';		/* make sure string is terminated */

      if (*p == '"')
         p++;			/* skip ending quote */
      else
         fprintf(stderr, "Syntax error on param %s: no closing quote\n",
					param_name);
   }
   else {

      /* Just copy everything over until end mark.  Caller must check	*/
      /* syntax because we don't know what is expected afterward.	*/

      while (*p != '\0' && *p != ',' && *p != ')' && !isspace(*p))
         *out++ = *p++;
      *out = '\0';		/* make sure string is terminated */

   }

   *ptr = p;
   return buffer;
}

/************************************************************************/
/* v2param_remove_file - Remove the parameter file.  No errors are	*/
/* checked for or reported.						*/
/************************************************************************/

void v2param_remove_file(filename)
char *filename;
{
   if (filename != NULL)
      (void)remove(filename);
}

