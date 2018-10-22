$!****************************************************************************
$!
$! Build proc for MIPL module v2param_standalone
$! VPACK Version 1.9, Thursday, August 09, 2007, 16:26:24
$!
$! Execute by entering:		$ @v2param_standalone
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module v2param_standalone ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to v2param_standalone.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("v2param_standalone.imake") .nes. ""
$   then
$      vimake v2param_standalone
$      purge v2param_standalone.bld
$   else
$      if F$SEARCH("v2param_standalone.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake v2param_standalone
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @v2param_standalone.bld "STD"
$   else
$      @v2param_standalone.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create v2param_standalone.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack v2param_standalone.com -mixed -
	-s v2param.c v2param_subs.c -
	-i v2param.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create v2param.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*    N  O  T  E							*/
/* This is a STANDALONE VERSION of rtl/v2param.com, for use ONLY when	*/
/* the program is needed but the RTL is not available.  It is identical	*/
/* code-wise to the RTL version, with the exception of including	*/
/* v2param_subs in this .com file instead of getting them from the	*/
/* shell_vicar subsystem, and a few include file differences.  If the	*/
/* RTL (really, shell_vicar) is being used, the v2param_subs routines	*/
/* are referenced in there too; thus the nominal version shares the	*/
/* same source.								*/
/************************************************************************/

/************************************************************************/
/* v2param - A program to facilitate access to parameter files written	*/
/* by shell-VICAR output routines (q_out, xqout).  This program is	*/
/* typically run in backquotes in some kind of variable setting routine,*/
/* for example:								*/
/* $R2LIB/minmax ...							*/
/* setenv MIN "`v2param minival`"					*/
/*									*/
/* See below for usage notes.						*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* prototypes for v2param_subs */

int v2param_count_elements(
                char *param_value,
                char *param_name);
char *v2param_find_entry(
                char *filename,
                char *param_name);
char *v2param_get_file();
char *v2param_get_one_value(
                char **p,
                char *param_name);
void v2param_remove_file(
                char *filename);

int main(int argc, char *argv[])
{
   int help_flag = 0;
   int name_flag = 0;
   int remove_flag = 0;
   int index_flag = 0;
   int index;
   int count_flag = 0;
   int test_flag = 0;
   int verbatim_flag = 0;

   char *param_name = NULL;
   char *param_value;
   int element_count;
   char *p, *element;

   int i;
   char *filename;

   /* Process command-line options.  Any unrecognized options are	*/
   /* assumed to be the parameter name.					*/

   for (i=1; i<argc; i++) {
      if (strcasecmp(argv[i], "-h") == 0 || strcasecmp(argv[i], "-help") == 0)
         help_flag++;
      else if (strcasecmp(argv[i], "-file") == 0)
         name_flag++;
      else if (strcasecmp(argv[i], "-rm") == 0)
         remove_flag++;
      else if (strcasecmp(argv[i], "-i") == 0) {
         index_flag++;
         if (i >= argc-1)
            help_flag++;		/* usage error */
         else {
            i++;			/* get count argument */
            index = atoi(argv[i]);
         }
      }
      else if (strcasecmp(argv[i], "-n") == 0)
         count_flag++;
      else if (strcasecmp(argv[i], "-test") == 0)
         test_flag++;
      else if (strcasecmp(argv[i], "-v") == 0)
         verbatim_flag++;
      else
         param_name = argv[i];
   }

   /* Check for help, bail out after printing message (to stderr) */

   if (help_flag) {
      fprintf(stderr, "Usage:\n\
v2param -h              Print help message (or -help)\n\
v2param -file           Report name of param file\n\
v2param -rm             Remove param file (may be combined with other options)\n\
v2param param           Report value of given param, (separate lines if needed)\n\
v2param -i 3 param      Report 4th element of given param (0 based)\n\
v2param -n param        Report number of elements in given param\n\
v2param -test param     Report 1 if param exists or 0 if not\n\
v2param -v param        Report value of given param verbatim from the file\n\
                        TBD: quotes on strings?\n" );

      exit(1);
   }

   /* Get name of parameter file to use. */

   filename = v2param_get_file();

   if (filename == NULL) {
      fprintf(stderr, "Unable to find param file name.\n");
      exit(1);
   }

   if (name_flag) {
      printf("%s\n", filename);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* If param is needed but not given, print an error */

   if (param_name == NULL) {
      if (remove_flag) {		/* remove by itself is okay */
         v2param_remove_file(filename);
         exit(0);
      }
      fprintf(stderr, "Parameter name required.\n");
      exit(1);
   }

   /* Find parameter entry */

   param_value = v2param_find_entry(filename, param_name);

   if (param_value == NULL) {
      if (test_flag) {
         printf("0\n");
         if (remove_flag)
            v2param_remove_file(filename);
         exit(0);
      }
      else
         fprintf(stderr, "Parameter '%s' not found.\n", param_name);
      exit(1);
   }
   if (test_flag) {
      printf("1\n");
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Count number of elements */

   if (count_flag) {
      element_count = v2param_count_elements(param_value, param_name);
      printf("%d\n", element_count);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Print verbatim mode */

   if (verbatim_flag) {
      printf("%s\n", param_value);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Check index */

   if (index_flag && index < 0) {
      fprintf(stderr, "Invalid index: %d\n", index);
      exit(1);
   }

   /* Print value(s) for scalar entry */

   if (*param_value != '(') {
      if (index_flag && index != 0) {
         fprintf(stderr, "Index %d is past end of parameter %s\n",
					index, param_name);
         exit(1);
      }
      p = param_value;
      element = v2param_get_one_value(&p, param_name);
      while (isspace(*p)) p++;
      if (*p != '\0')
         fprintf(stderr, "Syntax error on param %s: extra data at end\n",
						param_name);
      printf("%s\n", element);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Print value(s) for multivalued entry.  Different elements are	*/
   /* printed on different lines, although this will look the same as	*/
   /* embedded spaces to the backquote (`) command.			*/

   p = param_value+1;		/* skip the paren */
   element_count = 0;

   do {
      while (isspace(*p)) p++;
      element = v2param_get_one_value(&p, param_name);

      /* Print the value if we're not indexing, or if the index matches */

      if (element && !index_flag || index == element_count)
         printf("%s\n", element);
      if (element)
         element_count++;

      while (isspace(*p)) p++;
      if (*p != ',' && *p != ')') {
         fprintf(stderr, "Syntax error on param %s: missing , or )\n",
						param_name);
         break;
      }
      if (*p == ',') p++;		/* skip comma */
   } while (*p != ')' && *p != '\0');
   if (*p != ')') {
      fprintf(stderr, "Syntax error on param %s: missing )\n", param_name);
   }
   p++;
   while (isspace(*p)) *p++;
   if (*p != '\0') {
      fprintf(stderr, "Syntax error on param %s: extra data at end\n",
						param_name);
   }
   if (index_flag && index >= element_count)
      fprintf(stderr, "Index %d is past end of parameter %s\n",
					index, param_name);

   if (remove_flag)
      v2param_remove_file(filename);
   exit(0);

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create v2param_subs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* v2param_subs - Support routines for accessing shell-VICAR output	*/
/* parameter files.  Used mostly by v2param.c, but can be used in	*/
/* other applications which call VICAR programs as well.		*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "xvmaininc.h"

#if CUSERID_AVAIL_OS == 0
#include <pwd.h>
#endif

#define MAX_PARAM_SIZE 16384

char *v2param_get_one_value();

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

char *v2param_get_file()
{
   char *env, *userid;
   char *cuserid();
   static char default_name[100];
#if CUSERID_AVAIL_OS
   char *cuserid();
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
   while (isspace(*p)) *p++;
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create v2param.imake
#define PROGRAM v2param

#define MODULE_LIST v2param.c v2param_subs.c

#define MAIN_LANG_C
#define USES_ANSI_C

/* #define LIB_RTL */	/* Not used in the standalone version */
/* #define LIB_TAE */

$ Return
$!#############################################################################
