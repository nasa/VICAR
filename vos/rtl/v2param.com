$!****************************************************************************
$!
$! Build proc for MIPL module v2param
$! VPACK Version 1.8, Thursday, November 06, 1997, 16:46:27
$!
$! Execute by entering:		$ @v2param
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
$ write sys$output "*** module v2param ***"
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
$ write sys$output "Invalid argument given to v2param.com file -- ", primary
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
$   if F$SEARCH("v2param.imake") .nes. ""
$   then
$      vimake v2param
$      purge v2param.bld
$   else
$      if F$SEARCH("v2param.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake v2param
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @v2param.bld "STD"
$   else
$      @v2param.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create v2param.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack v2param.com -
	-s v2param.c -
	-i v2param.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create v2param.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*   N  O  T  E								*/
/* Maintain this file in parallel with util/v2param_standalone.com.	*/
/* See that file for further comments and explanation.			*/
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

#include "xvmaininc.h"
#include "zvproto.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "strcasecmp.h"

int main(int argc, char *argv[])
{
   int help_flag = 0;
   int name_flag = 0;
   int remove_flag = 0;
   int index_flag = 0;
   int index = 0;
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

      if ((element && !index_flag) || (index == element_count))
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
   while (isspace(*p)) p++;
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
$ Return
$!#############################################################################
$Imake_File:
$ create v2param.imake
#define PROGRAM v2param

#define MODULE_LIST v2param.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
