#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "pack.h"

/* This must be updated with each new release.                               */

char *version="1.5";

/* Cognizant Programmer:  Paul Bartholomew      April 15, 1991               */
/* Revision History:                                                         */
/* Ver    Date     F/R   Description                                         */
/* ---  --------  -----  --------------------------------------------------- */
/* 1.6  10/05/00   n/a   TXH-Disabled the conversion to all lower-cases.     */
/*                           The default now assumes inputs are mixed cases. */
/* 1.5  11/15/94   n/a   RGD-Added -mixed option to allow mixed-case names.  */
/* 1.4	06/26/92   n/a   PDB-Added "std" and "doc" unpack options.           */
/* 1.3  04/07/92   n/a   PDB-Added -q parameter.  Reversed exit() codes to   */
/*                       conform to ANSI C.  Added SYSTEM unpack options.    */
/* 1.2  01/08/92   n/a   PDB-Renamed mipl_unpack to vunpack.                 */
/* 1.1  08/07/91   n/a   PDB-Added support for the .imake template files.    */
/* 1.0  04/15/91   n/a   PDB-Original release.                               */

/* This program is designed to unpack files from a .COM file created by      */
/* VPACK.  It will unpack either selected files or all files, depending      */
/* on the keyword selected.  Current valid keywords include:  all, unpack,   */
/* source, sorc, pdf, test, make, build, other, system, and repack.  If no   */
/* keyword is selected, the program will unpack all files from the .COM file.*/

struct unpack {
   char *filename;
   int  found;
};


main(argc, argv)
int  argc;
char *argv[];
{
   FILE   *infile;
   struct unpack files[100];
   char   com_filename[80];
   int    action=0, num_files=0, which_files=SPECIFIC_FILES, verbose=TRUE;
   int mixed_case = TRUE; /* SET TO ALLOW FOR MIXED CASES, ALWAYS -- T. HUANG */


   if (argc < 2) {
      print_syntax_message();
      exit(1);
   }

/* REMOVED TO PRESERVE THE INPUT COMMAND-LINE CASES  -- T. HUANG
   mixed_case = convert_lower_case(argc, argv);
*/
   parse_command(argc, argv, com_filename, &action, &num_files, files,
                 &which_files, &verbose);

   if (verbose) {
     printf("\nBeginning program VUNPACK, Version %s\n", version);
     printf("\nOpening input file %s\n\n", com_filename);
   };

   infile = fopen(com_filename, "r");
   if (infile == NULL) {
      printf("Unable to open input file %s\n", com_filename);
      exit(1);
   }

/* The user can either unpack individual files or complete sections of files */
/* or both.  If both were selected, we'll need to reset the file pointer     */
/* after we've searched the sections so that we can begin at the beginning   */
/* again when we look for the individual data files.                         */

   if (action) {
      unpack_sections(infile, &action, num_files, files, verbose);
      if (fseek(infile, 0, SEEK_SET) != 0) {
         printf("Error setting file position in file %s\n", com_filename);
         fclose(infile);
      }
   }

/* If there are still some bits set, then there is at least one group of     */
/* files that we didn't find.  If we were looking for all the files in the   */
/* .COM file or if we were looking for the system (e.g., source, imake, and  */
/* pdf) files in the .COM file, then several bits may have been set, regard- */
/* less of whether the files were actually present in the .COM file.  For    */
/* that case, no error message is necessary.  On the other hand, if the user */
/* requested a specific set of files and we didn't find them, then an error  */
/* message is necessary.                                                     */

   if (action && (which_files != ALL_FILES && which_files != SYS_FILES &&
                  which_files != STD_FILES))
      missing_section_msg(com_filename, action);

/* Look for individual data files if that option was selected.               */

   if (num_files)
      unpack_files(com_filename, infile, num_files, files, verbose);

/* If we couldn't find some of them print appropriate error message(s).      */
   if (!all_found(num_files, files))
      missing_files_msg(com_filename, num_files, files);

   fclose(infile);
   if (verbose)
     printf("\n\n");
   exit (0);
}


/* This routine converts all the command-line arguments to lower case.  This */
/* is particularly important in the UNIX environment which is case-sensitive.*/
/* If the "-mixed" flag is seen, the rest of the line is *not* converted.    */
/* TRUE is returned if -mixed is seen, FALSE otherwise.                      */

int convert_lower_case(argc, argv)
int argc;
char *argv[];
{
   int i;

   for (i = 0; i < argc; i++) {
      lower_case(argv[i]);
      if (strcmp(argv[i], "-mixed") == 0) {
         return TRUE;
      }
   }
   return FALSE;
}


/* This function strips the extension off of the given filename and returns  */
/* the extension in the "ext" parameter.  The original filename is untouched.*/
/* If the file does not have an extension, the function returns NULL in the  */
/* "ext" parameter.                                                          */

get_file_extension(infile, ext)
char *infile;
char *ext;
{
   char *ptr;

   if ((ptr = strchr(infile, '.')) != NULL)
      strcpy(ext, ptr+1);
   else
      ext[0] = '\0';
}


/* This routine reads in the remaining command line arguments to determine   */
/* what part of the .COM file to unpack.  If there are no more arguments,    */
/* the default is to unpack everything and all the bits in the "action"      */
/* parameter are set.  If there are arguments, they are evaluated and the    */
/* corresponding bits are set.                                               */

int parse_command(argc, argv, com_filename, action, num_files, files,
                  which_files, verbose)
int  argc;
char *argv[];
char *com_filename;
int  *action;
int  *num_files;
struct unpack files[];
int  *which_files;
int  *verbose;
{
   int  i, arg_ctr, done, end_of_line, length;
   int  com_file_found=FALSE, get_filenames=FALSE;
   char *arg_ptr, cur_arg[256], *temp;

/* Traverse the list of command line arguments.  The list should begin with  */
/* the name of the file to be unpacked.                                      */

   for (arg_ctr = 1; arg_ctr < argc; arg_ctr++) {

      if (strcmp(argv[arg_ctr], "-mixed") == 0)	/* Ignore the -mixed flag */
         continue;

/* Select the next command line argument.                                    */

      arg_ptr = argv[arg_ctr];
      done = FALSE;

/* And loop until we have found all the file names present in the argument.  */

      while (!done) {

/* First strip any white space--e.g., commas and spaces.                     */

         end_of_line = strip_space(&arg_ptr);

/* Next, copy the parameter to a temporary storage location and look for the */
/* end of the current character string.  We null-terminate the temporary     */
/* variable where the current character string ends.                         */

         strcpy(cur_arg, arg_ptr);
         temp = cur_arg;
         end_of_line = find_next_space(&temp);
         *temp = '\0';

/* If we didn't reach a null terminator, then there is more than one file-   */
/* name in this parameter and we will need to continue parsing the string.   */
/* If we do find a null terminator, we're done with the string.              */

         end_of_line = find_next_space(&arg_ptr);
         if (end_of_line)
            done = TRUE;

/* The first command line argument should contain the name of the .COM file  */
/* we are creating.  We copy that to the parameter, then continue with the   */
/* next argument.                                                            */

         if (!com_file_found) {
            strcpy(com_filename, cur_arg);
            com_file_found = TRUE;
            continue;
         }

/* Now determine what the current argument is and what action we need to do  */
/* as a result.                                                              */

/* If we have a "-f" as a parameter, then we will be unpacking specific      */
/* files.  We will need to evaluate subsequent parameters as filenames rather*/
/* than keywords.  Since we want to allow mixed use--i.e., both specific     */
/* files as well as file sections--we also want to check each subsequent     */
/* parameter to determine if we're back into keywords again.                 */

         if (!strcmp(cur_arg, "-f")) {
            get_filenames = TRUE;
            continue;
         }
         else if (!strcmp(cur_arg, "-q")) {
            *verbose = FALSE;
            get_filenames = FALSE;
            continue;
         }
         else if (check_keywords(cur_arg, action, which_files)) {
            get_filenames = FALSE;
         }

/* If the parameter wasn't a keyword and we're not picking up filenames, then*/
/* we have an invalid parameter, which we ignore.                            */

         else if (!get_filenames) {
            printf("\nInvalid parameter -- \"%s\".\n", cur_arg);
            printf("VUNPACK will ignore it.\n");
         }

/* If we are picking up filenames, we copy the current argument into the     */
/* array and increment the file counter.                                     */

         if (get_filenames) {
            length = (int) (temp - cur_arg);
            files[*num_files].filename = (char *) malloc(length+1);
            strcpy(files[*num_files].filename, cur_arg);
            files[*num_files].found = FALSE;
            (*num_files)++;
         }
      }
   }

/* The "which_files" flag is used to tell us whether the user has requested  */
/* a specific set of files, all of the files, or the system set of files be  */
/* unpacked.  Normally, this program will check to see if the files that the */
/* user has requested are in the .COM file and were actually unpacked.  This */
/* program will return an error message telling the user about any missing   */
/* set(s) of files.  If all files or system set of files were requested,     */
/* however, then we are not looking for a specific set of files and no error */
/* message is necessary.                                                     */

/* If we didn't have any parameters then we unpack everything.               */

   if (get_filenames==FALSE && *action == 0) {
      *action = ALL_BITS;
      *which_files = ALL_FILES;
   }
   return SUCCESS;
}


/* This routine unpacks the requested files from the input COM file.  It     */
/* can unpack the Repack file, the Source file(s), the Make files, the PDF   */
/* files, the Test file(s), or any combination of the above.  It does this by*/
/* reading in the input .COM file, one line at a time, checking for the      */
/* labels that begin each section.  When it finds a label, it determines     */
/* whether that section has been requested.  If so, it unpacks it--if not, it*/
/* loops to the next section.                                                */

int unpack_sections(infile, action, num_files, files, verbose)
FILE   *infile;
int    *action;
int    num_files;
struct unpack files[];
int    verbose;
{
   int  done=FALSE, ctr=0, found, section_done, status;
   char str[256], filename[80];

   while (!done) {

/* Step 1:  Find the beginning of the next section in the COM file.          */

      found = FALSE;
      while (!found && !done) {
         if (fgets(str, 255, infile) == NULL)
            done = TRUE;
         found = find_section_header(str, &ctr);
      }

/* Step 2:  Determine if we need the files from this section by checking the */
/*          appropriate bit in the "action" parameter.  If we don't need     */
/*          them, continue until we get to the next section.                 */

      if (found && BIT_TEST(*action, 1 << ctr)) {

         section_done = FALSE;

/* Step 3:  Unpack all the files from the current section and write them out */
/*          to disk.  Continue until you reach the "end of section" marker   */
/*          in the COM file or until you reach the end of the file.          */

         while (fgets(str, 255, infile) != NULL && !done && !section_done) {

/* Skip over the intermediate garbage and look for a line that begins        */
/* "$ create".  That's the line that will give us our file name and that     */
/* signals the beginning of the file.                                        */

            if (!strncmp(str, "$ create", 8)) {
               get_filename(str, filename);
               if (verbose)
                  printf("Unpacking file %s\n", filename);

/* Since we may be unpacking individual files as well as sections of files,  */
/* we check this filename against the list of files we are to unpack and set */
/* the found flag if we locate it.                                           */

               check_filename(filename, num_files, files);

/* Now write the file out to disk.                                           */

               if (write_source_file(infile, filename, &section_done) != SUCCESS) {
                  printf("\nUnable to create output file %s\n", filename);
                  done = TRUE;
               }
            }

/* If we find a "return" line, we're finished with the current section and   */
/* we break out of this loop to go back to the top and check the next section*/

            else if (!strncmp(str, "$ Return", 8))
               section_done = TRUE;
         }
      }

/* Clear the set bit.  This way, if only one bit is set, we don't have to    */
/* loop through the entire file.  We can stop when we've unpacked as much of */
/* the file as is requested.                                                 */

      if (found) {
         *action = CLEAR_BIT(*action, 1 << ctr);
         if (!(*action))
            done = TRUE;
      }
   }
   return SUCCESS;
}


int unpack_files(com_filename, infile, num_files, files, verbose)
char *com_filename;
FILE *infile;
int  num_files;
struct unpack files[];
{
   int  done=FALSE, section_done=FALSE;
   char str[256], filename[80];

   while (fgets(str, 255, infile) != NULL && !done) {

/* Skip over the intermediate garbage and look for a line that begins        */
/* "$ create".  That's the line that will give us our file name and that     */
/* signals the beginning of the file.                                        */

      if (!strncmp(str, "$ create", 8)) {
         get_filename(str, filename);
         if (check_filename(filename, num_files, files)) {
            if (verbose)
               printf("Unpacking file %s\n", filename);

/* Now write the file out to disk.                                           */

            if (write_source_file(infile, filename, &section_done) != SUCCESS) {
               printf("\nUnable to create output file %s\n", filename);
               done = TRUE;
            }
         }
         if (all_found(num_files, files))
            done = TRUE;
      }
   }
   return SUCCESS;
}


/* This routine creates the output file from the information in the input    */
/* COM file.  The file name for the newly created file is also a parameter   */
/* to the routine.  The routine continues passing information from the input */
/* file to the output file until one of the following occurs:                */
/* 	1.  It reaches the end of the input file.                            */
/* 	2.  It reaches a terminator in the COM file signalling the end of    */
/*          the source file.  This terminator will either be a comment line  */
/*          (i.e., a line that begins with "$!") or a return line (a line    */
/*          that begins with "$ Return").                                    */
/* This routine also handles the special case of a COM file embedded in a COM*/
/* file.  If it finds that the file begins with DECK, it ignores the second  */
/* case above and terminates only when it receives an EOD.                   */

int write_source_file(infile, filename, end_of_section)
FILE *infile;
char *filename;
int  *end_of_section;
{
   FILE *tempfile;
   char str[256];
   int  done=FALSE, comfile=FALSE;

/* Open the output file.                                                     */

#ifdef vms
   tempfile = fopen(filename, "w", "rat=cr", "rfm=var");
#else
   tempfile = fopen(filename, "w");
#endif
   if (tempfile == NULL)
     return FAILURE;

/* Determine if we have any information remaining in the input file.         */

   if (fgets(str, 255, infile) == NULL)
      return FAILURE;

/* Check for a .COM or a .BLD file.  If not, output the first line to the    */
/* newly created file.  If so, set the flag.                                 */

   if (strncmp(str, "$ DECK", 6) == 0)
      comfile = TRUE;
   else
      fputs(str, tempfile);

/* Continue passing information from the old file to the new until done.     */

   while (!done) {

/* Case 1:  We've reached the end of the input file.                         */

      if (fgets(str, 255, infile) == NULL)
         done = TRUE;

/* Case 2:  This is a .COM or .BLD file and we've reached the end of file    */
/*          marker.                                                          */

      else if (comfile && !strncmp(str, EOFSTRING, strlen(EOFSTRING)))
         done = TRUE;

/* Case 3:  We've reached a comment line or return line in the original      */
/*          .COM file, signalling the end of the "create".                   */

      else if (!strncmp(str, "$!", 2) && !comfile)
         done = TRUE;
      else if (!strncmp(str, "$ Return", 8) && !comfile) {
         done = TRUE;
         *end_of_section = TRUE;
      }

/* Otherwise, we transfer the string to the new file.                        */

      else
         fputs(str, tempfile);
   }
   fclose(tempfile);
   return SUCCESS;
}


/* This routine strips the file name from the input string and returns it in */
/* the filename parameter.  We find the file names in the COM file when we   */
/* locate a string that begins:  "$ create filename.ext".  By setting an     */
/* offset into the string, we can copy just the file name.  In addition, the */
/* string includes a "newline" character ("\n"), which must be removed from  */
/* the end of the file name.                                                 */

get_filename(instr, filename)
char *instr;
char *filename;
{
   int length;

   strcpy(filename, instr+NAME_OFFSET);
   length = strlen(filename);

/* Truncate the file name by one character to remove the newline character.  */

   filename[length-1] = '\0';
   return SUCCESS;
}


/* This function converts a string to upper case.                            */

upper_case(str)
char *str;
{
   int i;

   for (i = 0; str[i] != '\0'; i++)
      str[i] = toupper(str[i]);
}


/* This function converts a string to lower case.                            */

lower_case(str)
char *str;
{
   int i;

   for (i = 0; str[i] != '\0'; i++)
      str[i] = tolower(str[i]);
}


print_syntax_message()
{
   printf("\nVUNPACK, Version %s\n", version);
   printf("\nImproper syntax.  This program should be called as follows:\n");
   printf("\n    vunpack file.com [keyword(s)]\n");
   printf("or: vunpack file.com -f filename.1 filename.2 etc.\n");
   printf("\nThe keywords are:  source, pdf, imake, build, make, test, repack,");
   printf("\n                   doc, std, system, unpack, all.\n");
   printf("If you omit the keywords, the default is to unpack all of the files.\n");
}


/* This routine determines if the current line contains a section header,    */
/* as given by the "section" array.  If so, it returns the array index of    */
/* the section header.  If we shift 1 left the number of bits corresponding  */
/* to the array index, and compare it to the "action" variable in the call-  */
/* ing routine, we can determine if we need the files from this section.     */

int find_section_header(str, ptr)
char *str;
int  *ptr;
{
   int found=FALSE, i;

   if (substring(str, "_File:")) {
      for (i = 0; sections[i][1] != 0 && !found; i++) {
         if (!strncmp(str, sections[i][1], strlen(sections[i][1]))) {
            found=TRUE;
            *ptr = i;
         }
      }
   }
   return found;
}


/* This routine checks a filename against the list of files to be unpacked.  */
/* It returns TRUE if the filename matches one of the files to be unpacked   */
/* and returns FALSE otherwise.  This routine also sets the "found" flag in  */
/* the files data structure to indicate that the file has been found.        */

int check_filename(filename, num_files, files)
char   *filename;
int    num_files;
struct unpack files[];
{
   int i, found=FALSE;

   for (i = 0; i < num_files; i++) {
      if (!strcmp(filename, files[i].filename)) {
         found = TRUE;
         files[i].found = TRUE;
         break;
      }
   }
   return found;
}



/* This routine determines if all of the files to be unpacked have been      */
/* found.  It traverses the files array looking at the "found" flag in the   */
/* data structure.  The first flag that is FALSE indicates that not all of   */
/* the files has been found, so this routine exits immediately rather than   */
/* wasting time checking the remaining flags.  If all of the flags have been */
/* set to TRUE, this function returns TRUE as well.                          */

int all_found(num_files, files)
int    num_files;
struct unpack files[];
{
   int i, all=TRUE;

   for (i = 0; i < num_files; i++) {
      if (files[i].found == FALSE) {
         all = FALSE;
         break;
      }
   }
   return all;
}


/* This function is called whenever the program is unable to find one or more*/
/* requested files.  It tells the user which files were not found in the     */
/* .COM file.                                                                */

missing_files_msg(com_filename, num_files, files)
char   *com_filename;
int    num_files;
struct unpack files[];
{
   int i;

   for (i = 0; i < num_files; i++) {
      if (!files[i].found)
         printf("Unable to locate file %s in %s.\n", files[i].filename,
                com_filename);
   }
}


/* This routine is called whenever the program is unable to find a requested */
/* section of files (e.g., "source").  It determines which bits are still    */
/* set in the unpack "action" parameter (i.e., which sections were not found)*/
/* and prints an appropriate error message.                                  */

missing_section_msg(com_filename, action)
char *com_filename;
int  action;
{
   if (BIT_TEST(action, REPACK_BIT))
      printf("Unable to locate REPACK file in file %s\n", com_filename);
   if (BIT_TEST(action, SOURCE_BIT))
      printf("Unable to locate SOURCE file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, MAKE_BIT))
      printf("Unable to locate MAKE file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, IMAKE_BIT))
      printf("Unable to locate IMAKE file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, BUILD_BIT))
      printf("Unable to locate BUILD file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, PDF_BIT))
      printf("Unable to locate PDF file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, TEST_BIT))
      printf("Unable to locate TEST file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, DOC_BIT))
      printf("Unable to locate DOC file(s) in file %s\n", com_filename);
   if (BIT_TEST(action, OTHER_BIT))
      printf("Unable to locate OTHER file(s) in file %s\n", com_filename);
}


int check_keywords(cur_arg, action, which_files)
char *cur_arg;
int  *action;
int  *which_files;
{
   int found=FALSE;

   if (!strcmp(cur_arg, "source") || !strcmp(cur_arg, "sorc")) {
      *action |= SOURCE_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "build")) {
      *action |= BUILD_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "make")) {
      *action |= MAKE_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "imake")) {
      *action |= IMAKE_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "pdf")) {
      *action |= PDF_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "system")) {
      *action |= SYS_BITS;
      *which_files = SYS_FILES;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "std") || !strcmp(cur_arg, "standard")) {
      *action |= STD_BITS;
      *which_files = STD_FILES;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "test")) {
      *action |= TEST_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "doc")) {
      *action |= DOC_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "repack")) {
      *action |= REPACK_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "other")) {
      *action |= OTHER_BIT;
      found = TRUE;
   }
   else if (!strcmp(cur_arg, "unpack") || !strcmp(cur_arg, "all")) {
      *action |= ALL_BITS;
      *which_files = ALL_FILES;
      found = TRUE;
   }
   return found;
}


/* This program strips white space from the front of a string, setting a     */
/* pointer to the first non-space character in the string.  If this non-     */
/* space character is the end of the line or the end of the string, then     */
/* the function returns TRUE.  Otherwise, the function returns FALSE.        */

int strip_space(ptr)
char **ptr;
{
   int end_of_line=FALSE;

   while (**ptr == ' ' || **ptr == '\t' || **ptr == ',')
      (*ptr)++;

   if (**ptr == '-' && (*((*ptr)+1) == '\n' || *((*ptr)+1) == '\0'))
      end_of_line = TRUE;

   if (**ptr == '\n' || **ptr == '\0')
      end_of_line = TRUE;

   return end_of_line;
}


/* This function moves a string pointer to the first non-character in the    */
/* string--i.e., the first separator.  If the pointer is pointing to a new-  */
/* line or the end of the string, the function returns TRUE.                 */

int find_next_space(ptr)
char **ptr;
{
   int end_of_line=FALSE;

   while (**ptr != ' ' && **ptr != '\t' && **ptr != ',' && **ptr != '\n' &&
          **ptr != '\0')
      (*ptr)++;

   if (**ptr == '\n' || **ptr == '\0')
      end_of_line = TRUE;

   return end_of_line;
}


/* This routine determines if a string is present in another string.  It     */
/* returns TRUE if the second string is found in the first, FALSE otherwise. */
/* This is similar to the strstr() function, which is not supported on some  */
/* C compilers.                                                              */

int substring(str, substr)
char *str, *substr;
{
   char *ptr;

/* If the substring is longer than the string we're checking, then it can't  */
/* be contained in it, so we return FALSE.                                   */

   if ((int)strlen(substr) > (int)strlen(str))
      return FALSE;

/* Similarly, if the length of either string is zero, we can also return     */
/* FALSE.                                                                    */

   if (strlen(substr) == 0 || strlen(str) == 0)
      return FALSE;

/* Otherwise, set the pointer to beginning of the string.                    */

   ptr = str;

/* When the character pointed to by the pointer matches the first character  */
/* in the substring, we do a string compare to see if we have a complete     */
/* match.  If not, we continue checking.  If so, we're done.                 */

   while(ptr = strchr(ptr, *substr)) {
      if (!strncmp(ptr, substr, strlen(substr)))
         return TRUE;
      else
         ptr++;
   }
   return FALSE;
}
