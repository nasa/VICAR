#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "pack.h"

#define COMMENT		fputs("$!\n", output_file);
#define FILE_SEP	fputs("$!-----------------------------------------------------------------------------\n", output_file);
#define SECTION_SEP	fputs("$!#############################################################################\n", output_file);
#define LINE_LENGTH	77
#define UNPORTED_MODE	-99
#define MIXED_MODE      -98

/* This must be updated with each new release.                               */

char *version="1.9";

/* Cognizant Programmer:  Paul Bartholomew	April 15, 1991               */
/* Revision History:                                                         */
/* Ver    Date     F/R   Description                                         */
/* ---  --------  -----  --------------------------------------------------- */
/* 2.0  10/05/00   n/a   TXH-Disabled the conversion to all lower-cases.     */
/*                           The default now assumes inputs are mixed cases. */
/* 1.9  10/27/97   n/a   VXP-Added 'newline' symbol to the end of a file if  */
/*                          that file doesn't end with 'newline' already.    */
/* 1.8  11/15/94   n/a   RGD-Added '-mixed' option to allow mixed-case names */
/* 1.7  07/01/93  81880  PDB-Added '-u' option for use on unported modules;  */
/*                          added code to the .com file header to flag       */
/*                          an invalid parameter to the .com file.           */
/* 1.6  01/12/93   n/a   PDB-Fixed SGI compatibility problem by changing an  */
/*                          fgetc() to an fgets().                           */
/* 1.5  07/20/92   n/a   PDB-Modified default option when no source code from*/
/*                          "unpack" to "pdf".                               */
/* 1.4  06/26/92   n/a   PDB-Added "std" and "doc" options.                  */
/* 1.3  04/07/92   n/a   PDB-Reversed exit() codes to conform to ANSI C.     */
/*                          Added code to insert DECK/DOLLARS in front of    */
/*                          source code.                                     */
/* 1.2  01/08/92   n/a   PDB-Renamed mipl_pack to vpack.                     */
/* 1.1  08/07/91   n/a   PDB-Added support for .imake template files.        */
/* 1.0  04/15/91   n/a   PDB-Original Release                                */

/* This module is designed to pack a selected list of files into a ".COM"    */
/* file.  It is designed to be (relatively) system-independent, using only   */
/* ANSI C functions to enable it to be compiled and executed on a variety of */
/* platforms.  The .COM file created is designed to be self-extracting/com-  */
/* piling/linking in the VAX VMS environment, but will need an "UNPACK"      */
/* utility in the UNIX environment.                                          */

/* The files packed into the .COM file may include:                          */
/* 1.  Source file(s)                                                        */
/* 2.  VMS build file(s)                                                     */
/* 3.  UNIX make file(s)                                                     */
/* 4.  .imake file templates (which will use the vimake utility)             */
/* 5.  PDF file(s)                                                           */
/* 6.  Test file(s)                                                          */
/* 7.  Other file(s)                                                         */
/* In addition, this program will create a Repack file that it will insert   */
/* into the .COM file.                                                       */

/* The files may be in any order, but must be contained within a section,    */
/* e.g., all the source files must be together, but may be in any order      */
/* within the section.  Also, the sections may be in any order in the .COM   */
/* file.  All the sections are optional and it is possible to build a .COM   */
/* file that contains only source code, for example.                         */

struct pack {
   char *filename;
   int  filetype;
};


main(argc, argv)
int  argc;
char *argv[];
{
   int  num_files=0, action=0, ported=TRUE;
   struct pack files[100];
   char com_filename[80];
   FILE *output_file;
   int mixed_case = TRUE; /* SET TO ALLOW FOR MIXED CASES, ALWAYS -- T. HUANG */

/* Convert all the command line options, filenames, etc. to lower case.      */
/* REMOVED TO PRESERVE THE INPUT COMMAND-LINE CASES  -- T. HUANG
   mixed_case = convert_lower_case(argc, argv);
*/
   printf("\nBeginning program VPACK, Version %s\n", version);

   if (argc < 2) {
      print_syntax_message();
         exit(1);
   }
   else if (argc == 2) {
      if (!parse_file(argv[1], com_filename, &num_files, files, &action, &ported, &mixed_case))
         exit(1);
   }
   else if (argc > 2) {
      parse_command(argc, argv, com_filename, &num_files, files, &action, &ported, &mixed_case);
   }

/* Open the .COM file to be created.                                         */

#ifdef vms
   output_file = fopen(com_filename, "w", "rat=cr", "rfm=var");
#else
   output_file = fopen(com_filename, "w");
#endif
   if (output_file == NULL) {
      printf("\nUnable to open output file %s\n", com_filename);
      exit(1);
   }

   printf("\nCreating file %s\n\n", com_filename);

/* Write the file header of the newly created .COM file.  This includes all  */
/* the shell script commands which make the file self-extracting.            */

   write_header(output_file, com_filename, num_files, files, action, ported, mixed_case);

/* Create the .REPACK file that will automatically call this program to      */
/* repack the file at a later time.  This becomes part of the new .COM file  */
/* and is created each time the main .COM file is unpacked.                  */

   create_repack(output_file, com_filename, num_files, files, ported, mixed_case);

/* Pack the files into the .COM file.                                        */

   add_files(output_file, num_files, files);

   fclose(output_file);
   printf("\n");
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


/* This routine checks a command-line parameter to determine if it is one of */
/* the reserved options (e.g., "-s", "-o", etc.).  If so, the function       */
/* returns TRUE, and the integer corresponding to the option is returned in  */
/* the "option" parameter.  If not, the function returns FALSE and the       */
/* "option" parameter is unchanged.                                          */

int check_options(parm, option, found)
char *parm;
int  *option;
int  *found;
{
   int i, valid = TRUE;

   *found = FALSE;

   if (!strncmp(parm, "-u", 2)) {
      *found = TRUE;
      valid = TRUE;
      *option = UNPORTED_MODE;
      return valid;
   }

   if (!strncmp(parm, "-mixed", 6)) {
      *found = TRUE;
      valid = TRUE;
      *option = MIXED_MODE;
      return valid;
   }

   if (parm[0] == '-' && parm[1] != '\n' && parm[1] != '\0' && parm[1] != ' ') {
      for (i = 0; sections[i][0] != 0 && !(*found); i++) {
         if (!strcmp(parm, sections[i][0])) {
            *found=TRUE;
            *option = i;
            valid = TRUE;
         }
      }
      if (!(*found))
         valid = FALSE;
   }
   return valid;
}


/* This routine parses a repack file to find the name of the .COM file to be */
/* created and the names of the file to be packed into the file.             */

int parse_file(repack_filename, com_filename, num_files, files, action, ported, mixed_case)
char *repack_filename;
char *com_filename;
int  *num_files;
struct pack files[];
int  *action, *ported;
int *mixed_case;
{
   FILE *repack_file;
   char str[256], *ptr, cur_arg[256], *temp;
   int  length, end_of_line, option = -1, done=FALSE, valid, found, prev = -1;

   repack_file = fopen(repack_filename, "r");
   if (repack_file == NULL) {
      printf("\nUnable to open the repack file \"%s\".\n", repack_filename);
      return FAILURE;
   }

/* Check the beginning lines of the file for the name of the .COM file to be */
/* built by vpack.  We use the "str" parameter to read in the file, one line */
/* at a time.  The "ptr" parameter will mark where we are in the current     */
/* string after we return from the routine.  This is to handle the case where*/
/* there is more than one filename on a line.                                */

   if (!get_com_filename(repack_file, com_filename, str, &ptr, &end_of_line, *mixed_case)) {
      printf("\nError reading data from repack file \"%s\".\n", repack_filename);
      fclose(repack_file);
      return FAILURE;
   }

/* Search the file, one line at a time, until we reach the end of the file.  */

   while (!done) {

/* First we determine if we're at the end of the line.  If we are, we pick   */
/* up the next string from the file.                                         */

      if (end_of_line) {
         if ((ptr = fgets(str, 255, repack_file)) == NULL)
            done = TRUE;
         else if (!strncmp(str, "$ Exit", 6))
            done = TRUE;
         else if (*ptr == '$')
            ptr++;
      }

/* If the file contains blank lines or lines that contain only null space,   */
/* this loop will get us past them to lines that contain data.               */

      while (!done && (end_of_line = strip_space(&ptr)) == TRUE) {
         if ((ptr = fgets(str, 255, repack_file)) == NULL)
            done = TRUE;
         else if (!strncmp(str, "$ Exit", 6))
            done = TRUE;
         else if (*ptr == '$')
            ptr++;
      }

/* Now we pick up the next argument in the string by copying the string to   */
/* the cur_arg string.  We look for the next space in the string and null-   */
/* terminate the cur_arg string there.  This will leave us with the cur_arg  */
/* string containing only one parameter.  We then move the pointer to our    */
/* original string past this one parameter.  The next time through the loop, */
/* we'll be working with the next parameter in the line.                     */

      if (!done) {
         strcpy(cur_arg, ptr);
         temp = cur_arg;
         end_of_line = find_next_space(&temp);
         *temp = '\0';
         if (!*mixed_case)
            lower_case(cur_arg);

/* Now move the pointer in the original string past the parameter we just    */
/* created.                                                                  */

         end_of_line = find_next_space(&ptr);

/* Check the current argument to determine if it is a command-line option,   */
/* e.g., "-s".  If it is, we set the flag to indicate what type of files we  */
/* will be processing next--then continue to the next argument.  If it is    */
/* not, then it's a filename which we add to the array.  We also add the     */
/* file type based on the most recent "-x" parameter.                        */

         valid = check_options(cur_arg, &option, &found);

         if (!valid) {
            printf("\nInvalid option \"%s\"\n", cur_arg);
            return FAILURE;
         }

         if (!found) {

            if (option == -1) {
               printf("\nMissing command-line option.  VPACK doesn't know what type of files\n");
               printf("these are (e.g., \"-s\", \"-p\", etc.).\n");
               return FAILURE;
            }

/* Allocate storage space for the filename and copy the filename into the    */
/* array.  Increment the file counter.                                       */

            length = (int) (temp - cur_arg);
            files[*num_files].filename = (char *) malloc(length+1);
            strcpy(files[*num_files].filename, cur_arg);

/* Set the file type based on the most recent "-x" parameter.                */

            files[*num_files].filetype = option;
            (*num_files)++;
         }
         else {
            if (option == UNPORTED_MODE) {
               *ported = FALSE;
            }
            else if (option == MIXED_MODE) {
               *mixed_case = TRUE;
            }

/* Set the appropriate bit in the "action" parameter to indicate that we are */
/* processing the given type of files.                                       */

            else if (option != prev) {
               *action = SET_BIT(*action, 1 << option);
               prev = option;
            }
         }
      }
   }

/* Since this program builds the repack file itself, we always set the repack*/
/* bit.                                                                      */

   *action = SET_BIT(*action, REPACK_BIT);
   fclose(repack_file);
   return SUCCESS;
}


/* This routine searches through the first few lines of a repack file for    */
/* the name of the .COM file to be built by vpack.  It returns the name of   */
/* the file in the com_filename parameter.  We use the "str" parameter to    */
/* hold the contents of the file, one line at a time.  We use the "ptr" para-*/
/* meter to mark our place in the current string.  This is done because the  */
/* line containing the .COM filename may also hold other parameters.  So we  */
/* pass the string and the pointer back to the calling context.  This func-  */
/* tion also checks for the end of the current line and passes that informa- */
/* tion back in the end_of_line parameter.  If there is an error reading from*/
/* the file, this functions returns FAILURE.  Otherwise, it returns SUCCESS. */

int get_com_filename(repack_file, com_filename, str, ptr, end_of_line, mixed_case)
FILE *repack_file;
char *com_filename;
char *str, **ptr;
int  *end_of_line;
int mixed_case;
{
   char *temp, *substring();

/* First we try to find the name of the .COM file to pack the other files in */

   if (fgets(str, 255, repack_file) != NULL) {

/* If we find the name of the program on this line, then we look for the     */
/* first non-blank character after that.  That should be the beginning of    */
/* the name of our .COM file.                                                */

      if ((*ptr = substring(str, "vpack")) != NULL)
         (*ptr) += 6;
      else if ((*ptr = substring(str, "mipl_pack")) != NULL)
         (*ptr) += 10;

/* If we didn't find "mipl_pack" on the first line, then we can assume that  */
/* we've begun the repack file with the name of the .COM file to be created. */

      else 
         *ptr = str;
   }

/* If we couldn't read anything from the file, return with an error.         */

   else
      return FAILURE;

   if (**ptr == '$')
      (*ptr)++;

/* Move the pointer past the blank spaces.                                   */

   while ((*end_of_line = strip_space(ptr)) == TRUE) {
      if ((*ptr = fgets(str, 255, repack_file)) == NULL)
         return FAILURE;
      else if (**ptr == '$')
         (*ptr)++;
   }

/* Theoretically, our pointer should be pointing to the name of the .COM file*/

   strcpy(com_filename, *ptr);
   temp = com_filename;

/* Now we look for the first space in the filename and null-terminate there. */

   *end_of_line = find_next_space(&temp);
   *temp = '\0';
   if (!mixed_case)
      lower_case(com_filename);

/* Now move the pointer in the original string past the file name we just    */
/* created.                                                                  */

   *end_of_line = find_next_space(ptr);
   return SUCCESS;
}


/* This routine searches through the command-line parameter list to deter-   */
/* the names of the files to be packed into the com file.  It accepts the    */
/* argument list as input, and outputs the list of files.  The two may not   */
/* be the same, since the user can separate the command line parameters with */
/* a comma rather than with a space.  In this instance, the list of files    */
/* would all be present in one parameter rather than a separate file for each*/
/* parameter.                                                                */

int parse_command(argc, argv, com_filename, num_files, files, action, ported, mixed_case)
int  argc;
char *argv[];
char *com_filename;
int  *num_files;
struct pack files[];
int  *action, *ported;
int  *mixed_case;
{
   int  arg_ctr, done, option = -1, com_file_found=FALSE, end_of_line, length;
   int  found, valid, prev = -1;
   char *arg_ptr, cur_arg[256], *temp;

/* Traverse the list of command line arguments, beginning with the first file*/
/* name.  The argument list may also include some command line options for   */
/* this program as well as the name of the output file--hence we process     */
/* those first before calling this routine.                                  */

   for (arg_ctr = 1; arg_ctr < argc; arg_ctr++) {

/* Select the next command line argument.                                    */

      arg_ptr = argv[arg_ctr];
      done = FALSE;

/* And loop until we have found all the file names present in the argument.  */

      while (!done) {

/* First strip any white space--e.g., commas and spaces.                     */

         end_of_line = strip_space(&arg_ptr);

/* Next, copy the parameter to a temporary storage location and look for the */
/* end of the current character string.  We null terminate the temporary     */
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

/* Check the current argument to determine if it is a command-line option,   */
/* e.g., "-s".  If it is, we set the flag to indicate what type of files we  */
/* will be processing next--then continue to the next argument.  If it is    */
/* not, then it's a filename which we add to the array.  We also add the     */
/* file type based on the most recent "-x" parameter.                        */

         valid = check_options(cur_arg, &option, &found);

         if (!valid) {
            printf("\nInvalid option \"%s\"\n", cur_arg);
            return FAILURE;
         }

         if (!found) {

            if (option == -1) {
               printf("\nMissing command-line option.  VPACK doesn't know what type of files\n");
               printf("these are (e.g., \"-s\", \"-p\", etc.).\n");
               return FAILURE;
            }

/* Allocate storage space for the filename and copy the filename into the    */
/* array.  Increment the file counter.                                       */

            length = (int) (temp - cur_arg);
            files[*num_files].filename = (char *) malloc(length+1);
            strcpy(files[*num_files].filename, cur_arg);

/* Set the file type based on the most recent "-x" parameter.                */

            files[*num_files].filetype = option;
            (*num_files)++;
         }
         else {
            if (option == UNPORTED_MODE) {
               *ported = FALSE;
            }
            else if (option == MIXED_MODE) {
               *mixed_case = TRUE;
            }

/* Set the appropriate bit in the "action" parameter to indicate that we are */
/* processing the given type of files.                                       */

            else if (option != prev) {
               *action = SET_BIT(*action, 1 << option);
               prev = option;
            }
         }
      }
   }

/* Since this program builds the repack file itself, we always set the repack*/
/* bit.                                                                      */

   *action = SET_BIT(*action, REPACK_BIT);

   return SUCCESS;
}


/* This routine appends files to the .COM file.  Since the .COM file has to  */
/* be self-extracting in the VMS environment, this routine also has to add   */
/* labels to the .COM file in strategic locations--e.g., before the source   */
/* file(s), before the build file, before the make file, before the pdf file,*/
/* and before the test file(s).  We use the file type in the files array     */
/* to let us know when we are shifting from one type to the next.            */

int add_files(output_file, num_files, files)
FILE *output_file;
int  num_files;
struct pack files[];
{
   FILE *tmpfile;
   char *tmpchar, file_ext[10], tmpstr[256];
   int  i, com_file, status, type= -1, no_ext=FALSE;

   for (i = 0; i < num_files; i++) {
      printf("Appending file %s\n", files[i].filename);
      com_file = FALSE;

/* Open the file (for read-only) so that we can transfer its contents to the */
/* new .COM file.                                                            */

      tmpfile = fopen(files[i].filename, "r");
      if (tmpfile == NULL) {
         printf("\nError opening file.  Unable to append file %s.\n", files[i].filename);
         return FAILURE;
      }

/* Separate out the file extension so that we can determine the file type.   */

      get_file_extension(files[i].filename, file_ext);
      if (file_ext[0] == '\0')
         no_ext = TRUE;

/* If the new file type differs from the most recent, we are beginning a new */
/* section.  We add the necessary separator garbage to the .COM file.        */

      if (files[i].filetype != type) {
         type = files[i].filetype;
         if (i != 0) {
            fputs("$ Return\n", output_file);
            SECTION_SEP;
         }
         fprintf(output_file, "%s\n", sections[type][1]);
      }

/* If the file is a COM file then it will include script commands that need  */
/* be handled differently.  In the .COM file we are creating, we use the     */
/* "$ create filename" command to create the file.  The "create" command     */
/* terminates when it encounters a line beginning with a "$", which would    */
/* mean that a COM file cannot create another COM file.  We get around this  */
/* by bracketing the COM file we will wish created with the "DECK" and "EOD" */
/* commands.                                                                 */

      if (!no_ext) {
         if (!strcmp(file_ext, "com") || !strcmp(file_ext, "make") ||
             !strcmp(file_ext, "bld"))
            com_file = TRUE;
      }

/* Belatedly remembered that it is possible for source files to also have    */
/* lines beginning with a "$" (e.g., "$DESCRIPTOR").  This means that source */
/* files will also have to be bracketed.                                     */

      if (files[i].filetype == SOURCE_LABEL)
         com_file = TRUE;

/* Added the new documentation file type, which may also have lines begin-   */
/* ning with a "$".  Unlikely, perhaps, but possible.                        */

      if (files[i].filetype == DOC_LABEL)
         com_file = TRUE;

      fprintf(output_file, "$ create %s\n", files[i].filename);
      if (com_file)
         fprintf(output_file, "$ DECK/DOLLARS=\"%s\"\n", EOFSTRING);

/* Now append the file to the COM file, one line at a time.                  */

      while ((tmpchar = fgets(tmpstr, 255, tmpfile)) != NULL)
         fputs(tmpstr, output_file);

/* Now append 'newline' character if the file doesn't end with one.          */

      if (tmpstr[strlen(tmpstr)-1] != '\n')
	 fputs("\n", output_file);

/* If it's a COM file, mark the end of the file so that it will be unpacked  */
/* properly in the VMS environment.                                          */

      if (com_file)
         fprintf(output_file, "%s\n", EOFSTRING);

/* If this was the last file, add a RETURN.  Otherwise, add a line to mark   */
/* the break between one file and the next.                                  */

      if (i == num_files-1) {
         fputs("$ Return\n", output_file);
         SECTION_SEP;
      }
      else {

/* If the next file is of the same type as the current one, mark the break   */
/* between them.  Otherwise, we're beginning a whole new section and we'll   */
/* be adding a section separator line the next time through the loop.        */

         if (files[i].filetype == files[i+1].filetype)
            FILE_SEP;
      }
      fclose(tmpfile);
   }
   return SUCCESS;
}


/* This function strips the file extension off a file name and returns the   */
/* file name minus the extension in the "outfile" parameter.  The original   */
/* filename is untouched.  If there is no file extension, this routine just  */
/* performs a string copy from the input file name to the output file name.  */

remove_file_extension(infile, outfile)
char *infile;
char *outfile;
{
   char *ptr;

   strcpy(outfile, infile);
   if ((ptr = strchr(outfile, '.')) != NULL)
      *ptr = '\0';
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


/* This function creates the .REPACK file that can be used to repack the     */
/* file into another .COM file at a later time.  When you call the .COM file */
/* with the UNPACK option, this file will be created, allowing you to easily */
/* repack the .COM file when you are finished with any modifications.        */

create_repack(output_file, filename, num_files, files, ported, mixed_case)
FILE *output_file;
char *filename;
int  num_files;
struct pack files[];
int  ported;
int  mixed_case;
{
   int  i, length, type = -1;
   char tempfile[90];

   remove_file_extension(filename, tempfile);
   fprintf(output_file, "%s\n", sections[REPACK_LABEL][1]);
   fprintf(output_file, "$ create %s.repack\n", tempfile);
   fprintf(output_file, "$ DECK/DOLLARS=\"%s\"\n", EOFSTRING);
   fprintf(output_file, "$ vpack %s", filename);
   if (!ported)
      fputs(" -u", output_file);
   if (mixed_case)
      fputs(" -mixed", output_file);

   for (i = 0; i < num_files; i++) {
      if (files[i].filetype != type) {
         type = files[i].filetype;
         fprintf(output_file, " -\n\t%s", sections[type][0]);
         length = 8;
      }
      length += strlen(files[i].filename) + 1;
      if (length > LINE_LENGTH) {
         fputs(" -\n\t  ", output_file);
         length = 10 + strlen(files[i].filename) + 1;
      }
      fprintf(output_file, " %s", files[i].filename);
   }

   fputs("\n$ Exit\n", output_file);
   fprintf(output_file, "%s\n", EOFSTRING);
   fputs("$ Return\n", output_file);
   SECTION_SEP;
   return SUCCESS;
}


/* This function determines the current time/date and returns it in a string */
/* formatted as follows:  dayofweek, month, dd, yyyy, hh:mm:ss.              */
current_date(datestr)
char *datestr;
{
   static char *day[] = {"Sunday", "Monday", "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday", "Sunday"};
   static char *mon[] = {"January", "February", "March", "April", "May",
                         "June", "July", "August", "September", "October",
                         "November", "December"};
   time_t temptime;
   struct tm *temptm;

   temptime = time(NULL);
   temptm = localtime(&temptime);
   sprintf(datestr, "%s, %s %02d, %4d, %02d:%02d:%02d\0",
		day[temptm->tm_wday],
		mon[temptm->tm_mon],
		temptm->tm_mday,
		temptm->tm_year+1900,
		temptm->tm_hour,
		temptm->tm_min,
		temptm->tm_sec);
}


/* This routine creates the VMS-compatible file header for the .COM file.    */
/* The header includes all the script commands necessary to make the .COM    */
/* file self-extracting/compiling/linking in the VMS environment.            */

int write_header(output_file, output_filename, num_files, files, action, ported, mixed_case)
FILE *output_file;
char *output_filename;
int  num_files;
struct pack files[];
int  action, ported;
int mixed_case;
{
   char tempfile[80], *ptr, datestr[50];
   int  length, i, first;

   remove_file_extension(output_filename, tempfile);
   if (!mixed_case)
      lower_case(tempfile);
   current_date(datestr);

   fputs("$!****************************************************************************\n", output_file);
   COMMENT;
   fprintf(output_file, "$! Build proc for MIPL module %s\n", tempfile);
   fprintf(output_file, "$! VPACK Version %s, %s\n", version, datestr);
   COMMENT;
   fprintf(output_file, "$! Execute by entering:		$ @%s\n", tempfile);
   COMMENT;
   fputs("$! The primary option controls how much is to be built.  It must be in\n", output_file);
   fputs("$! the first parameter.  Only the capitalized letters below are necessary.\n", output_file);
   COMMENT;
   fputs("$! Primary options are:\n", output_file);

/* If we don't have source code and an imake file, then we won't have        */
/* anything to compile, nor any way to compile it.                           */

   if (BIT_TEST(action, SOURCE_BIT) &&
      (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT)))
      fputs("$!   COMPile     Compile the program modules\n", output_file);

/* These options will always be left in, regardless of whether they actually */
/* do anything or not, because of a stupid design decision made by Bob Deen. */

   fputs("$!   ALL         Build a private version, and unpack the PDF and DOC files.\n", output_file);
   fputs("$!   STD         Build a private version, and unpack the PDF file(s).\n", output_file);
   fputs("$!   SYStem      Build the system version with the CLEAN option, and\n", output_file);
   fputs("$!               unpack the PDF and DOC files.\n", output_file);
   fputs("$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options\n", output_file);
   fputs("$!   UNPACK      All files are created.\n", output_file);

   if (BIT_TEST(action, REPACK_BIT))
      fputs("$!   REPACK      Only the repack file is created.\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT)) {
      fputs("$!   SOURCE      Only the source files are created.\n", output_file);
      fputs("$!   SORC        Only the source files are created.\n", output_file);
      fputs("$!               (This parameter is left in for backward compatibility).\n", output_file);
   }
   if (BIT_TEST(action, PDF_BIT))
      fputs("$!   PDF         Only the PDF file is created.\n", output_file);
   if (BIT_TEST(action, TEST_BIT))
      fputs("$!   TEST        Only the test files are created.\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.\n", output_file);
   if (BIT_TEST(action, BUILD_BIT))
      fputs("$!   BUILD       Only the VMS build file is created.\n", output_file);
   if (BIT_TEST(action, MAKE_BIT))
      fputs("$!   MAKE        Only the UNIX make file is created.\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$!   DOC         Only the documentation files are created.\n", output_file);
   if (BIT_TEST(action, OTHER_BIT))
      fputs("$!   OTHER       Only the \"other\" files are created.\n", output_file);

   COMMENT;
   if (ported)
      fputs("$!   The default is to use the STD parameter if none is provided.\n", output_file);
   else
      fputs("$!   The default is to use the SYS parameter if none is provided.\n", output_file);
   COMMENT;
   fputs("$!****************************************************************************\n", output_file);
   COMMENT;
   fputs("$! The secondary options modify how the primary option is performed.\n", output_file);
   fputs("$! Note that secondary options apply to particular primary options,\n", output_file);
   fputs("$! listed below.  If more than one secondary is desired, separate them by\n", output_file);
   fputs("$! commas so the entire list is in a single parameter.\n", output_file);
   COMMENT;
   fputs("$! Secondary options are:\n", output_file);

   if (BIT_TEST(action, SOURCE_BIT) &&
      (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT))) {
      fputs("$! COMPile,ALL:\n", output_file);
      fputs("$!   DEBug      Compile for debug               (/debug/noopt)\n", output_file);
      fputs("$!   PROfile    Compile for PCA                 (/debug)\n", output_file);
      fputs("$!   LISt       Generate a list file            (/list)\n", output_file);
      fputs("$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)\n", output_file);
   }

   fputs("$! CLEAN:\n", output_file);
   fputs("$!   OBJ        Delete object and list files, and purge executable (default)\n", output_file);
   fputs("$!   SRC        Delete source and make files\n", output_file);

   COMMENT;
   fputs("$!****************************************************************************\n", output_file);
   COMMENT;

   fprintf(output_file, "$ write sys$output \"*** module %s ***\"\n", tempfile);
   COMMENT;
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$ Create_Source = \"\"\n", output_file);
   if (BIT_TEST(action, REPACK_BIT))
      fputs("$ Create_Repack =\"\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$ Create_PDF = \"\"\n", output_file);
   if (BIT_TEST(action, TEST_BIT))
      fputs("$ Create_Test = \"\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$ Create_Imake = \"\"\n", output_file);
   if (BIT_TEST(action, BUILD_BIT))
      fputs("$ Create_Build = \"\"\n", output_file);
   if (BIT_TEST(action, MAKE_BIT))
      fputs("$ Create_Make = \"\"\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$ Create_Doc = \"\"\n", output_file);
   if (BIT_TEST(action, OTHER_BIT))
      fputs("$ Create_Other = \"\"\n", output_file);
   if (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT))
      fputs("$ Do_Make = \"\"\n", output_file);
   COMMENT;
   fputs("$! Parse the primary option, which must be in p1.\n", output_file);
   fputs("$ primary = f$edit(p1,\"UPCASE,TRIM\")\n", output_file);
   if (ported)
      fputs("$ if (primary.eqs.\"\") then primary = \" \"\n", output_file);
   else
      fputs("$ if (primary.eqs.\"\") then primary = \"SYS\"\n", output_file);
   fputs("$ secondary = f$edit(p2,\"UPCASE,TRIM\")\n", output_file);
   COMMENT;
   fputs("$ if primary .eqs. \"UNPACK\" then gosub Set_Unpack_Options\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT) &&
      (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT)))
      fputs("$ if (f$locate(\"COMP\", primary) .eqs. 0) then gosub Set_Exe_Options\n", output_file);

   fputs("$ if (f$locate(\"ALL\", primary) .eqs. 0) then gosub Set_All_Options\n", output_file);
   fputs("$ if (f$locate(\"STD\", primary) .eqs. 0) then gosub Set_Default_Options\n", output_file);
   fputs("$ if (f$locate(\"SYS\", primary) .eqs. 0) then gosub Set_Sys_Options\n", output_file);
   fputs("$ if primary .eqs. \" \" then gosub Set_Default_Options\n", output_file);

   if (BIT_TEST(action, REPACK_BIT))
      fputs("$ if primary .eqs. \"REPACK\" then Create_Repack = \"Y\"\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$ if primary .eqs. \"SORC\" .or. primary .eqs. \"SOURCE\" then Create_Source = \"Y\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$ if primary .eqs. \"PDF\" then Create_PDF = \"Y\"\n", output_file);
   if (BIT_TEST(action, TEST_BIT))
      fputs("$ if primary .eqs. \"TEST\" then Create_Test = \"Y\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$ if primary .eqs. \"IMAKE\" then Create_Imake = \"Y\"\n", output_file);
   if (BIT_TEST(action, BUILD_BIT))
      fputs("$ if primary .eqs. \"BUILD\" then Create_Build = \"Y\"\n", output_file);
   if (BIT_TEST(action, MAKE_BIT))
      fputs("$ if primary .eqs. \"MAKE\" then Create_Make = \"Y\"\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$ if primary .eqs. \"DOC\" then Create_Doc = \"Y\"\n", output_file);
   if (BIT_TEST(action, OTHER_BIT))
      fputs("$ if primary .eqs. \"OTHER\" then Create_Other = \"Y\"\n", output_file);
   if (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT))
      fputs("$ if (f$locate(\"CLEAN\", primary) .eqs. 0) then Do_Make = \"Y\"\n", output_file);
   COMMENT;

   fputs("$ if (", output_file);
   length = 6;
   first = TRUE;
   if (BIT_TEST(action, SOURCE_BIT)) {
      if (first) {
         length += 13;
         fputs("Create_Source", output_file);
         first = FALSE;
      }
      else {
         length += 19;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Source", output_file);
            length = 21;
         }
         else {
            fputs(" .or. Create_Source", output_file);
         }
      }
   }

   if (BIT_TEST(action, REPACK_BIT)) {
      if (first) {
         length += 13;
         fputs("Create_Repack", output_file);
         first = FALSE;
      }
      else {
         length += 19;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Repack", output_file);
            length = 21;
         }
         else {
            fputs(" .or. Create_Repack", output_file);
         }
      }
   }

   if (BIT_TEST(action, PDF_BIT)) {
      if (first) {
         length += 10;
         fputs("Create_PDF", output_file);
         first = FALSE;
      }
      else {
         length += 16;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_PDF", output_file);
            length = 18;
         }
         else {
            fputs(" .or. Create_PDF", output_file);
         }
      }
   }

   if (BIT_TEST(action, TEST_BIT)) {
      if (first) {
         length += 11;
         fputs("Create_Test", output_file);
         first = FALSE;
      }
      else {
         length += 17;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Test", output_file);
            length = 19;
         }
         else {
            fputs(" .or. Create_Test", output_file);
         }
      }
   }

   if (BIT_TEST(action, IMAKE_BIT)) {
      if (first) {
         length += 12;
         fputs("Create_Imake", output_file);
         first = FALSE;
      }
      else {
         length += 18;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Imake", output_file);
            length = 20;
         }
         else {
            fputs(" .or. Create_Imake", output_file);
         }
      }
   }

   if (BIT_TEST(action, BUILD_BIT)) {
      if (first) {
         length += 12;
         fputs("Create_Build", output_file);
         first = FALSE;
      }
      else {
         length += 18;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Build", output_file);
            length = 20;
         }
         else {
            fputs(" .or. Create_Build", output_file);
         }
      }
   }

   if (BIT_TEST(action, MAKE_BIT)) {
      if (first) {
         length += 11;
         fputs("Create_Make", output_file);
         first = FALSE;
      }
      else {
         length += 17;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Make", output_file);
            length = 19;
         }
         else {
            fputs(" .or. Create_Make", output_file);
         }
      }
   }

   if (BIT_TEST(action, DOC_BIT)) {
      if (first) {
         length += 10;
         fputs("Create_Doc", output_file);
         first = FALSE;
      }
      else {
         length += 16;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Doc", output_file);
            length = 18;
         }
         else {
            fputs(" .or. Create_Doc", output_file);
         }
      }
   }

   if (BIT_TEST(action, OTHER_BIT)) {
      if (first) {
         length += 12;
         fputs("Create_Other", output_file);
         first = FALSE;
      }
      else {
         length += 18;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Create_Other", output_file);
            length = 20;
         }
         else {
            fputs(" .or. Create_Other", output_file);
         }
      }
   }

   if (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT)) {
      if (first) {
         length += 7;
         fputs("Do_Make", output_file);
         first = FALSE;
      }
      else {
         length += 13;
         if (length >= 80) {
            fputs(" .or -\n", output_file);
            fputs("        Do_Make", output_file);
            length = 15;
         }
         else {
            fputs(" .or. Do_Make", output_file);
         }
      }
   }
   
   fputs(") -\n", output_file);
   fputs("        then goto Parameter_Okay\n", output_file);
   fprintf(output_file, "$ write sys$output \"Invalid argument given to %s.com file -- \", primary\n", tempfile);
   fputs("$ write sys$output \"For a list of valid arguments, please see the header of\"\n", output_file);
   fputs("$ write sys$output \"of this .com file.\"\n", output_file);
   fputs("$ exit\n", output_file);
   COMMENT;
   fputs("$Parameter_Okay:\n", output_file);
   if (BIT_TEST(action, REPACK_BIT))
      fputs("$ if Create_Repack then gosub Repack_File\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$ if Create_Source then gosub Source_File\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$ if Create_PDF then gosub PDF_File\n", output_file);
   if (BIT_TEST(action, TEST_BIT))
      fputs("$ if Create_Test then gosub Test_File\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$ if Create_Imake then gosub Imake_File\n", output_file);
   if (BIT_TEST(action, BUILD_BIT))
      fputs("$ if Create_Build then gosub Build_File\n", output_file);
   if (BIT_TEST(action, MAKE_BIT))
      fputs("$ if Create_Make then gosub Make_File\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$ if Create_Doc then gosub Doc_File\n", output_file);
   if (BIT_TEST(action, OTHER_BIT))
      fputs("$ if Create_Other then gosub Other_File\n", output_file);
   if (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT))
      fputs("$ if Do_Make then gosub Run_Make_File\n", output_file);
   fputs("$ exit\n", output_file);
   COMMENT;
   fputs("$ Set_Unpack_Options:\n", output_file);
   if (BIT_TEST(action, REPACK_BIT))
      fputs("$   Create_Repack = \"Y\"\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$   Create_Source = \"Y\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$   Create_PDF = \"Y\"\n", output_file);
   if (BIT_TEST(action, TEST_BIT))
      fputs("$   Create_Test = \"Y\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$   Create_Imake = \"Y\"\n", output_file);
   if (BIT_TEST(action, BUILD_BIT))
      fputs("$   Create_Build = \"Y\"\n", output_file);
   if (BIT_TEST(action, MAKE_BIT))
      fputs("$   Create_Make = \"Y\"\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$   Create_Doc = \"Y\"\n", output_file);
   if (BIT_TEST(action, OTHER_BIT))
      fputs("$   Create_Other = \"Y\"\n", output_file);
   fputs("$ Return\n", output_file);
   COMMENT;

   if (BIT_TEST(action, SOURCE_BIT) &&
      (BIT_TEST(action, BUILD_BIT) || BIT_TEST(action, IMAKE_BIT))) {
      fputs("$ Set_EXE_Options:\n", output_file);
      fputs("$   Create_Source = \"Y\"\n", output_file);
      if (BIT_TEST(action, IMAKE_BIT))
         fputs("$   Create_Imake = \"Y\"\n", output_file);
      else
         fputs("$   Create_Build = \"Y\"\n", output_file);
      fputs("$   Do_Make = \"Y\"\n", output_file);
      fputs("$ Return\n", output_file);
      COMMENT;
   }

   fputs("$ Set_Default_Options:\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$   Create_Source = \"Y\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$   Create_Imake = \"Y\"\n", output_file);
   else
      fputs("$   Create_Build = \"Y\"\n", output_file);
   fputs("$   Do_Make = \"Y\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$   Create_PDF = \"Y\"\n", output_file);
   fputs("$ Return\n", output_file);
   COMMENT;

   fputs("$ Set_All_Options:\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$   Create_Source = \"Y\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$   Create_Imake = \"Y\"\n", output_file);
   else
      fputs("$   Create_Build = \"Y\"\n", output_file);
   fputs("$   Do_Make = \"Y\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$   Create_PDF = \"Y\"\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$   Create_Doc = \"Y\"\n", output_file);
   fputs("$ Return\n", output_file);
   COMMENT;

   fputs("$ Set_Sys_Options:\n", output_file);
   if (BIT_TEST(action, SOURCE_BIT))
      fputs("$   Create_Source = \"Y\"\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT))
      fputs("$   Create_Imake = \"Y\"\n", output_file);
   else
      fputs("$   Create_Build = \"Y\"\n", output_file);
   if (BIT_TEST(action, PDF_BIT))
      fputs("$   Create_PDF = \"Y\"\n", output_file);
   if (BIT_TEST(action, DOC_BIT))
      fputs("$   Create_Doc = \"Y\"\n", output_file);
   fputs("$   Do_Make = \"Y\"\n", output_file);
   fputs("$ Return\n", output_file);
   COMMENT;

   fputs("$Run_Make_File:\n", output_file);
   if (BIT_TEST(action, IMAKE_BIT)) {
      fprintf(output_file, "$   if F$SEARCH(\"%s.imake\") .nes. \"\"\n", tempfile);
      fputs("$   then\n", output_file);
      fprintf(output_file, "$      vimake %s\n", tempfile);
      fprintf(output_file, "$      purge %s.bld\n", tempfile);
      fputs("$   else\n", output_file);
      fprintf(output_file, "$      if F$SEARCH(\"%s.bld\") .eqs. \"\"\n", tempfile);
      fputs("$      then\n", output_file);
      fputs("$         gosub Imake_File\n", output_file);
      fprintf(output_file, "$         vimake %s\n", tempfile);
      fputs("$      else\n", output_file);
      fputs("$      endif\n", output_file);
      fputs("$   endif\n", output_file);
   }
   else
      fprintf(output_file, "$   if F$SEARCH(\"%s.bld\") .eqs. \"\" then gosub Build_File\n", tempfile);
   fputs("$   if (primary .eqs. \" \")\n", output_file);
   fputs("$   then\n", output_file);
   fprintf(output_file, "$      @%s.bld \"STD\"\n", tempfile);
   fputs("$   else\n", output_file);
   fprintf(output_file, "$      @%s.bld \"''primary'\" \"''secondary'\"\n", tempfile);
   fputs("$   endif\n", output_file);
   fputs("$ Return\n", output_file);

   SECTION_SEP;
   return SUCCESS;
}


/* This routine prints an error message to the operator telling him the proper*/
/* syntax to follow when using this program.                                  */
print_syntax_message()
{
   printf("\nImproper syntax.  You must give the vpack program at least one parameter.\n");
   printf("You may call the program in one of two ways:\n");
   printf("\n");
   printf("     vpack file.com [-u] [-s source file(s)] [-i IMAKE template file(s)]\n");
   printf("           [-b VMS build file(s)] [-m UNIX make file(s)]\n");
   printf("           [-p PDF file(s)] [-t Test file(s)] [-d Doc. file(s)]\n");
   printf("           [-o \"Other\" file(s)]\n");
   printf("\n");
   printf("You must use at least one of these options when you call vpack from the\n");
   printf("command line.  You may use any or all of the options, in any order you like,\n");
   printf("as long as each \"-x\" parameter is followed by a list of one or more files.\n");
   printf("Except for \"-u\", which tells vpack whether these files are unported or not.\n");
   printf("\n");
   printf("The second method of calling this program is to use a file instead of the\n");
   printf("command line.  In that case, you need only include the name of the file\n");
   printf("that vpack is to use.  The information present in the file should be\n");
   printf("the same as that used from the command line.\n");
   printf("\n");
   printf("     vpack file.repack\n\n");
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
/* returns a pointer to the location of the string if it is found, NULL      */
/* otherwise.  This is similar to the strstr() function, which is not sup-   */
/* ported on some C compilers.                                               */

char *substring(str, substr)
char *str, *substr;
{
   char *ptr;

/* If the substring is longer than the string we're checking, then it can't  */
/* be contained in it, so we return FALSE.                                   */

   if ((int)strlen(substr) > (int)strlen(str))
      return NULL;

/* Similarly, if the length of either string is zero, we can also return     */
/* FALSE.                                                                    */

   if (strlen(substr) == 0 || strlen(str) == 0)
      return NULL;

/* Otherwise, set the pointer to beginning of the string.                    */

   ptr = str;

/* When the character pointed to by the pointer matches the first character  */
/* in the substring, we do a string compare to see if we have a complete     */
/* match.  If not, we continue checking.  If so, we're done.                 */

   while(ptr = strchr(ptr, *substr)) {
      if (!strncmp(ptr, substr, strlen(substr)))
         return ptr;
      else
         ptr++;
   }
   return NULL;
}
