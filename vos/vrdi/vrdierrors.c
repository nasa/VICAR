/* This program is a C rewrite of the VMS script VRDIERRORS.COM the original comments
   for the script are preceded by a "$ !" and have not been updated to reflect
   this C version which is intended to produce identical results. Pardon the goto's
   this is a literal translation of the script. 
*/
/*
$!*************************************************************************
$!
$!      VRDIERRORS.COM -- Builds some of the VRDI files containing error
$!                    information.  Files built are:
$!
$!              VRDIFAC.MSG  -- Help for error messages (must be converted
$!                              to VRDIFAC.INX by MSGBLD)
$!              VERRDEFS.H   -- C include file defining symbolic names for
$!                              errors
$!              VERRDEFS.FIN -- FORTRAN version of VERRDEFS.H
$!
$!      Errors are defined in VRDIERRS.DAT, and must contain each of the
$!      following fields:
$!
$!              KEY    -- Key associated with error, used to find error in
$!                        VRDIFAC help files
$!              NUMBER -- Integer number associated with error.
$!              NAME   -- Symbolic name for error used inside vicar2
$!              MESSAGE -- Error message text given upon occurrence of error
$!              HELP   -- Help text describing the meaning and user action
$!                        needed for the error.
$!
$!      In addition, the following directive is recognized:
$!
$!              SKIP  -- Skip to the next error number.  Used to create
$!                       an empty slot in the message tables.  When deleting
$!                       an error from VRDIERRS.DAT, it should be replaced with
$!                       the SKIP directive, so that the error numbers of the
$!                       errors following it do not change.  In addition,
$!                       when adding a new error to the tables, a SKIP may
$!                       be replaced with the error message data.
$!
$!      Syntax rules for VRDIERRS.DAT are as follows.  All records containing
$!      keywords will start with the string "%%", followed immediately by
$!      the keyword, one space, and then the data associated with the keyword.
$!      The one exception to this rule is HELP, which shall start a block
$!      of text.  The text shall be determined to be over by either reaching
$!      the end of file or by encountering the double percent sign at the
$!      beginning of a record.
$!
$!      A modest attempt is made to ensure that the VRDIERRS.DAT file is
$!      in the proper format.
$!
$!      Example:  (All lines must have the "%%" in the first column)
$!
$!              %%KEY UNDEFOPT
$!              %%NUMBER -65537
$!              %%NAME UNDEFINED_OPTIONAL
$!              %%MESSAGE Undefined optional argument; program error
$!              %%HELP
$!              Explanation:
$!                              explanation
$!
$!              User action:
$!                              action to take . . .
$!              %%KEY . . .
$!
$!      and so on for each message.  All errors MUST be defined in
$!      VRDIERRS.DAT.
$!
$!      A mere copy (scaled down) of D. Stanfill's ERRORS.BLD, April 1986
$!
$!*************************************************************************
$!      Create the beginning of the source for each module
$!
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main()
 {
  FILE *vrdierrs_dat;
  FILE *verrordefs_h;
  FILE *verrdefs_fin;
  FILE *vrdifac_msg;

  char *expected;
  long cur_num;
  int len;
  char number_str[25];
  int number;
  char in_record[2001];
  char key[2001];
  char name[2001];
  char fname[2001];
  char message[2001];


  verrordefs_h = fopen( "verrdefs.h", "w" );
  verrdefs_fin = fopen( "verrdefs.fin", "w" );
  vrdifac_msg = fopen( "vrdifac.msg", "w" );


  fputs("/* VERRDEFS -- Definition of all symbolic names for VRDI internal\n", verrordefs_h );
  fputs(" * errors.  This module is used by application programs to define\n", verrordefs_h );
  fputs(" * symbolic names for the VRDI errors.  \n", verrordefs_h );
  fputs(" *\n", verrordefs_h );
  fputs(" *\tNOTE :  This file is built by VRDIERRORS.COM.  It should never\n", verrordefs_h );
  fputs(" *\t\tbe modified directly.  To make changes to the source,\n", verrordefs_h );
  fputs(" *\t\tchange VRDIERRORS.COM.\n", verrordefs_h );
  fputs(" *\n", verrordefs_h );
  fputs(" */\n", verrordefs_h );

  fputs("C VERRDEFS -- Definition of all symbolic names for VRDI internal\n", verrdefs_fin );
  fputs("C errors.  This module is used by application programs to define\n", verrdefs_fin );
  fputs("C symbolic names for the VRDI errors.\n", verrdefs_fin );
  fputs("C\n", verrdefs_fin );
  fputs("C\tNOTE :  This file is built by VRDIERRORS.COM.  It should never\n", verrdefs_fin );
  fputs("C\t\tbe modified directly.  To make changes to the source,\n", verrdefs_fin );
  fputs("C\t\tchange VRDIERRORS.COM.\n", verrdefs_fin );
  fputs("C\n", verrdefs_fin );

  fputs("!\n", vrdifac_msg );
  fputs("!\tMessage file for facility VIC2\n", vrdifac_msg );
  fputs("!\n", vrdifac_msg );
  fputs("!  This file contains the message id's and detailed texts for\n", vrdifac_msg );
  fputs("!  the VRDI internal errors.\n", vrdifac_msg );
  fputs("!\n", vrdifac_msg );
  fputs("!\n", vrdifac_msg );
  fputs("!\tNOTE :  This file is built by VRDIERRORS.COM.  It should never\n", vrdifac_msg );
  fputs("!\t\tbe modified directly.  To make changes to the source,\n", vrdifac_msg );
  fputs("!\t\tchange VRDIERRORS.COM.\n", vrdifac_msg );
  fputs("!\n", vrdifac_msg );
  fputs("!\n", vrdifac_msg );

/* 
$!
$!*************************************************************************
$!
$!      Now start reading data file VRDIERRS.DAT and writing
$!      the data to their respective files.  Each file will
$!      have an output record which shall be built and finally
$!      written to the file.
$!
$!*************************************************************************
$!
*/

  cur_num = -65537;  /* cur_num is to check the NUMBER field for consistency  */

  vrdierrs_dat = fopen( "vrdierrs.dat", "r" );

  FIND_TOP:
  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto EARLY_END;
  fgets( in_record, 2000, vrdierrs_dat );
  if ( strncmp( in_record, "%%SKIP", 6 ) == 0 )
   {
    cur_num = cur_num - 1;
   } 
  else if ( strncmp( in_record, "%%KEY ", 6) != 0 ) goto FIND_TOP;
  TOP:
  strcpy( key, &in_record[6] );
  if ( (len = strlen(key)) != 0 )
    if( key[len - 1] == '\n')
       key[len - 1] = '\0';


  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto EARLY_END;
  expected = "%%NUMBER ";
  if ( strncmp( in_record, "%%NUMBER ", 9 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( number_str, &in_record[9] );
  sscanf(number_str, " %d", &number );
  if ( number != cur_num ) goto BAD_NUMBER;
  cur_num = cur_num - 1;

  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto EARLY_END;
  expected = "%%NAME ";
  if ( strncmp( in_record, "%%NAME ", 7 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( name, &in_record[7] );
  if ( (len = strlen(name)) != 0 )
    if( name[len - 1] == '\n') 
       name[len - 1] = '\0'; 

  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto EARLY_END;
  expected = "%%MESSAGE ";
  if ( strncmp( in_record, "%%MESSAGE ", 10 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( message, &in_record[10] );
  if ( (len = strlen(message)) != 0 )
    if( message[len - 1] == '\n')
       message[len - 1] = '\0';


  fprintf( verrordefs_h, "#define %s %d\n", key, number );
  fprintf( verrdefs_fin, "      INTEGER %s\n", key );
  fprintf( verrdefs_fin, "      PARAMETER (%s = %d)\n", key, number );
  
/*
$ !
$ !     Now start writing detailed help to appropriate files
$ !
 */  
  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto EARLY_END;
  expected = "%%HELP";
  if ( strncmp( in_record, "%%HELP", 6 )  != 0 ) goto BAD_DIRECTIVE;
  fprintf( vrdifac_msg, ".KEY %s\t! %s\n", key, name );

  WRT_HLP:
  strcpy( in_record, "  " );
  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto HLP_DONE;
  if ( strncmp( in_record, "%%", 2 ) == 0 ) goto HLP_DONE;
  fputs( in_record, vrdifac_msg );
  goto WRT_HLP;

/*
$ !
$ !     All the text is written, now polish off temp file
*/
  
  HLP_DONE:
  if ( strncmp( in_record, "%%", 2 ) != 0 ) goto DONE;
  
/*
$ !
$ !     Get the next directive -- must be either SKIP or KEY.
$ !     If SKIPs are encountered, keep skipping until KEY is found.
$ !
*/

  CHECK_SKIP:
  if ( strncmp( in_record, "%%SKIP", 6 ) != 0 ) goto CHECK_KEY;
  cur_num = cur_num - 1;
  if ( fgets( in_record, 2000, vrdierrs_dat ) == NULL ) goto DONE;
  goto CHECK_SKIP;

  CHECK_KEY:
  expected = "%%KEY ";
  if ( strncmp( in_record, "%%KEY ", 6 ) != 0 ) goto BAD_DIRECTIVE;
  goto TOP;


/* ! At this point, the end of VRDIERRS.DAT has been reached */
/*
$ !
$ !     Now we need to tack the end of each file if needed onto what
$ !     has been created so far, and then build the VIC2ERRORS.TEX file
$ !     from all the .TMP_TEX files.
$ !
*/

  DONE:
  fclose(vrdierrs_dat);
  fclose(verrordefs_h);
  fclose(verrdefs_fin);

  puts("****************************************");
  puts("*                                      *");
  puts("* VRDIERRORS.COM:                      *");
  puts("* All files successfully created       *");
  puts("*                                      *");
  puts("****************************************");
  exit(0);

  EARLY_END:
  printf( "**** Premature end of file encountered processing VRDIERRS.DAT\n");
  printf( "**** Check file contents\n");
  goto PLS_FIX;

  BAD_DIRECTIVE:
  printf( "**** Bad directive in VRDIERRS.DAT\n" );
  printf( "**** Expected %s\n", expected );
  printf( "**** Found %s\n", in_record );
  goto PLS_FIX;


  BAD_NUMBER:
  printf( "**** Numbering sequence in VRDIERRS.DAT is bad" );
  printf( "**** Found %d when expecting %ld\n", number, cur_num );
  

  PLS_FIX:
  puts("");
  puts("Please see documentation in ERRORBLD.COM and fix");
  puts("ERROR file build aborted");

 }

 

  
  
