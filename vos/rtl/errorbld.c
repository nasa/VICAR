/* This program is a C rewrite of the VMS script ERRORBLD.COM the original comments
   for the script are preceded by a "$ !" and have not been updated to reflect
   this C version which is intended to produce identical results. Pardon the goto's
   this is a literal translation of the script. 
*/

/* $ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/* $ !     ERRORBLD.COM -- Build all the vicar files containing error            */
/* $ !                     information.  Files built are:                        */
/* $ !                                                                           */
/* $ !             VIC2FAC.MSG -- Help for error messages (must be converted     */
/* $ !                            to VIC2FAC.INX by MSGBLD)                      */
/* $ !             SYS_MSG.C   -- Vicar2 internal routine to issue error messages*/
/* $ !             VIC2ERRORS.TEX -- Section in VICAR user's guide describing    */
/* $ !                               possible VICAR2 errors                      */
/* $ !             ERRDEFS.H    -- C include file defining symbolic names for    */
/* $ !                            errors                                         */
/* $ !             ERRDEFS.FIN  -- FORTRAN version of ERRDEFS.H                  */
/* $ !                                                                           */
/* $ !     ******** This command procedure MUST be executed before the      ******/
/* $ !     ******** shareable image may be linked or the user guide built!! ******/
/* $ !                                                                           */
/* $ !     Errors are defined in ERRORS.DAT, and must contain each of the        */
/* $ !     following fields:                                                     */
/* $ !                                                                           */
/* $ !             KEY    -- Key associated with error, used to find error in    */
/* $ !                       VIC2FAC help files                                  */
/* $ !             NUMBER -- Integer number associated with error.               */
/* $ !             NAME   -- Symbolic name for error used inside vicar2          */
/* $ !             MESSAGE -- Error message text given upon occurrence of error  */
/* $ !             HELP   -- Help text describing the meaning and user action    */
/* $ !                       needed for the error.                               */
/* $ !                                                                           */
/* $ !     In addition, the following directive is recognized:                   */
/* $ !                                                                           */
/* $ !             SKIP  -- Skip to the next error number.  Used to create       */
/* $ !                      an empty slot in the message tables.  When deleting  */
/* $ !                      an error from ERRORS.DAT, it should be replaced with */
/* $ !                      the SKIP directive, so that the error numbers of the */
/* $ !                      errors following it do not change.  In addition,     */
/* $ !                      when adding a new error to the tables, a SKIP may    */
/* $ !                      be replaced with the error message data.             */
/* $ !                                                                           */
/* $ !     Syntax rules for ERRORS.DAT are as follows.  All records containing   */
/* $ !     keywords will start with the string "%%", followed immediately by     */
/* $ !     the keyword, one space, and then the data associated with the keyword.*/
/* $ !     The one exception to this rule is HELP, which shall start a block     */
/* $ !     of text.  The text shall be determined to be over by either reaching  */
/* $ !     the end of file or by encountering the double percent sign at the     */
/* $ !     beginning of a record.                                                */
/* $ !                                                                           */
/* $ !     A modest attempt is made to ensure that the ERRORS.DAT file is        */
/* $ !     in the proper format.                                                 */
/* $ !                                                                           */
/* $ !     Example:  (All lines must have the "%%" in the first column)          */
/* $ !                                                                           */
/* $ !             %%KEY UNDEFOPT                                                */
/* $ !             %%NUMBER -2                                                   */
/* $ !             %%NAME UNDEFINED_OPTIONAL                                     */
/* $ !             %%MESSAGE Undefined optional argument; program error          */
/* $ !             %%HELP                                                        */
/* $ !             Explanation:                                                  */
/* $ !                             explanation                                   */
/* $ !                                                                           */
/* $ !             User action:                                                  */
/* $ !                             action to take . . .                          */
/* $ !             %%KEY . . .                                                   */
/* $ !                                                                           */
/* $ !     and so on for each message.  All errors MUST be defined in            */
/* $ !     ERRORS.DAT.  Any new error information that is added must be          */
/* $ !     added only to ERRORS.DAT.  Any new modules making use of specific     */
/* $ !     error information must be added to this command procedure to be       */
/* $ !     built properly.                                                       */
/* $ !                                                                           */
/* $ !     D. Stanfill, April 1986                                               */
/* $ !                                                                           */
/* $ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/* $ !                                                                           */
/* $ !     Create the beginning of the source for each module                    */
/* $ !                                                                           */
/*                                                                               */
/*                                                                               */





#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main()
 {
  FILE *errors_dat;
  FILE *errordefs_h;
  FILE *errdefs_fin;
  FILE *sys_msg_c;
  FILE *vic2fac_msg;
  FILE *vic2errors_tex;
  FILE *bottom_tex;
  FILE *tmp_tex;

  char *expected;
  int cur_num;
  int len;
  char number_str[25];
  int number;
  char in_record[2001];
  char key[2001];
  char name[2001];
  char fname[2001];
  char message[2001];




  errordefs_h = fopen( "errdefs.h", "w" );
  errdefs_fin = fopen( "errdefs.fin", "w" );
  sys_msg_c = fopen( "sys_msg.c", "w" );
  vic2fac_msg = fopen( "vic2fac.msg", "w" );
  vic2errors_tex = fopen( "vic2errors.tex", "w" );
  bottom_tex = fopen( "bottom.tex", "w" );

  fputs( "#ifndef ERRDEFS_H\n", errordefs_h );
  fputs( "#define ERRDEFS_H\n", errordefs_h );
  fputs( "\n", errordefs_h );
  fputs( "/* ERRDEFS -- Definition of all symbolic names for VICAR2 internal\n", errordefs_h );
  fputs( " * errors.  This module is used by the VICAR2 routines themselves\n", errordefs_h );
  fputs( " * as well as application programs.\n", errordefs_h );
  fputs( " *\n", errordefs_h );
  fputs( " *\tNOTE :  This file is built by ERRORBLD.COM.  It should never\n", errordefs_h );
  fputs( " *\t\tbe modified directly.  To make changes to the source,\n", errordefs_h );
  fputs( " *\t\tchange ERRORBLD.COM.\n", errordefs_h );
  fputs( " *\n", errordefs_h );
  fputs( " */\n", errordefs_h );

  fputs( "C ERRDEFS -- Definition of all symbolic names for VICAR2 internal\n", errdefs_fin );
  fputs( "C errors.  This module is used by application programs to define\n", errdefs_fin );
  fputs( "C symbolic names for the VICAR2 errors.  The values are the same\n", errdefs_fin );
  fputs( "C as those used internally by VICAR2.\n", errdefs_fin );
  fputs( "C\n", errdefs_fin );
  fputs( "C\tNOTE :  This file is built by ERRORBLD.COM.  It should never\n", errdefs_fin );
  fputs( "C\t\tbe modified directly.  To make changes to the source,\n", errdefs_fin );
  fputs( "C\t\tchange ERRORBLD.COM.\n", errdefs_fin );
  fputs( "C\n", errdefs_fin );

  fputs( "/* sys_msg -- write out a message from the vicar2 system describing\n", sys_msg_c );
  fputs( " * an error.\n", sys_msg_c );
  fputs( " *\n", sys_msg_c );
  fputs( " *\tNOTE :  This file is built by ERRORBLD.COM.  It should never\n", sys_msg_c );
  fputs( " *\t\tbe modified directly.  To make changes to the source,\n", sys_msg_c );
  fputs( " *\t\tchange ERRORBLD.COM.\n", sys_msg_c );
  fputs( " *\n", sys_msg_c );
  fputs( " */\n", sys_msg_c );
  fputs( "#include \"xvmaininc.h\"\n", sys_msg_c );
  fputs( "#include \"defines.h\"\n", sys_msg_c );
  fputs( "#include \"rtlintproto.h\"\n", sys_msg_c );
  fputs( "#include \"declares.h\"\n", sys_msg_c );
  fputs( "#include \"externs.h\"\n", sys_msg_c );
  fputs( "#include <string.h>\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "#define NO_UNIT_FLAG 0x100\n", sys_msg_c );
  fputs( "#define IO_FLAG 0x400\n", sys_msg_c );
  fputs( "#define MASK\t0x0FF\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "void v2_sys_msg(unit, code)\n", sys_msg_c );
  fputs( "int unit, code;\n", sys_msg_c );
  fputs( "{\n", sys_msg_c );
  fputs( "   static char *rname[] =\n", sys_msg_c );
  fputs( "   {\n", sys_msg_c );
  fputs( "      \"XVADD\", \"XVCLOSE\", \"XVGET\", \"XVOPEN\", \"XVREAD\", \"XVWRITE\",\n", sys_msg_c );
  fputs( "      \"XLADD\", \"XLDEL\", \"XLGET\", \"XLHINFO\", \"XLINFO\",\t\"XLNINFO\",\n", sys_msg_c );
  fputs( "      \"XVUNIT\", \"XVCOMMAND\", \"XVPARM\", \"XVIPARM\", \"XVP\",\n", sys_msg_c );
  fputs( "      \"XVPCNT\", \"XVIPCNT\", \"XVPSTAT\", \"XVIPSTAT\", \"XVPONE\", \"XVIPONE\",\n", sys_msg_c );
  fputs( "      \"XLGETLABEL\", \"XVPARMD\", \"XVIPARMD\",\n", sys_msg_c );
  fputs( "      \"XVTRANS_SET\", \"XVTRANS_IN\", \"XVTRANS_OUT\", \"XVTRANS_INU\",\n", sys_msg_c );
  fputs( "      \"XVPIXSIZEU\", \"XVPIXSIZE\", \"XVTPINFO\", \"XVTPMODE\", \"XVTPSET\", \"XVFILPOS\",\n", sys_msg_c );
  fputs( "      \"XVCMDOUT\", \"XVIP\", \"XVPOPEN\", \"XVPOUT\", \"XVHOST\", \"XVPIXSIZEB\",\n", sys_msg_c );
  fputs( "      \"XVTRANS_INB\", \"XLPINFO\"\n", sys_msg_c );
  fputs( "   };\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "   static struct\n", sys_msg_c );
  fputs( "   {\n", sys_msg_c );
  fputs( "      char *key;\n", sys_msg_c );
  fputs( "      char *text;\n", sys_msg_c );
  fputs( "   } err_messages[] =\n", sys_msg_c );
  fputs( "   {\n", sys_msg_c );



  fputs("!\n", vic2fac_msg );
  fputs("!\tMessage file for facility VIC2\n", vic2fac_msg );
  fputs("!\n", vic2fac_msg );
  fputs("!  This file contains the message id's and detailed texts for\n", vic2fac_msg );
  fputs("!  the Vicar2 internal errors.\n", vic2fac_msg );
  fputs("!\n", vic2fac_msg );
  fputs("!\n", vic2fac_msg );
  fputs("!\tNOTE :  This file is built by ERRORBLD.COM.  It should never\n", vic2fac_msg );
  fputs("!\t\tbe modified directly.  To make changes to the source,\n", vic2fac_msg );
  fputs("!\t\tchange ERRORBLD.COM.\n", vic2fac_msg );
  fputs("!\n", vic2fac_msg );
  fputs("!\n", vic2fac_msg );


  fputs("\\newpage\n", vic2errors_tex);
  fputs("\\section{Error Messages}\n", vic2errors_tex);
  fputs("\\label{errors}\n", vic2errors_tex);
  fputs("\\subsection{Error message format}\n", vic2errors_tex);
  fputs("This section describes the meaning of the VICAR error messages.\n", vic2errors_tex);
  fputs("VICAR error messages are given in the following form:\n", vic2errors_tex);
  fputs("\\begin{quote}\n", vic2errors_tex);
  fputs("[VIC2--{\\em key}] {\\em message}\n", vic2errors_tex);
  fputs("\\end{quote}\n", vic2errors_tex);
  fputs("where VIC2 indicates that the message was issued from the VICAR2\n", vic2errors_tex);
  fputs("package, and {\\em key} is the specific key or identifier for the message\n", vic2errors_tex);
  fputs("given.  The message key may be used to ask for help with the\n", vic2errors_tex);
  fputs("HELP-MESSAGE command in the VICAR supervisor or to look up a message\n", vic2errors_tex);
  fputs("in this section.  In addition, the key is stored internally by the \n", vic2errors_tex);
  fputs("supervisor, so that by simply typing a question mark (?) to the \n", vic2errors_tex);
  fputs("prompt, help on the last error message given is received.\n", vic2errors_tex);
  fputs("\\subsection{Messages by key}\n", vic2errors_tex);
  fputs("This section lists VICAR2 error messages in alphabetical order by\n", vic2errors_tex);
  fputs("key.  The accompanying message, the numerical value, and the symbolic\n", vic2errors_tex);
  fputs("name by which the error may be referenced in a program are given,\n", vic2errors_tex);
  fputs("followed by a detailed description of what the message means, and the\n", vic2errors_tex);
  fputs("action required to correct the error.\n", vic2errors_tex);
  fputs("\n", vic2errors_tex);
  fputs("\\begin{enumerate}\n", vic2errors_tex);
  fclose(vic2errors_tex);


  fputs("\\end{enumerate}\n", bottom_tex);
  fputs("\n", bottom_tex);
  fputs("\\subsection{Messages by numerical value}\n", bottom_tex);
  fputs("For easy reference, the VICAR2 error messages are listed here in\n", bottom_tex);
  fputs("their numerical order, giving the key associated with the value, and\n", bottom_tex);
  fputs("the symbolic name which may be used to reference it in a program.\n", bottom_tex);
  fputs("The detailed help for each message may be found in the previous\n", bottom_tex);
  fputs("section under the key name.\n", bottom_tex);
  fputs("\\begin{itemize}\n", bottom_tex);

/*$ !                                                                     */
/*$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       */
/*$ !                                                                     */
/*$ !     Now start reading data file ERRORS.DAT and writing              */
/*$ !     the data to their respective files.  Each file will             */
/*$ !     have an output record which shall be built and finally          */
/*$ !     written to the file, except VIC2ERRORS.TEX, in which the errors */
/*$ !     must be sorted alphabetically.  To do this, we shall            */
/*$ !     build a separate little file for each error message,            */
/*$ !     and then append them in alphabetical order to VIC2ERRORS.TEX.   */
/*$ !     The f$search() function will do this automatically for us.      */
/*$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       */
/*$ !                                                                     */

  cur_num = -2;  /* cur_num is to check the NUMBER field for consistency  */

  errors_dat = fopen( "errors.dat", "r" );

  FIND_TOP:
  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto EARLY_END;
  fgets( in_record, 2000, errors_dat );
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


  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto EARLY_END;
  expected = "%%NUMBER ";
  if ( strncmp( in_record, "%%NUMBER ", 9 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( number_str, &in_record[9] );
  sscanf(number_str, " %d", &number );
  if ( number != cur_num ) goto BAD_NUMBER;
  cur_num = cur_num - 1;

  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto EARLY_END;
  expected = "%%NAME ";
  if ( strncmp( in_record, "%%NAME ", 7 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( name, &in_record[7] );
  if ( (len = strlen(name)) != 0 )
    if( name[len - 1] == '\n') 
       name[len - 1] = '\0'; 

  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto EARLY_END;
  expected = "%%MESSAGE ";
  if ( strncmp( in_record, "%%MESSAGE ", 10 ) != 0 ) goto BAD_DIRECTIVE;
  strcpy( message, &in_record[10] );
  if ( (len = strlen(message)) != 0 )
    if( message[len - 1] == '\n')
       message[len - 1] = '\0';


  fprintf( sys_msg_c, "/*%d*/ {\"%s\",\"%s\"},\n", number, key, message );
  fprintf( errordefs_h, "#define %s %d\n", name, number );
  fprintf( errdefs_fin, "      INTEGER %s\n", name );
  fprintf( errdefs_fin, "      PARAMETER (%s = %d)\n", name, number );
  fprintf( bottom_tex, "\\item %d Key: %s \\\\\n", number, key );
  fprintf( bottom_tex, "Symbolic Name: %s\n", name );
  
/*
$ !
$ !     Now start writing detailed help to appropriate files
$ !
 */  
  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto EARLY_END;
  expected = "%%HELP";
  if ( strncmp( in_record, "%%HELP", 6 )  != 0 ) goto BAD_DIRECTIVE;
  fprintf( vic2fac_msg, ".KEY %s\t! %s\n", key, name );
  sprintf( fname, "%s.tmp_tex", key );
  tmp_tex = fopen( fname, "w" );
  fprintf( tmp_tex, "\n\\item %s Symbolic Name: %s\n\n[VIC2-%s] %s\n\n", key, name, key, message );
  

  WRT_HLP:
  strcpy( in_record, "  " );
  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto HLP_DONE;
  if ( strncmp( in_record, "%%", 2 ) == 0 ) goto HLP_DONE;
  fputs( in_record, vic2fac_msg );
  fputs( in_record, tmp_tex );
  goto WRT_HLP;

/*
$ !
$ !     All the text is written, now polish off temp file
*/
  
  HLP_DONE:
  fclose( tmp_tex );
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
  fputs( "{0,0},\n", sys_msg_c );
  if ( fgets( in_record, 2000, errors_dat ) == NULL ) goto DONE;
  goto CHECK_SKIP;

  CHECK_KEY:
  expected = "%%KEY ";
  if ( strncmp( in_record, "%%KEY ", 6 ) != 0 ) goto BAD_DIRECTIVE;
  goto TOP;


/* ! At this point, the end of ERRORS.DAT has been reached */
/*
$ !
$ !     Now we need to tack the end of each file if needed onto what
$ !     has been created so far, and then build the VIC2ERRORS.TEX file
$ !     from all the .TMP_TEX files.
$ !
*/

  DONE:
  fclose(errors_dat);
  /* append tail on sys_msg_c */

  fputs( "{0,0}\t\t/* Terminator entry */\n", sys_msg_c );
  fputs( "   };\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "   static char msgbuf[200];\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "   if ((current_call & NO_UNIT_FLAG) || (unit == NO_UNIT))\n", sys_msg_c );
  fputs( "      sprintf(msgbuf, \"Exception in %s\", rname[MASK&current_call]);\n", sys_msg_c );
  fputs( "   else\n", sys_msg_c );
  fputs( "      sprintf(msgbuf, \"Exception in %s, processing file: %s\",\n", sys_msg_c );
  fputs( "              rname[MASK&current_call], CURRENT_S_VALUE(NAME));\n", sys_msg_c );
  fputs( "   zvmessage(msgbuf, \"VIC2-GENERR\");\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "   if (code<0) {\n", sys_msg_c );
  fputs( "      if ((code >= LAST_ERROR) && (err_messages[-code-2].key != 0)) {\n", sys_msg_c );
  fputs( "\t char err_key[18];\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "\t strcpy(err_key,\"VIC2-\");\n", sys_msg_c );
  fputs( "\t strcat(err_key,err_messages[-code-2].key);\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "\t zvmessage(err_messages[-code-2].text, err_key);\n", sys_msg_c );
  fputs( "      }\n", sys_msg_c );
  fputs( "      else {\n", sys_msg_c );
  fputs( "\t sprintf(msgbuf,\n", sys_msg_c );
  fputs( "\t    \"Unrecognized error status %d;  Notify system programmer\", \n", sys_msg_c );
  fputs( "\t    code);\n", sys_msg_c );
  fputs( "\t zvmessage(msgbuf, \"VIC2-BADSTAT\");\n", sys_msg_c );
  fputs( "      }\n", sys_msg_c );
  fputs( "   }\n", sys_msg_c );
  fputs( "   else\t{\t/* if status is positive, it is a system error, so ask\t*/\n", sys_msg_c );
  fputs( "    \t\t/* the system for the message.\t\t\t\t*/\n", sys_msg_c );
  fputs( "      v2_hostmsg(code, msgbuf+1, 199);\t\t/* get the error msg */\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "      msgbuf[0] = (msgbuf[1] == '%') ? '%' : ' ';  /* protect C string from % */\n", sys_msg_c );
  fputs( "\t\t\t\t\t     /* 'cuz TAE runs through sprintf */\n", sys_msg_c );
  fputs( "      zvmessage(msgbuf, \"VIC2-HOSTMSG\");\n", sys_msg_c );
  fputs( "   }\n", sys_msg_c );
  fputs( "   if (current_call & IO_FLAG) {\t\t/* if read/write */\n", sys_msg_c );
  fputs( "      sprintf(msgbuf, \" Current line in image = %d\", CURRENT_I_VALUE(IMG_REC));\n", sys_msg_c );
  fputs( "      zvmessage(msgbuf, \" \");\n", sys_msg_c );
  fputs( "   }\n", sys_msg_c );
  fputs( "\n", sys_msg_c );
  fputs( "   return;\n", sys_msg_c );
  fputs( "}\n", sys_msg_c );

  fclose(sys_msg_c);


  cur_num = cur_num + 1;
  fprintf( errordefs_h, "#define LAST_ERROR %d\n", cur_num );
  fputs("#endif /* ERRDEFS_H */\n", errordefs_h );
  fclose( errordefs_h );

  fputs("      INTEGER LAST_ERROR\n",  errdefs_fin );
  fprintf ( errdefs_fin, "      PARAMETER (LAST_ERROR = %d)\n", cur_num );
  fclose( errdefs_fin );

  fputs( ".KEY GENERR\t! general error\n", vic2fac_msg );
  fputs( "Explanation:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "This message is informational, indicating that some error has\n", vic2fac_msg );
  fputs( "occurred.  It should always be accompanied by a detailed error message.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "User action:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "Look at the accompanying message to find out what the error is\n", vic2fac_msg );
  fputs( "and act accordingly.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( ".KEY HOSTMSG\t! A host dependent error occurred, message to follow\n", vic2fac_msg );
  fputs( "Explanation:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "This is an informational message only.  A host dependent error\n", vic2fac_msg );
  fputs( "occurred, and the message is given.  The host message (and\n", vic2fac_msg );
  fputs( "probably a stack trace if under VMS) should appear immediately\n", vic2fac_msg );
  fputs( "following this message.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "User action:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "Interpret the host error message and act accordingly.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( ".KEY BADSTAT\t! Unrecognized error status\n", vic2fac_msg );
  fputs( "Explanation:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "VICAR's internal error handler has been asked to issue a message\n", vic2fac_msg );
  fputs( "for a status it does not recognize.  This is probably a failure\n", vic2fac_msg );
  fputs( "on the part of the system programmer to keep the executive error\n", vic2fac_msg );
  fputs( "modules up to date.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "User action:\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );
  fputs( "Record the status value given and notify the system programmer\n", vic2fac_msg );
  fputs( "immediately.\n", vic2fac_msg );
  fputs( "\n", vic2fac_msg );

/*
$ !
$ !     Now everything is done except the LaTeX file, VIC2ERRORS.TEX.
$ !     Fetch the .TMP_TEX files one at a time and append them to
$ !     the VIC2ERRORS file.
$ !
*/

  system ( "cat *.tmp_tex >> vic2errors.tex" );
  fclose( bottom_tex );
  system ( "cat bottom.tex >> vic2errors.tex" );
  vic2errors_tex = fopen( "vic2errors.tex", "a" );
  fputs( "\\end{itemize}\n", vic2errors_tex );
  fputs( "\\end{document}\n", vic2errors_tex );
  fclose(vic2errors_tex);
  system ( "sed -e \"s/\\_/\\\\\\_/g\" vic2errors.tex > vic2errors.tmp" );
  system ( "rm -f vic2errors.tex; mv vic2errors.tmp vic2errors.tex" );
  system ( "rm -f bottom.tex");
  system ( "rm -f *.tmp_tex");
  puts("****************************************");
  puts("*                                      *");
  puts("* ERRORBLD.COM:                        *");
  puts("* All files successfully created       *");
  puts("*                                      *");
  puts("****************************************");
  exit(0);

  EARLY_END:
  printf( "**** Premature end of file encountered processing ERRORS.DAT\n");
  printf( "**** Check file contents\n");
  goto PLS_FIX;

  BAD_DIRECTIVE:
  printf( "**** Bad directive in ERRORS.DAT\n" );
  printf( "**** Expected %s\n", expected );
  printf( "**** Found %s\n", in_record );
  goto PLS_FIX;


  BAD_NUMBER:
  printf( "**** Numbering sequence in ERRORS.DAT is bad" );
  printf( "**** Found %d when expecting %d\n", number, cur_num );
  

  PLS_FIX:
  puts("");
  puts("Please see documentation in ERRORBLD.COM and fix");
  puts("ERROR file build aborted");

 }

 

  
  
