$!****************************************************************************
$!
$! Build proc for MIPL module comment
$! VPACK Version 1.9, Friday, November 13, 1998, 13:34:20
$!
$! Execute by entering:		$ @comment
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module comment ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to comment.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("comment.imake") .nes. ""
$   then
$      vimake comment
$      purge comment.bld
$   else
$      if F$SEARCH("comment.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake comment
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @comment.bld "STD"
$   else
$      @comment.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create comment.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack comment.com -
	-s comment.c -
	-i comment.imake -
	-p comment.pdf -
	-t tstcomment.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create comment.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"

#include  <stdio.h>
#include <string.h>

/**********************************************************************/
/* PROGRAM: comment                                                   */
/**********************************************************************/
/*                                                                    */
/*   Program "comment " is a VICAR applications program which types   */
/*   current local date and either types the input comment into the   */
/*   log for the VICAR session or prompts the operator to enter a     */
/*   comment to be included in the session log.                       */
/**********************************************************************/
/*                                                                    */
/* HISTORY:                                                           */
/*                                                                    */
/*   Converted to 'C' and ported to UNIX  by CRI          Jan   95    */
/*   Prepared in Fortran-77 for MIPL by Steve Pohorsky    Sept  84    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*                                                                    */
/*   Input args : message                                             */
/*                                                                    */
/*   Output args: None                                                */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* MAIN PROGRAM                                                       */
/*                                                                    */
/**********************************************************************/


/*comment(message)
char message[80]; */
void main44()
{
    long  time(), t;
    int	  i,status,num_values,defaults_used;
    char  *date_time, *ctime();
    char  aday[3], amonth[4], ayear[5], adate[8];
    char  line[80];
    char  bar[25];
    char  message[80];
    char  c;

    zifmessage("COMMENT version 13-NOV-98");
    status = zvparm ("MESSAGE",message,&num_values,&defaults_used,1,80);
                                        /* Get the message to display   */
    strcpy(bar,"--------------------");
    zvmessage(bar,"");
    strcpy(bar,"\n--------------------");
    /*               Get current time                                 */

    t = time(0);

    /*               Convert to Local in ASCII String                 */

    date_time = ctime(&t); 

    /*               Gather date and time pieces                      */

    strncpy(ayear, (date_time + 20), 4);
    strncpy(amonth, (date_time + 4), 3);
    strncpy(aday, (date_time + 8), 2);
    strncpy(adate, (date_time +11), 8);

    sprintf(line, "\nDate: %.2s-%.3s-%.4s    Time: %.8s\0\n",
            aday,amonth,ayear,adate);

    zvmessage(line, "");
    if (defaults_used == 1)
      {
        printf("comment: ");           /*    Request a one line comment  */
        gets(message);   
       }
     else
       {
         if (message[0] == ' ')
            for ( i = 0; i < 80 ; i++)
               message[i]= message[i+1];       
         if (message[0] !='\0')
               zvmessage(message,""); 
       }
     zvmessage(bar,"");
} 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create comment.imake
#define PROGRAM  comment

#define MODULE_LIST comment.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create comment.pdf
process help=*
    PARM MESSAGE TYPE=(STRING,79) DEFAULT=""

!# annot function="VICAR Procedure Generation"
!# annot keywords=(executes,"COLOR test file",LABELC)
end-proc
.TITLE
Displays comments during execution of a procedure
.HELP
PURPOSE
The COMMENT program is used to have a PDF file display a comment as it
executes. For example, to record the actions of a test file, you might
execute the following commands:
    VICAR>enable-log
    VICAR>comment "Execution of COLOR test file before modification+
    VICAR>+to remove call to LABELC"
    VICAR>tstcolor
    VICAR>disable-log

These commands would produce the following session file:
    comment "Execution of COLOR test file before modification +
    to remove call to LABELC"
    Beginning VICAR task COMMENT             
    --------------------
 
    Date: 29-JUN-87    Time: 16:04:16
    Execution of COLOR test file before modification to remove call to LABELC
 
    --------------------
    tstcolor
    GEN COLORA NL=3 NS=10 IVAL=0 LINC=128 SINC=128
    Beginning VICAR task GEN                 
    GEN VERSION 5
    GEN TASK COMPLETED
    LIST COLORA
	:    :    :
    disable-log

COMMENT can also be used to simply date a session. If you enter
    VICAR>enable-log
    VICAR>comment ""
    VICAR>disable-log

the session file will contain
    comment ""
    Beginning VICAR task COMMENT             
    --------------------
 
    Date: 29-JUN-87    Time: 16:19:20
 
    --------------------
    disable-log


If no argument is supplied to COMMENT, you are interactively requested
for a comment. For example, if you place a call to COMMENT in a PDF file,
then every time the PDF file is executed, you are requested to enter a
comment; this comment can then be related to the particular session. For
example, the command sequence
    VICAR>enable-log
    VICAR>comment

will yield the following display and prompt:
    Beginning VICAR task COMMENT             
    --------------------
 
    Date: 29-JUN-87    Time: 16:19:45
    Comment:

If you answered by typing
    A test with the shorter version of the Oakland data set

the display would show
    Comment: A test with the shorter version of the Oakland data set
    A test with the shorter version of the Oakland data set

    --------------------

The session file would contain:
    comment
    Beginning VICAR task COMMENT             
    --------------------
 
    Date: 29-JUN-87    Time: 16:19:45
    A test with the shorter version of the Oakland data set

    --------------------

If you answered by simply entering the RETURN key, then the session file
would appear the same as if you had specified the command
    comment ""


RESTRICTIONS
The comment text is limited to 79 characters.

HISTORY
02-JAN-95 CRS (CRI) Made portable for UNIX
13-NOV-98 RRP       Changed ayear to be 4 digit.
.LEVEL1
.VARIABLE MSG
Message to display
.LEVEL2
.VARIABLE MSG
This parameter contains the message to be displayed as the comment. If
not specified, or if the first character of the specified message is
blank, then the message line is omitted completely.

The message is limited to 79 characters.
.END
procedure=commentdate
refgbl $echo
body
let $echo="no"
write "--------------------"
datetime
write " "
getcomment &MESSAGE
end-proc
$ Return
$!#############################################################################
$Test_File:
$ create tstcomment.pdf
procedure
refgbl $echo
refgbl $autousage
body 
let _onfail="continue"
let $echo="yes"
!
! First, the two batch comment methods:
comment ""		! without comment message
comment message="Message"	! with comment message
!
! Request a comment
comment "***** Answer Comment: prompt with <RETURN> *****" 
comment
comment "***** Answer next Comment: prompt with some text + <RETURN> *****"
comment
!
end-proc
$ Return
$!#############################################################################
