$!****************************************************************************
$!
$! Build proc for MIPL module usr_message
$! VPACK Version 1.8, Friday, March 21, 1997, 17:31:09
$!
$! Execute by entering:		$ @usr_message
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module usr_message ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to usr_message.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("usr_message.imake") .nes. ""
$   then
$      vimake usr_message
$      purge usr_message.bld
$   else
$      if F$SEARCH("usr_message.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake usr_message
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @usr_message.bld "STD"
$   else
$      @usr_message.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create usr_message.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack usr_message.com -
	-s usr_message.c -
	-i usr_message.imake -
	-t tusr_message.c tusr_message.imake tusr_message.pdf -
	   tstusr_message.pdf -
	-o usr_message.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create usr_message.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************

     Subroutine module USR_MESSAGE.C

These subroutines have been written to provide a set of UNIX-ported
sprintf/zvmessage utilities.  Their purpose is two-fold:

    1) To isolate all zvmessage calls so that changes to zvmessage can
    be made globally with changes to this module only; and

    2) To make it slightly easier & faster for programmers to include 
    lucid, userful, readable messages to the user in their software.
        - Most user messages can be reduced from two or more lines to one 
          (since you don't need to type out the "sprintf()" lines yourself 
          anymore.)
        - Also, formatting is simpler. The "\n" at the end of sprintf format 
          lines should not be included, since zvmessage inserts its own carriage
          returns. (See the example below.)

       ********Start of example***********************************************
       #include "vicmain_c"
       #include "message.h"
       #define VERSION "5-4-1993"
       main44() {
        int max=10,min=1,status=-3;
         message("Initializing, please wait.");
         message1("VICAR program SPAM, revision %s",VERSION);
         message2("Maximum scale is %d, minimum scale is %d.",max,min);
         errmess1("SPAM> malloc error, status=%d",status,"SPAM-W-BADMALLOC");
       }
       ********End of example***********************************************

Routines in this module:

  null_message  -  prints out a blank line
  message       -  same as zvmessage()  (no sprintf)
  message1      -  zvmessage + sprintf with 1 variable
  message2      -  zvmessage + sprintf with 2 variables
  message3      -  zvmessage + sprintf with 3 variables
  message4      -  zvmessage + sprintf with 4 variables
  
  errmess       -  zvmessage with error key (no sprintf)
  errmess1      -  zvmessage with error key + sprintf with 1 variable
  errmess2      -  zvmessage with error key + sprintf with 2 variables
  errmess3      -  zvmessage with error key + sprintf with 3 variables
  errmess4      -  zvmessage with error key + sprintf with 4 variables

Variables may be of any type; they are picked up as voids.

Text should be no longer than 249 characters. Use additional "message*"'s as
needed for longer text.
------------------------------------------------------------------------------

     Written by M. O'Shaughnessy (with thanks to Bob Deen for concept)
     Revision history:
     5/5/1993  MOS  Original
     7/12/1994 MOS  Extended message length from 80 to 249+1; converted
                    routines to ANSI C.
*****************************************************************************/
#include "xvmaininc.h"
#include <stdio.h>
#include <string.h>
#define MESSAGE_LINE_SIZE 249+1  /* Changed from 80. Max size for zvmessage */
                                 /* is 249+1 for both msg and key.          */
#define ROUTINE /* */

/* prototypes */
void null_message (void);
void message( char *msg);
void message1( char *fmt, void *v1);
void message2( char *fmt, void *v1, void *v2);
void message3( char *fmt, void *v1, void *v2, void *v3);
void message4( char *fmt, void *v1, void *v2, void *v3, void *v4);
void errmess( char *msg, char *key);
void errmess1( char *fmt, void *v1, char *key);
void errmess2( char *fmt, void *v1, void *v2, char *key);
void errmess3( char *fmt, void *v1, void *v2, void *v3, char *key);
void errmess4( char *fmt, void *v1, void *v2, void *v3, void *v4, char *key);

ROUTINE void null_message(void)  /* prints out a blank line */
{
  zvmessage(" ",0);
}

ROUTINE void message(char *msg)
{
  if (MESSAGE_LINE_SIZE < strlen(msg) ) {
    zvmessage("USR_MESSAGE> Message length exceeds 249.",0);
    return;
  }
  zvmessage(msg,0);
}

ROUTINE void message1(char *fmt, void *v1)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1);
  message(text);
}

ROUTINE void message2(char *fmt,void *v1,void *v2)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2);
  message(text);
}                           

ROUTINE void message3(char *fmt,void *v1,void *v2,void *v3)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2,v3);
  message(text);
}

ROUTINE void message4(char *fmt,void *v1,void *v2,void *v3,void *v4)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2,v3,v4);
  message(text);
}

ROUTINE void errmess(char *fmt,char *key) 
{
  if (MESSAGE_LINE_SIZE < strlen(fmt) ) {
    zvmessage("USR_MESSAGE> Message length exceeds 249.",0);
    return;
  }
  if (MESSAGE_LINE_SIZE < strlen(key) ) {
    zvmessage("USR_MESSAGE> Error key length exceeds 249.",0);
    return;
  }
  zvmessage(fmt,key);
}

ROUTINE void errmess1(char *fmt,void *v1, char *key)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1);
  errmess(text,key);
}                          

ROUTINE void errmess2(char *fmt,void *v1,void *v2,char *key)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2);
  errmess(text,key);
}                          

ROUTINE void errmess3(char *fmt,void *v1,void *v2,void *v3,char *key)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2,v3);
  errmess(text,key);
}                          

ROUTINE void errmess4(char *fmt,void *v1,void *v2,void *v3,void *v4,char *key)
{
  char text[MESSAGE_LINE_SIZE+10];
  sprintf(text,fmt,v1,v2,v3,v4);
  errmess(text,key);
}                          

/* end module usr_message.c */
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create usr_message.imake
/* Imake file for VICAR subroutines USR_MESSAGE */

#define SUBROUTINE usr_message

#define MODULE_LIST usr_message.c

#define P2_SUBLIB

#define USES_ANSI_C
/*#define LIB_LOCAL */
$ Return
$!#############################################################################
$Test_File:
$ create tusr_message.c
#include "vicmain_c"
#include <string.h>

void main44() {
   int i=1,j,k,l;
   char s[20];
   strcpy(s,"ERR-CODE-F");
   zvmessage("The next line will be blank.",0);
   null_message();
   message("The above line is blank. This is a test of message.");
   errmess("This is a test of errmess.",s);  

   message1("This is a test of message1. %d.",i);
   errmess1("This is a test of errmess1. %d.",i,s);

   j=i;i++;
   message2("This is a test of message2. %d, %d.",i,j);
   errmess2("This is a test of errmess2. %d, %d.",i,j,s);

   k=j;j++;i++;
   message3("This is a test of message3. %d, %d, %d.",i,j,k);
   errmess3("This is a test of errmess3. %d, %d, %d.",i,j,k,s);

   l=k;k++;j++;i++;
   message4("This is a test of message4. %d, %d, %d, %d.",i,j,k,l);  
   errmess4("This is a test of errmess4. %d, %d, %d, %d.",i,j,k,l,s);

   zvmessage("Now test the error handling....try to print a string that is too long.",0);
   message("A very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              long string!");
}
/* end module tusr_message.c */
$!-----------------------------------------------------------------------------
$ create tusr_message.imake
/* Imake file for Test of VICAR subroutine usr_message */

#define PROGRAM tusr_message

#define MODULE_LIST tusr_message.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
/****
#define LIB_LOCAL 
****/
$!-----------------------------------------------------------------------------
$ create tusr_message.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstusr_message.pdf
procedure help=*
body
TUSR_MESSAGE
end-proc
.title 
TSTUSR_MESSAGE.PDF
.help
The output should look like this:
----------------------------------------------------------------------------
The next line will be blank.

The above line is blank. This is a test of message.
[ERR-CODE-F] This is a test of errmess.
This is a test of message1. 1.
[ERR-CODE-F] This is a test of errmess1. 1.
This is a test of message2. 2, 1.
[ERR-CODE-F] This is a test of errmess2. 2, 1.
This is a test of message3. 3, 2, 1.
[ERR-CODE-F] This is a test of errmess3. 3, 2, 1.
This is a test of message4. 4, 3, 2, 1.
[ERR-CODE-F] This is a test of errmess4. 4, 3, 2, 1.
Now test the error handling....try to print a string that is too long.
USR_MESSAGE> Message length exceeds 249.
----------------------------------------------------------------------------
.end
$ Return
$!#############################################################################
$Other_File:
$ create usr_message.hlp
1 Usr_message

    Routines in this module:

    null_message  -  prints out a blank line
    message       -  same as zvmessage()  (no sprintf)
    message1      -  zvmessage + sprintf with 1 variable
    message2      -  zvmessage + sprintf with 2 variables
    message3      -  zvmessage + sprintf with 3 variables
    message4      -  zvmessage + sprintf with 4 variables
  
    errmess       -  zvmessage with error key (no sprintf)
    errmess1      -  zvmessage with error key + sprintf with 1 variable
    errmess2      -  zvmessage with error key + sprintf with 2 variables
    errmess3      -  zvmessage with error key + sprintf with 3 variables
    errmess4      -  zvmessage with error key + sprintf with 4 variables

    These subroutines have been written to provide a set of UNIX-ported
    C-callable sprintf/zvmessage utilities.  Their purpose is two-fold:

    1) To isolate all zvmessage calls so that changes to zvmessage can
    be made globally with changes to this module only; and

    2) To make it slightly easier & faster for programmers to include 
    lucid, userful, readable messages to the user in their software.
        - Most user messages can be reduced from two or more lines to one 
          (since you don't need to type out the "sprintf()" lines yourself 
          anymore.)
        - Also, formatting is simpler. The "\n" at the end of sprintf format 
          lines should not be included, since zvmessage inserts its own carriage
          returns. (See the example.)

    Variables may be of any type; they are picked up as voids.

    Text should be no longer than 249 characters, not including the '\0'. If 
    the sprintf'd string exceeds the maximum length, it will not be printed 
    and an error message will be printed instead. Use additional "message*"'s 
    as needed for longer text.

2 Calling Sequences

    The variables used in message1-message4, and errmess1-errmess4, may be
    of any type, so long as the conversion types (%d, %s, %f, etc) match
    the variable type. The relative placement of variables and conversion
    types is the same as sprintf().

    Please note that, UNLIKE SPRINTF, these routines should not be 
    called with a '\n' to force a newline. They include their own
    newlines; any additional '\n's added will create extra carriage
    returns.

    int i,j,status;
    char str,errkey[10];
    float r;
    
    null_message();
    message("text, no formats");
    message1("text with format such as %d",
             i);
    message2("text with format such as %d and %d",
             i,j);
    message3("text with format such as %d and %d and %s",
             i,j,str);
    message4("text with format such as %d and %d and %s and %f",
             i,j,str,r);
    errmess("Error message",
             errkey);
    errmess1("Error message with format %d",
             status,errkey)
    errmess2("Error message with format %d and %d",
             status,i,errkey)
    errmess3("Error message with format %d and %d and %s",
             status,i,str,errkey)
    errmess4("Error message with format %d and %d and %s and %f",
             status,i,str,r,errkey)

2 Example

       ********Start of example (linked to usr_message.c)********************
       #include "vicmain_c"
       #define VERSION "5-4-1993"
       main44() {
        int max=10,min=1,status=-3;
         message("Initializing, please wait.");
         message1("VICAR program SPAM, revision %s",VERSION);
         message2("Maximum scale is %d, minimum scale is %d.",max,min);
         errmess1("SPAM> malloc error, status=%d",status,"SPAM-W-BADMALLOC");
       }
       ********End of example***********************************************

2 History
    
	Original Programmer: Megan O'Shaughnessy
        Revisions:	     
        6/11/93	MOS	Original
        7/12/94 MOS	Converted to ANSI C. Extended maximum message
                        and error key lengths from 80 to 249 (the maximum
                        length for zvmessage.)
        4/18/97 TXH     Ported to HP 

2 Operation
    
	The routines in this module are based on the format string and
        any variables being sprintf()'d into a string, and then 
        the string is used in a call to zvmessage, with or without
        an error key. See the code for details; it's very simple.
$ Return
$!#############################################################################
