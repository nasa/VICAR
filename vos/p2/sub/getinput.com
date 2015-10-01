$!****************************************************************************
$!
$! Build proc for MIPL module getinput
$! VPACK Version 1.9, Wednesday, August 25, 2010, 11:31:52
$!
$! Execute by entering:		$ @getinput
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
$ write sys$output "*** module getinput ***"
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
$ write sys$output "Invalid argument given to getinput.com file -- ", primary
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
$   if F$SEARCH("getinput.imake") .nes. ""
$   then
$      vimake getinput
$      purge getinput.bld
$   else
$      if F$SEARCH("getinput.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getinput
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getinput.bld "STD"
$   else
$      @getinput.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getinput.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getinput.com -mixed -
	-s getinput.c -
	-i getinput.imake -
	-t tgetinput.c tgetinputf.f tgetinput.imake tgetinput.pdf -
	-o getinput.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getinput.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************/
/*                                                                            */
/*  FORTRAN:                                                                  */
/*  integer inpchar, waitforinput, pollinput, startup                         */
/*  inpchar = waitforinput(startup)                                           */
/*  inpchar = pollinput(startup)                                              */
/*                                                                            */
/*  C:                                                                        */
/*  int inpchar, startup;                                                     */
/*  inpchar = zwaitforinput(startup);                                         */
/*  inpchar = zpollinput(startup);                                            */
/*                                                                            */
/*  These functions are designed to get non-blocking keyboard input with-     */
/*  out making the user press <Enter>.  The two functions are identical in    */
/*  what keys they will and will not handle (more on that later), but they    */
/*  differ in one key respect:  the waitforinput() routine uses system rou-   */
/*  tines to wait until a key is pressed.  Control is not returned to the     */
/*  calling program until a key has been pressed.  The pollinput() routine,   */
/*  on the other hand, checks to see if a key has been pressed.  If it has,   */
/*  the value of the key is returned; otherwise, the function returns the     */
/*  null character (0).  Control is returned immediately to the calling       */
/*  program.  If an error occurs while trying to set up the terminal and read */
/*  the input, the functions return a value of VGI_INP_ERROR (-1).  If the    */
/*  functions are unable to determine which key has been pressed, it returns  */
/*  a value of VGI_NOT_KNOWN (-127).                                          */
/*                                                                            */
/*  Under most conditions, user input can and should be done using the stan-  */
/*  dard C or Fortran routines.  However, if you are designing an interactive */
/*  menu, for example, then you will need to be able to get the characters as */
/*  the user types them--waitforinput() is ideal for this situation.  The     */
/*  menu system in the Browse program is an example of this.                  */
/*                                                                            */
/*  Similarly, if you need to set up a task which can be carrying out other   */
/*  tasks while waiting for user input, then pollinput() should be used.  An  */
/*  example is the scrolling routine in the View program, where the user can  */
/*  use the keyboard to control the scrolling rate and direction inter-       */
/*  actively.                                                                 */
/*                                                                            */
/*  The only parameter to these functions is the 'startup' parameter.  This,  */
/*  when set to True (1), initializes the terminal properly for input.        */
/*  You should leave the parameter set to True until you are through getting  */
/*  input.  When you are through gathering input, you must call the routine   */
/*  one last time with a parameter of False (0) to reset the terminal to      */
/*  normal.  For this last case, each of the functions returns the null       */
/*  character which can be discarded by the calling program.                  */
/*                                                                            */
/*  One word of warning--the pollinput() routine uses system resources rather */
/*  heavily if all you are doing is waiting in a loop for input.  If that is  */
/*  the case, use the waitforinput() routine instead.                         */
/*                                                                            */
/*  Now, what keys do these routines handle and how does it pass the data     */
/*  back to you?  Basically, the routines are set up to handle Vt200 terminal */
/*  keyboard input.  This means that it handles all the normal typewriter     */
/*  keys as well as the arrow keys, PF1, PF2, PF3, PF4, Next, Prev, Find,     */
/*  Select, Insert, Remove, F7-F14, and F17-F20.  The function keys F1-F6 are */
/*  reserved for use by the terminal and I couldn't find the values for F15   */
/*  and F16 (nor any way to generate them).                                   */
/*                                                                            */
/*  In addition, these routines can also handle most of the <Ctrl>-<key> com- */
/*  binations.  In both Unix and VMS, some of these <Ctrl> sequences are      */
/*  reserved by the system and can't be handled by these routines.  In VMS,   */
/*  the routines can handle every combination *except* <Ctrl>-C, <Ctrl>-O,    */
/*  <Ctrl>-Q, <Ctrl>-S, <Ctrl>-T, <Ctrl>-X, and <Ctrl>-Y.  (For some reason,  */
/*  pressing <Ctrl>-X returns the control sequence associated with <Ctrl>-U.) */
/*                                                                            */
/*  In Unix, the routines can handle every combination except <Ctrl>-C,       */
/*  <Ctrl>-O, <Ctrl>-Q, <Ctrl>-S, <Ctrl>-Z.                                   */
/*                                                                            */
/*  One other wonderful little problem is what happens when you press <Enter> */
/*  on Unix vs. VMS.  In VMS, pressing <Enter> generates ASCII 13 (a carriage */
/*  return).  Similarly, pressing <Ctrl>-M has the same effect.  In Unix, on  */
/*  the other hand, pressing <Enter> generates ASCII 10 (a line feed) and     */
/*  pressing <Ctrl>-M does the same thing.  In both systems, pressing <Ctrl>-J*/
/*  generates ASCII 10 (line feed).                                           */
/*                                                                            */
/*  So, how did I deal with this?  Simple, really.  I decided to take the     */
/*  matter into my own hands so that the application programmer wouldn't      */
/*  have to deal with it.  On either system, pressing <Ctrl>-J, <Ctrl>-M,     */
/*  or <Enter> all do the same thing, return the C newline character ('\n').  */
/*  So far, this seems to be ASCII 10 on every system that I've tested this   */
/*  program on.  If your program will be checking for <Enter>, then you       */
/*  should test the input character against '\n' rather than specifically     */
/*  against <Enter>.                                                          */
/*                                                                            */
/*  This brings us to the next topic of discussion: how do you know what key  */
/*  has been pressed?  The routines basically split the key pressed into two  */
/*  categories: standard and extended.  The standard keystrokes consist of    */
/*  all the standard typewriter keys, as well as the <Ctrl>-<key> combina-    */
/*  tions.  I call these standard keystrokes because they consist of one      */
/*  character for each keystroke (even the <Ctrl>-<key> combinations which    */
/*  are mapped to ASCII 1-26).                                                */
/*                                                                            */
/*  The extended keystrokes, on the other hand, are all the function keys,    */
/*  PF keys, arrow keys, and special keys (e.g., Insert, Remove, etc.).       */
/*  These are called extended keystrokes because each of these keys, when     */
/*  pressed, generates several characters.  For example, function key F19     */
/*  generates the following sequence of five characters:  <ESC>[33~.          */
/*                                                                            */
/*  I decided to put the processing of such extended keys into these sub-     */
/*  routines to avoid making each application program deal with them.  If an  */
/*  extended key is pressed, these routines automatically determine which     */
/*  key has been pressed.                                                     */
/*                                                                            */
/*  So how do you know which key has been pressed?  There is an associated    */
/*  include file that has been released at the same time as these routines    */
/*  call vgi_keys.h.  This include file provides defines for every extended   */
/*  key that can be processed by these routines, as well as others, such as   */
/*  the <Ctrl>-<key> combinations and such special keys as <Tab> and <Back-   */
/*  space>.  The include file does not have defines for the standard charac-  */
/*  ter set because you can deal with that in C quite nicely.  The include    */
/*  also includes the definitions for the special cases (VGI_NOT_KNOWN and    */
/*  VGI_INP_ERROR).                                                           */
/*                                                                            */
/*  For examples of the usage of these routines, I suggest you look at the    */
/*  test file that accompanies them.  It gives a straightforward look at how  */
/*  to call them and how to process the return values.                        */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew                                   */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  03-10-93   N/A   PDB - Initial release.                                   */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "vgi_keys.h"
#include "zvproto.h"
#include <unistd.h>

#if VMS_OS
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#else
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#define RESET		0
#define RAW		1
#define STDCHAR		2
#define CTRLCHAR	3
#endif

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

#ifndef BOOLEAN
typedef unsigned char 	BOOLEAN;
#endif

#define WAIT_INPUT	20
#define POLL_INPUT	21

int settermmode();
int get_extended_input();
int setblock();
int get_extended_key();
int get_PF_key();
int get_function_key();


/*  This routine waits for the user to press a key, then processes the key    */
/*  and returns the value to the calling context.  The 'startup' parameter    */
/*  should be set to True (1) while getting input.  The routine should be     */
/*  called one last time when you are through gathering input with a para-    */
/*  meter value of False (0) to restore the terminal to normal.               */

int FTN_NAME2(waitforinput, WAITFORINPUT) (startup)
int *startup;
{
   return (zwaitforinput(*startup));
}

int zwaitforinput(startup)
int startup;
{
   char inpchar, errmsg[81];
   static BOOLEAN first = TRUE;

#if VMS_OS
   int stat;
   static int  channel;
   static struct {
      unsigned short status;
      unsigned short count;
      long info;
   } iosb;
   $DESCRIPTOR(name, "sys$output");

   if (startup) {
      if (first) {
         stat = SYS$ASSIGN(&name, &channel, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error assigning a channel to your terminal:", "");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (VGI_INP_ERROR);
         }
         first = FALSE;
      }

      stat = SYS$QIOW(0, channel, IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
           &iosb, 0, 0, &inpchar, 1, 0, 0, 0, 0);
      if (stat != SS$_NORMAL || iosb.status != SS$_NORMAL) {
         zvmessage("Error reading terminal input:","");
         sprintf(errmsg, "     status = %d, iosb status = %d", stat, iosb.status);
         zvmessage(errmsg, "");
         return (VGI_INP_ERROR);
      }
   }
   else {
      if (!first) {
         SYS$CANCEL(channel);
         SYS$DASSGN(channel);
         first = TRUE;
      }
      return ((int) '\0');
   }

#else

   int stat;
   static int terminal;

   if (startup) {
      if (first) {
         first = FALSE;
         terminal = open("/dev/tty", O_RDONLY, 0);
         if (terminal == -1) {
            zvmessage("Unable to obtain terminal file descriptor--using default instead.","");
            terminal = 0;
         }
         settermmode(terminal, STDCHAR);
      }

      stat = read(terminal, &inpchar, sizeof(inpchar));
      if (stat <= 0) {
         zvmessage("Error reading terminal input:","");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
         return (VGI_INP_ERROR);
      }
   }
   else {
      if (!first) {
         settermmode(terminal, RESET);
         close(terminal);
         first = TRUE;
      }
      return ((int) '\0');
   }
#endif

   if (inpchar == VGI_ESC)
      return (get_extended_input(WAIT_INPUT));
   else if (inpchar == VGI_CR || inpchar == VGI_LF)
      return ((int) '\n');
   else
      return ((int) inpchar);
}


/*  This routine checks to see if a key has been pressed.  If so, it returns  */
/*  the value of the key; otherwise, it return '\0', the null character.      */
/*  The 'startup' parameter should be set to True (1) while getting input.    */
/*  The routine should be called one last time when you are through gathering */
/*  input with a parameter value of False (0) to restore the terminal to      */
/*  normal operation.                                                         */

int FTN_NAME2(pollinput, POLLINPUT) (startup)
int *startup;
{
   return (zpollinput(*startup));
}

int zpollinput(startup)
int startup;
{
   char inpchar, errmsg[81];
   static BOOLEAN first = TRUE;

#if VMS_OS
   int stat;
   static char theChar;
   static int channel;
   static long eventFlag;
   static struct {
      unsigned short status;
      unsigned short count;
      long info;
   } iosb;
   $DESCRIPTOR(name, "sys$output");

   if (startup) {
      if (first) {
         stat = SYS$ASSIGN(&name, &channel, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error assigning a channel to your terminal:","");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (VGI_INP_ERROR);
         }
         stat = LIB$GET_EF(&eventFlag);
         if (stat != SS$_NORMAL) {
            zvmessage("Error getting an event flag for the QIO operation:","");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (VGI_INP_ERROR);
         }
         stat = SYS$CLREF(eventFlag);
         if (stat != SS$_WASCLR && stat != SS$_WASSET) {
            zvmessage("Error clearing the event flag for the QIO operation:","");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (VGI_INP_ERROR);
         }
         stat = SYS$QIO(eventFlag, channel,
              IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
              &iosb, 0, 0, &theChar, 1, 0, 0, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error setting up the first QIO operation:","");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (VGI_INP_ERROR);
         }
         first = FALSE;
      }

      stat = SYS$READEF(eventFlag, &stat);
      if (stat != SS$_WASSET)
         return ((int) '\0');

      if (iosb.status != SS$_NORMAL)
         return (VGI_INP_ERROR);

      inpchar = theChar;
      stat = SYS$QIO(eventFlag, channel,
              IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
              &iosb, 0, 0, &theChar, 1, 0, 0, 0, 0);
      if (stat != SS$_NORMAL) {
         zvmessage("Error setting up the next QIO operation:","");
         sprintf(errmsg, "     status = %d", stat);
         zvmessage(errmsg, "");
         return (VGI_INP_ERROR);
      }
   }
   else {
      if (!first) {
         SYS$CANCEL(channel);
         SYS$DASSGN(channel);
         LIB$FREE_EF(&eventFlag);
         first = TRUE;
      }
      return ((int) '\0');
   }

#else

   static int  terminal;

   if (startup) {
      if (first) {
         first = FALSE;
         terminal = open("/dev/tty", O_RDONLY, 0);
         if (terminal == -1) {
            zvmessage("NOTICE:  Unable to obtain terminal file descriptor:  using 0 instead.","");
            terminal = 0;
         }
         settermmode(terminal, STDCHAR);
         setblock(terminal, FALSE);
      }

      if (read(terminal, &inpchar, sizeof(inpchar)) <= 0)
         return ((int) '\0');
   }
   else {
      if (!first) {
         settermmode(terminal, RESET);
         setblock(terminal, TRUE);
         close(terminal);
         first = TRUE;
      }
      return ((int) '\0');
   }
#endif

   if (inpchar == VGI_ESC)
      return (get_extended_input(POLL_INPUT));
   else if (inpchar == VGI_CR || inpchar == VGI_LF)
      return ((int) '\n');
   else
      return ((int) inpchar);
}


int get_extended_input(mode)
int mode;
/* The GET_EXTENDED_INPUT routine is called when the first character received */
/* from the keyboard is an [ESCAPE] character (ASCII 27).  This routine pro-  */
/* cesses the characters that follow in the stream to determine which key was */
/* actually pressed.                                                          */
{
   int inpchar, return_char;

   if (mode == WAIT_INPUT)
      inpchar = zwaitforinput(TRUE);
   else
      while ((inpchar = zpollinput(TRUE)) == (int) '\0')
         ;

   if (inpchar == (int) '\0')
      return_char = VGI_ESC;
   else if (inpchar == (int) '[')
      return_char = get_extended_key(mode);
   else if (inpchar == (int) 'O')
      return_char = get_PF_key(mode);
   
   return (return_char);
}


/*  This routine is called when the user presses a PF key or, on some X       */
/*  terminals, an arrow key.  It determines which key was pressed and returns */
/*  that value.                                                               */

int get_PF_key(mode)
int mode;
{
   int inpchar, return_char;

   if (mode == WAIT_INPUT)
      inpchar = zwaitforinput(TRUE);
   else
      while ((inpchar = zpollinput(TRUE)) == (int) '\0')
         ;

   if (inpchar == (int) 'P')
      return_char = VGI_PF1;
   else if (inpchar == (int) 'Q')
      return_char = VGI_PF2;
   else if (inpchar == (int) 'R')
      return_char = VGI_PF3;
   else if (inpchar == (int) 'S')
      return_char = VGI_PF4;
   else if (inpchar == (int) 'A')
      return_char = VGI_UP;
   else if (inpchar == (int) 'B')
      return_char = VGI_DOWN;
   else if (inpchar == (int) 'C')
      return_char = VGI_RIGHT;
   else if (inpchar == (int) 'D')
      return_char = VGI_LEFT;
   else
      return_char = VGI_NOT_KNOWN;

   return (return_char);
}


/*  This routine is called when you need to continue processing values to     */
/*  determine which key was actually pressed.  It handles arrow keys (on      */
/*  vt200 type terminals) immediately or calls another routine to determine   */
/*  which of the more complicated keys was pressed.                           */

int get_extended_key(mode)
int mode;
{
   int inpchar, return_char;

   if (mode == WAIT_INPUT)
      inpchar = zwaitforinput(TRUE);
   else
      while ((inpchar = zpollinput(TRUE)) == (int) '\0')
         ;

   if (inpchar == (int) 'A')
      return_char = VGI_UP;
   else if (inpchar == (int) 'B')
      return_char = VGI_DOWN;
   else if (inpchar == (int) 'C')
      return_char = VGI_RIGHT;
   else if (inpchar == (int) 'D')
      return_char = VGI_LEFT;
   else
      return_char = get_function_key(inpchar, mode);

   return (return_char);
}


/*  This routine is the last step in the chain.  If this routine can't find   */
/*  out which key was pressed, it returns VGI_NOT_KNOWN.  This handles all of */
/*  the function keys as well as the 'special' keys (e.g., Insert, Remove,    */
/*  etc.).                                                                    */

int get_function_key(prevchar, mode)
int prevchar, mode;
{
   int inpchar, return_char, fkeynum;

   if (mode == WAIT_INPUT)
      inpchar = zwaitforinput(TRUE);
   else
      while ((inpchar = zpollinput(TRUE)) == (int) '\0')
         ;

   if (inpchar == (int) '\0')
      return_char = VGI_NOT_KNOWN;
   else if (inpchar == (int) '~') {
      if (prevchar == (int) '1')
         return_char = VGI_FIND;
      else if (prevchar == (int) '2')
         return_char = VGI_INSERT;
      else if (prevchar == (int) '3')
         return_char = VGI_REMOVE;
      else if (prevchar == (int) '4')
         return_char = VGI_SELECT;
      else if (prevchar == (int) '5')
         return_char = VGI_PREV;
      else if (prevchar == (int) '6')
         return_char = VGI_NEXT;
   }
   else if (prevchar >= (int) '1' && prevchar <= (int) '3' &&
        inpchar >= (int) '0' && inpchar <= (int) '9') {
      fkeynum = (prevchar - (int) '0') * 10 + inpchar - (int) '0';
      if (fkeynum == 18)
         return_char = VGI_F7;
      else if (fkeynum == 19)
         return_char = VGI_F8;
      else if (fkeynum == 20)
         return_char = VGI_F9;
      else if (fkeynum == 21)
         return_char = VGI_F10;
      else if (fkeynum == 23)
         return_char = VGI_F11;
      else if (fkeynum == 24)
         return_char = VGI_F12;
      else if (fkeynum == 25)
         return_char = VGI_F13;
      else if (fkeynum == 26)
         return_char = VGI_F14;
      else if (fkeynum == 31)
         return_char = VGI_F17;
      else if (fkeynum == 32)
         return_char = VGI_F18;
      else if (fkeynum == 33)
         return_char = VGI_F19;
      else if (fkeynum == 34)
         return_char = VGI_F20;
      else
         return_char = VGI_NOT_KNOWN;
      if (mode == WAIT_INPUT)
         inpchar = zwaitforinput(TRUE);
      else {
         while ((inpchar = zpollinput(TRUE)) == (int) '\0')
            ;
      }
   }
   else {
      if (inpchar != (int) '\0' && inpchar != (int) '~') {
         if (mode == WAIT_INPUT)
            inpchar = zwaitforinput(TRUE);
         else {
            while ((inpchar = zpollinput(TRUE)) == (int) '\0')
               ;
         }
      }
      return_char = VGI_NOT_KNOWN;
   }
   return (return_char);
}

#if UNIX_OS

int setblock(terminal, on)
int terminal;
BOOLEAN on;
{
   static int blockf, nonblockf;
   static BOOLEAN first = TRUE;
   int flags;
   char errmsg[81];

   if (first) {
      first = FALSE;
      if ((flags = fcntl(terminal, F_GETFL, 0)) == -1) {
         zvmessage("Error getting fcntl flags:","");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
      blockf = flags & ~O_NDELAY;
      nonblockf = flags | O_NDELAY;
   }
   if (fcntl(terminal, F_SETFL, on ? blockf : nonblockf) == -1) {
      zvmessage("Error setting terminal blocking mode:","");
      sprintf(errmsg, "     errno = %d; ", errno);
      zvmessage(errmsg, "");
      perror(NULL);
   }
}


int settermmode(terminal, mode)
int terminal, mode;
{
   static struct termios tbufsave;
   struct termios tbuf;
   static int termset = FALSE;
   char errmsg[81];

   if (mode != RESET) {
      if (tcgetattr(terminal, &tbuf) == -1) {
         zvmessage("Error getting tty info:","");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
      tbufsave = tbuf;
      termset = TRUE;

      if (mode == RAW) {
	 /* used to include IUCLC also */
         tbuf.c_iflag &= ~(INLCR | ICRNL | ISTRIP | IXON | BRKINT);
         tbuf.c_oflag &= ~OPOST;
         tbuf.c_lflag &= ~(ICANON | ISIG | ECHO);
      }
      else if (mode == STDCHAR) {
         tbuf.c_lflag &= ~(ICANON | ECHO);
      }
      else if (mode == CTRLCHAR) {
         tbuf.c_iflag &= ~BRKINT;
         tbuf.c_lflag &= ~(ICANON | ISIG | ECHO);
      }

      tbuf.c_cc[4] = sizeof(char);
      tbuf.c_cc[5] = 2;
      if (tcsetattr(terminal, TCSANOW, &tbuf) == -1) {
         zvmessage("Error setting terminal mode:","");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
   }
   else {
      if (termset) {
         if (tcsetattr(terminal, TCSANOW, &tbufsave) == -1) {
            zvmessage("Error re-setting terminal:","");
            sprintf(errmsg, "     errno = %d; ", errno);
            zvmessage(errmsg, "");
            perror(NULL);
         }
         termset = FALSE;
      }
   }
}
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getinput.imake
#define SUBROUTINE getinput

#define MODULE_LIST getinput.c

#define USES_C

#define P2_SUBLIB
#define LIB_RTL
$ Return
$!#############################################################################
$Test_File:
$ create tgetinput.c
#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"
#include "vgi_keys.h"

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

main44()
{
   int  inpchar, done = FALSE;

   zvmessage("\nTesting GETINPUT C interface.\n", "");
   zvmessage("Testing \"wait mode\" input handling.  Please type a random assortment of", "");
   zvmessage("characters; then type \"q\" to quit and go on to the next test.\n", "");

   while (!done) {
      inpchar = zwaitforinput(TRUE);
      zprintchar(inpchar);
      if (inpchar == (int) 'q' || inpchar == (int) 'Q')
         done = TRUE;
   }
   inpchar = zwaitforinput(FALSE);

   zvmessage("\nTesting \"polling mode\" input handling.  Please type a random assortment of", "");
   zvmessage("characters; then type \"q\" to quit and go on to the next test.\n", "");

   done = FALSE;
   while (!done) {
      inpchar = zpollinput(TRUE);
      if (inpchar != (int) '\0')
         zprintchar(inpchar);
      if (inpchar == 'q' || inpchar == 'Q')
         done = TRUE;
   }
   inpchar = zpollinput(FALSE);
   zvmessage("\nDone testing GETINPUT C interface.\n", "");

   FTN_NAME(tgetinputf)();
   exit(0);
}


FTN_NAME(printchar)(inpchar)
int *inpchar;
{
   return (zprintchar(*inpchar));
}

zprintchar(inpchar)
int inpchar;
{
   char msg[81];

   switch (inpchar) {
      case VGI_INP_ERROR:
         zvmessage("An error occurred--unable to read keyboard input.", "");
         break;
      case VGI_CTRL_A:
         zvmessage("You typed <CTRL>-A", "");
         break;
      case VGI_CTRL_B:
         zvmessage("You typed <CTRL>-B", "");
         break;
      case VGI_CTRL_C:
         zvmessage("You typed <CTRL>-C", "");
         break;
      case VGI_CTRL_D:
         zvmessage("You typed <CTRL>-D", "");
         break;
      case VGI_CTRL_E:
         zvmessage("You typed <CTRL>-E", "");
         break;
      case VGI_CTRL_F:
         zvmessage("You typed <CTRL>-F", "");
         break;
      case VGI_CTRL_G:
         zvmessage("You typed <CTRL>-G", "");
         break;
      case VGI_CTRL_H:
         zvmessage("You typed <CTRL>-H or <BackSpace>", "");
         break;
      case VGI_CTRL_I:
         zvmessage("You typed <CTRL>-I or <TAB>", "");
         break;
      case VGI_CTRL_J:
         zvmessage("You typed <Return> or <LF>", "");
         break;
      case VGI_CTRL_K:
         zvmessage("You typed <CTRL>-K", "");
         break;
      case VGI_CTRL_L:
         zvmessage("You typed <CTRL>-L or <FormFeed>", "");
         break;
      case VGI_CTRL_M:
         zvmessage("You typed <Return> or <LF>", "");
         break;
      case VGI_CTRL_N:
         zvmessage("You typed <CTRL>-N", "");
         break;
      case VGI_CTRL_O:
         zvmessage("You typed <CTRL>-O", "");
         break;
      case VGI_CTRL_P:
         zvmessage("You typed <CTRL>-P", "");
         break;
      case VGI_CTRL_Q:
         zvmessage("You typed <CTRL>-Q", "");
         break;
      case VGI_CTRL_R:
         zvmessage("You typed <CTRL>-R", "");
         break;
      case VGI_CTRL_S:
         zvmessage("You typed <CTRL>-S", "");
         break;
      case VGI_CTRL_T:
         zvmessage("You typed <CTRL>-T", "");
         break;
      case VGI_CTRL_U:
         zvmessage("You typed <CTRL>-U", "");
         break;
      case VGI_CTRL_V:
         zvmessage("You typed <CTRL>-V", "");
         break;
      case VGI_CTRL_W:
         zvmessage("You typed <CTRL>-W", "");
         break;
      case VGI_CTRL_X:
         zvmessage("You typed <CTRL>-X", "");
         break;
      case VGI_CTRL_Y:
         zvmessage("You typed <CTRL>-Y", "");
         break;
      case VGI_CTRL_Z:
         zvmessage("You typed <CTRL>-Z", "");
         break;
      case VGI_DEL:
         zvmessage("You typed <DEL> or <BackSpace>", "");
         break;
      case VGI_ESC:
         zvmessage("You typed <ESC>", "");
         break;
      case VGI_PF1:
         zvmessage("You typed <PF1>", "");
         break;
      case VGI_PF2:
         zvmessage("You typed <PF2>", "");
         break;
      case VGI_PF3:
         zvmessage("You typed <PF3>", "");
         break;
      case VGI_PF4:
         zvmessage("You typed <PF4>", "");
         break;
      case VGI_F1:
         zvmessage("You typed <F1>", "");
         break;
      case VGI_F2:
         zvmessage("You typed <F2>", "");
         break;
      case VGI_F3:
         zvmessage("You typed <F3>", "");
         break;
      case VGI_F4:
         zvmessage("You typed <F4>", "");
         break;
      case VGI_F5:
         zvmessage("You typed <F5>", "");
         break;
      case VGI_F6:
         zvmessage("You typed <F6>", "");
         break;
      case VGI_F7:
         zvmessage("You typed <F7>", "");
         break;
      case VGI_F8:
         zvmessage("You typed <F8>", "");
         break;
      case VGI_F9:
         zvmessage("You typed <F9>", "");
         break;
      case VGI_F10:
         zvmessage("You typed <F10>", "");
         break;
      case VGI_F11:
         zvmessage("You typed <F11>", "");
         break;
      case VGI_F12:
         zvmessage("You typed <F12>", "");
         break;
      case VGI_F13:
         zvmessage("You typed <F13>", "");
         break;
      case VGI_F14:
         zvmessage("You typed <F14>", "");
         break;
      case VGI_F15:
         zvmessage("You typed <F15>", "");
         break;
      case VGI_F16:
         zvmessage("You typed <F16>", "");
         break;
      case VGI_F17:
         zvmessage("You typed <F17>", "");
         break;
      case VGI_F18:
         zvmessage("You typed <F18>", "");
         break;
      case VGI_F19:
         zvmessage("You typed <F19>", "");
         break;
      case VGI_F20:
         zvmessage("You typed <F20>", "");
         break;
      case VGI_UP:
         zvmessage("You typed <Up Arrow>", "");
         break;
      case VGI_DOWN:
         zvmessage("You typed <Down Arrow>", "");
         break;
      case VGI_RIGHT:
         zvmessage("You typed <Right Arrow>", "");
         break;
      case VGI_LEFT:
         zvmessage("You typed <Left Arrow>", "");
         break;
      case VGI_NEXT:
         zvmessage("You typed <NEXT>", "");
         break;
      case VGI_PREV:
         zvmessage("You typed <PREV>", "");
         break;
      case VGI_FIND:
         zvmessage("You typed <FIND>", "");
         break;
      case VGI_SELECT:
         zvmessage("You typed <SELECT>", "");
         break;
      case VGI_INSERT:
         zvmessage("You typed <INSERT>", "");
         break;
      case VGI_REMOVE:
         zvmessage("You typed <REMOVE>", "");
         break;
      case VGI_NOT_KNOWN:
         zvmessage("You typed a key which the subroutine does not recognize.", "");
         break;
      default:
         sprintf(msg, "You typed '%c' (ASCII %d)", (char) inpchar, inpchar);
         zvmessage(msg, "");
         break;
   }
}
$!-----------------------------------------------------------------------------
$ create tgetinputf.f
C Fortran routine to test Fortran bridge to subroutines pollinput() and
C waitinput()

       SUBROUTINE tgetinputf()
       integer inpchar, waitforinput, pollinput
       integer done

       call xvmessage('Testing GETINPUT Fortran interface.', ' ')
       call xvmessage(' ', ' ')

       call xvmessage('Testing wait mode input handling.', ' ')
       call xvmessage('Type "q" to quit.', ' ')
       call xvmessage(' ', ' ')

       done = 0
       do while (done .eq. 0)
          inpchar = waitforinput(1)
          call printchar(inpchar)
          if (inpchar .eq. 113 .or. inpchar .eq. 81) then
             done = 1
          endif
       enddo
       inpchar = waitforinput(0)

       call xvmessage(' ', ' ')
       call xvmessage('Testing polling mode input handling.', ' ')
       call xvmessage('Type "q" to quit.', ' ')
       call xvmessage(' ', ' ')

       done = 0
       do while (done .eq. 0)
          inpchar = pollinput(1)
          if (inpchar .ne. 0) then
             call printchar(inpchar)
          endif
          if (inpchar .eq. 113 .or. inpchar .eq. 81) then
             done = 1
          endif
       enddo
       inpchar = pollinput(0)

       call xvmessage(' ', ' ')
       call xvmessage('Done testing GETINPUT Fortran interface.' , ' ')
       call xvmessage(' ', ' ')

       return
       end
$!-----------------------------------------------------------------------------
$ create tgetinput.imake
#define PROGRAM tgetinput

#define MODULE_LIST tgetinput.c tgetinputf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_LOCAL
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$!-----------------------------------------------------------------------------
$ create tgetinput.pdf
process
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getinput.hlp
1 GETINPUT

  FORTRAN:
  integer inpchar, waitforinput, pollinput, startup
  inpchar = waitforinput(startup)
  inpchar = pollinput(startup)

  C:
  int inpchar, startup;
  inpchar = zwaitforinput(startup);
  inpchar = zpollinput(startup);

  These functions are designed to get non-blocking keyboard input with-
  out making the user press <Enter>.  The two functions are identical in
  what keys they will and will not handle (more on that later), but they
  differ in one key respect:  the waitforinput() routine uses system rou-
  tines to wait until a key is pressed.  Control is not returned to the
  calling program until a key has been pressed.  The pollinput() routine,
  on the other hand, checks to see if a key has been pressed.  If it has,
  the value of the key is returned; otherwise, the function returns the
  null character (0).  Control is returned immediately to the calling
  program.  If an error occurs while trying to set up the terminal and read
  the input, the functions return a value of VGI_INP_ERROR (-1).  If the
  functions are unable to determine which key has been pressed, it returns
  a value of VGI_NOT_KNOWN (-127).

  Under most conditions, user input can and should be done using the stan-
  dard C or Fortran routines.  However, if you are designing an interactive
  menu, for example, then you will need to be able to get the characters as
  the user types them--waitforinput() is ideal for this situation.  The
  menu system in the Browse program is an example of this.

  Similarly, if you need to set up a task which can be carrying out other
  tasks while waiting for user input, then pollinput() should be used.  An
  example is the scrolling routine in the View program, where the user can
  use the keyboard to control the scrolling rate and direction inter-
  actively.

  The only parameter to these functions is the 'startup' parameter.  This,
  when set to True (1), initializes the terminal properly for input.
  You should leave the parameter set to True until you are through getting
  input.  When you are through gathering input, you must call the routine
  one last time with a parameter of False (0) to reset the terminal to
  normal.  For this last case, each of the functions returns the null
  character which can be discarded by the calling program.

  One word of warning--the pollinput() routine uses system resources rather
  heavily if all you are doing is waiting in a loop for input.  If that is
  the case, use the waitforinput() routine instead.

  Now, what keys do these routines handle and how does it pass the data
  back to you?  Basically, the routines are set up to handle Vt200 terminal
  keyboard input.  This means that it handles all the normal typewriter
  keys as well as the arrow keys, PF1, PF2, PF3, PF4, Next, Prev, Find,
  Select, Insert, Remove, F7-F14, and F17-F20.  The function keys F1-F6 are
  reserved for use by the terminal and I couldn't find the values for F15
  and F16 (nor any way to generate them).

  In addition, these routines can also handle most of the <Ctrl>-<key> com-
  binations.  In both Unix and VMS, some of these <Ctrl> sequences are
  reserved by the system and can't be handled by these routines.  In VMS,
  the routines can handle every combination *except* <Ctrl>-C, <Ctrl>-O,
  <Ctrl>-Q, <Ctrl>-S, <Ctrl>-T, <Ctrl>-X, and <Ctrl>-Y.  (For some reason,
  pressing <Ctrl>-X returns the control sequence associated with <Ctrl>-U.)

  In Unix, the routines can handle every combination except <Ctrl>-C,
  <Ctrl>-O, <Ctrl>-Q, <Ctrl>-S, <Ctrl>-Z.

  One other wonderful little problem is what happens when you press <Enter>
  on Unix vs. VMS.  In VMS, pressing <Enter> generates ASCII 13 (a carriage
  return).  Similarly, pressing <Ctrl>-M has the same effect.  In Unix, on
  the other hand, pressing <Enter> generates ASCII 10 (a line feed) and
  pressing <Ctrl>-M does the same thing.  In both systems, pressing <Ctrl>-J
  generates ASCII 10 (line feed).

  So, how did I deal with this?  Simple, really.  I decided to take the
  matter into my own hands so that the application programmer wouldn't
  have to deal with it.  On either system, pressing <Ctrl>-J, <Ctrl>-M,
  or <Enter> all do the same thing, return the C newline character ('\n').
  So far, this seems to be ASCII 10 on every system that I've tested this
  program on.  If your program will be checking for <Enter>, then you
  should test the input character against '\n' rather than specifically
  against <Enter>.

  This brings us to the next topic of discussion: how do you know what key
  has been pressed?  The routines basically split the key pressed into two
  categories: standard and extended.  The standard keystrokes consist of
  all the standard typewriter keys, as well as the <Ctrl>-<key> combina-
  tions.  I call these standard keystrokes because they consist of one
  character for each keystroke (even the <Ctrl>-<key> combinations which
  are mapped to ASCII 1-26).

  The extended keystrokes, on the other hand, are all the function keys,
  PF keys, arrow keys, and special keys (e.g., Insert, Remove, etc.).
  These are called extended keystrokes because each of these keys, when
  pressed, generates several characters.  For example, function key F19
  generates the following sequence of five characters:  <ESC>[33~.

  I decided to put the processing of such extended keys into these sub-
  routines to avoid making each application program deal with them.  If an
  extended key is pressed, these routines automatically determine which
  key has been pressed.

  So how do you know which key has been pressed?  There is an associated
  include file that has been released at the same time as these routines
  call vgi_keys.h.  This include file provides defines for every extended
  key that can be processed by these routines, as well as others, such as
  the <Ctrl>-<key> combinations and such special keys as <Tab> and <Back-
  space>.  The include file does not have defines for the standard charac-
  ter set because you can deal with that in C quite nicely.  The include
  also includes the definitions for the special cases (VGI_NOT_KNOWN and
  VGI_INP_ERROR).

  For examples of the usage of these routines, I suggest you look at the
  test file that accompanies them.  It gives a straightforward look at how
  to call them and how to process the return values.

2 History

  Original Programmer:  Paul Bartholomew  03-10-93
  Current Cognizant Programmer:  Paul Bartholomew  03-10-93
  Source Language:  C
$ Return
$!#############################################################################
