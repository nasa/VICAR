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
