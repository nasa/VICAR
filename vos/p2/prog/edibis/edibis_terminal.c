/*
 *  edibis_terminal.c
 *
 *   Handles ALL terminal I/O  and does non-blocking
 *   reads direct from keyboard/pad.
 *
 *   NB: This is not fully ported to all hosts.
 *   8-13-2011 - RJB - Fixes for gcc4.4.4 on 64-bit linux 
 *   12-08-2012 - RJB - Converted to POSIX termios.h compatibility
 *                  on LINUX and MacOSX 10.6.8 

 */

#include <stdio.h>
#include <unistd.h>         /* read and close */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"        /* zvmessage */
#if VMS_OS
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#else
#ifndef SOLARIS
#include <termios.h>        /* replaced termio.h - 12/8/2012 */
#endif
#ifdef SOLARIS
#include <termio.h>
#include <curses.h>
#endif
#include <term.h>
#include <sys/types.h>
#include <stdio.h>
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

#if UNIX_OS
/* prototypes */
int FTN_NAME(get_char)(int *startup);
static int settermmode(int terminal, int mode);
#endif

/*  This routine waits for the user to press a key, then processes the key    */
/*  and returns the value to the calling context.  The 'startup' parameter    */
/*  should be set to True (1) while getting input.  The routine should be     */
/*  called one last time when you are through gathering input with a para-    */
/*  meter value of False (0) to restore the terminal to normal.               */

/*  --Borrowed from GETINPUT.COM. May be eliminated if and when the           */
/*  "waitforinput" call can indicate whether the key returned was a Keypad    */
/*   escape sequence or not. This routine returns everything.                 */

int FTN_NAME(get_char)(startup)
int *startup;
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
   $DESCRIPTOR(name, "sys$command");

   if (*startup) {
      if (first) {
         stat = SYS$ASSIGN(&name, &channel, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error assigning a channel to your terminal:", "");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (0);
         }
         first = FALSE;
      }

      stat = SYS$QIOW(0, channel, IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
           &iosb, 0, 0, &inpchar, 1, 0, 0, 0, 0);
      if (stat != SS$_NORMAL || iosb.status != SS$_NORMAL) {
         zvmessage("Error reading terminal input:"," ");
         sprintf(errmsg, "     status = %d, iosb status = %d", stat, iosb.status);
         zvmessage(errmsg, " ");
         return (0);
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

   ssize_t stat;
   static int terminal;

   if (startup) {
      if (first) {
         first = FALSE;
         terminal = open("/dev/tty", O_RDONLY, 0);
         if (terminal == -1) {
            zvmessage("Unable to obtain terminal file descriptor--using default instead."," ");
            terminal = 0;
         }
         settermmode(terminal, STDCHAR);
      }

      stat = read(terminal, &inpchar, sizeof(inpchar));
      if (stat <= 0) {
         zvmessage("Error reading terminal input:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, " ");
         perror(NULL);
         return (0);
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

   /* deal with VAX/UNIX differences in <return> codes */
  
   if ((int)inpchar == 10 || (int)inpchar == 13)
      return (13);
   else
      return ((int) inpchar);
}



#if UNIX_OS
static int settermmode(terminal, mode)
int terminal, mode;
{
   static struct termios tbufsave;          /* termio to termios */
   struct termios tbuf;                     /* termio to termios */
   static int termset = FALSE;
//   char errmsg[81];

   if (mode != RESET) {
/*
      if (ioctl(terminal, TCGETA, &tbuf) == -1) {
         zvmessage("Error getting tty info:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }

    change for termios.h
*/
   if (tcgetattr(terminal, &tbufsave) != 0)
           perror("edibis_terminal: tcgetattr error");      

      tbufsave = tbuf;
      termset = TRUE;

      if (mode == RAW) {
         tbuf.c_iflag &= (short unsigned int)(~(INLCR | ICRNL | ISTRIP | IXON | BRKINT));
         tbuf.c_oflag &= (short unsigned int)(~OPOST);
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ISIG | ECHO));
      }
      else if (mode == STDCHAR) {
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ECHO));
      }
      else if (mode == CTRLCHAR) {
         tbuf.c_iflag &= (short unsigned int)(~BRKINT);
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ISIG | ECHO));
      }

      tbuf.c_cc[4] = sizeof(char);
      tbuf.c_cc[5] = 2;
/*      if (ioctl(terminal, TCSETAF, &tbuf) == -1) {
         zvmessage("Error setting terminal mode:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
*/
      if (tcsetattr(terminal, TCSAFLUSH, &tbufsave) != 0)
           perror("edibis_terminal: tcsetattr error");
   }
   else {
      if (termset) {
/*         if (ioctl(terminal, TCSETAF, &tbufsave) == -1) {
            zvmessage("Error re-setting terminal:", " ");
            sprintf(errmsg, "     errno = %d; ", errno);
            zvmessage(errmsg, "");
            perror(NULL);
         }
*/     
      if (tcsetattr(terminal,  TCSAFLUSH, &tbufsave) != 0)
           perror("edibis_terminal: tcsetattr reset error");
         termset = FALSE;
      }
   }
return(termset);
}
#endif



