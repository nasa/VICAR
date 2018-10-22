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
