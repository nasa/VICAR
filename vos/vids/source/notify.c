/*		Notify.c
 * This file contains code to handle the sending of messages
 * to the user.  It stands in its own file so that we can use locally
 * global variables to control the print amount.
 */
#include "VIDSdefs.h"
#include "xvmaininc.h"		/* for UNIX_OS and VMS_OS */

MessageType howMuch = Inform;

/************************************************************************/
/* NotifyUser is a cross between printf and p_mput; It will put the
 * parameters into the message string, and print out the message to the
 * standard output with the key tied to it, a la TAE.  The message is
 * actually sent back to TAE for printing; this allows the print to appear
 * in the session/batch logs.
 *
 * NOTE:  Unfortunately, this form of communication does not work in TAE
 * under Unix.  So, for Unix, we just print the buffer directly.
 * THIS NEEDS TO BE FIXED SO SESSION LOGS WILL WORK IN UNIX!!!
 *
 * NOTE:  You cannot send a type double to this routine, as it is bigger
 * than a pointer.  Use sprintf then call this routine with the message.
 */
NotifyUser(type,key,message,p1,p2,p3,p4,p5,p6,p7,p8)
  MessageType type;		/* What type of message is this?	*/
  char	*key,*message;		/* msg to print with (maybe empty) key	*/
  int	p1,p2,p3,p4,p5,p6,p7,p8;
{
  char locbuf[256];
  
  if (type <= howMuch)
  {
    sprintf(locbuf,message,p1,p2,p3,p4,p5,p6,p7,p8);
#if UNIX_OS
    zvmessage(locbuf,key);		/* for direct printing */
#else
    send_message(locbuf,key);
#endif
  }
  return;
}
/************************************************************************/
/* SetMessage sets the amount of each message to print. 
 */
SetMessage(type)
  MessageType type;
{
  howMuch = type;
  return;
}
/************************************************************************/
/* GetMessage returns the type of messages to print (returns what was
 * last given to SetMessage).
 */
MessageType GetMessage()
{
  return howMuch;
}
