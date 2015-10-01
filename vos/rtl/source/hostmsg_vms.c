#include <descrip.h>
#include <ssdef.h>

/***************************************************************************
 *	hostmsg.  Returns a string with the system error message.
 ***************************************************************************
 */

hostmsg(code, msg, maxlen)
int code;				/* the system error code */
char *msg;				/* out: the string */
int maxlen;				/* in: max length of msg */

{
   int i;
   int len;

   $DESCRIPTOR(msg_desc, msg);

/* Msg shouldn't have to be cleared... This fixes a really weird bug though */
/* where the end-of-string null would sometimes be ignored!  I don't know   */
/* what the real problem is, but this works.				    */

   for (i=0; i<maxlen; i++)
      msg[i] = '\0';

   if ((code & 1) != SS$_NORMAL) {
      msg_desc.dsc$w_length = maxlen;

      sys$getmsg(code, &len, &msg_desc, 1, 0);
      if (len >= maxlen) len = maxlen-1;
      msg[len] = '\0';
   }
}
