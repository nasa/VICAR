/*	xdesignal- Display a System Error Message
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 5, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = xdesignal ( Code )
 *
 *	Parameter List:
 *
 *		Code:	System Status Code
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#if VMS_OS
#include <descrip.h>
#include <ssdef.h>
#endif /* VMS_OS */

#define MESS_SIZE 132

#if UNIX_OS
#include <errno.h>
extern int errno;
char *get_strerror();
#endif

FUNCTION xdesignal ( Code )
INTEGER	Code;
   {
   return ( zdesignal ( *Code ));
   }

FUNCTION zdesignal ( code )
int	code;
   {
   int i, len, codes;
   static char mess[MESS_SIZE+1];
   static char error_key[20];

#if VMS_OS
   $DESCRIPTOR(msg_desc, mess);
#endif /* VMS_OS */

   if ( IS_XD_CODE( code )) {
      if ( IS_KNOWN_CODE( code )) {
         strcpy( error_key, "VRDI-" );
         strcat( error_key, KEY( code ) );
         zvmessage( MSG( code ), error_key );
         }

      else {      /* Code is not known */
         sprintf( mess, "Unrecognized error code %d ", code );
         zvmessage( mess, "VRDI-BADSTAT" );
         }
      }
   else {         /* Code is not an XD code */
#if VMS_OS
      codes = code;
      zvmessage( "Host Message follows: ", "VRDI_HOSTMSG" );
      for (i=0; i<MESS_SIZE+1; i++)
         mess[i] = '\0';

      if ((codes & 1) != SS$_NORMAL) {
         msg_desc.dsc$w_length = MESS_SIZE+1;
         sys$getmsg(codes, &len, &msg_desc, 1, 0);
         if (len >= MESS_SIZE)
            len = MESS_SIZE;
         mess[len] = '\0';
         zvmessage( mess, "" );
         }
#endif /* VMS_OS */

#if UNIX_OS
      if (code != SUCCESS) {
         zvmessage("Host Message follows: ", "VRDI_HOSTMSG" );
         zvmessage(get_strerror(errno), "");
         }
#endif /* UNIX_OS */
      }
   return ( SUCCESS );
   }




#if UNIX_OS

#if HOSTMSG_UNIX_OS
extern int sys_nerr;		/* system defined number of error codes */
extern char *sys_errlist[];	/* system string vector of error messages */
#else
char *strerror();
#endif

char *get_strerror(num)
int num;
{
#if HOSTMSG_UNIX_OS
   if (num < sys_nerr && num > 0)
      return (sys_errlist[num]);
   else
      return ("unknown system error");
#else
   return strerror(num);
#endif
}

#endif		/* UNIX_OS */

