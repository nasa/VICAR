#define S_NO_MESSAGE 512  /*NOMESSAGE bit in $SWITCH. See RTL routine xvzinit*/
#include "zvproto.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zifmessage(char* message)
{
   int n, swtch;
/*  ==================================================================  */
   zvp("$switch", &swtch, &n);	   /* Get $switch to check NOMESSAGE flag. */
   if ((swtch & S_NO_MESSAGE) == 0)   
      zvmessage(message,"");
}

