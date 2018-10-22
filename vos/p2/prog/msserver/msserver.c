#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include "ms_defines.h"
#include "cltsub.h"
#include "lclsub.h"
#include "ms_bridge.h"


int main (int argc, char **argv)
{
   int  ld,
        sd;

   pid_t  childpid;

   const char *cm = "SpiceServer";

   msclt_log (cm, "**** Starting SpiceServer v.01-27-1999 *****");
   if (mssvr_initAcceptor (&ld)) 
   {
      msclt_log (cm, "init FAILED !!!!");
      msclt_log (cm, "Server Terminating !!!!");
   }
   else msclt_log (cm, "init SUCCESS !!!!");

   zms_erract ("set", "return");


   /* Ignore the SIGCHLD signal, which is sent when a child process dies.  
    * When SIGCHLD's disposition is set to ignore,
    * children never create zombies.
    */
   signal(SIGCHLD, SIG_IGN);


   for (;;) 
   {
      if ((sd = mssvr_getNewClient(ld)) == (-1)) 
      {
         msclt_log (cm, "Cannot get new client");
         msclt_log (cm, "Server Terminating !!!!");
         exit (1);
      }

      switch (childpid = fork())
      {
         case -1 : 
            msclt_log (cm, "fork() FAILED");
            msclt_log (cm, "Server Terminating !!!!");
            exit (0);
            break;

         case 0 : 
            close (ld);  /* close listener descriptor */
            mssvr_handleRequest (sd);
            close (sd);  /* close socket descriptor */
            exit (0);
            break;

         default :
            close (sd); 
      }
   }
}
