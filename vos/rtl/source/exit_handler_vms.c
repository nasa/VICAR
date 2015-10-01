#include "xvmaininc.h"

/* Declare an exit handler */

int exit_handler(func)
void (*func)();
{

   struct EX_CB			/* Exit handler control block */
   {
      struct EX_CB *flink;	/* used by VMS				*/
      long hndlrpt;		/* pointer to my handler		*/
      char numarg;		/* number of arguments to follow	*/
      char fill[3];		/* fill out the longword		*/
      long *statargpt;		/* pointer to mandatory first arg	*/
   /* long farg1,farg2,farg3; */ /* other optional arguments		*/
   };

   static struct EX_CB excb;
   static int code;
   int status;

   excb.flink = 0;
   excb.fill[0] = excb.fill[1] = excb.fill[2] = 0;		/* required */
   excb.hndlrpt = func;
   excb.numarg = 1;
   excb.statargpt = &code;

   return sys$dclexh(&excb);
}

