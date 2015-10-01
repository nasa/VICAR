#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include <stdlib.h>

/* Declare an exit handler */

int v2_exit_handler(void (*func)(int,void*))
{

#if ON_EXIT_AVAIL_OS

   if (on_exit(func, 0) == 0)
      return SUCCESS;

   return FAILURE;

#else

   return FAILURE;

#endif

}

