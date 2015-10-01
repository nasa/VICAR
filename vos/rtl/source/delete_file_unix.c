#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

/* Delete the file */

int v2_delete_file(int unit)
{
   int status;

   status = unlink(CURRENT_S_VALUE(NAME));
   if (status != 0)
      return errno;

   return SUCCESS;
}
