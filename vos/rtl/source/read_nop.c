#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine is for physical I/O to memory and array files.  Since the  */
/* entire file is in the buffer, no actual I/O is necessary.  This routine */
/* is just a stub.							   */

int v2_read_nop(void)
{
   return SUCCESS;
}
