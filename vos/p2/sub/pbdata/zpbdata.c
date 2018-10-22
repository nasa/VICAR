#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/*
Bridge for PBDATA in C, called from C
*/
int zpbdata(name,buf)

char *name;
float *buf;
{
   int i, status;
   i=strlen(name);

   status = FTN_NAME(xpbdata) (name, &i, buf );
   return status;
 }
