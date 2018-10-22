/*

  Written by Thomas Roatsch, DLR             2-Sep-1993
  Prototypes added by Vadim Parizher, JPL   27-Jun-1997
  strcpy overlap bug fixed by Walt Bunch, JPL 19-Jul-2017

*/

/* Function file_no_path returns the filename without path */

#include "xvmaininc.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void file_no_path(char *filename)
{
#ifdef __VMS
   char *value;
   
   value = strrchr(filename,':');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
   value = strrchr(filename,']');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
#else
   char *slash;
   char *tmp;

   slash = strrchr(filename,'/');
   if (slash != NULL) {
     tmp = strdup(&slash[1]);
     strcpy(filename,tmp);
     free(tmp);
   }
#endif   
}
