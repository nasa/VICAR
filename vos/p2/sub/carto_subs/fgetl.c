/*******************************************************************************

  Title:    fgetl
  Author:   Mike Burl 
  Function: Similar to the fgets() library function except that the
            carriage return and newline characters are not included
            as part of the line. Gets at most n characters per line
            where n is a user-specified value.

*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include "fgetl.h"

char *fgetl(char *s, int n, FILE *fp)

{
  int  L;
  char *val;
  int  i;

  if ((val = fgets(s, n, fp)) != NULL) {
    L = strlen(s);
    i = L-1;
    while ((i >= 0) && ((s[i] == '\n') || (s[i] == '\r'))) {
      s[i--] = '\0';
    }
  }

  return(val);
}
