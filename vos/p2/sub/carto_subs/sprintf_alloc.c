/*******************************************************************************

  Title:     sprintf_alloc
  Author:    Mike Burl
  Date:      2005/03/16
  Function:  Routine to do an sprintf into a string that is allocated to be
               exactly the right size to hold it. Note that unlike sprintf,
               the first arg to sprintf_alloc must be **char (NOT *char).

  History:   

*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include "qmalloc.h"
#include "burl.h"
#include "sprintf_alloc.h"


#define MAXSPRINTF 32768

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/


/**************************************/
/* sprintf_alloc                      */
/**************************************/

int sprintf_alloc(char **S_adr, char *fmt, ...)

{
  va_list   ap;
  char      buffer[MAXSPRINTF];
  char      *S;
  int       L;
  char      infunc[] = "sprintf_alloc";

/*-------------------------------------------------------------------------------*/
  va_start(ap, fmt);
  vsprintf(buffer, fmt, ap);
  va_end(ap);
  L = strlen(buffer);
  S = (char *) qmalloc((L+1), sizeof(char), 0, infunc, "S");
  sprintf(S, "%s", buffer);
  *S_adr = S;

  return(OK);
}
