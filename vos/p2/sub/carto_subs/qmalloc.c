/*******************************************************************************

  Title:     qmalloc
  Author:    Mike Burl
  Date:      Nov 17, 1998
  Function:  Routine to malloc space and report errors if out of space. Can be
               used to allocate space and reset (i.e., like calloc) if the
               reset flag is set to TRUE.

  History:   2005/04/28 (MCB) - Added a bit more info to the error message
             2005/02/09 (MCB) - Added include of stdio.h to resolve stderr.

*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "qmalloc.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/


/**************************************/
/* qmalloc                            */
/**************************************/

void *qmalloc(size_t nelem, size_t elsize, int reset, char *infunc, char *vname)

{
  void  *ptr;

/*-------------------------------------------------------------------------------*/
  if (reset == QMALLOC_TRUE) {
    ptr = calloc(nelem, elsize);
  }
  else {
    ptr = malloc(nelem * elsize);
  }

  if (ptr == NULL) {
    fprintf(stderr, "ERROR (%s): couldn't malloc space for %s, requested (%ld X %ld)\n", infunc, vname, (unsigned long) nelem, (unsigned long) elsize);
    return(NULL);
  }
  else {
    return(ptr);
  }
}
