/*******************************************************************************
Title:    strsel.c
Author:   Mike Burl
Date:     2005/01/05

Function: Pick a substring out of a bigger string

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "utils_return_values.h"
#include "qmalloc.h"
#include "strsel.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* strsel                             */
/**************************************/

/* Note: dest must be preallocated to have at least i1-i0+2 elements. */

int strsel(char *dest, char *src, int i0, int i1)
{
  int                    i, j, L;
  /*  char                   infunc[] = "strsel"; */

  /*--------------------------------------------------------------*/
  L = i1-i0+1;
  for (i = i0, j = 0; i <= i1; i++, j++) {
    dest[j] = src[i];
  }
  dest[L] = '\0';

  return(OK);
}
