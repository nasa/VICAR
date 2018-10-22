/*******************************************************************************

Title:    pfx.c
Author:   Mike Burl
Date:     Dec 26, 1996
Function: This is a C function to separate a string into path, file, and extension.
          Three integers indicate the index where each portion starts. If there is
          no path, p = -1. If there is no extension e = L, where L is the string 
          length.
 
Usage:    pfx(char *s, int *p, int *f, int *e);

*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include "pfx.h"

#define SLASH '/'
#define PERIOD '.'

/**************************************/
/* pfx                                */
/**************************************/

void pfx(char *s, int *p, int *f, int *e)

{
  int   L;
  char  *p_end, *e_start;

  L = strlen(s);
  
  /* Find rightmost slash */
  p_end = strrchr(s, SLASH);

  /* Find rightmost period */
  e_start = strrchr(s, PERIOD);

  if ((p_end != NULL) && (e_start != NULL)) {
    *p = 0;
    *f = p_end-s+1;
    *e = e_start-s;
    if (*e <= *f) {
      *e = L;
    }
  }
  else if ((p_end == NULL) && (e_start != NULL)) {
    *p = -1;
    *f = 0;
    *e = e_start-s;
    if (*e <= *f) {
      *e = L;
    }
  }
  else if ((p_end != NULL) && (e_start == NULL)) {  
    *p = 0;
    *f = p_end-s+1;
    *e = L;
  }
  else {
    *p = -1;
    *f = 0;
    *e = L;
  }

}   
