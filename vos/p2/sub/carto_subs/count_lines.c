/*******************************************************************************

Title:    count_lines
Author:   Mike Burl
Date:     Oct 16, 1998
Function: This is the C-version of the count_lines.m routine I wrote for MATLAB.
            There are four different ways to call the function, as listed under USAGE.
            For the most basic version (a), the function opens a file, counts the 
            number of lines and closes the file. For (b), we leave the file open
            and "rewound". For (c) we count lines beginning with certain comment
            characters separately. Option (d) is a combination of (b) and (c).
 
Usage:    (1) count_lines(filename, 1, &n_lines);
          (2) count_lines(filename, 2, &n_lines, &fp);
          (3) count_lines(filename, 3, cc, &n_lines, &n_comments);
          (4) count_lines(filename, 4, cc, &n_lines, &n_comments, &fp);

NOTE:    A shortcut for remembering the correct value of _usage_ is to
           count the number of variable arguments (i.e., the args after
           _usage_ in the function call).

*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include "burl.h"
#include "count_lines.h"

#ifndef MAXBUF
#define MAXBUF 4096
#endif

/**********************************/
/* count_lines                    */
/**********************************/

int count_lines(char *filename, int usage, ...)

{
  int         *nl_adr;
  FILE        **fp_adr, *fp;
  int         *nc_adr;
  char        *cc;
  va_list     ap;
  char        buffer[MAXBUF];
  int         nl, nc;
  char        infunc[] = "count_lines";

  /*-------------------------------------------------------------------------------*/
  va_start(ap, usage);

  switch(usage) {
    case 1: {
      nl_adr = va_arg(ap, int *);
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      nl = 0;
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        ++nl;
      }
      *nl_adr = nl;
      fclose(fp);
      break;
    }
    case 2: {
      nl_adr = va_arg(ap, int *);
      fp_adr = va_arg(ap, FILE **);
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      nl = 0;
      while (fgets(buffer,MAXBUF, fp) != NULL) {
        ++nl;
      }

      *nl_adr = nl;
      *fp_adr = fp;
      rewind(fp);
       
      break;
    }
    case 3: {
      cc = va_arg(ap, char *);
      /* printf("Comment characters: %s\n", cc);*/
      nl_adr = va_arg(ap, int *);
      nc_adr = va_arg(ap, int *);
      nl = 0;
      nc = 0;
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        if (buffer[0] == cc[0]) {
          ++nc;
        }
        else {
          ++nl;
        }
      }

      *nl_adr = nl;
      *nc_adr = nc;
      fclose(fp);
      break;
    }
    case 4: {
      cc = va_arg(ap, char *);
      printf("Comment characters: %s\n", cc);
      nl_adr = va_arg(ap, int *);
      nc_adr = va_arg(ap, int *);
      fp_adr = va_arg(ap, FILE **);
      fp = fopen(filename, "r");

      nl = 0;
      nc = 0;
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        if (buffer[0] == cc[0]) {
          ++nc;
        }
        else {
          ++nl;
        }
      }

      *nl_adr = nl;
      *nc_adr = nc;
      *fp_adr = fp;
      rewind(fp);
      break;
    }
    default: {
      fprintf(stderr, "ERROR (%s): usage %d is not supported\n", infunc, usage);       
      return(ERR);
    }
  }

  va_end(ap);

  return(OK);
}
