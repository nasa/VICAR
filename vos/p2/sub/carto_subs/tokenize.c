#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils_return_values.h"
#include "qmalloc.h"
#include "tokenize.h"

/*******************************************************************************

Title:    tokenize.c (also includes function extract_token)
Author:   Mike Burl
Date:     20030611
Function: Find beginning and end of each token in line. Tokens are separated by 
            sep_chars or white space.

History:  2005/02/09 (MCB) - Added a function to determine maximum token length, 
            since this is commonly needed (for example, to allocatate an array of 
            characters of appropriate size to hold any of the tokens).

*******************************************************************************/


/*******************************/
/* TOKENIZE                    */
/*******************************/
/* Find beginning and end of each token in line. Tokens are separated by sep_chars or
   white space. White space or separators within a quoted string are ignored. i.e., the
   quoted string is returned as a single token. Also, if multiple separators occur
   with no valid non-sep/non-white characters between, it is treated as
   no token, rather than some kind of null token. */

int tokenize(char *line, char *sep_char, int *n_adr, int **b_adr, int **e_adr)
{
  int     l, s;
  int     *b, *e;
  int     *btmp, *etmp;
  int     i, k, n;
  int     inquote, intoken;
  int     white_space, sep;
  char    infunc[] = "tokenize";

  l = strlen(line);
  s = strlen(sep_char);
  
  btmp = (int *) qmalloc(l, sizeof(int), 1, infunc, "btmp");
  etmp = (int *) qmalloc(l, sizeof(int), 1, infunc, "etmp");
  
  inquote = 0;
  intoken = 0;
  n = 0;
  for (i = 0; i < l; i++) {
    if (inquote == 1) {
      /* See if this char ends the quote */
      if (line[i] == '"') {
        inquote = 0; 
      }
    }
    else {
      /* See if this char starts a quote */
      if (line[i] == '"') {
        inquote = 1;
        btmp[n] = i;
        intoken = 1;
      }
      else {
        /* See if this char is white space */
        if ((line[i] == ' ') || (line[i] == '\t') || (line[i] == '\n')) {
          white_space = 1;
        }
        else {
          white_space = 0;
        }
        /* See if this char is one of the sep chars */
        sep = 0;
        for (k = 0; k < s; k++) {
          if (line[i] == sep_char[k]) {
            sep = 1;
            break;
          }
        }
        if ((white_space == 1) || (sep == 1)) {  
          if (intoken == 1) {
            etmp[n] = i-1;
            intoken = 0;
            n++;
          }
        }
        else {
          /* Regular character */
          if (intoken == 0) {
            btmp[n] = i;
            intoken = 1;
	  }
	}
      }
    }
  }

  if (inquote == 1) {
    fprintf(stderr, "ERROR (%s): mismatched quotes in line %s\n", infunc, line);
    return(ERR);
  }
  if (intoken == 1) {
    /* Need to close it off */
    etmp[n] = l-1;
    intoken = 0;
    n++;
  }

  /* Copy the good data from btmp and etmp to e and b */
  b = (int *) qmalloc(n, sizeof(int), 1, infunc, "b");
  e = (int *) qmalloc(n, sizeof(int), 1, infunc, "e");
  for (k = 0; k < n; k++) {
    b[k] = btmp[k];
    e[k] = etmp[k];
  }
  *e_adr = e;
  *b_adr = b;
  *n_adr = n;
  free(btmp);
  free(etmp);
 
  return(OK);
}

/*******************************/
/* GET_MAX_TOKEN_LENGTH        */
/*******************************/
int get_max_token_length(int n_tokens, int *bt, int *et)
{
  int   i;
  int   L;

  L = 0;
  for (i = 0; i < n_tokens; i++) {
    if (et[i] - bt[i] + 1 > L) {
      L = et[i]-bt[i]+1;
    }
  }
  return(L);
}

/*******************************/
/* EXTRACT_TOKEN               */
/*******************************/
/* Extract w-th token from source and copy to dest with termination.
   Dest must be preallocated to at least e[w]-b[w]+2 characters. */

int extract_token(char *dest, char *src, int *b, int *e, int w)
{
  /*  char    infunc[] = "extract_token"; */

  strncpy(dest, src+b[w], e[w]-b[w]+1);
  dest[e[w]-b[w]+1] = '\0';
 
  return(OK);
}

/*******************************/
/* GET_TOKEN_LIST              */
/*******************************/
/* First call tokenize to determine the beginning and end positions of the tokens */
/* and the number of tokens, then call this function to extract each token into */
/* the equivalent of a cell array of strings */

int get_token_list(char *src, int n_tokens, int *b, int *e, char ***token_list_adr)
{
  int  i, L;
  char **token_list;
  char infunc[] = "get_token_list";

  token_list = (char **) qmalloc(n_tokens, sizeof(char *), 0, infunc, "token_list");
  *token_list_adr = token_list;

  for (i = 0; i < n_tokens; i++) {
    L = e[i] - b[i] + 1;
    token_list[i] = (char *) qmalloc((L+1), sizeof(char), 0, infunc, "token_list[i]");
    extract_token(token_list[i], src, b, e, i);
  }

  return(OK);
}
