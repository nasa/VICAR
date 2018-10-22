
#include <stdio.h>
#include <string.h>

/*---------------------------------------------------------------------------*/
int getline_vicar (s, linelength, infile, eof)
     char *s;
     int linelength;
     FILE *infile;
     int *eof;
{
  int c, i = 0;
  
  memset (s,'\0',linelength);
  while (i < linelength && (c=getc(infile)) != EOF && c != '\n')
    s[i++] = c;
  
  if (c == '\n')
    s[i++] = c;
  
  *eof = (c == EOF) ? 1 : 0;
  
  return i;
}

