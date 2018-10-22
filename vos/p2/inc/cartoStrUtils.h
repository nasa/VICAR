/*#ifndef CARTOSTRUTILS_H
#define CARTOSTRUTILS_H*/

void rztrim( char * buf );

void nicelen( char * hdr, double val, char * buf);

void scalefmt( char * outbuf, double scale1, double scale2 );

void trnsfmt( char * outbuf, double * t );

int grab( char * p, char c, char * buf );

/*================================================================

ms_dnum

ms_dnum converts a string to a double and moves the pointer, also
allows for positive and negative exponent with e or E or D or d, for
example 123.45E-002

function return : double

argument :
      1. num_ptr: input, char **num_ptr;

*/

double ms_dnum ( char ** num_ptr );

/*  parse ascii file to fill a TAE TCL variable   A. Zobrist  5/26/00   */


/*================================================================
ms_num

ms_num converts a string to an integer.

function return : integer

argument :
      1. num_ptr: input, char *num_ptr;

*/

int ms_num (char *num_ptr);

/*===================================================================

ms_find

ms_find searches string str1 for the substring str2 and returns
a pointer to the first location in string after the substring.

function return : character pointer

arguments :

      1. str1: input, char *str1;

      2. str2: input, char *str2;

Null pointer is returned if substring is not found.

*/

char *ms_find( char * str1, char * str2 );

char *nameget(char* s);

/*#endif*/
