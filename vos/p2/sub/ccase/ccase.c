#include "xvmaininc.h"
#include "ftnbridge.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

/*  Values of flag - type of case conversion to perform */

#define UPPER  1
#define LOWER -1
#define SWAP   0
void zccase(char* str, int flag, int max);
void zuprcase(char *str);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(ccase, CCASE) (char *str, int *flag, int *max, ZFORSTR_PARAM)

#if 0
char *str;     /* string on which to do case conversion */
int *flag;     /* type of case conversion to perform    */
int *max;      /* maximum number of characters in str. However,
                  processing will cease on encoutering a 0 (null)
                  or after the Fortran length of the CHARACTER string
                  is reached.
                  If max < 0, processing continues until a 0 (null) is
                  encountered or until the Fortran length of the 
                  CHARACTER string is reached.
                  Thus, max may be set to -1 for a Fortran CHARACTER string.*/
#endif
{
   ZFORSTR_BLOCK
   char *c_string;
   int length, maxl;

   zsfor2len(length, str, &str, 3, 1, 1, max);       /* 3 args for ccase   */
   c_string = (char *)calloc(1,(length+1));	     /* str is 1st arg,  */
   zsfor2c(c_string, length, str, &str, 3, 1, 1,max);/* str is 1st string  */

   maxl = *max;
   if (length < maxl)    maxl = length;

   zccase( c_string, *flag, maxl );

   zsc2for( c_string, maxl, str, &str, 3, 1, 1, max); /* 3 args for ccase   */
   free (c_string);

}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zccase(char* str, int flag, int max)
#if 0
  char *str;	/* string on which to do case conversion */
  int flag;	/* type of case conversion to perform */
  int max;	/* maximum number of characters in str.  However, */
		/* processing will stop if a NULL is encountered. */
#endif
{
  int i, maxl;

/*  ==================================================================  */

 maxl = max;   /*  Negative max means keep going until NULL  */
 if (maxl < 0)  maxl = strlen(str);

 switch( flag) {
 case UPPER:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) 
		     str[i] = toupper(str[i]);
		break;

 case LOWER:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) 
		     str[i] = tolower(str[i]);
		break;

 case SWAP:	
 default:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) {
		     if ( islower(str[i]) )         str[i] = toupper(str[i]);
		     else if ( isupper(str[i]) )    str[i] = tolower(str[i]);
		}
		break;                  
 }
}
/************************************************************************/
/* C-Callable short cut for converting null-terminated string to upper case.*/
/************************************************************************/

void zuprcase(char *str)
{
  zccase( str, UPPER, -1);   /*  -1 means convert the whole string  */
}


