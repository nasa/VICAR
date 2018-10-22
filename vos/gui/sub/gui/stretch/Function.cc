/* Function.cc -- contains code for compiling and executing user-specified
 * functions.
 */
#include "Function.h"
#include <string.h>

void BlockFill(int val, char *dst, int len)
{
   memset(dst, val, len);
}

/************************************************************************/
/* ParseFunction verifies and compiles a user function string to make it
 * useable for ApplyFunction().  The string must be in upper case.
 */

#define STRINGSIZ 	100

int ParseFunction(char *string, FunctionDef *func)
{
  char	locStr[101];		/* local copy of string		*/
  int  i, j;

  i = 0; j = 0;
  while (string && i < (int)strlen(string))	/* first, replace all	*/
  {						/* occurrences of DN	*/
    if (strncmp(&string[i], "DN", 2) == 0)	/* with IN1 for knuth	*/
    {
      strncpy(&locStr[j], "IN1", 3);
      j += 3; i += 2;
    }
    else
    {
      locStr[j] = string[i];
      j++; i++;
    }
  }
  locStr[j] = '$';		/* knuth() requires str to end with $	*/

  BlockFill(0, (char *)func, sizeof(FunctionDef));
  i = zknuth(locStr, (int *)func);		/* parse the string		*/
  if (i != 0) return 0;
  return 1;
}


/************************************************************************/
/* ApplyFunction will apply the compiled function func to the argument
 * contained in the compiled expression.
 * Returns the result of the evaluated function.
 */
float ApplyFunction(FunctionDef *func, float arg)
/*  FunctionDef *func		in: compiled function		*/
/*  float arg			in: variable to evaluate	*/
{
  float result;

  func->args[0] = arg;
  zxknuth((float *)func, &result);
  return result;
}
