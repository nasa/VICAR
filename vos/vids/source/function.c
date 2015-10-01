/* function.c -- contains code for compiling and executing user-specified
 * functions.
 */
#include "VIDSdefs.h"
/************************************************************************/
/* ParseFunction verifies and compiles a user function string to make it
 * useable for ApplyFunction().  The string must be in upper case.
 */
int ParseFunction(string, func)
  char			*string;	/* in: user function string	*/
  FunctionDef		*func;		/* out: compiled function	*/
{
  char	locStr[STRINGSIZ+1];		/* local copy of string		*/
  int	i,j;

  i = 0; j = 0;
  while (i < strlen(string))			/* first, replace all	*/
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
  BlockFill(0, func, sizeof(FunctionDef));
  i = zknuth(locStr, func);		/* parse the string		*/
  if (i != 0) return FAIL;
  return SUCCESS;
}
/************************************************************************/
/* ApplyFunction will apply the compiled function func to the argument
 * contained in the compiled expression.
 * Returns the result of the evaluated function.
 */
float ApplyFunction(func, arg)
  FunctionDef *func;			/* in: compiled function	*/
  float arg;				/* in: variable to evaluate	*/
{
  float result;

  func->args[0] = arg;
  zxknuth(func, &result);
  return result;
}
/************************************************************************/
