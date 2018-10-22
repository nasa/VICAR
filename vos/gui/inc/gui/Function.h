/* function.c -- contains code for compiling and executing user-specified
 * functions.
 */
#ifndef FUNCTION_H
#define FUNCTION_H
#include <string.h>

extern "C" int zknuth(char *,int *);
extern "C" void zxknuth(float *, float *);

void BlockFill(int val, char *dst, int len);

typedef struct FunctionDef
{
  float         args[20];       /* list of arguments to the function    */
  char          buf[980];       /* buffer to hold compiled function     */
}FunctionDef;

int ParseFunction(char *string, FunctionDef *func);
float ApplyFunction(FunctionDef *func, float arg);
#endif
