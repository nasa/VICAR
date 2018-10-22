#include <string.h>

void upcase();


/*---------------------------------------------------------------------------*/
void upcase (instring)
  char *instring;
{
  int length, j;

  length = strlen(instring);
  for (j = 0; j < length; j++)
    instring[j] = toupper(instring[j]);
}  

