#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/*

C Language Bridges for GALILEO NAIF TOOLKIT ROUTINES:

	BODN2C_G

*/

/*
 1st bridge for BODN2C_G, called from C 
*/
void zbodn2c_g(body_name, body_id, status)

char *body_name;
int *body_id;
int *status;
{
   int i;

   i = strlen(body_name);

   FTN_NAME(xbodn2c_g) (body_name, &i, body_id, status);
}
