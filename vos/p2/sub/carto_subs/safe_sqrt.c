/*******************************************************************************

  Title:    safe_sqrt
  Author:   Mike Burl 
  Date:     20070309
  Function: Check for less than zero condition before taking sqrt.
              Issue warning and return 0 if argumnet is negative.

  History: 
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "safe_sqrt.h"


/**********************/
/* SAFE_SQRT          */
/**********************/

double safe_sqrt(double a)

{
  char       infunc[] = "safe_sqrt";

  if (a < D_ZERO) {
    fprintf(stderr, "WARNING (%s): received negative operand = %.15f\n", infunc, a);
    return(D_ZERO);
  }
  else {
    return(sqrt(a));
  }
}
