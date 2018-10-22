/*******************************************************************************

  Title:    verbosity_manager
  Author:   Mike Burl 
  Date:     2005/01/26
  Function: Functions to set and get the verbosity level

*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "verbosity_manager.h"

/*******************************/
/* GLOBAL DECLARATIONS         */
/*******************************/
static int verbosity_level = 0;

/**************************************/
/* get_verbosity_level                */
/**************************************/
int get_verbosity_level(void)

{
  return(verbosity_level);
}

/**************************************/
/* set_verbosity_level                */
/**************************************/
void set_verbosity_level(int level)

{
  verbosity_level = level;

  return;
}
