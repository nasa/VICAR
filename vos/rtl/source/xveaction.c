#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Set up the default error action and user message */

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int FTN_NAME2(xveaction, XVEACTION) (char *action, char *message, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_action[MAX_SHORT_STRING_SIZE+1];
   char c_message[MAX_STRING_SIZE+1];

   zsfor2c(c_action, MAX_SHORT_STRING_SIZE, action, &action, 2, 1, 1, message);
   zsfor2c(c_message, MAX_STRING_SIZE, message, &action, 2, 2, 2, message);

   return zveaction(c_action, c_message);

}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zveaction(char *action, char *message)
{
   int status;
   char temp[MAX_SHORT_STRING_SIZE+1];
   VALUE_TYPE value;

   if (strlen(action) > MAX_SHORT_STRING_SIZE)
      return BAD_ERR_ACT_VALUE;

   v2_make_upper_case(temp, action);

   value.s = temp;
   status = v2_error_action(value);
   if (status != SUCCESS)
      return status;

   value.s = message;
   status = v2_error_mess(value);
   if (status != SUCCESS)
      return status;

   strcpy(def_err_act, temp);
   strcpy(def_err_mess, message);

   return SUCCESS;

}
