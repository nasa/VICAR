#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/************************************************************************/
/* xvcommand - Sends a command string to TAE to be executed.		*/
/*									*/
/* The string must be an intrinsic command or a procedure PDF that uses	*/
/* only intrinsic commands, i.e. no processes, no DCL.  This is mainly	*/
/* intended for use by VIDS, but it has other applications.		*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvcommand, XVCOMMAND) (char *fcmd, int *status, ZFORSTR_PARAM)
/* char *fcmd;			In: command to be executed */
/* int *status;			Out: returned status */
{
   ZFORSTR_BLOCK
   char c_cmd[250];

   zsfor2c(c_cmd, 249, fcmd, &fcmd, 2, 1, 1, status);

   *status = zvcommand(c_cmd);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

#if RTL_USE_TAE

int zvcommand(
   char *cmd			/* In: command to execute */
)

{
   int status;

   current_call = VCMD;		/* for error handler */

/* Note that q_cmdwait() can abort if q_init, q_string, or p_inim fail,	*/
/* but those are not likely to fail (certainly not from user input) so	*/
/* it should be okay.							*/

   status = q_cmdwait(cmd);
   if (status == P_BADNAME) {	/* no $SFI, probably means bad command */
      v2_error_handler(NO_UNIT, XVCOMMAND_ERROR);
      return XVCOMMAND_ERROR;
   }
   if (status != SUCCESS) {
      v2_error_handler(NO_UNIT, XVCOMMAND_FAIL);
      return status;
   }

   return SUCCESS;
}

#else

int zvcommand(char * UNUSED(command))
{
   current_call = VCMD;		/* for error handler */
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

