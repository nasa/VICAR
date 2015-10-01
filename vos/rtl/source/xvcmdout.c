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
/* xvcmdout - Sends a command string to TAE to be executed, and returns	*/
/* output values in the xviparm et al parblock.				*/
/*									*/
/* The string must be an intrinsic command or a procedure PDF that uses	*/
/* only intrinsic commands, i.e. no processes, no DCL.  This is mainly	*/
/* intended for use by VIDS, but it has other applications.		*/
/*									*/
/* xvcmdout() differs from xvcommand() only in that xvcmdout() makes	*/
/* output variables accessible.  This is most useful with the JGET	*/
/* VIDS command, which returns values through the parblock mechanism	*/
/* to the caller.  The returned values are then available via xviparm	*/
/* (and the other xvip* routines)... i.e. they're in the interactive	*/
/* parblock.								*/
/*									*/
/* Historical note:  This routine started out as xvrecpar(), which	*/
/* had to be called after xvcommand().  It was implemented using	*/
/* modifications to the core of TAE.  However, with TAE 4.1 new		*/
/* features were added that allowed the same functionality built-in	*/
/* to TAE.  To take advantage of them, xvcommand() and xvrecpar() had	*/
/* to be combined into a single routine, xvcmdout().			*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvcmdout, XVCMDOUT) (char *fcmd, int *status, ZFORSTR_PARAM)
/* char *fcmd;			In: command to be executed */
/* int *status;			Out: returned status */
{
   ZFORSTR_BLOCK
   char c_cmd[250];

   zsfor2c(c_cmd, 249, fcmd, &fcmd, 2, 1, 1, status);

   *status = zvcmdout(c_cmd);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

#if RTL_USE_TAE

int zvcmdout(
   char *cmd			/* In: command to execute */
)

{
   int status;
   struct VARIABLE *sfi;

   current_call = VCMDOUT;		/* for error handler */

/* Note that q_cmd() can abort if q_init, or q_string fail, but those	*/
/* are not likely to fail (certainly not from user input) so it should	*/
/* be okay.								*/

   status = q_cmd(cmd);
   if (status != SUCCESS) {
      v2_error_handler(NO_UNIT, XVCOMMAND_ERROR);
      return XVCOMMAND_ERROR;
   }

   status = p_inim((struct PARBLK*) &iparb, P_BYTES, P_CONT);
				/* Get result to iparb */
   if (status != SUCCESS) {
      v2_error_handler(NO_UNIT, XVCOMMAND_ERROR);
      return XVCOMMAND_ERROR;
   }

   // The cast is ok. The only difference between PARBLK and
   // LARGE_PARBLK is the size of the pool, which isn't used by p_fvar.
   sfi = p_fvar((struct PARBLK*) &iparb, "$SFI"); /* Get completion code */
   if (sfi) {
      status = IVAL(*sfi, 0);
      if (status != SUCCESS) {
         v2_error_handler(NO_UNIT, XVCOMMAND_FAIL);
         return status;
      }
      return SUCCESS;
   }

   v2_error_handler(NO_UNIT, XVCOMMAND_ERROR);	/* $SFI not found */
   return XVCOMMAND_ERROR;

}

#else

int zvcmdout(char * UNUSED(cmd))
{
   current_call = VCMDOUT;		/* for error handler */
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

