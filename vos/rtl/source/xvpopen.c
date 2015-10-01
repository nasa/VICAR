#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/************************************************************************/
/* Open a parameter file to be read by zvpinit().			*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/
/*  NOTE: n_par and max_parm_size are obsolete and are no longer used!	*/

void FTN_NAME2(xvpopen, XVPOPEN) (int *status, int *UNUSED(n_par), 
		int *UNUSED(max_parm_size),
		char *filename, char *error_act, int *unit, ZFORSTR_PARAM)
/* int *status;		out: return status				*/
/* int *n_par;		in: max # of params ***OBSOLETE***		*/
/* int *max_parm_size;	in: max size of all parms ***OBSOLETE***	*/
/* char *filename;	in: name of parameter file to be opened	*/
/* char *error_act;	in: value for OPEN_ACT,IO_ACT,ERR_ACT of file*/
/* int *unit;		out: unit number of file			*/
{
   ZFORSTR_BLOCK
   char c_filename[MAX_FILE_NAME_SIZE+1];
   char c_error_act[MAX_SHORT_STRING_SIZE+1];
   int *punit;
#if VMS_OS
   int dummy;
#endif

   current_call = VPOPEN;

   zsfor2c(c_filename, MAX_FILE_NAME_SIZE, filename, &status, 6, 4, 1, unit);

#if VMS_OS	/* must allow error_act & unit to be optional for VMS... grrr */
   if (n_args() >= 5)	/* hopefully this is temporary only */
      zsfor2c(c_error_act, MAX_SHORT_STRING_SIZE, error_act, &status, 6, 5, 2,
						unit);
   else
      c_error_act[0] = '\0';
   if (n_args() == 6)
      punit = unit;
   else
      punit = &dummy;
#else
   zsfor2c(c_error_act, MAX_SHORT_STRING_SIZE, error_act, &status, 6,5,2, unit);
   punit = unit;
#endif

   *status = zvpopen(c_filename, c_error_act, punit);

   return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

int zvpopen(
   char *filename,	/* input: name of parameter file to be opened	*/
   char *error_act,	/* input: value for OPEN_ACT and IO_ACT of file	*/
   int *unit		/* output: unit number of par file		*/
)

{
   int status;

   status = zvunit(&parm_file_unit,"XX",1,"U_NAME",filename, NULL);
   if (status != SUCCESS)
      return status;

   status = zvadd(parm_file_unit,"OPEN_ACT",error_act,"IO_ACT",error_act,
				   "LAB_ACT",error_act, NULL);
   if (status != SUCCESS)
      return status;

   *unit = parm_file_unit;

   status = zvopen(parm_file_unit,"OP","WRITE","TYPE","PARMS","U_NL",1,
		"U_NS",512,"U_FORMAT","BYTE","O_FORMAT","BYTE", NULL);
   if (status != SUCCESS)
      return status;

   current_call = VPOPEN;

   status = v2_parm_init(parm_file_unit);
   if (status != SUCCESS)
      return status;

   return SUCCESS;
}
