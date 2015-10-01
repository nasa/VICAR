#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Set the tape position globals to the proper values */

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int FTN_NAME2(xvtpset, XVTPSET) (char *name, int *tfile, int *trec,
					ZFORSTR_PARAM)
/* char *name;		In: symbolic name for tape drive	*/
/* int *tfile;		In: File number for $TFILE		*/
/* int *trec;		In: Record number for $TREC		*/
{
   ZFORSTR_BLOCK
   char c_name[MAX_STRING_SIZE+1];

   zsfor2c(c_name, MAX_STRING_SIZE, name, &name, 3, 1, 1, trec);

   return zvtpset(c_name, *tfile, *trec);

}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvtpset(char *name, int tfile, int trec)
{
#if RTL_USE_TAPE
   int status;
   char c_name[MAX_STRING_SIZE+1];
   int t_index, t_file;
#endif

   current_call = VTPSET;

#if RTL_USE_TAPE

   v2_make_upper_case_max(c_name, name, MAX_STRING_SIZE);

/* Find tape index */

   status = v2_i_analyze(c_name, i_tape, i_count, &t_index, &t_file);
   if (status != I_TAPE) {
      v2_error_handler(NO_UNIT, DEVICE_NOT_MOUNTED);
      return DEVICE_NOT_MOUNTED;
   }

   i_file[t_index] = tfile;		/* Update tape globals */
   i_rec[t_index] = trec;

   return SUCCESS;

#else

   v2_error_handler(NO_UNIT, NO_TAPE_SUPPORT);	/* Tapes not supported */
   return NO_TAPE_SUPPORT;

#endif

}
