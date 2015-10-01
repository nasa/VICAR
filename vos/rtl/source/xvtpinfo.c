#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Outputs the tape drive device name and file position, given	*/
/* the symbolic name of the tape.				*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int FTN_NAME2(xvtpinfo, XVTPINFO) (char *sym_name, char *dev_name, int *tfile,
				int *trec, ZFORSTR_PARAM)
/* char *sym_name;	In: symbolic name for tape drive	*/
/* char *dev_name;	Out: tape drive device name		*/
/* int *tfile;		Out: File number from $TFILE		*/
/* int *trec;		Out: Rec number from $TREC		*/
{
   ZFORSTR_BLOCK
   char c_sym_name[MAX_STRING_SIZE+1];
   char c_dev_name[MAX_STRING_SIZE+1];
   int status;

   zsfor2c(c_sym_name, MAX_STRING_SIZE, sym_name, &sym_name, 4, 1, 1, trec);

   status = zvtpinfo(c_sym_name, c_dev_name, tfile, trec);

   zsc2for(c_dev_name, 0, dev_name, &sym_name, 4, 2, 2, trec);

   return status;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvtpinfo(char *sym_name, char * dev_name, 
	     int * tfile, int * trec)
{
#if RTL_USE_TAPE
   int status;
   char c_sym_name[MAX_STRING_SIZE+1];
   int t_index, t_file;
#endif

   current_call = VTPINFO;

#if RTL_USE_TAPE

   v2_make_upper_case_max(c_sym_name, sym_name, MAX_STRING_SIZE);

/* Find tape index */

   status = v2_i_analyze(c_sym_name, i_tape, i_count, &t_index, &t_file);
   if (status != I_TAPE) {
      dev_name[0] = '\0';
      *tfile = *trec = 0;
      v2_error_handler(NO_UNIT, DEVICE_NOT_MOUNTED);
      return DEVICE_NOT_MOUNTED;
   }

   v2_i_crack(i_tape[t_index], c_sym_name, dev_name);  /* Look up device name */

   *tfile = i_file[t_index];
   *trec = i_rec[t_index];

   return SUCCESS;

#else

   dev_name[0] = '\0';
   *tfile = *trec = 0;
   v2_error_handler(NO_UNIT, NO_TAPE_SUPPORT);	/* Tapes not supported */
   return NO_TAPE_SUPPORT;

#endif

}
