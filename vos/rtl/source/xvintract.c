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

/* Initiate interactive parameter session and return the parameters in	*/
/* the buffer accessed by XVIPARM et al.				*/
/*									*/
/*    SUBCMD = subcommand in PDF. If blank, the entire PDF is used.	*/
/*		If a dash, then "procname-" is used in XQDYNP (this	*/
/*		is the preferred method).				*/
/*    PROMPT = prompt string to be used. If blank, the TAE default	*/
/*		(list of parameters) is used.				*/
/*									*/
/* The string passed in to q_dynp is either blank or of the form	*/
/* "-" or "-subcmd".  The main proc name is no longer prepended		*/
/* to the string.  q_dynp is smart enough to use the current proc	*/
/* if it is left off the string.					*/


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvintract, XVINTRACT) (char *subcmd, char *prompt, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_subcmd[MAX_STRING_SIZE+1], c_prompt[MAX_STRING_SIZE+1];

   zsfor2c(c_subcmd, MAX_STRING_SIZE, subcmd, &subcmd, 2, 1, 1, prompt);
   zsfor2c(c_prompt, MAX_STRING_SIZE, prompt, &subcmd, 2, 2, 2, prompt);

   zvintract(c_subcmd, c_prompt);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

#if RTL_USE_TAE

void zvintract(char *subcmd, char *prompt)
{
   char pdfnam[MAX_STRING_SIZE+1];

   strcpy(pdfnam, "");
   if (subcmd != NULL && strcmp(subcmd,"") != 0 &&
			 strcmp(subcmd," ") != 0) {	/* non-null string */
      if (*subcmd == '-')
         strcat(pdfnam, subcmd);
      else {
         strcat(pdfnam, "-");
         strcat(pdfnam, subcmd);
      }
   }

   q_init((struct PARBLK*) &iparb, P_BYTES, P_CONT);
   if (prompt != NULL && strcmp(prompt,"") != 0 &&
			 strcmp(prompt," ") != 0)	/* non-null prompt */
      q_string((struct PARBLK*) &iparb, "_PROMPT", 1, &prompt, P_ADD);
   q_dynp((struct PARBLK*) &iparb, pdfnam, M_FULLPDF);
   p_inim((struct PARBLK*) &iparb, P_BYTES, P_ABORT);

   return;
}

#else

void zvintract(char *UNUSED(subcmd), char *UNUSED(prompt))
{
   return;
}

#endif

