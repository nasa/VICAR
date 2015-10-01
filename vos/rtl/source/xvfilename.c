#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if VMS_OS
#include <descrip.h>
#include <ssdef.h>
#include <lnmdef.h>
#endif

/************************************************************************/
/* Given a filename as input by the user, returns a filename suitable	*/
/* for use with a system open() call or other non-VICAR file operation.	*/
/* For Unix, this means that environment variables and ~username are	*/
/* expanded.  For VMS, this means the old-style temporary filename	*/
/* suffix (.Zxx) is added.  For both systems, the "+" form of temporary	*/
/* file is expanded.							*/
/*									*/
/* This function does not need to be called if the RTL is used for I/O	*/
/* (it is called internally by x/zvopen()).  But, any program that gets	*/
/* a filename from the user for use in non-RTL I/O should make use of	*/
/* this function.							*/
/*									*/
/* Because this function is intended for use with non-VICAR I/O, only	*/
/* disk filenames are supported.  Names specifying tape files or memory	*/
/* files will be treated as if they are disk files, which may provide	*/
/* surprising results.  Note that '*' as a wildcard character is legal	*/
/* in this function, and is passed through unchanged (so the output	*/
/* has an '*' in it).							*/
/*									*/
/* For Unix, the expansions are as follows:				*/
/*	$var or ${var}		Expand environment variable "var"	*/
/*	~user			Expand to home directory of user "user"	*/
/*	~			Expand to home dir of current user ($HOME) */
/*	$$			Insert a single $ (no env var)		*/
/*	+			Expand to translation of "$VTMP/" (for	*/
/*                                      temporary files)		*/
/*									*/
/* For VMS, the expansions are as follows:				*/
/*	+			Expand to "v2$tmp:"			*/
/*	no + and no suffix	Append ".Zxx" (xx from v2$pidcode)	*/
/* Note that logical names are expanded.				*/
/*									*/
/* The temporary filename locations ($VTMP and v2$tmp) are set up in	*/
/* vicset2.csh or vicset2.com.						*/
/*									*/
/* The length argument to zvfilename represents the length of the	*/
/* output string buffer (to avoid overflow).  A length of 0 means the	*/
/* buffer is unlimited, and it is the user's responsibility to make	*/
/* sure there is no overflow.						*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvfilename, XVFILENAME) (char *in_name, char *out_name,
					int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char *c_in_name;
   char *c_out_name;
   int in_len, out_len;

   zsfor2len(in_len, in_name, &in_name, 3, 1, 1, status);
   c_in_name = malloc(in_len);
   if (c_in_name == NULL) {
      *status = INSUFFICIENT_MEMORY;
      return;
   }

   zsfor2len(out_len, out_name, &in_name, 3, 2, 2, status);
   c_out_name = malloc(out_len+1);
   if (c_out_name == NULL) {
      free(c_in_name);
      *status = INSUFFICIENT_MEMORY;
      return;
   }

   zsfor2c(c_in_name, in_len-1, in_name, &in_name, 3, 1, 1, status);

   *status = zvfilename(c_in_name, c_out_name, out_len+1);

   zsc2for(c_out_name, 0, out_name, &in_name, 3, 2, 2, status);

   free(c_in_name);
   free(c_out_name);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvfilename(char *in_name, char *out_name, int out_len)
{
   char *ptr;
   int status;
#if VMS_OS
   char *dot, *bracket, *semi;
   static int length;
   static char tmpname2[1024];
   static char tmpname[1024];		/* big enough for anything! */
   static struct {
      short length, code;
      int *addr, *retaddr, end;
   } trnlogitmlst = {1024, LNM$_STRING, tmpname, &length, 0};
   $DESCRIPTOR(logtabd, "LNM$DCL_LOGICAL");
   $DESCRIPTOR(named, tmpname2);
   long attr = LNM$M_CASE_BLIND;
#endif

   ptr = v2_expand_filename(in_name, FALSE, &status);

   if (status != SUCCESS)
      return status;

#if VMS_OS
   /* Here we have to check to see if ".Zxx" is needed because it is	*/
   /* normally handled by RMS.						*/

   if (first_call)
      v2_general_initialize();		/* get default filename if needed */

   /* First, translate the logical name, if any, to insure we get the	*/
   /* same result as the RTL would when opening the file.  The name	*/
   /* ends up in tmpname2.						*/

   strcpy(tmpname2, ptr);
   named.dsc$w_length = strlen(tmpname2);
   do {
      status = sys$trnlnm(&attr, &logtabd, &named, 0, &trnlogitmlst);
      if (status == SS$_NORMAL) {
         if (tmpname[length-1] == ':') length--;	/* strip trailing : */
         tmpname[length] = '\0';
         strcpy(tmpname2, tmpname);
         named.dsc$w_length = strlen(tmpname2);
      }
   } while (status == SS$_NORMAL);	/* loop till no more translations */

   ptr = tmpname2;

   dot = strrchr(ptr, '.');		/* Find last occurrence in string */
   bracket = strrchr(ptr, ']');

   /* Need to add the .Zxx if a dot doesn't exist, or exists only before a ] */

   if (dot == NULL || (bracket != NULL && dot < bracket)) {

      semi = strrchr(ptr, ';');
      if (semi == NULL)			/* Copy whole name */
         strcpy(tmpname, ptr);
      else {
         strncpy(tmpname, ptr, (semi - ptr));	/* copy up to ; */
         tmpname[semi-ptr] = '\0';
      }
      strcat(tmpname, default_file_name);	/* Add .Zxx */

      if (semi)
         strcat(tmpname, semi);	/* Add version number back in */

      ptr = tmpname;
   }
#endif /* VMS_OS */

   if (out_len) {
      strncpy(out_name, ptr, out_len);
      out_name[out_len-1] = '\0';

      if ((int) strlen(ptr) >= out_len)
         return NAME_TRUNCATED;
   }
   else					/* No output length specified */
      strcpy(out_name, ptr);

   return SUCCESS;

}
