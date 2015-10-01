#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

#include <ctype.h>


static int v2_preset_equals(char *s, char *pat);
static void v2_preset_get_act(char *src, char *dest);
static void v2_preset_get_mess(char *src, char *dest, int maxlen);
static void v2_preset_get_format(char *src, char *dest);

/************************************************************************/
/* This routine searches for the optionals "OPEN_ACT" and "OPEN_MES",	*/
/* and puts them into the current_table.  This routine is called before	*/
/* the normal process_optionals is.  This is done so that if an error	*/
/* occurs in XVOPEN before process_optionals sets OPEN_ACT, the action	*/
/* the caller desires can still take place.				*/
/*									*/
/* NOTE: This routine should be maintained in parallel with		*/
/* open_act_preset_for!!						*/
/************************************************************************/

int v2_open_act_preset_c(
   int unit,				/* Unit number			    */
   int nopts,				/* # optional args or -1 if unknown */
   va_list *params			/* optional arguments pointer	    */
)

{
   int status;
   int optno;
   char *s;
   char keyword[PARAM_NAME_LENGTH+1];
   VALUE_TYPE value;

   optno = 0;
   do {
      s = va_arg(*params, char *);		/* get the keyword */
      optno++;

      if (s != NULL && s[0] != '\0') {
         if (optno >= nopts && nopts >= 0) {
            return ODD_NUMBER_OF_OPTIONALS;	/* keyword with no value */
         }

         if (strlen(s) <= PARAM_NAME_LENGTH) {
            v2_make_upper_case(keyword, s);
            if (EQUAL(keyword, "OPEN_ACT")) {
               value.s = va_arg(*params, char *);
               optno++;

               status = v2_error_action(value);	/* validate the value */
               if (status != SUCCESS)
                  return status;

               status = v2_add_str_current_table(value.s, OPEN_ACT,
					current_table[unit], default_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else if (EQUAL(keyword, "OPEN_MES")) {

               value.s = va_arg(*params, char *);
               optno++;

               status = v2_error_mess(value);	/* validate the value */
               if (status != SUCCESS)
                  return status;

               status = v2_add_msg_current_table(value.s, OPEN_MES,
					current_table[unit], default_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else {
               value.s = va_arg(*params, char *); /* Assume 4-byte optionals */
               optno++;
            }
         }
      }
   } while (s != NULL && s[0] != '\0');		/* end main optionals loop */

   return SUCCESS;
}

/************************************************************************/
/* This routine searches for the optionals "OPEN_ACT" and "OPEN_MES",	*/
/* and puts them into the current_table.  This routine is called before	*/
/* the normal process_optionals is.  This is done so that if an error	*/
/* occurs in XVOPEN before process_optionals sets OPEN_ACT, the action	*/
/* the caller desires can still take place.				*/
/*									*/
/* This routine also performs the actions of get_nopts().  This avoids	*/
/* having to pre-scan the list twice for XVOPEN.			*/
/*									*/
/* NOTE: This routine should be maintained in parallel with		*/
/* open_act_preset_c!!							*/
/************************************************************************/

int v2_open_act_preset_for(
   int unit,				/* Unit number			    */
   int *nopts,				/* Returned # of opt args	    */
   int *nargs,				/* Returned # of args overall	    */
   int nconst,				/* Input # of constant args	    */
   va_list *params			/* optional arguments pointer	    */
)

{
   int status;
   VALUE_TYPE value;
   char *forstr;
   char *s;
   char tempstr[MAX_STRING_SIZE+1];

   *nopts = 0;
   *nargs = nconst;

   do {
      forstr = va_arg(*params, char *);		/* get the keyword */
      (*nopts)++;
      (*nargs)++;
      s = v2_sfor2ptr(forstr);		/* as a pointer to the chars */

      if (s[0] != ' ' && s[0] != '\0') {

         if (v2_preset_equals(s, "OPEN_ACT")) {

            value.s = va_arg(*params, char *);
            value.s = v2_sfor2ptr(value.s);
            v2_preset_get_act(value.s, tempstr);
            value.s = tempstr;
            (*nopts)++;
            (*nargs)++;

            status = v2_error_action(value);	/* validate the value */
            if (status != SUCCESS)
               return status;

            status = v2_add_str_current_table(value.s, OPEN_ACT,
					current_table[unit], default_table);
	    if (status != SUCCESS)
	       return UNABLE_TO_STORE_OPTIONAL;
         }
         else if (v2_preset_equals(s, "OPEN_MES")) {

            value.s = va_arg(*params, char *);
            value.s = v2_sfor2ptr(value.s);
            v2_preset_get_mess(value.s, tempstr, unit_table[OPEN_MES].size);
            value.s = tempstr;
            (*nopts)++;
            (*nargs)++;

            status = v2_error_mess(value);		/* validate the value */
            if (status != SUCCESS)
               return status;

            status = v2_add_msg_current_table(value.s, OPEN_MES,
					current_table[unit], default_table);
	    if (status != SUCCESS)
	       return UNABLE_TO_STORE_OPTIONAL;
         }
         else {
            value.s = va_arg(*params, char *);	/* Assume 4-byte optionals */
            (*nopts)++;
            (*nargs)++;
         }
      }
			/* end main optionals loop */
   } while (s[0] != ' ' && s[0] != '\0');

   return SUCCESS;
}

/************************************************************************/
/* This routine searches for the optional "FORMAT" (and, since we're in	*/
/* the mess, "ERR_ACT" and "ERR_MESS", but they aren't really required),*/
/* and puts them into the label_table.  This routine is called before	*/
/* the normal process_optionals is.  This is done so that XLADD and	*/
/* XLGET can know the data type of their "value" argument so the	*/
/* optionals can be processed correctly.				*/
/*									*/
/* This routine also performs the actions of get_nopts().  This avoids	*/
/* having to pre-scan the list twice.					*/
/************************************************************************/

int v2_format_preset_for(
   int unit,				/* Unit number			    */
   int *nopts,				/* Returned # of opt args	    */
   int *nargs,				/* Returned # of args overall	    */
   int nconst,				/* Input # of constant args	    */
   va_list *params			/* optional arguments pointer	    */
)

{
   int status;
   int format_found;			/* it's a required argument */
   VALUE_TYPE value;
   char *forstr;
   char *s;
   char tempstr[MAX_STRING_SIZE+1];

   format_found = FALSE;

   *nopts = 0;
   *nargs = nconst;

   do {
      forstr = va_arg(*params, char *);		/* get the keyword */
      (*nopts)++;
      (*nargs)++;
      s = v2_sfor2ptr(forstr);		/* as a pointer to the chars */

      if (s[0] != ' ' && s[0] != '\0') {

         if (v2_preset_equals(s, "FORMAT")) {

            value.s = va_arg(*params, char *);
            value.s = v2_sfor2ptr(value.s);
            v2_preset_get_format(value.s, tempstr);
            value.s = tempstr;
            (*nopts)++;
            (*nargs)++;

            status = v2_label_format(value);	/* validate the value */
            if (status != SUCCESS)
               return status;

            status = v2_add_str_current_table(value.s, LFORMAT,
					label_table[unit], label_default_table);
	    if (status != SUCCESS)
	       return UNABLE_TO_STORE_OPTIONAL;

	    format_found = TRUE;
         }

         else if (v2_preset_equals(s, "ERR_ACT")) {

            value.s = va_arg(*params, char *);
            value.s = v2_sfor2ptr(value.s);
            v2_preset_get_act(value.s, tempstr);
            value.s = tempstr;
            (*nopts)++;
            (*nargs)++;

            status = v2_error_action(value);	/* validate the value */
            if (status != SUCCESS)
               return status;

            status = v2_add_str_current_table(value.s, ERR_ACT,
					label_table[unit], label_default_table);
	    if (status != SUCCESS)
	       return UNABLE_TO_STORE_OPTIONAL;
         }
         else if (v2_preset_equals(s, "ERR_MESS")) {

            value.s = va_arg(*params, char *);
            value.s = v2_sfor2ptr(value.s);
            v2_preset_get_mess(value.s, tempstr, label_options[ERR_MESS].size);
            value.s = tempstr;
            (*nopts)++;
            (*nargs)++;

            status = v2_error_mess(value);		/* validate the value */
            if (status != SUCCESS)
               return status;

            status = v2_add_msg_current_table(value.s, ERR_MESS,
					label_table[unit], label_default_table);
	    if (status != SUCCESS)
	       return UNABLE_TO_STORE_OPTIONAL;
         }
         else {
            value.s = va_arg(*params, char *);	/* Assume 4-byte optionals */
            (*nopts)++;
            (*nargs)++;
         }
      }
			/* end main optionals loop */
   } while (s[0] != ' ' && s[0] != '\0');

#if VMS_OS		/* FORMAT is required unless we're in VMS */

   return SUCCESS;

#else

   if (format_found)
      return SUCCESS;
   else
      return FORMAT_OPTIONAL_REQUIRED;

#endif

}

/************************************************************************/
/* Check for the string 'pat' at 's'.  The string at 's' can be mixed	*/
/* case, and may not have a null terminator.  This is called only by	*/
/* open_act_preset_for().						*/
/************************************************************************/

static int v2_preset_equals(char *s, char *pat)
{
   char ch;

   while (*pat != '\0') {
      ch = islower(*s) ? toupper(*s++) : *s++;
      if (ch != *pat++)
         return FALSE;
   }
   return TRUE;
}

/************************************************************************/
/* Copy the OPEN_ACT or ERR_ACT string at src to dest.  The string at	*/
/* 'src' may not have a null terminator, so copy only as long as	*/
/* there's a valid OPEN_ACT character.  This is called only by		*/
/* open_act_preset_for() and format_preset_for().			*/
/************************************************************************/

static void v2_preset_get_act(char *src, char *dest)
{
   char ch;

   while (((ch=toupper(*src++)) == 'U') || (ch == 'S') || (ch == 'A'))
      *dest++ = ch;
   *dest = '\0';
}

/************************************************************************/
/* Copy the OPEN_MES or ERR_MESS string at src to dest, with maximum	*/
/* length maxlen.  The string at 'src' may not have a null terminator,	*/
/* so copy only as long as it's a printable character or white space,	*/
/* and the length is ok.  If we get 3 white space characters in a row,	*/
/* we rather arbitrarily assume it's the end of the string.  This is	*/
/* called only by open_act_preset_for() and format_preset_for().	*/
/************************************************************************/

static void v2_preset_get_mess(char *src, char *dest, int maxlen)
{
   int len = 0;
   int white = 0;

   while ((isprint(*src) || isspace(*src)) && len < maxlen) {
      if (isspace(*src)) {
         if (white++ > 2)
            break;			/* too much white space */
      }
      else
         white = 0;
      *dest++ = *src++;
      len++;
   }
   *dest = '\0';

}

/************************************************************************/
/* Copy the FORMAT string at src to dest.  The string at 'src' may not	*/
/* have a null terminator, so simply use v2_preset_equals() to check for*/
/* the valid FORMAT values.  This is called only by format_preset_for().*/
/************************************************************************/

static void v2_preset_get_format(char *src, char *dest)
{
   if (v2_preset_equals(src, "INT"))
      strcpy(dest, "INT");
   else if (v2_preset_equals(src, "REAL"))
      strcpy(dest, "REAL");
   else if (v2_preset_equals(src, "STRING"))
      strcpy(dest, "STRING");
   else if (v2_preset_equals(src, "DOUB"))
      strcpy(dest, "DOUB");
   else
      strcpy(dest, "XX");    /* will be caught by v2_label_format validation */
}
