#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/************************************************************************/
/* This routine takes the optional arguments and the associated tables,	*/
/* and fills in the tables with the values from the optionals keywords.	*/
/* All values either go into the appropriate place in the currval_table	*/
/* (input mode), or go from there into the user's variables (output	*/
/* mode).  All optionals are in C format, i.e. pass by value for input,	*/
/* and pass by reference for output and strings.  All strings are	*/
/* standard null-terminated C strings.					*/
/*									*/
/* NOTE: This routine should be maintained in parallel with		*/
/* process_optionals_for!!						*/
/************************************************************************/

int v2_process_optionals_c(
   int UNUSED(unit),			/* Unit number			*/
   struct UNIT_TABLE* opts_table, 	/* Table of valid optionals	*/
   int n_entries,			/* Num entries in opts_table	*/
   VALUE_TABLE* currval_table,		/* Current value table		*/
   VALUE_TABLE *def_table,		/* Default value table		*/
   int nopts,				/* # optional args or -1 if unk	*/
   va_list *params			/* optional arguments pointer	*/
)

{
   int i, status;
   int optno;
   int found_match;
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

         /* Search the opts_table for this keyword */

         if (strlen(s) > PARAM_NAME_LENGTH)
            return UNDEFINED_OPTIONAL;

         v2_make_upper_case(keyword, s);

         found_match = FALSE;
         for (i=0; i<n_entries; i++) {
            if (opts_table[i].access & current_access) {
               if (EQUAL(keyword,opts_table[i].name)) {
                  found_match = TRUE;
                  break;
               }
            }
         }
      
         if (!found_match)
            return UNDEFINED_OPTIONAL;

         /* If this is an INPUT keyword, or a SYSTEM keyword during an Open */
         /* or Add call, then it's an input value.  Copy the value from the */
         /* parameter to the currval_table.				 */

         if ((opts_table[i].mode == INPUT) ||
            ((opts_table[i].mode == SYSTEM) && (current_access & (O|A)))) {

            /* Get the value for the keyword */

            if (opts_table[i].type == STRING || opts_table[i].type == MESSAGE)
               value.s = va_arg(*params, char *);
            else
               value.i = va_arg(*params, int);
            optno++;

            /* If a validation routine is available, call to check the value */

            if (opts_table[i].validation != NULL) {
               status = (*opts_table[i].validation)(value);
               if (status != SUCCESS)
                  return status;
            }

            /* Now, actually put the value in the table */

            if (opts_table[i].type == STRING) {
               status = v2_add_str_current_table(value.s, i, currval_table,
						    def_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else if (opts_table[i].type == MESSAGE) {
               status = v2_add_msg_current_table(value.s, i, currval_table,
						     def_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else		/* type == INTEGER or other 4-byte value */
               currval_table[i].ivalue = value.i;

         }		/* end INPUT mode */

      /* If it's not an input value, it must be an output value.  Copy the  */
      /* value from the currval_table to the user's buffer.  Note that	    */
      /* everything is passed by reference here, since the values are	    */
      /* going out.  Type==INTADDR is a special case: we just save the	    */
      /* address the user passes in, and the top-level routine will fill    */
      /* it in later.  This is because the value is not known at this time. */

         else {
            if (opts_table[i].type == STRING || opts_table[i].type == MESSAGE) {
               value.s = va_arg(*params, char *);
               optno++;
               strncpy(value.s, currval_table[i].pvalue, opts_table[i].size);
               if (strlen(currval_table[i].pvalue) >= opts_table[i].size)
                  *(value.s+opts_table[i].size-1)='\0'; /*guarantee terminator*/
            }
            else {
               value.ip = va_arg(*params, int *);
               optno++;
               if (opts_table[i].type == INTADDR)
                  currval_table[i].ipvalue = value.ip;
               else					/* type == INTEGER */
                  *value.ip = currval_table[i].ivalue;
            }
         }		/* end OUTPUT mode */
      }
   } while (s != NULL && s[0] != '\0');		/* end main optionals loop */

   if ((optno != nopts) && (nopts >= 0))
      return ODD_NUMBER_OF_OPTIONALS;

   return SUCCESS;
}


/************************************************************************/
/* This routine takes the optional arguments and the associated tables,	*/
/* and fills in the tables with the values from the optionals keywords.	*/
/* All values either go into the appropriate place in the currval_table	*/
/* (input mode), or go from there into the user's variables (output	*/
/* mode).  All optionals are in Fortran format, i.e. pass by reference	*/
/* at all times.  Strings may be either Fortran type or C type strings,	*/
/* although Fortran strings are preferred, as C routines should use the	*/
/* C entry points.							*/
/*									*/
/* NOTE: This routine should be maintained in parallel with		*/
/* process_optionals_c!!						*/
/************************************************************************/

int v2_process_optionals_for(
   int UNUSED(unit),			/* Unit number			*/
   struct UNIT_TABLE* opts_table,	/* Table of valid optionals	*/
   int n_entries,			/* # entries in opts_table	*/
   VALUE_TABLE* currval_table,		/* Current value table		*/
   VALUE_TABLE* def_table,		/* Default value table		*/
   int nopts,				/* # optional args or -1 if unk	*/
   va_list *params,			/* optional arguments pointer	*/
   char *argptr,			/* ptr to first actual arg	*/
   int nargs,				/* total # of arguments		*/
   int argno,				/* arg # of last constant arg	*/
   int strno,				/* string # of last constant arg*/
   va_list *str_params,			/* ptr to string lengths	*/
   int *str_which			/* "which" for sfor2c		*/
)

{
   int i, status;
   int optno;
   int found_match;
   char *forstr;
   char s[MAX_STRING_SIZE+1];
   char tempstr[MAX_STRING_SIZE+1];
   char keyword[PARAM_NAME_LENGTH+1];
   VALUE_TYPE value;

   if (nopts == 0)			/* no optionals and we know it */
      return SUCCESS;

   optno = 0;
   do {
      forstr = va_arg(*params, char *);		/* get the keyword */
      optno++;  argno++;  strno++;			/* as a C string */
      v2_sfor2c(s, PARAM_NAME_LENGTH, forstr, argptr, nargs, argno, strno,
				str_params, str_which);

      if (s[0] != ' ' && s[0] != '\0') {
         if (optno >= nopts && nopts >= 0) {
            return ODD_NUMBER_OF_OPTIONALS;	/* keyword with no value */
         }

         /* Search the opts_table for this keyword */

         v2_make_upper_case(keyword, s);

         found_match = FALSE;
         for (i=0; i<n_entries; i++) {
            if (opts_table[i].access & current_access) {
              if (EQUAL(keyword,opts_table[i].name)) {
                  found_match = TRUE;
                  break;
               }
            }
         }
      
         if (!found_match)
            return UNDEFINED_OPTIONAL;

         /* If this is an INPUT keyword, or a SYSTEM keyword during an Open */
         /* or Add call, then it's an input value.  Copy the value from the */
         /* parameter to the currval_table.				 */

         if ((opts_table[i].mode == INPUT) ||
            ((opts_table[i].mode == SYSTEM) && (current_access & (O|A)))) {

            /* Get the value for the keyword */

            if (opts_table[i].type == STRING || opts_table[i].type == MESSAGE) {
               value.s = va_arg(*params, char *);
               optno++;  argno++;  strno++;
               v2_sfor2c(tempstr, opts_table[i].size, value.s,
		      argptr, nargs, argno, strno, str_params, str_which);
               value.s = tempstr;
            }
            else {
               value.i = *((int *)va_arg(*params, int *));
               optno++;  argno++;
            }

            /* If a validation routine is available, call to check the value */

            if (opts_table[i].validation != NULL) {
               status = (*opts_table[i].validation)(value);
               if (status != SUCCESS)
                  return status;
            }

            /* Now, actually put the value in the table */

            if (opts_table[i].type == STRING) {
               status = v2_add_str_current_table(value.s, i, currval_table,
						    def_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else if (opts_table[i].type == MESSAGE) {
               status = v2_add_msg_current_table(value.s, i, currval_table,
						     def_table);
	       if (status != SUCCESS)
	          return UNABLE_TO_STORE_OPTIONAL;
            }
            else		/* type == INTEGER or other 4-byte value */
               currval_table[i].ivalue = value.i;

         }		/* end INPUT mode */

      /* If it's not an input value, it must be an output value.  Copy the */
      /* value from the currval_table to the user's buffer.  Type==INTADDR */
      /* is a special case: we just save the address the user passes in,   */
      /* and the top-level routine will fill it in later.  This is because */
      /* the value is not known at this time.				   */

         else {
            if (opts_table[i].type == STRING || opts_table[i].type == MESSAGE) {
               value.s = va_arg(*params, char *);
               optno++;  argno++;  strno++;
               v2_sc2for(currval_table[i].pvalue, 0, value.s,
		      argptr, nargs, argno, strno, str_params, str_which);
            }
            else {
               value.ip = va_arg(*params, int *);
               optno++;  argno++;
               if (opts_table[i].type == INTADDR)
                  currval_table[i].ipvalue = value.ip;
               else					/* type == INTEGER */
                  *value.ip = currval_table[i].ivalue;
            }
         }		/* end OUTPUT mode */
      }
		/* end main optionals loop */
   } while ((s[0] != ' ' && s[0] != '\0') && ((optno < nopts) || (nopts < 0)));

   if ((optno != nopts) && (nopts >= 0))
      return ODD_NUMBER_OF_OPTIONALS;

   return SUCCESS;
}

