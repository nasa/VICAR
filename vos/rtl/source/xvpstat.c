#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This subroutine is used to return various information about	*/
/* a parameter without returning its value.			*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvpstat, XVPSTAT) (char *key, int *count, int *def, int *maxlen,
						char *type, ZFORSTR_PARAM)
/* char *key;		in: parameter name. */
/* int *count;		out: actual count of param. If 0, param not found */
/* int *def;		out: default flag.  =1 if defaulted */
/* int *maxlen;		out: max length of any single item (not incl term) */
/* char *type;		out: string indicating type of parameter */
{
   ZFORSTR_BLOCK
   int i;
   char c_key[MAX_STRING_SIZE+1], c_type[MAX_STRING_SIZE+1];

   current_call = VPSTAT;

   zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 5, 1, 1, type);

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   v2_get_pstat((struct PARBLK*) &parb, c_key, count, def, maxlen, c_type);

   zsfor2c(c_type, MAX_STRING_SIZE, type, &key, 5, 5, 2, type);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvpstat(
   char *key,		/* in: parameter name. */
   int *count,		/* out: actual count of param. If 0, param not found */
   int *def,		/* out: default flag.  =1 if defaulted */
   int *maxlen,		/* out: max length of any single item (not incl term) */
   char *type		/* out: string indicating type of parameter */
)

{

   current_call = VPSTAT;

   return v2_get_pstat((struct PARBLK*) &parb, key, count, def, maxlen, type);

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvipstat,XVIPSTAT) (char *key, int *count, int *def, int *maxlen,
						char *type, ZFORSTR_PARAM)
/* char *key;		in: parameter name. */
/* int *count;		out: actual count of param. If 0, param not found */
/* int *def;		out: default flag.  =1 if defaulted */
/* int *maxlen;		out: max length of any single item (not incl term) */
/* char *type;		out: string indicating type of parameter */
{
   ZFORSTR_BLOCK
   int i;
   char c_key[MAX_STRING_SIZE+1], c_type[MAX_STRING_SIZE+1];

   current_call = VPSTAT;

   zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 5, 1, 1, type);

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   v2_get_pstat((struct PARBLK*) &iparb, c_key, count, def, maxlen, c_type);

   zsfor2c(c_type, MAX_STRING_SIZE, type, &key, 5, 5, 2, type);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvipstat(
   char *key,		/* in: parameter name. */
   int *count,		/* out: actual count of param. If 0, param not found */
   int *def,		/* out: default flag.  =1 if defaulted */
   int *maxlen,		/* out: max length of any single item (not incl term) */
   char *type		/* out: string indicating type of parameter */
)

{

   current_call = VPSTAT;

   return v2_get_pstat((struct PARBLK*) &iparb, key, count, def, maxlen, type);

}
