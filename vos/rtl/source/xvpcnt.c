#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "symtab.inc"
#endif
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/* This subroutine is used to return the count of a given	*/
/* parameter without returning its value.			*/

#if RTL_USE_TAE


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

#if VMS_OS
void FTN_NAME2(xvpcnt, XVPCNT) (char *key, int *count, int *def, ZFORSTR_PARAM)
/* int *def;	out: Default flag ***VMS OLD COMPAT ONLY!!  DON'T USE!!*** */
#else
void FTN_NAME2(xvpcnt, XVPCNT) (char *key, int *count, ZFORSTR_PARAM)
#endif
/* char *key;		in: parameter name. */
/* int *count;		out: actual count of param. If 0, param not found */
{
   ZFORSTR_BLOCK
   int i;
   struct VARIABLE *v;
   char c_key[MAX_STRING_SIZE+1];

   current_call = VPCNT;

#if VMS_OS
   if (n_args() >= 3)
      zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 3, 1, 1, def);
   else
      zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 2, 1, 1, count);
#else
   zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 2, 1, 1, count);
#endif

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   v = p_fvar((struct PARBLK*) &parb, c_key); /* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return;
   }

   *count = v->v_count;
#if VMS_OS
   if (n_args() >= 3)
      *def = v->v_default;
#endif

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvpcnt(
   char *key,		/* in: parameter name. */
   int *count		/* out: actual count of param. If 0, param not found */
)
{
   struct VARIABLE *v;

   current_call = VPCNT;

   v = p_fvar((struct PARBLK*) &parb, key); /* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   *count = v->v_count;

   return SUCCESS;
}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

#if VMS_OS
void FTN_NAME2(xvipcnt,XVIPCNT) (char *key, int *count, int *def, ZFORSTR_PARAM)
/* int *def;	out: Default flag ***VMS OLD COMPAT ONLY!!  DON'T USE!!*** */
#else
void FTN_NAME2(xvipcnt, XVIPCNT) (char *key, int *count, ZFORSTR_PARAM)
#endif
/* char *key;		in: parameter name. */
/* int *count;		out: actual count of param. If 0, param not found */
{
   ZFORSTR_BLOCK
   int i;
   struct VARIABLE *v;
   char c_key[MAX_STRING_SIZE+1];

   current_call = VIPCNT;

#if VMS_OS
   if (n_args() >= 3)
      zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 3, 1, 1, def);
   else
      zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 2, 1, 1, count);
#else
   zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 2, 1, 1, count);
#endif

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   v = p_fvar((struct PARBLK*) &iparb, c_key); /* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return;
   }

   *count = v->v_count;
#if VMS_OS
   if (n_args() >= 3)
      *def = v->v_default;
#endif

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvipcnt(
   char *key,		/* in: parameter name. */
   int *count		/* out: actual count of param. If 0, param not found */
)
{
   struct VARIABLE *v;

   current_call = VIPCNT;

   v = p_fvar((struct PARBLK*) &iparb, key); /* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   *count = v->v_count;

   return SUCCESS;
}

#else

void FTN_NAME2(xvpcnt, XVPCNT) (void * UNUSED(key), int *count)
{
   current_call = VPCNT;
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return;
}

int zvpcnt(char * UNUSED(key), int *count)
{
   current_call = VPCNT;
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

void FTN_NAME2(xvipcnt, XVIPCNT) (void * UNUSED(key), int *count)
{
   current_call = VIPCNT;
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return;
}

int zvipcnt(char * UNUSED(key), int *count)
{
   current_call = VIPCNT;
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

