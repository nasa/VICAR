#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "pgminc.inc"
#endif
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#if VMS_OS
#include "nargs_vms.h"
#endif

/* Prints an error message with optional message key.  Works either	*/
/* inside VICAR, in which case the message goes to the terminal and all	*/
/* log files, or standalone, in which case it goes only to the terminal.*/

/* We must be careful about which TAE routine we call.  If this is a C	*/
/* application, where we called z_init to start up, then we need to	*/
/* call m_msg().  If this is a Fortran application, which called	*/
/* xzinit() to start, we need to call p_mput().  We get this info from	*/
/* global variable applic_lang, which is set up by zv_rtl_init() and	*/
/* xvzinit().  Note that it is not a simple matter of calling m_msg	*/
/* from zvmessage and p_mput from xvmessage, because a C app may call a	*/
/* Fortran sub that calls xvmessage.  It must still use m_msg because	*/
/* that's how the package was initialized.				*/

#if VMS_OS
#pragma nostandard		/* turn off portability check on PUBLICDEF */
#endif

PUBLICREF int in_vicar;

#if VMS_OS
#pragma standard
#endif


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvmessage, XVMESSAGE) (char *msg, char *key, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_msg[250];
   char c_key[250];

   zsfor2c(c_msg, 249, msg, &msg, 2, 1, 1, key);

#if VMS_OS		/* must allow 'key' to be optional for VMS... grrr */
   if (n_args() > 1)	/* hopefully this is temporary only */
      zsfor2c(c_key, 249, key, &msg, 2, 2, 2, key);
   else
      c_key[0] = '\0';
#else
   zsfor2c(c_key, 249, key, &msg, 2, 2, 2, key);
#endif

   zvmessage(c_msg, c_key);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvmessage(char *msg, char *key)
{

#if RTL_USE_TAE
   if (in_vicar)				/* if in normal VICAR, use   */
   {						/* VICAR/TAE message logging */
      if (key == NULL)
         key = "";	/* just to be safe... TAE probably accepts NULL */
      if (strspn(key," ") == strlen(key))	/* nothing but blanks */
         key = "";
      if (applic_lang == C_LANG)
         m_msg(msg, key);
      else
         p_mput(msg, key);
   }
   else		/* if not in normal VICAR, just use a printf */
#endif
   {
      if ((key == NULL) || (strlen(key) == 0) || (key[0] == ' '))
	 printf("%s\n", msg);
      else
	 printf("[%s] %s\n", key, msg);
   }
   return;
}
