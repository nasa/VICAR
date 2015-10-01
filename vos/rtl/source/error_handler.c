#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Handle an error by checking the appropriate error action and doing	*/
/* what it indicates: system message, user message, and/or abort.	*/

#define ABEND_FLAG 'A'		/* flags corresponding to XXX_ACT characters */
#define SYS_MSG 'S'
#define USER_MSG 'U'
#define LAB_DEF_CHAR 'D'	/* ERR_ACT defaulted, so use LAB_ACT */

#define OPEN_FLAG 0x800		/* flags corresponding to current_call bits */
#define IO_FLAG 0x400
#define ERR_FLAG 0x200
#define NO_UNIT_FLAG 0x100

				/* generic error, no unit associated with it */
#define GEN_ERR_ACT(x) \
 (((current_call&NO_UNIT_FLAG)!=0)&&(strchr(def_err_act,x)!=0))
#define OPEN_ERR_ACT(x) \
 (((current_call&OPEN_FLAG)!=0)&&(strchr(CURRENT_S_VALUE(OPEN_ACT),x)!=0))
#define IO_ERR_ACT(x) \
 (((current_call&IO_FLAG)!=0)&&(strchr(CURRENT_S_VALUE(IO_ACT),x)!=0))

#define LAB_ERR_ACT(x) \
 (LAB_DEF ? LAB_ACTION(x) : ERR_ACTION(x))
#define LAB_DEF \
(((current_call&ERR_FLAG)!=0)&&(strchr(LABEL_S_VALUE(ERR_ACT),LAB_DEF_CHAR)!=0))
#define LAB_ACTION(x) \
 (((current_call&ERR_FLAG)!=0)&&(strchr(CURRENT_S_VALUE(LAB_ACT),x)!=0))
#define ERR_ACTION(x) \
 (((current_call&ERR_FLAG)!=0)&&(strchr(LABEL_S_VALUE(ERR_ACT),x)!=0))


void v2_error_handler(int unit, int code)
{
   if (code == SUCCESS) return;		/* just in case */

   if (code == NO_SUCH_UNIT)		/* special case: tables not defined */
   {
      zvmessage("**! Undefined unit: call XVUNIT first", "VIC2-NOSCHUN");
      return;
   }

   /* first check for system message flag */
   if (GEN_ERR_ACT(SYS_MSG) ||
	OPEN_ERR_ACT(SYS_MSG) || 
	IO_ERR_ACT(SYS_MSG) ||
	LAB_ERR_ACT(SYS_MSG)) v2_sys_msg(unit, code);

   /* now check for user message flag */
   if (GEN_ERR_ACT(USER_MSG)) zvmessage(def_err_mess, " ");
   if (OPEN_ERR_ACT(USER_MSG)) zvmessage(CURRENT_S_VALUE(OPEN_MES), " ");
   if (IO_ERR_ACT(USER_MSG)) zvmessage(CURRENT_S_VALUE(IO_MESS), " ");
   if (LAB_ERR_ACT(USER_MSG))
   {
      if (LAB_DEF)
         zvmessage(CURRENT_S_VALUE(LAB_MESS), " ");
      else
         zvmessage(LABEL_S_VALUE(ERR_MESS), " ");
   }

   /* now check for abend flag */
   if (GEN_ERR_ACT(ABEND_FLAG) ||
	OPEN_ERR_ACT(ABEND_FLAG) || 
	IO_ERR_ACT(ABEND_FLAG) ||
	LAB_ERR_ACT(ABEND_FLAG))
   {
      if (code <= 0 || v2_error_code == SUCCESS)
         v2_error_code = 0;		/* VICAR error message */
      else
         v2_error_code = code;		/* VMS   error message */
      zabend();
   }

   return;
}
