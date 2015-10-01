#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"
#include <time.h>
#include <stdio.h>
#if CUSERID_AVAIL_OS == 0
#include <pwd.h>
#else
char *cuserid (char *s);
#endif

/* Get the user ID, date/time, and proc name for the history label */

void v2_collect_history_info(void)
{
   int status;
   int maxlen;
   char *value, *newline;
   time_t tim;
#if CUSERID_AVAIL_OS
#else
   struct passwd *pw;
#endif

   if (strlen(history[TASK].value) != 0)	/* Already collected */
      return;

   maxlen = sizeof(history[TASK].value);	/* max len of dest string */
   maxlen = MIN(maxlen, MAX_HIST_NAME_SIZE+1);	/* limit size for back compat */

   status = v2_get_one_string_parm("_PROC", 0, &value);	/* Get proc name */
   if (status == SUCCESS) {
      strncpy(history[TASK].value, value, maxlen);
      history[TASK].value[maxlen-1] = '\0';
   }
   else						/* Not there, use default */
      strcpy(history[TASK].value, TASK_KEY);
   v2_make_upper_case(history[TASK].value, history[TASK].value);

   maxlen = sizeof(history[USER].value);	/* max len of dest string */
#if CUSERID_AVAIL_OS
   value = cuserid(NULL);			/* Get user ID */
#else
   pw = getpwuid(getuid());
   value = pw->pw_name;
#endif
   strncpy(history[USER].value, value, maxlen);
   history[USER].value[maxlen-1] = '\0';

   maxlen = sizeof(history[DAT_TIM].value);	/* max len of dest string */
   tim = time(0);				/* Get date and time */
   value = ctime(&tim);
   strncpy(history[DAT_TIM].value, value, maxlen);
   history[DAT_TIM].value[maxlen-1] = '\0';
   newline = strchr(history[DAT_TIM].value, '\n'); /* Truncate @ trailing \n */
   if (newline != NULL)
      *newline = '\0';
}
