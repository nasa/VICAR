#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Determine the starting and stopping point for the history label	*/
/* determined by 'task' and 'instance'.  If strlen(task) == 0, then the	*/
/* last task in the label is used (the "current task").  Note that only	*/
/* the first MAX_HIST_NAME_SIZE (8) characters of the task name are	*/
/* significant.								*/

int v2_find_task(
   int unit,			/* in: unit number */
   char *task,			/* in: task name (or "" for current task) */
   int instance,		/* in: instance # of task */
   char **start,		/* out: pointer to start of task */
   char **stop			/* out: pointer to (just past) end of task */
)

{
   char *p;
   char *pstart, *pstop;
   char *value;
   int vallen;
   int found;
   int label_instance;

   p = CURRENT_S_VALUE(LABELS);

   found = FALSE;
   label_instance = 1;

   *start = NULL;

   while (!found) {
      pstop = v2_find_entry(p, TASK_KEY, &value, &vallen, &pstart);
      if (pstop == NULL)
         break;			/* only way out of loop if strlen(task)==0 */
      if ((strncmp(v2_dequoted(value,vallen), task, MAX_HIST_NAME_SIZE) == 0) &&
          (strlen(task) != 0)) {
         if (instance == label_instance) {	/* found task & instance! */
            *start = pstart;
            found = TRUE;
         }
         label_instance++;
      }
      p = pstop;		/* look for next one */
   }

   if (strlen(task) == 0)		/* no task given */
      *start = p;			/* so use last valid task */

   if (*start == NULL)
      return NO_SUCH_TASK;

   if (*start == CURRENT_S_VALUE(LABELS))
      return NO_TASKS_IN_LABEL;		/* can only happen if task=="" */

   pstop = v2_find_entry(p, TASK_KEY, &value, &vallen, &pstart);
   *stop = pstart;		/* find end of task (==beginning of next) */

   return SUCCESS;
}
