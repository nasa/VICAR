#include "xvmaininc.h"
#include "errdefs.h"
#include "defines.h"
#include "zvproto.h"
#include <stdlib.h>
#include <string.h>

#define TASK_NAME_LENGTH 33		/* 32 chars +1 for null term */
#define NTASKS_GUESS 20			/* enough for most labels */

/************************************************************************/
/* find_hist_key							*/
/*									*/
/* Search the history label of the given file to find either the first	*/
/* or last occurrence of the label key in the label as a whole, and	*/
/* return the task name and instance number of where it was found.	*/
/* Returns SUCCESS if the key was found, or NO_SUCH_KEY if not.  Other	*/
/* VICAR errors may be returned as well, if they occur.  Note that	*/
/* the standard error actions are turned off for this routine, so it is	*/
/* the caller's responsibility to check the return code and call	*/
/* zvsignal() if needed.						*/
/*									*/
/* Written by: Bob Deen 7/12/95						*/
/************************************************************************/

int find_hist_key
(
    int unit,		/* in: unit number */
    char *key,		/* in: key to search for */
    int lastflag,	/* in: TRUE=find last, FALSE=find first */
    char *task,		/* out: task name where key found */
    int *instance	/* out: instance number of task where key found */
)
{
   char *tasks;
   int *instances;
   int ntasks, total_tasks;
   int status;
   int iter_count;
   char format_rtn[20];			/* dummies for zlinfo */
   int maxlen_rtn, nel_rtn;		/* ditto */
   int i, cur_task;

   /* Allocate a buffer for zlhinfo that will cover most cases.  If the	*/
   /* buffer isn't big enough, we'll expand it and try again.		*/

   total_tasks = NTASKS_GUESS;
   iter_count = 0;

   do {
      if (iter_count++ > 3)	/* Check for infinite loop */
         return INTERNAL_ERROR;	/* (it should only loop twice at most) */

      ntasks = total_tasks;
      tasks = (char *)malloc(ntasks * TASK_NAME_LENGTH);
      if (tasks == NULL)
         return INSUFFICIENT_MEMORY;
      instances = (int *)malloc(ntasks * sizeof(int));
      if (instances == NULL)
         return INSUFFICIENT_MEMORY;

      status = zlhinfo(unit, tasks, instances, &ntasks, "ERR_ACT", "",
		"NRET", &total_tasks, "ULEN", TASK_NAME_LENGTH, NULL);

      if (status != SUCCESS) {
         free(tasks);
         free(instances);
         return status;
      }

      /* Check to see if we got them all */

      if (total_tasks > ntasks) {		/* nope, recycle */
         free(tasks);
         free(instances);
      }
   } while (total_tasks > ntasks);

   /* Now we have the history task list, so search it to find the label */

   for (i=0; i<ntasks; i++) {
      if (lastflag)			/* search from the end */
         cur_task = ntasks - i - 1;
      else				/* search from the start */
         cur_task = i;

      status = zlinfo(unit, "history", key, format_rtn, &maxlen_rtn, &nel_rtn,
		"ERR_ACT", "", "HIST", tasks + (cur_task*TASK_NAME_LENGTH),
		"INSTANCE", instances[cur_task], NULL);

      if (status == SUCCESS) {		/* Found it! */
         strcpy(task, tasks + (cur_task * TASK_NAME_LENGTH));
         *instance = instances[cur_task];
         free(tasks);
         free(instances);

         return SUCCESS;
      }
   }

   /* Key not found in label */

   free(tasks);
   free(instances);

   return NO_SUCH_KEY;

}

