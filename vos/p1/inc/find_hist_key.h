#ifndef FIND_HIST_KEY_H
#define FIND_HIST_KEY_H

/* Prototype for find_hist_key */

int find_hist_key
(
int unit,               /* in: unit number */
char *key,              /* in: key to search for */
int lastflag,           /* in: TRUE=find last, FALSE=find first */
char *task,             /* out: task name where key found */
int *instance           /* out: instance number of task where key found */
);

#endif


