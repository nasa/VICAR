#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include <string.h>
#include <stdio.h>

/* Returns old Vicar 1 72-byte labels in a buffer.  The buffer is a	*/
/* single string with each item 72 bytes long.  There is no separator	*/
/* between the labels.							*/
/* NOTE:  For vic1lab, the buffer is a Fortran character*n variable.	*/
/* For vic1labx, the buffer is not.					*/
/* For zvic1lab, the buffer may not be null terminated.			*/

/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2(vic1lab, VIC1LAB) (int *unit, int *status, int *nlabs, char *buf,
						int *maxlabs, ZFORSTR_PARAM)
#if 0
int *unit;			/* In: unit number */
int *status;			/* Out: status value */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: character*n buffer to contain labels */
int *maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for string size max) */
#endif
{
   ZFORSTR_BLOCK
   int len, max;

   max = *maxlabs;
   zsfor2len(len, buf, &unit, 5, 4, 1, maxlabs);
   if (*maxlabs == 0 || len < *maxlabs*72)
      max = len/72;

   memset(zsfor2ptr(buf), ' ', len);
   *status = zvic1lab(*unit, nlabs, zsfor2ptr(buf), max);
   return;
}

void FTN_NAME2(vic1labx, VIC1LABX) (unit, status, nlabs, buf, maxlabs)
int *unit;			/* In: unit number */
int *status;			/* Out: status value */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: byte buffer to contain labels */
int *maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for no max) */
{
   *status = zvic1lab(*unit, nlabs, buf, *maxlabs);
   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvic1lab(unit, nlabs, buf, maxlabs)
int unit;			/* In: unit number */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: buffer to contain labels */
int maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for no max) */
{
   int status, i;
   char task[16];
   int instance, ninst;
   char key[10];
   char label[256];

/* Get the name of the first task */

   ninst = 1;
   status = zlhinfo(unit, task, &instance, &ninst, NULL);
   if (status != SUCCESS)
      return status;

/* Get the number of Vicar1 labels present */

   status = zlget(unit, "HISTORY", "NLABS", nlabs, "HIST", task,
			"INSTANCE", 1, NULL);
   if (status != SUCCESS) {
      *nlabs = 0;	/* Assume if error that no Vicar1 labels are present */
      return SUCCESS;
   }

   if (maxlabs != 0 && maxlabs < *nlabs)	/* check for max # of labels */
      *nlabs = maxlabs;

/* Now get NLABS 72 byte labels */

   for (i = 0; i < *nlabs; i++) {
      sprintf(key, "LAB%02d", i+1);
      status = zlget(unit, "HISTORY", key, label, "HIST", task,
				"INSTANCE", 1, NULL);
      strncpy(buf+i*72, label, 72);
      if (status != SUCCESS) {
         *nlabs = i;
         return status;
      }
   }

   return SUCCESS;
}
