#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Get the entire label into a buffer.  This routine is not recommended	*/
/* for general use.							*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xlgetlabel, XLGETLABEL) (int *unitp, char *buf, int *bufsize,
		int *status, ZFORSTR_PARAM)
/* int *unitp;		In: Unit number */
/* char *buf;		Out: Buffer for label */
/* int *bufsize;	In/out: Size of buffer/size of label returned */
/* int *status;		Out: Status return */
{
   ZFORSTR_BLOCK
   int unit;
   char *p;

   unit = *unitp;

   if (first_call)
      v2_general_initialize();

   current_call = LGETLABEL;

   if (v2_valid_unit(unit) != SUCCESS) {
      *status = NO_SUCH_UNIT;
      v2_error_handler(unit, *status);
      return;
   }

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN)) {
      *status = TOO_LATE;
      v2_error_handler(unit, *status);
      return;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      *status = FILE_NOT_OPEN;
      v2_error_handler(unit, *status);
      return;
   }

   if (CURRENT_I_VALUE(FLAGS) & NO_LABELS) {
      *status = FILE_HAS_NO_LABEL;
      v2_error_handler(unit, *status);
      return;
   }

/* Before label processing occurs, the labels must be read into local mem */

   if (CURRENT_S_VALUE(LABELS) == NULL) {
      *status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (*status != SUCCESS) {
         v2_error_handler(unit, *status);
         return;
      }
      CURRENT_S_VALUE(LABELS) = p;
   }

/* Copy over the label buffer if *bufsize != 0 */

   if (*bufsize == 0)
      *bufsize = strlen(CURRENT_S_VALUE(LABELS));
   else {
      zsc2for(CURRENT_S_VALUE(LABELS), *bufsize, buf, &unitp, 4, 2, 1, status);
      *bufsize = MIN(*bufsize, (int) strlen(CURRENT_S_VALUE(LABELS)));
   }

   *status = SUCCESS;

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zlgetlabel(
   int unit,		/* In: Unit number */
   char *buf,		/* Out: Buffer for label */
   int *bufsize		/* In/out: Size of buffer/size of label returned */
)

{
   int status;
   char *p;

   if (first_call)
      v2_general_initialize();

   current_call = LGETLABEL;

   if (v2_valid_unit(unit) != SUCCESS) {
      status = NO_SUCH_UNIT;
      v2_error_handler(unit, status);
      return status;
   }

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN)) {
      status = TOO_LATE;
      v2_error_handler(unit, status);
      return status;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      status = FILE_NOT_OPEN;
      v2_error_handler(unit, status);
      return status;
   }

   if (CURRENT_I_VALUE(FLAGS) & NO_LABELS) {
      status = FILE_HAS_NO_LABEL;
      v2_error_handler(unit, status);
      return status;
   }

/* Before label processing occurs, the labels must be read into local mem */

   if (CURRENT_S_VALUE(LABELS) == NULL) {
      status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS) {
         v2_error_handler(unit, status);
         return status;
      }
      CURRENT_S_VALUE(LABELS) = p;
   }

/* Copy over the label buffer if *bufsize != 0 */

   if (*bufsize == 0)
      *bufsize = strlen(CURRENT_S_VALUE(LABELS));
   else {
      strncpy(buf, CURRENT_S_VALUE(LABELS), *bufsize);
      *(buf + *bufsize - 1) = '\0';
      *bufsize = strlen(buf);
   }

   return SUCCESS;
}
