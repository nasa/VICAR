#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/
/* Allocates and returns a pointer to *all* the elements for this label	*/
/* item in *newvalue.  The inner dimension of *newvalue is returned in	*/
/* *internal_length.  It is the caller's responsibility to copy the	*/
/* values from here to the user's buffer, and to deallocate *newvalue.	*/
/* *start_element and *u_nelements return the starting element # and	*/
/* the # of elements to copy from *newvalue.  *outformat gets set to a	*/
/* string describing the data format (STRING, INT, etc.).  It is	*/
/* statically allocated in this routine, so use it or copy it before	*/
/* you call this routine again!						*/
/************************************************************************/

int c_xlget(
   int unit,
   char *type,
   char *key,
   char **newvalue,		/* out: internal buffer for all elements */
				/*   (allocated by this routine; free it!) */
   int *internal_length,	/* out: len of C array elements in newvalue */
   int *start_element,		/* out: starting element user wants */
   int *u_nelements,		/* out: # of elements user wants */
   char **outformat		/* out: pointer to format string */
)

{
   int status, dummy;
   char *p;
   char *start, *stop;
   char *value;
   int vallen;
   int level;
   int nelements;		/* # of elements in label */
   int maxlength;		/* max length of items actually in label */
   static char format[MAX_SHORT_STRING_SIZE+1];

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      return TOO_LATE;

   if (CURRENT_I_VALUE(FLAGS) & NO_LABELS)
      return FILE_HAS_NO_LABEL;

   *outformat = format;

   /* Before label processing occurs, the labels must be read into local mem */

   if (CURRENT_S_VALUE(LABELS) == NULL) {
      status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;
      CURRENT_S_VALUE(LABELS) = p;
   }

   /* Find desired label portion... either a task in the history label,	*/
   /* or the system label.  "Start" and "stop" end up pointing at the	*/
   /* label substring containing the desired portion.			*/

   if (EQUAL(type, "HISTORY")) {		/* History label */
      status = v2_find_task(unit, LABEL_S_VALUE(HIST), LABEL_I_VALUE(INSTANCE),
			 &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type,"PROPERTY")) {		/* Property label */
      if (strlen(LABEL_S_VALUE(PROPERTY)) == 0)	/* Must have a property name */
         return PROPERTY_OPTIONAL_REQUIRED;
      status = v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
			LABEL_I_VALUE(INSTANCE), &start, &stop, NULL);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "SYSTEM")) {		/* System label */
      status = v2_find_system(unit, &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else return BAD_LABEL_TYPE;	/* Neither HISTORY nor SYSTEM nor PROPERTY */

   status = v2_find_key(&start, &stop, key, &value, &vallen);
   if (status != SUCCESS)
      return CANNOT_FIND_KEY;

   strcpy(format, LABEL_VALUE(LFORMAT));	/* may not be given */

   status = v2_evaluate_label_item(value, vallen, format, &maxlength,
		&nelements, &level, &dummy);
   if (status != SUCCESS)
      return status;

   *internal_length = maxlength;
   if (EQUAL(format, "STRING"))
      (*internal_length)++;		/* leave room for null terminator */

   *newvalue = malloc(*internal_length * nelements);	/* Allocate buffer */
   if (*newvalue == NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;

   status = v2_obtain_label_values(value, vallen, *newvalue, nelements,
		*internal_length, format);
   if (status != SUCCESS)
      return status;

   /* Figure out start_element and u_nelements */

   *start_element = LABEL_I_VALUE(ELEMENT) - 1;
   if (LABEL_I_VALUE(ELEMENT) == 0)	/* defaulted */
      *start_element = 0;
   if (LABEL_I_VALUE(ELEMENT) == -1)	/* special value */
      *start_element = nelements;

   *u_nelements = LABEL_I_VALUE(NELEMENTS);
   if (LABEL_I_VALUE(NELEMENTS) == 0)	/* defaulted */
      *u_nelements = 1;
   if (LABEL_I_VALUE(NELEMENTS) == -1)	/* special value */
      *u_nelements = nelements;

   if (*start_element + *u_nelements > nelements)  /* Adjust to the number */
      *u_nelements = nelements - *start_element;   /* actually present.    */
   if (*u_nelements < 0)
      *u_nelements = 0;

   /* Return any optionals requested */

   if (LABEL_IP_VALUE(NRET) != NULL)
      *LABEL_IP_VALUE(NRET) = *u_nelements;

   if (LABEL_IP_VALUE(LENGTH) != 0)
      *LABEL_IP_VALUE(LENGTH) = maxlength;

   if (LABEL_IP_VALUE(LEVEL) != 0)
      *LABEL_IP_VALUE(LEVEL) = level;

   /* Save ptr to just after this item; used by XLNINFO to find "next" item */
   LABEL_S_VALUE(POSITION) = stop;

   return SUCCESS;
}
