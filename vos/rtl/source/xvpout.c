#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

#define ABORT(x) {v2_error_handler(parm_file_unit,x); return x;}

/************************************************************************/
/* Write a parameter out to the file opened by xvpopen().		*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvpout, XVPOUT) (int *status, char *parm_name, char *parm_value,
		char *format, int *count, ZFORSTR_PARAM)
/* int *status;		Out: return status				*/
/* char *parm_name;	In: name of parameter to be stored		*/
/* char *parm_value;	In: value of parameter to be stored		*/
/* char *format;	In: data format				*/
/* int *count;		In: TCL count of parm			*/
{
   ZFORSTR_BLOCK
   char *pformat, c_format[MAX_SHORT_STRING_SIZE+1];
   char c_name[NAMESIZE+1];
   char *value;
   int len;
   int string = FALSE;

   current_call = VPOUT;

   pformat = zsfor2ptr(format);
   v2_make_upper_case_max(c_format, pformat, MAX_SHORT_STRING_SIZE);

   len = 0;

   zsfor2c(c_name, NAMESIZE, parm_name, &status, 5, 2, 1, count);
   if (strncmp(c_format, "STRING", strlen("STRING")) == 0) {	/* string */
      string = TRUE;
      value = parm_value;
      zsfor2c_array(&value, &len, *count, value, status, 5, 3, 2, count);
      if (*status != SUCCESS) {
         v2_error_handler(parm_file_unit, *status);
         return;
      }
      zsfor2c(c_format, MAX_SHORT_STRING_SIZE, format, &status, 5, 4, 3, count);
   }
   else {						/* not a string */
      value = parm_value;
      zsfor2c(c_format, MAX_SHORT_STRING_SIZE, format, &status, 5, 4, 2, count);
   }

   *status = zvpout(c_name, value, c_format, *count, len);

   if (string)
      free(value);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvpout(
   char *parm_name,	/* In: name of parameter to be stored		*/
   void *parm_value,	/* In: value of parameter to be stored		*/
   char *parm_format,	/* In: data format				*/
   int count,		/* In: TCL count of parm			*/
   int length		/* In: length of each string in parm_value if	*/
			/*     multivalued string, or 0.		*/
)

{
   struct PARM_LINE header;
   char format[MAX_SHORT_STRING_SIZE+1];
   char name[NAMESIZE+1];
   int i, status;
   char *p;

   current_call = VPOUT;

   v2_make_upper_case_max(format, parm_format, MAX_SHORT_STRING_SIZE);
   v2_make_upper_case_max(name, parm_name, NAMESIZE);

   /* Get data type and size */

   if (EQUAL(format, "INT")) {
      header.p_type = PARM_V_INTEGER;
      header.p_size = sizeof(int);
   }
   else if (EQUAL(format, "REAL8") || EQUAL(format, "DOUB")) {
      header.p_type = PARM_V_REAL8;
      header.p_size = sizeof(double);
   }
   else if (EQUAL(format, "REAL")) {
      header.p_type = PARM_V_REAL;
      header.p_size = sizeof(float);
   }
   else if (EQUAL(format, "STRING")) {
      header.p_type = PARM_V_STRING;
      header.p_size = length;
      if (length == 0) {
         if (count == 1)
            header.p_size = strlen(parm_value)+1;
         else
            ABORT(IMPROPER_LENGTH);
      }
   }
   else
      ABORT(IMPROPER_FORMAT_STRING);

   /* Fill in the rest of the header */

   header.version = 0;
   header.name_len = strlen(name) + 1;
   header.p_count = count;

   if (header.p_type != PARM_V_STRING)
      header.val_len = header.p_count * header.p_size;
   else {
      header.val_len = 0;		/* Get total length of strings */
      p = parm_value;
      for (i=0; i<header.p_count; i++) {
         header.val_len += strlen(p)+1;
         p += header.p_size;
      }
   }

   /* Write out the file line structure */

   status = v2_parm_write((char *) &header, sizeof(header));
   if (status != SUCCESS)
      return status;

   status = v2_parm_write(name, header.name_len);
   if (status != SUCCESS)
      return status;

   /* Now write out the actual values */

   if (header.p_type != PARM_V_STRING) {
      status = v2_parm_write(parm_value, header.val_len);
      if (status != SUCCESS)
         return status;
   }
   else {		/* Write out strings concatenated with 0's in between */
      p = parm_value;
      for (i=0; i<header.p_count; i++) {
         status = v2_parm_write(p, strlen(p)+1);
         if (status != SUCCESS)
            return status;
         p += header.p_size;
      }
   }

   return SUCCESS;
}
