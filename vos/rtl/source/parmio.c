#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#define PARM_BUFSIZE 512
#define NOUNIT	-1

/* Note: The error_handler() is called for all errors occuring in this	*/
/* module.  Callers need only check the status and return to the user.	*/

#define ABORT(x) {v2_error_handler(unit,x); return x;}
#define CHKSTATUS  {if (status != SUCCESS) return status;}

static char buf[PARM_BUFSIZE];
static int bufswritten;
static int bufoffset;
static int unused;
static int dirty;
static int valid;
static int unit = NOUNIT;

/************************************************************************/
/* status = parm_init(inunit);						*/
/*									*/
/* Initializes the parameter I/O routines.				*/
/************************************************************************/

int v2_parm_init(int inunit)
{

   if (unit != NOUNIT)
      ABORT(MULTIPLE_PARAMETER_FILES);

   bufswritten = 0;
   bufoffset = 0;
   unused = PARM_BUFSIZE;
   dirty = FALSE;
   valid = FALSE;
   unit = inunit;

   return SUCCESS;
}


/************************************************************************/
/* status = parm_write(addr, len);					*/
/*									*/
/* Writes a buffer of length 'len' pointed to by 'addr' to the file.	*/
/************************************************************************/

int v2_parm_write(char *addr, int len)
{
   int status;
   int bytesleft;
   int i, n;
   int useroffset;

   bytesleft = len;
   useroffset = 0;

   while (bytesleft > 0) {
      n = MIN(bytesleft, unused);
      if (n > 0) {
	 for (i=0; i<n; i++)
	    buf[bufoffset+i] = addr[useroffset+i];
	 useroffset += n;
	 bufoffset += n;
	 bytesleft -= n;
	 unused -= n;
	 dirty = TRUE;
      }

      if (bytesleft > 0) {	/* empty the buffer */
	 status = zvwrit(unit, buf, NULL);
	 CHKSTATUS;
	 dirty = FALSE;
	 bufswritten++;
	 bufoffset = 0;
	 unused = PARM_BUFSIZE;
	 for (i=0; i<PARM_BUFSIZE; i++)	/* unnecessary */
	    buf[i] = 0;
      }
   }

   return SUCCESS;
}

/************************************************************************/
/* status = parm_read(addr, len);					*/
/*									*/
/* Reads a buffer of length 'len' from the file into the location	*/
/* pointed to by 'addr'.						*/
/************************************************************************/

int v2_parm_read(char addr[], int len)
{
   int status;
   int bytesleft;
   int i, n;
   int useroffset;

   bytesleft = len;
   useroffset = 0;

   while (bytesleft > 0) {
      if (valid) {		/* use what's in the buffer */
	 n = MIN(bytesleft, unused);
	 if (n > 0) {
	    for (i=0; i<n; i++)
	       addr[useroffset+i] = buf[bufoffset+i];
	    useroffset += n;
	    bufoffset += n;
	    bytesleft -= n;
	    unused -= n;
	 }
      }

      if (bytesleft > 0) {	/* re-fill the buffer */
	 status = zvread(unit, buf, NULL);
	 CHKSTATUS;
	 valid = TRUE;
	 bufoffset = 0;
	 unused = PARM_BUFSIZE;
      }
   }

   return SUCCESS;
}


/************************************************************************/
/* status = parm_close();						*/
/*									*/
/* Flushes the buffer if there's unwritten data in it.			*/
/************************************************************************/

int v2_parm_close()
{
   int status;

   if (dirty) {
      status = zvwrit(unit, buf, NULL);
      CHKSTATUS;
      bufswritten++;
   }

   if (bufswritten > 1) { /* update NL if more than one buffer was written out*/
      status = zldel(unit, "system", "NL", NULL);
      CHKSTATUS;
      status = zladd(unit, "system", "NL", &bufswritten, "format", "int", NULL);
      CHKSTATUS;
   }

   unit = NOUNIT;

   return SUCCESS;
}

