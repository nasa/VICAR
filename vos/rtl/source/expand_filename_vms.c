#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Given a pathname, expand it to a form acceptable to open().		*/
/* This involves translating + characters in the filename.  This is	*/
/* considerably simpler than the Unix version because open() (or RMS	*/
/* equivalent) can handle logical names, so they do not have to be	*/
/* expanded.								*/
/*									*/
/* Allowable forms:							*/
/*	+			Expand to temporary filename		*/
/*									*/
/* A pointer is returned to the resultant pathname, which is stored in	*/
/* a static data area.  It must be used immediately or copied before	*/
/* this routine is called!						*/
/*									*/
/* If "makedir' is TRUE, and the "+" form is used to refer to the	*/
/* temporary directory, then the temp dir will be created if needed.	*/
/* Errors such as invalid usernames or undefined environment variables	*/
/* are indicated in the "status" return.  The function returns a pointer*/
/* to the offending variable or user name.				*/
/*									*/
/* Temporary filenames in VMS start with "vtmp:", which is a rooted	*/
/* directory logical name.  If there is a "[" in the user's given name,	*/
/* e.g. "+[xyz]file.dat", then only "vtmp:" is prepended, giving	*/
/* "vtmp:[xyz]file.dat".  This allows the user to create subdirectories.*/
/* If there is no "[", e.g. "+file.dat", then "vtmp:[000000]" is	*/
/* prepended due to weird VMS syntax rules, giving			*/
/* "vtmp:[000000]file.dat", which works out to be the directory that	*/
/* vtmp points to.							*/
/************************************************************************/

char *v2_expand_filename(inpath, makedir, status)
char *inpath;
int makedir;
int *status;
{
   static char outpath[2048];		/* big enough for anything! */

   *status = SUCCESS;

   strcpy(outpath, "");

   if (*inpath == '+') {		/* Temporary file prefix */
      inpath++;				/* skip the '+' */
      strcat(outpath, "vtmp:");			/* Prepend vtmp: logical */
      if (strchr(inpath, '[') == NULL)
         strcat(outpath, "[000000]");		/* No [, so add [000000] */

      if (makedir) {			/* Create the temp directory */
         mkdir("vtmp:[000000]", 0777);		/* All access */
         /* Ignore errors here since they'll be caught by the open call */
      }
   }

   strcat(outpath, inpath);		/* Copy over the filename */

   *status = SUCCESS;
   return outpath;
}

