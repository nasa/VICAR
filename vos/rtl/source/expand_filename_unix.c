#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <pwd.h>
#include <sys/stat.h>

/************************************************************************/
/* Given a pathname, expand it to a form acceptable to open().		*/
/* This involves translating $, ~, and + characters in the filename.	*/
/* Allowable forms:							*/
/*	$var or ${var}		Expand environment variable "var"	*/
/*	~user			Expand to home directory of user "user"	*/
/*	~			Expand to home dir of current user ($HOME) */
/*	+			Expand to "$VTMP/" for temporary files	*/
/*      $$			Insert a single $ (no env var)		*/
/*									*/
/* A pointer is returned to the resultant pathname, which is stored in	*/
/* a static data area.  It must be used immediately or copied before	*/
/* this routine is called!						*/
/* If "makedir' is TRUE, and the "+" form is used to refer to the	*/
/* temporary directory, then the temp dir will be created if needed.	*/
/* Errors such as invalid usernames or undefined environment variables	*/
/* are indicated in the "status" return.  The function returns a pointer*/
/* to the offending variable or user name.				*/
/************************************************************************/

char *v2_expand_filename(char *inpath, int makedir, int *status)
{

#if !X86_NT_ARCH			/* for real Unixes... */

   static char outpath[2048];		/* big enough for anything! */
   static char name[MAX_FILE_NAME_SIZE+1];
   char *env;
   struct passwd *pw;
   int len;

   *status = SUCCESS;

   strcpy(outpath, "");

   if (*inpath == '~') {		/* User's home directory */
      inpath++;				/* skip the '~' */
      len = strcspn(inpath, "/");	/* Find the slash to end the name */
      strncpy(name, inpath, len);
      name[len] = '\0';
      if (len == 0) {			/* No name, use $HOME */
         env = getenv("HOME");
         if (env == NULL) {		/* HOME not there??? */
            *status = UNDEF_ENV_VAR;
            strcpy(name, "HOME");
            return name;
         }
         strcat(outpath, env);
      }
      else {				/* Name given, get the home dir */
         pw = getpwnam(name);
         if (pw == NULL) {		/* Name not present */
            *status = UNDEF_USER_NAME;
            return name;
         }
         strcat(outpath, pw->pw_dir);
      }
      inpath += len;			/* Skip the name, leave the '/' */
   }

   if (*inpath == '+') {		/* Temporary file prefix */
      inpath++;				/* skip the '+' */
      env = getenv("VTMP");
      if (env == NULL)			/* not defined */
         strcat(outpath, "/tmp/");
      else {
         strcat(outpath, env);
         strcat(outpath, "/");
         if (makedir) {			/* Create the temp directory */
            mkdir(env, 0777);		/* All access (modified by umask) */
	    /* Ignore errors here since they'll be caught by the open() call */
         }
      }
   }

   /* Search for environment variables in the rest of the string */

   while ((len = strcspn(inpath, "$")) != (int) strlen(inpath)) {	/* Find '$'s */

      strncat(outpath, inpath, len);		/* Copy all up to '$' to out */
      inpath += len;
      inpath++;					/* Skip the '$' */

      if (*inpath == '$') {		/* Doubled '$'s mean a single $ */
         strcat(outpath, "$");
         inpath++;
         continue;			/* Next loop iteration */
      }

      if (*inpath == '{') {		/* enclosed in {} */
         inpath++;			/* skip '{' */
         len = strcspn(inpath, "}");	/* Find the '}' to end the name */
         strncpy(name, inpath, len);
         name[len] = '\0';
         if (len == (int) strlen(inpath)) {	/* No '}' */
            *status = UNCLOSED_BRACES;
            return name;
         }
         inpath += len;
         inpath++;			/* skip the '}' */
      }
      else {				/* no {}, end name at slash */
         len = strcspn(inpath, "/");
         strncpy(name, inpath, len);
         name[len] = '\0';
         inpath += len;
      }

      env = getenv(name);
      if (env == NULL) {			/* oops! */
         *status = UNDEF_ENV_VAR;
         return name;
      }
      strcat(outpath, env);
   }

   strcat(outpath, inpath);		/* Copy any remainder */

   *status = SUCCESS;
   return outpath;

#else

/* Win NT - this routine is essentially a no-op.  It should be replaced	*/
/* by something "real" for the NT filename conventions at some point.	*/

   static char outpath[2048];		/* big enough for anything! */
   *status = SUCCESS;
   strcpy(outpath, inpath);
   return outpath;

#endif

}

