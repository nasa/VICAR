/************************************************************************/
/* Shell-VICAR routine to take the output parblk (a la q_out()) and	*/
/* write its contents to a file for later retrieval.			*/
/************************************************************************/

#include "taeconf.inp"
#include "tae_lib.h"
#include "parblk.inc"
#include "zvproto.h"
#include <string.h>
#include "taeextproto.h"

int zzq_out(struct PARBLK *parblk)
{
   struct VARIABLE *v, *v_loop;
   char outbuf[STRINGSIZ+1];
   int i;
   CODE code;
   int endcol = STRINGSIZ;
   char *output_filename;
   FILE *fp;

   output_filename = v2param_get_file();
   if (output_filename == NULL) {
      fprintf(stderr,
	"Unable to find filename for output parameters.  Check $V2PARAM_FILE.\n");
      return FAIL;
   }
   v2param_remove_file(output_filename);	/* just in case... */

   fp = fopen(output_filename, "w");
   if (fp == NULL) {
      fprintf(stderr, "Unable to open output parameter file '%s'\n",
		output_filename);
      return FAIL;
   }

   for (v_loop = parblk->symtab.link; v_loop != NULL; v_loop = v_loop->v_link) {
      v = RESOLVE(v_loop);
      if (v == NULL)
         continue;		/* skip this one */

      sprintf(outbuf, "%s = ", v->v_name);

      for (i = 0; i < v->v_count; i++) {
	code = m_fpval(v, i, outbuf, endcol, 
		       (endcol > STRINGSIZ ? endcol : STRINGSIZ));
         if (code == FAIL) {		/* if it won't fit */
            fprintf(fp, "%s\\\n", outbuf);
            strcpy(outbuf, "");
            i--;			/* do this one over */
         }
      }
      if (v->v_count > 0)
         outbuf[strlen(outbuf)-2] = '\0';	/* delete last , */
      else if (v->v_count == 0)
         strcat(outbuf, "--");
      else
         strcat(outbuf, "NULL");
      fprintf(fp, "%s\n", outbuf);

   }

   fclose(fp);

   return SUCCESS;
}

