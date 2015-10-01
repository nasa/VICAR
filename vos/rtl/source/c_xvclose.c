#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvclose(int unit)
{
   int status;
   int free_unit;

#if RTL_USE_COMPRESSION
   if (COMPRESSED) {
      status = v2_compress_close(unit);
      return status;
   }
#endif

   /* If labels have been modified, check them to see if they need to be */
   /* rewritten to the image.						 */

   if ((EQUAL(CURRENT_S_VALUE(OP), "WRITE") ||
        EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))   &&
       (CURRENT_I_VALUE(FLAGS) & LABELS_MODIFIED) &&
       ! (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))) {

      status=v2_check_out_lbl_on_close(unit);
      if (status != SUCCESS)
         return status;
   }

   status = v2_close_file(unit);
   if (status != SUCCESS)
      return status;

   if (v2_substr(CURRENT_S_VALUE(CLOS_ACT),"DELETE"))
      v2_delete_file(unit);

   free_unit = v2_substr(CURRENT_S_VALUE(CLOS_ACT),"FREE"); /* Save for later */

   v2_close_unit(unit);		/* Clear current table and active unit table. */
   v2_final_cleanup(unit);	/* To clear up all details. */

   if (free_unit)
      v2_deactivate_a_unit(unit);

   return SUCCESS;
}
