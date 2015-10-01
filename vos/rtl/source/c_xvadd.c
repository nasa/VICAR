#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvadd(int unit)
{

   if (v2_substr(CURRENT_S_VALUE(COND),"NOLABELS")) /* No label processing */
      CURRENT_I_VALUE(FLAGS) |= NO_LABELS;

   if (v2_substr(CURRENT_S_VALUE(COND),"NOBLOCK"))  /* Tapes won't be blocked */
      CURRENT_I_VALUE(FLAGS) |= NOBLOCK;

   if (v2_substr(CURRENT_S_VALUE(COND),"BINARY")) /* Access binary area of img*/
      CURRENT_I_VALUE(FLAGS) |= BINARY;

   if (v2_substr(CURRENT_S_VALUE(COND),"VARREC")) { /*Tape w/variable len recs*/
      if ((CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
          (CURRENT_I_VALUE(FLAGS) & NOBLOCK))
         CURRENT_I_VALUE(FLAGS) |= VARREC;
      else
         return VARREC_ERROR;
   }

   /* If one of the host type labels is given as "NATIVE" or "LOCAL", */
   /* replace it with the correct type for the native machine.        */

   if (EQUAL(CURRENT_S_VALUE(HOST),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(HOST),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,HOST,NATIVE_HOST_LABEL);
   if (EQUAL(CURRENT_S_VALUE(INTFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(INTFMT),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,INTFMT,NATIVE_INTFMT);
   if (EQUAL(CURRENT_S_VALUE(REALFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(REALFMT),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,REALFMT,NATIVE_REALFMT);
   if (EQUAL(CURRENT_S_VALUE(BHOST),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BHOST),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,BHOST,NATIVE_HOST_LABEL);
   if (EQUAL(CURRENT_S_VALUE(BINTFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BINTFMT),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,BINTFMT,NATIVE_INTFMT);
   if (EQUAL(CURRENT_S_VALUE(BREALFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BREALFMT),"LOCAL"))
      v2_add_lbl_item_value_tbl(unit,BREALFMT,NATIVE_REALFMT);

   return SUCCESS;
}

