#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

void v2_full_init_value_tbl(int unit)
{
   int i;

   for (i=0; i<N_UNIT_TABLE_ENTRIES; i++)
      CURRENT_VALUE(i) = default_table[i].value;

   for (i=0; i<N_LABEL_TABLE_ENTRIES; i++)
      LABEL_VALUE(i) = label_default_table[i].value;
}
