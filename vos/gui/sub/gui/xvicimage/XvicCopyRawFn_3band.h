/************************************************************************/
/* XvicCopyRawFn_3band.h						*/
/* This code, called by XvicCopyRawFn_zoom.h, takes care of stretches	*/
/* for 3-band data.  It in turn includes XvicCopyRaw_color.h to		*/
/* actually do the work.						*/
/************************************************************************/

#include "XvicCopyRaw_name.h"

static void
#ifdef _NO_PROTO
FN_NAME(3band)(ARG_LIST)
ARG_DECL
#else
FN_NAME(3band)(PROTO)
#endif
{
VARS

   if (biw->bim.use_rgb_lut) {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* Color, stretched */				\
      dn_red = biw->bim.red_lut  [RED_PIXEL];				\
      dn_grn = biw->bim.green_lut[GRN_PIXEL];				\
      dn_blu = biw->bim.blue_lut [BLU_PIXEL];
#include "XvicCopyRaw_color.h"

   }
   else {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* Color, unstretched */			\
      dn_red = RED_PIXEL;						\
      dn_grn = GRN_PIXEL;						\
      dn_blu = BLU_PIXEL;
#include "XvicCopyRaw_color.h"

   }
}


