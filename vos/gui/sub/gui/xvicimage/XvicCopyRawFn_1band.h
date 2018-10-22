/************************************************************************/
/* XvicCopyRawFn_1band.h						*/
/* This code, called by XvicCopyRawFn_zoom.h, takes care of stretches	*/
/* and pseudocoloring for single-band data.  It in turn includes	*/
/* XvicCopyRaw_color.h or XvicCopyRaw_bw.h (1-band PS becomes color)	*/
/* to actually do the work.						*/
/************************************************************************/

#include "XvicCopyRaw_name.h"

static void
#ifdef _NO_PROTO
FN_NAME(1band)(ARG_LIST)
ARG_DECL
#else
FN_NAME(1band)(PROTO)
#endif
{
VARS

   if (biw->bim.ps_as_color) {		/* Pseudo (only 8-bit at this point) */
      if (biw->bim.use_stretch_lut) {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* Pseudo, stretched */				\
      dn = biw->bim.stretch_lut[BW_PIXEL];				\
      dn_red = biw->bim.red_lut[dn];					\
      dn_grn = biw->bim.green_lut[dn];					\
      dn_blu = biw->bim.blue_lut[dn];
#include "XvicCopyRaw_color.h"

      }
      else {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* Pseudo, unstretched */			\
      dn = BW_PIXEL;							\
      dn_red = biw->bim.red_lut[dn];					\
      dn_grn = biw->bim.green_lut[dn];					\
      dn_blu = biw->bim.blue_lut[dn];
#include "XvicCopyRaw_color.h"

      }
   }

   else {					/* BW */
      if (biw->bim.use_stretch_lut) {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* BW, stretched */				\
      dn = biw->bim.stretch_lut[BW_PIXEL];
#include "XvicCopyRaw_bw.h"

      }
      else {

#ifdef GET_PIXELS
#undef GET_PIXELS
#endif
#define GET_PIXELS	/* BW, unstretched */				\
      dn = BW_PIXEL;
#include "XvicCopyRaw_bw.h"

      }
   }

}

