/************************************************************************/
/* XvicCopyRawFn_zoom.h							*/
/* This code actually generates the functions called by			*/
/* XvicCopyRawCall_zoom.h.  It defines pixel-access macros based on	*/
/* the data type.  This is simple for byte, and for half/uhalf, we just	*/
/* use a (pre-generated) 16-bit LUT to convert to byte (then do the	*/
/* same things byte does).  For other data types, a preloop is done	*/
/* inside the line loop but outside the pixel loop) which converts the	*/
/* entire line to half, then goes through the above process.  The data	*/
/* type test is done *inside* this preloop (on a per-pixel basis)	*/
/* because it won't be noticed with the other necessary calculations	*/
/* (like limit checks) which are done per pixel, and it also lessens	*/
/* the combinatorial code explosion (XvicCopyRaw generates a HUGE	*/
/* object file!).							*/
/************************************************************************/

#ifdef DTYPE
#undef DTYPE
#endif
#ifdef MODE
#undef MODE
#endif

/************************************************************************/

#define DTYPE DTYPE_BYTE

#define X_PRELOOP

/*----------------------------------------------------------------------*/
/* Byte, color */

#define MODE MODE_COLOR

#define RED_PIXEL *(image->red_pixels + src_offset)
#define GRN_PIXEL *(image->grn_pixels + src_offset)
#define BLU_PIXEL *(image->blu_pixels + src_offset)

#include "XvicCopyRawFn_3band.h"

#undef RED_PIXEL
#undef GRN_PIXEL
#undef BLU_PIXEL

/*----------------------------------------------------------------------*/
/* Byte, BW or pseudo */

#undef MODE
#define MODE MODE_BW

#define BW_PIXEL *(image->bw_pixels + src_offset)

#include "XvicCopyRawFn_1band.h"

#undef BW_PIXEL

/************************************************************************/

#undef DTYPE
#define DTYPE DTYPE_HALF

#undef X_PRELOOP
#define X_PRELOOP

/*----------------------------------------------------------------------*/
/* half, color */

#undef MODE
#define MODE MODE_COLOR

#define RED_PIXEL biw->bim.lookup16_red[*(XvicUHalf *)(image->red_pixels + src_offset)]
#define GRN_PIXEL biw->bim.lookup16_grn[*(XvicUHalf *)(image->grn_pixels + src_offset)]
#define BLU_PIXEL biw->bim.lookup16_blu[*(XvicUHalf *)(image->blu_pixels + src_offset)]

#include "XvicCopyRawFn_3band.h"

#undef RED_PIXEL
#undef GRN_PIXEL
#undef BLU_PIXEL

/*----------------------------------------------------------------------*/
/* half, 16-bit pseudo */

#undef MODE
#define MODE MODE_PSEUDO

#define RED_PIXEL biw->bim.lookup16_red[*(XvicUHalf *)(image->bw_pixels + src_offset)]
#define GRN_PIXEL biw->bim.lookup16_grn[*(XvicUHalf *)(image->bw_pixels + src_offset)]
#define BLU_PIXEL biw->bim.lookup16_blu[*(XvicUHalf *)(image->bw_pixels + src_offset)]

#include "XvicCopyRawFn_3band.h"

#undef RED_PIXEL
#undef GRN_PIXEL
#undef BLU_PIXEL

/*----------------------------------------------------------------------*/
/* half, bw */

#undef MODE
#define MODE MODE_BW

#define BW_PIXEL biw->bim.lookup16_bw[*(XvicUHalf *)(image->bw_pixels + src_offset)]

#include "XvicCopyRawFn_1band.h"

#undef BW_PIXEL

/************************************************************************/

#undef DTYPE
#define DTYPE DTYPE_OTHER

#define X_PRELOOP_START							   \
      x_pre_rem = x_pre_rem_start;					   \
      src_offset = y_pre_offset * image->line_width + image->start_offset  \
			+ x_pre_offset;					   \
      buf_diff = y_pre_offset * image->line_width + image->start_offset;   \
      for (x_dpy = dpy_area->x1; x_dpy <= dpy_area->x2; x_dpy++) {

#define X_PRELOOP_END X_LOOP_END

#define PRESCALE(dest, src)						\
      if ((double)src < biw->bim.raw_data_min)				\
         dest = 0;							\
      else if ((double)src >= biw->bim.raw_data_max)			\
         dest = biw->bim.scaled_data_max;				\
      else								\
         dest = floor((src - biw->bim.raw_data_min) * biw->bim.prescale_factor);

/*----------------------------------------------------------------------*/
/* other, color */

#undef MODE
#define MODE MODE_COLOR

/* 3-band preloop */

#undef X_PRELOOP
#define X_PRELOOP							\
   X_PRELOOP_START							\
      switch (biw->bim.data_type) {					\
         case XvicBYTE:							\
            byte_dn = *(XvicByte *)(image->red_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicByte)], \
			byte_dn);					\
            byte_dn = *(XvicByte *)(image->grn_pixels + src_offset);	\
            PRESCALE(half_buffer2[(src_offset-buf_diff) / sizeof(XvicByte)], \
			byte_dn);					\
            byte_dn = *(XvicByte *)(image->blu_pixels + src_offset);	\
            PRESCALE(half_buffer3[(src_offset-buf_diff) / sizeof(XvicByte)], \
			byte_dn);					\
            break;							\
         case XvicFULL:							\
            full_dn = *(XvicFull *)(image->red_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicFull)], \
			full_dn);					\
            full_dn = *(XvicFull *)(image->grn_pixels + src_offset);	\
            PRESCALE(half_buffer2[(src_offset-buf_diff) / sizeof(XvicFull)], \
			full_dn);					\
            full_dn = *(XvicFull *)(image->blu_pixels + src_offset);	\
            PRESCALE(half_buffer3[(src_offset-buf_diff) / sizeof(XvicFull)], \
			full_dn);					\
            break;							\
         case XvicUFULL:						\
            ufull_dn = *(XvicUFull *)(image->red_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicUFull)], \
			ufull_dn);					\
            ufull_dn = *(XvicUFull *)(image->grn_pixels + src_offset);	\
            PRESCALE(half_buffer2[(src_offset-buf_diff) / sizeof(XvicUFull)], \
			ufull_dn);					\
            ufull_dn = *(XvicUFull *)(image->blu_pixels + src_offset);	\
            PRESCALE(half_buffer3[(src_offset-buf_diff) / sizeof(XvicUFull)], \
			ufull_dn);					\
            break;							\
         case XvicREAL:							\
            real_dn = *(XvicReal *)(image->red_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicReal)], \
			real_dn);					\
            real_dn = *(XvicReal *)(image->grn_pixels + src_offset);	\
            PRESCALE(half_buffer2[(src_offset-buf_diff) / sizeof(XvicReal)], \
			real_dn);					\
            real_dn = *(XvicReal *)(image->blu_pixels + src_offset);	\
            PRESCALE(half_buffer3[(src_offset-buf_diff) / sizeof(XvicReal)], \
			real_dn);					\
            break;							\
         case XvicDOUBLE:						\
            double_dn = *(XvicDouble *)(image->red_pixels + src_offset); \
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicDouble)], \
			double_dn);					\
            double_dn = *(XvicDouble *)(image->grn_pixels + src_offset); \
            PRESCALE(half_buffer2[(src_offset-buf_diff) / sizeof(XvicDouble)], \
			double_dn);					\
            double_dn = *(XvicDouble *)(image->blu_pixels + src_offset); \
            PRESCALE(half_buffer3[(src_offset-buf_diff) / sizeof(XvicDouble)], \
			double_dn);					\
            break;							\
      }									\
   X_PRELOOP_END

#define RED_PIXEL biw->bim.lookup16_red[				\
			half_buffer1[(src_offset-buf_diff)/src_pixel_size]]
#define GRN_PIXEL biw->bim.lookup16_grn[				\
			half_buffer2[(src_offset-buf_diff)/src_pixel_size]]
#define BLU_PIXEL biw->bim.lookup16_blu[				\
			half_buffer3[(src_offset-buf_diff)/src_pixel_size]]

#include "XvicCopyRawFn_3band.h"

#undef RED_PIXEL
#undef GRN_PIXEL
#undef BLU_PIXEL

/*----------------------------------------------------------------------*/
/* other, bw */

#undef MODE
#define MODE MODE_BW

#undef X_PRELOOP
#define X_PRELOOP							\
   X_PRELOOP_START							\
      switch (biw->bim.data_type) {					\
         case XvicBYTE:							\
            byte_dn = *(XvicByte *)(image->bw_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicByte)], \
			byte_dn);					\
            break;							\
         case XvicFULL:							\
            full_dn = *(XvicFull *)(image->bw_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicFull)], \
			full_dn);					\
            break;							\
         case XvicUFULL:						\
            ufull_dn = *(XvicUFull *)(image->bw_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicUFull)], \
			ufull_dn);					\
            break;							\
         case XvicREAL:							\
            real_dn = *(XvicReal *)(image->bw_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicReal)], \
			real_dn);					\
            break;							\
         case XvicDOUBLE:						\
            double_dn = *(XvicDouble *)(image->bw_pixels + src_offset);	\
            PRESCALE(half_buffer1[(src_offset-buf_diff) / sizeof(XvicDouble)], \
			double_dn);					\
            break;							\
      }									\
   X_PRELOOP_END

#define BW_PIXEL biw->bim.lookup16_bw[					\
			half_buffer1[(src_offset-buf_diff)/src_pixel_size]]

#include "XvicCopyRawFn_1band.h"

#undef BW_PIXEL

/*----------------------------------------------------------------------*/
/* other, 16-bit pseudo */

#undef MODE
#define MODE MODE_PSEUDO

/* Same X_PRELOOP as above for 1 band */

#define RED_PIXEL biw->bim.lookup16_red[				\
			half_buffer1[(src_offset-buf_diff)/src_pixel_size]]
#define GRN_PIXEL biw->bim.lookup16_grn[				\
			half_buffer1[(src_offset-buf_diff)/src_pixel_size]]
#define BLU_PIXEL biw->bim.lookup16_blu[				\
			half_buffer1[(src_offset-buf_diff)/src_pixel_size]]

#include "XvicCopyRawFn_3band.h"

#undef RED_PIXEL
#undef GRN_PIXEL
#undef BLU_PIXEL

#undef X_PRELOOP_START
#undef X_PRELOOP
#undef X_PRELOOP_END

