#include "XvicBasicImageP.h"
#include <math.h>       /* only for floor()... is it really needed??!!!!*/

/* #define DPR(x) printf x */
#define DPR(x)

/************************************************************************/
/* Dither macros							*/
/************************************************************************/
/* The following dither algorithm was invented by Dave Kagels of the	*/
/* Digital Image Animation Lab (DIAL) at JPL.  Please contact Dave if	*/
/* you have any questions about the algorithm.  Basically, the		*/
/* algorithm works as follows.  The green component is separated from	*/
/* the red-blue component, which remains combined.  So, part of the	*/
/* lookup table is a pure green ramp (red and blue are both 0), while	*/
/* the bulk of the lookup table is a combination of red and blue ramps	*/
/* (with green 0), so that all combinations of the red and blue		*/
/* intensities are represented.  The green and red-blue pixels are	*/
/* alternated in a checkerboard pattern.  For each of the two		*/
/* components of this checkerboard, an ordered dither (4x4 in this case)*/
/* is superimposed on top.  So, the 4x4 ordered dither actually covers	*/
/* an 8x8 area because of the checkerboard.  The value returned in	*/
/* "dest" is the actual index value into the colormap.			*/

#define KAGELS_DITHER(dest,r,g,b,x,y) {					\
   if (((x)+(y))&1) {  register int rc, bc;				\
      rc=(int)(((int)(r))*(biw->bim.red_levels-1)/16);			\
      bc=(int)(((int)(b))*(biw->bim.blue_levels-1)/16);			\
      (dest)=biw->bim.cmap_rb[(rc>>4)+kdpat[rc&15][((x)+6)&7][((y)+7)&7]+ \
             biw->bim.red_levels*((bc>>4)+kdpat[bc&15][((x)+5)&7][((y)+6)&7])];\
   } else {  register int gc;						\
      gc=(int)(((int)(g))*(biw->bim.green_levels-1)/16);		\
      (dest)=biw->bim.cmap_green[(gc>>4)+kdpat[gc&15][(x)&7][(y)&7]];	\
}  }

/* This is a standard 4x4 dither pattern.  The value returned in "dest"	*/
/* is a DN between 0 and levels-1, NOT a colormap index.		*/

#define ORDERED_DITHER(dest,dn,levels,x,y) { register int dnc;		\
   dnc=(int)(((int)(dn))*(levels-1)/16);				\
   (dest)=(dnc>>4) + odpat[dnc&15][(x)&3][(y)&3];			\
}


/************************************************************************/
/* Dither patterns							*/
/************************************************************************/

/* Kagels dither pattern */

static unsigned char kdpat[16][8][8]= {
   {	{ 0,0,0,0,0,0,0,0 },		/* 0 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,0,0,0,0,0,0 },		/* 1 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,1,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,0,0,1,0,0,0 },		/* 2 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,0,0,1,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,0,0,1,0,0,0 },		/* 3 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,0,0,1,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,0,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,0,0,1,0,0,0 },		/* 4 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,0,0,1,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,0,0,1,0,1,0 },		/* 5 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,0,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 6 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 7 */
	{ 0,0,0,0,0,0,0,0 },
	{ 0,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,0,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 8 */
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 9 */
	{ 0,1,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 10 */
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,0 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 11 */
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,0,0,0,0,1 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 12 */
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,0,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 13 */
	{ 0,1,0,1,0,1,0,0 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,0,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 14 */
	{ 0,1,0,1,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,1,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,0,0,1 }   },
   {	{ 1,0,1,0,1,0,1,0 },		/* 15 */
	{ 0,1,0,1,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,0,0,1,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,1,0,1,0,1 },
	{ 1,0,1,0,1,0,1,0 },
	{ 0,1,0,1,0,0,0,1 }   }
};

/* Ordered dither pattern */

static unsigned char odpat[16][4][4]= {
   {	{ 0,0,0,0 },		/* 0 */
	{ 0,0,0,0 },
	{ 0,0,0,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,0,0 },		/* 1 */
	{ 0,0,0,0 },
	{ 0,0,0,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,0,0 },		/* 2 */
	{ 0,0,0,0 },
	{ 0,0,1,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,1,0 },		/* 3 */
	{ 0,0,0,0 },
	{ 0,0,1,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,1,0 },		/* 4 */
	{ 0,0,0,0 },
	{ 1,0,1,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,1,0 },		/* 5 */
	{ 0,1,0,0 },
	{ 1,0,1,0 },
	{ 0,0,0,0 }  },
   {	{ 1,0,1,0 },		/* 6 */
	{ 0,1,0,0 },
	{ 1,0,1,0 },
	{ 0,0,0,1 }  },
   {	{ 1,0,1,0 },		/* 7 */
	{ 0,1,0,1 },
	{ 1,0,1,0 },
	{ 0,0,0,1 }  },
   {	{ 1,0,1,0 },		/* 8 */
	{ 0,1,0,1 },
	{ 1,0,1,0 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,0 },		/* 9 */
	{ 0,1,0,1 },
	{ 1,0,1,0 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,0 },		/* 10 */
	{ 0,1,0,1 },
	{ 1,0,1,1 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,1 },		/* 11 */
	{ 0,1,0,1 },
	{ 1,0,1,1 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,1 },		/* 12 */
	{ 0,1,0,1 },
	{ 1,1,1,1 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,1 },		/* 13 */
	{ 1,1,0,1 },
	{ 1,1,1,1 },
	{ 0,1,0,1 }  },
   {	{ 1,1,1,1 },		/* 14 */
	{ 1,1,0,1 },
	{ 1,1,1,1 },
	{ 0,1,1,1 }  },
   {	{ 1,1,1,1 },		/* 15 */
	{ 1,1,1,1 },
	{ 1,1,1,1 },
	{ 0,1,1,1 }  }
};


/************************************************************************/
/* The function below generates the calls.  This generates the code.	*/
/************************************************************************/

#define DTYPE_BYTE 0
#define DTYPE_HALF 1
#define DTYPE_OTHER 2

#define MODE_COLOR 0
#define MODE_PSEUDO 1
#define MODE_BW 2

#define PROTO								\
   XvicBasicImageWidget biw,						\
   XvicImageData *image,						\
   XImage *ximage,							\
   _XvicRect *dpy_area,							\
   int src_pixel_size,							\
   int y_pre_offset,							\
   int y_pre_rem,							\
   int y_pre_inc,							\
   int y_pre_inc_rem,							\
   int x_pre_offset,							\
   int x_pre_rem_start,							\
   int x_pre_inc,							\
   int x_pre_inc_rem,							\
   register unsigned char *dest_ptr,					\
   int y_dest_inc,							\
   XvicUHalf *half_buffer1,						\
   XvicUHalf *half_buffer2,						\
   XvicUHalf *half_buffer3


#define ARG_LIST							\
biw, image, ximage, dpy_area, src_pixel_size, y_pre_offset, y_pre_rem,	\
y_pre_inc, y_pre_inc_rem, x_pre_offset, x_pre_rem_start, x_pre_inc,	\
x_pre_inc_rem, dest_ptr, y_dest_inc, half_buffer1, half_buffer2, half_buffer3

#define ARG_DECL							\
   XvicBasicImageWidget biw;						\
   XvicImageData *image;						\
   XImage *ximage;							\
   _XvicRect *dpy_area;							\
   int src_pixel_size;							\
   int y_pre_offset;							\
   int y_pre_rem;							\
   int y_pre_inc;							\
   int y_pre_inc_rem;							\
   int x_pre_offset;							\
   int x_pre_rem_start;							\
   int x_pre_inc;							\
   int x_pre_inc_rem;							\
   register unsigned char *dest_ptr;					\
   int y_dest_inc;							\
   XvicUHalf *half_buffer1;						\
   XvicUHalf *half_buffer2;						\
   XvicUHalf *half_buffer3;

#define VARS								\
   register unsigned char dn, dn_red, dn_grn, dn_blu;			\
   register int src_offset;  /* x_pre_offset biased to start of actual data */ \
   register int x_dpy;							\
   register int x_pre_rem;						\
   int y_dpy;								\
   XvicByte byte_dn;							\
   XvicFull full_dn;							\
   XvicUFull ufull_dn;							\
   XvicReal real_dn;							\
   XvicDouble double_dn;						\
   int buf_diff;     /* difference between image index and half_buffer index */
 
/************************************************************************/

#define EASYZOOM

#ifdef Y_LOOP_START
#undef Y_LOOP_START
#endif
#define Y_LOOP_START							\
   for (y_dpy = dpy_area->y1; y_dpy <= dpy_area->y2; y_dpy++) {

#ifdef X_LOOP_START
#undef X_LOOP_START
#endif
#define X_LOOP_START							\
      x_pre_rem = x_pre_rem_start;					\
      src_offset = y_pre_offset * image->line_width + image->start_offset \
			+ x_pre_offset;					\
									\
      for (x_dpy = dpy_area->x1; x_dpy <= dpy_area->x2; x_dpy++) {

#ifdef X_LOOP_END
#undef X_LOOP_END
#endif
#define X_LOOP_END							\
         /* Increment X coordinates to next one */			\
									\
         src_offset += x_pre_inc;					\
      }

#ifdef Y_LOOP_END
#undef Y_LOOP_END
#endif
#define Y_LOOP_END							\
      /* Increment Y coordinates to next one */				\
									\
      y_pre_offset += y_pre_inc;					\
      y_pre_rem += y_pre_inc_rem;					\
      if (y_pre_rem >= YEZI) {						\
         y_pre_offset++;						\
         y_pre_rem -= YEZI;						\
      }									\
									\
      dest_ptr += y_dest_inc;						\
   }

#include "XvicCopyRawFn_zoom.h"

/************************************************************************/

#undef EASYZOOM

/* X,Y_LOOP_START are the same */
#ifdef X_LOOP_END
#undef X_LOOP_END
#endif
#define X_LOOP_END							\
         /* Increment X coordinates to next one */			\
									\
         src_offset += x_pre_inc;					\
         x_pre_rem += x_pre_inc_rem;					\
         if (x_pre_rem >= XEZI) {					\
            src_offset += src_pixel_size;				\
            x_pre_rem -= XEZI;						\
         }								\
      }

/* Y_LOOP_END is the same */
									
#include "XvicCopyRawFn_zoom.h"


/************************************************************************/
/* _XvicCopyRawXimage							*/
/*									*/
/* Copies raw image data into an XImage structure.  This routine is	*/
/* really the heart of the entire widget.  The area parameter is in	*/
/* Img coordinates and must not be bigger than the tile.  The area	*/
/* parameter must also completely fit in both the image and the ximage.	*/
/* The ximage struct is assumed to have the coordinates specified by	*/
/* the tile, while the image structure describes its own coordinates.	*/
/*									*/
/* The basic idea is to loop over the output area (dpy coords), and	*/
/* figure out what pixel to get from the input (pre coords).		*/
/*									*/
/* In order to avoid an expensive zoom divide for every pixel, the	*/
/* steps between pixels are maintained in a "pre_offset" (the actual	*/
/* offset to the current pixel) and a "pre_rem" (the remainder of the	*/
/* division had it taken place).  The increments are likewise maintained*/
/* as an integer part and a remainder.  When the remainder exceeds the	*/
/* divisor, the integer "pre_offset" is incremented.			*/
/*									*/
/* This code gets complicated because of the loop unrolling.  Instead	*/
/* of having loops with if tests inside (for things like zoom, dither,	*/
/* stretch, data type, etc.), we do all the if tests outside and	*/
/* repeat the loops a bunch of times.  This greatly improves the speed	*/
/* of the display.  This is done via repeated macros, and repeated	*/
/* includes of code segments.  It is complicated more by the need to	*/
/* split these cased up into multiple functions, because the expanded	*/
/* code was too big for the compiler.  Thus all the XvicCopyRaw*.h	*/
/* files.								*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicCopyRawXimage(biw, tile, image, ximage, area)
   XvicBasicImageWidget biw;
   Tile *tile;
   XvicImageData *image;
   XImage *ximage;
   _XvicRect *area;
#else
_XvicCopyRawXimage(
   XvicBasicImageWidget biw,
   Tile *tile,
   XvicImageData *image,
   XImage *ximage,
   _XvicRect *area)
#endif /* _NO_PROTO */
{
   _XvicRect dpy_area_struct;
   _XvicRect *dpy_area = &dpy_area_struct;
   register unsigned char dn, dn_red, dn_grn, dn_blu;
   static XvicUHalf *half_buffer1 = NULL;
   static XvicUHalf *half_buffer2 = NULL;
   static XvicUHalf *half_buffer3 = NULL;
   static int half_buffer_size = 0;

   /* Coords set once at beginning */
   int bytes_per_pixel;
   int x_dpy_offset, y_dpy_offset;
   int x_pre_offset;
   int x_pre, y_pre;
   int x_pre_rem_start;
   int x_pre_inc, x_pre_inc_rem;
   int y_pre_inc, y_pre_inc_rem;
   int y_dest_inc;
   int src_pixel_size;

   /* Coords modified in y loop */
   int y_pre_offset;
   int y_dpy;
   int y_pre_rem;

   /* Coords modified in x loop */
   register unsigned char *dest_ptr;
   register int src_offset;   /* x_pre_offset biased to start of actual data */
   register int x_dpy;
   register int x_pre_rem;

   dpy_area->x1 = X1_Img2Dpy(area->x1);
   dpy_area->x2 = X2_Img2Dpy(area->x2);
   dpy_area->y1 = Y1_Img2Dpy(area->y1);
   dpy_area->y2 = Y2_Img2Dpy(area->y2);

   bytes_per_pixel = ximage->bits_per_pixel / 8;
   src_pixel_size = biw->bim.pixel_size;

   y_dpy_offset = dpy_area->y1 - tile->dpy.y1;

   y_pre = Y_Dpy2Pre(dpy_area->y1);
   y_pre_rem = (dpy_area->y1 * YEZO + YSUB) - (y_pre * YEZI);
   y_pre_offset = y_pre - Y1_Img2Pre(image->y);

   y_pre_inc = IDIV(YEZO, YEZI);
   y_pre_inc_rem = (YEZO) - (y_pre_inc * YEZI);

   x_dpy_offset = dpy_area->x1 - tile->dpy.x1;

   x_pre = X_Dpy2Pre(dpy_area->x1);
   x_pre_rem_start = (dpy_area->x1 * XEZO + XSUB) - (x_pre * XEZI);
   x_pre_offset = (x_pre - X1_Img2Pre(image->x)) * src_pixel_size;

   x_pre_inc = IDIV(XEZO, XEZI);
   x_pre_inc_rem = (XEZO) - (x_pre_inc * XEZI);
   x_pre_inc *= src_pixel_size;

   dest_ptr = (unsigned char *)ximage->data +
			y_dpy_offset * ximage->bytes_per_line +
			x_dpy_offset * bytes_per_pixel;
   y_dest_inc = (ximage->bytes_per_line -
		(dpy_area->x2 - dpy_area->x1 + 1) * bytes_per_pixel);

   /* See if a temporary buffer is needed (non-byte, non-half data) */
   /* Re-allocate it if it's not big enough. */

   if ( ! ((biw->bim.data_type == XvicBYTE && biw->bim.lut16_type == XvicRAW) ||
           biw->bim.data_type == XvicHALF || biw->bim.data_type == XvicUHALF)) {

      if ((IDIV((biw->bim.tile_width+1) * XPZI - 1, XPZO) * sizeof(XvicUHALF))
				> half_buffer_size) {
         if (half_buffer1 != NULL)
            _XvicFree(biw, half_buffer1, half_buffer_size);
         if (half_buffer2 != NULL)
            _XvicFree(biw, half_buffer2, half_buffer_size);
         if (half_buffer3 != NULL)
            _XvicFree(biw, half_buffer3, half_buffer_size);
         half_buffer_size =
		IDIV((biw->bim.tile_width+1)*XPZI-1, XPZO) * sizeof(XvicUHALF);
         half_buffer1 = _XvicMalloc(biw, half_buffer_size);
         half_buffer2 = _XvicMalloc(biw, half_buffer_size);
         half_buffer3 = _XvicMalloc(biw, half_buffer_size);
      }
   }

   if (x_pre_inc_rem == 0) {	/* No pixel replication or weird zoom */

#define EASYZOOM

#include "XvicCopyRawCall_zoom.h"

   }
   else {			/* Pixel replication or weird zoom */

#undef EASYZOOM

#include "XvicCopyRawCall_zoom.h"

   }
}


/************************************************************************/
/* _XvicGetDitherPixmap							*/
/*									*/
/* Creates a pixmap to use as a tiling pattern for dithering, given a	*/
/* dither type and RGB color.  The colors are 16-bit values, the same	*/
/* as what's in an XColor struct.  If a GC is set up with this pixmap	*/
/* for tile and fill_style set to FillTiled, then graphics may be drawn	*/
/* with the GC and they will be dithered just like image data.  Note	*/
/* that the pixmap must be regenerated if the foreground color changes,	*/
/* and there is no way to use a background color.  The size of the	*/
/* current pixmap is passed in; if it is not the right size the pixmap	*/
/* is freed and reallocated.  Note that the pixmap and sizes are passed	*/
/* by reference not value.  The passed-in pixmap may be NULL (although	*/
/* not the pointer to the pixmap).  It is assumed that the colormap is	*/
/* set up to support the requested type of dithering, and that this is	*/
/* only used for 8-bit visuals.						*/
/*									*/
/* This function is in this module simply to make use of the dither	*/
/* tables and macros.  It is used only in Image, not BasicImage.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicGetDitherPixmap(biw, pixmap, width, height, type, red, green, blue)
   XvicBasicImageWidget biw;
   Pixmap *pixmap;
   int *width;
   int *height;
   int type;
   int red;
   int green;
   int blue;
#else
_XvicGetDitherPixmap(
   XvicBasicImageWidget biw,
   Pixmap *pixmap,
   int *width,
   int *height,
   int type,
   int red,
   int green,
   int blue)
#endif /* _NO_PROTO */
{
   register unsigned char dn_red, dn_grn, dn_blu, dn_red2, dn_grn2, dn_blu2;
   int x_dpy, y_dpy;
   register unsigned char *dest_ptr;
   XImage *ximage;
   int desired_size;

   if (!XtIsRealized((Widget) biw))
      return;			/* Do nothing if not realized */

   /* Get the Pixmap to use */

   if (type == XvicKAGELS)
      desired_size = 8;		/* 8x8 block */
   else
      desired_size = 4;		/* 4x4 block */

   if (*pixmap) {		/* Check to see if given one is sufficient */
      if (*height != desired_size || *width != desired_size) {
         _XvicMemoryReturn(biw, *height * *width);
         XFreePixmap(XtDisplay(biw), *pixmap);
         *pixmap = None;
      }
   }
   if (!*pixmap) {		/* Allocate a new pixmap if needed */
      _XvicMemoryGrab(biw, desired_size * desired_size);
      *pixmap = XCreatePixmap(XtDisplay(biw), XtWindow(biw),
		desired_size, desired_size, biw->core.depth);
      /*!!!! Should do something with a BadAlloc error here!!!!*/
      *width = desired_size;
      *height = desired_size;
   }

   /* Set up the temp XImage to use */

   biw->bim.protect_tmp_ximage = TRUE;
   _XvicGetXImage(biw, &biw->bim.tmp_ximage, *width, *height);
   biw->bim.protect_tmp_ximage = FALSE;
   ximage = biw->bim.tmp_ximage;

   /* Get the color into unsigned chars for the includes */

   dn_red = red>>8;
   dn_grn = green>>8;
   dn_blu = blue>>8;

   /* Now do the dithering.  This code is a much simplified version of	*/
   /* the above.							*/

   if (type == XvicKAGELS) {
      for (y_dpy = 0; y_dpy < *height; y_dpy++) {
         dest_ptr=(unsigned char *)ximage->data + y_dpy*ximage->bytes_per_line;
         for (x_dpy = 0; x_dpy < *width; x_dpy++) {
            KAGELS_DITHER(*dest_ptr++, dn_red, dn_grn, dn_blu, x_dpy, y_dpy);
         }
      }
   }
   else {			/* XvicORDERED */
      if (biw->bim.colormap_policy == XvicFULL) {		/* 332 */
         for (y_dpy = 0; y_dpy < *height; y_dpy++) {
            dest_ptr=(unsigned char *)ximage->data+y_dpy*ximage->bytes_per_line;
            for (x_dpy = 0; x_dpy < *width; x_dpy++) {
               ORDERED_DITHER(dn_red2, dn_red, 8, x_dpy, y_dpy);
               ORDERED_DITHER(dn_grn2, dn_grn, 8, x_dpy, y_dpy);
               ORDERED_DITHER(dn_blu2, dn_blu, 4, x_dpy, y_dpy);
               *dest_ptr++ = (dn_red2 << 5) | (dn_grn2 << 2) | dn_blu2;
            }
         }
      }
      else {							/* 232 */
         for (y_dpy = 0; y_dpy < *height; y_dpy++) {
            dest_ptr=(unsigned char *)ximage->data+y_dpy*ximage->bytes_per_line;
            for (x_dpy = 0; x_dpy < *width; x_dpy++) {
               ORDERED_DITHER(dn_red2, dn_red, 4, x_dpy, y_dpy);
               ORDERED_DITHER(dn_grn2, dn_grn, 8, x_dpy, y_dpy);
               ORDERED_DITHER(dn_blu2, dn_blu, 4, x_dpy, y_dpy);
               *dest_ptr++ = (dn_red2 << 5) | (dn_grn2 << 2) | dn_blu2 | 0x80;
            }
         }
      }
   }

   /* Now copy the XImage to the Pixmap */

   XSetClipMask(XtDisplay(biw), biw->bim.img_gc, None);
   XPutImage(XtDisplay(biw), *pixmap, biw->bim.img_gc, ximage,
	0, 0, 0, 0, *width, *height);
}

