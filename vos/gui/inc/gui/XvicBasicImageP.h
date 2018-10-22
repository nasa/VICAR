#ifndef _XvicBasicImageP_h
#define _XvicBasicImageP_h

#ifdef NO_MOTIF				/* -D compile option */
#define USE_MOTIF 0
#else
#define USE_MOTIF 1
#endif

#include <Xm/Xm.h>
#include "XvicBasicImage.h"
#include <math.h>			/* only for floor() function */

#if USE_MOTIF
#if XmVERSION==1 && XmREVISION<=1	/* Some things are different in 1.2...*/
#define MOTIF_1_1
#endif
#endif

#if USE_MOTIF
#include <Xm/XmP.h>
#ifndef MOTIF_1_1
#include <Xm/PrimitiveP.h>
#endif
#endif

#include "XvicRegion.h"

#if USE_MOTIF
#ifdef MOTIF_1_1	/* These definitions got left out of 1.1 */
#define XmInheritBorderHighlight ((XtWidgetProc) _XtInherit)
#define XmInheritBorderUnhighlight ((XtWidgetProc) _XtInherit)
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* Method function typedefs						*/
/************************************************************************/

typedef void (*ExposeOverlayProc)(
#if NeedFunctionPrototypes
   Widget,
   _XvicRegion *,
   _XvicRect *
#endif
);

typedef void (*MoveOverlayProc)(
#if NeedFunctionPrototypes
   Widget,
   int,			/* x_src */
   int,			/* y_src */
   int,			/* width */
   int,			/* height */
   int,			/* x_dest */
   int			/* y_dest */
#endif
);

typedef void (*ClearOverlayProc)(
#if NeedFunctionPrototypes
   Widget,
   int,			/* x */
   int,			/* y */
   int,			/* width */
   int			/* height */
#endif
);

typedef void (*ReleaseGrColorsProc)(
#if NeedFunctionPrototypes
   Widget
#endif
);

typedef void (*SetUpGrColorsProc)(
#if NeedFunctionPrototypes
   Widget
#endif
);

typedef void (*InstallColormapProc)(
#if NeedFunctionPrototypes
   Widget
#endif
);

#define XvicInheritExposeOverlay	((ExposeOverlayProc) _XtInherit)
#define XvicInheritMoveOverlay		((MoveOverlayProc) _XtInherit)
#define XvicInheritClearOverlay		((ClearOverlayProc) _XtInherit)
#define XvicInheritReleaseGrColors	((ReleaseGrColorsProc) _XtInherit)
#define XvicInheritSetUpGrColors	((SetUpGrColorsProc) _XtInherit)
#define XvicInheritInstallColormap	((InstallColormapProc) _XtInherit)

#define CALL_ExposeOverlay(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.expose_overlay) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.expose_overlay) args

#define CALL_MoveOverlay(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.move_overlay) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.move_overlay) args

#define CALL_ClearOverlay(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.clear_overlay) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.clear_overlay) args

#define CALL_ReleaseGrColors(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.release_gr_colors) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.release_gr_colors) args

#define CALL_SetUpGrColors(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.set_up_gr_colors) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.set_up_gr_colors) args

#define CALL_InstallColormap(w, args) \
	if (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.install_colormap) \
	   (((XvicBasicImageWidgetClass)XtClass(w))->basic_image_class.install_colormap) args

/************************************************************************/
/* Class structure							*/
/************************************************************************/

typedef struct _XvicBasicImageClassPart
{
   ExposeOverlayProc	expose_overlay;
   MoveOverlayProc	move_overlay;
   ClearOverlayProc	clear_overlay;
   ReleaseGrColorsProc	release_gr_colors;
   SetUpGrColorsProc	set_up_gr_colors;
   InstallColormapProc	install_colormap;
   XtPointer		extension;	/* Pointer to extension record */
} XvicBasicImageClassPart;

typedef struct _XvicBasicImageClassRec
{
   CoreClassPart		core_class;
#if USE_MOTIF
   XmPrimitiveClassPart		primitive_class;
#endif
   XvicBasicImageClassPart	basic_image_class;
} XvicBasicImageClassRec;

externalref XvicBasicImageClassRec xvicBasicImageClassRec;

/************************************************************************/
/* Structures needed by Instance					*/
/************************************************************************/

/* Values for DN_transform, which specifies how to get a color index	*/
/* value (the value put in the XImage) from the DN value.		*/

#define TRANS_FULLCOLOR	0		/* DN is direct 24-bit color value */
#define TRANS_DIRECT	1		/* DN is direct color index value */
#define TRANS_HALF	3		/* index = (DN>>1) | 0x80 */
#define TRANS_332	4		/* index computed via 3/3/2 RGB */
#define TRANS_232	5		/* index computed via 2/3/2 RGB */
#define TRANS_CMAP	6		/* use cmap tables to get index */


#define LUT_SIZE	256
#define CMAP_SIZE_MAX	256
#define LUT16_SIZE	65536

typedef int *LUT;			/* dimension LUT_SIZE */
typedef int *LUT16;			/* dimension LUT16_SIZE */
typedef unsigned long *Dn2Cmap;		/* translates from DN value to    */
					/* colormap pixel (dim. LUT_SIZE) */

typedef struct _VisualStuff
{
   Visual		*visual;
   int			class;
   unsigned long	red_mask;
   unsigned long	green_mask;
   unsigned long	blue_mask;
   int			red_shift;	/* # bits to shift in long for RGB */
   int			green_shift;
   int			blue_shift;
   int			red_nbits;	/* # of bits for each color */
   int			green_nbits;
   int			blue_nbits;
   int			cmap_size;
} VisualStuff;

typedef struct _Tile {
   _XvicRect		img;		/* Coordinates in image space */
   _XvicRect		pre;		/* Coordinates in prezoom space */
   _XvicRect		dpy;		/* Coordinates in display space */
   XvicImageData	*raw;
   XImage		*ximage;
   Pixmap		pixmap;
   int			raw_size;	/* alloc'ed size of data area */
   int			ximage_size;
   int			pixmap_size;
   int			lru;		/* count for Least Recently Used */
} Tile;

/************************************************************************/
/* Instance structure							*/
/************************************************************************/

typedef struct _XvicBasicImagePart
{
   XtCallbackList	expose_callback;
   XtCallbackList	resize_callback;
   XtCallbackList	visible_area_callback;
   XtCallbackList	work_proc_active_callback;

   int			image_width;
   int			image_height;
   int			tile_width;
   int			tile_height;
   Dimension		view_width;
   Dimension		view_height;

   int			x_pan;
   int			y_pan;
   int			x_subpixel_pan;
   int			y_subpixel_pan;

   int			x_zoom;
   int			y_zoom;
   int			image_zoom;

   int			x_zoom_in;
   int			x_zoom_out;
   int			y_zoom_in;
   int			y_zoom_out;

   int			x_prezoom_in;
   int			x_prezoom_out;
   int			x_presubpixel_pan;
   int			y_prezoom_in;
   int			y_prezoom_out;
   int			y_presubpixel_pan;

   int			maximum_memory;

   unsigned char	colormap_policy;
   unsigned char	data_save_policy;
   unsigned char	stretch_policy;
   unsigned char	work_proc_policy;
   unsigned char	dither_mode;
   unsigned char	image_mode;
   unsigned char	lut_type;
   unsigned char	visual_type;
   unsigned char	constrain_pan;
   unsigned char	data_type;
   unsigned char	lut16_type;
   Boolean		enable_direct_color;

   int			gray_levels;
   int			red_levels;
   int			green_levels;
   int			blue_levels;

   double		raw_data_min;
   double		raw_data_max;
   int			scaled_data_max;
   int			output_data_max;

   unsigned char	bw_dither;
   unsigned char	bw_stretch_policy;
   unsigned char	bw_colormap_policy;
   unsigned char	bw_visual_type;

   unsigned char	color_dither;
   unsigned char	color_stretch_policy;
   unsigned char	color_colormap_policy;
   unsigned char	color_visual_type;

   unsigned char	pseudo_dither;
   unsigned char	pseudo_stretch_policy;
   unsigned char	pseudo_colormap_policy;
   unsigned char	pseudo_visual_type;

   /* Private members - not directly settable via resources */

   VisualStuff		vis;
   Colormap		colormap;
   Boolean		private_cmap;		/* TRUE if we created cmap */
   Boolean		cmap_alloc_mode;	/* AllocAll or AllocNone */
   int			gray_alloced;	/* # of cells in default colormap */
   int			green_alloced;	/* alloced to each color if       */
   int			red_alloced;	/* colormapPolicy is XvicALLOC.   */
   int			blue_alloced;
   int			rb_alloced;	/* normally red*blue except if error */
   Boolean		alloc_private;	/* TRUE if colors alloced private */
   Boolean		ps_as_color;	/* TRUE if PS displayed like COLOR */
   LUT			red_lut;	/* Actual LUTs set by user */
   LUT			green_lut;
   LUT			blue_lut;
   LUT			stretch_lut;
   Dn2Cmap		cmap_gray;	/* mappings from intensity to	*/
   Dn2Cmap		cmap_green;	/* cmap cell (for dithering)	*/
   Dn2Cmap		cmap_rb;
   int			DN_transform;	/* How to get color index from DN */
   Boolean		use_stretch_lut;
   Boolean		use_rgb_lut;

   LUT16		red_lut16;	/* Actual LUTs set by user */
   LUT16		green_lut16;
   LUT16		blue_lut16;
   LUT16		stretch_lut16;
   unsigned char	*lookup16_bw;	/* Merged 16-bit LUT's */
   unsigned char	*lookup16_red;
   unsigned char	*lookup16_grn;
   unsigned char	*lookup16_blu;

   double		prescale_factor;
   int			pixel_size;

   int			x_eff_zoom_in;	/* effective zoom factors */
   int			x_eff_zoom_out;	/* (does prezoom->screen zooming) */
   int			y_eff_zoom_in;
   int			y_eff_zoom_out;
   int			x_dpy_off;    /* coord offset for hilite, shadow, etc */
   int			y_dpy_off;

   int			x_screen_pan;	/* pan value modified for zoom */
   int			y_screen_pan;
   int			x_old_screen_pan;	/* previous pan value */
   int			y_old_screen_pan;
   int			pan_count;	/* # of outstanding pan GraphicsExpose*/
   int			pan_dir;	/* Horiz or Vert pan (or both) */
   int			x_min_pan;	/* valid while pan_count > 0 */
   int			x_max_pan;	/* these are for GraphicsExpose */
   int			y_min_pan;
   int			y_max_pan;
   int			x_min_exp_pan;	/* valid while pan_count > 0 */
   int			x_max_exp_pan;	/* these are for normal Expose */
   int			y_min_exp_pan;
   int			y_max_exp_pan;

   Tile			*tiles;		/* List of image tiles */
   int			num_tiles;
   int			num_tiles_x;
   int			num_tiles_y;

   XImage		*tmp_ximage;	/* Used/reused by DrawTileRaw */
   Boolean		protect_tmp_ximage; /*Stop free during alloc of itself*/

   _XvicRegion		*expose_rgn;	/* Damage list for expose */
   Boolean		work_proc_pending;
   Dimension		old_view_width;	/* So Resize can fix borders */
   Dimension		old_view_height;

   GC			img_gc;
   GC			pan_gc;

   int			memory_used;	/* compare to maximum_memory */
   int			lru;		/* count for Least Recently Used */
   int			current_tile;	/* index of last-touched tile */

} XvicBasicImagePart;

#if !USE_MOTIF		/* fake Primitive struct elements we need */
typedef struct _XmPrimitivePart {
   int		highlight_thickness;
   int		shadow_thickness;
} XmPrimitivePart;
#endif

typedef struct _XvicBasicImageRec
{
   CorePart		core;
   XmPrimitivePart	primitive;
   XvicBasicImagePart	bim;		/* short name cuz it's used a bunch */
} XvicBasicImageRec;

/************************************************************************/
/* Private functions needed by other modules				*/
/************************************************************************/

#ifdef _NO_PROTO

extern void _XvicCopyRawXimage();
extern void _XvicFree();
extern void _XvicGetDitherPixmap();
extern void _XvicGetVisibleRect();
extern void _XvicGetXImage();
extern void *_XvicMalloc();
extern void _XvicMemoryGrab();
extern void _XvicMemoryPanic();
extern void _XvicMemoryReturn();
extern void _XvicRedisplay_Internal();
extern void _XvicSetClipFromRgn();

#else

extern void _XvicCopyRawXimage(
		XvicBasicImageWidget biw,
		Tile *tile,
		XvicImageData *image,
		XImage *ximage,
		_XvicRect *area);

extern void _XvicFree(
		XvicBasicImageWidget biw,
		void *ptr,
		int size);

extern void _XvicGetDitherPixmap(
		XvicBasicImageWidget biw,
		Pixmap *pixmap,
		int *width,
		int *height,
		int type,
		int red,
		int green,
		int blue);

extern void _XvicGetVisibleRect(
		XvicBasicImageWidget biw,
		_XvicRect *rect);

extern void _XvicGetXImage(
		XvicBasicImageWidget biw,
		XImage **ximage,
		int width,
		int height);

extern void *_XvicMalloc(
		XvicBasicImageWidget biw,
		int size);

extern void _XvicMemoryGrab(
		XvicBasicImageWidget biw,
		int size);

extern void _XvicMemoryPanic(
		XvicBasicImageWidget biw);

extern void _XvicMemoryReturn(
		XvicBasicImageWidget biw,
		int size);

extern void _XvicRedisplay_Internal(
		XvicBasicImageWidget biw,
		XExposeEvent *event,
		Region region);

extern void _XvicSetClipFromRgn(
		XvicBasicImageWidget biw,
		_XvicRegion *rgn,
		GC gc,
		int x_off,
		int y_off);

#endif /* _NO_PROTO */

/************************************************************************/
/* Macros needed by implementation					*/
/************************************************************************/

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

/* Integer division that truncates downwards always.  Assumes y > 0	*/
/* #define IDIV(x,y) ((x)>=0 ? (x)/(y) : ((x)-(y)+1)/(y))		*/
/* This is implemented as a static function instead of a macro to avoid	*/
/* horrendously long macro expansions that confuse the Sun debugger.	*/
/* IDIV is not used in time-critical inner loops, so this should be	*/
/* okay.								*/

#ifdef _NO_PROTO
static int _XvicIdiv(x, y) int x,y; { return (x>=0 ? x/y : (x-y+1)/y); }
#else
static int _XvicIdiv(int x, int y) { return (x>=0 ? x/y : (x-y+1)/y); }
#endif
#define IDIV(x,y) _XvicIdiv(x,y)

/* Integer modulo that works for negative numbers, just like IDIV.	*/

#define IMOD(x,y) ((x)-(IDIV((x),(y))*(y)))

/* Derive a tile index from the X and Y tile numbers.  Assumes the	*/
/* availability of "biw".						*/

#define TILE_INDEX(x,y) ((y)*(biw->bim.num_tiles_x) + (x))

/* "Touch" a tile by updating its LRU count and setting it to be the	*/
/* current tile.  Only needed by the memory functions to keep track	*/
/* of what to (or what not to) free.  Assumes availability of "biw".	*/

#define TOUCH_TILE(index) { biw->bim.tiles[index].lru = biw->bim.lru++; \
	biw->bim.current_tile = index; }

/* Bits to set for biw->bim.pan_dir					*/

#define PAN_HORIZ	0x01
#define PAN_VERT	0x02

/* Round a floating-point number into an int.  5/4 rounding.		*/
/* floor() is necessary because fp->int conversion truncates toward 0,	*/
/* and we need to truncate toward negative infinity always.		*/

#define ROUND(fp) ((int)floor(fp+0.5))

/* Macros to access the widget data elements.  Assumes the availability	*/
/* of "biw".								*/

/* Note: PSEUDO_SET is not sufficient for pseudo mode; need also !COLOR_MODE */
#define PSEUDO_SET ((biw->bim.lut_type == XvicPSEUDO) || \
		    (biw->bim.lut_type == XvicPSEUDO_ONLY) || \
		    (biw->bim.lut16_type == XvicPSEUDO) || \
		    (biw->bim.lut16_type == XvicPSEUDO_ONLY))
#define COLOR_MODE (biw->bim.image_mode == XvicCOLOR)

#define CMAP_SIZE (biw->bim.vis.cmap_size)

#define XZI (biw->bim.x_zoom_in)
#define XZO (biw->bim.x_zoom_out)
#define YZI (biw->bim.y_zoom_in)
#define YZO (biw->bim.y_zoom_out)
#define XPZI (biw->bim.x_prezoom_in)
#define XPZO (biw->bim.x_prezoom_out)
#define YPZI (biw->bim.y_prezoom_in)
#define YPZO (biw->bim.y_prezoom_out)
#define XEZI (biw->bim.x_eff_zoom_in)
#define XEZO (biw->bim.x_eff_zoom_out)
#define YEZI (biw->bim.y_eff_zoom_in)
#define YEZO (biw->bim.y_eff_zoom_out)

#define XSUB (biw->bim.x_subpixel_pan)
#define YSUB (biw->bim.y_subpixel_pan)
#define XPSUB (biw->bim.x_presubpixel_pan)
#define YPSUB (biw->bim.y_presubpixel_pan)

#define XPAN (biw->bim.x_screen_pan)
#define YPAN (biw->bim.y_screen_pan)

/* Apply a zoom to an image size to get a screen size (ZOOM), or the	*/
/* reverse (UNZOOM).  Remainders are rounded up so all pixels will be	*/
/* accounted for.  Essentially, it returns a worst-case size.  Must be	*/
/* used only for sizes, not for coordinates.				*/

#define ZOOMX(width) IDIV(((width)+1) * XZI - 1, XZO)
#define ZOOMY(height) IDIV(((height)+1) * YZI - 1, YZO)

#define UNZOOMX(width) IDIV(IDIV((width) * XEZO + XEZI - 1, XEZI) \
				* XPZO + XPZI - 1, XPZI)
#define UNZOOMY(height) IDIV(IDIV((height) * YEZO + YEZI - 1, YEZI) \
				* YPZO + YPZI - 1, YPZI)

/* Convert coordinates between screen coordinates (Scr), display	*/
/* coordinates (Dpy), prezoomed	coordinates (Pre), and image coordinates*/
/* (Img).  The difference between Scr and Dpy is that Screen coords take*/
/* into account the pan value, while Display coords have a pan of 0.	*/
/* (The border offset is always taken into account at the actual X call	*/
/* interface.)  The forms ending in 1 refer to the left/top edge of an	*/
/* area, while forms ending in 2 refer to the right/bottom edge of an	*/
/* area.  The forms without 1 or 2 apply to either.  The forms ending	*/
/* in "C" are intended for floating-point Img coordinates and convert	*/
/* to/from integer as well (C is for Cursor).  The integral value of	*/
/* the float is the center of the pixel.  The +/- 0.5 is to compensate	*/
/* for x.0 being in the center of the pixel instead of the edge.  The	*/
/* "fp" forms are not complete formulas and are for internal use by the	*/
/* "C" forms only.  These macros all assume the availability of "biw".	*/

#define X_Scr2Dpy(x) ((x) + XPAN)
#define Y_Scr2Dpy(y) ((y) + YPAN)

#define X_Dpy2Pre(x) IDIV((x) * XEZO + XSUB, XEZI)
#define Y_Dpy2Pre(y) IDIV((y) * YEZO + YSUB, YEZI)

/* Partial formula.  Should subtract 0.5 at the end for true Pre coords	*/
/* but this is always used with Pre2Img which just immediately adds	*/
/* it back in again, so it's omitted for efficiency.			*/
/* Correct formula:  (((x+0.5) * XEZO + XSUB) / XEZI - 0.5)		*/
#define Xfp_Dpy2Pre(x) (((x+0.5) * XEZO + XSUB) / XEZI)
#define Yfp_Dpy2Pre(y) (((y+0.5) * YEZO + YSUB) / YEZI)

#define X_Pre2Img(x) IDIV((x) * XPZO + XPSUB, XPZI)
#define Y_Pre2Img(y) IDIV((y) * YPZO + YPSUB, YPZI)

/* Partial formula.  Should add 0.5 to x/y; see X/Yfp_Dpy2Pre().	*/
#define Xfp_Pre2Img(x) (((x) * XPZO + XPSUB) / XPZI - 0.5)
#define Yfp_Pre2Img(y) (((y) * YPZO + YPSUB) / YPZI - 0.5)

#define X_Dpy2Img(x) X_Pre2Img(X_Dpy2Pre(x))
#define Y_Dpy2Img(y) Y_Pre2Img(Y_Dpy2Pre(y))

#define X_Scr2Img(x) X_Dpy2Img(X_Scr2Dpy(x))
#define Y_Scr2Img(y) Y_Dpy2Img(Y_Scr2Dpy(y))

#define XC_Dpy2Img(x) Xfp_Pre2Img(Xfp_Dpy2Pre((double)x))
#define YC_Dpy2Img(y) Yfp_Pre2Img(Yfp_Dpy2Pre((double)y))

#define XC_Scr2Img(x) XC_Dpy2Img(X_Scr2Dpy((double)x))
#define YC_Scr2Img(y) YC_Dpy2Img(Y_Scr2Dpy((double)y))

/*------*/

#define X1_Img2Pre(x) IDIV((x) * XPZI - XPSUB + XPZO - 1, XPZO)
#define Y1_Img2Pre(y) IDIV((y) * YPZI - YPSUB + YPZO - 1, YPZO)

#define X2_Img2Pre(x) IDIV(((x)+1) * XPZI - XPSUB - 1, XPZO)
#define Y2_Img2Pre(y) IDIV(((y)+1) * YPZI - YPSUB - 1, YPZO)

/* Partial formula.  Should subtract 0.5 at the end for true Pre coords	*/
/* but this is always used with Pre2Dpy which just immediately adds	*/
/* it back in again, so it's omitted for efficiency.			*/
/* Correct formula:  (((x+0.5) * XPZI - XPSUB) / XPZO - 0.5)		*/
#define Xfp_Img2Pre(x) (((x+0.5) * XPZI - XPSUB) / XPZO)
#define Yfp_Img2Pre(y) (((y+0.5) * YPZI - YPSUB) / YPZO)

#define X1_Pre2Dpy(x) IDIV((x) * XEZI - XSUB + XEZO - 1, XEZO)
#define Y1_Pre2Dpy(y) IDIV((y) * YEZI - YSUB + YEZO - 1, YEZO)

#define X2_Pre2Dpy(x) IDIV(((x)+1) * XEZI - XSUB - 1, XEZO)
#define Y2_Pre2Dpy(y) IDIV(((y)+1) * YEZI - YSUB - 1, YEZO)

/* Partial formula.  Should add 0.5 to x/y; see X/Yfp_Img2Pre().	*/
#define Xfp_Pre2Dpy(x) (((x) * XEZI - XSUB) / XEZO - 0.5)
#define Yfp_Pre2Dpy(y) (((y) * YEZI - YSUB) / YEZO - 0.5)

#define X_Dpy2Scr(x) ((x) - XPAN)
#define Y_Dpy2Scr(y) ((y) - YPAN)

#define X1_Img2Dpy(x) X1_Pre2Dpy(X1_Img2Pre(x))
#define Y1_Img2Dpy(y) Y1_Pre2Dpy(Y1_Img2Pre(y))

#define X2_Img2Dpy(x) X2_Pre2Dpy(X2_Img2Pre(x))
#define Y2_Img2Dpy(y) Y2_Pre2Dpy(Y2_Img2Pre(y))

#if 0			/* old */
#define XC_Img2Dpy(x) IDIV(X1_Img2Dpy(x)+X2_Img2Dpy(x), 2)
#define YC_Img2Dpy(y) IDIV(Y1_Img2Dpy(y)+Y2_Img2Dpy(y), 2)
#endif

#define XC_Img2Dpy(x) ROUND(Xfp_Pre2Dpy(Xfp_Img2Pre(x)))
#define YC_Img2Dpy(y) ROUND(Yfp_Pre2Dpy(Yfp_Img2Pre(y)))

#define X1_Img2Scr(x) X_Dpy2Scr(X1_Img2Dpy(x))
#define Y1_Img2Scr(y) Y_Dpy2Scr(Y1_Img2Dpy(y))

#define X2_Img2Scr(x) X_Dpy2Scr(X2_Img2Dpy(x))
#define Y2_Img2Scr(y) Y_Dpy2Scr(Y2_Img2Dpy(y))

#define XC_Img2Scr(x) X_Dpy2Scr(XC_Img2Dpy(x))
#define YC_Img2Scr(y) Y_Dpy2Scr(YC_Img2Dpy(y))

/* Take a (position,size) pair and constrain it to fit within the view	*/
/* area (so start >= 0 and end < view_size).  The '?' is due to the	*/
/* use of unsigned numbers for the height and width.			*/

#define X_ViewConstrain(x,w) { \
      if ((int)(x) < 0) {   (w) -= ( -(x));  (x) = 0;   } \
      if ((int)((x)+(w)) > (int)biw->bim.view_width) \
         (w) = ((int)biw->bim.view_width>(x)) ? biw->bim.view_width-(x) : 0; }
#define Y_ViewConstrain(y,h) { \
      if ((int)(y) < 0) {   (h) -= ( -(y));  (y) = 0;   } \
      if ((int)((y)+(h)) > (int)biw->bim.view_height) \
         (h) = ((int)biw->bim.view_height>(y)) ? biw->bim.view_height-(y) : 0; }

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicBasicImageP_h */
/* DON'T ADD STUFF AFTER THIS #endif */

