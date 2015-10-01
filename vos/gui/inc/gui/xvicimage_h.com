$!****************************************************************************
$!
$! Build proc for MIPL module xvicimage_h
$! VPACK Version 1.8, Wednesday, April 09, 1997, 15:06:52
$!
$! Execute by entering:		$ @xvicimage_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module xvicimage_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvicimage_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xvicimage_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xvicimage_h.bld "STD"
$   else
$      @xvicimage_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvicimage_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvicimage_h.com -mixed -
	-s XvicBasicImage.h XvicBasicImageP.h XvicImage.h XvicImageP.h -
	   XvicImageOverlay.h XvicImageOverlayP.h XvicRegion.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XvicBasicImage.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _XvicBasicImage_h
#define _XvicBasicImage_h

#include <X11/Intrinsic.h>

#ifndef _NO_PROTO
#if !defined(__STDC__) && !defined(__cplusplus)
#define _NO_PROTO
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* version								*/
/* 1.0	Initial release, no version numbers present			*/
/* 1.1	Save modes for bw,color,pseudo independently, add visualType	*/
/*	and workProcActiveCallback					*/
/* 1.2	Support for non-byte data					*/
/* 1.3  Support for fractional-pixel graphics (affects only callback	*/
/*	struct in BasicImage)						*/
/* 1.4  Added XvicDOUBLE_ARG macro in order to support 64-bit machines	*/
/************************************************************************/

#define XvicBasicImageMAJOR_VERSION		1
#define XvicBasicImageMINOR_VERSION		4

/************************************************************************/
/* type defines								*/
/************************************************************************/

typedef struct _XvicBasicImageClassRec	*XvicBasicImageWidgetClass;
typedef struct _XvicBasicImageRec	*XvicBasicImageWidget;

/************************************************************************/
/* extern class								*/
/************************************************************************/

externalref WidgetClass xvicBasicImageWidgetClass;

/************************************************************************/
/* fast subclass define							*/
/************************************************************************/

#ifndef XvicIsBasicImage
#define XvicIsBasicImage(w)	XtIsSubclass(w, xvicBasicImageWidgetClass)
#endif /* XvicIsBasicImage */

/************************************************************************/
/* class strings							*/
/************************************************************************/

#define XvicCCmapCells		"CmapCells"
#define XvicCColormapPolicy	"ColormapPolicy"
#define XvicCConstrainPan	"ConstrainPan"
#define XvicCDataRange		"DataRange"
#define XvicCDataSavePolicy	"DataSavePolicy"
#define XvicCDataType		"DataType"
#define XvicCDitherMode		"DitherMode"
#define XvicCEnableDirectColor	"EnableDirectColor"
#define XvicCImageMode		"ImageMode"
#define XvicCLutType		"LutType"
#define XvicCLut16Type		"Lut16Type"
#define XvicCMemory		"Memory"
#define XvicCPan		"Pan"
#define XvicCStretchPolicy	"StretchPolicy"
#define XvicCSubpixelPan	"SubpixelPan"
#define XvicCVisualType		"VisualType"
#define XvicCWorkProcPolicy	"WorkProcPolicy"
#define XvicCZoom		"Zoom"
#define XvicCZoomDenom		"ZoomDenom"
#define XvicCZoomNumer		"ZoomNumer"

/************************************************************************/
/* representation type strings						*/
/************************************************************************/

#define XvicRColormapPolicy	"ColormapPolicy"
#define XvicRConstrainPan	"ConstrainPan"
#define XvicRDataSavePolicy	"DataSavePolicy"
#define XvicRDataType		"DataType"
#define XvicRDitherMode		"DitherMode"
#define XvicRDouble		"Double"
#define XvicRImageMode		"ImageMode"
#define XvicRLutType		"LutType"
#define XvicRStretchPolicy	"StretchPolicy"
#define XvicRVisualType		"VisualType"
#define XvicRWorkProcPolicy	"WorkProcPolicy"

/************************************************************************/
/* instance strings							*/
/************************************************************************/

#define XvicNblueLevels		"blueLevels"
#define XvicNbwColormapPolicy	"bwColormapPolicy"
#define XvicNbwDither		"bwDither"
#define XvicNbwStretchPolicy	"bwStretchPolicy"
#define XvicNbwVisualType	"bwVisualType"
#define XvicNcolorColormapPolicy "colorColormapPolicy"
#define XvicNcolorDither	"colorDither"
#define XvicNcolorStretchPolicy	"colorStretchPolicy"
#define XvicNcolorVisualType	"colorVisualType"
#define XvicNcolormapPolicy	"colormapPolicy"
#define XvicNconstrainPan	"constrainPan"
#define XvicNdataSavePolicy	"dataSavePolicy"
#define XvicNdataType		"dataType"
#define XvicNditherMode		"ditherMode"
#define XvicNenableDirectColor	"enableDirectColor"
#define XvicNexposeCallback	"exposeCallback"
#define XvicNgrayLevels		"grayLevels"
#define XvicNgreenLevels	"greenLevels"
#define XvicNimageHeight	"imageHeight"
#define XvicNimageMode		"imageMode"
#define XvicNimageWidth		"imageWidth"
#define XvicNimageZoom		"imageZoom"
#define XvicNlutType		"lutType"
#define XvicNlut16Type		"lut16Type"
#define XvicNmaximumMemory	"maximumMemory"
#define XvicNoutputDataMax	"outputDataMax"
#define XvicNpseudoColormapPolicy "pseudoColormapPolicy"
#define XvicNpseudoDither	"pseudoDither"
#define XvicNpseudoStretchPolicy "pseudoStretchPolicy"
#define XvicNpseudoVisualType	"pseudoVisualType"
#define XvicNrawDataMax		"rawDataMax"
#define XvicNrawDataMin		"rawDataMin"
#define XvicNredLevels		"redLevels"
#define XvicNresizeCallback	"resizeCallback"
#define XvicNscaledDataMax	"scaledDataMax"
#define XvicNstretchPolicy	"stretchPolicy"
#define XvicNtileHeight		"tileHeight"
#define XvicNtileWidth		"tileWidth"
#define XvicNviewHeight		"viewHeight"
#define XvicNviewWidth		"viewWidth"
#define XvicNvisibleAreaCallback "visibleAreaCallback"
#define XvicNvisualType		"visualType"
#define XvicNworkProcActiveCallback "workProcActiveCallback"
#define XvicNworkProcPolicy	"workProcPolicy"
#define XvicNxPan		"xPan"
#define XvicNxPreSubpixelPan	"xPreSubpixelPan"
#define XvicNxPreZoomIn		"xPreZoomIn"
#define XvicNxPreZoomOut	"xPreZoomOut"
#define XvicNxSubpixelPan	"xSubpixelPan"
#define XvicNxZoom		"xZoom"
#define XvicNxZoomIn		"xZoomIn"
#define XvicNxZoomOut		"xZoomOut"
#define XvicNyPan		"yPan"
#define XvicNyPreSubpixelPan	"yPreSubpixelPan"
#define XvicNyPreZoomIn		"yPreZoomIn"
#define XvicNyPreZoomOut	"yPreZoomOut"
#define XvicNySubpixelPan	"ySubpixelPan"
#define XvicNyZoom		"yZoom"
#define XvicNyZoomIn		"yZoomIn"
#define XvicNyZoomOut		"yZoomOut"

/************************************************************************/
/* enumerated constants							*/
/************************************************************************/

/* ColormapPolicy */
#define XvicFULL		10
#define XvicHALF		11
#define XvicDITHER		12
#define XvicALLOC		13
#define XvicFULL_COLOR		14

/* ConstrainPan */
#define XvicNONE		20
#define XvicX_ONLY		21
#define XvicY_ONLY		22
#define XvicBOTH		23

/* DataSavePolicy */
/* #define XvicNONE */
#define XvicRAW			31
#define XvicXIMAGE		32
#define XvicPIXMAP		33

/* DitherMode */
/* #define XvicNONE */
#define XvicORDERED		41
#define XvicKAGELS		42

/* ImageMode */
#define XvicCOLOR		50
#define XvicBW			51

/* LutType */
#define XvicSTRETCH		60
/* #define XvicRAW */
#define XvicPSEUDO		62
#define XvicPSEUDO_ONLY		63

/* StretchPolicy */
#define XvicUSE_HW		70
#define XvicUSE_SW		71

/* WorkProcPolicy */
/* #define XvicNONE */
#define XvicREAD		81
#define XvicALL			82

/* Callback Reasons */
#define XvicCR_EXPOSE		90
#define XvicCR_RESIZE		91
#define XvicCR_VISIBLE_AREA	92
#define XvicCR_WORK_PROC_ACTIVE	93
/* Continued in XvicImage.h */

/* Memory disposition for XvicImageWrite() */
#define XvicMEMORY_APPLIC	120
#define XvicMEMORY_WIDGET	121
#define XvicMEMORY_SHARED	122

/* Visual Type */
#define XvicUSE_DEFAULT		130
#define XvicUSE_8BIT		131
#define XvicUSE_24BIT		132

/* Data Type */
#define XvicBYTE		140
/* #define XvicHALF		11	defined by ColormapPolicy above */
#define XvicUHALF		142
/* #define XvicFULL		10	defined by ColormapPolicy above */
#define XvicUFULL		144
#define XvicREAL		145
#define XvicDOUBLE		146

/************************************************************************/
/* bit flags								*/
/************************************************************************/

/* For XvicImageCallbackStruct->flags, used with XvicCR_VISIBLE_AREA */
#define XvicPAN_CHANGED		0x0001
#define XvicSUBPIXEL_CHANGED	0x0002
#define XvicSIZE_CHANGED	0x0004
#define XvicZOOM_CHANGED	0x0008
#define XvicMODE_CHANGED	0x0010
#define XvicDITHER_CHANGED	0x0020
#define XvicRANGE_CHANGED	0x0040

/************************************************************************/
/* typedef's								*/
/************************************************************************/

/* The data type defines may need to change on some platforms.  They	*/
/* must be the closest data type possible to the description listed in	*/
/* the doc file.  If a platform needs different types, use #if's here.	*/

typedef unsigned char XvicByte;
typedef short int XvicHalf;
typedef unsigned short int XvicUHalf;
typedef int XvicFull;
typedef unsigned int XvicUFull;
typedef float XvicReal;
typedef double XvicDouble;

/************************************************************************/
/* macros								*/
/************************************************************************/

/* This macro must be used to pass doubles in a XtArgList or VarArgs	*/
/* call.  It compensates for the fact that Xt expects the arg to be	*/
/* passed by value when sizeof(data) <= sizeof(XtPointer) (e.g. 64-bit	*/
/* pointer machines), and by address otherwise.  Consistency!		*/

/* We really want to do this:						*/
/* #if sizeof(XtPointer) >= sizeof(double)				*/
/* but can't do sizeof() in the preprocessor, so we do the next best	*/
/* thing.  The ugly typecasting in the first case is to prevent the	*/
/* compiler from *converting* double to an int type when XtSetArg	*/
/* does its typecast to XtArgVal; we want to just copy the bits.	*/

#define XvicDOUBLE_ARG(x) ((sizeof(XtArgVal)>=sizeof(double)) ? \
	(XtArgVal)(*((long *)(&x))) : (XtArgVal)&x)

/************************************************************************/
/* public structures							*/
/************************************************************************/

/* Callback structure */

typedef struct _XvicImageCallbackStruct
{
	int	reason;
	XEvent	*event;
	int	x;
	int	y;
	int	width;
	int	height;
	int	prezoom_x;
	int	prezoom_y;
	int	prezoom_width;
	int	prezoom_height;
	int	new_view_width;
	int	new_view_height;
	unsigned int	flags;
	String	*input_params;
	Cardinal input_num_params;
	int	x_pan;
	int	y_pan;
	Boolean	on_screen;
	double	x_fp;
	double	y_fp;
} XvicImageCallbackStruct;

/* Image data structure */

typedef struct _XvicImageData
{
	unsigned char	*bw_pixels;
	unsigned char	*red_pixels;
	unsigned char	*grn_pixels;
	unsigned char	*blu_pixels;
	int		x;
	int		y;
	int		width;
	int		height;
	int		memory_control;
	int		line_width;		/* bytes*/
	int		start_offset;		/* bytes*/
} XvicImageData;

/************************************************************************/
/* public functions							*/
/************************************************************************/

#ifdef _NO_PROTO

extern Widget XvicCreateBasicImage();
extern void XvicImageClear();
extern void XvicImageDisplayBounds();
extern void XvicImageGetColorLUT();
extern void XvicImageGetColorLUT16();
extern void XvicImageGetMonoLUT();
extern void XvicImageGetMonoLUT16();
extern void XvicImageSetColorLUT();
extern void XvicImageSetColorLUT16();
extern void XvicImageSetMonoLUT();
extern void XvicImageSetMonoLUT16();
extern void XvicImageWrite();

#else

extern Widget XvicCreateBasicImage(
		Widget		parent,
		char		*name,
		ArgList		args,
		Cardinal	argCount);

extern void XvicImageClear(
		Widget		w);

extern void XvicImageDisplayBounds(
		Widget		w,
		int		*x1,
		int		*y1,
		int		*x2,
		int		*y2);

extern void XvicImageGetColorLUT(
		Widget		w,
		int		*red_lut,
		int		*green_lut,
		int		*blue_lut);

extern void XvicImageGetColorLUT16(
		Widget		w,
		int		*red_lut,
		int		*green_lut,
		int		*blue_lut,
		int		lut_size);

extern void XvicImageGetMonoLUT(
		Widget		w,
		int		*lut);

extern void XvicImageGetMonoLUT16(
		Widget		w,
		int		*lut,
		int		lut_size);

extern void XvicImageSetColorLUT(
		Widget		w,
		int		*red_lut,
		int		*green_lut,
		int		*blue_lut);

extern void XvicImageSetColorLUT16(
		Widget		w,
		int		*red_lut,
		int		*green_lut,
		int		*blue_lut,
		int		lut_size);

extern void XvicImageSetMonoLUT(
		Widget		w,
		int		*lut);

extern void XvicImageSetMonoLUT16(
		Widget		w,
		int		*lut,
		int		lut_size);

extern void XvicImageWrite(
		Widget		w,
		XvicImageData	*image,
#if NeedWidePrototypes
		int		new_data);
#else
		Boolean		new_data);
#endif /* NeedWidePrototypes */

#endif /* _NO_PROTO */

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicBasicImage_h */
/* DON'T ADD STUFF AFTER THIS #endif */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicBasicImageP.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImage.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _XvicImage_h
#define _XvicImage_h

#include <X11/Intrinsic.h>
#include "XvicBasicImage.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* version								*/
/* 1.0	Initial release.  No version present				*/
/* 1.1  Added HW overlay enable/disable, fix various overlay problems	*/
/* 1.2	Added fractional-pixel graphics and cursor locations		*/
/* 1.3  Added XvicDOUBLE_ARG macro in order to support 64-bit machines	*/
/*	(def'd in BasicImage but changes cursor(XY)fp calling sequence)	*/
/************************************************************************/

#define XvicImageMAJOR_VERSION		1
#define XvicImageMINOR_VERSION		3

/************************************************************************/
/* type defines								*/
/************************************************************************/

typedef struct _XvicImageClassRec	*XvicImageWidgetClass;
typedef struct _XvicImageRec		*XvicImageWidget;

/************************************************************************/
/* extern class								*/
/************************************************************************/

externalref WidgetClass xvicImageWidgetClass;

/************************************************************************/
/* fast subclass define							*/
/************************************************************************/

#ifndef XvicIsImage
#define XvicIsImage(w) 		XtIsSubclass(w, xvicImageWidgetClass)
#endif /* XvicIsImage */

/************************************************************************/
/* class strings							*/
/************************************************************************/

#define XvicCCursor		"Cursor"
#define XvicCCursorBackground	"CursorBackground"
#define XvicCCursorForeground	"CursorForeground"
#define XvicCCursorLoc		"CursorLoc"
#define XvicCCursorLocFp	"CursorLocFp"
#define XvicCCursorMode		"CursorMode"
#define XvicCEnableHWOverlay	"EnableHWOverlay"
#define XvicCScrollBarDisplayPolicy "ScrollBarDisplayPolicy"
#define XvicCTrackFloatingCursor "TrackFloatingCursor"

/************************************************************************/
/* representation type strings						*/
/************************************************************************/

/* Note that XvicRSBDP is different than XmRSBDP because Xvic allows	*/
/* XvicNEVER as a value.  Class & instance names can be the same.	*/

#define XvicRCursorMode		"CursorMode"
#define XvicRScrollBarDisplayPolicy "XvicScrollBarDisplayPolicy"

/************************************************************************/
/* instance strings							*/
/************************************************************************/

#define XvicNcursor		"cursor"
#define XvicNcursorBackground	"cursorBackground"
#define XvicNcursorCallback	"cursorCallback"
#define XvicNcursorForeground	"cursorForeground"
#define XvicNcursorMode		"cursorMode"
#define XvicNcursorX		"cursorX"
#define XvicNcursorXfp		"cursorXfp"
#define XvicNcursorY		"cursorY"
#define XvicNcursorYfp		"cursorYfp"
#define XvicNenableHWOverlay	"enableHWOverlay"
#define XvicNinputCallback	"inputCallback"
#define XvicNpanCallback	"panCallback"
#define XvicNscrollBarDisplayPolicy "scrollBarDisplayPolicy"
#define XvicNtrackFloatingCursor "trackFloatingCursor"

/************************************************************************/
/* enumerated constants							*/
/************************************************************************/

/* Callback Reasons (continued from XvicBasicImage.h) */
#define XvicCR_PAN		94
#define XvicCR_INPUT		95
#define XvicCR_CURSOR		96

/* ScrollBarDisplayPolicy */
#define XvicSTATIC		210
#define XvicAS_NEEDED		211
#define XvicNEVER		212

/* CursorMode */
#define XvicFLOATING		220
#define XvicPLANTED		221

/* Justify settings for text drawing routines */
#define XvicJUST_LEFT		230
#define XvicJUST_CENTER		231
#define XvicJUST_RIGHT		232

/************************************************************************/
/* public structures							*/
/************************************************************************/

/* Callback structure is defined by XvicBasicImage. */

typedef int XvicColor;
typedef int XvicGC;
typedef int XvicID;

typedef struct _XvicArc
{
	double	x;
	double	y;
	double	width;
	double	height;
	int	angle1;
	int	angle2;
} XvicArc;

typedef struct _XvicPoint
{
	double	x;
	double	y;
} XvicPoint;

typedef struct _XvicRectangle
{
	double	x;
	double	y;
	double	width;
	double	height;
} XvicRectangle;

typedef struct _XvicSegment
{
	double	x1;
	double	y1;
	double	x2;
	double	y2;
} XvicSegment;

/************************************************************************/
/* public functions							*/
/************************************************************************/

#ifdef _NO_PROTO

extern Widget XvicCreateImage();
extern void XvicImageChangeGC();
extern XvicGC XvicImageCreateGC();
extern XvicGC XvicImageCreateRubberGC();
extern XvicID XvicImageDrawArc();
extern XvicID XvicImageDrawArcs();
extern XvicID XvicImageDrawBitmap();
extern XvicID XvicImageDrawImageString();
extern XvicID XvicImageDrawImageString16();
extern XvicID XvicImageDrawLine();
extern XvicID XvicImageDrawLines();
extern XvicID XvicImageDrawPoint();
extern XvicID XvicImageDrawPoints();
extern XvicID XvicImageDrawRectangle();
extern XvicID XvicImageDrawRectangles();
extern XvicID XvicImageDrawSegments();
extern XvicID XvicImageDrawString();
extern XvicID XvicImageDrawString16();
extern XvicID XvicImageDrawText();
extern XvicID XvicImageDrawText16();
extern void XvicImageEraseObject();
extern void XvicImageEraseOverlay();
extern XvicID XvicImageFillArc();
extern XvicID XvicImageFillArcs();
extern XvicID XvicImageFillPolygon();
extern XvicID XvicImageFillRectangle();
extern XvicID XvicImageFillRectangles();
extern void XvicImageFreeGC();
extern XvicColor XvicImageGetGrColor();
extern XvicColor XvicImageGetGrColorRGB();
extern void XvicImageMoveObject();
extern void XvicImageSetDashes();
extern void XvicImageSetFontCursor();
extern void XvicImageSetGlyphCursor();
extern void XvicImageSetPixmapCursor();

#else

extern Widget XvicCreateImage(
		Widget		parent,
		char		*name,
		ArgList		args,
		Cardinal	argCount);

extern void XvicImageChangeGC(
		Widget		w,
		XvicGC		gc,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicGC XvicImageCreateGC(
		Widget		w,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicGC XvicImageCreateRubberGC(
		Widget		w,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicID XvicImageDrawArc(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height,
		int		angle1,
		int		angle2);

extern XvicID XvicImageDrawArcs(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicArc		*arcs,
		int		narcs);

extern XvicID XvicImageDrawBitmap(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		Pixmap		bitmap,
		unsigned int	width,
		unsigned int	height,
		int		hot_x,
		int		hot_y);

extern XvicID XvicImageDrawImageString(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	fg,
		XvicColor	bg,
		double		x,
		double		y,
		char		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawImageString16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	fg,
		XvicColor	bg,
		double		x,
		double		y,
		XChar2b		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawLine(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x1,
		double		y1,
		double		x2,
		double		y2);

extern XvicID XvicImageDrawLines(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		mode);

extern XvicID XvicImageDrawPoint(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y);

extern XvicID XvicImageDrawPoints(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		mode);

extern XvicID XvicImageDrawRectangle(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height);

extern XvicID XvicImageDrawRectangles(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicRectangle	*rectangles,
		int		nrectangles);

extern XvicID XvicImageDrawSegments(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicSegment	*segments,
		int		nsegments);

extern XvicID XvicImageDrawString(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		char		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawString16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XChar2b		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawText(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XTextItem	*items,
		int		nitems,
		int		justify);

extern XvicID XvicImageDrawText16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XTextItem16	*items,
		int		nitems,
		int		justify);

extern void XvicImageEraseObject(
		Widget		w,
		XvicID		id);

extern void XvicImageEraseOverlay(
		Widget		w);

extern XvicID XvicImageFillArc(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height,
		int		angle1,
		int		angle2);

extern XvicID XvicImageFillArcs(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicArc		*arcs,
		int		narcs);

extern XvicID XvicImageFillPolygon(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		shape,
		int		mode);

extern XvicID XvicImageFillRectangle(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height);

extern XvicID XvicImageFillRectangles(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicRectangle	*rectangles,
		int		nrectangles);

extern void XvicImageFreeGC(
		Widget		w,
		XvicGC		gc);

extern XvicColor XvicImageGetGrColor(
		Widget		w,
		XColor		*xcolor);

extern XvicColor XvicImageGetGrColorRGB(
		Widget		w,
		int		red,
		int		green,
		int		blue);

extern void XvicImageMoveObject(
		Widget		w,
		XvicID		id,
		double		delta_x,
		double		delta_y);

extern void XvicImageSetDashes(
		Widget		w,
		XvicGC		gc,
		int		dash_offset,
		char		dash_list[],
		int		n);

extern void XvicImageSetFontCursor(
		Widget		w,
		unsigned int	shape);

extern void XvicImageSetGlyphCursor(
		Widget		w,
		Font		source_font,
		Font		mask_font,
		unsigned int	source_char,
		unsigned int	mask_char);

extern void XvicImageSetPixmapCursor(
		Widget		w,
		Pixmap		source,
		Pixmap		mask,
		unsigned int	x,
		unsigned int	y);

#endif /* _NO_PROTO */

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImage_h */
/* DON'T ADD STUFF AFTER THIS #endif */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImageP.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _XvicImageP_h
#define _XvicImageP_h

/* Motif is required for XvicImage, unlike XvicBasicImage */

#include "XvicImage.h"
#include <Xm/Xm.h>

#if XmVERSION==1 && XmREVISION<=1	/* Some things are different in 1.2...*/
#define MOTIF_1_1
#endif

#include <Xm/XmP.h>
#include "XvicBasicImageP.h"
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#ifndef MOTIF_1_1
#include <Xm/PrimitiveP.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* Class structure							*/
/************************************************************************/

typedef struct _XvicImageClassPart
{
   XtPointer	extension;	/* Pointer to extension record */
} XvicImageClassPart;

typedef struct _XvicImageClassRec
{
   CoreClassPart		core_class;
   XmPrimitiveClassPart		primitive_class;
   XvicBasicImageClassPart	basic_image_class;
   XvicImageClassPart		image_class;
} XvicImageClassRec;

externalref XvicImageClassRec xvicImageClassRec;

/************************************************************************/
/* Structures needed by Instance					*/
/************************************************************************/

typedef struct _GrColor
{
   XColor xcolor;	/* desired values and pixel to use if not dither */
   Pixmap gc_tile;	/* Pixmap to use for dither tiling or NULL */
   int width, height;	/* w/h of Pixmap (if present) */
   Boolean active;	/* True if this color is in active use (not freed) */
   Boolean alloc_def;	/* True if this color is alloced from sys cmap */
   Boolean alloc_pvt;	/* True if alloced from private cmap (may be both) */
			/* (private cmap may be biw or overlay) */
} GrColor;

typedef struct _GrGC
{
   XGCValues values;	/* Values to set in the real GC */
   unsigned long mask;	/* (overridden by anything needed in SetUpGraphicsGC) */
   int dash_offset;	/* Dashes must be set separately... */
   char *dash_list;
   int num_dashes;
   Boolean is_rubber;	/* True if this is for rubber-banding */
   Boolean active;	/* True if this color is in active use (not freed) */
   GC gc;		/* The real X GC */
} GrGC;

/************************************************************************/
/* Object structures.  GrAny contains the common elements, and each	*/
/* graphics element type adds its own parameters.  This is modeled	*/
/* somewhat after the XEvent structure.  Gee, C++ sure would be handy	*/
/* here...								*/
/************************************************************************/

typedef enum {
	GrFillArc, GrFillArcs, GrFillPolygon, GrFillRectangle, GrFillRectangles,
	GrArc, GrArcs, GrBitmap, GrLine, GrLines, GrPoint, GrPoints,
	GrRectangle, GrRectangles, GrSegments,
	GrImageString, GrImageString16, GrString, GrString16, GrText, GrText16
} GrType;

typedef struct _GrAnyObject {
   GrType type;
   XvicID id;
   XvicColor color;
   XvicGC gc;
   struct _XvicRectFp {		/* like _XvicRect but floating-point */
      double x1, y1;
      double x2, y2;
   } bounds;			/* in FP Img coordinates */
} GrAnyObject;

typedef struct _GrArcObject {
   GrAnyObject any;
   double x, y;
   double width, height;
   int angle1, angle2;
} GrArcObject;

typedef struct _GrArcsObject {
   GrAnyObject any;
   XvicArc *arcs;
   int narcs;
} GrArcsObject;

typedef struct _GrBitmapObject {
   GrAnyObject any;
   double x, y;
   Pixmap bitmap;
   unsigned int width, height;
   int hot_x, hot_y;
} GrBitmapObject;

typedef struct _GrLineObject {
   GrAnyObject any;
   double x1, y1, x2, y2;
} GrLineObject;

typedef struct _GrLinesObject {
   GrAnyObject any;
   XvicPoint *points;
   int npoints;
   int mode;
   int shape;			/* for FillPolygon only */
} GrLinesObject;

typedef struct _GrPointObject {
   GrAnyObject any;
   double x, y;
} GrPointObject;

typedef struct _GrRectangleObject {
   GrAnyObject any;
   double x, y;
   double width, height;
} GrRectangleObject;

typedef struct _GrRectanglesObject {
   GrAnyObject any;
   XvicRectangle *rectangles;
   int nrectangles;
} GrRectanglesObject;

typedef struct _GrSegmentsObject {
   GrAnyObject any;
   XvicSegment *segments;
   int nsegments;
} GrSegmentsObject;

typedef struct _GrAnyStringObject {
   GrAnyObject any;
   int justify;
   int ascent;		/* Calculated by CalcStringBounds() */
   int descent;
   int width;
   int rbearing;
   int lbearing;
} GrAnyStringObject;

typedef struct _GrStringObject {
   GrAnyStringObject anystr;
   XvicColor fg;		/* bg is any.color to make drawing easier */
   double x, y;
   char *string;
   int length;
} GrStringObject;

typedef struct _GrString16Object {
   GrAnyStringObject anystr;
   XvicColor fg;		/* bg is any.color to make drawing easier */
   double x, y;
   XChar2b *string;
   int length;
} GrString16Object;

typedef struct _GrTextObject {
   GrAnyStringObject anystr;
   double x, y;
   XTextItem *items;
   int nitems;
} GrTextObject;

typedef struct _GrText16Object {
   GrAnyStringObject anystr;
   double x, y;
   XTextItem16 *items;
   int nitems;
} GrText16Object;

/* Note that the objects pointed at by gr_object array in the instance	*/
/* record are allocated to the *individual* size (sizeof(GrLineObject),	*/
/* etc.) not the size of the union (sizeof(GrObject)).			*/

typedef union _GrObject
{
   GrType			type;
   GrAnyObject			any;
   GrArcObject			arc;		/* Also for FillArc */
   GrArcsObject			arcs;		/* Also for FillArcs */
   GrBitmapObject		bitmap;
   GrLineObject			line;
   GrLinesObject		lines;	/* Also for FillPolygon and Points */
   GrPointObject		point;
   GrRectangleObject		rectangle;	/* Also for FillRectangle */
   GrRectanglesObject		rectangles;	/* Also for FillRectangles */
   GrSegmentsObject		segments;
   GrAnyStringObject		anystr;		/* For all the following... */
   GrStringObject		string;		/* Also for ImageString */
   GrString16Object		string16;	/* Also for ImageString16 */
   GrTextObject			text;
   GrText16Object		text16;
} GrObject;

/************************************************************************/
/* OverlayType and OverlayInfo describe the SERVER_OVERLAY_VISUALS	*/
/* property of servers that have hardware overlays.  This really should	*/
/* be in an X include somewhere, if overlays were officially supported.	*/
/* The VITec documentation (the only docs I could find) defines only	*/
/* three fields per overlay visual.  However, the HP and SGI define	*/
/* four fields.  The R6 Fresco *source* (Xdisplay.cxx) claims that the	*/
/* fourth field is the overlay "layer", numbered from top-to-bottom.	*/
/* It also calls the "value" field "transparent" and uses long instead	*/
/* of unsigned long (like the ViTEC docs show).  Ahh, standardization!!	*/
/************************************************************************/

typedef enum { NotTransparent = 0, TransparentPixel = 1, TransparentMask = 2 }
	OverlayType;

typedef struct _OverlayInfo
{
   unsigned long vid;
   OverlayType type;
   unsigned long value;
   unsigned long layer;
} OverlayInfo;

/************************************************************************/
/* Instance structure							*/
/************************************************************************/

typedef struct _XvicImagePart
{
   XtCallbackList	cursor_callback;
   XtCallbackList	input_callback;
   XtCallbackList	pan_callback;
   XtCallbackList	unrealize_callback;

   unsigned char	scrollbar_display_policy;
   unsigned char	cursor_mode;
   int			cursor_x;
   int			cursor_y;
   double		cursor_x_fp;
   double		cursor_y_fp;

   String		cursor;
   String		cursor_foreground;
   String		cursor_background;

   Boolean		track_floating_cursor;

   Boolean		enable_hw_overlay;

   /* Private members - not directly settable via resources */

   XmScrolledWindowWidget scroll_win;		/* ScrolledWindow parent */
   XmScrollBarWidget	h_scrollbar;		/* Horizontal scrollbar */
   XmScrollBarWidget	v_scrollbar;		/* Vertical scrollbar */

   Boolean		hsb_managed;		/* True iff currently managed */
   Boolean		vsb_managed;

   int			x_mouse_scr_pan;	/* Position of last mouse pan */
   int			y_mouse_scr_pan;	/* in Scr coordinates */

   Boolean		in_scrollbar;	/* True iff SetValues from SB callback*/

   GrColor		*gr_colors;	/* array of graphics colors */
   int			num_gr_colors;

   GrGC			*gr_gc_list;	/* array of user GC structs */
   int			num_gr_gc;

   GrObject		**gr_objects;	/* array of pointers to GrObjects */
   int			num_gr_objects;
   XvicID		max_gr_id;	/* max ID for assigning new ones */

   Pixmap		curs_source;
   Pixmap		curs_mask;
   unsigned int		curs_hot_x;
   unsigned int		curs_hot_y;
   unsigned int		curs_width;
   unsigned int		curs_height;
   Cursor		x_cursor;	/* X ID of cursor */

   GrColor		curs_fg;	/* For drawing the planted cursor */
   GrColor		curs_bg;
   GC			curs_fg_gc;
   GC			curs_bg_gc;
   Boolean		plant_set;	/* True if plant cur is ready to draw */
   Boolean		erasing_cursor;	/* True if plant cur is being erased */
   double		plant_curs_x;	/* Img coordinates of planted cursor */
   double		plant_curs_y;

   Visual		*overlay_visual;
   int			overlay_depth;
   OverlayType		overlay_type;
   unsigned long	overlay_value;
   Widget		overlay_widget;
   Colormap		overlay_colormap;
   int			overlay_cmap_size;
   GC			overlay_pan_gc;

   int			default_cmap_refcnt[CMAP_SIZE_MAX];
			/* below is for either biw->cmap or overlay->cmap */
   int			private_cmap_refcnt[CMAP_SIZE_MAX];

} XvicImagePart;

typedef struct _XvicImageRec
{
   CorePart		core;
   XmPrimitivePart	primitive;
   XvicBasicImagePart	bim;		/* short name cuz it's used a bunch */
   XvicImagePart	image;
} XvicImageRec;

/************************************************************************/
/* Private functions needed by other modules				*/
/************************************************************************/

#ifdef _NO_PROTO

extern void _XvicImageSetFontCursor();
extern void _XvicImageSetGlyphCursor();
extern void _XvicImageSetPixmapCursor();
extern void _XvicImageSetStringCursor();

#else

extern void _XvicImageSetFontCursor(
		XvicImageWidget	iw,
		unsigned int	shape);

extern void _XvicImageSetGlyphCursor(
		XvicImageWidget	iw,
		Font		source_font,
		Font		mask_font,
		unsigned int	source_char,
		unsigned int	mask_char);

extern void _XvicImageSetPixmapCursor(
		XvicImageWidget	iw,
		Pixmap		source,
		Pixmap		mask,
		unsigned int	x,
		unsigned int	y);

extern void _XvicImageSetStringCursor(
		XvicImageWidget iw,
		char *name);

#endif /* _NO_PROTO */

/************************************************************************/
/* Macros needed by implementation					*/
/************************************************************************/

#define CURSORFONT_NAME		"cursor"
#define DEFAULT_CURSOR		"crosshair"
#define DEFAULT_CURSOR_SHAPE	XC_crosshair

/* This value is used to indicate that the cursor resource values are	*/
/* not set.  It is possible this could be a valid value, but highly	*/
/* unlikely.  If the user is 32K pixels off the edge of the image,	*/
/* it's his own problem!  The STR form is used as a string default	*/
/* in the resource list for doubles.					*/
#define CURSOR_NO_LOC		-32767
#define CURSOR_NO_LOC_STR	"-32767"

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImageP_h */
/* DON'T ADD STUFF AFTER THIS #endif */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImageOverlay.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _XvicImageOverlay_h
#define _XvicImageOverlay_h

#include <X11/Intrinsic.h>

#ifndef _NO_PROTO
#if !defined(__STDC__) && !defined(__cplusplus)
#define _NO_PROTO
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* type defines								*/
/************************************************************************/

typedef struct _XvicImageOverlayClassRec *XvicImageOverlayWidgetClass;
typedef struct _XvicImageOverlayRec	*XvicImageOverlayWidget;

/************************************************************************/
/* extern class								*/
/************************************************************************/

externalref WidgetClass xvicImageOverlayWidgetClass;

/************************************************************************/
/* fast subclass define							*/
/************************************************************************/

#ifndef XvicIsImageOverlay
#define XvicIsImageOverlay(w)	XtIsSubclass(w, xvicImageOverlayWidgetClass)
#endif /* XvicIsImageOverlay */

/************************************************************************/
/* class strings							*/
/************************************************************************/

#define XvicCVisual		"Visual"

/************************************************************************/
/* representation type strings						*/
/************************************************************************/

#define XvicRVisual		"Visual"

/************************************************************************/
/* instance strings							*/
/************************************************************************/

#define XvicNvisual		"visual"

/************************************************************************/
/* enumerated constants							*/
/************************************************************************/

/* None */

/************************************************************************/
/* public structures							*/
/************************************************************************/

/* None */

/************************************************************************/
/* public functions							*/
/************************************************************************/

/* None */

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImageOverlay_h */
/* DON'T ADD STUFF AFTER THIS #endif */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImageOverlayP.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _XvicImageOverlayP_h
#define _XvicImageOverlayP_h

#include "XvicImageOverlay.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* Method function typedefs						*/
/************************************************************************/

/* None */

/************************************************************************/
/* Class structure							*/
/************************************************************************/

typedef struct _XvicImageOverlayClassPart
{
   XtPointer		extension;	/* Pointer to extension record */
} XvicImageOverlayClassPart;

typedef struct _XvicImageOverlayClassRec
{
   CoreClassPart		core_class;
   XvicImageOverlayClassPart	image_overlay_class;
} XvicImageOverlayClassRec;

externalref XvicImageOverlayClassRec xvicImageOverlayClassRec;

/************************************************************************/
/* Structures needed by Instance					*/
/************************************************************************/

/* None */

/************************************************************************/
/* Instance structure							*/
/************************************************************************/

typedef struct _XvicImageOverlayPart
{
   Visual		*visual;

} XvicImageOverlayPart;

typedef struct _XvicImageOverlayRec
{
   CorePart		core;
   XvicImageOverlayPart	ov;		/* short name cuz it's used a bunch */
} XvicImageOverlayRec;

/************************************************************************/
/* Private functions needed by other modules				*/
/************************************************************************/

/* None */

/************************************************************************/
/* Macros needed by implementation					*/
/************************************************************************/

/* None */

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImageOverlayP_h */
/* DON'T ADD STUFF AFTER THIS #endif */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicRegion.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _Xvicregion_h
#define _Xvicregion_h

#ifndef _NO_PROTO
#if !defined(__STDC__) && !defined(__cplusplus)
#define _NO_PROTO
#endif
#endif

/************************************************************************/
/* Utilities for handling regions.  The functionality here closely	*/
/* mimics the X Region routines, but the X routines use short ints	*/
/* for coordinates, and we need full ints.  These routines are intended	*/
/* for internal use of the Xvic routines only.				*/
/************************************************************************/

typedef struct __XvicRect {		/* Application-visible structure */
   int x1, y1;
   int x2, y2;
} _XvicRect;

typedef struct __XvicRegion {		/* Opaque structure */
   int num_rects;
   int rects_alloc;
   _XvicRect *rects;
} _XvicRegion;

#ifdef _NO_PROTO

int _XvicRectIntersectRect();
int _XvicRegionBounds();
_XvicRegion *_XvicRegionCreate();
_XvicRegion *_XvicRegionCreateIntersect();
_XvicRegion *_XvicRegionCreateRect();
void _XvicRegionDestroy();
int _XvicRegionGetNumRects();
_XvicRect *_XvicRegionGetRectangles();
int _XvicRegionHasRect();
int _XvicRegionIntersect();
int _XvicRegionIsEmpty();
int _XvicRegionOffset();
int _XvicRegionSubtract();
int _XvicRegionUnion();

#else

int _XvicRectIntersectRect(
		_XvicRect *r1,
		_XvicRect *r2,
		_XvicRect *dest);
int _XvicRegionBounds(
		_XvicRegion *rgn,
		_XvicRect *rect);
_XvicRegion *_XvicRegionCreate();
_XvicRegion *_XvicRegionCreateIntersect(
		_XvicRect *rect,
		_XvicRegion *rgn);
_XvicRegion *_XvicRegionCreateRect(
		_XvicRect *rect);
void _XvicRegionDestroy(
		_XvicRegion *rgn);
int _XvicRegionGetNumRects(
		_XvicRegion *rgn);
_XvicRect *_XvicRegionGetRectangles(
		_XvicRegion *rgn);
int _XvicRegionHasRect(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionIntersect(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionIsEmpty(
		_XvicRegion *rgn);
int _XvicRegionOffset(
		_XvicRegion *rgn,
		int xoff,
		int yoff);
int _XvicRegionSubtract(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionUnion(
		_XvicRect *rect,
		_XvicRegion *rgn);

#endif /* _NO_PROTO */

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
