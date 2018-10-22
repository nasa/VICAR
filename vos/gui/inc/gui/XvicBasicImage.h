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

