#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/DrawP.h>
#include "XvicBasicImageP.h"
#include "XvicRegion.h"
#include <limits.h>		/* only for SetDataRangeDefaults() */
#include <math.h>	/* only for floor()... is it really needed??!!!!*/
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>

/* #define DPR(x) printf x */
#define DPR(x)

/************************************************************************/
/************************************************************************/
/*  D E C L A R A T I O N S						*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* Static Function Declarations						*/
/************************************************************************/

#ifdef _NO_PROTO

/* Core Methods */

static void ClassInitialize();
static void ClassPartInitialize();
static void Destroy();
static void Initialize();
static XtGeometryResult QueryGeometry();
static void Realize();
static void Redisplay();
static void Resize();
static Boolean SetValues();
static void SetValuesAlmost();

/* Primitive Methods */

static void BorderUnhighlight();

/* BasicImage Methods */

static void InstallColormap();

/* Action Procedures */

/* Misc */

static void AllocRawData();
static void CopyCurrentToSavedResources();
static void CopyDefaultColors();
static void CopyRawData();
static void CopyXimagePixmap();
static void CreateTiles();
static void DestroyTiles();
static void DoPan();
static void DrawTilePixmap();
static void DrawTileRaw();
static void DrawTileXimage();
static void ExposeTile();
static void FatalError();
static void FreeAllocedColors();
static void FreeAllTileData();
static Boolean FreeLRUTile();
static void FreeRawData();
static Boolean FreeTmpXimageIfTooBig();
static void FreeXTileData();
static void GetColormap();
static Boolean GetVisual();
static void GraphicsExposeHandler();
static Tile *NextMemoryTile();
static Tile *NextTile();
static void RedrawBorder();
static void ReduceRational();
static void SetDataRangeDefaults();
static void SetDataType();
static void SetDisplayTransform();
static void SetTileCoords();
static void SetTileDpyCoords();
static void SetUpColormap();
static void SetUpGCs();
static void SetUpZoomPan();
static void ValidateCmapResources();
static void WarningMsg();
static Boolean WorkProc();

/* Resource Converters */

static void RegisterConverters();

#else

/* Core Methods */

static void ClassInitialize(void);
static void ClassPartInitialize(
		WidgetClass wc);
static void Destroy(
		Widget w);
static void Initialize(
		Widget req_w,
		Widget new_w,
		ArgList args,
		Cardinal *num_args);
static XtGeometryResult QueryGeometry(
		Widget w,
		XtWidgetGeometry *proposed,
		XtWidgetGeometry *answer);
static void Realize(
		Widget w,
		XtValueMask *value_mask,
		XSetWindowAttributes *attributes);
static void Redisplay(
		Widget w,
		XEvent *event,
		Region region);
static void Resize(
		Widget w);
static Boolean SetValues(
		Widget current,
		Widget request,
		Widget set,
		ArgList args,
		Cardinal *num_args);
static void SetValuesAlmost(
		Widget old_w,
		Widget new_w,
		XtWidgetGeometry *request,
		XtWidgetGeometry *reply);

/* Primitive Methods */

static void BorderUnhighlight(
		Widget w);

/* BasicImage Methods */

static void InstallColormap(
		Widget w);

/* Action Procedures */

/* Misc */

static void AllocRawData(
		XvicBasicImageWidget biw,
		XvicImageData *raw,
		int size);
static void CopyCurrentToSavedResources(
		XvicBasicImageWidget biw);
static void CopyDefaultColors(
		XvicBasicImageWidget biw,
		XColor *cells,
		int number);
static void CopyRawData(
		XvicBasicImageWidget biw,
		Tile *tile,
		XvicImageData *image,
		_XvicRect *area,
		Boolean *mem_okay);
static void CopyXimagePixmap(
		XvicBasicImageWidget biw,
		Tile *tile,
		XImage *ximage,
		Pixmap pixmap,
		_XvicRect *area);
static void CreateTiles(
		XvicBasicImageWidget biw);
static void DestroyTiles(
		XvicBasicImageWidget biw);
static void DoPan(
		XvicBasicImageWidget biw);
static void DrawTilePixmap(
		XvicBasicImageWidget biw,
		Tile *tile,
		Pixmap pixmap,
#if NeedWidePrototypes
		int draw_all);
#else
		Boolean draw_all);
#endif
static void DrawTileRaw(
		XvicBasicImageWidget biw,
		Tile *tile,
		XvicImageData *raw,
#if NeedWidePrototypes
		int draw_all);
#else
		Boolean draw_all);
#endif
static void DrawTileXimage(
		XvicBasicImageWidget biw,
		Tile *tile,
		XImage *ximage,
#if NeedWidePrototypes
		int draw_all);
#else
		Boolean draw_all);
#endif
static void ExposeTile(
		XvicBasicImageWidget biw,
		Tile *tile);
static void FatalError(
		XvicBasicImageWidget biw,
		char *name,
		char *def);
static void FreeAllocedColors(
		XvicBasicImageWidget biw);
static void FreeAllTileData(
		XvicBasicImageWidget biw);
static Boolean FreeLRUTile(
		XvicBasicImageWidget biw);
static void FreeRawData(
		XvicBasicImageWidget biw,
		XvicImageData *raw,
		int raw_size);
static Boolean FreeTmpXimageIfTooBig(
		XvicBasicImageWidget biw);
static void FreeXTileData(
		XvicBasicImageWidget biw);
static void GetColormap(
		XvicBasicImageWidget biw);
static Boolean GetVisual(
		XvicBasicImageWidget biw);
static void GraphicsExposeHandler(
		Widget w,
		XtPointer client_data,
		XEvent *event,
		Boolean *continue_to_dispatch);
static Tile *NextMemoryTile(
		XvicBasicImageWidget biw);
static Tile *NextTile(
		XvicBasicImageWidget biw);
static void RedrawBorder(
		XvicBasicImageWidget biw);
static void ReduceRational(
		int *numer,
		int *denom);
static void SetDataRangeDefaults(
		XvicBasicImageWidget biw,
		ArgList args,
		Cardinal *num_args,
		double old_min,
		double old_max);
static void SetDataType(
		XvicBasicImageWidget biw);
static void SetDisplayTransform(
		XvicBasicImageWidget biw);
static void SetTileCoords(
		XvicBasicImageWidget biw);
static void SetTileDpyCoords(
		XvicBasicImageWidget biw);
static void SetUpColormap(
		XvicBasicImageWidget biw);
static void SetUpGCs(
		XvicBasicImageWidget biw);
static void SetUpZoomPan(
		XvicBasicImageWidget biw);
static void ValidateCmapResources(
		XvicBasicImageWidget biw);
static void WarningMsg(
		XvicBasicImageWidget biw,
		char *name,
		char *def);
static Boolean WorkProc(
		XtPointer client_data);

/* Resource Converters */

static void RegisterConverters(void);

#endif /* _NO_PROTO */

/************************************************************************/
/* Translation tables							*/
/************************************************************************/

/* We don't really want any new translations, but Primitive	*/
/* will only give us the traversal translations if we already	*/
/* have a table.  Hey, doesn't make sense to me either!		*/

// static char defaultTranslations[] = "";

/************************************************************************/
/* Action List								*/
/************************************************************************/

/* None */

/************************************************************************/
/* Resources								*/
/************************************************************************/

static XtResource resources[] =
{
    {
	XvicNblueLevels,
	XvicCCmapCells,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.blue_levels),
	XtRImmediate,
	(XtPointer) 13
    },
    {
	XvicNbwColormapPolicy,
	XvicCColormapPolicy,
	XvicRColormapPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.bw_colormap_policy),
	XtRImmediate,
	(XtPointer) XvicHALF
    },
    {
	XvicNbwDither,
	XvicCDitherMode,
	XvicRDitherMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.bw_dither),
	XtRImmediate,
	(XtPointer) XvicNONE
    },
    {
	XvicNbwStretchPolicy,
	XvicCStretchPolicy,
	XvicRStretchPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.bw_stretch_policy),
	XtRImmediate,
	(XtPointer) XvicUSE_HW
    },
    {
	XvicNbwVisualType,
	XvicCVisualType,
	XvicRVisualType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.bw_visual_type),
	XtRImmediate,
	(XtPointer) XvicUSE_DEFAULT
    },
    {
	XvicNcolorColormapPolicy,
	XvicCColormapPolicy,
	XvicRColormapPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.color_colormap_policy),
	XtRImmediate,
	(XtPointer) XvicHALF
    },
    {
	XvicNcolorDither,
	XvicCDitherMode,
	XvicRDitherMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.color_dither),
	XtRImmediate,
	(XtPointer) XvicORDERED
    },
    {
	XvicNcolorStretchPolicy,
	XvicCStretchPolicy,
	XvicRStretchPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.color_stretch_policy),
	XtRImmediate,
	(XtPointer) XvicUSE_SW
    },
    {
	XvicNcolorVisualType,
	XvicCVisualType,
	XvicRVisualType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.color_visual_type),
	XtRImmediate,
	(XtPointer) XvicUSE_DEFAULT
    },
    {
	XvicNcolormapPolicy,
	XvicCColormapPolicy,
	XvicRColormapPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.colormap_policy),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNconstrainPan,
	XvicCConstrainPan,
	XvicRConstrainPan,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.constrain_pan),
	XtRImmediate,
	(XtPointer) XvicNONE
    },
    {
	XvicNdataSavePolicy,
	XvicCDataSavePolicy,
	XvicRDataSavePolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.data_save_policy),
	XtRImmediate,
	(XtPointer) XvicRAW
    },
    {
	XvicNdataType,
	XvicCDataType,
	XvicRDataType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.data_type),
	XtRImmediate,
	(XtPointer) XvicBYTE
    },
    {
	XvicNditherMode,
	XvicCDitherMode,
	XvicRDitherMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.dither_mode),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNenableDirectColor,
	XvicCEnableDirectColor,
	XmRBoolean,
	sizeof(Boolean),
	XtOffsetOf(XvicBasicImageRec, bim.enable_direct_color),
	XtRImmediate,
	(XtPointer) True
    },
    {
	XvicNexposeCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicBasicImageRec, bim.expose_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNgrayLevels,
	XvicCCmapCells,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.gray_levels),
	XtRImmediate,
	(XtPointer) 16
    },
    {
	XvicNgreenLevels,
	XvicCCmapCells,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.green_levels),
	XtRImmediate,
	(XtPointer) 16
    },
    {
	XvicNimageHeight,
	XtCHeight,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.image_height),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNimageMode,
	XvicCImageMode,
	XvicRImageMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.image_mode),
	XtRImmediate,
	(XtPointer) XvicCOLOR
    },
    {
	XvicNimageWidth,
	XtCWidth,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.image_width),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNimageZoom,
	XvicCZoom,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.image_zoom),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNlutType,
	XvicCLutType,
	XvicRLutType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.lut_type),
	XtRImmediate,
	(XtPointer) XvicSTRETCH
    },
    {
	XvicNlut16Type,
	XvicCLut16Type,
	XvicRLutType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.lut16_type),
	XtRImmediate,
	(XtPointer) XvicRAW
    },
    {
	XvicNmaximumMemory,
	XvicCMemory,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.maximum_memory),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNoutputDataMax,
	XvicCDataRange,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.output_data_max),
	XtRImmediate,
	(XtPointer) 65535
    },
    {
	XvicNpseudoColormapPolicy,
	XvicCColormapPolicy,
	XvicRColormapPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.pseudo_colormap_policy),
	XtRImmediate,
	(XtPointer) XvicHALF
    },
    {
	XvicNpseudoDither,
	XvicCDitherMode,
	XvicRDitherMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.pseudo_dither),
	XtRImmediate,
	(XtPointer) XvicORDERED
    },
    {
	XvicNpseudoStretchPolicy,
	XvicCStretchPolicy,
	XvicRStretchPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.pseudo_stretch_policy),
	XtRImmediate,
	(XtPointer) XvicUSE_SW
    },
    {
	XvicNpseudoVisualType,
	XvicCVisualType,
	XvicRVisualType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.pseudo_visual_type),
	XtRImmediate,
	(XtPointer) XvicUSE_DEFAULT
    },
    {
	XvicNrawDataMax,
	XvicCDataRange,
	XvicRDouble,
	sizeof(double),
	XtOffsetOf(XvicBasicImageRec, bim.raw_data_max),
	XtRString,
	(XtPointer) "0"
    },
    {
	XvicNrawDataMin,
	XvicCDataRange,
	XvicRDouble,
	sizeof(double),
	XtOffsetOf(XvicBasicImageRec, bim.raw_data_min),
	XtRString,
	(XtPointer) "0"
    },
    {
	XvicNredLevels,
	XvicCCmapCells,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.red_levels),
	XtRImmediate,
	(XtPointer) 16
    },
    {
	XvicNresizeCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicBasicImageRec, bim.resize_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNscaledDataMax,
	XvicCDataRange,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.scaled_data_max),
	XtRImmediate,
	(XtPointer) 65535
    },
    {
	XvicNstretchPolicy,
	XvicCStretchPolicy,
	XvicRStretchPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.stretch_policy),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNtileHeight,
	XtCHeight,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.tile_height),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNtileWidth,
	XtCWidth,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.tile_width),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNviewHeight,
	XtCHeight,
	XtRDimension,
	sizeof(Dimension),
	XtOffsetOf(XvicBasicImageRec, bim.view_height),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNviewWidth,
	XtCWidth,
	XtRDimension,
	sizeof(Dimension),
	XtOffsetOf(XvicBasicImageRec, bim.view_width),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNvisibleAreaCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicBasicImageRec, bim.visible_area_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNvisualType,
	XvicCVisualType,
	XvicRVisualType,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.visual_type),
	XtRImmediate,
	(XtPointer) XvicUSE_DEFAULT
    },
    {
	XvicNworkProcActiveCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicBasicImageRec, bim.work_proc_active_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNworkProcPolicy,
	XvicCWorkProcPolicy,
	XvicRWorkProcPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicBasicImageRec, bim.work_proc_policy),
	XtRImmediate,
	(XtPointer) XvicALL
    },
    {
	XvicNxPan,
	XvicCPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNxPreSubpixelPan,
	XvicCSubpixelPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_presubpixel_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNxPreZoomIn,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_prezoom_in),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNxPreZoomOut,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_prezoom_out),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNxSubpixelPan,
	XvicCSubpixelPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_subpixel_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNxZoom,
	XvicCZoom,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_zoom),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNxZoomIn,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_zoom_in),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNxZoomOut,
	XvicCZoomDenom,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.x_zoom_out),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNyPan,
	XvicCPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNyPreSubpixelPan,
	XvicCSubpixelPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_presubpixel_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNyPreZoomIn,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_prezoom_in),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNyPreZoomOut,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_prezoom_out),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNySubpixelPan,
	XvicCSubpixelPan,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_subpixel_pan),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNyZoom,
	XvicCZoom,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_zoom),
	XtRImmediate,
	(XtPointer) 0
    },
    {
	XvicNyZoomIn,
	XvicCZoomNumer,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_zoom_in),
	XtRImmediate,
	(XtPointer) 1
    },
    {
	XvicNyZoomOut,
	XvicCZoomDenom,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicBasicImageRec, bim.y_zoom_out),
	XtRImmediate,
	(XtPointer) 1
    }
};

/************************************************************************/
/* Class record								*/
/************************************************************************/

externaldef(xvicbasicimageclassrec)
		XvicBasicImageClassRec xvicBasicImageClassRec =
{
  { /* core_class record */
#if USE_MOTIF
    /* superclass         */	(WidgetClass) &xmPrimitiveClassRec,
#else
    /* superclass         */	(WidgetClass) &widgetClassRec,
#endif
    /* class_name         */	"XvicBasicImage",
    /* widget_size        */	sizeof(XvicBasicImageRec),
    /* class_initialize   */	ClassInitialize,
    /* class_part_init    */	ClassPartInitialize,
    /* class_inited       */	FALSE,
    /* initialize         */	Initialize,
    /* initialize_hook    */	(XtArgsProc) NULL,
    /* realize            */	Realize,
    /* actions            */	NULL,
    /* num_actions        */    0,
    /* resources          */	resources,
    /* num_resources      */	XtNumber(resources),
    /* xrm_class          */	NULLQUARK,
    /* compress_motion    */	TRUE,
    /* compress_exposure  */	XtExposeNoCompress,
    /* compress_enterlv   */	TRUE,
    /* visible_interest   */	FALSE,
    /* destroy            */    Destroy,
    /* resize             */    Resize,
    /* expose             */    Redisplay,
    /* set_values         */	SetValues,
    /* set_values_hook    */	(XtArgsFunc) NULL,
    /* set_values_almost  */	SetValuesAlmost,
    /* get_values_hook    */	(XtArgsProc) NULL,
    /* accept_focus       */	(XtAcceptFocusProc) NULL,
    /* version            */	XtVersion,
    /* callback_private   */	NULL,
    /* tm_table           */	NULL,	/* defaultTranslations !!!!*/
    /* query_geometry     */	QueryGeometry,
    /* display_accelerator*/	(XtStringProc) NULL,
    /* extension record   */	NULL,
  },
#if USE_MOTIF
  { /* primitive_class record */
    /* border_highlight   */	XmInheritBorderHighlight,
    /* border_unhighlight */	BorderUnhighlight,
    /* translations       */	XtInheritTranslations,
    /* arm_and_activate   */	NULL,
    /* syn_resources      */	NULL,
    /* num_syn_resources  */	0,
    /* extension          */	NULL,
  },
#endif
  { /* basic_image_class record */
    /* expose_overlay     */	XvicInheritExposeOverlay,
    /* move_overlay       */	XvicInheritMoveOverlay,
    /* clear_overlay      */    XvicInheritClearOverlay,
    /* release_gr_colors  */	XvicInheritReleaseGrColors,
    /* set_up_gr_colors   */	XvicInheritSetUpGrColors,
    /* install_colormap   */	InstallColormap,
    /* extension          */	NULL,
  }

};

externaldef(xvicbasicimagewidgetclass) WidgetClass xvicBasicImageWidgetClass =
				(WidgetClass) &xvicBasicImageClassRec;

/************************************************************************/
/************************************************************************/
/*  C O R E   M E T H O D S						*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* ClassInitialize method						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ClassInitialize()
#else
ClassInitialize(void)
#endif
{
   RegisterConverters();
}

/************************************************************************/
/* ClassPartInitialize method						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ClassPartInitialize(wc)
   WidgetClass wc;
#else
ClassPartInitialize(
   WidgetClass wc)
#endif
{
   XvicBasicImageWidgetClass c = (XvicBasicImageWidgetClass) wc;

   /* Set up class method pointers */

   if (c->basic_image_class.expose_overlay == XvicInheritExposeOverlay)
      c->basic_image_class.expose_overlay = NULL;	/* nothing to do */

   if (c->basic_image_class.move_overlay == XvicInheritMoveOverlay)
      c->basic_image_class.move_overlay = NULL;	/* nothing to do */

   if (c->basic_image_class.clear_overlay == XvicInheritClearOverlay)
      c->basic_image_class.clear_overlay = NULL;	/* nothing to do */

   if (c->basic_image_class.release_gr_colors == XvicInheritReleaseGrColors)
      c->basic_image_class.release_gr_colors = NULL;	/* nothing to do */

   if (c->basic_image_class.set_up_gr_colors == XvicInheritSetUpGrColors)
      c->basic_image_class.set_up_gr_colors = NULL;	/* nothing to do */

   if (c->basic_image_class.install_colormap == XvicInheritInstallColormap)
      c->basic_image_class.install_colormap = InstallColormap;
}

/************************************************************************/
/* Destroy method							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Destroy(w)
   Widget w;
#else
Destroy(
   Widget w)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;

   FreeAllocedColors(biw);
   if (biw->bim.colormap && biw->bim.private_cmap)
      XFreeColormap(XtDisplay(biw), biw->bim.colormap);

   if (biw->bim.expose_rgn)
      _XvicRegionDestroy(biw->bim.expose_rgn);

   DestroyTiles(biw);

   if (biw->bim.img_gc != NULL)
      XFreeGC(XtDisplay(biw), biw->bim.img_gc);
   if (biw->bim.pan_gc != NULL)
      XFreeGC(XtDisplay(biw), biw->bim.pan_gc);

   XtRemoveEventHandler((Widget)biw, 0, TRUE, GraphicsExposeHandler, NULL);

   if (biw->bim.tmp_ximage) {
      _XvicMemoryReturn(biw,
	biw->bim.tmp_ximage->height * biw->bim.tmp_ximage->bytes_per_line);
      XDestroyImage(biw->bim.tmp_ximage);
   }

   if (biw->bim.lookup16_bw)
      _XvicFree(biw, biw->bim.lookup16_bw, LUT16_SIZE * sizeof(unsigned char));
   if (biw->bim.lookup16_red)
      _XvicFree(biw, biw->bim.lookup16_red, LUT16_SIZE * sizeof(unsigned char));
   if (biw->bim.lookup16_grn)
      _XvicFree(biw, biw->bim.lookup16_grn, LUT16_SIZE * sizeof(unsigned char));
   if (biw->bim.lookup16_blu)
      _XvicFree(biw, biw->bim.lookup16_blu, LUT16_SIZE * sizeof(unsigned char));

   if (biw->bim.red_lut16)
      _XvicFree(biw, biw->bim.red_lut16, LUT16_SIZE * sizeof(int));
   if (biw->bim.green_lut16)
      _XvicFree(biw, biw->bim.green_lut16, LUT16_SIZE * sizeof(int));
   if (biw->bim.blue_lut16)
      _XvicFree(biw, biw->bim.blue_lut16, LUT16_SIZE * sizeof(int));
   if (biw->bim.stretch_lut16)
      _XvicFree(biw, biw->bim.stretch_lut16, LUT16_SIZE * sizeof(int));

   _XvicFree(biw, biw->bim.cmap_gray, LUT_SIZE * sizeof(Dn2Cmap[1]));
   _XvicFree(biw, biw->bim.cmap_green, LUT_SIZE * sizeof(Dn2Cmap[1]));
   _XvicFree(biw, biw->bim.cmap_rb, LUT_SIZE * sizeof(Dn2Cmap[1]));

   _XvicFree(biw, biw->bim.red_lut, LUT_SIZE * sizeof(LUT[1]));
   _XvicFree(biw, biw->bim.green_lut, LUT_SIZE * sizeof(LUT[1]));
   _XvicFree(biw, biw->bim.blue_lut, LUT_SIZE * sizeof(LUT[1]));
   _XvicFree(biw, biw->bim.stretch_lut, LUT_SIZE * sizeof(LUT[1]));

   if (biw->bim.memory_used != 0)	/*!!!! debugging only !!!!*/
      printf("Memory leak in Destroy!!! %d left\n", biw->bim.memory_used);	/*!!!!*/

}

/************************************************************************/
/* Initialize method							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Initialize(req_w, new_w, args, num_args)
   Widget req_w;
   Widget new_w;
   ArgList args;
   Cardinal *num_args;
#else
Initialize(
   Widget req_w,
   Widget new_w,
   ArgList args,
   Cardinal *num_args)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) new_w;
   Display *dpy;
   Boolean new_visual;
   int i;
   Dimension dpy_size, img_size;

   dpy = XtDisplay(biw);

   biw->bim.vis.visual = NULL;
   biw->bim.colormap = None;
   biw->bim.private_cmap = FALSE;
   biw->bim.gray_alloced = 0;
   biw->bim.green_alloced = 0;
   biw->bim.blue_alloced = 0;
   biw->bim.rb_alloced = 0;
   biw->bim.alloc_private = FALSE;
   biw->bim.ps_as_color = FALSE;
   biw->bim.work_proc_pending = FALSE;
   biw->bim.tiles = NULL;
   biw->bim.num_tiles = 0;
   biw->bim.memory_used = 0;
   biw->bim.lru = 0;
   biw->bim.current_tile = -1;
   biw->bim.img_gc = NULL;
   biw->bim.pan_gc = NULL;
   biw->bim.pan_count = 0;
   biw->bim.pan_dir = 0;
   biw->bim.tmp_ximage = NULL;
   biw->bim.protect_tmp_ximage = FALSE;

#if !USE_MOTIF
   biw->primitive.shadow_thickness = 0;		/* fake values; not used */
   biw->primitive.highlight_thickness = 0;
#endif

   biw->bim.cmap_gray = _XvicMalloc(biw, LUT_SIZE * sizeof(Dn2Cmap[1]));
   biw->bim.cmap_green = _XvicMalloc(biw, LUT_SIZE * sizeof(Dn2Cmap[1]));
   biw->bim.cmap_rb = _XvicMalloc(biw, LUT_SIZE * sizeof(Dn2Cmap[1]));

   biw->bim.red_lut = _XvicMalloc(biw, LUT_SIZE * sizeof(LUT[1]));
   biw->bim.green_lut = _XvicMalloc(biw, LUT_SIZE * sizeof(LUT[1]));
   biw->bim.blue_lut = _XvicMalloc(biw, LUT_SIZE * sizeof(LUT[1]));
   biw->bim.stretch_lut = _XvicMalloc(biw, LUT_SIZE * sizeof(LUT[1]));

   biw->bim.red_lut16 = NULL;
   biw->bim.green_lut16 = NULL;
   biw->bim.blue_lut16 = NULL;
   biw->bim.stretch_lut16 = NULL;

   biw->bim.lookup16_bw = NULL;
   biw->bim.lookup16_red = NULL;
   biw->bim.lookup16_grn = NULL;
   biw->bim.lookup16_blu = NULL;

   for (i=0; i<LUT_SIZE; i++) {
      biw->bim.red_lut[i] = i;
      biw->bim.green_lut[i] = i;
      biw->bim.blue_lut[i] = i;
      biw->bim.stretch_lut[i] = i;
   }

   SetUpZoomPan(biw);
   biw->bim.x_old_screen_pan = biw->bim.x_screen_pan;
   biw->bim.y_old_screen_pan = biw->bim.y_screen_pan;

   /* imageHeight and imageWidth default to 1, which is a pretty	*/
   /* useless value but it's better than crashing the widget.		*/

   if (biw->bim.image_height <= 0)
      biw->bim.image_height = 1;
   if (biw->bim.image_width <= 0)
      biw->bim.image_width = 1;

   /* Set up the size of the widget.  If the core size is specified,	*/
   /* by the user, it is used.  If not, viewHeight/Width are used to	*/
   /* compute the size.  If they're not present, imageHeight/Width	*/
   /* are used with the current zoom factor to compute the size, with	*/
   /* a maximum of the screen size.					*/

   if (req_w->core.width != 0) {	/* Use core size */
      biw->bim.view_width = req_w->core.width -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
      if ((int)req_w->core.width < (int)(2*(biw->primitive.highlight_thickness)+
				            biw->primitive.shadow_thickness))
         biw->bim.view_width = 0;	/* can't check < 0 because unsigned */
   }
   if (req_w->core.height != 0) {
      biw->bim.view_height = req_w->core.height -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
      if ((int)req_w->core.height<(int)(2*(biw->primitive.highlight_thickness) +
				           biw->primitive.shadow_thickness))
         biw->bim.view_height = 0;	/* can't check < 0 because unsigned */
   }
   biw->bim.x_dpy_off = biw->primitive.highlight_thickness +
			biw->primitive.shadow_thickness;
   biw->bim.y_dpy_off = biw->primitive.highlight_thickness +
			biw->primitive.shadow_thickness;

   if (biw->bim.view_width == 0) {	/* Not given, calc from imageWidth */
      img_size = ZOOMX(biw->bim.image_width);
      dpy_size = DisplayWidth(dpy, DefaultScreen(dpy));
      biw->bim.view_width = MIN(img_size, dpy_size);
   }
   if (biw->bim.view_height == 0) {	/* Not given, calc from imageHeight */
      img_size = ZOOMY(biw->bim.image_height);
      dpy_size = DisplayHeight(dpy, DefaultScreen(dpy));
      biw->bim.view_height = MIN(img_size, dpy_size);
   }

   /* Recompute core size based on now-correct view size.  This may be	*/
   /* redundant if the user actually specified the core size.		*/

   biw->core.width = biw->bim.view_width +
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   biw->core.height = biw->bim.view_height +
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);

   /* Save view width and height in case we get resized */

   biw->bim.old_view_width = biw->bim.view_width;
   biw->bim.old_view_height = biw->bim.view_height;

   /* Check tile size */

   if (biw->bim.tile_width <= 0 || biw->bim.tile_width >= biw->bim.image_width)
      biw->bim.tile_width = biw->bim.image_width;
   if (biw->bim.tile_height <= 0 || biw->bim.tile_height>=biw->bim.image_height)
      biw->bim.tile_height = biw->bim.image_height;

   /* Set up mode-specific resources.  If a value is explicitly given	*/
   /* for the "current" value, don't change it.  Otherwise, install the	*/
   /* mode-specific resource into current.				*/

   if (COLOR_MODE) {
      if (biw->bim.dither_mode == 0)
         biw->bim.dither_mode = biw->bim.color_dither;
      if (biw->bim.stretch_policy == 0)
         biw->bim.stretch_policy = biw->bim.color_stretch_policy;
      if (biw->bim.colormap_policy == 0)
         biw->bim.colormap_policy = biw->bim.color_colormap_policy;
      if (biw->bim.visual_type == 0)
         biw->bim.visual_type = biw->bim.color_visual_type;
   }
   else if (PSEUDO_SET) {
      if (biw->bim.dither_mode == 0)
         biw->bim.dither_mode = biw->bim.pseudo_dither;
      if (biw->bim.stretch_policy == 0)
         biw->bim.stretch_policy = biw->bim.pseudo_stretch_policy;
      if (biw->bim.colormap_policy == 0)
         biw->bim.colormap_policy = biw->bim.pseudo_colormap_policy;
      if (biw->bim.visual_type == 0)
         biw->bim.visual_type = biw->bim.pseudo_visual_type;
   }
   else {		/* BW */
      if (biw->bim.dither_mode == 0)
         biw->bim.dither_mode = biw->bim.bw_dither;
      if (biw->bim.stretch_policy == 0)
         biw->bim.stretch_policy = biw->bim.bw_stretch_policy;
      if (biw->bim.colormap_policy == 0)
         biw->bim.colormap_policy = biw->bim.bw_colormap_policy;
      if (biw->bim.visual_type == 0)
         biw->bim.visual_type = biw->bim.bw_visual_type;
   }

   new_visual = GetVisual(biw);		/* return is ignored */
   ValidateCmapResources(biw);
   GetColormap(biw);
   SetUpColormap(biw);
   SetDisplayTransform(biw);
   SetDataRangeDefaults(biw, args, num_args, 0.0, 0.0);
   SetDataType(biw);

   do {
      biw->bim.expose_rgn = _XvicRegionCreate();
      if (!biw->bim.expose_rgn) _XvicMemoryPanic(biw);
   } while (biw->bim.expose_rgn == NULL);

   CreateTiles(biw);

   XtAddEventHandler((Widget)biw, 0, TRUE, GraphicsExposeHandler, NULL);

   CopyCurrentToSavedResources(biw);

}

/************************************************************************/
/* QueryGeometry method							*/
/*									*/
/* The preferred size is specified by viewHeight/Width.  However, those	*/
/* fields are modified based on the actual size (i.e. if the app changes*/
/* viewH/W, and the geometry change is denied or modified, viewH/W	*/
/* reflect the actual size, not what was requested by the app).  So,	*/
/* viewH/W are always consistent with core.h/w.  The preferred size is	*/
/* therefore always the current size, so the query always returns No	*/
/* (unless the proposed size *is* the current size!).  We could maintain*/
/* the user's requested viewH/W separately and use it to compute the	*/
/* preferred size, but the effort involved is probably not worthwhile	*/
/* since the geometry is normally constrained by the window manager	*/
/* anyway.								*/
/************************************************************************/

static XtGeometryResult
#ifdef _NO_PROTO
QueryGeometry(w, proposed, answer)
   Widget w;
   XtWidgetGeometry *proposed;
   XtWidgetGeometry *answer;
#else
QueryGeometry(
   Widget w,
   XtWidgetGeometry *proposed,
   XtWidgetGeometry *answer)
#endif
{

   answer->request_mode = CWWidth | CWHeight;  /* set up fields we care about */

   answer->width = w->core.width;
   answer->height = w->core.height;

   if (((proposed->request_mode & (CWWidth | CWHeight)) ==
				  (CWWidth | CWHeight)) &&
	 proposed->width == answer->width &&
	 proposed->height == answer->height)
      return XtGeometryYes;

   return XtGeometryNo;

}

/************************************************************************/
/* Realize method							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Realize(w, value_mask, attributes)
   Widget w;
   XtValueMask *value_mask;
   XSetWindowAttributes *attributes;
#else
Realize(
   Widget w,
   XtValueMask *value_mask,
   XSetWindowAttributes *attributes)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;

   *value_mask |= CWBackingStore;
   attributes->backing_store = NotUseful;	/*!!!!*/ /* S/B WhenMapped */

   *value_mask |= CWColormap;
   attributes->colormap = biw->bim.colormap;

   *value_mask |= CWBitGravity;
   attributes->bit_gravity = NorthWestGravity;

   *value_mask |= CWBorderPixel;
   attributes->border_pixel = 0;		/*!!!!*/ /* fix this */

   DPR(("Realize, mask=%x\n", *value_mask));

   XtCreateWindow((Widget)biw, InputOutput, biw->bim.vis.visual,
		*value_mask, attributes);

   CALL_InstallColormap(biw, ((Widget)biw));

   SetUpGCs(biw);

}

/************************************************************************/
/* Redisplay (Expose) method						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Redisplay(w, event, region)
   Widget w;
   XEvent *event;
   Region region;
#else
Redisplay(
   Widget w,
   XEvent *event,
   Region region)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   XExposeEvent *exp_event = (XExposeEvent *)event;
   XExposeEvent new_event;

   new_event.x = exp_event->x - biw->bim.x_dpy_off;
   new_event.width = exp_event->width;
   X_ViewConstrain(new_event.x, new_event.width);
   new_event.y = exp_event->y - biw->bim.y_dpy_off;
   new_event.height = exp_event->height;
   Y_ViewConstrain(new_event.y, new_event.height);

   /* Redisplay highlight/shadow if necessary (not needed for internal	*/
   /* exposures).  Needed if event was modified above.			*/

   if (exp_event->x < biw->bim.x_dpy_off || new_event.width!=exp_event->width ||
       exp_event->y < biw->bim.y_dpy_off || new_event.height!=exp_event->height)
      RedrawBorder(biw);

   if (biw->bim.pan_count != 0) {		/* outstanding pans */
      if (biw->bim.pan_dir & PAN_HORIZ) {
         new_event.x -= (biw->bim.x_screen_pan - biw->bim.x_min_exp_pan);
         new_event.width += (biw->bim.x_max_exp_pan - biw->bim.x_min_exp_pan);
         X_ViewConstrain(new_event.x, new_event.width);
      }
      if (biw->bim.pan_dir & PAN_VERT) {
         new_event.y -= (biw->bim.y_screen_pan - biw->bim.y_min_exp_pan);
         new_event.height += (biw->bim.y_max_exp_pan - biw->bim.y_min_exp_pan);
         Y_ViewConstrain(new_event.y, new_event.height);
      }
   }

   _XvicRedisplay_Internal(biw, &new_event, region);

}

/************************************************************************/
/* Resize method							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Resize(w)
   Widget w;
#else
Resize(
   Widget w)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   XvicImageCallbackStruct cb;
   int bdr;
   XExposeEvent ev;

   /* Recompute the view size based on the new core size */
   DPR(("prev view_w=%d, h=%d\n", biw->bim.view_width, biw->bim.view_height));

   biw->bim.view_width = biw->core.width -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   if ((int)biw->core.width < (int)(2 * (biw->primitive.highlight_thickness) +
				   biw->primitive.shadow_thickness))
      biw->bim.view_width = 0;		/* can't check < 0 because unsigned */
   biw->bim.view_height = biw->core.height -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   if ((int)biw->core.height < (int)(2 * (biw->primitive.highlight_thickness) +
				    biw->primitive.shadow_thickness))
      biw->bim.view_height = 0;		/* can't check < 0 because unsigned */

   /* If we have been resized bigger, we must expose the area under the	*/
   /* left/bottom border (highlight & shadow).  Sad but true.  All the	*/
   /* Motif examples I could find redraw everything on a resize.  Sigh.	*/
   /* Might as well redraw the new border here as well.			*/

   if (XtIsRealized((Widget) biw)) {
      bdr = biw->primitive.highlight_thickness+biw->primitive.shadow_thickness;
      if (biw->bim.old_view_width < biw->bim.view_width) {
         ev.x = biw->bim.old_view_width;
         ev.y = 0;
         ev.width = bdr;
         ev.height = biw->bim.old_view_height + bdr;
         if (biw->bim.old_view_height < biw->bim.view_height)
            ev.height = biw->bim.old_view_height;    /* avoid double expose */
         					     /* on overlap area */
         _XvicRedisplay_Internal(biw, &ev, NULL);
      }
      if (biw->bim.old_view_height < biw->bim.view_height) {
         ev.x = 0;
         ev.y = biw->bim.old_view_height;
         ev.width = biw->bim.old_view_width + bdr;
         ev.height = bdr;
         _XvicRedisplay_Internal(biw, &ev, NULL);
      }
      RedrawBorder(biw);
   }
   biw->bim.old_view_width = biw->bim.view_width;
   biw->bim.old_view_height = biw->bim.view_height;

   /* If the view was resized, we need to check the pan value to see	*/
   /* if it needs to be constrained to the new view.  If so, do the pan	*/
   /* to make it right.  We do this after delivering our synthetic	*/
   /* Expose events so we don't build up an outstanding pan count (it's	*/
   /* more efficient this way).  If the pans are not constrained, this	*/
   /* section is a no-op.						*/

   SetUpZoomPan(biw);

   if ((biw->bim.x_screen_pan != biw->bim.x_old_screen_pan) ||
       (biw->bim.y_screen_pan != biw->bim.y_old_screen_pan))
      DoPan(biw);

   /* Call the resizeCallback procedure if present */

   if (biw->bim.resize_callback) {
      cb.reason = XvicCR_RESIZE;
      cb.new_view_width = biw->bim.view_width;
      cb.new_view_height = biw->bim.view_height;

      XtCallCallbackList(w, biw->bim.resize_callback, (XtPointer) &cb);
   }
}

/************************************************************************/
/* SetValues method							*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
SetValues(current, request, set, args, num_args)
   Widget current;
   Widget request;
   Widget set;
   ArgList args;
   Cardinal *num_args;
#else
SetValues(
   Widget current,
   Widget request,
   Widget set,
   ArgList args,
   Cardinal *num_args)
#endif
{
   XvicBasicImageWidget old = (XvicBasicImageWidget) current;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) set;
   Widget topLevel = NULL;
   XvicImageCallbackStruct cb;
   Boolean new_visual = FALSE;
   Boolean new_colormap = FALSE;
   Boolean realize = FALSE;
   Boolean managed = FALSE;
   Boolean shell_resize;
   /* One of these being TRUE makes the others below it also TRUE */
   Boolean trash_tiles = FALSE;		/* Invalidate tile structs themselves */
   Boolean trash_coords = FALSE;	/* Invalidate tile prezoom coords */
   Boolean trash_all_data = FALSE;	/* Invalidate all saved data */
   Boolean trash_X_data = FALSE;	/* Invalidate saved Ximages & Pixmaps */
   Boolean full_screen_expose = FALSE;	/* Re-expose entire screen */
   Boolean new_lookup16 = FALSE;

   cb.flags = 0;

   /* Check the values of image_width and image_height.  If they are	*/
   /* nonsense, reset them to 1 (to avoid crashing) and continue.	*/

   if (biw->bim.image_height <= 0)
      biw->bim.image_height = 1;
   if (biw->bim.image_width <= 0)
      biw->bim.image_width = 1;

   /* Copy "saved" values for display mode resources into "current"	*/
   /* values, unless the current values have been explicitly changed.	*/
   /* This usually results in a no-op since the saved and current	*/
   /* values are the same, unless of course the user changed the saved	*/
   /* values (in which case this causes the changes to go "active").	*/
   /* Also, if the mode has changed, this copies the new mode's saved	*/
   /* values into current.						*/

   if (COLOR_MODE) {
      if (biw->bim.dither_mode == old->bim.dither_mode)
         biw->bim.dither_mode = biw->bim.color_dither;
      if (biw->bim.stretch_policy == old->bim.stretch_policy)
         biw->bim.stretch_policy = biw->bim.color_stretch_policy;
      if (biw->bim.colormap_policy == old->bim.colormap_policy)
         biw->bim.colormap_policy = biw->bim.color_colormap_policy;
      if (biw->bim.visual_type == old->bim.visual_type)
         biw->bim.visual_type = biw->bim.color_visual_type;
   }
   else if (PSEUDO_SET) {
      if (biw->bim.dither_mode == old->bim.dither_mode)
         biw->bim.dither_mode = biw->bim.pseudo_dither;
      if (biw->bim.stretch_policy == old->bim.stretch_policy)
         biw->bim.stretch_policy = biw->bim.pseudo_stretch_policy;
      if (biw->bim.colormap_policy == old->bim.colormap_policy)
         biw->bim.colormap_policy = biw->bim.pseudo_colormap_policy;
      if (biw->bim.visual_type == old->bim.visual_type)
         biw->bim.visual_type = biw->bim.pseudo_visual_type;
   }
   else {		/* BW */
      if (biw->bim.dither_mode == old->bim.dither_mode)
         biw->bim.dither_mode = biw->bim.bw_dither;
      if (biw->bim.stretch_policy == old->bim.stretch_policy)
         biw->bim.stretch_policy = biw->bim.bw_stretch_policy;
      if (biw->bim.colormap_policy == old->bim.colormap_policy)
         biw->bim.colormap_policy = biw->bim.bw_colormap_policy;
      if (biw->bim.visual_type == old->bim.visual_type)
         biw->bim.visual_type = biw->bim.bw_visual_type;
   }

   /* Check for a change in visual type.  This may happen as a result	*/
   /* of colormapPolicy, imageMode, visualType, or enableDirectColor	*/
   /* changing.  Note: colormapPolicy itself may change to/from		*/
   /* FULL_COLOR as a result of the GetVisual call, which is why we	*/
   /* call ValidateCmapResources afterwards.				*/

   if (old->bim.image_mode != biw->bim.image_mode ||
            old->bim.colormap_policy != biw->bim.colormap_policy ||
	    old->bim.visual_type != biw->bim.visual_type ||
	    old->bim.enable_direct_color != biw->bim.enable_direct_color)
      new_visual = GetVisual(biw);

   /* For some strange, unexplained reason, doing a Unrealize/Realize	*/
   /* pair (followed by a Manage of course) will often leave the widget	*/
   /* at absolute minimum size instead of what it should be.  This is	*/
   /* despite the core size resources being correct!  Anyway, the	*/
   /* workaround for this is to find the Shell widget, turn off		*/
   /* allowShellResize temporarily, do the realize/unrealize pair, then	*/
   /* restore allowShellResize to what it was.  This way, the enclosing	*/
   /* window doesn't change size, and everybody's happy.  It also	*/
   /* avoids ugly flashing effects as the window shrinks and (maybe)	*/
   /* re-expands.  Also, we must Unrealize before munging the colormap,	*/
   /* and re-Realize afterwards.					*/

   if (new_visual) {
      if (XtIsManaged((Widget) biw))
         managed = TRUE;
      if (XtIsRealized((Widget) biw)) {
         realize = TRUE;
         topLevel = (Widget) biw;
         while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
            topLevel = XtParent(topLevel);
         XtVaGetValues(topLevel, XtNallowShellResize, &shell_resize, NULL);
         XtVaSetValues(topLevel, XtNallowShellResize, FALSE, NULL);
         XtUnrealizeWidget((Widget)biw);
      }
      if (biw->bim.tmp_ximage) {
         _XvicMemoryReturn(biw,
	     biw->bim.tmp_ximage->height * biw->bim.tmp_ximage->bytes_per_line);
         XDestroyImage(biw->bim.tmp_ximage);
         biw->bim.tmp_ximage = NULL;
      }
   }

   if (old->bim.image_mode != biw->bim.image_mode) {
      trash_all_data = TRUE;
      cb.flags |= XvicMODE_CHANGED;
      new_lookup16 = TRUE;
   }

   if (old->bim.data_type != biw->bim.data_type) {
      trash_all_data = TRUE;
      new_lookup16 = TRUE;
      cb.flags |= XvicMODE_CHANGED;
      SetDataRangeDefaults(biw, args, num_args, old->bim.raw_data_min,
						old->bim.raw_data_max);
   }

   ValidateCmapResources(biw);
   /* It's probably faster to call this every time than to test if needed */
   SetDisplayTransform(biw);

   if (old->bim.lut16_type != biw->bim.lut16_type) {
      new_lookup16 = TRUE;
      cb.flags |= XvicDITHER_CHANGED;	/* will trash X data, below */
   }

   if (old->bim.raw_data_min != biw->bim.raw_data_min ||
       old->bim.raw_data_max != biw->bim.raw_data_max ||
       old->bim.scaled_data_max != biw->bim.scaled_data_max ||
       old->bim.output_data_max != biw->bim.output_data_max) {
      trash_X_data = TRUE;
      new_lookup16 = TRUE;
      cb.flags |= XvicRANGE_CHANGED;
   }

   if (new_lookup16)
      SetDataType(biw);

   /* Check to see if we need a new colormap.  We only need one if the	*/
   /* visual changed, or if the colormapPolicy changed and one of them	*/
   /* was ALLOC.							*/

   if (((old->bim.colormap_policy != biw->bim.colormap_policy) &&
        (old->bim.colormap_policy == XvicALLOC ||
         biw->bim.colormap_policy == XvicALLOC)) ||
       new_visual) {
      GetColormap(biw);
      new_colormap = TRUE;
   }

   /* Rewrite the values in the colormap if most anything changed.	*/
   /* lut_type is a special case; a change here always needs to call	*/
   /* SetUpColormap, but it only needs to trash the X data if		*/
   /* stretch_policy is SW.						*/

   if (old->bim.colormap_policy != biw->bim.colormap_policy ||
       old->bim.stretch_policy != biw->bim.stretch_policy ||
       old->bim.dither_mode != biw->bim.dither_mode ||
       old->bim.gray_levels != biw->bim.gray_levels ||
       old->bim.red_levels != biw->bim.red_levels ||
       old->bim.green_levels != biw->bim.green_levels ||
       old->bim.blue_levels != biw->bim.blue_levels)
      cb.flags |= XvicDITHER_CHANGED;

   if ((cb.flags & XvicDITHER_CHANGED) ||
       old->bim.image_mode != biw->bim.image_mode ||
       new_colormap ||
       ((old->bim.lut_type != biw->bim.lut_type) &&
		biw->bim.stretch_policy == XvicUSE_SW)) {
      CALL_ReleaseGrColors(biw,((Widget)biw));
      SetUpColormap(biw);
      CALL_SetUpGrColors(biw,((Widget)biw));
      trash_X_data = TRUE;
   }
   else if (old->bim.lut_type != biw->bim.lut_type)
      SetUpColormap(biw);	/* stretchPolicy is HW & nothing else changed */

   if (old->bim.lut_type != biw->bim.lut_type)
      cb.flags |= XvicDITHER_CHANGED;

   if (new_visual) {
      if (realize) {
         if (managed)
            XtManageChild((Widget)biw);
         XtRealizeWidget((Widget)biw);
         XtVaSetValues(topLevel, XtNallowShellResize, shell_resize, NULL);
      }
      trash_X_data = TRUE;
   }

   if (new_colormap)
      CALL_InstallColormap(biw, ((Widget)biw));

   /* Set up the zoom factors and screen pan */

   SetUpZoomPan(biw);

   /* Check tile size */

   if (biw->bim.tile_width <= 0 || biw->bim.tile_width > biw->bim.image_width)
      biw->bim.tile_width = biw->bim.image_width;
   if (biw->bim.tile_height <= 0 || biw->bim.tile_height>biw->bim.image_height)
      biw->bim.tile_height = biw->bim.image_height;

   /* Check for a change in image or tile size, which blows away all	*/
   /* saved data, including the tile structs themselves.		*/

   if (old->bim.image_height != biw->bim.image_height ||
       old->bim.image_width != biw->bim.image_width) {
      trash_tiles = TRUE;
      cb.flags |= XvicSIZE_CHANGED;
   }
   if (old->bim.tile_height != biw->bim.tile_height ||
       old->bim.tile_width != biw->bim.tile_width)
      trash_tiles = TRUE;

   /* Changes in prezoom values invalidate the data, but also forces	*/
   /* recomputing the prezoom coordinates in the tiles.			*/

   if (old->bim.x_prezoom_in != biw->bim.x_prezoom_in ||
       old->bim.x_prezoom_out != biw->bim.x_prezoom_out ||
       old->bim.y_prezoom_in != biw->bim.y_prezoom_in ||
       old->bim.y_prezoom_out != biw->bim.y_prezoom_out ||
       old->bim.x_presubpixel_pan != biw->bim.x_presubpixel_pan ||
       old->bim.y_presubpixel_pan != biw->bim.y_presubpixel_pan)
      trash_coords = TRUE;

   /* Changes in data save policy invalidate all saved data, but the	*/
   /* tiles themselves are okay.					*/

   if (old->bim.data_save_policy != biw->bim.data_save_policy)
      trash_all_data = TRUE;

   /* Changes in subpixel pan or zoom factor only invalidate the X data */

   if (old->bim.x_zoom_in != biw->bim.x_zoom_in ||
       old->bim.x_zoom_out != biw->bim.x_zoom_out ||
       old->bim.y_zoom_in != biw->bim.y_zoom_in ||
       old->bim.y_zoom_out != biw->bim.y_zoom_out) {
      trash_X_data = TRUE;
      cb.flags = XvicZOOM_CHANGED;
   }
   if (old->bim.x_subpixel_pan != biw->bim.x_subpixel_pan ||
       old->bim.y_subpixel_pan != biw->bim.y_subpixel_pan) {
      trash_X_data = TRUE;
      cb.flags = XvicSUBPIXEL_CHANGED;
   }

   /* Check for a change in view size.  Xt will automatically resize. */

   if (old->bim.view_width != biw->bim.view_width) {
      biw->core.width = biw->bim.view_width +
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   }
   if (old->bim.view_height != biw->bim.view_height) {
      biw->core.height = biw->bim.view_height +
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   }
   if (old->primitive.highlight_thickness!=biw->primitive.highlight_thickness ||
       old->primitive.shadow_thickness != biw->primitive.shadow_thickness) {
      biw->bim.x_dpy_off = biw->primitive.highlight_thickness +
			   biw->primitive.shadow_thickness;
      biw->bim.y_dpy_off = biw->primitive.highlight_thickness +
			   biw->primitive.shadow_thickness;
      full_screen_expose = TRUE;
   }

   /* Set up the hierarchy of Booleans - setting one sets all the ones	*/
   /* below it.								*/

   if (trash_tiles)
      trash_coords = TRUE;
   if (trash_coords)
      trash_all_data = TRUE;
   if (trash_all_data)
      trash_X_data = TRUE;
   if (trash_X_data)
      full_screen_expose = TRUE;

   if (trash_tiles)
      CreateTiles(biw);
   if (trash_coords)
      SetTileCoords(biw);
   if (trash_all_data)
      FreeAllTileData(biw);
   if (trash_X_data) {
      FreeXTileData(biw);
      SetTileDpyCoords(biw);
   }

   /* If the pan value has changed, do it, unless we're doing a full	*/
   /* expose anyway, in which case there's no point.  Note that we	*/
   /* check the actual screen pan against the old screen pan, because	*/
   /* that's the really important value.  We don't care if the resource	*/
   /* changed if it doesn't affect the screen pan (e.g. due to a zoom	*/
   /* out).  We also don't compare against "old->bim" because we have	*/
   /* our own old values.						*/

   if ((biw->bim.x_screen_pan != biw->bim.x_old_screen_pan) ||
       (biw->bim.y_screen_pan != biw->bim.y_old_screen_pan)) {
      if (full_screen_expose) {
         biw->bim.x_old_screen_pan = biw->bim.x_screen_pan;
         biw->bim.y_old_screen_pan = biw->bim.y_screen_pan;
      }
      else
         DoPan(biw);
   }

   /* However, we do care about x_pan and y_pan for the visibleArea	*/
   /* callback.								*/

   if (old->bim.x_pan != biw->bim.x_pan || old->bim.y_pan != biw->bim.y_pan)
      cb.flags |= XvicPAN_CHANGED;

   if (old->bim.maximum_memory != biw->bim.maximum_memory) {
      _XvicMemoryGrab(biw, 0);		/* Check new memory usage */
   }

   /* There's nothing to do here if work_proc_policy changes */

   /* Save the current set of display modes */

   CopyCurrentToSavedResources(biw);

   /* Call the visibleAreaCallback if needed */

   if (cb.flags && biw->bim.visible_area_callback) {
      cb.reason = XvicCR_VISIBLE_AREA;
      XtCallCallbackList((Widget)biw, biw->bim.visible_area_callback, &cb);
   }

   return full_screen_expose;

}

/************************************************************************/
/* SetValuesAlmost method						*/
/*									*/
/* If this routine is called, it means the geometry change requested	*/
/* by SetValues was rejected.  We accept whatever compromise we're	*/
/* given, but we must reset viewHeight/Width to reflect the actual	*/
/* geometry.  Note that the Resize method is not called unless the	*/
/* geometry actually changed.						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetValuesAlmost(old_w, new_w, request, reply)
   Widget old_w;
   Widget new_w;
   XtWidgetGeometry *request;
   XtWidgetGeometry *reply;
#else
SetValuesAlmost(
   Widget old_w,
   Widget new_w,
   XtWidgetGeometry *request,
   XtWidgetGeometry *reply)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) new_w;

   biw->bim.view_width = biw->core.width -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   if ((int)biw->core.width < (int)(2 * (biw->primitive.highlight_thickness) +
				   biw->primitive.shadow_thickness))
      biw->bim.view_width = 0;		/* can't check < 0 because unsigned */
   biw->bim.view_height = biw->core.height -
		2 * (biw->primitive.highlight_thickness +
		     biw->primitive.shadow_thickness);
   if ((int)biw->core.height < (int)(2 * (biw->primitive.highlight_thickness) +
				    biw->primitive.shadow_thickness))
      biw->bim.view_height = 0;		/* can't check < 0 because unsigned */

   *request = *reply;		/* accept the geometry */

}

/************************************************************************/
/************************************************************************/
/*  P R I M I T I V E   M E T H O D S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* BorderUnhighlight method (XmPrimitive)				*/
/*									*/
/* This routine is needed only because of brain-damage on the part of	*/
/* Motif Primitive.  For some reason, it doesn't just clear the		*/
/* highlight, it first draws a border using the *parent* widget's	*/
/* background GC!!  Talk about mondo illegal - no wonder Motif doesn't	*/
/* like different visual types!  This code is copied straight from	*/
/* Primitive.c, with the exception of course of removing that silly	*/
/* _XmDrawBorder(1.1)/_XmDrawHighlight(1.2) call.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
BorderUnhighlight(w)
   Widget w;
#else
BorderUnhighlight(
   Widget w)
#endif
{

#if USE_MOTIF

   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int thick = biw->primitive.highlight_thickness;

   biw->primitive.highlighted = FALSE;
   biw->primitive.highlight_drawn = FALSE;

   if (biw->core.width == 0 || biw->core.height == 0 || thick == 0)
      return;

#ifdef MOTIF_1_1
   XClearArea(XtDisplay(biw), XtWindow(biw),
		0, 0, biw->core.width, thick, FALSE);
   XClearArea(XtDisplay(biw), XtWindow(biw),
		0, biw->core.height - thick, biw->core.width, thick, FALSE);
   XClearArea(XtDisplay(biw), XtWindow(biw),
		0, 0, thick, biw->core.height, FALSE);
   XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->core.width - thick, 0, thick, biw->core.height, FALSE);
#else
   _XmClearBorder(XtDisplay(biw), XtWindow(biw), 0, 0,
		biw->core.width, biw->core.height, thick);
#endif

#else /* USE_MOTIF */
   return;
#endif

}

/************************************************************************/
/************************************************************************/
/*  B A S I C I M A G E   M E T H O D S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* InstallColormap							*/
/*									*/
/* Tells the window manager about the colormap so it will be installed	*/
/* at the appropriate times.  It also sets the X attribute on the	*/
/* window.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
InstallColormap(w)
   Widget w;
#else
InstallColormap(
   Widget w)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   Widget wlist[2];
   Widget topLevel;

   if (!XtIsRealized((Widget) biw))
      return;

   XSetWindowColormap(XtDisplay((Widget)biw), XtWindow((Widget)biw),
			biw->bim.colormap);

   /* Window list must be (widget,top-level).  So, traverse the widget	*/
   /* tree backwards until we find the shell widget, and use its window.*/

   wlist[0] = (Widget) biw;
   topLevel = (Widget) biw;
   while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
      topLevel = XtParent(topLevel);
   wlist[1] = topLevel;

   XtSetWMColormapWindows(topLevel, wlist, 2);
}

/************************************************************************/
/************************************************************************/
/*  P U B L I C   F U N C T I O N S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* XvicCreateBasicImage							*/
/************************************************************************/

Widget
#ifdef _NO_PROTO
XvicCreateBasicImage(parent, name, args, argCount)
   Widget parent;
   char *name;
   ArgList args;
   Cardinal argCount;
#else
XvicCreateBasicImage(
   Widget parent,
   char *name,
   ArgList args,
   Cardinal argCount)
#endif /* _NO_PROTO */
{
   return (XtCreateWidget(name, xvicBasicImageWidgetClass, parent,
			args, argCount));
}

/************************************************************************/
/* XvicImageClear							*/
/*									*/
/* Invalidates all saved data and clears the window, causing expose	*/
/* events to occur for the entire displayed area.  This funciton is	*/
/* normally used to change the image being displayed.  One option is to	*/
/* call XvicImageWrite on the new data with new_data set to True, but	*/
/* this is not always practical, and the application has to figure out	*/
/* what is being displayed so it doesn't send too much data.  Usually,	*/
/* it is easier to tell the widget to invalidate all its data and allow	*/
/* the normal Expose callback to repaint the screen.  This function	*/
/* accomplishes that task.						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageClear(w)
   Widget w;
#else
XvicImageClear(
   Widget w)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;

   FreeAllTileData(biw);		/* nuke all the data */
   SetTileDpyCoords(biw);

   if (XtIsRealized((Widget)biw))	/* clear window and generate expose */
      XClearArea(XtDisplay((Widget)biw), XtWindow((Widget)biw), 0,0,0,0, True);

}

/************************************************************************/
/* XvicImageDisplayBounds						*/
/*									*/
/* Returns the Image coordinates of the edges of the displayed		*/
/* window.  The coordinates may be off the edge of the image if the	*/
/* window is bigger than the image.  This is just a convenience		*/
/* routine; the application could get view_width and view_height and	*/
/* figure it out for itself.  Note that at some zooms, the returned	*/
/* coordinates may be slightly larger than the window, but they will	*/
/* never be smaller.							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageDisplayBounds(w, x1, y1, x2, y2)
   Widget w;
   int *x1;
   int *y1;
   int *x2;
   int *y2;
#else
XvicImageDisplayBounds(
   Widget w,
   int *x1,
   int *y1,
   int *x2,
   int *y2)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;

   *x1 = X_Scr2Img(0);
   *y1 = Y_Scr2Img(0);
   *x2 = X_Scr2Img(biw->bim.view_width-1);
   *y2 = Y_Scr2Img(biw->bim.view_height-1);
}

/************************************************************************/
/* XvicImageGetColorLUT							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageGetColorLUT(w, red_lut, green_lut, blue_lut)
   Widget w;
   int *red_lut;
   int *green_lut;
   int *blue_lut;
#else
XvicImageGetColorLUT(
   Widget w,
   int *red_lut,
   int *green_lut,
   int *blue_lut)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;

   if (red_lut != NULL) {
      for (i=0; i<LUT_SIZE; i++)
         red_lut[i] = biw->bim.red_lut[i];
   }
   if (green_lut != NULL) {
      for (i=0; i<LUT_SIZE; i++)
         green_lut[i] = biw->bim.green_lut[i];
   }
   if (blue_lut != NULL) {
      for (i=0; i<LUT_SIZE; i++)
         blue_lut[i] = biw->bim.blue_lut[i];
   }
}

/************************************************************************/
/* XvicImageGetColorLUT16						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageGetColorLUT16(w, red_lut, green_lut, blue_lut, lut_size)
   Widget w;
   int *red_lut;
   int *green_lut;
   int *blue_lut;
   int lut_size;
#else
XvicImageGetColorLUT16(
   Widget w,
   int *red_lut,
   int *green_lut,
   int *blue_lut,
   int lut_size)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;

   if (lut_size <= 0 || lut_size > LUT16_SIZE)
      lut_size = LUT16_SIZE;

   if (red_lut != NULL) {
      if (biw->bim.red_lut16 == NULL) {
         for (i=0; i<lut_size; i++)
            red_lut[i] = i;
      }
      else {
         for (i=0; i<lut_size; i++)
            red_lut[i] = biw->bim.red_lut16[i];
      }
   }
   if (green_lut != NULL) {
      if (biw->bim.green_lut16 == NULL) {
         for (i=0; i<lut_size; i++)
            green_lut[i] = i;
      }
      else {
         for (i=0; i<lut_size; i++)
            green_lut[i] = biw->bim.green_lut16[i];
      }
   }
   if (blue_lut != NULL) {
      if (biw->bim.blue_lut16 == NULL) {
         for (i=0; i<lut_size; i++)
            blue_lut[i] = i;
      }
      else {
         for (i=0; i<lut_size; i++)
            blue_lut[i] = biw->bim.blue_lut16[i];
      }
   }
}

/************************************************************************/
/* XvicImageGetMonoLUT							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageGetMonoLUT(w, lut)
   Widget w;
   int *lut;
#else
XvicImageGetMonoLUT(
   Widget w,
   int *lut)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;

   if (lut != NULL) {
      for (i=0; i<LUT_SIZE; i++)
         lut[i] = biw->bim.stretch_lut[i];
   }
}

/************************************************************************/
/* XvicImageGetMonoLUT16						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageGetMonoLUT16(w, lut, lut_size)
   Widget w;
   int *lut;
   int lut_size;
#else
XvicImageGetMonoLUT16(
   Widget w,
   int *lut,
   int lut_size)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;

   if (lut != NULL) {
      if (biw->bim.stretch_lut16 == NULL) {
         for (i=0; i<lut_size; i++)
            lut[i] = i;
      }
      else {
         for (i=0; i<lut_size; i++)
            lut[i] = biw->bim.stretch_lut16[i];
      }
   }
}

/************************************************************************/
/* XvicImageSetColorLUT							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetColorLUT(w, red_lut, green_lut, blue_lut)
   Widget w;
   int *red_lut;
   int *green_lut;
   int *blue_lut;
#else
XvicImageSetColorLUT(
   Widget w,
   int *red_lut,
   int *green_lut,
   int *blue_lut)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;
   Boolean new_stretch;
   XExposeEvent event;

   new_stretch = FALSE;

   if (red_lut != NULL) {
      for (i=0; i<LUT_SIZE; i++) {
         if (red_lut[i] != biw->bim.red_lut[i]) {
            new_stretch = TRUE;
            break;
         }
      }
      for (i=0; i<LUT_SIZE; i++)
         biw->bim.red_lut[i] = red_lut[i];
   }
   if (green_lut != NULL) {
      if (!new_stretch) {
         for (i=0; i<LUT_SIZE; i++) {
            if (green_lut[i] != biw->bim.green_lut[i]) {
               new_stretch = TRUE;
               break;
            }
         }
      }
      for (i=0; i<LUT_SIZE; i++)
         biw->bim.green_lut[i] = green_lut[i];
   }
   if (blue_lut != NULL) {
      if (!new_stretch) {
         for (i=0; i<LUT_SIZE; i++) {
            if (blue_lut[i] != biw->bim.blue_lut[i]) {
               new_stretch = TRUE;
               break;
            }
         }
      }
      for (i=0; i<LUT_SIZE; i++)
         biw->bim.blue_lut[i] = blue_lut[i];
   }

   if (new_stretch) {
      if (biw->bim.stretch_policy == XvicUSE_HW)
         SetUpColormap(biw);
      else {				/* Repaint whole screen */
         FreeXTileData(biw);		/* stretches are already in X data! */
         if (XtIsRealized((Widget) biw)) {
            event.x = 0;
            event.y = 0;
            event.width = biw->bim.view_width;
            event.height = biw->bim.view_height;

            XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->bim.x_dpy_off, biw->bim.y_dpy_off,
		biw->bim.view_width, biw->bim.view_height,
		FALSE);

            _XvicRedisplay_Internal(biw, &event, NULL);	/* Full expose */
         }
      }
   }
}

/************************************************************************/
/* XvicImageSetColorLUT16						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetColorLUT16(w, red_lut, green_lut, blue_lut, lut_size)
   Widget w;
   int *red_lut;
   int *green_lut;
   int *blue_lut;
   int lut_size;
#else
XvicImageSetColorLUT16(
   Widget w,
   int *red_lut,
   int *green_lut,
   int *blue_lut,
   int lut_size)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;
   Boolean new_stretch;
   XExposeEvent event;

   new_stretch = FALSE;

   if (red_lut != NULL) {
      if (biw->bim.red_lut16 == NULL)
         biw->bim.red_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
      for (i=0; i<lut_size; i++) {
         if (red_lut[i] != biw->bim.red_lut16[i]) {
            new_stretch = TRUE;
            break;
         }
      }
      for (i=0; i<lut_size; i++)
         biw->bim.red_lut16[i] = red_lut[i];
   }
   if (green_lut != NULL) {
      if (biw->bim.green_lut16 == NULL)
         biw->bim.green_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
      if (!new_stretch) {
         for (i=0; i<lut_size; i++) {
            if (green_lut[i] != biw->bim.green_lut16[i]) {
               new_stretch = TRUE;
               break;
            }
         }
      }
      for (i=0; i<lut_size; i++)
         biw->bim.green_lut16[i] = green_lut[i];
   }
   if (blue_lut != NULL) {
      if (biw->bim.blue_lut16 == NULL)
         biw->bim.blue_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
      if (!new_stretch) {
         for (i=0; i<lut_size; i++) {
            if (blue_lut[i] != biw->bim.blue_lut16[i]) {
               new_stretch = TRUE;
               break;
            }
         }
      }
      for (i=0; i<lut_size; i++)
         biw->bim.blue_lut16[i] = blue_lut[i];
   }

   if (new_stretch) {
      SetDataType(biw);
      FreeXTileData(biw);		/* stretches are already in X data! */
      if (XtIsRealized((Widget) biw)) {		/* Repaint whole screen */
         event.x = 0;
         event.y = 0;
         event.width = biw->bim.view_width;
         event.height = biw->bim.view_height;

         XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->bim.x_dpy_off, biw->bim.y_dpy_off,
		biw->bim.view_width, biw->bim.view_height,
		FALSE);

         _XvicRedisplay_Internal(biw, &event, NULL);	/* Full expose */
      }
   }
}

/************************************************************************/
/* XvicImageSetMonoLUT							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetMonoLUT(w, lut)
   Widget w;
   int *lut;
#else
XvicImageSetMonoLUT(
   Widget w,
   int *lut)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;
   Boolean new_stretch;
   XExposeEvent event;

   new_stretch = FALSE;

   if (lut != NULL) {
      for (i=0; i<LUT_SIZE; i++) {
         if (lut[i] != biw->bim.stretch_lut[i]) {
            new_stretch = TRUE;
            break;
         }
      }
      for (i=0; i<LUT_SIZE; i++)
         biw->bim.stretch_lut[i] = lut[i];
   }

   if (new_stretch) {
      if (biw->bim.stretch_policy == XvicUSE_HW)
         SetUpColormap(biw);
      else {				/* Repaint whole screen */
         FreeXTileData(biw);		/* stretches are already in X data! */
         if (XtIsRealized((Widget) biw)) {
            event.x = 0;
            event.y = 0;
            event.width = biw->bim.view_width;
            event.height = biw->bim.view_height;

            XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->bim.x_dpy_off, biw->bim.y_dpy_off,
		biw->bim.view_width, biw->bim.view_height,
		FALSE);

            _XvicRedisplay_Internal(biw, &event, NULL);	/* Full expose */
         }
      }
   }
}

/************************************************************************/
/* XvicImageSetMonoLUT16						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetMonoLUT16(w, lut, lut_size)
   Widget w;
   int *lut;
   int lut_size;
#else
XvicImageSetMonoLUT16(
   Widget w,
   int *lut,
   int lut_size)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   int i;
   Boolean new_stretch;
   XExposeEvent event;

   new_stretch = FALSE;

   if (lut != NULL) {
      if (biw->bim.stretch_lut16 == NULL)
         biw->bim.stretch_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
      for (i=0; i<lut_size; i++) {
         if (lut[i] != biw->bim.stretch_lut16[i]) {
            new_stretch = TRUE;
            break;
         }
      }
      for (i=0; i<lut_size; i++)
         biw->bim.stretch_lut16[i] = lut[i];
   }

   if (new_stretch) {
      SetDataType(biw);
      FreeXTileData(biw);		/* stretches are already in X data! */
      if (XtIsRealized((Widget) biw)) {
         event.x = 0;
         event.y = 0;
         event.width = biw->bim.view_width;
         event.height = biw->bim.view_height;

         XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->bim.x_dpy_off, biw->bim.y_dpy_off,
		biw->bim.view_width, biw->bim.view_height,
		FALSE);

         _XvicRedisplay_Internal(biw, &event, NULL);	/* Full expose */
      }
   }
}

/************************************************************************/
/************************************************************************/
/* XvicImageWrite							*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageWrite(w, image, new_data)
   Widget w;
   XvicImageData *image;
   Boolean new_data;
#else
XvicImageWrite(
   Widget w,
   XvicImageData *image,
#if NeedWidePrototypes
   int new_data)
#else
   Boolean new_data)
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   _XvicRect user_rect;
   _XvicRect intersect_rect;
   Tile *tile;
   int i;
   int width, height;
   Boolean mem_okay = TRUE;		/* TRUE if MEMORY_WIDGET is possible */

   if (!XtIsRealized((Widget) biw))
      return;				/* Do nothing if not realized */

   user_rect.x1 = image->x;
   user_rect.y1 = image->y;
   user_rect.x2 = image->x + image->width - 1;
   user_rect.y2 = image->y + image->height - 1;

   /* Check which tiles this goes in */

   for (i=0; i<biw->bim.num_tiles; i++) {
      tile = &biw->bim.tiles[i];
      if (_XvicRectIntersectRect(&user_rect, &tile->img, &intersect_rect)) {

         /* Found a tile */

         TOUCH_TILE(i);

         switch (biw->bim.data_save_policy) {

            case XvicNONE:

               DrawTileRaw(biw, tile, image, new_data);

               break;

            case XvicRAW:

               CopyRawData(biw, tile, image, &intersect_rect, &mem_okay);
               DrawTileRaw(biw, tile, tile->raw, new_data);

               break;

            case XvicXIMAGE:

               width = tile->dpy.x2 - tile->dpy.x1 + 1;
               height = tile->dpy.y2 - tile->dpy.y1 + 1;
               _XvicGetXImage(biw, &tile->ximage, width, height);
               tile->ximage_size = height * tile->ximage->bytes_per_line;

               _XvicCopyRawXimage(biw, tile, image, tile->ximage,
					&intersect_rect);
               DrawTileXimage(biw, tile, tile->ximage, new_data);

               break;

            case XvicPIXMAP:

               width = tile->dpy.x2 - tile->dpy.x1 + 1;
               height = tile->dpy.y2 - tile->dpy.y1 + 1;
               if (tile->pixmap == None) {	/* allocate it */
                  _XvicMemoryGrab(biw, width * height);
                  tile->pixmap = XCreatePixmap(XtDisplay(biw), XtWindow(biw),
			width, height, biw->core.depth);
                  tile->pixmap_size = width * height;
                  /*!!!! Should do something with a BadAlloc error here!!!!*/
               }
               biw->bim.protect_tmp_ximage = TRUE;
               _XvicGetXImage(biw, &biw->bim.tmp_ximage, width, height);
               biw->bim.protect_tmp_ximage = FALSE;
               _XvicCopyRawXimage(biw, tile, image, biw->bim.tmp_ximage,
					&intersect_rect);
               CopyXimagePixmap(biw, tile, biw->bim.tmp_ximage, tile->pixmap,
				&intersect_rect);
               DrawTilePixmap(biw, tile, tile->pixmap, new_data);

               break;

            default:
               FatalError(biw, "BadDataSavePolicy",
		   "Internal error: Unknown dataSavePolicy in XvicImageWrite");
         }
      }
   }
}

/************************************************************************/
/************************************************************************/
/*  P R I V A T E   E X T E R N A L   F U N C T I O N S			*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* _XvicFree								*/
/*									*/
/* Frees memory allocated by _XvicMalloc, keeping track of total memory	*/
/* in use.  The size is (unfortunately) needed to keep the memory	*/
/* counter correct (standard free() doesn't give you a size).		*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicFree(biw, ptr, size)
   XvicBasicImageWidget biw;
   void *ptr;
   int size;
#else
_XvicFree(
   XvicBasicImageWidget biw,
   void *ptr,
   int size)
#endif /* _NO_PROTO */
{
   _XvicMemoryReturn(biw, size);

   free(ptr);
}

/************************************************************************/
/* _XvicGetVisibleRect							*/
/*									*/
/* Returns a _XvicRect structure describing the visible area.		*/
/* Note: "visible" means it's visible according to the window size,	*/
/* pan, etc.  It does not take into account window obscurations or	*/
/* iconification or anything like that.					*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicGetVisibleRect(biw, rect)
   XvicBasicImageWidget biw;
   _XvicRect *rect;
#else
_XvicGetVisibleRect(
   XvicBasicImageWidget biw,
   _XvicRect *rect)
#endif /* _NO_PROTO */
{
   rect->x1 = X_Scr2Img(0);
   rect->x1 = MAX(rect->x1, 0);
   rect->y1 = Y_Scr2Img(0);
   rect->y1 = MAX(rect->y1, 0);
   rect->x2 = X_Scr2Img(biw->bim.view_width-1);
   rect->x2 = MIN(rect->x2, biw->bim.image_width-1);
   rect->y2 = Y_Scr2Img(biw->bim.view_height-1);
   rect->y2 = MIN(rect->y2, biw->bim.image_height-1);
}

/************************************************************************/
/* _XvicGetXImage							*/
/*									*/
/* Gets an XImage structure and data area that are big enough for the	*/
/* given width and height.  Any existing XImage at the given address	*/
/* is re-used if possible (if it is big enough); if not, it is freed	*/
/* and a new one is created.  If no XImage has yet been allocated,	*/
/* *ximage better be NULL!						*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicGetXImage(biw, ximage, width, height)
   XvicBasicImageWidget biw;
   XImage **ximage;
   int width;
   int height;
#else
_XvicGetXImage(
   XvicBasicImageWidget biw,
   XImage **ximage,
   int width,
   int height)
#endif /* _NO_PROTO */
{

   /* Check to make sure the XImage struct is big enough */

   if (*ximage != NULL) {
      if ((*ximage)->width < width || (*ximage)->height < height) {
         _XvicMemoryReturn(biw,
	     (*ximage)->height * (*ximage)->bytes_per_line);
         XDestroyImage(*ximage);
         *ximage = NULL;
      }
   }

   if (*ximage == NULL) {			/* Create a new one */
      do {
         *ximage = XCreateImage(XtDisplay(biw), biw->bim.vis.visual,
		biw->core.depth, ZPixmap, 0, NULL,
		width, height, 8, 0);
         if (!*ximage) _XvicMemoryPanic(biw);
      } while (*ximage == NULL);

      (*ximage)->data =
		(char *)_XvicMalloc(biw, height * (*ximage)->bytes_per_line);
   }
}

/************************************************************************/
/* _XvicMalloc								*/
/*									*/
/* Allocates memory for use within the widget, keeping track of the	*/
/* total memory utilization.  _XvicMemoryGrab is called to check the	*/
/* memory limits and free anything up if it overflows.  If, after that,	*/
/* the actual allocation fails, we must do the same sorts of things to	*/
/* try to free up memory until the allocation succeeds.  Note that	*/
/* if _XvicMemoryGrab is unable to get the memory beneath the specified	*/
/* limit, the malloc will proceed anyway, on the theory that we're	*/
/* already at the absolute minimum memory usage if _XvicMemoryGrab	*/
/* can't do anything else, so it's better to use a little more than to	*/
/* risk crashing the widget.						*/
/*									*/
/* Note that this routine will never return NULL.  If the allocation	*/
/* doesn't succeed after freeing everything possible, the memory panic	*/
/* routine is called.  If it returns, we try again.  If it doesn't	*/
/* return, we have aborted.  So, the application doesn't have to deal	*/
/* with it.								*/
/************************************************************************/

void *
#ifdef _NO_PROTO
_XvicMalloc(biw, size)
   XvicBasicImageWidget biw;
   int size;
#else
_XvicMalloc(
   XvicBasicImageWidget biw,
   int size)
#endif /* _NO_PROTO */
{
   void *ptr;

   _XvicMemoryGrab(biw, size);

   do {
      ptr = (void *)malloc(size);
      if (!ptr) _XvicMemoryPanic(biw);
   } while (ptr == NULL);

   return ptr;

}

/************************************************************************/
/* _XvicMemoryGrab							*/
/*									*/
/* Grabs memory from the system when _XvicMalloc can't be used		*/
/* directly.  It increments the used memory by the specified amount,	*/
/* and checks for overflow.  If there is an overflow, tmp_ximage is	*/
/* freed if it is too big, and then tiles are freed (if available) in	*/
/* Least Recently Used order, until the memory goes under the bounds,	*/
/* or we run out of tiles (current_tile is never freed).  The		*/
/* assumption here is that the memory is about to be allocated, so	*/
/* space must be provided.  Calling _XvicMemoryGrab with a size of 0	*/
/* causes a check on memory usage (possibly freeing things) without	*/
/* actually reserving any more space.  This is used in SetValues when	*/
/* maximumMemory changes.						*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicMemoryGrab(biw, size)
   XvicBasicImageWidget biw;
   int size;
#else
_XvicMemoryGrab(
   XvicBasicImageWidget biw,
   int size)
#endif /* _NO_PROTO */
{
   Boolean deleted = TRUE;

   if (biw->bim.maximum_memory > 0 &&
       biw->bim.memory_used + size > biw->bim.maximum_memory && deleted) {

      DPR(("XvicMemoryGrab MEMORY OVERFLOW!!! mem used=%d, new=%d, max=%d\n",
		biw->bim.memory_used, size, biw->bim.maximum_memory));	/*!!!!*/

      FreeTmpXimageIfTooBig(biw);

      while (biw->bim.maximum_memory > 0 &&
          biw->bim.memory_used + size > biw->bim.maximum_memory && deleted) {

         /* If we have a tile list, search it to find the least recently*/
         /* used tile, and free that tile.  Keep it up until there's	*/
         /* nothing left to delete, or we free up enough space.  Note	*/
         /* that current_tile is never affected.			*/

         deleted = FreeLRUTile(biw);
      }
   }

   biw->bim.memory_used += size;

#if 0	/*!!!!*/
DPR(("MemoryGrab: used=%d, amount=%d\n", biw->bim.memory_used, size));
#endif

}

/************************************************************************/
/* _XvicMemoryPanic							*/
/*									*/
/* This routine is called when a malloc() has failed.  It frees up some	*/
/* memory and returns so the application can try again.  Eventually,	*/
/* when it can't free any more, it prints a fatal error and dies.	*/
/* The application should not expect this routine to return unless some	*/
/* memory was freed up.							*/
/* ****Possible future enhancement***!!!!				*/
/* Create an application callback for a memory panic situation.  The	*/
/* application can free anything it can, then return to let the widget	*/
/* try again.  Destroying this widget is not an option because the	*/
/* call stack must be unwound (back to the event loop) before the	*/
/* destruction happens, and there would be too many cases of null	*/
/* pointers to deal with before it gets there.  This memory callback is	*/
/* not yet implemented because if the application is running that close	*/
/* to the edge, there are plenty of opportunities for crashing in X	*/
/* that *can't* be trapped.  So, it hardly seems worth it.  Applications*/
/* have control over memory usage via XvicNmaximumMemory, and the	*/
/* widget will free everything it possibly can before dying anyway.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicMemoryPanic(biw)
   XvicBasicImageWidget biw;
#else
_XvicMemoryPanic(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{

   /* First check the temp ximage struct */

   if (FreeTmpXimageIfTooBig(biw))
      return;				/* try again */

   /* Next try freeing tiles.  FreeLRUTile will keep returning TRUE	*/
   /* until all possible tiles have been freed.				*/

   if (FreeLRUTile(biw))
      return;				/* try again */

   /* Uh oh... no more memory to free.  Bummer.  This is where an	*/
   /* application callback would go if it were implemented.  As it is,	*/
   /* just bail out with a fatal error.					*/

   FatalError(biw, "NoMemory", "Out of physical memory; no more can be freed");
}

/************************************************************************/
/* _XvicMemoryReturn							*/
/*									*/
/* Returns memory to the system when _XvicFree can't be used.		*/
/* Basically, it just decrements the used memory by the specified	*/
/* amount.								*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicMemoryReturn(biw, size)
   XvicBasicImageWidget biw;
   int size;
#else
_XvicMemoryReturn(
   XvicBasicImageWidget biw,
   int size)
#endif /* _NO_PROTO */
{

   biw->bim.memory_used -= size;
   if (biw->bim.memory_used < 0) {
      DPR(("_XvicMemoryReturn() MEMORY UNDERFLOW!! new count=%d, freed %d\n",
		biw->bim.memory_used, size));
   }

if (biw->bim.memory_used == 0)	/*!!!!*/
   DPR(("MemoryRetn: used=%d, size=%d\n", biw->bim.memory_used, size)); /*!!!!*/

}

/************************************************************************/
/* _XvicRedisplay_Internal						*/
/*									*/
/* This does the work of the Redisplay method.  It should be called by	*/
/* internal functions rather than the real Redisplay because it doesn't	*/
/* do the *_(min|max)_exp_pan adjustment, since a direct Redisplay	*/
/* call won't have problems with the pan changing before the expose	*/
/* event is processed.  Only the four fields x, y, width, and height	*/
/* are used from the given event.					*/
/* Important:  This routine also does not subtract out the display	*/
/* offset (*_dpy_off).  So, for internal callers, the given event is	*/
/* already in real Scr coordinate space.				*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicRedisplay_Internal(biw, event, region)
   XvicBasicImageWidget biw;
   XExposeEvent *event;
   Region region;
#else
_XvicRedisplay_Internal(
   XvicBasicImageWidget biw,
   XExposeEvent *event,
   Region region)
#endif
{
   _XvicRect expose_rect, visible_rect;
   Tile *tile;
   _XvicRegion *outside_rgn;
   _XvicRect *rect_list;
   int i, n;
   Boolean status;
   int x, y, width, height;
   XvicImageCallbackStruct cb;

   expose_rect.x1 = event->x;
   expose_rect.x2 = event->x + event->width - 1;
   expose_rect.y1 = event->y;
   expose_rect.y2 = event->y + event->height - 1;

   /* Redraw the border if we are clobbering it. */

   if (expose_rect.x1 > expose_rect.x2 || expose_rect.y1 > expose_rect.y2)
      return;		/* Nothing to do */

   if (expose_rect.x1 < 0 || expose_rect.x2 >= (int)biw->bim.view_width ||
       expose_rect.y1 < 0 || expose_rect.y2 >= (int)biw->bim.view_height)
      RedrawBorder(biw);

   expose_rect.x1 = X_Scr2Img(expose_rect.x1);
   expose_rect.y1 = Y_Scr2Img(expose_rect.y1);
   expose_rect.x2 = X_Scr2Img(expose_rect.x2);
   expose_rect.y2 = Y_Scr2Img(expose_rect.y2);

   do {
      status = _XvicRegionUnion(&expose_rect, biw->bim.expose_rgn);
      if (!status) _XvicMemoryPanic(biw);
   } while (!status);

   /* Clip the expose region to the actual visible region		*/

   _XvicGetVisibleRect(biw, &visible_rect);
   _XvicRegionIntersect(&visible_rect, biw->bim.expose_rgn);

   /* Get the region outide the above clip, so we can clear it		*/

   do {
      outside_rgn = _XvicRegionCreateRect(&expose_rect);
      if (!outside_rgn) _XvicMemoryPanic(biw);
   } while (outside_rgn == NULL);
   do {
      status = _XvicRegionSubtract(&visible_rect, outside_rgn);
      if (!status) _XvicMemoryPanic(biw);
   } while (!status);
   n = _XvicRegionGetNumRects(outside_rgn);
   if (n != 0) {			/* Clear outside areas */
      rect_list = _XvicRegionGetRectangles(outside_rgn);
      for (i=0; i<n; i++) {
         x = X1_Img2Scr(rect_list[i].x1);
         width = X2_Img2Scr(rect_list[i].x2) - x + 1;
         y = Y1_Img2Scr(rect_list[i].y1);
         height = Y2_Img2Scr(rect_list[i].y2) - y + 1;
         X_ViewConstrain(x, width);
         Y_ViewConstrain(y, height);
         if (width > 0 && height > 0) {
            XClearArea(XtDisplay(biw), XtWindow(biw),
		x + biw->bim.x_dpy_off, y + biw->bim.y_dpy_off,
		width, height, FALSE);
            expose_rect.x1 = X_Scr2Dpy(x);
            expose_rect.y1 = Y_Scr2Dpy(y);
            expose_rect.x2 = X_Scr2Dpy(x+width-1);
            expose_rect.y2 = Y_Scr2Dpy(y+height-1);
            CALL_ClearOverlay(biw, ((Widget)biw, x, y, width, height));
#if 0	/*!!!! Do we really want to expose overlay outside the image? !!!!*/
            CALL_ExposeOverlay(biw, ((Widget)biw, NULL, &expose_rect));
#endif
         }
      }
   }
   _XvicRegionDestroy(outside_rgn);

   /* Either set up the work proc, or expose some areas */

   if (biw->bim.work_proc_policy == XvicALL) {
      if (!biw->bim.work_proc_pending) {
         XtAppAddWorkProc(XtWidgetToApplicationContext((Widget)biw),
			WorkProc, biw);
         biw->bim.work_proc_pending = TRUE;
         if (biw->bim.work_proc_active_callback) {
            cb.reason = XvicCR_WORK_PROC_ACTIVE;
            cb.flags = True;
            XtCallCallbackList((Widget)biw,
			biw->bim.work_proc_active_callback, &cb);
         }
      }
   }
   else {

      /* Expose areas that are in memory (no callback) first */

      while ((tile = NextMemoryTile(biw)))
         ExposeTile(biw, tile);

      /* XvicREAD means expose the rest (callback required) in the work proc */

      if (biw->bim.work_proc_policy == XvicREAD) {
         if (!biw->bim.work_proc_pending &&
	     !_XvicRegionIsEmpty(biw->bim.expose_rgn)) {
            XtAppAddWorkProc(XtWidgetToApplicationContext((Widget)biw),
			WorkProc, biw);
            biw->bim.work_proc_pending = TRUE;
            if (biw->bim.work_proc_active_callback) {
               cb.reason = XvicCR_WORK_PROC_ACTIVE;
               cb.flags = True;
               XtCallCallbackList((Widget)biw,
			biw->bim.work_proc_active_callback, &cb);
            }
         }
      }
      else {

         /* Expose the rest here */

         while ((tile = NextTile(biw)))
            ExposeTile(biw, tile);
      }
   }
}

/************************************************************************/
/* _XvicSetClipFromRgn							*/
/*									*/
/* Sets up clipping rectangles in the given GC based on the given	*/
/* region (which is in image coordinates).				*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicSetClipFromRgn(biw, rgn, gc, x_off, y_off)
   XvicBasicImageWidget biw;
   _XvicRegion *rgn;
   GC gc;
   int x_off;
   int y_off;
#else
_XvicSetClipFromRgn(
   XvicBasicImageWidget biw,
   _XvicRegion *rgn,
   GC gc,
   int x_off,
   int y_off)
#endif /* _NO_PROTO */
{
   int i, n;
   _XvicRect *img_rect;
   XRectangle *scr_rect;

   /* Convert region to screen-space list of rectangles for clipping */

   img_rect = _XvicRegionGetRectangles(rgn);
   n = _XvicRegionGetNumRects(rgn);

   scr_rect = _XvicMalloc(biw, n * sizeof(XRectangle));

   for (i=0; i<n; i++) {
      scr_rect[i].x = X1_Img2Scr(img_rect[i].x1);
      scr_rect[i].y = Y1_Img2Scr(img_rect[i].y1);
      scr_rect[i].width = X2_Img2Scr(img_rect[i].x2) - scr_rect[i].x + 1;
      scr_rect[i].height = Y2_Img2Scr(img_rect[i].y2) - scr_rect[i].y + 1;

      /* Constrain to fit within view because scr->img->scr conversions	*/
      /* may extend past the original view (due to zooms).		*/

      X_ViewConstrain(scr_rect[i].x, scr_rect[i].width);
      Y_ViewConstrain(scr_rect[i].y, scr_rect[i].height);
      scr_rect[i].x += x_off;
      scr_rect[i].y += y_off;
   }

   /* Set the clipping area */

   XSetClipRectangles(XtDisplay(biw), gc, 0, 0, scr_rect, n, Unsorted);

   _XvicFree(biw, scr_rect, n * sizeof(XRectangle));
}

/************************************************************************/
/************************************************************************/
/*  M I S C E L L A N E O U S   F U N C T I O N S			*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* AllocRawData								*/
/*									*/
/* Allocates space for raw data if it hasn't been already.  The "size"	*/
/* argument is the w*h*pixel_size calculated in prezoom coordinates.	*/
/* It could be calculated by this routine via (Img2Pre on raw->x,y,	*/
/* width,height), but the caller needs the value anyway (and can	*/
/* calculate it easier), so it might as well be passed in.		*/
/* Note:  unlike other routines, this does not clear & reallocate if	*/
/* the space is already allocated.  It just returns.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
AllocRawData(biw, raw, size)
   XvicBasicImageWidget biw;
   XvicImageData *raw;
   int size;
#else
AllocRawData(
   XvicBasicImageWidget biw,
   XvicImageData *raw,
   int size)
#endif /* _NO_PROTO */
{

   if (biw->bim.image_mode == XvicBW) {
      if (raw->bw_pixels == NULL)
         raw->bw_pixels = _XvicMalloc(biw, size);
   }
   else {
      if (raw->red_pixels == NULL)
         raw->red_pixels = _XvicMalloc(biw, size);

      if (raw->grn_pixels == NULL)
         raw->grn_pixels = _XvicMalloc(biw, size);

      if (raw->blu_pixels == NULL)
         raw->blu_pixels = _XvicMalloc(biw, size);
   }
}

/************************************************************************/
/* CopyCurrentToSavedResources						*/
/*									*/
/* Copies the "current" display mode resources to the appropriate	*/
/* "saved" resource set based on color/bw/pseudo mode of the display.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CopyCurrentToSavedResources(biw)
   XvicBasicImageWidget biw;
#else
CopyCurrentToSavedResources(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   if (COLOR_MODE) {
      biw->bim.color_dither = biw->bim.dither_mode;
      biw->bim.color_stretch_policy = biw->bim.stretch_policy;
      biw->bim.color_colormap_policy = biw->bim.colormap_policy;
      biw->bim.color_visual_type = biw->bim.visual_type;
   }
   else if (PSEUDO_SET) {
      biw->bim.pseudo_dither = biw->bim.dither_mode;
      biw->bim.pseudo_stretch_policy = biw->bim.stretch_policy;
      biw->bim.pseudo_colormap_policy = biw->bim.colormap_policy;
      biw->bim.pseudo_visual_type = biw->bim.visual_type;
   }
   else {		/* BW */
      biw->bim.bw_dither = biw->bim.dither_mode;
      biw->bim.bw_stretch_policy = biw->bim.stretch_policy;
      biw->bim.bw_colormap_policy = biw->bim.colormap_policy;
      biw->bim.bw_visual_type = biw->bim.visual_type;
   }
}

/************************************************************************/
/* CopyDefaultColors							*/
/*									*/
/* Copies a given number of colors from the bottom of the default	*/
/* colormap to the bottom of the given XColor array.  The HP defaults	*/
/* to 255 entries instead of 256, hence the XCellsOfScreen call (!).	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CopyDefaultColors(biw, cells, number)
   XvicBasicImageWidget biw;
   XColor *cells;
   int number;
#else
CopyDefaultColors(
   XvicBasicImageWidget biw,
   XColor *cells,
   int number)
#endif /* _NO_PROTO */
{
   XColor default_cells[CMAP_SIZE_MAX];
   int i;
   Display *dpy;

   dpy = XtDisplay(biw);

   number = MIN(number, XCellsOfScreen(XtScreen((Widget)biw)));
   for (i=0; i<number; i++)
      default_cells[i].pixel = i;
   XQueryColors(dpy, DefaultColormap(dpy, DefaultScreen(dpy)),
		default_cells, number);

   for (i=0; i<number; i++) {
      cells[i].red = default_cells[i].red;
      cells[i].green = default_cells[i].green;
      cells[i].blue = default_cells[i].blue;
   }
}

/************************************************************************/
/* CopyRawData								*/
/*									*/
/* Copies raw data from the user-supplied buffer into the internal	*/
/* Tile structure.  The XvicImageData structure is new, but the actual	*/
/* data area may be shared, depending on memory_control.  The actual	*/
/* area copied is defined by the "area" parameter, which is in Img	*/
/* coordinates, and must not go outside the tile's area.  Note that	*/
/* if an XvicImageData structure already exists for a tile, it must	*/
/* have a data area of the right size (because they're all cleared out	*/
/* if something changes).  The only issue is one of who frees the data.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CopyRawData(biw, tile, image, area, mem_okay)
   XvicBasicImageWidget biw;
   Tile *tile;
   XvicImageData *image;
   _XvicRect *area;
   Boolean *mem_okay;
#else
CopyRawData(
   XvicBasicImageWidget biw,
   Tile *tile,
   XvicImageData *image,
   _XvicRect *area,
   Boolean *mem_okay)
#endif /* _NO_PROTO */
{
   Boolean full_tile;
   int width, height, line;
   int src_offset, dest_offset;
   _XvicRect pre_area;

   full_tile = (memcmp((void *)&tile->img,(void *)area,sizeof(_XvicRect)) == 0);

   pre_area.x1 = X1_Img2Pre(area->x1);
   pre_area.x2 = X2_Img2Pre(area->x2);
   pre_area.y1 = Y1_Img2Pre(area->y1);
   pre_area.y2 = Y2_Img2Pre(area->y2);

   /* Create the tile->raw data structure if it doesn't already exist */

   if (tile->raw == NULL) {
      tile->raw = _XvicMalloc(biw, sizeof(XvicImageData));
      tile->raw->bw_pixels = NULL;
      tile->raw->red_pixels = NULL;
      tile->raw->grn_pixels = NULL;
      tile->raw->blu_pixels = NULL;
      tile->raw->x = tile->img.x1;
      tile->raw->y = tile->img.y1;
      tile->raw->width = tile->img.x2 - tile->img.x1 + 1;
      tile->raw->height = tile->img.y2 - tile->img.y1 + 1;
      tile->raw->memory_control = XvicMEMORY_APPLIC;
      tile->raw_size = 0;
   }

   if (image->memory_control == XvicMEMORY_WIDGET &&
       full_tile && *mem_okay) {

      /* Use the user's buffer if MEMORY_WIDGET, it's a full tile, and	*/
      /* it's the first full tile in this call.				*/

      FreeRawData(biw, tile->raw, tile->raw_size);
      tile->raw->memory_control = XvicMEMORY_WIDGET;
      tile->raw->line_width = image->line_width;
      tile->raw->start_offset = image->start_offset +
		(pre_area.y1 - Y1_Img2Pre(image->y)) * image->line_width +
		(pre_area.x1 - X1_Img2Pre(image->x)) * biw->bim.pixel_size;
      tile->raw_size = (Y2_Img2Pre(image->y + image->height - 1) -
			Y1_Img2Pre(image->y) + 1) * image->line_width;
      *mem_okay = FALSE;		/* Can only take over one tile */
      if (biw->bim.image_mode == XvicBW) {
         tile->raw->bw_pixels = image->bw_pixels;
         tile->raw->red_pixels = NULL;
         tile->raw->grn_pixels = NULL;
         tile->raw->blu_pixels = NULL;
         _XvicMemoryGrab(biw, tile->raw_size);
      }
      else {
         tile->raw->bw_pixels = NULL;
         tile->raw->red_pixels = image->red_pixels;
         tile->raw->grn_pixels = image->grn_pixels;
         tile->raw->blu_pixels = image->blu_pixels;
         _XvicMemoryGrab(biw, tile->raw_size * 3);
      }
   }
   else if (image->memory_control == XvicMEMORY_SHARED && full_tile) {

      /* Use the user's buffer if MEMORY_SHARED, and it's a full tile.	*/

      FreeRawData(biw, tile->raw, tile->raw_size);
      tile->raw->memory_control = XvicMEMORY_SHARED;
      tile->raw->line_width = image->line_width;
      tile->raw->start_offset = image->start_offset +
		(pre_area.y1 - Y1_Img2Pre(image->y)) * image->line_width +
		(pre_area.x1 - X1_Img2Pre(image->x)) * biw->bim.pixel_size;
      tile->raw_size = 0;
      if (biw->bim.image_mode == XvicBW) {
         tile->raw->bw_pixels = image->bw_pixels;
         tile->raw->red_pixels = NULL;
         tile->raw->grn_pixels = NULL;
         tile->raw->blu_pixels = NULL;
      }
      else {
         tile->raw->bw_pixels = NULL;
         tile->raw->red_pixels = image->red_pixels;
         tile->raw->grn_pixels = image->grn_pixels;
         tile->raw->blu_pixels = image->blu_pixels;
      }
   }
   else {

      /* If the user's buffer is MEMORY_APPLIC, or if it's a partial	*/
      /* tile, or if it's MEMORY_WIDGET but we've already saved one	*/
      /* tile from the buffer, then we need to copy the data to our	*/
      /* own buffer (after maybe allocating it first).			*/

      if (tile->raw->memory_control==XvicMEMORY_SHARED) /* can't write to it */
         FreeRawData(biw, tile->raw, tile->raw_size);

      tile->raw->memory_control = XvicMEMORY_APPLIC;
      tile->raw->line_width = (tile->pre.x2 - tile->pre.x1 + 1) *
							biw->bim.pixel_size;
      tile->raw->start_offset = 0;
      tile->raw_size = (tile->pre.y2-tile->pre.y1+1) * tile->raw->line_width;
      AllocRawData(biw, tile->raw, tile->raw_size); /* NOP if already alloced */

      width = (pre_area.x2 - pre_area.x1 + 1);	/* w,h of area to copy */
      height = (pre_area.y2 - pre_area.y1 + 1);
      src_offset = image->start_offset +
		(pre_area.y1 - Y1_Img2Pre(image->y)) * image->line_width +
		(pre_area.x1 - X1_Img2Pre(image->x)) * biw->bim.pixel_size;
      dest_offset = (pre_area.y1 - tile->pre.y1) * tile->raw->line_width +
		    (pre_area.x1 - tile->pre.x1) * biw->bim.pixel_size;
      if (biw->bim.image_mode == XvicBW) {
         for (line = 0; line < height; line++) {
            memcpy((void *)(tile->raw->bw_pixels + dest_offset),
		   (void *)(image->bw_pixels + src_offset),
		   width * biw->bim.pixel_size);
            dest_offset += tile->raw->line_width;
            src_offset += image->line_width;
         }
      }
      else {					/* Color */
         for (line = 0; line < height; line++) {
            memcpy((void *)(tile->raw->red_pixels + dest_offset),
		   (void *)(image->red_pixels + src_offset),
		   width * biw->bim.pixel_size);
            memcpy((void *)(tile->raw->grn_pixels + dest_offset),
		   (void *)(image->grn_pixels + src_offset),
		   width * biw->bim.pixel_size);
            memcpy((void *)(tile->raw->blu_pixels + dest_offset),
		   (void *)(image->blu_pixels + src_offset),
		   width * biw->bim.pixel_size);
            dest_offset += tile->raw->line_width;
            src_offset += image->line_width;
         }
      }
   }
}

/************************************************************************/
/* CopyXimagePixmap							*/
/*									*/
/* Copies an XImage into a Pixmap for the given area.  The area		*/
/* parameter is in Img coordinates.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CopyXimagePixmap(biw, tile, ximage, pixmap, area)
   XvicBasicImageWidget biw;
   Tile *tile;
   XImage *ximage;
   Pixmap pixmap;
   _XvicRect *area;
#else
CopyXimagePixmap(
   XvicBasicImageWidget biw,
   Tile *tile,
   XImage *ximage,
   Pixmap pixmap,
   _XvicRect *area)
#endif /* _NO_PROTO */
{
   int x_offset, y_offset;

   _XvicRect dpy_area;

   dpy_area.x1 = X1_Img2Dpy(area->x1);
   dpy_area.x2 = X2_Img2Dpy(area->x2);
   dpy_area.y1 = Y1_Img2Dpy(area->y1);
   dpy_area.y2 = Y2_Img2Dpy(area->y2);

   x_offset = dpy_area.x1 - tile->dpy.x1;
   y_offset = dpy_area.y1 - tile->dpy.y1;

   XSetClipMask(XtDisplay(biw), biw->bim.img_gc, None);

   XPutImage(XtDisplay(biw), pixmap, biw->bim.img_gc, ximage,
		x_offset, y_offset, x_offset, y_offset,
		dpy_area.x2 - dpy_area.x1 + 1, dpy_area.y2 - dpy_area.y1 + 1);
}

/************************************************************************/
/* CreateTiles								*/
/*									*/
/* Creates all the tiles for the image, deleting old ones if necessary.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CreateTiles(biw)
   XvicBasicImageWidget biw;
#else
CreateTiles(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i;

   if (biw->bim.tiles)
      DestroyTiles(biw);

   biw->bim.num_tiles_x = (biw->bim.image_width + biw->bim.tile_width - 1) /
				biw->bim.tile_width;
   biw->bim.num_tiles_y = (biw->bim.image_height + biw->bim.tile_height -1) /
				biw->bim.tile_height;
   biw->bim.num_tiles = biw->bim.num_tiles_x * biw->bim.num_tiles_y;

   biw->bim.tiles = (Tile *)_XvicMalloc(biw, biw->bim.num_tiles * sizeof(Tile));

   for (i=0; i<biw->bim.num_tiles; i++) {
      biw->bim.tiles[i].raw = NULL;
      biw->bim.tiles[i].raw_size = 0;
      biw->bim.tiles[i].ximage = NULL;
      biw->bim.tiles[i].ximage_size = 0;
      biw->bim.tiles[i].pixmap = None;
      biw->bim.tiles[i].pixmap_size = 0;
   }

   SetTileCoords(biw);
   SetTileDpyCoords(biw);

}

/************************************************************************/
/* DestroyTiles								*/
/*									*/
/* Frees data from all tiles, then frees the tile structures themselves	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DestroyTiles(biw)
   XvicBasicImageWidget biw;
#else
DestroyTiles(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   if (biw->bim.tiles) {
      FreeAllTileData(biw);
      _XvicFree(biw, biw->bim.tiles, biw->bim.num_tiles * sizeof(Tile));
      biw->bim.tiles = NULL;
   }
   biw->bim.num_tiles = 0;
   biw->bim.num_tiles_x = 0;
   biw->bim.num_tiles_y = 0;

}

/************************************************************************/
/* DoPan								*/
/*									*/
/* Actually performs a pan of the image.  It first copies the still-	*/
/* visible data using XCopyArea, then it generates synthetic expose	*/
/* events to fill in the new area.  Two events are potentially		*/
/* generated, one for a Y pan (which extends the entire width), and one	*/
/* for an X pan (which extends over the height not covered by the Y pan	*/
/* event).  Redisplay must be called directly so it can use the proper	*/
/* pan values to re-create the image coordinates.  There are timing	*/
/* problems with using normal X expose events (i.e. XSendEvent) - the	*/
/* pan may change before the event is delivered, which causes an area	*/
/* to not be updated.							*/
/*									*/
/* Everything below is in Screen coordinates.  The *_dpy_off offset	*/
/* (for shadow & highlight) is not applied until the very end,		*/
/* in the XCopyArea call itself or just before the event is sent.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DoPan(biw)
   XvicBasicImageWidget biw;
#else
DoPan(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   XExposeEvent xevent, yevent;
   int delta;				/* amount we moved */
   int x_src, y_src;			/* Source coords for copy */
   int x_dest, y_dest;			/* Dest coords for copy */
   int copy_width, copy_height;		/* Size of area to copy */
   Boolean expose_all = FALSE;
   Boolean top;

   if (!XtIsRealized((Widget) biw)) {
      biw->bim.x_old_screen_pan = biw->bim.x_screen_pan;
      biw->bim.y_old_screen_pan = biw->bim.y_screen_pan;
      return;				/* Not realized, nothing to do */
   }

   if (biw->bim.x_screen_pan == biw->bim.x_old_screen_pan &&
       biw->bim.y_screen_pan == biw->bim.y_old_screen_pan)
      return;				/* No change, nothing to do */

   xevent.type = Expose;
   yevent.type = Expose;

   /* Move data up, expose bottom */

   if (biw->bim.y_screen_pan >= biw->bim.y_old_screen_pan) {
      delta = biw->bim.y_screen_pan - biw->bim.y_old_screen_pan;

      y_src = delta;
      y_dest = 0;
      copy_height = biw->bim.view_height - delta;

      if (delta >= (int)biw->bim.view_height)
         expose_all = TRUE;
      else {
         yevent.x = 0;
         yevent.width = biw->bim.view_width;
         yevent.y = biw->bim.view_height - delta;
         yevent.height = delta;

         xevent.y = 0;		/* Set X expose height to avoid double expose */
         xevent.height = biw->bim.view_height - delta;
      }
      top = FALSE;
   }

   /* Move data down, expose top */

   else {
      delta = biw->bim.y_old_screen_pan - biw->bim.y_screen_pan;

      y_src = 0;
      y_dest = delta;
      copy_height = biw->bim.view_height - delta;

      if (delta >= (int)biw->bim.view_height)
         expose_all = TRUE;
      else {
         yevent.x = 0;
         yevent.width = biw->bim.view_width;
         yevent.y = 0;
         yevent.height = delta;

         xevent.y = delta;	/* Set X expose height to avoid double expose */
         xevent.height = biw->bim.view_height - delta;
      }
      top = TRUE;
   }

   /* Move data left, expose right */

   if (biw->bim.x_screen_pan >= biw->bim.x_old_screen_pan) {
      delta = biw->bim.x_screen_pan - biw->bim.x_old_screen_pan;

      x_src = delta;
      x_dest = 0;
      copy_width = biw->bim.view_width - delta;

      if (delta >= (int)biw->bim.view_width)
         expose_all = TRUE;
      else {
         xevent.x = biw->bim.view_width - delta;
         xevent.width = delta;
      }
   }

   /* Move data right, expose left */

   else {
      delta = biw->bim.x_old_screen_pan - biw->bim.x_screen_pan;

      x_src = 0;
      x_dest = delta;
      copy_width = biw->bim.view_width - delta;

      if (delta >= (int)biw->bim.view_width)
         expose_all = TRUE;
      else {
         xevent.x = 0;
         xevent.width = delta;
      }
   }

   if (expose_all) {			/* Repaint whole screen */
      yevent.x = 0;
      yevent.y = 0;
      yevent.width = biw->bim.view_width;
      yevent.height = biw->bim.view_height;

      XClearArea(XtDisplay(biw), XtWindow(biw),
		biw->bim.x_dpy_off, biw->bim.y_dpy_off,
		biw->bim.view_width, biw->bim.view_height,
		FALSE);
      CALL_ClearOverlay(biw, ((Widget)biw, 0, 0,
			biw->bim.view_width, biw->bim.view_height));

      _XvicRedisplay_Internal(biw, &yevent, NULL);	/* Full expose */
   }
   else {			/* Normal (partial-screen) pan */

      /* Do the pan */

      XCopyArea(XtDisplay(biw), XtWindow(biw), XtWindow(biw), biw->bim.pan_gc,
		x_src + biw->bim.x_dpy_off, y_src + biw->bim.y_dpy_off,
		copy_width, copy_height,
		x_dest + biw->bim.x_dpy_off, y_dest + biw->bim.y_dpy_off);
      CALL_MoveOverlay(biw, ((Widget)biw, x_src, y_src, copy_width, copy_height,
			x_dest, y_dest));

      /* Clear out the area to be exposed so it looks better */

      if (yevent.height != 0) {
         XClearArea(XtDisplay(biw), XtWindow(biw),
		yevent.x + biw->bim.x_dpy_off, yevent.y + biw->bim.y_dpy_off,
		yevent.width, yevent.height, FALSE);
         CALL_ClearOverlay(biw, ((Widget)biw, yevent.x, yevent.y,
			yevent.width, yevent.height));
      }
      if (xevent.width != 0) {
         XClearArea(XtDisplay(biw), XtWindow(biw),
		xevent.x + biw->bim.x_dpy_off, xevent.y + biw->bim.y_dpy_off,
		xevent.width, xevent.height, FALSE);
         CALL_ClearOverlay(biw, ((Widget)biw, xevent.x, xevent.y,
			xevent.width, xevent.height));
      }

      /* If the y expose is on top, do it first because it looks better	*/
      /* to paint top-down.  If the y expose is on bottom, do it last.	*/

      if (yevent.height != 0 && top)		/* Y-direction expose */
         _XvicRedisplay_Internal(biw, &yevent, NULL);
      if (xevent.width != 0)			/* X-direction expose */
         _XvicRedisplay_Internal(biw, &xevent, NULL);
      if (yevent.height != 0 && !top)		/* Y-direction expose */
         _XvicRedisplay_Internal(biw, &yevent, NULL);

      /* Keep track of the number of outstanding GraphicsExpose events	*/
      /* we should get, and the min/max pans while some are left.	*/
      /* This is to prevent holes in the image if a new pan happens	*/
      /* before all the previous GraphicsExpose events have been fully	*/
      /* processed.  Any GraphicsExposes are stretched to cover the	*/
      /* min/max size of all pans.  Any normal Exposes are stretched	*/
      /* additionally by the amount of this pan.  Once there are no	*/
      /* more outstanding GraphicsExposes, the pan min/max is reset.	*/

      biw->bim.pan_count++;

      if (yevent.height != 0)
         biw->bim.pan_dir |= PAN_VERT;
      if (xevent.width != 0)
         biw->bim.pan_dir |= PAN_HORIZ;

      if (biw->bim.pan_count == 1) {		/* was == 0, i.e. first pan */
         biw->bim.x_min_pan = biw->bim.x_screen_pan;
         biw->bim.x_max_pan = biw->bim.x_screen_pan;
         biw->bim.y_min_pan = biw->bim.y_screen_pan;
         biw->bim.y_max_pan = biw->bim.y_screen_pan;
         biw->bim.x_min_exp_pan =
		MIN(biw->bim.x_screen_pan, biw->bim.x_old_screen_pan);
         biw->bim.x_max_exp_pan =
		MAX(biw->bim.x_screen_pan, biw->bim.x_old_screen_pan);
         biw->bim.y_min_exp_pan =
		MIN(biw->bim.y_screen_pan, biw->bim.y_old_screen_pan);
         biw->bim.y_max_exp_pan =
		MAX(biw->bim.y_screen_pan, biw->bim.y_old_screen_pan);
      }
      else {
         biw->bim.x_min_pan = MIN(biw->bim.x_min_pan, biw->bim.x_screen_pan);
         biw->bim.x_max_pan = MAX(biw->bim.x_max_pan, biw->bim.x_screen_pan);
         biw->bim.y_min_pan = MIN(biw->bim.y_min_pan, biw->bim.y_screen_pan);
         biw->bim.y_max_pan = MAX(biw->bim.y_max_pan, biw->bim.y_screen_pan);
         biw->bim.x_min_exp_pan =
		MIN(biw->bim.x_min_exp_pan, biw->bim.x_screen_pan);
         biw->bim.x_max_exp_pan =
		MAX(biw->bim.x_max_exp_pan, biw->bim.x_screen_pan);
         biw->bim.y_min_exp_pan =
		MIN(biw->bim.y_min_exp_pan, biw->bim.y_screen_pan);
         biw->bim.y_max_exp_pan =
		MAX(biw->bim.y_max_exp_pan, biw->bim.y_screen_pan);
      }
   }
   biw->bim.x_old_screen_pan = biw->bim.x_screen_pan;
   biw->bim.y_old_screen_pan = biw->bim.y_screen_pan;
}

/************************************************************************/
/* DrawTilePixmap							*/
/*									*/
/* Draws a tile using a pixmap.  Removes the area covered by the tile	*/
/* from the expose damage list.  If draw_all is TRUE, the entire tile	*/
/* is drawn, otherwise, only areas corresponding to the expose damage	*/
/* list are drawn.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DrawTilePixmap(biw, tile, pixmap, draw_all)
   XvicBasicImageWidget biw;
   Tile *tile;
   Pixmap pixmap;
   Boolean draw_all;
#else
DrawTilePixmap(
   XvicBasicImageWidget biw,
   Tile *tile,
   Pixmap pixmap,
#if NeedWidePrototypes
   int draw_all)
#else
   Boolean draw_all)
#endif
#endif /* _NO_PROTO */

{
   _XvicRect visible_rect, draw_bounds;
   _XvicRegion *intersect_rgn;
   Boolean status;

   /* Get region to draw into */

   if (draw_all) {
      _XvicGetVisibleRect(biw, &visible_rect);
      do {
         intersect_rgn = _XvicRegionCreateRect(&visible_rect);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
      _XvicRegionIntersect(&tile->img, intersect_rgn);
   }
   else {
      do {
         intersect_rgn = _XvicRegionCreateIntersect(
				&tile->img, biw->bim.expose_rgn);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
   }

   _XvicSetClipFromRgn(biw, intersect_rgn, biw->bim.img_gc,
		biw->bim.x_dpy_off, biw->bim.y_dpy_off);

   /* Now draw the pixmap */

   status = _XvicRegionBounds(intersect_rgn, &draw_bounds);
   if (status) {		/* skip display if region is empty */
      draw_bounds.x1 = X1_Img2Dpy(draw_bounds.x1);
      draw_bounds.y1 = Y1_Img2Dpy(draw_bounds.y1);
      draw_bounds.x2 = X2_Img2Dpy(draw_bounds.x2);
      draw_bounds.y2 = Y2_Img2Dpy(draw_bounds.y2);

      XCopyArea(XtDisplay(biw), pixmap, XtWindow(biw), biw->bim.img_gc,
	draw_bounds.x1 - tile->dpy.x1,
	draw_bounds.y1 - tile->dpy.y1,
	draw_bounds.x2 - draw_bounds.x1 + 1,
	draw_bounds.y2 - draw_bounds.y1 + 1,
	X_Dpy2Scr(draw_bounds.x1) + biw->bim.x_dpy_off,
	Y_Dpy2Scr(draw_bounds.y1) + biw->bim.y_dpy_off);

      CALL_ExposeOverlay(biw, ((Widget)biw, intersect_rgn, &draw_bounds));
   }

   do {
      status = _XvicRegionSubtract(&tile->img, biw->bim.expose_rgn);
      if (!status) _XvicMemoryPanic(biw);
   } while (!status);

   _XvicRegionDestroy(intersect_rgn);

}

/************************************************************************/
/* DrawTileRaw								*/
/*									*/
/* Draws a tile using raw data.  Removes the area covered by the tile	*/
/* from the expose damage list.  If draw_all is TRUE, the entire tile	*/
/* is drawn, otherwise, only areas corresponding to the expose damage	*/
/* list are drawn.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DrawTileRaw(biw, tile, raw, draw_all)
   XvicBasicImageWidget biw;
   Tile *tile;
   XvicImageData *raw;
   Boolean draw_all;
#else
DrawTileRaw(
   XvicBasicImageWidget biw,
   Tile *tile,
   XvicImageData *raw,
#if NeedWidePrototypes
   int draw_all)
#else
   Boolean draw_all)
#endif
#endif /* _NO_PROTO */

{
   _XvicRect visible_rect, draw_bounds;
   _XvicRegion *intersect_rgn;
   int width, height;
   Boolean status;

   /* Get region to draw into */

   if (draw_all) {
      _XvicGetVisibleRect(biw, &visible_rect);
      do {
         intersect_rgn = _XvicRegionCreateRect(&visible_rect);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
      _XvicRegionIntersect(&tile->img, intersect_rgn);
   }
   else {
      do {
         intersect_rgn = _XvicRegionCreateIntersect(
				&tile->img, biw->bim.expose_rgn);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
   }

   status = _XvicRegionBounds(intersect_rgn, &draw_bounds);
   _XvicRegionDestroy(intersect_rgn); /* DrawTileXimage removes from dmg list */

   if (status) {			/* skip display if region is empty */
      width = tile->dpy.x2 - tile->dpy.x1 + 1;
      height = tile->dpy.y2 - tile->dpy.y1 + 1;

      biw->bim.protect_tmp_ximage = TRUE;
      _XvicGetXImage(biw, &biw->bim.tmp_ximage, width, height);
      biw->bim.protect_tmp_ximage = FALSE;

      _XvicCopyRawXimage(biw, tile, raw, biw->bim.tmp_ximage, &draw_bounds);

      DrawTileXimage(biw, tile, biw->bim.tmp_ximage, draw_all);
   }
}

/************************************************************************/
/* DrawTileXimage							*/
/*									*/
/* Draws a tile using an XImage.  Removes the area covered by the tile	*/
/* from the expose damage list.  If draw_all is TRUE, the entire tile	*/
/* is drawn, otherwise, only areas corresponding to the expose damage	*/
/* list are drawn.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DrawTileXimage(biw, tile, ximage, draw_all)
   XvicBasicImageWidget biw;
   Tile *tile;
   XImage *ximage;
   Boolean draw_all;
#else
DrawTileXimage(
   XvicBasicImageWidget biw,
   Tile *tile,
   XImage *ximage,
#if NeedWidePrototypes
   int draw_all)
#else
   Boolean draw_all)
#endif
#endif /* _NO_PROTO */

{
   _XvicRect visible_rect, draw_bounds;
   _XvicRegion *intersect_rgn;
   Boolean status;

   /* Get region to draw into */

   if (draw_all) {
      _XvicGetVisibleRect(biw, &visible_rect);
      do {
         intersect_rgn = _XvicRegionCreateRect(&visible_rect);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
      _XvicRegionIntersect(&tile->img, intersect_rgn);
   }
   else {
      do {
         intersect_rgn = _XvicRegionCreateIntersect(
				&tile->img, biw->bim.expose_rgn);
         if (!intersect_rgn) _XvicMemoryPanic(biw);
      } while (intersect_rgn == NULL);
   }

   _XvicSetClipFromRgn(biw, intersect_rgn, biw->bim.img_gc,
		biw->bim.x_dpy_off, biw->bim.y_dpy_off);

   /* Now draw the pixmap */

   status = _XvicRegionBounds(intersect_rgn, &draw_bounds);
   if (status) {			/* skip display if region is empty */
      draw_bounds.x1 = X1_Img2Dpy(draw_bounds.x1);
      draw_bounds.y1 = Y1_Img2Dpy(draw_bounds.y1);
      draw_bounds.x2 = X2_Img2Dpy(draw_bounds.x2);
      draw_bounds.y2 = Y2_Img2Dpy(draw_bounds.y2);

      XPutImage(XtDisplay(biw), XtWindow(biw), biw->bim.img_gc, ximage,
	draw_bounds.x1 - tile->dpy.x1,
	draw_bounds.y1 - tile->dpy.y1,
	X_Dpy2Scr(draw_bounds.x1) + biw->bim.x_dpy_off,
	Y_Dpy2Scr(draw_bounds.y1) + biw->bim.y_dpy_off,
	draw_bounds.x2 - draw_bounds.x1 + 1,
	draw_bounds.y2 - draw_bounds.y1 + 1);

      CALL_ExposeOverlay(biw, ((Widget)biw, intersect_rgn, &draw_bounds));
   }

   do {
      status = _XvicRegionSubtract(&tile->img, biw->bim.expose_rgn);
      if (!status) _XvicMemoryPanic(biw);
   } while (!status);

   _XvicRegionDestroy(intersect_rgn);

}

/************************************************************************/
/* ExposeTile								*/
/*									*/
/* Exposes the given tile, by either drawing it if it is in memory, or	*/
/* by generating an application callback.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ExposeTile(biw, tile)
   XvicBasicImageWidget biw;
   Tile *tile;
#else
ExposeTile(
   XvicBasicImageWidget biw,
   Tile *tile)
#endif /* _NO_PROTO */
{
   XvicImageCallbackStruct cb;

   if (tile->pixmap)
      DrawTilePixmap(biw, tile, tile->pixmap, FALSE);
   else if (tile->ximage)
      DrawTileXimage(biw, tile, tile->ximage, FALSE);
   else if (tile->raw)
      DrawTileRaw(biw, tile, tile->raw, FALSE);
   else {

      /* Application callback: XvicImageWrite() will do the Draw's. */

      cb.reason = XvicCR_EXPOSE;
      cb.event = NULL;
      cb.x = tile->img.x1;
      cb.y = tile->img.y1;
      cb.width = tile->img.x2 - tile->img.x1 + 1;
      cb.height = tile->img.y2 - tile->img.y1 + 1;
      cb.prezoom_x = tile->pre.x1;
      cb.prezoom_y = tile->pre.y1;
      cb.prezoom_width = tile->pre.x2 - tile->pre.x1 + 1;
      cb.prezoom_height = tile->pre.y2 - tile->pre.y1 + 1;

      if (biw->bim.expose_callback)
         XtCallCallbackList((Widget)biw, biw->bim.expose_callback, &cb);
   }
}

/************************************************************************/
/* FatalError								*/
/*									*/
/* Report a fatal error.  "name" is the identifier for the error	*/
/* message, while "def" is the default string to use if the error	*/
/* database is not available (which is almost always the case).		*/
/* See XtAppErrorMsg().							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FatalError(biw, name, def)
   XvicBasicImageWidget biw;
   char *name;
   char *def;
#else
FatalError(
   XvicBasicImageWidget biw,
   char *name,
   char *def)
#endif /* _NO_PROTO */
{
   XtAppErrorMsg(XtWidgetToApplicationContext((Widget)biw),
	name, "XvicBasicImage", "XvicBasicImageWidgetError", def,
	(String *)NULL, (Cardinal *)NULL);
   exit(1);	/* XtAppErrorMsg should never return, but just in case... */
}

/************************************************************************/
/* FreeAllocedColors							*/
/*									*/
/* Frees any colors that have been allocated in the system colormap.	*/
/* Should only be called for colormapPolicy of ALLOC.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeAllocedColors(biw)
   XvicBasicImageWidget biw;
#else
FreeAllocedColors(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Display *dpy;

   dpy = XtDisplay(biw);

   if (biw->bim.gray_alloced) {
      if (biw->bim.colormap)
         XFreeColors(dpy, biw->bim.colormap, biw->bim.cmap_gray,
		biw->bim.gray_alloced, 0);
      biw->bim.gray_alloced = 0;
   }

   if (biw->bim.green_alloced) {
      if (biw->bim.colormap)
         XFreeColors(dpy, biw->bim.colormap, biw->bim.cmap_green,
		biw->bim.green_alloced, 0);
      biw->bim.green_alloced = 0;
   }

   if (biw->bim.rb_alloced) {
      if (biw->bim.colormap)
         XFreeColors(dpy, biw->bim.colormap, biw->bim.cmap_rb,
		biw->bim.rb_alloced, 0);
      biw->bim.red_alloced = 0;
      biw->bim.blue_alloced = 0;
      biw->bim.rb_alloced = 0;
   }
   biw->bim.alloc_private = FALSE;
}

/************************************************************************/
/* FreeAllTileData							*/
/*									*/
/* Frees up (invalidates) data from all tiles.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeAllTileData(biw)
   XvicBasicImageWidget biw;
#else
FreeAllTileData(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i;

   FreeXTileData(biw);

   for (i=0; i<biw->bim.num_tiles; i++) {
      if (biw->bim.tiles[i].raw) {
         FreeRawData(biw, biw->bim.tiles[i].raw, biw->bim.tiles[i].raw_size);
         _XvicFree(biw, biw->bim.tiles[i].raw, sizeof(XvicImageData));
         biw->bim.tiles[i].raw = NULL;
      }
   }
}

/************************************************************************/
/* FreeLRUTile								*/
/*									*/
/* Frees up the buffer for the least-recently-used tile.  Returns TRUE	*/
/* if anything was freed, FALSE otherwise.				*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
FreeLRUTile(biw)
   XvicBasicImageWidget biw;
#else
FreeLRUTile(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i;
   int lru_tile, lru_value;

   lru_value = biw->bim.lru;	/* the max it can be */
   lru_tile = -1;

   if (biw->bim.tiles) {		/* Don't do anything if no tiles! */
      for (i=0; i<biw->bim.num_tiles; i++) {
         if (biw->bim.tiles[i].lru && biw->bim.tiles[i].lru < lru_value) {
            lru_tile = i;		/* Found an earlier one */
            lru_value = biw->bim.tiles[i].lru;
         }
      }

      /* If we've found the LRU tile (and it's not the current one), free it */

      if (lru_tile != -1 && lru_tile != biw->bim.current_tile) {
         i = lru_tile;
         if (biw->bim.tiles[i].raw) {
            FreeRawData(biw, biw->bim.tiles[i].raw, biw->bim.tiles[i].raw_size);
            _XvicFree(biw, biw->bim.tiles[i].raw, sizeof(XvicImageData));
            biw->bim.tiles[i].raw = NULL;
         }
         if (biw->bim.tiles[i].ximage) {
            _XvicMemoryReturn(biw, biw->bim.tiles[i].ximage_size);
            XDestroyImage(biw->bim.tiles[i].ximage);
            biw->bim.tiles[i].ximage = NULL;
         }
         if (biw->bim.tiles[i].pixmap) {
            _XvicMemoryReturn(biw, biw->bim.tiles[i].pixmap_size);
            XFreePixmap(XtDisplay(biw), biw->bim.tiles[i].pixmap);
            biw->bim.tiles[i].pixmap = None;
         }

         /* Mark the tile as having been recently accessed.  We don't	*/
         /* want to use TOUCH_TILE here because we don't want to change	*/
         /* the setting for current_tile.				*/

         biw->bim.tiles[i].lru = biw->bim.lru++;

         return TRUE;
      }
   }

   return FALSE;
}

/************************************************************************/
/* FreeRawData								*/
/*									*/
/* Frees up the data pointed at by the image structure, if the widget	*/
/* is managing it.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeRawData(biw, raw, raw_size)
   XvicBasicImageWidget biw;
   XvicImageData *raw;
   int raw_size;
#else
FreeRawData(
   XvicBasicImageWidget biw,
   XvicImageData *raw,
   int raw_size)
#endif /* _NO_PROTO */
{

   if (raw->memory_control == XvicMEMORY_APPLIC ||	/* alloced by us */
       raw->memory_control == XvicMEMORY_WIDGET) {	/* alloced for us */
      if (raw->bw_pixels)
         _XvicFree(biw, raw->bw_pixels, raw_size);
      raw->bw_pixels = NULL;
      if (raw->red_pixels)
         _XvicFree(biw, raw->red_pixels, raw_size);
      raw->red_pixels = NULL;
      if (raw->grn_pixels)
         _XvicFree(biw, raw->grn_pixels, raw_size);
      raw->grn_pixels = NULL;
      if (raw->blu_pixels)
         _XvicFree(biw, raw->blu_pixels, raw_size);
      raw->blu_pixels = NULL;
   }
}

/************************************************************************/
/* FreeTmpXimageIfTooBig						*/
/*									*/
/* Frees up biw->bim.tmp_ximage if it is bigger than it needs to be	*/
/* (which is tile height x width).  It will be reallocated the correct	*/
/* size next time it is needed.  tmp_ximage can grow significantly	*/
/* beyond its necessary size due to large zoom factors.  This function	*/
/* is called by the memory management routines to keep it under control	*/
/* when memory is tight.  If protect_tmp_ximage is true, then we are	*/
/* currently trying to allocate tmp_ximage itself, so don't do anything	*/
/* to pull the rug out from under ourselves!				*/
/*									*/
/* The function returns TRUE if it was able to free something, FALSE	*/
/* otherwise.								*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
FreeTmpXimageIfTooBig(biw)
   XvicBasicImageWidget biw;
#else
FreeTmpXimageIfTooBig(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   if (biw->bim.protect_tmp_ximage)
      return FALSE;				/* nothing to do */

   if (biw->bim.tmp_ximage == NULL)
      return FALSE;				/* also nothing to do */

   if (biw->bim.tmp_ximage->width <= ZOOMX(biw->bim.tile_width) &&
       biw->bim.tmp_ximage->height <= ZOOMY(biw->bim.tile_height))
      return FALSE;				/* tmp_ximage is right size */

   DPR(("freeing tmp ximage, size=%d\n", biw->bim.tmp_ximage->height * biw->bim.tmp_ximage->bytes_per_line));
   _XvicMemoryReturn(biw,
	biw->bim.tmp_ximage->height * biw->bim.tmp_ximage->bytes_per_line);
   XDestroyImage(biw->bim.tmp_ximage);
   biw->bim.tmp_ximage = NULL;

   return TRUE;
}

/************************************************************************/
/* FreeXTileData							*/
/*									*/
/* Frees up (invalidates) data from tiles that are saving Ximages or	*/
/* Pixmaps.  _XvicMemoryReturn must be called manually since _XvicFree()*/
/* is not used.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeXTileData(biw)
   XvicBasicImageWidget biw;
#else
FreeXTileData(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i;

   for (i=0; i<biw->bim.num_tiles; i++) {
      if (biw->bim.tiles[i].ximage) {
         _XvicMemoryReturn(biw, biw->bim.tiles[i].ximage_size);
         XDestroyImage(biw->bim.tiles[i].ximage);
         biw->bim.tiles[i].ximage = NULL;
      }
      if (biw->bim.tiles[i].pixmap) {
         _XvicMemoryReturn(biw, biw->bim.tiles[i].pixmap_size);
         XFreePixmap(XtDisplay(biw), biw->bim.tiles[i].pixmap);
         biw->bim.tiles[i].pixmap = None;
      }
   }
}

/************************************************************************/
/* GetColormap								*/
/*									*/
/* Creates a colormap, or gets a pointer to the default colormap.	*/
/* Any old colormaps are destroyed on the theory that this should only	*/
/* be called if the visual type changes, or if colormapPolicy goes	*/
/* to or from XvicALLOC.  The widget does not have to be realized for	*/
/* this function.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetColormap(biw)
   XvicBasicImageWidget biw;
#else
GetColormap(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Display *dpy;
   int screen;
   int alloc;

   dpy = XtDisplay(biw);
   screen = DefaultScreen(dpy);

   /* In ALLOC mode, use default colormap if the visuals match (if not,   */
   /* punt by creating a private one anyway so at least it doesn't crash) */

   if ((biw->bim.colormap_policy == XvicALLOC) &&
       (biw->bim.vis.visual == DefaultVisual(dpy, screen))) {

      FreeAllocedColors(biw);
      if (biw->bim.colormap && biw->bim.private_cmap)
         XFreeColormap(dpy, biw->bim.colormap);
      biw->bim.colormap = DefaultColormap(dpy, screen);
      biw->bim.private_cmap = FALSE;
      biw->bim.red_alloced = 0;
      biw->bim.green_alloced = 0;
      biw->bim.blue_alloced = 0;
      biw->bim.gray_alloced = 0;
      biw->bim.rb_alloced = 0;
      biw->bim.alloc_private = FALSE;
      biw->bim.cmap_alloc_mode = AllocNone;
   }
   else {

      if ((biw->bim.colormap_policy == XvicALLOC) &&
          (biw->bim.vis.visual != DefaultVisual(dpy, screen))) {
         WarningMsg(biw, "NoMatchVisual",
		"XvicNcolormapPolicy is Alloc but visual doesn't match root window.\nColormap may be wrong.");
      }

      /* Use a private colormap */

      if (biw->bim.vis.class == DirectColor ||
          biw->bim.vis.class == PseudoColor ||
          biw->bim.vis.class == GrayScale)
         alloc = AllocAll;
      else			/* TrueColor, StaticColor, StaticGray */
         alloc = AllocNone;

      FreeAllocedColors(biw);
      if (biw->bim.colormap && biw->bim.private_cmap)
         XFreeColormap(dpy, biw->bim.colormap);

      biw->bim.colormap = XCreateColormap(dpy, RootWindow(dpy, screen),
			biw->bim.vis.visual, alloc);
      biw->bim.private_cmap = TRUE;
      biw->bim.red_alloced = 0;
      biw->bim.green_alloced = 0;
      biw->bim.blue_alloced = 0;
      biw->bim.gray_alloced = 0;
      biw->bim.rb_alloced = 0;
      biw->bim.alloc_private = FALSE;
      biw->bim.cmap_alloc_mode = alloc;
   }

   /* Originally we called XtVaSetValues here to set core's XtNcolormap	*/
   /* resource (only if the widget was realized since it's called from	*/
   /* Initialize()!).  However, that got into a recursive call to	*/
   /* SetValues (since this is called from SetValues), which is bad	*/
   /* news for a number of reasons.  Since all that Core (or Primitive	*/
   /* for that matter) does with the colormap is to call		*/
   /* XChangeWindowAttributes, which is called anyway in		*/
   /* InstallColormap (which is always called after this routine,	*/
   /* eventually), there's no need for the SetValues call, so we just	*/
   /* directly set core.colormap.					*/

   biw->core.colormap = biw->bim.colormap;
}

/************************************************************************/
/* GetVisual								*/
/*									*/
/* Determines what visual type to use based on the current resources.	*/
/* Returns TRUE if the visual type has changed.  This routine will	*/
/* change colormapPolicy to or away from FULL_COLOR if needed.		*/
/* This could depend on colormapPolicy, e.g. 3/3/2 DirectColor		*/
/* instead of PseudoColor.						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
GetVisual(biw)
   XvicBasicImageWidget biw;
#else
GetVisual(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Display *dpy;
   int screen;
   int depth, class;
   int status;
   XVisualInfo vinfo;
   Visual *old_visual;
   unsigned long mask;
   Visual *default_visual;
   XVisualInfo *def_rtn;
   int n;

   dpy = XtDisplay(biw);
   screen = DefaultScreen(dpy);
   status = 0;
   old_visual = biw->bim.vis.visual;

   /* Try 24-bit visuals first */

   if (biw->bim.visual_type == XvicUSE_24BIT ||
       (biw->bim.visual_type == XvicUSE_DEFAULT &&
        biw->bim.image_mode == XvicCOLOR)) {

      depth = 24;
      if (biw->bim.enable_direct_color) {
         class = DirectColor;
         status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
      }
      if (!status) {
         class = TrueColor;
         status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
      }

      /*!!!!*/ /* Non-24-bit color, like 3-3-2 DirectColor?? */
   }

   /* Check for Alloc mode.  If so, use the default visual, as long as	*/
   /* it's 8 bits.  Don't use ALLOC with 8 bits at all, if the root is	*/
   /* not 8 bits, because there are a lot of 8-bit assumptions w.r.t.	*/
   /* allocating colors from the default colormap.			*/

   if (!status && biw->bim.colormap_policy == XvicALLOC) {
      default_visual = XDefaultVisual(dpy, screen);
      vinfo.visualid = XVisualIDFromVisual(default_visual);
      def_rtn = XGetVisualInfo(dpy, VisualIDMask, &vinfo, &n);
      memcpy((void *)&vinfo, (void *)def_rtn, sizeof(XVisualInfo));
      XFree(def_rtn);
      if (vinfo.depth == 8)		/* it's okay */
         status = 1;
      else {			/* try 24 again, w/o ALLOC */
         biw->bim.colormap_policy = XvicHALF;
         biw->bim.visual_type = XvicUSE_24BIT;
         return GetVisual(biw);		/* try again */
      }
   }

   /* Try 8-bit visuals next */

   if (!status) {
      depth = 8;
      class = PseudoColor;
      status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
   }
   if (!status) {
      class = StaticColor;
      status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
   }
   if (!status) {
      class = GrayScale;
      status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
   }
   if (!status) {
      class = StaticGray;
      status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
   }

   /*!!!!*/ /* Non-8-bit modes? */

   /* If no 8-bit visual is available, we check for 24-bit visuals	*/
   /* again (they weren't checked for in BW/pseudo mode with visualType	*/
   /* == USE_DEFAULT).  However, since I know of no 24-bit displays	*/
   /* that don't also have 8-bit visuals, it hardly seems worth it.	*/

   if (!status) {
      depth = 24;
      class = DirectColor;
      status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
      if (!status) {
         class = TrueColor;
         status = XMatchVisualInfo(dpy, screen, depth, class, &vinfo);
      }
   }

   if (!status)
      FatalError(biw, "NoVisual", "Unable to find a visual to use!");

   biw->bim.vis.visual = vinfo.visual;
   biw->bim.vis.class = vinfo.class;
   biw->bim.vis.cmap_size = vinfo.colormap_size;
   biw->core.depth = vinfo.depth;

   /* Determine # of bits and shift value for each primary color.	*/
   /* Only matters with TrueColor or DirectColor, but it doesn't hurt	*/
   /* with other visual types.						*/

   biw->bim.vis.red_mask = vinfo.red_mask;
   mask = biw->bim.vis.red_mask;
   biw->bim.vis.red_shift = 0;
   biw->bim.vis.red_nbits = 0;
   if (mask) {
      while ((mask&1) == 0) {
         biw->bim.vis.red_shift++;
         mask >>= 1;
      }
      while ((mask&1) != 0) {
         biw->bim.vis.red_nbits++;
         mask >>= 1;
      }
   }
   biw->bim.vis.green_mask = vinfo.green_mask;
   mask = biw->bim.vis.green_mask;
   biw->bim.vis.green_shift = 0;
   biw->bim.vis.green_nbits = 0;
   if (mask) {
      while ((mask&1) == 0) {
         biw->bim.vis.green_shift++;
         mask >>= 1;
      }
      while ((mask&1) != 0) {
         biw->bim.vis.green_nbits++;
         mask >>= 1;
      }
   }
   biw->bim.vis.blue_mask = vinfo.blue_mask;
   mask = biw->bim.vis.blue_mask;
   biw->bim.vis.blue_shift = 0;
   biw->bim.vis.blue_nbits = 0;
   if (mask) {
      while ((mask&1) == 0) {
         biw->bim.vis.blue_shift++;
         mask >>= 1;
      }
      while ((mask&1) != 0) {
         biw->bim.vis.blue_nbits++;
         mask >>= 1;
      }
   }

   /* Check consistency of colormapPolicy of FULL_COLOR */

   if (biw->bim.vis.class == DirectColor || biw->bim.vis.class == TrueColor)
      biw->bim.colormap_policy = XvicFULL_COLOR;
   else {
      if (biw->bim.colormap_policy == XvicFULL_COLOR) {
         biw->bim.colormap_policy = XvicHALF;
      }
   }

   DPR(("visual_id=%x, class=%d, depth=%d\n", vinfo.visualid, vinfo.class, vinfo.depth));

   return (biw->bim.vis.visual != old_visual);

}

/************************************************************************/
/* GraphicsExposeHandler						*/
/*									*/
/* Handles GraphicsExpose and NoExpose events from DoPan's XCopyArea	*/
/* call.  GraphicsExposes must be handled by calling Redisplay to	*/
/* repaint the missing area.  First, however, the area must be expanded	*/
/* by the difference between the min and max pans, in order to ensure	*/
/* that no holes are left.  Redisplay must be called directly for the	*/
/* same reason that it must be for DoPan.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GraphicsExposeHandler(w, client_data, event, continue_to_dispatch)
   Widget w;
   XtPointer client_data;
   XEvent *event;
   Boolean *continue_to_dispatch;
#else
GraphicsExposeHandler(
   Widget w,
   XtPointer client_data,
   XEvent *event,
   Boolean *continue_to_dispatch)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) w;
   XGraphicsExposeEvent *xe = (XGraphicsExposeEvent *) event;
   XExposeEvent expose;

   if (event->xany.type == GraphicsExpose) {
      expose.type = Expose;
      expose.x = xe->x - biw->bim.x_dpy_off;
      expose.width = xe->width;
      X_ViewConstrain(expose.x, expose.width);
      expose.y = xe->y - biw->bim.y_dpy_off;
      expose.height = xe->height;
      Y_ViewConstrain(expose.y, expose.height);

      if (biw->bim.pan_count != 0) {	/* outstanding pans */
         if (biw->bim.pan_dir & PAN_HORIZ) {
            expose.x -= (biw->bim.x_screen_pan - biw->bim.x_min_pan);
            expose.width += (biw->bim.x_max_pan - biw->bim.x_min_pan);
            X_ViewConstrain(expose.x, expose.width);
         }
         if (biw->bim.pan_dir & PAN_VERT) {
            expose.y -= (biw->bim.y_screen_pan - biw->bim.y_min_pan);
            expose.height += (biw->bim.y_max_pan - biw->bim.y_min_pan);
            Y_ViewConstrain(expose.y, expose.height);
         }
      }

      /* Now do the expose */

      _XvicRedisplay_Internal(biw, &expose, NULL);

      /* Adjust the outstanding GraphicsExpose count.  This event	*/
      /* closes out a GraphicsExpose set (i.e. from one XCopyArea) only	*/
      /* if the event count is 0.					*/

      if (xe->count == 0) {
         biw->bim.pan_count--;
         if (biw->bim.pan_count == 0)
            biw->bim.pan_dir = 0;
         if (biw->bim.pan_count < 0)
            FatalError(biw, "PanCountNeg",
				"Internal error: pan_count went negative");
      }
   }

   /* For a NoExpose count, there is nothing to do but decrement the	*/
   /* outstanding GraphicsExpose count.  A single XCopyArea will	*/
   /* generate one NoExpose, or a set of GraphicsExpose, but never both.*/

   if (event->xany.type == NoExpose) {
      biw->bim.pan_count--;
      if (biw->bim.pan_count == 0)
         biw->bim.pan_dir = 0;
      if (biw->bim.pan_count < 0)
         FatalError(biw, "PanCountNeg",
				"Internal error: pan_count went negative!");
   }
}

/************************************************************************/
/* NextMemoryTile							*/
/*									*/
/* Returns a pointer to the next tile that needs exposing.  The tile	*/
/* data must already be in memory, so an application callback is not	*/
/* needed.  Returns NULL if there are no in-memory tiles that need	*/
/* exposing.								*/
/************************************************************************/

static Tile *
#ifdef _NO_PROTO
NextMemoryTile(biw)
   XvicBasicImageWidget biw;
#else
NextMemoryTile(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Tile *tile;
   int i;

   for (i=0; i<biw->bim.num_tiles; i++) {
      tile = &biw->bim.tiles[i];
      if (tile->raw || tile->ximage || tile->pixmap) {	/* Exists in memory */

         if (_XvicRegionHasRect(&tile->img, biw->bim.expose_rgn)) {
            TOUCH_TILE(i);
            return tile;
         }
      }
   }
   return NULL;

}

/************************************************************************/
/* NextTile								*/
/*									*/
/* Returns a pointer to the next tile that needs exposing.  The tile	*/
/* data may or may not already be in memory, so an application callback	*/
/* may be needed (probably will be needed since NextMemory tile is	*/
/* normally called first).  Returns NULL if there are no tiles that	*/
/* need exposing.							*/
/************************************************************************/

static Tile *
#ifdef _NO_PROTO
NextTile(biw)
   XvicBasicImageWidget biw;
#else
NextTile(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Tile *tile;
   int i;

   for (i=0; i<biw->bim.num_tiles; i++) {
      tile = &biw->bim.tiles[i];
      if (_XvicRegionHasRect(&tile->img, biw->bim.expose_rgn)) {
         TOUCH_TILE(i);
         return tile;
      }
   }
   return NULL;

}

/************************************************************************/
/* RedrawBorder								*/
/*									*/
/* Redraws the highlight border and/or shadow around the widget.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
RedrawBorder(biw)
   XvicBasicImageWidget biw;
#else
RedrawBorder(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{

#if USE_MOTIF

   if (biw->primitive.highlight_thickness != 0) {
      if (biw->primitive.highlighted)
         (*(((XvicBasicImageWidgetClass) XtClass(biw))->
		primitive_class.border_highlight))((Widget)biw);
      else
         (*(((XvicBasicImageWidgetClass) XtClass(biw))->
		primitive_class.border_unhighlight))((Widget)biw);
   }

   if (biw->primitive.shadow_thickness != 0) {

#ifdef MOTIF_1_1
      _XmDrawShadow(XtDisplay(biw), XtWindow(biw),
		biw->primitive.top_shadow_GC, biw->primitive.bottom_shadow_GC,
		biw->primitive.shadow_thickness,
		biw->primitive.highlight_thickness,
		biw->primitive.highlight_thickness,
		biw->core.width - 2 * biw->primitive.highlight_thickness,
		biw->core.height - 2 * biw->primitive.highlight_thickness);
#else
      _XmDrawShadows(XtDisplay(biw), XtWindow(biw),
		biw->primitive.top_shadow_GC, biw->primitive.bottom_shadow_GC,
		biw->primitive.highlight_thickness,
		biw->primitive.highlight_thickness,
		biw->core.width - 2 * biw->primitive.highlight_thickness,
		biw->core.height - 2 * biw->primitive.highlight_thickness,
		biw->primitive.shadow_thickness,
		XmSHADOW_OUT);
#endif

   }

#else /* USE_MOTIF */
   return;
#endif

}

/************************************************************************/
/* ReduceRational							*/
/*									*/
/* Takes pointers to the numerator and denominator of a rational number,*/
/* and reduces them so they share no common integer factors.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ReduceRational(numer, denom)
   int *numer;
   int *denom;
#else
ReduceRational(
   int *numer,
   int *denom)
#endif /* _NO_PROTO */
{
   int factor;

   factor = 2;

   while (factor <= *numer && factor <= *denom) {
      while ((*numer % factor == 0) && (*denom % factor == 0)) {
         *numer /= factor;
         *denom /= factor;
      }
      if (factor == 2)
         factor++;
      else
         factor+=2;		/* skip evens */
   }
}

/************************************************************************/
/* SetDataRangeDefaults							*/
/*									*/
/* Sets up the defaults for the rawDataMin and rawDataMax when the	*/
/* data type changes.  The two arguments specify the old values of	*/
/* min and max.  If the current values are different (for either one!),	*/
/* we assume the user has set them and thus don't change the defaults.	*/
/* We also check the arg list to see if the user might have explicitly	*/
/* set them to the same value they used to be.  If neither of those are	*/
/* true, then the user hasn't changed the range, and we reset it to	*/
/* the default value for the data type.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetDataRangeDefaults(biw, args, num_args, old_min, old_max)
   XvicBasicImageWidget biw;
   ArgList args;
   Cardinal *num_args;
   double old_min;
   double old_max;
#else
SetDataRangeDefaults(
   XvicBasicImageWidget biw,
   ArgList args,
   Cardinal *num_args,
   double old_min,
   double old_max)
#endif /* _NO_PROTO */
{
   int i;

   if (biw->bim.raw_data_min != old_min || biw->bim.raw_data_max != old_max)
      return;			/* User reset them, nothing to do */

   for (i=0; i<*num_args; i++) {
      if ((strcmp(args[i].name, XvicNrawDataMin) == 0) ||
          (strcmp(args[i].name, XvicNrawDataMax) == 0))
         return;		/* User reset them, nothing to do */
   }

   switch (biw->bim.data_type) {	/* User didn't set, apply defaults */
      case XvicBYTE:
         biw->bim.raw_data_min = 0;
         biw->bim.raw_data_max = 255;
         break;
      case XvicHALF:
         biw->bim.raw_data_min = SHRT_MIN;
         biw->bim.raw_data_max = SHRT_MAX;
         break;
      case XvicUHALF:
         biw->bim.raw_data_min = 0;
         biw->bim.raw_data_max = USHRT_MAX;
         break;
      case XvicFULL:
         biw->bim.raw_data_min = INT_MIN;
         biw->bim.raw_data_max = INT_MAX;
         break;
      case XvicUFULL:
         biw->bim.raw_data_min = 0;
         biw->bim.raw_data_max = UINT_MAX;
         break;
      case XvicREAL:
      case XvicDOUBLE:
         biw->bim.raw_data_min = 0.0;
         biw->bim.raw_data_max = 1.0;
         break;
      default:			/* shouldn't happen */
         FatalError(biw, "BadDataType",
	    "Internal error: Unknown dataType in SetDataRangeDefaults()");
   }
}

/************************************************************************/
/* SetDataType								*/
/*									*/
/* Sets up the pixel size, lookup tables, and transformation parameters	*/
/* for the given data type.						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetDataType(biw)
   XvicBasicImageWidget biw;
#else
SetDataType(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i;
   int dn, red_dn=0, grn_dn=0, blu_dn=0;
   int min, max;
   unsigned short offset;
   Boolean use_bw, use_color;

   /* Validate values */

   if (biw->bim.scaled_data_max <= 0)
      biw->bim.scaled_data_max = 1;
   if (biw->bim.scaled_data_max >= LUT16_SIZE)
      biw->bim.scaled_data_max = LUT16_SIZE-1;
   if (biw->bim.output_data_max <= 0)
      biw->bim.output_data_max = 1;
   if (biw->bim.output_data_max >= LUT16_SIZE)
      biw->bim.output_data_max = LUT16_SIZE-1;
   if (biw->bim.raw_data_max <= biw->bim.raw_data_min)
      biw->bim.raw_data_max = biw->bim.raw_data_min + 1;	/* arbitrary */

   /* Allocate the necessary lookup buffers */

   use_bw = FALSE;
   use_color = FALSE;

   if (biw->bim.data_type != XvicBYTE || biw->bim.lut16_type != XvicRAW) {
      if (biw->bim.image_mode == XvicCOLOR ||
	  biw->bim.lut16_type == XvicPSEUDO ||
	  biw->bim.lut16_type == XvicPSEUDO_ONLY) {

         use_color = TRUE;

         if (biw->bim.lookup16_red == NULL)
            biw->bim.lookup16_red =
			_XvicMalloc(biw, LUT16_SIZE * sizeof(unsigned char));
         if (biw->bim.lookup16_grn == NULL)
            biw->bim.lookup16_grn =
			_XvicMalloc(biw, LUT16_SIZE * sizeof(unsigned char));
         if (biw->bim.lookup16_blu == NULL)
            biw->bim.lookup16_blu =
			_XvicMalloc(biw, LUT16_SIZE * sizeof(unsigned char));

         /* Make sure LUTs are allocated */

         if (biw->bim.red_lut16 == NULL) {
            biw->bim.red_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
            for (i=0; i<LUT16_SIZE; i++)
               biw->bim.red_lut16[i] = i;
         }
         if (biw->bim.green_lut16 == NULL) {
            biw->bim.green_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
            for (i=0; i<LUT16_SIZE; i++)
               biw->bim.green_lut16[i] = i;
         }
         if (biw->bim.blue_lut16 == NULL) {
            biw->bim.blue_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
            for (i=0; i<LUT16_SIZE; i++)
               biw->bim.blue_lut16[i] = i;
         }
      }
      else {
         use_bw = TRUE;

         if (biw->bim.lookup16_bw == NULL)
            biw->bim.lookup16_bw =
			_XvicMalloc(biw, LUT16_SIZE * sizeof(unsigned char));
      }
      /* BW stretch could be needed in both modes (e.g. pseudo), so be safe */
      if (biw->bim.stretch_lut16 == NULL) {
         biw->bim.stretch_lut16 =
			(LUT16)_XvicMalloc(biw, LUT16_SIZE * sizeof(int));
         for (i=0; i<LUT16_SIZE; i++)
            biw->bim.stretch_lut16[i] = i;
      }
   }

   if (!use_bw && biw->bim.lookup16_bw) {
      _XvicFree(biw, biw->bim.lookup16_bw,LUT16_SIZE*sizeof(unsigned char));
      biw->bim.lookup16_bw = NULL;
   }
   if (!use_color && biw->bim.lookup16_red) {
      _XvicFree(biw, biw->bim.lookup16_red,LUT16_SIZE*sizeof(unsigned char));
      biw->bim.lookup16_red = NULL;
   }
   if (!use_color && biw->bim.lookup16_grn) {
      _XvicFree(biw, biw->bim.lookup16_grn,LUT16_SIZE*sizeof(unsigned char));
      biw->bim.lookup16_grn = NULL;
   }
   if (!use_color && biw->bim.lookup16_blu) {
      _XvicFree(biw, biw->bim.lookup16_blu,LUT16_SIZE*sizeof(unsigned char));
      biw->bim.lookup16_blu = NULL;
   }

   /* Set up pixel size */

   switch (biw->bim.data_type) {
      case XvicBYTE:
         biw->bim.pixel_size = sizeof(XvicByte); break;
      case XvicHALF:
         biw->bim.pixel_size = sizeof(XvicHalf); break;
      case XvicUHALF:
         biw->bim.pixel_size = sizeof(XvicUHalf); break;
      case XvicFULL:
         biw->bim.pixel_size = sizeof(XvicFull); break;
      case XvicUFULL:
         biw->bim.pixel_size = sizeof(XvicUFull); break;
      case XvicREAL:
         biw->bim.pixel_size = sizeof(XvicReal); break;
      case XvicDOUBLE:
         biw->bim.pixel_size = sizeof(XvicDouble); break;

      default:			/* shouldn't happen */
         FatalError(biw, "BadDataType",
	    "Internal error: Unknown dataType in SetDataType()");
   }

   /* Set up lookup tables.  This is a three-step process, involving	*/
   /* prescale, stretch LUT, and postscale.  All are combined into one	*/
   /* lookup table for half/uhalf types.  All except prescale are	*/
   /* combined for the others (prescale must be done a pixel at a time	*/
   /* during the display process).					*/

   if (biw->bim.data_type == XvicBYTE && biw->bim.lut16_type == XvicRAW)
      return;				/* Nothing to do for byte/raw */

   if (biw->bim.data_type == XvicREAL || biw->bim.data_type == XvicDOUBLE)
      biw->bim.prescale_factor = (biw->bim.scaled_data_max + 1) /
			(biw->bim.raw_data_max - biw->bim.raw_data_min);
   else
      biw->bim.prescale_factor = (biw->bim.scaled_data_max + 1) /
			(biw->bim.raw_data_max - biw->bim.raw_data_min + 1);

   if (biw->bim.data_type == XvicHALF) {
      min = SHRT_MIN;
      max = SHRT_MAX + 1;	/* loop is < not <= */
   }
   else {
      min = 0;
      max = LUT16_SIZE;
   }

   for (i=min; i<max; i++) {
      if (biw->bim.data_type != XvicHALF && biw->bim.data_type != XvicUHALF)
         dn = i;
      else {					/* Prescale */
         if (i < biw->bim.raw_data_min)
            dn = 0;
         else if (i >= biw->bim.raw_data_max)
            dn = biw->bim.scaled_data_max;
         else
            dn = floor((i - biw->bim.raw_data_min) * biw->bim.prescale_factor);
      }
      if (use_color) {				/* Stretch lookup table */
         switch (biw->bim.lut16_type) {
            case XvicRAW:
               red_dn = dn;
               grn_dn = dn;
               blu_dn = dn;
               break;
            case XvicSTRETCH:
            case XvicPSEUDO_ONLY:
               red_dn = biw->bim.red_lut16[dn];
               grn_dn = biw->bim.green_lut16[dn];
               blu_dn = biw->bim.blue_lut16[dn];
               break;
            case XvicPSEUDO:
               red_dn = biw->bim.red_lut16[biw->bim.stretch_lut16[dn]];
               grn_dn = biw->bim.green_lut16[biw->bim.stretch_lut16[dn]];
               blu_dn = biw->bim.blue_lut16[biw->bim.stretch_lut16[dn]];
               break;
         }
         offset = i;
         if (i < 0) offset = i + LUT16_SIZE;	/* compensate for sign */
						/* Postscale */
         biw->bim.lookup16_red[offset] = (unsigned char)
			(((int)red_dn*256) / (biw->bim.output_data_max+1));
         biw->bim.lookup16_grn[offset] = (unsigned char)
			(((int)grn_dn*256) / (biw->bim.output_data_max+1));
         biw->bim.lookup16_blu[offset] = (unsigned char)
			(((int)blu_dn*256) / (biw->bim.output_data_max+1));
      }
      else {
         if (biw->bim.lut16_type == XvicSTRETCH)   /* Stretch lookup table */
            dn = biw->bim.stretch_lut16[dn];
         offset = i;
         if (i < 0) offset = i + LUT16_SIZE;	/* compensate for sign */
						/* Postscale */
         biw->bim.lookup16_bw[offset] = (unsigned char)
			(((int)dn*256) / (biw->bim.output_data_max+1));
      }
   }
}

/************************************************************************/
/* SetDisplayTransform							*/
/*									*/
/* Sets up DN_transform to indicate how to get a color index from a DN	*/
/* value.  Also, sets flags indicating whether or not to use the LUTs.	*/
/* This routine is not really needed; the tests could be done in	*/
/* _XvicCopyRawXimage, but it's more efficient to do them once here.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetDisplayTransform(biw)
   XvicBasicImageWidget biw;
#else
SetDisplayTransform(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{

   biw->bim.use_rgb_lut = FALSE;
   biw->bim.use_stretch_lut = FALSE;
   biw->bim.DN_transform = TRANS_DIRECT;

   /* LUTs are only used for USE_SW (otherwise they are set by SetUpColormap) */

   if (biw->bim.stretch_policy == XvicUSE_SW) {

      if ((biw->bim.image_mode == XvicCOLOR ||
	   biw->bim.lut16_type == XvicPSEUDO ||
	   biw->bim.lut16_type==XvicPSEUDO_ONLY) &&
	  biw->bim.lut_type == XvicSTRETCH)
         biw->bim.use_rgb_lut = TRUE;

      if (biw->bim.ps_as_color &&		/* 8-bit pseudo only here! */
	  (biw->bim.lut_type==XvicPSEUDO || biw->bim.lut_type==XvicPSEUDO_ONLY))
         biw->bim.use_rgb_lut = TRUE;

      if (biw->bim.image_mode == XvicBW &&
          (biw->bim.lut_type == XvicSTRETCH || biw->bim.lut_type == XvicPSEUDO))
         biw->bim.use_stretch_lut = TRUE;
   }

   /* Now figure out how to display based on the various modes */

   switch (biw->bim.colormap_policy) {

      case XvicFULL_COLOR:			/* 24-bit color */
         biw->bim.DN_transform = TRANS_FULLCOLOR;
         break;

      case XvicFULL:
         if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color)
            biw->bim.DN_transform = TRANS_332;
         else
            biw->bim.DN_transform = TRANS_DIRECT;
         break;

      case XvicHALF:
         if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color)
            biw->bim.DN_transform = TRANS_232;
         else
            biw->bim.DN_transform = TRANS_HALF;
         break;

      case XvicDITHER:
      case XvicALLOC:
         biw->bim.DN_transform = TRANS_CMAP;
         break;

      default:			/* shouldn't happen */
         FatalError(biw, "BadColormapPolicy",
	    "Internal error: Unknown colormapPolicy in SetDisplayTransform()");

   }
}

/************************************************************************/
/* SetTileCoords							*/
/*									*/
/* Sets the coordinates (image and prezoom) for all tiles.  Also resets	*/
/* the LRU counts, since all are now equal (and empty).			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetTileCoords(biw)
   XvicBasicImageWidget biw;
#else
SetTileCoords(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int xt, yt;
   int index;

   for (yt = 0; yt < biw->bim.num_tiles_y; yt++) {
      for (xt = 0; xt < biw->bim.num_tiles_x; xt++) {
         index = TILE_INDEX(xt,yt);
         biw->bim.tiles[index].img.x1 = xt * biw->bim.tile_width;
         biw->bim.tiles[index].img.x2 =
		MIN((xt+1) * biw->bim.tile_width - 1, biw->bim.image_width-1);
         biw->bim.tiles[index].img.y1 = yt * biw->bim.tile_height;
         biw->bim.tiles[index].img.y2 =
		MIN((yt+1) * biw->bim.tile_height - 1, biw->bim.image_height-1);

         biw->bim.tiles[index].pre.x1=X1_Img2Pre(biw->bim.tiles[index].img.x1);
         biw->bim.tiles[index].pre.x2=X2_Img2Pre(biw->bim.tiles[index].img.x2);
         biw->bim.tiles[index].pre.y1=Y1_Img2Pre(biw->bim.tiles[index].img.y1);
         biw->bim.tiles[index].pre.y2=Y2_Img2Pre(biw->bim.tiles[index].img.y2);
         biw->bim.tiles[index].lru = 0;
      }
   }
   biw->bim.lru = 0;
   biw->bim.current_tile = -1;
}

/************************************************************************/
/* SetTileDpyCoords							*/
/*									*/
/* Sets the display coordinates for all tiles.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetTileDpyCoords(biw)
   XvicBasicImageWidget biw;
#else
SetTileDpyCoords(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int xt, yt;
   int index;

   for (yt = 0; yt < biw->bim.num_tiles_y; yt++) {
      for (xt = 0; xt < biw->bim.num_tiles_x; xt++) {
         index = TILE_INDEX(xt,yt);

         biw->bim.tiles[index].dpy.x1=X1_Pre2Dpy(biw->bim.tiles[index].pre.x1);
         biw->bim.tiles[index].dpy.x2=X2_Pre2Dpy(biw->bim.tiles[index].pre.x2);
         biw->bim.tiles[index].dpy.y1=Y1_Pre2Dpy(biw->bim.tiles[index].pre.y1);
         biw->bim.tiles[index].dpy.y2=Y2_Pre2Dpy(biw->bim.tiles[index].pre.y2);
      }
   }
}

/************************************************************************/
/* SetUpColormap							*/
/*									*/
/* Fills in the colormap with the appropriate values.  If stretchPolicy	*/
/* is USE_SW, the colormap is set to the standard values.  If it is	*/
/* USE_HW, the current stretches are taken into account and the		*/
/* stretched values are written out.  This function should be called	*/
/* whenever the stretch changes with USE_HW (no need to call it for a	*/
/* new stretch with USE_SW).						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetUpColormap(biw)
   XvicBasicImageWidget biw;
#else
SetUpColormap(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int i, r, g, b, rb;
   int offs;
   int green_start, rb_start, gray_start;
   int status;
   XColor cells[CMAP_SIZE_MAX];
   XColor cell;
   Display *dpy;
   unsigned long plane_mask;
   Boolean msg_issued, msg_issued_rb;

   dpy = XtDisplay(biw);

   if (biw->bim.colormap == None) {	/* shouldn't happen! */
      GetColormap(biw);
      CALL_InstallColormap(biw, ((Widget)biw));
   }

   if (biw->bim.cmap_alloc_mode == AllocNone &&
       biw->bim.colormap_policy != XvicALLOC)
      return;			/* Colormap can't be changed so just return */

   switch (biw->bim.colormap_policy) {

      case XvicFULL_COLOR:			/* 24-bit color */
         for (i=0; i<CMAP_SIZE; i++) {
            if (biw->bim.stretch_policy == XvicUSE_SW ||
		biw->bim.lut_type == XvicRAW) {		/* use straight ramp */
               cells[i].red = i << 8;
               cells[i].green = i << 8;
               cells[i].blue = i << 8;
            }
            else if (biw->bim.image_mode == XvicCOLOR ||
		biw->bim.lut_type == XvicPSEUDO_ONLY) {	/* use color LUTs */
               cells[i].red = biw->bim.red_lut[i] << 8;
               cells[i].green = biw->bim.green_lut[i] << 8;
               cells[i].blue = biw->bim.blue_lut[i] << 8;
            }
            else if (biw->bim.lut_type == XvicSTRETCH) { /* use bw LUTs */
               cells[i].red = cells[i].green = cells[i].blue =
			biw->bim.stretch_lut[i] << 8;
            }
            else {				/* must be BW, PSEUDO */
               cells[i].red = biw->bim.red_lut[biw->bim.stretch_lut[i]] << 8;
               cells[i].green=biw->bim.green_lut[biw->bim.stretch_lut[i]]<<8;
               cells[i].blue= biw->bim.blue_lut[biw->bim.stretch_lut[i]]<< 8;
            }

            cells[i].pixel = (i<<biw->bim.vis.red_shift) |
			     (i<<biw->bim.vis.green_shift) |
			     (i<<biw->bim.vis.blue_shift);
            cells[i].flags = DoRed | DoGreen | DoBlue;
         }
         XStoreColors(dpy, biw->bim.colormap, cells, CMAP_SIZE);
         break;

      case XvicFULL:
         if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color) {

            /* 3/3/2 RGB pattern */

            for (r=0; r<8; r++)
               for (g=0; g<8; g++)
                  for (b=0; b<4; b++) {
                     cells[(r<<5)|(g<<2)|b].red = (65535 * r) / 7;
                     cells[(r<<5)|(g<<2)|b].green = (65535 * g) / 7;
                     cells[(r<<5)|(g<<2)|b].blue = (65535 * b) / 3;
                  }
         }
         else {
            for (i=0; i<CMAP_SIZE; i++) {
               if (biw->bim.stretch_policy == XvicUSE_SW ||
                   biw->bim.lut_type == XvicRAW)
                  cells[i].red = cells[i].green = cells[i].blue = i << 8;
               else if (biw->bim.lut_type == XvicSTRETCH)
                  cells[i].red = cells[i].green = cells[i].blue =
				biw->bim.stretch_lut[i] << 8;
               else if (biw->bim.lut_type == XvicPSEUDO) {
                  cells[i].red = biw->bim.red_lut[biw->bim.stretch_lut[i]] << 8;
                  cells[i].green=biw->bim.green_lut[biw->bim.stretch_lut[i]]<<8;
                  cells[i].blue= biw->bim.blue_lut[biw->bim.stretch_lut[i]]<< 8;
               }
               else {		/* Must be XvicPSEUDO_ONLY */
                  cells[i].red = biw->bim.red_lut[i] << 8;
                  cells[i].green = biw->bim.green_lut[i] << 8;
                  cells[i].blue = biw->bim.blue_lut[i] << 8;
               }
            }
         }
         for (i=0; i<CMAP_SIZE; i++) {
            cells[i].pixel = i;
            cells[i].flags = DoRed | DoGreen | DoBlue;
         }
         XStoreColors(dpy, biw->bim.colormap, cells, CMAP_SIZE);
         break;

      case XvicHALF:
         if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color) {

            /* 2/3/2 RGB pattern */

            for (r=0; r<4; r++)
               for (g=0; g<8; g++)
                  for (b=0; b<4; b++) {
                     cells[128+((r<<5)|(g<<2)|b)].red = (65535 * r) / 3;
                     cells[128+((r<<5)|(g<<2)|b)].green = (65535 * g) / 7;
                     cells[128+((r<<5)|(g<<2)|b)].blue = (65535 * b) / 3;
                  }
         }
         else {
            offs = CMAP_SIZE/2;
            for (i=0; i<CMAP_SIZE/2; i++) {
               if (biw->bim.stretch_policy == XvicUSE_SW ||
                   biw->bim.lut_type == XvicRAW)
                  cells[offs+i].red = cells[offs+i].green = cells[offs+i].blue =
				(i*2) << 8;
               else if (biw->bim.lut_type == XvicSTRETCH)
                  cells[offs+i].red = cells[offs+i].green = cells[offs+i].blue =
				biw->bim.stretch_lut[i*2] << 8;
               else if (biw->bim.lut_type == XvicPSEUDO) {
                  cells[offs+i].red =
			biw->bim.red_lut[biw->bim.stretch_lut[i*2]] << 8;
                  cells[offs+i].green =
			biw->bim.green_lut[biw->bim.stretch_lut[i*2]] << 8;
                  cells[offs+i].blue =
			biw->bim.blue_lut[biw->bim.stretch_lut[i*2]] << 8;
               }
               else {		/* Must be XvicPSEUDO_ONLY */
                  cells[offs+i].red = biw->bim.red_lut[i*2] << 8;
                  cells[offs+i].green = biw->bim.green_lut[i*2] << 8;
                  cells[offs+i].blue = biw->bim.blue_lut[i*2] << 8;
               }
            }
         }
         CopyDefaultColors(biw, cells, CMAP_SIZE/2);

         for (i=0; i<CMAP_SIZE; i++) {
            cells[i].pixel = i;
            cells[i].flags = DoRed | DoGreen | DoBlue;
         }
         XStoreColors(dpy, biw->bim.colormap, cells, CMAP_SIZE);
         break;


      case XvicDITHER:
         if (biw->bim.gray_levels == 1)
            biw->bim.gray_levels = 0;
         if (biw->bim.red_levels == 1)
            biw->bim.red_levels = 0;
         if (biw->bim.green_levels == 1)
            biw->bim.green_levels = 0;
         if (biw->bim.blue_levels == 1)
            biw->bim.blue_levels = 0;
         green_start = CMAP_SIZE - biw->bim.green_levels;
         rb_start = green_start - (biw->bim.red_levels * biw->bim.blue_levels);
         gray_start = rb_start - biw->bim.gray_levels;

         /* Green ramp */

         for (g=0; g<biw->bim.green_levels; g++) {
            cells[green_start + g].green =
		(((CMAP_SIZE-1) * g) / (biw->bim.green_levels - 1)) << 8;
            cells[green_start + g].red = 0;
            cells[green_start + g].blue = 0;
            biw->bim.cmap_green[g] = green_start + g;
         }

         /* Red/Blue combination ramp */

         for (b=0; b<biw->bim.blue_levels; b++) {
            for (r=0; r<biw->bim.red_levels; r++) {
               cells[rb_start + r + (b * biw->bim.red_levels)].red =
		(((CMAP_SIZE-1) * r) / (biw->bim.red_levels - 1)) << 8;
               cells[rb_start + r + (b * biw->bim.red_levels)].blue =
		(((CMAP_SIZE-1) * b) / (biw->bim.blue_levels - 1)) << 8;
               cells[rb_start + r + (b * biw->bim.red_levels)].green = 0;
               biw->bim.cmap_rb[r + (b * biw->bim.red_levels)] =
			rb_start + r + (b * biw->bim.red_levels);
            }
         }

         /* Gray ramp */

         for (i=0; i<biw->bim.gray_levels; i++) {
            cells[gray_start + i].red = cells[gray_start + i].green =
					cells[gray_start + i].blue =
		(((CMAP_SIZE-1) * i) / (biw->bim.gray_levels - 1)) << 8;
            biw->bim.cmap_gray[i] = gray_start + i;
         }

         CopyDefaultColors(biw, cells, gray_start);

         for (i=0; i<CMAP_SIZE; i++) {
            cells[i].pixel = i;
            cells[i].flags = DoRed | DoGreen | DoBlue;
         }
         XStoreColors(dpy, biw->bim.colormap, cells, CMAP_SIZE);
         break;

      case XvicALLOC:

         if (biw->bim.image_mode == XvicBW && !biw->bim.ps_as_color &&
	     (biw->bim.dither_mode==XvicNONE ||
	      biw->bim.dither_mode==XvicORDERED)) {

            /* Allocate grays */

            if (biw->bim.stretch_policy == XvicUSE_HW) {

               /* For HW stretch, we must allocate cells private */

               if (biw->bim.gray_alloced != biw->bim.gray_levels ||
                   !biw->bim.alloc_private)
                  FreeAllocedColors(biw);
               if (biw->bim.gray_alloced == 0) {
                  msg_issued = FALSE;
                  if (biw->bim.gray_levels == 1)
                     biw->bim.gray_levels = 0;
                  do {
                     status = XAllocColorCells(dpy, biw->bim.colormap, FALSE,
			&plane_mask,0,biw->bim.cmap_gray, biw->bim.gray_levels);
                     if (!status) {
                        if (!msg_issued)
                           WarningMsg(biw, "TooManyGrays",
				"Can't allocate all colors requested by XvicNgrayLevels.\nReducing number of gray levels.");
                        msg_issued = TRUE;
                        if (biw->bim.gray_levels > 16)
                           biw->bim.gray_levels-=8;
                        else
                           biw->bim.gray_levels--;
                        if (biw->bim.gray_levels <= 1) {
                           WarningMsg(biw, "NoGrays",
				"Can't allocate any gray colorcells!  Visual presentation may be wrong.");
                           status = 1;	/* can't do anything else; exit loop */
                        }
                     }
                  } while (!status && biw->bim.gray_levels > 1);

                  biw->bim.gray_alloced = biw->bim.gray_levels;
                  biw->bim.alloc_private = TRUE;
               }
               for (i=0; i<biw->bim.gray_levels; i++) {
                  cells[i].pixel = biw->bim.cmap_gray[i];
                  cells[i].flags = DoRed | DoGreen | DoBlue;
                  offs = ((CMAP_SIZE-1) * i) / (biw->bim.gray_levels - 1);
                  if (biw->bim.lut_type == XvicRAW)
                     cells[i].red = cells[i].green = cells[i].blue = offs << 8;
                  else if (biw->bim.lut_type == XvicSTRETCH)
                     cells[i].red = cells[i].green = cells[i].blue =
                        biw->bim.stretch_lut[offs] << 8;
                  else if (biw->bim.lut_type == XvicPSEUDO) {
                     cells[i].red =
			biw->bim.red_lut[biw->bim.stretch_lut[offs]] << 8;
                     cells[i].green =
			biw->bim.green_lut[biw->bim.stretch_lut[offs]] << 8;
                     cells[i].blue =
			biw->bim.blue_lut[biw->bim.stretch_lut[offs]] << 8;
                  }
                  else {	/* Must be XvicPSEUDO_ONLY */
                     cells[i].red = biw->bim.red_lut[offs] << 8;
                     cells[i].green = biw->bim.green_lut[offs] << 8;
                     cells[i].blue = biw->bim.blue_lut[offs] << 8;
                  }
               }
               XStoreColors(dpy, biw->bim.colormap, cells,biw->bim.gray_levels);
            }
            else {		/* USE_SW */

               /* For SW stretch, allocate cells shared */

               if (biw->bim.gray_alloced != biw->bim.gray_levels ||
                   biw->bim.alloc_private) {

                  /* If the right # of non-private cells are already */
                  /* allocated, then there is nothing to do.         */

                  msg_issued = FALSE;
                  if (biw->bim.gray_levels == 1)
                     biw->bim.gray_levels = 0;
                  do {
                     FreeAllocedColors(biw);
                     for (i=0; i<biw->bim.gray_levels; i++) {
                        cell.red = cell.green = cell.blue =
		          (((CMAP_SIZE-1) * i) / (biw->bim.gray_levels-1)) << 8;
                        status = XAllocColor(dpy, biw->bim.colormap, &cell);
                        if (status) {	/* got it */
                           biw->bim.cmap_gray[i] = cell.pixel;
                           biw->bim.gray_alloced++;
                        }
                        else {		/* Allocation error! */
                           if (!msg_issued)
                              WarningMsg(biw, "TooManyGrays",
				"Can't allocate all colors requested by XvicNgrayLevels.\nReducing number of gray levels.");
                           msg_issued = TRUE;
                           if (biw->bim.gray_levels > 16)
                              biw->bim.gray_levels-=8;
                           else
                              biw->bim.gray_levels--;
                           if (biw->bim.gray_levels == 1) {
                              WarningMsg(biw, "NoGrays",
				"Can't allocate any gray colorcells!  Visual presentation may be wrong.");
                           }
                           break;	/* Exit 'for' and try again */
                        }	/* end else allocation error */
                     }		/* end for */
                  } while (!status && biw->bim.gray_levels > 1);

                  biw->bim.alloc_private = FALSE;
               }
            }
         }
         else {		/* No grays, so alloc Kagels dither pattern */

            if (biw->bim.green_alloced != biw->bim.green_levels ||
                biw->bim.red_alloced != biw->bim.red_levels ||
                biw->bim.blue_alloced != biw->bim.blue_levels ||
                biw->bim.alloc_private) {

               /* If the right # of non-private cells are already */
               /* allocated, then there is nothing to do.         */

               msg_issued = FALSE;
               msg_issued_rb = FALSE;

               if (biw->bim.red_levels == 1)
                  biw->bim.red_levels = 0;
               if (biw->bim.green_levels == 1)
                  biw->bim.green_levels = 0;
               if (biw->bim.blue_levels == 1)
                  biw->bim.blue_levels = 0;
               do {
                  FreeAllocedColors(biw);

                  /* Green ramp */

                  for (g=0; g<biw->bim.green_levels; g++) {
                     cell.green =
		         (((CMAP_SIZE-1) * g) / (biw->bim.green_levels-1)) << 8;
                     cell.red = cell.blue = 0;
                     status = XAllocColor(dpy, biw->bim.colormap, &cell);
                     if (status) {	/* got it */
                        biw->bim.cmap_green[g] = cell.pixel;
                        biw->bim.green_alloced++;
                     }
                     else {		/* Allocation error! */
                        if (!msg_issued)
                           WarningMsg(biw, "TooManyGreens",
				"Can't allocate all colors requested by XvicNgreenLevels.\nReducing number of green levels.");
                        msg_issued = TRUE;
                        if (biw->bim.green_levels > 16)
                           biw->bim.green_levels-=8;
                        else
                           biw->bim.green_levels--;
                        if (biw->bim.green_levels <= 1) {
                           WarningMsg(biw, "NoGreens",
				"Can't allocate any green colorcells!  Visual presentation may be wrong.");
                           biw->bim.green_levels = 0;
                        }
                        break;		/* Exit 'for' and try again */
                     }		/* end else allocation error */
                  }	/* end for */

                  /* If alloc error on green, skip red/blue until grn is right*/
                  if (!status && biw->bim.green_levels > 1)
                     continue;

                  /* Red/Blue combination ramp */

                  /* Think of this as: */
                  /* for (b=0..blue_levels)  for (r=0..red_levels) */
                  /* but the loops are combined for the error-handling "break"*/

                  for (rb=0;rb<biw->bim.blue_levels*biw->bim.red_levels;rb++) {
                     if (biw->bim.red_levels == 0)	/* avoid divide by 0 */
                        b = r = 0;
                     else {
                        b = rb / biw->bim.red_levels;
                        r = rb % biw->bim.red_levels;
                     }
                     cell.red =
			  (((CMAP_SIZE-1) * r) / (biw->bim.red_levels-1)) << 8;
                     cell.blue =
			  (((CMAP_SIZE-1) * b) / (biw->bim.blue_levels-1)) << 8;
                     cell.green = 0;
                     status = XAllocColor(dpy, biw->bim.colormap, &cell);
                     if (status) {		/* got it */
                        /* really [r + (b*biw->bim.red_levels)] */
                        biw->bim.cmap_rb[biw->bim.rb_alloced] = cell.pixel;
                        biw->bim.rb_alloced++;
                     }
                     else {			/* Allocation error! */
                        if (!msg_issued_rb)
                           WarningMsg(biw, "TooManyRedBlues",
				"Can't allocate all colors requested by XvicNredLevels and XvicNblueLevels.\nReducing number of red and/or blue levels.");
                        msg_issued_rb = TRUE;
                        /* We only decrement by 1 cuz r&b are usually balanced*/
                        if (biw->bim.blue_levels >= biw->bim.red_levels)
                           biw->bim.blue_levels--;
                        else
                           biw->bim.red_levels--;
                        if (biw->bim.red_levels <= 1 ||
				biw->bim.blue_levels <= 1) {
                           WarningMsg(biw, "NoRedBlues",
				"Can't allocate any red/blue colorcells!  Visual presentation may be wrong.");
                           biw->bim.red_levels = biw->bim.blue_levels = 0;
                        }
                        break;		/* Exit 'for' and try again */
                     }		/* end else allocation error */
                  }	/* end for */
                  biw->bim.red_alloced = biw->bim.red_levels;
                  biw->bim.blue_alloced = biw->bim.blue_levels;

                  biw->bim.alloc_private = FALSE;

               } while (!status && (biw->bim.green_levels > 1 ||
			(biw->bim.red_levels > 1 && biw->bim.blue_levels > 1)));

            }		/* end if alloced==levels */
         }		/* end else color */

         break;

      default:			/* can't happen */
         FatalError(biw, "BadColormapPolicy2",
	    "Internal error: Unknown colormapPolicy in SetUpColormap()");
   }
}

/************************************************************************/
/* SetUpGCs								*/
/*									*/
/* Sets any GC's needed by the widget.  Frees any old ones first	*/
/* (in case the widget was re-realized).				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetUpGCs(biw)
   XvicBasicImageWidget biw;
#else
SetUpGCs(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   unsigned long valueMask;
   XGCValues values;

   if (biw->bim.img_gc != NULL)
      XFreeGC(XtDisplay(biw), biw->bim.img_gc);
   if (biw->bim.pan_gc != NULL)
      XFreeGC(XtDisplay(biw), biw->bim.pan_gc);

   values.graphics_exposures = FALSE;

   /* The SGI server forces us to use ClipByChildren for the overlay	*/
   /* plane, otherwise image draws go into the overlay if the visuals	*/
   /* happen to match.  The VITec, on the other hand, forces us to use	*/
   /* IncludeInferiors or the underlay image won't get drawn at all!	*/
   /* Since the SGI is the inconsistent one (it only bleeds through to	*/
   /* the overlay if the visuals are the same; if they're different	*/
   /* then subwindow_mode doesn't matter), we choose to explicitly	*/
   /* check for an SGI server.  (Plus, we already check for SGI in	*/
   /* Image, so let's only get one special case).  The HP seems not to	*/
   /* care.								*/

   if (strcmp(ServerVendor(XtDisplay((Widget)biw)), "Silicon Graphics") == 0)
      values.subwindow_mode = ClipByChildren;
   else
      values.subwindow_mode = IncludeInferiors;

   valueMask = GCGraphicsExposures | GCSubwindowMode;

   biw->bim.img_gc = XCreateGC(XtDisplay(biw), XtWindow(biw),
		valueMask, &values);

   values.graphics_exposures = TRUE;

   biw->bim.pan_gc = XCreateGC(XtDisplay(biw), XtWindow(biw),
		valueMask, &values);

#if USE_MOTIF

   /* The following are used for the highlight/shadow.  This is copied	*/
   /* directly from Primitive.c; we can't just call GetHighlightGC etc.	*/
   /* because the routines are static.  Why oh why don't widgets do all	*/
   /* their display-related stuff in the Realize method instead of	*/
   /* initialize??  Sigh.						*/
   /* Primitive.c doesn't check for GC existence before releasing it,	*/
   /* so we don't here either.  It should be ok since this is only	*/
   /* called from Realize, after Primitive has had a chance to set	*/
   /* them up.  Primitive's Destroy will free them.			*/
   /* It's up to the application to set colors/pixmaps right (mainly	*/
   /* because I don't want to deal with it!).  It can always set the	*/
   /* highlight/shadow thicknesses to 0.				*/

   XtReleaseGC((Widget)biw, biw->primitive.highlight_GC);
   valueMask = GCForeground | GCBackground;
   values.foreground = biw->primitive.highlight_color;
   values.background = biw->core.background_pixel;
   if ((biw->primitive.highlight_pixmap != None) &&
       (biw->primitive.highlight_pixmap != XmUNSPECIFIED_PIXMAP)) {
      valueMask |= GCFillStyle | GCTile;
      values.fill_style = FillTiled;
      values.tile = biw->primitive.highlight_pixmap;
   }
   biw->primitive.highlight_GC = XtGetGC((Widget)biw, valueMask, &values);

   XtReleaseGC((Widget)biw, biw->primitive.top_shadow_GC);
   valueMask = GCForeground | GCBackground;
   values.foreground = biw->primitive.top_shadow_color;
   values.background = biw->primitive.foreground;
   if ((biw->primitive.top_shadow_pixmap != None) &&
       (biw->primitive.top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)) {
      valueMask |= GCFillStyle | GCTile;
      values.fill_style = FillTiled;
      values.tile = biw->primitive.top_shadow_pixmap;
   }
   biw->primitive.top_shadow_GC = XtGetGC((Widget)biw, valueMask, &values);

   XtReleaseGC((Widget)biw, biw->primitive.bottom_shadow_GC);
   valueMask = GCForeground | GCBackground;
   values.foreground = biw->primitive.bottom_shadow_color;
   values.background = biw->primitive.foreground;
   if ((biw->primitive.bottom_shadow_pixmap != None) &&
       (biw->primitive.bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)) {
      valueMask |= GCFillStyle | GCTile;
      values.fill_style = FillTiled;
      values.tile = biw->primitive.bottom_shadow_pixmap;
   }
   biw->primitive.bottom_shadow_GC = XtGetGC((Widget)biw, valueMask, &values);

#endif /* USE_MOTIF */

}

/************************************************************************/
/* SetUpZoomPan								*/
/*									*/
/* Sets the zoom resources, possibly using imageZoom and x|yZoom, then	*/
/* validates them all, reduces them, and calculates the effective zooms.*/
/* Also, constrains the pan value (if needed) and calculates the screen	*/
/* pan value based on the (new) zooms.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetUpZoomPan(biw)
   XvicBasicImageWidget biw;
#else
SetUpZoomPan(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   int view_width, view_height;

   /* If imageZoom was specified, set both X & Y zooms */

   if (biw->bim.image_zoom != 0) {
      biw->bim.x_zoom = biw->bim.image_zoom;
      biw->bim.y_zoom = biw->bim.image_zoom;
      biw->bim.image_zoom = 0;
   }

   /* If xZoom or yZoom were specified, set the actual X & Y zooms */

   if (biw->bim.x_zoom != 0) {
      if (biw->bim.x_zoom > 0) {
         biw->bim.x_zoom_in = biw->bim.x_zoom;
         biw->bim.x_zoom_out = 1;
      }
      else {
         biw->bim.x_zoom_in = 1;
         biw->bim.x_zoom_out = - biw->bim.x_zoom;
      }
      biw->bim.x_zoom = 0;
   }
   if (biw->bim.y_zoom != 0) {
      if (biw->bim.y_zoom > 0) {
         biw->bim.y_zoom_in = biw->bim.y_zoom;
         biw->bim.y_zoom_out = 1;
      }
      else {
         biw->bim.y_zoom_in = 1;
         biw->bim.y_zoom_out = - biw->bim.y_zoom;
      }
      biw->bim.y_zoom = 0;
   }

   /* Validate x/yZoomIn/Out */

   if (biw->bim.x_zoom_in <= 0)
      biw->bim.x_zoom_in = 1;
   if (biw->bim.x_zoom_out <= 0)
      biw->bim.x_zoom_out = 1;
   if (biw->bim.y_zoom_in <= 0)
      biw->bim.y_zoom_in = 1;
   if (biw->bim.y_zoom_out <= 0)
      biw->bim.y_zoom_out = 1;

   ReduceRational(&biw->bim.x_zoom_in, &biw->bim.x_zoom_out);
   ReduceRational(&biw->bim.y_zoom_in, &biw->bim.y_zoom_out);
   ReduceRational(&biw->bim.x_prezoom_in, &biw->bim.x_prezoom_out);
   ReduceRational(&biw->bim.y_prezoom_in, &biw->bim.y_prezoom_out);

   /* Calculate effective zoom factors */

   biw->bim.x_eff_zoom_in = biw->bim.x_zoom_in * biw->bim.x_prezoom_out;
   biw->bim.x_eff_zoom_out = biw->bim.x_zoom_out * biw->bim.x_prezoom_in;
   biw->bim.y_eff_zoom_in = biw->bim.y_zoom_in * biw->bim.y_prezoom_out;
   biw->bim.y_eff_zoom_out = biw->bim.y_zoom_out * biw->bim.y_prezoom_in;

   ReduceRational(&biw->bim.x_eff_zoom_in, &biw->bim.x_eff_zoom_out);
   ReduceRational(&biw->bim.y_eff_zoom_in, &biw->bim.y_eff_zoom_out);

   /* Constrain the pan values if necessary so the image never strays	*/
   /* from the edges of the window (unless the window is too big of	*/
   /* course, in which case the pan is 0).  We must convert the view	*/
   /* size to Img coordinates before the comparison.			*/

   if (biw->bim.constrain_pan==XvicX_ONLY || biw->bim.constrain_pan==XvicBOTH) {
      view_width = X_Dpy2Img(biw->bim.view_width);
      if (biw->bim.x_pan > biw->bim.image_width - view_width)
         biw->bim.x_pan = biw->bim.image_width - view_width;
      if (biw->bim.x_pan < 0)
         biw->bim.x_pan = 0;
   }
   if (biw->bim.constrain_pan==XvicY_ONLY || biw->bim.constrain_pan==XvicBOTH) {
      view_height = Y_Dpy2Img(biw->bim.view_height);
      if (biw->bim.y_pan > biw->bim.image_height - view_height)
         biw->bim.y_pan = biw->bim.image_height - view_height;
      if (biw->bim.y_pan < 0)
         biw->bim.y_pan = 0;
   }

   biw->bim.x_screen_pan =
		(biw->bim.x_pan * biw->bim.x_zoom_in) / biw->bim.x_zoom_out;
   biw->bim.y_screen_pan =
		(biw->bim.y_pan * biw->bim.y_zoom_in) / biw->bim.y_zoom_out;

}

/************************************************************************/
/* ValidateCmapResources						*/
/*									*/
/* Makes sure that all resources having to do with the colormap are	*/
/* valid and consistent with each other.  Invalid resources are quietly	*/
/* changed to match the settings of higher-priority resources.  The	*/
/* visual must be set up before calling this routine.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ValidateCmapResources(biw)
   XvicBasicImageWidget biw;
#else
ValidateCmapResources(
   XvicBasicImageWidget biw)
#endif /* _NO_PROTO */
{
   Boolean changeable_colormap;
   Boolean pseudo_mode;
   Boolean like_bw;

   if (biw->bim.image_mode != XvicBW && biw->bim.image_mode != XvicCOLOR)
      biw->bim.image_mode = XvicCOLOR;		/* Shouldn't happen!! */

   /* Can't use biw->bim.cmap_alloc_mode because it may not be set up yet! */
   changeable_colormap = (biw->bim.vis.class == DirectColor ||
          biw->bim.vis.class == PseudoColor ||
          biw->bim.vis.class == GrayScale);

   /* Make sure we don't try to use a Pseudocolor LUT in color mode */

   if (biw->bim.image_mode == XvicCOLOR) {
      if (biw->bim.lut_type == XvicPSEUDO)
         biw->bim.lut_type = XvicSTRETCH;
      if (biw->bim.lut_type == XvicPSEUDO_ONLY)
         biw->bim.lut_type = XvicRAW;

      if (biw->bim.lut16_type == XvicPSEUDO)
         biw->bim.lut16_type = XvicSTRETCH;
      if (biw->bim.lut16_type == XvicPSEUDO_ONLY)
         biw->bim.lut16_type = XvicRAW;
   }

   /* If 16-bit pseudocolor is on, turn off 8-bit pseudo */

   if (biw->bim.lut16_type==XvicPSEUDO||biw->bim.lut16_type==XvicPSEUDO_ONLY) {
      if (biw->bim.lut_type == XvicPSEUDO)
         biw->bim.lut_type = XvicSTRETCH;
      if (biw->bim.lut_type == XvicPSEUDO_ONLY)
         biw->bim.lut_type = XvicRAW;
   }

   /* Check for pseudocolor mode */

   pseudo_mode = (biw->bim.image_mode == XvicBW &&
     (biw->bim.lut_type==XvicPSEUDO || biw->bim.lut_type==XvicPSEUDO_ONLY ||
      biw->bim.lut16_type==XvicPSEUDO || biw->bim.lut16_type==XvicPSEUDO_ONLY));

   /* Since pseudocolor is either displayed like BW or like Color	*/
   /* depending on stretchPolicy, set some flags to make the		*/
   /* comparisons easier below.  NOTE:  this may change...		*/

   like_bw = TRUE;
   if ((biw->bim.image_mode == XvicCOLOR) ||
       (pseudo_mode && (biw->bim.stretch_policy == XvicUSE_SW))) {
      like_bw = FALSE;
   }

   switch (biw->bim.colormap_policy) {

      case XvicFULL:			/* these are treated the same */
      case XvicHALF:
         if (biw->bim.dither_mode != XvicNONE &&
	     biw->bim.dither_mode != XvicORDERED)
            biw->bim.dither_mode = XvicNONE;

         if (like_bw) {
            if (!changeable_colormap || biw->bim.dither_mode == XvicORDERED) {
               biw->bim.stretch_policy = XvicUSE_SW;
               if (pseudo_mode)
                  like_bw = FALSE;
            }
         }
         if (!like_bw) {	/* color */
            biw->bim.stretch_policy = XvicUSE_SW;
         }
         break;

      case XvicDITHER:
         biw->bim.stretch_policy = XvicUSE_SW;
         if (pseudo_mode)
            like_bw = FALSE;

         if (!like_bw) {	/* color */
            biw->bim.dither_mode = XvicKAGELS;
         }
         break;

      case XvicALLOC:
         if (!changeable_colormap)
            WarningMsg(biw, "NoChangeCmap",
		"XvicNcolormapPolicy is Alloc but the colormap is not writable.\nColormap may be wrong.");

         if (like_bw) {
            if (biw->bim.dither_mode == XvicKAGELS ||
	        biw->bim.dither_mode == XvicORDERED) {
               biw->bim.stretch_policy = XvicUSE_SW;
               if (pseudo_mode)
                  like_bw = FALSE;
            }
         }
         if (!like_bw) {	/* color */
            biw->bim.dither_mode = XvicKAGELS;
            biw->bim.stretch_policy = XvicUSE_SW;
         }
         break;

      case XvicFULL_COLOR:
         biw->bim.dither_mode = XvicNONE;	/* Just to make sure */
         if (!changeable_colormap)
            biw->bim.stretch_policy = XvicUSE_SW;
         break;

      default:			/* shouldn't happen, but just in case... */
         biw->bim.colormap_policy = XvicHALF;
         ValidateCmapResources(biw);	/* do it again... */
         break;
   }

   /* Set the ps_as_color flag if we're in pseudocolor displayed as color */

   biw->bim.ps_as_color = FALSE;
   if (pseudo_mode && biw->bim.stretch_policy == XvicUSE_SW) {
      biw->bim.ps_as_color = TRUE;
   }

   /* Make sure the "sum" of *_levels doesn't exceed the size of the	*/
   /* colormap.  If so, just set them all back to the defaults.		*/
   /* Allowance is made for ALLOC mode since it only allocs gray or	*/
   /* color modes, not both.						*/
   /* Also, any active levels in ALLOC mode are set to a minimum of 2	*/
   /* since it doesn't make much sense to have less.  Since we don't	*/
   /* know which are "active" if not in ALLOC, we can't do any minimum	*/
   /* settings.								*/
   /*!!!!*/ /* Don't just use 256 here in case we have a bigger cmap!! */

   if (biw->bim.colormap_policy == XvicALLOC) {
      if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color ||
		biw->bim.dither_mode == XvicKAGELS) {
         if (((biw->bim.red_levels * biw->bim.blue_levels) +
	      biw->bim.green_levels) > CMAP_SIZE) {
            biw->bim.red_levels = 16;
            biw->bim.green_levels = 16;
            biw->bim.blue_levels = 13;
         }
      }
      else {
         if (biw->bim.gray_levels > CMAP_SIZE)
            biw->bim.gray_levels = 16;
      }
   }
   else {
      if (((biw->bim.red_levels * biw->bim.blue_levels) +
           biw->bim.green_levels + biw->bim.gray_levels) > CMAP_SIZE) {
         biw->bim.red_levels = 16;
         biw->bim.green_levels = 16;
         biw->bim.blue_levels = 13;
         biw->bim.gray_levels = 16;
      }
   }
}

/************************************************************************/
/* WarningMsg								*/
/*									*/
/* Report a warning message.  "name" is the identifier for the warning	*/
/* message, while "def" is the default string to use if the error	*/
/* database is not available (which is almost always the case).		*/
/* This is just syntactic sugar for XtAppWarningMsg().			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
WarningMsg(biw, name, def)
   XvicBasicImageWidget biw;
   char *name;
   char *def;
#else
WarningMsg(
   XvicBasicImageWidget biw,
   char *name,
   char *def)
#endif /* _NO_PROTO */
{
   XtAppWarningMsg(XtWidgetToApplicationContext((Widget)biw),
	name, "XvicBasicImage", "XvicBasicImageWidgetWarning", def,
	(String *)NULL, (Cardinal *)NULL);
}

/************************************************************************/
/* WorkProc								*/
/*									*/
/* Check for a tile that needs exposing, and do it.  Tiles already in	*/
/* memory have priority.  If more tiles remain, return FALSE so the	*/
/* WorkProc will be called again, else return TRUE to terminate the	*/
/* WorkProc.  Only one tile is exposed per call.			*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
WorkProc(client_data)
   XtPointer client_data;
#else
WorkProc(
   XtPointer client_data)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)client_data;
   _XvicRect visible_rect;
   Tile *tile;
   XvicImageCallbackStruct cb;

   /* Clip the expose region to the actual visible region	*/
   /* Should be a nop, since this is done in Redisplay too.	*/

   _XvicGetVisibleRect(biw, &visible_rect);
   _XvicRegionIntersect(&visible_rect, biw->bim.expose_rgn);

   if (_XvicRegionIsEmpty(biw->bim.expose_rgn)) {
      biw->bim.work_proc_pending = FALSE;
      if (biw->bim.work_proc_active_callback) {
         cb.reason = XvicCR_WORK_PROC_ACTIVE;
         cb.flags = False;
         XtCallCallbackList((Widget)biw,
			biw->bim.work_proc_active_callback, &cb);
      }
      return TRUE;				/* Nothing to do */
   }

   /* Try to expose an area in memory (no callback) first */

   tile = NextMemoryTile(biw);
   if (tile)
      ExposeTile(biw, tile);
   else {
      tile = NextTile(biw);
      if (tile)
         ExposeTile(biw, tile);
   }
   if (_XvicRegionIsEmpty(biw->bim.expose_rgn)) {
      biw->bim.work_proc_pending = FALSE;
      if (biw->bim.work_proc_active_callback) {
         cb.reason = XvicCR_WORK_PROC_ACTIVE;
         cb.flags = False;
         XtCallCallbackList((Widget)biw,
			biw->bim.work_proc_active_callback, &cb);
      }
      return TRUE;				/* done! */
   }

   return FALSE;				/* do it again */
}


/************************************************************************/
/************************************************************************/
/*  R E S O U R C E   C O N V E R T E R S				*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* StringsAreEqual							*/
/*									*/
/* Determines if two strings are equal, ignoring case and "Xm" or	*/
/* "Xvic" prefix.  Test string must be lower case.			*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
StringsAreEqual(in_str, test_str)
   register char *in_str;
   register char *test_str;
#else
StringsAreEqual(
   register char *in_str,
   register char *test_str)
#endif /* _NO_PROTO */
{
   if (((in_str[0]=='X') || (in_str[0]=='x')) &&
       ((in_str[1]=='M') || (in_str[1]=='m')))
      in_str += 2;
   else
      if (((in_str[0]=='X') || (in_str[0]=='x')) &&
          ((in_str[1]=='V') || (in_str[1]=='v')) &&
          ((in_str[2]=='I') || (in_str[2]=='i')) &&
          ((in_str[3]=='C') || (in_str[3]=='c')))
         in_str += 4;

   do {
      if (tolower(*in_str) != *test_str++)
         return FALSE;
   } while (*in_str++);

   return TRUE;
}

/************************************************************************/
/* CvtStringToColormapPolicy						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToColormapPolicy(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToColormapPolicy(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToColormapPolicy", "XtToolkitError",
	    "String to ColormapPolicy conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "full"))
      val = XvicFULL;
   else if (StringsAreEqual((char *)fromVal->addr, "half"))
      val = XvicHALF;
   else if (StringsAreEqual((char *)fromVal->addr, "dither"))
      val = XvicDITHER;
   else if (StringsAreEqual((char *)fromVal->addr, "alloc"))
      val = XvicALLOC;
   else if (StringsAreEqual((char *)fromVal->addr, "full_color"))
      val = XvicFULL_COLOR;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRColormapPolicy);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToConstrainPan						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToConstrainPan(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToConstrainPan(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToConstrainPan", "XtToolkitError",
	    "String to ConstrainPan conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "none"))
      val = XvicNONE;
   else if (StringsAreEqual((char *)fromVal->addr, "x_only"))
      val = XvicX_ONLY;
   else if (StringsAreEqual((char *)fromVal->addr, "y_only"))
      val = XvicY_ONLY;
   else if (StringsAreEqual((char *)fromVal->addr, "both"))
      val = XvicBOTH;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRConstrainPan);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToDataSavePolicy						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToDataSavePolicy(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToDataSavePolicy(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToDataSavePolicy", "XtToolkitError",
	    "String to DataSavePolicy conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "none"))
      val = XvicNONE;
   else if (StringsAreEqual((char *)fromVal->addr, "raw"))
      val = XvicRAW;
   else if (StringsAreEqual((char *)fromVal->addr, "ximage"))
      val = XvicXIMAGE;
   else if (StringsAreEqual((char *)fromVal->addr, "pixmap"))
      val = XvicPIXMAP;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRDataSavePolicy);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToDataType							*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToDataType(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToDataType(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToDataType", "XtToolkitError",
	    "String to DataType conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "byte"))
      val = XvicBYTE;
   else if (StringsAreEqual((char *)fromVal->addr, "half"))
      val = XvicHALF;
   else if (StringsAreEqual((char *)fromVal->addr, "uhalf"))
      val = XvicUHALF;
   else if (StringsAreEqual((char *)fromVal->addr, "full"))
      val = XvicFULL;
   else if (StringsAreEqual((char *)fromVal->addr, "ufull"))
      val = XvicUFULL;
   else if (StringsAreEqual((char *)fromVal->addr, "real"))
      val = XvicREAL;
   else if (StringsAreEqual((char *)fromVal->addr, "double"))
      val = XvicDOUBLE;
   else if (StringsAreEqual((char *)fromVal->addr, "doub"))
      val = XvicDOUBLE;			/* just in case... */
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRDataType);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToDitherMode						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToDitherMode(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToDitherMode(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToDitherMode", "XtToolkitError",
	    "String to DitherMode conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "none"))
      val = XvicNONE;
   else if (StringsAreEqual((char *)fromVal->addr, "ordered"))
      val = XvicORDERED;
   else if (StringsAreEqual((char *)fromVal->addr, "kagels"))
      val = XvicKAGELS;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRDitherMode);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToDouble							*/
/* I can't BELIEVE this isn't included in Xt or Xm!!  Conversion to	*/
/* float is, but not to double.  Go figure.				*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToDouble(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToDouble(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static double val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToDouble", "XtToolkitError",
	    "String to Double conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (sscanf((char *)fromVal->addr, "%lf", &val) != 1) {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRDouble);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(double)) {
         toVal->size = sizeof(double);
         return FALSE;
      }
      *(double *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(double);
   return TRUE;
}

/************************************************************************/
/* CvtStringToImageMode							*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToImageMode(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToImageMode(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToImageMode", "XtToolkitError",
	    "String to ImageMode conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "color"))
      val = XvicCOLOR;
   else if (StringsAreEqual((char *)fromVal->addr, "bw"))
      val = XvicBW;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRImageMode);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToLutType							*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToLutType(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToLutType(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToLutType", "XtToolkitError",
	    "String to LutType conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "stretch"))
      val = XvicSTRETCH;
   else if (StringsAreEqual((char *)fromVal->addr, "raw"))
      val = XvicRAW;
   else if (StringsAreEqual((char *)fromVal->addr, "pseudo"))
      val = XvicPSEUDO;
   else if (StringsAreEqual((char *)fromVal->addr, "pseudo_only"))
      val = XvicPSEUDO_ONLY;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRLutType);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToStretchPolicy						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToStretchPolicy(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToStretchPolicy(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToStretchPolicy", "XtToolkitError",
	    "String to StretchPolicy conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "use_hw"))
      val = XvicUSE_HW;
   else if (StringsAreEqual((char *)fromVal->addr, "use_sw"))
      val = XvicUSE_SW;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRStretchPolicy);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToVisualType						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToVisualType(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToVisualType(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToVisualType", "XtToolkitError",
	    "String to VisualType conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "use_default"))
      val = XvicUSE_DEFAULT;
   else if (StringsAreEqual((char *)fromVal->addr, "use_8bit"))
      val = XvicUSE_8BIT;
   else if (StringsAreEqual((char *)fromVal->addr, "use_24bit"))
      val = XvicUSE_24BIT;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRVisualType);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* CvtStringToWorkProcPolicy						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToWorkProcPolicy(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToWorkProcPolicy(
   Display *dpy,
   XrmValue *args,
   Cardinal *num_args,
   XrmValue *fromVal,
   XrmValue *toVal,
   XtPointer *data)
#endif /* _NO_PROTO */

{
   static unsigned char val;

   if (*num_args != 0)
      XtAppWarningMsg(
	    XtDisplayToApplicationContext(dpy),
	    "wrongParameters", "cvtStringToWorkProcPolicy", "XtToolkitError",
	    "String to WorkProcPolicy conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "none"))
      val = XvicNONE;
   else if (StringsAreEqual((char *)fromVal->addr, "read"))
      val = XvicREAD;
   else if (StringsAreEqual((char *)fromVal->addr, "all"))
      val = XvicALL;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRWorkProcPolicy);
      return FALSE;
   }
   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(unsigned char)) {
         toVal->size = sizeof(unsigned char);
         return FALSE;
      }
      *(unsigned char *)(toVal->addr) = val;
   }
   else
      toVal->addr = (XtPointer)&val;
   toVal->size = sizeof(unsigned char);
   return TRUE;
}

/************************************************************************/
/* RegisterConverters							*/
/*									*/
/* Registers all type converters for this widget.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
RegisterConverters()
#else
RegisterConverters(void)
#endif /* _NO_PROTO */
{
   static Boolean firstTime = TRUE;

   if (!firstTime)
      return;
   firstTime = FALSE;

   XtSetTypeConverter(XtRString,
			XvicRColormapPolicy,
			CvtStringToColormapPolicy,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRConstrainPan,
			CvtStringToConstrainPan,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRDataSavePolicy,
			CvtStringToDataSavePolicy,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRDataType,
			CvtStringToDataType,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRDitherMode,
			CvtStringToDitherMode,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRDouble,
			CvtStringToDouble,
			NULL, 0, XtCacheNone, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRImageMode,
			CvtStringToImageMode,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRLutType,
			CvtStringToLutType,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRStretchPolicy,
			CvtStringToStretchPolicy,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRVisualType,
			CvtStringToVisualType,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRWorkProcPolicy,
			CvtStringToWorkProcPolicy,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
}

/************************************************************************/

