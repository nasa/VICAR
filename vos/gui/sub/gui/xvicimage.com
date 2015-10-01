$!****************************************************************************
$!
$! Build proc for MIPL module xvicimage
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:48
$!
$! Execute by entering:		$ @xvicimage
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module xvicimage ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvicimage.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xvicimage.imake") .nes. ""
$   then
$      vimake xvicimage
$      purge xvicimage.bld
$   else
$      if F$SEARCH("xvicimage.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvicimage
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvicimage.bld "STD"
$   else
$      @xvicimage.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvicimage.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvicimage.com -mixed -
	-s XvicBasicImage.c XvicCopyRaw.c XvicImage.c XvicImageOverlay.c -
	   XvicRegion.c XvicStringCursor.c XvicCopyRawCall_zoom.h -
	   XvicCopyRawFn_zoom.h XvicCopyRawFn_1band.h XvicCopyRawFn_3band.h -
	   XvicCopyRaw_bw.h XvicCopyRaw_color.h XvicCopyRaw_name.h -
	-i xvicimage.imake -
	-o XvicImage.doc -
	-t Test_IW test_iw.c test_iw.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XvicBasicImage.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRaw.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImage.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "XvicImageP.h"
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include "XvicImageOverlay.h"
#include <stdlib.h>
#include <ctype.h>

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
static void GetValuesHook();
static void Initialize();
static XtGeometryResult QueryGeometry();
static void Realize();
static void Resize();
static Boolean SetValues();

/* Primitive Methods */

/* BasicImage Methods */

static void ExposeOverlay();
static void MoveOverlay();
static void ClearOverlay();
static void FreeGrColormap();
static void InstallColormap();
static void SetUpGrColormap();

/* Action Procedures */

static void CursorModeAction();
static void InputAction();
static void MousePanAction();
static void MousePanStartAction();
static void MoveCursorAction();
static void MoveCursorMouseAction();
static void MoveCursorScreenAction();
static void PanEdgeAction();
static void PanHalfViewAction();
static void PanOneAction();

/* Callback procedures */

static void HScrollCallback();
static void Unrealize();
static void VScrollCallback();

/* Event handlers */

static void MotionEventHandler();

/* Misc */

static XvicID AddArc();
static XvicID AddArcs();
static XvicID AddLines();
static XvicID AddRectangle();
static XvicID AddRectangles();
static XvicID AddString();
static XvicID AddString16();
static Boolean AllocOverlayColor();
static Boolean AllocPrivateColor();
static Boolean AllocSystemColor();
static _XvicRect *BoundingRect();
static void CalcStringBounds();
static void CallCursorCallback();
static void CallPanCallback();
static Boolean CheckValidGrColor();
static Boolean CheckValidGrGC();
static void ClearOverlayRegion();
static void ConfigureScrollbars();
static void CreateGrColorDitherPattern();
static void CreateOverlayWidget();
static void CreateScrollbars();
static void CreateXCursor();
static void DeallocGrColor();
static void DestroyScrollbars();
static void DrawObject();
static void DrawPlantedCursor();
static void ErasePlantedCursor();
static void FatalError();
static void FreeGrColorTile();
static void FreeOneGrGC();
static void FreeOneGrObject();
static _XvicRegion *GetBoundingRgnForGC();
static _XvicRegion *GetBoundingRgnForID();
static void GetClosestColor();
static Boolean GetCursorSizeFromFont();
static void GetFloatingCursorLoc();
static GC GetGrXgc();
static void GetHWOverlayGC();
static void GetHWOverlayVisual();
static void GetOneGrColor();
static XvicID GetGrID();
static Boolean GetXYFromEvent();
static void MaskPixmap();
static void MirrorSystemColor();
static void MoveCursor();
static void MoveOneObject();
static GrObject *NewObject();
static void RepaintOverlayRegion();
static void SetMotionEventHandler();
static void SetUpGraphicsGC();
static void SetXCursor();
static void WarningMsg();

/* Resource Converters */

static void RegisterConverters();

#else

/* Core Methods */

static void ClassInitialize(void);
static void ClassPartInitialize(
		WidgetClass wc);
static void Destroy(
		Widget w);
static void GetValuesHook(
		Widget w,
		ArgList args,
		Cardinal *num_args);
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
static void Resize(
		Widget w);
static Boolean SetValues(
		Widget current,
		Widget request,
		Widget set,
		ArgList args,
		Cardinal *num_args);

/* Primitive Methods */

/* BasicImage Methods */

static void ExposeOverlay(
		Widget w,
		_XvicRegion *expose_rgn,
		_XvicRect *expose_bounds);
static void MoveOverlay(
		Widget w,
		int x_src,
		int y_src,
		int width,
		int height,
		int x_dest,
		int y_dest);
static void ClearOverlay(
		Widget w,
		int x,
		int y,
		int width,
		int height);
static void FreeGrColormap(
		Widget w);
static void InstallColormap(
		Widget w);
static void SetUpGrColormap(
		Widget w);

/* Action Procedures */

static void CursorModeAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void InputAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void MousePanAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void MousePanStartAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void MoveCursorAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void MoveCursorMouseAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void MoveCursorScreenAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void PanEdgeAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void PanHalfViewAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);
static void PanOneAction(
		Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params);

/* Callback procedures */

static void HScrollCallback(
		Widget w,
		XtPointer client_data,
		XtPointer call_data);
static void Unrealize(
		Widget w,
		XtPointer client_data,
		XtPointer call_data);
static void VScrollCallback(
		Widget w,
		XtPointer client_data,
		XtPointer call_data);

/* Event handlers */

static void MotionEventHandler(
		Widget w,
		XtPointer client_data,
		XEvent *event,
		Boolean *continue_to_dispatch);

/* Misc */

static XvicID AddArc(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		double x,
		double y,
		double width,
		double height,
		int angle1,
		int angle2,
		GrType type);
static XvicID AddArcs(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		XvicArc *arcs,
		int narcs,
		GrType type);
static XvicID AddLines(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		XvicPoint *points,
		int npoints,
		int shape,
		int mode,
		GrType type);
static XvicID AddRectangle(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		double x,
		double y,
		double width,
		double height,
		GrType type);
static XvicID AddRectangles(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		XvicRectangle *rectangles,
		int nrectangles,
		GrType type);
static XvicID AddString(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor fg,
		XvicColor color,
		double x,
		double y,
		char *string,
		int length,
		int justify,
		GrType type);
static XvicID AddString16(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor fg,
		XvicColor color,
		double x,
		double y,
		XChar2b *string,
		int length,
		int justify,
		GrType type);
static Boolean AllocOverlayColor(
		XvicImageWidget iw,
		GrColor *color,
		XColor *cells_arg,
		Colormap cmap);
static Boolean AllocPrivateColor(
		XvicImageWidget iw,
		GrColor *color,
		XColor *cells_arg,
		Colormap cmap);
static Boolean AllocSystemColor(
		XvicImageWidget iw,
		GrColor *color,
		XColor *cells,
#if NeedWidePrototypes
		int lower_half);
#else
		Boolean lower_half);
#endif
static _XvicRect *BoundingRect(
		XvicImageWidget iw,
		GrObject *obj);
static void CalcStringBounds(
		XvicImageWidget iw,
		GrObject *obj);
static void CallCursorCallback(
		XvicImageWidget iw,
		double x,
		double y,
#if NeedWidePrototypes
		int force_off_screen);
#else
		Boolean force_off_screen);
#endif
static void CallPanCallback(
		XvicImageWidget iw);
static Boolean CheckValidGrColor(
		XvicImageWidget iw,
		XvicColor color);
static Boolean CheckValidGrGC(
		XvicImageWidget iw,
		XvicGC gr_gc,
		int print);
static void ClearOverlayRegion(
		XvicImageWidget iw,
		_XvicRegion *rgn);
static void ConfigureScrollbars(
		XvicImageWidget iw);
static void CreateGrColorDitherPattern(
		XvicImageWidget iw,
		GrColor *color);
static void CreateOverlayWidget(
		XvicImageWidget iw);
static void CreateScrollbars(
		XvicImageWidget iw);
static void CreateXCursor(
		XvicImageWidget iw);
static void DeallocGrColor(
		XvicImageWidget iw,
		GrColor *color);
static void DestroyScrollbars(
		XvicImageWidget iw);
static void DrawObject(
		XvicImageWidget iw,
		GrObject *obj,
		_XvicRegion *expose_rgn);
static void DrawPlantedCursor(
		XvicImageWidget iw);
static void ErasePlantedCursor(
		XvicImageWidget iw);
static void FatalError(
		XvicImageWidget iw,
		char *name,
		char *def);
static void FreeGrColorTile(
		XvicImageWidget iw,
		GrColor *color);
static void FreeOneGrGC(
		XvicImageWidget iw,
		GrGC *gr_gc);
static void FreeOneGrObject(
		XvicImageWidget iw,
		GrObject *object);
static _XvicRegion *GetBoundingRgnForGC(
		XvicImageWidget iw,
		XvicGC gc,
		_XvicRegion *rgn);
static _XvicRegion *GetBoundingRgnForID(
		XvicImageWidget iw,
		XvicID id,
		_XvicRegion *rgn);
static void GetClosestColor(
		XvicImageWidget iw,
		XColor *color,
		XColor *cells_arg,
		Colormap cmap,
		int cmap_size);
static Boolean GetCursorSizeFromFont(
		XvicImageWidget iw,
		XFontStruct *fs,
		unsigned int ch,
		int *width,
		int *height,
		unsigned int *hot_x,
		unsigned int *hot_y);
static void GetFloatingCursorLoc(
		XvicImageWidget iw,
		double *x,
		double *y);
static GC GetGrXgc(
		XvicImageWidget iw,
		Window win,
		XvicGC gr_gc,
		XvicColor color);
static void GetHWOverlayGC(
		XvicImageWidget iw);
static void GetHWOverlayVisual(
		XvicImageWidget iw);
static void GetOneGrColor(
		XvicImageWidget iw,
		GrColor *color,
		XColor *cells);
static XvicID GetGrID(
		XvicImageWidget iw,
		XvicID id);
static Boolean GetXYFromEvent(
		XvicImageWidget iw,
		Widget w,
		XEvent *event,
		int *x,
		int *y);
static void MaskPixmap(
		XvicImageWidget iw,
		Pixmap mask,
		Pixmap source,
		unsigned int width,
		unsigned int height);
static void MirrorSystemColor(
		XvicImageWidget iw,
		GrColor *color,
		XColor *cells,
		Colormap cmap);
static void MoveCursor(
		XvicImageWidget iw);
static void MoveOneObject(
		XvicImageWidget iw,
		GrObject *obj,
		double delta_x,
		double delta_y);
static GrObject *NewObject(
		XvicImageWidget iw,
		XvicID id,
		XvicGC gc,
		XvicColor color,
		int size);
static void RepaintOverlayRegion(
		XvicImageWidget iw,
		_XvicRegion *rgn);
static void SetMotionEventHandler(
		XvicImageWidget iw);
static void SetUpGraphicsGC(
		XvicImageWidget iw,
		GrColor *color,
		unsigned long *mask,
		XGCValues *values);
static void SetXCursor(
		XvicImageWidget iw);
static void WarningMsg(
		XvicImageWidget iw,
		char *name,
		char *def);

/* Resource Converters */

static void RegisterConverters(void);

#endif /* _NO_PROTO */

/************************************************************************/
/* Translation tables							*/
/************************************************************************/

/* Allowing Primitive to install its translations pretty much	*/
/* wipes out any use of the arrow keys.  So, we have to do our	*/
/* own.  For now, do nothing, and let the application deal with	*/
/* it.  We might want to install some default translations later*/ /*!!!!*/

/* static char defaultTranslations[] = ""; */

/************************************************************************/
/* Action List								*/
/************************************************************************/

static XtActionsRec ActionsList[] = {
	{ "CursorMode",		CursorModeAction },
	{ "Input",		InputAction },
	{ "MousePan",		MousePanAction },
	{ "MousePanStart",	MousePanStartAction },
	{ "MoveCursor",		MoveCursorAction },
	{ "MoveCursorMouse",	MoveCursorMouseAction },
	{ "MoveCursorScreen",	MoveCursorScreenAction },
	{ "PanEdge",		PanEdgeAction },
	{ "PanHalfView",	PanHalfViewAction },
	{ "PanOne",		PanOneAction }
};

/************************************************************************/
/* Resources								*/
/************************************************************************/

static XtResource resources[] =
{
    {
	XvicNcursor,
	XvicCCursor,
	XtRString,
	sizeof(String),
	XtOffsetOf(XvicImageRec, image.cursor),
	XtRImmediate,
	(XtPointer) DEFAULT_CURSOR
    },
    {
	XvicNcursorBackground,
	XvicCCursorBackground,
	XtRString,
	sizeof(String),
	XtOffsetOf(XvicImageRec, image.cursor_background),
	XtRImmediate,
	(XtPointer) "white"
    },
    {
	XvicNcursorCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicImageRec, image.cursor_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNcursorForeground,
	XvicCCursorForeground,
	XtRString,
	sizeof(String),
	XtOffsetOf(XvicImageRec, image.cursor_foreground),
	XtRImmediate,
	(XtPointer) "black"
    },
    {
	XvicNcursorMode,
	XvicCCursorMode,
	XvicRCursorMode,
	sizeof(unsigned char),
	XtOffsetOf(XvicImageRec, image.cursor_mode),
	XtRImmediate,
	(XtPointer) XvicFLOATING
    },
    {
	XvicNcursorX,
	XvicCCursorLoc,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicImageRec, image.cursor_x),
	XtRImmediate,
	(XtPointer) CURSOR_NO_LOC
    },
    {
	XvicNcursorXfp,
	XvicCCursorLocFp,
	XvicRDouble,
	sizeof(double),
	XtOffsetOf(XvicImageRec, image.cursor_x_fp),
	XtRString,
	(XtPointer) CURSOR_NO_LOC_STR
    },
    {
	XvicNcursorY,
	XvicCCursorLoc,
	XtRInt,
	sizeof(int),
	XtOffsetOf(XvicImageRec, image.cursor_y),
	XtRImmediate,
	(XtPointer) CURSOR_NO_LOC
    },
    {
	XvicNcursorYfp,
	XvicCCursorLocFp,
	XvicRDouble,
	sizeof(double),
	XtOffsetOf(XvicImageRec, image.cursor_y_fp),
	XtRString,
	(XtPointer) CURSOR_NO_LOC_STR
    },
    {
	XvicNenableHWOverlay,
	XvicCEnableHWOverlay,
	XtRBoolean,
	sizeof(Boolean),
	XtOffsetOf(XvicImageRec, image.enable_hw_overlay),
	XtRImmediate,
	(XtPointer) True
    },
    {
	XvicNinputCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicImageRec, image.input_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNpanCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicImageRec, image.pan_callback),
	XtRPointer,
	(XtPointer) NULL
    },
    {
	XvicNscrollBarDisplayPolicy,
	XvicCScrollBarDisplayPolicy,
	XvicRScrollBarDisplayPolicy,
	sizeof(unsigned char),
	XtOffsetOf(XvicImageRec, image.scrollbar_display_policy),
	XtRImmediate,
	(XtPointer) XvicAS_NEEDED
    },
    {
	XvicNtrackFloatingCursor,
	XvicCTrackFloatingCursor,
	XtRBoolean,
	sizeof(Boolean),
	XtOffsetOf(XvicImageRec, image.track_floating_cursor),
	XtRImmediate,
	(XtPointer) False
    },
    {
	XtNunrealizeCallback,
	XtCCallback,
	XtRCallback,
	sizeof(XtCallbackList),
	XtOffsetOf(XvicImageRec, image.unrealize_callback),
	XtRPointer,
	(XtPointer) NULL
    },

    /* Override resources for BasicImage */

    {
	XvicNconstrainPan,
	XvicCConstrainPan,
	XvicRConstrainPan,
	sizeof(unsigned char),
	XtOffsetOf(XvicImageRec, bim.constrain_pan),
	XtRImmediate,
	(XtPointer) XvicBOTH
    }
};

/************************************************************************/
/* Class record								*/
/************************************************************************/

externaldef(xvicimageclassrec)
		XvicImageClassRec xvicImageClassRec =
{
  { /* core_class record */
    /* superclass         */	(WidgetClass) &xvicBasicImageClassRec,
    /* class_name         */	"XvicImage",
    /* widget_size        */	sizeof(XvicImageRec),
    /* class_initialize   */	ClassInitialize,
    /* class_part_init    */	ClassPartInitialize,
    /* class_inited       */	FALSE,
    /* initialize         */	Initialize,
    /* initialize_hook    */	(XtArgsProc) NULL,
    /* realize            */	Realize,
    /* actions            */	ActionsList,
    /* num_actions        */    XtNumber(ActionsList),
    /* resources          */	resources,
    /* num_resources      */	XtNumber(resources),
    /* xrm_class          */	NULLQUARK,
    /* compress_motion    */	TRUE,
    /* compress_exposure  */	XtExposeNoCompress,
    /* compress_enterlv   */	TRUE,
    /* visible_interest   */	FALSE,
    /* destroy            */    Destroy,
    /* resize             */    Resize,
    /* expose             */    XtInheritExpose,
    /* set_values         */	SetValues,
    /* set_values_hook    */	(XtArgsFunc) NULL,
    /* set_values_almost  */	XtInheritSetValuesAlmost,
    /* get_values_hook    */	GetValuesHook,
    /* accept_focus       */	(XtAcceptFocusProc) NULL,
    /* version            */	XtVersion,
    /* callback_private   */	NULL,
    /* tm_table           */	NULL,
    /* query_geometry     */	QueryGeometry,
    /* display_accelerator*/	(XtStringProc) NULL,
    /* extension record   */	NULL,
  },
  { /* primitive_class record */
    /* border_highlight   */	XmInheritBorderHighlight,
    /* border_unhighlight */	XmInheritBorderUnhighlight,
    /* translations       */	XtInheritTranslations,
    /* arm_and_activate   */	NULL,
    /* syn_resources      */	NULL,
    /* num_syn_resources  */	0,
    /* extension          */	NULL,
  },
  { /* basic_image_class record */
    /* expose_overlay     */	ExposeOverlay,
    /* move_overlay       */	MoveOverlay,
    /* clear_overlay      */	ClearOverlay,
    /* release_gr_colors  */	FreeGrColormap,
    /* set_up_gr_colors   */	SetUpGrColormap,
    /* install_colormap   */	InstallColormap,
    /* extension          */	NULL,
  },
  { /* image_class record */
    /* extension          */	NULL,
  }
};

externaldef(xvicimagewidgetclass) WidgetClass xvicImageWidgetClass =
				(WidgetClass) &xvicImageClassRec;

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
   /* Nothing to do */
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
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int i;

   DestroyScrollbars(iw);

   FreeGrColormap((Widget)iw);

   FreeGrColorTile(iw, &iw->image.curs_fg);
   FreeGrColorTile(iw, &iw->image.curs_bg);
   for (i=0; i<iw->image.num_gr_colors; i++) {
      FreeGrColorTile(iw, &iw->image.gr_colors[i]);
   }
   if (iw->image.gr_colors)
      _XvicFree(biw, iw->image.gr_colors,
			iw->image.num_gr_colors * sizeof(GrColor));

   for (i=0; i<iw->image.num_gr_gc; i++)
      FreeOneGrGC(iw, &iw->image.gr_gc_list[i]);
   if (iw->image.gr_gc_list)
      _XvicFree(biw, iw->image.gr_gc_list,
			iw->image.num_gr_gc * sizeof(GrGC));

   for (i=0; i<iw->image.num_gr_objects; i++)
      FreeOneGrObject(iw, iw->image.gr_objects[i]);
   if (iw->image.gr_objects)
      _XvicFree(biw, iw->image.gr_objects,
			iw->image.num_gr_objects * sizeof(GrObject *));

   if (iw->image.cursor)
      _XvicFree(biw, iw->image.cursor, strlen(iw->image.cursor)+1);
   if (iw->image.cursor_foreground)
      _XvicFree(biw, iw->image.cursor_foreground,
		strlen(iw->image.cursor_foreground)+1);
   if (iw->image.cursor_background)
      _XvicFree(biw, iw->image.cursor_background,
		strlen(iw->image.cursor_background)+1);

   if (iw->image.curs_source)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_source);
   if (iw->image.curs_mask)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_mask);
   if (iw->image.x_cursor)
      XFreeCursor(XtDisplay((Widget)iw), iw->image.x_cursor);

   if (iw->image.curs_fg_gc)
      XFreeGC(XtDisplay((Widget)iw), iw->image.curs_fg_gc);
   if (iw->image.curs_bg_gc)
      XFreeGC(XtDisplay((Widget)iw), iw->image.curs_bg_gc);

   if (iw->image.overlay_widget) {
      if (iw->image.overlay_pan_gc)
         XFreeGC(XtDisplay(iw->image.overlay_widget), iw->image.overlay_pan_gc);
      XtDestroyWidget(iw->image.overlay_widget);
   }

}

/************************************************************************/
/* GetValuesHook method							*/
/*									*/
/* This method checks for cursorX, cursorY, cursorXfp, or cursorYfp.	*/
/* If the user requests	those values, we fill them in.  It is not	*/
/* practical to keep the widget instance updated, because we'd have to	*/
/* track motion events for no other reason.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetValuesHook(w, args, num_args)
   Widget w;
   ArgList args;
   Cardinal *num_args;
#else
GetValuesHook(
   Widget w,
   ArgList args,
   Cardinal *num_args)
#endif
{
   XvicImageWidget iw = (XvicImageWidget) w;
   int i;
   double x, y;
   Boolean queried = False;

   for (i=0; i < *num_args; i++) {
      if (strcmp(args[i].name, XvicNcursorX) == 0) {
         if (iw->image.cursor_mode == XvicPLANTED)
            *((int *)args[i].value) = ROUND(iw->image.plant_curs_x);
         else {
            if (!queried) {
               GetFloatingCursorLoc(iw, &x, &y);
               queried = True;
            }
            *((int *)args[i].value) = ROUND(x);
         }
      }
      else if (strcmp(args[i].name, XvicNcursorY) == 0) {
         if (iw->image.cursor_mode == XvicPLANTED)
            *((int *)args[i].value) = ROUND(iw->image.plant_curs_y);
         else {
            if (!queried) {
               GetFloatingCursorLoc(iw, &x, &y);
               queried = True;
            }
            *((int *)args[i].value) = ROUND(y);
         }
      }
      else if (strcmp(args[i].name, XvicNcursorXfp) == 0) {
         if (iw->image.cursor_mode == XvicPLANTED)
            *((double *)args[i].value) = iw->image.plant_curs_x;
         else {
            if (!queried) {
               GetFloatingCursorLoc(iw, &x, &y);
               queried = True;
            }
            *((double *)args[i].value) = x;
         }
      }
      else if (strcmp(args[i].name, XvicNcursorYfp) == 0) {
         if (iw->image.cursor_mode == XvicPLANTED)
            *((double *)args[i].value) = iw->image.plant_curs_y;
         else {
            if (!queried) {
               GetFloatingCursorLoc(iw, &x, &y);
               queried = True;
            }
            *((double *)args[i].value) = y;
         }
      }
   }
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
   XvicImageWidget iw = (XvicImageWidget) new_w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int i;
   char *ptr;
   XGCValues GCvalues;

   iw->image.h_scrollbar = NULL;
   iw->image.v_scrollbar = NULL;
   iw->image.hsb_managed = FALSE;
   iw->image.vsb_managed = FALSE;
   iw->image.x_mouse_scr_pan = 0;
   iw->image.y_mouse_scr_pan = 0;
   iw->image.in_scrollbar = FALSE;

   iw->image.gr_colors = NULL;
   iw->image.num_gr_colors = 0;

   iw->image.gr_gc_list = NULL;
   iw->image.num_gr_gc = 0;
   /* Ensure that NULL for GC will give us this default */
   GCvalues.line_width = 1;
   XvicImageCreateGC((Widget)iw, GCLineWidth, &GCvalues);

   iw->image.gr_objects = NULL;
   iw->image.num_gr_objects = 0;
   iw->image.max_gr_id = 0;

   iw->image.curs_source = None;
   iw->image.curs_mask = None;
   iw->image.x_cursor = None;
   iw->image.plant_curs_x = 0.0;
   iw->image.plant_curs_y = 0.0;
   iw->image.plant_set = False;
   iw->image.erasing_cursor = False;
   iw->image.curs_fg_gc = NULL;
   iw->image.curs_bg_gc = NULL;

   iw->image.curs_fg.active = False;
   iw->image.curs_fg.gc_tile = None;
   iw->image.curs_fg.width = 0;
   iw->image.curs_fg.height = 0;
   iw->image.curs_fg.alloc_def = False;
   iw->image.curs_fg.alloc_pvt = False;

   iw->image.curs_bg.active = False;
   iw->image.curs_bg.gc_tile = None;
   iw->image.curs_bg.width = 0;
   iw->image.curs_bg.height = 0;
   iw->image.curs_bg.alloc_def = False;
   iw->image.curs_bg.alloc_pvt = False;

   iw->image.overlay_widget = NULL;
   iw->image.overlay_visual = NULL;
   iw->image.overlay_pan_gc = NULL;

   /* Zero out reference counts for colormap allocation */

   for (i=0; i<CMAP_SIZE_MAX; i++) {
      iw->image.default_cmap_refcnt[i] = 0;
      iw->image.private_cmap_refcnt[i] = 0;
   }

   /* Check the parent to see if it's a ScrolledWindow subclass.  If	*/
   /* so, create the scrollbars and set up all the scrolling stuff.	*/
   /* NOTE: The ScrolledWindow really needs to be in APPLICATION_DEFINED*/
   /* mode, since that's how the Pan resourecs in BasicImage work.	*/
   /* If the ScrolledWindow is AUTOMATIC, we pretend it's not there	*/
   /* (it's the user's problem to figure out what it means then).	*/
   /* We actually use visualPolicy instead of scrollingPolicy because	*/
   /* that's what XmList consistently uses.				*/

   if (!(XmIsScrolledWindow(iw->core.parent)) ||
       (XmIsScrolledWindow(iw->core.parent) && 
        (((XmScrolledWindowWidget)(iw->core.parent))->swindow.VisualPolicy ==
			XmCONSTANT))) {
      iw->image.scroll_win = NULL;
      iw->image.scrollbar_display_policy = XvicNEVER;
   }
   else {			/* Play with scrollbars */

      iw->image.scroll_win = (XmScrolledWindowWidget) iw->core.parent;

      /* Set the SW areas, just in case it doesn't happen in ConfigureSB */

      XmScrolledWindowSetAreas((Widget)iw->image.scroll_win,
		NULL, NULL, (Widget)iw);

      /* Create and configure the scrollbars (if they'll ever be needed) */

      ConfigureScrollbars(iw);

      /* A possibility of scrollbars should force constrainPan to	*/
      /* XvicBOTH, but that's hard to do here since we shouldn't	*/
      /* call SetValues from Initialize.  So, if the user really	*/
      /* wants to screw himself up, who am I to judge?			*/

   }

   /* Check for and set up the HW overlay, if available */

   CreateOverlayWidget(iw);

   /* Make a copy of the cursor color strings */

   ptr = _XvicMalloc(biw, strlen(iw->image.cursor_background)+1);
   strcpy(ptr, iw->image.cursor_background);
   iw->image.cursor_background = ptr;
   ptr = _XvicMalloc(biw, strlen(iw->image.cursor_foreground)+1);
   strcpy(ptr, iw->image.cursor_foreground);
   iw->image.cursor_foreground = ptr;

   /* Set up the cursor.  First, make a copy of it to save */

   ptr = _XvicMalloc(biw, strlen(iw->image.cursor)+1);
   strcpy(ptr, iw->image.cursor);
   iw->image.cursor = ptr;

   SetMotionEventHandler(iw);

   /* Check the coordinates to see if they have been explicitly set. */

   if (iw->image.cursor_x != CURSOR_NO_LOC ||
       iw->image.cursor_y != CURSOR_NO_LOC ||
       iw->image.cursor_x_fp != CURSOR_NO_LOC ||
       iw->image.cursor_y_fp != CURSOR_NO_LOC) {
      MoveCursor(iw);	/* Safe now because the window isn't realized yet */
   }

   /* Now, set up the cursor shape to what's given */
   /* If the cursor is planted, the expose will draw it */

   _XvicImageSetStringCursor(iw, iw->image.cursor);

   /* Add the Unrealize handler for the overlay */

   XtAddCallback((Widget)iw, XtNunrealizeCallback, Unrealize, NULL);
}

/************************************************************************/
/* QueryGeometry method							*/
/*									*/
/* Look at the proposed geometry and add/delete scrollbars as needed.	*/
/* In spite of this, we'd still really rather not change sizes (see	*/
/* BasicImage's QueryGeometry for explanation), so we always return	*/
/* No, unless of course the requested size happens to match the current	*/
/* size, in which case we return Yes.					*/
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
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int new_width, new_height;
   int work_width, work_height;		/* zoomed size of image */
   int sb_size;
   int ht;			/* highlight_thickness for bug workaround */
   Boolean useHSB = FALSE;
   Boolean useVSB = FALSE;
   Widget manage_list[2], unmanage_list[2];
   int manage_count, unmanage_count;

   /* If the request mode is 0, we're apparently just being queried	*/
   /* for our preferred size.  Set it to the current size and return	*/
   /* XtGeometryNo (meaning I'd rather not change), unless we happen	*/
   /* to be the right size.  If we're not scrollable, or don't have	*/
   /* scrollbars, also return the same.					*/

   if (proposed->request_mode == 0 ||
       (iw->image.scroll_win == NULL ||
        (iw->image.h_scrollbar==NULL && iw->image.v_scrollbar==NULL)) ||
       (iw->image.scrollbar_display_policy == XvicNEVER)) {
      answer->width = w->core.width;
      answer->height = w->core.height;
      answer->request_mode = CWWidth | CWHeight;
      if (((proposed->request_mode & (CWWidth | CWHeight)) ==
				     (CWWidth | CWHeight)) &&
		proposed->width == answer->width &&
		proposed->height == answer->height)
         return XtGeometryYes;
      return XtGeometryNo;
   }

   /* We're being queried from a scrolled window.  Look at the		*/
   /* dimensions and manage/unmanage the scrollbars according to the	*/
   /* new size.  The scrolled window will take care of the actual	*/
   /* sizing.								*/

   if (proposed->request_mode & CWWidth)
      new_width = proposed->width;
   else
      new_width = iw->core.width;

   if (proposed->request_mode & CWHeight)
      new_height = proposed->height;
   else
      new_height = iw->core.height;

   work_width = X2_Img2Dpy(iw->bim.image_width) + 2*iw->bim.x_dpy_off;
   work_height = Y2_Img2Dpy(iw->bim.image_height) + 2*iw->bim.y_dpy_off;

   /* If we're too small now, or we have XvicSTATIC scrollbars, it's easy */

   if (((new_width < work_width) && (new_height < work_height)) ||
       iw->image.scrollbar_display_policy == XvicSTATIC) {
      useHSB = TRUE;
      useVSB = TRUE;
   }
   else {

      /* Here it gets interesting.  Check for a definite need for a	*/
      /* horizontal scrollbar, and set the available height accordingly.*/
      /* Then check to see if a vertical scrollbar is needed.  If so,	*/
      /* and the hsb wasn't needed before, check to see if it's needed	*/
      /* now.  That is actually the end of the chain logically, if you	*/
      /* work it out you see that adding the hsb the second time around	*/
      /* can't affect the need for the vsb.  Whew.			*/
      /* By the way, if the scrollbar hasn't been created (shouldn't	*/
      /* happen at this point), then don't turn it on!			*/

      /* Note: The use of "highlight_thickness" below is a BUG in	*/
      /* ScrolledWindow.  (It's marked as a "cool undocumented feature"	*/
      /* in the source, but it doesn't work right!).  The highlight is	*/
      /* taken into account in the Scrollbar widget itself, so the	*/
      /* core size (plus border which is outside the window) should be	*/
      /* sufficient.  However, ScrolledWindow, in its infinite wisdom,	*/
      /* adds in the highlight when it calculates layout - so with a	*/
      /* big highlight, you end up with an even *bigger* space between	*/
      /* the work window and the scrollbars!  Sigh.  Furthermore, this	*/
      /* only happens if traversal is on - if it's off, it sets the	*/
      /* variables to 0 so it doesn't make this "adjustment".		*/
      /* Even this doesn't work quite right because ScrolledWindow	*/
      /* doesn't offset the work window properly to line up with the	*/
      /* scrollbars when traversal is off.  Oh well, this should be	*/
      /* the kind of thing nobody ever notices.  Double sigh.		*/

      if (new_width < work_width && iw->image.h_scrollbar) {
         useHSB = TRUE;
         ht = 0;	/* Compensate for ScrolledWindow bug described above */
         if (iw->image.h_scrollbar->primitive.traversal_on)
            ht = iw->image.h_scrollbar->primitive.highlight_thickness;
         sb_size = iw->image.h_scrollbar->core.height +
		   2 * iw->core.border_width +	/* yes,border_width is square */
		   2 * ht +
		   iw->image.scroll_win->swindow.pad;
         if (sb_size >= new_height)
            new_height = 1;
         else
            new_height -= sb_size;
      }

      if (new_height < work_height && iw->image.v_scrollbar) {
         useVSB = TRUE;
         ht = 0;	/* Compensate for ScrolledWindow bug described above */
         if (iw->image.v_scrollbar->primitive.traversal_on)
            ht = iw->image.v_scrollbar->primitive.highlight_thickness;
         sb_size = iw->image.v_scrollbar->core.width +
		   2 * iw->core.border_width +
		   2 * ht +
		   iw->image.scroll_win->swindow.pad;
         if (sb_size >= new_width)
            new_width = 1;
         else
            new_width -= sb_size;
      }

      if (new_width < work_width && iw->image.h_scrollbar)
         useHSB = TRUE;
   }

   /* Since Xt(Un)ManageChild() will cause a reconfig which will call */
   /* this routine again, defer all management to happen at once.     */

   manage_count = 0;
   unmanage_count = 0;

   if (iw->image.h_scrollbar) {
      if (useHSB) {
         if (!iw->image.hsb_managed) {
            iw->image.hsb_managed = TRUE;
            manage_list[manage_count++] = (Widget) iw->image.h_scrollbar;
         }
      }
      else {
         if (iw->image.hsb_managed) {
            iw->image.hsb_managed = FALSE;
            unmanage_list[unmanage_count++] = (Widget) iw->image.h_scrollbar;
         }
      }
   }

   if (iw->image.v_scrollbar) {
      if (useVSB) {
         if (!iw->image.vsb_managed) {
            iw->image.vsb_managed = TRUE;
            manage_list[manage_count++] = (Widget) iw->image.v_scrollbar;
         }
      }
      else {
         if (iw->image.vsb_managed) {
            iw->image.vsb_managed = FALSE;
            unmanage_list[unmanage_count++] = (Widget) iw->image.v_scrollbar;
         }
      }
   }

   if (manage_count)
      XtManageChildren(manage_list, manage_count);
   if (unmanage_count)
      XtUnmanageChildren(unmanage_list, unmanage_count);

   /* We really don't want to change the size (see BasicImage's		*/
   /* QueryGeometry for details), so try to return No, even though we	*/
   /* know it'll be ignored.						*/

   answer->request_mode = CWHeight | CWWidth;
   answer->width = iw->core.width;
   answer->height = iw->core.height;
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
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidgetClass superclass =
			(XvicBasicImageWidgetClass)xvicBasicImageWidgetClass;
   int i;

   /* Call the BasicImage's realize method first */

   (*superclass->core_class.realize)(w, value_mask, attributes);

   /* Realize the overlay widget, if present.  It was unrealized by	*/
   /* our own Unrealize callback.  If it's there, we must install the	*/
   /* colormaps again so the overlay's cmap gets in.			*/

   if (iw->image.overlay_widget) {
      XtRealizeWidget(iw->image.overlay_widget);
      XtMapWidget(iw->image.overlay_widget);
      GetHWOverlayGC(iw);
      CALL_InstallColormap(iw, ((Widget)iw));

   }

   /* Set up any graphics colors */

   SetUpGrColormap((Widget)iw);

   /* Make sure we re-create the cursor GC's */

   iw->image.plant_set = False;

   /* Now set up the X cursor, if needed. */
   /* Planted cursor will get drawn during the expose. */

   SetXCursor(iw);

   /* Since the window may have changed, destroy all saved graphics GCs	*/

   for (i=0; i<iw->image.num_gr_gc; i++) {
      if (iw->image.gr_gc_list[i].active && iw->image.gr_gc_list[i].gc) {
         XFreeGC(XtDisplay((Widget)iw), iw->image.gr_gc_list[i].gc);
         iw->image.gr_gc_list[i].gc = NULL;
      }
   }

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
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidgetClass superclass =
			(XvicBasicImageWidgetClass)xvicBasicImageWidgetClass;

   /* Call the BasicImage's resize method first */

   (*superclass->core_class.resize)(w);

   /* Resize the overlay widget if necessary */

   if (iw->image.overlay_widget)
      XtResizeWidget(iw->image.overlay_widget,
		MAX(iw->bim.view_width,1), MAX(iw->bim.view_height,1), 0);

   /* Now reconfigure the scrollbars */

   ConfigureScrollbars(iw);

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
   XvicImageWidget old = (XvicImageWidget) current;
   XvicImageWidget iw = (XvicImageWidget) set;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   char *ptr;
   double curs_x, curs_y;

   /* If anything changed that could affect the scrollbars, reconfigure	*/
   /* them.								*/

   if (	old->image.scrollbar_display_policy !=
		iw->image.scrollbar_display_policy ||
	old->bim.constrain_pan != iw->bim.constrain_pan ||
	old->bim.image_width != iw->bim.image_width ||
	old->bim.image_height != iw->bim.image_height ||
	old->bim.view_width != iw->bim.view_width ||
	old->bim.view_height != iw->bim.view_height ||
	old->bim.x_subpixel_pan != iw->bim.x_subpixel_pan ||
	old->bim.y_subpixel_pan != iw->bim.y_subpixel_pan ||
	old->bim.x_zoom_in != iw->bim.x_zoom_in ||
	old->bim.x_zoom_out != iw->bim.x_zoom_out ||
	old->bim.y_zoom_in != iw->bim.y_zoom_in ||
	old->bim.y_zoom_out != iw->bim.y_zoom_out ||
	old->bim.x_prezoom_in != iw->bim.x_prezoom_in ||
	old->bim.x_prezoom_out != iw->bim.x_prezoom_out ||
	old->bim.y_prezoom_in != iw->bim.y_prezoom_in ||
	old->bim.y_prezoom_out != iw->bim.y_prezoom_out ||
	old->bim.x_presubpixel_pan != iw->bim.x_presubpixel_pan ||
	old->bim.y_presubpixel_pan != iw->bim.y_presubpixel_pan)

      ConfigureScrollbars(iw);

   /* If the pan values changed, update the scrollbar locations.  Note	*/
   /* that we care about x_pan here, not x_screen_pan, because the	*/
   /* scrollbars are actually maintained in Img coordinates.		*/
   /* The *_managed variables are only set if the scrollbar exists.	*/
   /* We only do this if we're not called from the scrollbar callback,	*/
   /* else the scrollbar flashes in a really bizarre way.		*/

   if (!iw->image.in_scrollbar) {
      if (	old->bim.x_pan != iw->bim.x_pan) {
         if (iw->image.hsb_managed)
            XtVaSetValues((Widget)iw->image.h_scrollbar,
			XmNvalue, iw->bim.x_pan, NULL);
      }

      if (	old->bim.y_pan != iw->bim.y_pan) {
         if (iw->image.vsb_managed)
            XtVaSetValues((Widget)iw->image.v_scrollbar,
			XmNvalue, iw->bim.y_pan, NULL);
      }
   }

   /* If the border changed, move the overlay widget */

   if (iw->image.overlay_widget &&
       (old->bim.x_dpy_off != iw->bim.x_dpy_off ||
        old->bim.y_dpy_off != iw->bim.y_dpy_off))
      XtMoveWidget(iw->image.overlay_widget,
		iw->bim.x_dpy_off, iw->bim.y_dpy_off);

   /* Set up the cursor.  We can simply compare pointers because we	*/
   /* always make a local copy of the string and there's no way the app	*/
   /* can re-use our pointer.  If the app explicitly sets the value	*/
   /* to the same as what it already is, we just set the cursor again -	*/
   /* a small price to pay for not doing another string compare in	*/
   /* every SetValues call.  We also completely reset the cursor if the	*/
   /* colors change.  A little inefficient, maybe, but this is not	*/
   /* something that happens often.					*/

   if (old->image.cursor != iw->image.cursor ||
       old->image.cursor_foreground != iw->image.cursor_foreground ||
       old->image.cursor_background != iw->image.cursor_background) {

      /* Copy the string to locally-allocated memory */

      if (old->image.cursor != iw->image.cursor) {
         if (old->image.cursor)
            _XvicFree(biw, old->image.cursor, strlen(old->image.cursor)+1);
         ptr = _XvicMalloc(biw, strlen(iw->image.cursor)+1);
         strcpy(ptr, iw->image.cursor);
         iw->image.cursor = ptr;
      }
      if (old->image.cursor_foreground != iw->image.cursor_foreground) {
         if (old->image.cursor_foreground)
            _XvicFree(biw, old->image.cursor_foreground,
			strlen(old->image.cursor_foreground)+1);
         ptr = _XvicMalloc(biw, strlen(iw->image.cursor_foreground)+1);
         strcpy(ptr, iw->image.cursor_foreground);
         iw->image.cursor_foreground = ptr;
      }
      if (old->image.cursor_background != iw->image.cursor_background) {
         if (old->image.cursor_background)
            _XvicFree(biw, old->image.cursor_background,
			strlen(old->image.cursor_background)+1);
         ptr = _XvicMalloc(biw, strlen(iw->image.cursor_background)+1);
         strcpy(ptr, iw->image.cursor_background);
         iw->image.cursor_background = ptr;
      }

      /* Now, set up the cursor to what's given */

      _XvicImageSetStringCursor(iw, iw->image.cursor);
   }

   if (old->image.cursor_mode != iw->image.cursor_mode) {
      SetXCursor(iw);
      SetMotionEventHandler(iw);
      if (iw->image.cursor_mode == XvicPLANTED) {	/* Planted now */
         GetFloatingCursorLoc(iw,
		&iw->image.plant_curs_x, &iw->image.plant_curs_y);
         DrawPlantedCursor(iw);
         CallCursorCallback(iw, iw->image.plant_curs_x, iw->image.plant_curs_y,
			    False);
      }
      else {						/* Floating now */
         ErasePlantedCursor(iw);
         GetFloatingCursorLoc(iw, &curs_x, &curs_y);
         CallCursorCallback(iw, curs_x, curs_y, False);
      }
   }

   if (old->image.track_floating_cursor != iw->image.track_floating_cursor)
      SetMotionEventHandler(iw);

   if (iw->image.cursor_x != CURSOR_NO_LOC ||
       iw->image.cursor_y != CURSOR_NO_LOC ||
       iw->image.cursor_x_fp != CURSOR_NO_LOC ||
       iw->image.cursor_y_fp != CURSOR_NO_LOC)
      MoveCursor(iw);

   return FALSE;

}

/************************************************************************/
/************************************************************************/
/*  B A S I C I M A G E   M E T H O D S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* ClearOverlay								*/
/*									*/
/* Clears a section of the hardware overlay, if it is in use.  The	*/
/* coordinates are Screen coords.  The caller is responsible for	*/
/* redrawing anything that might be needed.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ClearOverlay(w, x, y, width, height)
   Widget w;
   int x;
   int y;
   int width;
   int height;
#else
ClearOverlay(
   Widget w,
   int x,
   int y,
   int width,
   int height)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   if (!iw->image.overlay_widget)
      return;

   if (!XtIsRealized(iw->image.overlay_widget))
      return;

   XClearArea(XtDisplay(iw->image.overlay_widget),
		XtWindow(iw->image.overlay_widget),
		x, y, width, height, False);
}

/************************************************************************/
/* ExposeOverlay							*/
/*									*/
/* Expose a section of the overlay.  The region (in Img coordinates)	*/
/* can be used if a tight clip area is needed, or the bounds (in Dpy	*/
/* coordinates) can be used if a larger clip area is okay.  The image	*/
/* has already been drawn at this point.  Note that region may be NULL,	*/
/* in which case only the bounds describe the area.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ExposeOverlay(w, expose_rgn, expose_bounds)
   Widget w;
   _XvicRegion *expose_rgn;
   _XvicRect *expose_bounds;
#else
ExposeOverlay(
   Widget w,
   _XvicRegion *expose_rgn,
   _XvicRect *expose_bounds)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   _XvicRect rect, dummy_rect;
   _XvicRegion *rgn;
   int i;
   GrObject *obj;

   if (expose_rgn == NULL) {
      rect.x1 = X_Dpy2Img(expose_bounds->x1);
      rect.y1 = Y_Dpy2Img(expose_bounds->y1);
      rect.x2 = X_Dpy2Img(expose_bounds->x2);
      rect.y2 = Y_Dpy2Img(expose_bounds->y2);

      do {
         rgn = _XvicRegionCreateRect(&rect);
         if (!rgn) _XvicMemoryPanic(biw);
      } while (rgn == NULL);
   }
   else
      rgn = expose_rgn;

   /* If this is a hardware overlay, erase the area in question before	*/
   /* redrawing.  We don't need to do this for software overlay because	*/
   /* redrawing the image will take care of it.				*/

   if (iw->image.overlay_widget && XtIsRealized(iw->image.overlay_widget))
      ClearOverlayRegion(iw, rgn);

   /* Draw any graphics objects that are in the area */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      obj = iw->image.gr_objects[i];
      if (obj) {
         if (_XvicRegionHasRect(BoundingRect(iw, obj), rgn)) {
            DrawObject(iw, obj, rgn);
         }
      }
   }

   /* If the cursor is planted, and we're not trying to erase it right	*/
   /* now, and it at least partially covers this area, then redraw it.	*/

   if (iw->image.cursor_mode == XvicPLANTED && !iw->image.erasing_cursor) {
      rect.x1 = XC_Img2Dpy(iw->image.plant_curs_x) - iw->image.curs_hot_x;
      rect.y1 = YC_Img2Dpy(iw->image.plant_curs_y) - iw->image.curs_hot_y;
      rect.x2 = rect.x1 + iw->image.curs_width - 1;
      rect.y2 = rect.y1 + iw->image.curs_width - 1;
      if (_XvicRectIntersectRect(expose_bounds, &rect, &dummy_rect))
         DrawPlantedCursor(iw);
   }

   if (expose_rgn == NULL)
      _XvicRegionDestroy(rgn);

}

/************************************************************************/
/* FreeGrColormap							*/
/*									*/
/* Frees up the graphics colors in the colormaps.  Also marks all	*/
/* GrColor structures as not allocated.  This takes a Widget as an	*/
/* argument so it can be called via the ReleaseGrColors class method	*/
/* but it must really be an ImageWidget.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeGrColormap(w)
   Widget w;
#else
FreeGrColormap(
   Widget w)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   Display *dpy;
   unsigned long cmap_list[100];
   int i, n;

   /* Free anything allocated from the system cmap */

   DPR(("freeing graphics colormap\n"));
   dpy = XtDisplay(iw);
   n = 0;
   for (i=0; i<CMAP_SIZE_MAX; i++) {
      while (iw->image.default_cmap_refcnt[i]) {
         iw->image.default_cmap_refcnt[i]--;
         cmap_list[n++] = i;
         if (n == 100) {
            XFreeColors(dpy, DefaultColormap(dpy,DefaultScreen(dpy)),
			cmap_list, n, 0);
            n = 0;
         }
      }
   }
   if (n != 0)
      XFreeColors(dpy, DefaultColormap(dpy,DefaultScreen(dpy)),
		cmap_list, n, 0);

   /* Zero out the ref counts for the private cmap (no freeing needed) */

   for (i=0; i<CMAP_SIZE_MAX; i++)
      iw->image.private_cmap_refcnt[i] = 0;

   /* Mark as unallocated all graphics colors using the cmap */

   iw->image.curs_fg.alloc_def = False;
   iw->image.curs_fg.alloc_pvt = False;
   iw->image.curs_bg.alloc_def = False;
   iw->image.curs_bg.alloc_pvt = False;
   iw->image.plant_set = False;

   for (i=0; i<iw->image.num_gr_colors; i++) {
      iw->image.gr_colors[i].alloc_def = False;
      iw->image.gr_colors[i].alloc_pvt = False;
   }
}

/************************************************************************/
/* InstallColormap							*/
/*									*/
/* Tells the window manager about the colormap so it will be installed	*/
/* at the appropriate times.  It also sets the X attribute on the	*/
/* window.  This overrides BasicImage's InstallColormap because we need	*/
/* to install the overlay's cmap as well.				*/
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
   XvicImageWidget iw = (XvicImageWidget) w;
   Widget wlist[3];
   Widget topLevel;
   int i;
 
   if (!XtIsRealized((Widget) iw))
      return;
 
   XSetWindowColormap(XtDisplay((Widget)iw), XtWindow((Widget)iw),
			iw->bim.colormap);
 
   if (iw->image.overlay_widget && XtIsRealized(iw->image.overlay_widget))
      XSetWindowColormap(XtDisplay(iw->image.overlay_widget),
			XtWindow(iw->image.overlay_widget),
			iw->image.overlay_colormap);

   /* Window list must be (widget,{overlay},top-level).  So, traverse	*/
   /* the widget tree backwards until we find the shell widget, and use	*/
   /* its window.							*/
 
   wlist[0] = (Widget) iw;
   i = 1;
   if (iw->image.overlay_widget) {
      wlist[1] = iw->image.overlay_widget;
      i = 2;
   }
   topLevel = (Widget) iw;
   while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
      topLevel = XtParent(topLevel);
   wlist[i] = topLevel;
 
   XtSetWMColormapWindows(topLevel, wlist, i+1);
}

/************************************************************************/
/* MoveOverlay								*/
/*									*/
/* If a hardware overlay is in use, this routine moves it for use with	*/
/* a pan.  All coordinates are Screen coordinates.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MoveOverlay(w, x_src, y_src, width, height, x_dest, y_dest)
   Widget w;
   int x_src;
   int y_src;
   int width;
   int height;
   int x_dest;
   int y_dest;
#else
MoveOverlay(
   Widget w,
   int x_src,
   int y_src,
   int width,
   int height,
   int x_dest,
   int y_dest)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   if (!iw->image.overlay_widget)
      return;

   if (!XtIsRealized(iw->image.overlay_widget))
      return;

   XCopyArea(XtDisplay(iw->image.overlay_widget),
	XtWindow(iw->image.overlay_widget), XtWindow(iw->image.overlay_widget),
	iw->image.overlay_pan_gc,
	x_src, y_src, width, height, x_dest, y_dest);
}

/************************************************************************/
/* SetUpGrColormap							*/
/*									*/
/* Sets up all the currently active graphics colors.  This takes a	*/
/* Widget as an argument so it can be called via the SetUpGrColors	*/
/* class method but it must really be an ImageWidget.			*/
/* It is assumed that the caller will take care of redrawing the	*/
/* graphics.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
SetUpGrColormap(w)
   Widget w;
#else
SetUpGrColormap(
   Widget w)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int i;
   XColor cells[CMAP_SIZE_MAX];

   if (iw->image.num_gr_colors == 0)
      return;

   if (iw->image.overlay_widget) {
      for (i=0; i<iw->image.overlay_cmap_size; i++)
         cells[i].pixel = i;
      XQueryColors(XtDisplay((Widget)iw), iw->image.overlay_colormap, cells,
			iw->image.overlay_cmap_size);
   }
   else {
      for (i=0; i<CMAP_SIZE; i++)
         cells[i].pixel = i;
      XQueryColors(XtDisplay(iw), biw->bim.colormap, cells, CMAP_SIZE);
   }

   GetOneGrColor(iw, &iw->image.curs_fg, cells);
   GetOneGrColor(iw, &iw->image.curs_bg, cells);
   iw->image.plant_set = False;		/* New colors, must re-do GC's */
   for (i=0; i<iw->image.num_gr_colors; i++)
      GetOneGrColor(iw, &iw->image.gr_colors[i], cells);

}

/************************************************************************/
/************************************************************************/
/*  P U B L I C   F U N C T I O N S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* XvicCreateImage							*/
/*									*/
/* Creates a ScrolledWindow parent (with APPLICATION_DEFINED set), then	*/
/* creates the Image widget itself.  The Image widget is not managed	*/
/* in order to be consistent with Motif style (although I think it	*/
/* should be).								*/
/************************************************************************/

Widget
#ifdef _NO_PROTO
XvicCreateImage(parent, name, args, argCount)
   Widget parent;
   char *name;
   ArgList args;
   Cardinal argCount;
#else
XvicCreateImage(
   Widget parent,
   char *name,
   ArgList args,
   Cardinal argCount)
#endif /* _NO_PROTO */
{
   char *scroll_name;
   int n;
   ArgList Args;
   Widget sw, iw;

   if (name) {
      scroll_name = (char *)malloc(strlen(name)+3);
      if (scroll_name == NULL) {
         XtAppWarningMsg(XtWidgetToApplicationContext(parent),
		"NoMemory", "XvicCreateImage", "XvicImageWidgetError",
		"Out of physical memory in XvicCreateImage",
		(String *)NULL, (Cardinal *)NULL);
         return NULL;
      }
      strcpy(scroll_name, name);
      strcat(scroll_name, "SW");
   }
   else
      scroll_name = "SW";

   Args = (ArgList) malloc((argCount+3) * sizeof(Arg));
   if (Args == NULL) {
      XtAppWarningMsg(XtWidgetToApplicationContext(parent),
		"NoMemory", "XvicCreateImage", "XvicImageWidgetError",
		"Out of physical memory in XvicCreateImage",
		(String *)NULL, (Cardinal *)NULL);
      if (name)
         free(scroll_name);
      return NULL;
   }
   for (n=0; n<argCount; n++) {
      Args[n].name = args[n].name;
      Args[n].value = args[n].value;
   }

   XtSetArg(Args[n], XmNscrollingPolicy, (XtArgVal) XmAPPLICATION_DEFINED); n++;
   XtSetArg(Args[n], XmNvisualPolicy, (XtArgVal) XmVARIABLE); n++;
   XtSetArg(Args[n], XmNscrollBarDisplayPolicy, (XtArgVal) XmSTATIC); n++;

   sw = XtCreateManagedWidget(scroll_name, xmScrolledWindowWidgetClass, parent,
			Args, n);
   if (name)
      free(scroll_name);
   free(Args);

   iw = XtCreateWidget(name, xvicImageWidgetClass, sw, args, argCount);

   XtAddCallback(iw, XtNdestroyCallback, _XmDestroyParentCallback, NULL);

   return iw;

}

/************************************************************************/
/* XvicImageChangeGC							*/
/*									*/
/* Changes an existing GrGC.						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageChangeGC(w, gc, valuemask, values)
   Widget w;
   XvicGC gc;
   unsigned long valuemask;
   XGCValues *values;
#else
XvicImageChangeGC(
   Widget w,
   XvicGC gc,
   unsigned long valuemask,
   XGCValues *values)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   _XvicRegion *rgn;
   int i;

   /* Make sure the GC is valid */

   if (!CheckValidGrGC(iw, gc, True))
      return;

   /* Save the old region */

   rgn = GetBoundingRgnForGC(iw, gc, NULL);

   /* Go through each relevant GC flag and set the saved GC values.	*/
   /* We must keep the XGCValues struct up-to-date in case we need to	*/
   /* re-create the X GC.  Anybody got a better way??			*/

   if (valuemask & GCArcMode)
      iw->image.gr_gc_list[gc].values.arc_mode = values->arc_mode;
   if (valuemask & GCCapStyle)
      iw->image.gr_gc_list[gc].values.cap_style = values->cap_style;
   if (valuemask & GCDashList)
      iw->image.gr_gc_list[gc].values.dashes = values->dashes;
   if (valuemask & GCDashOffset)
      iw->image.gr_gc_list[gc].values.dash_offset = values->dash_offset;
   if (valuemask & GCFillRule)
      iw->image.gr_gc_list[gc].values.fill_rule = values->fill_rule;
   if (valuemask & GCFont)
      iw->image.gr_gc_list[gc].values.font = values->font;
   if (valuemask & GCFunction)
      iw->image.gr_gc_list[gc].values.function = values->function;
   if (valuemask & GCJoinStyle)
      iw->image.gr_gc_list[gc].values.join_style = values->join_style;
   if (valuemask & GCLineStyle)
      iw->image.gr_gc_list[gc].values.line_style = values->line_style;
   if (valuemask & GCLineWidth)
      iw->image.gr_gc_list[gc].values.line_width = values->line_width;
   if (valuemask & GCPlaneMask)
      iw->image.gr_gc_list[gc].values.plane_mask = values->plane_mask;

   iw->image.gr_gc_list[gc].mask |= valuemask;

   if (iw->image.gr_gc_list[gc].gc)
      XChangeGC(XtDisplay((Widget)iw), iw->image.gr_gc_list[gc].gc,
			valuemask, values);

   /* Update any text objects with new extents if the font changed */

   if (valuemask & GCFont) {
      for (i=0; i<iw->image.num_gr_objects; i++) {
         if (iw->image.gr_objects[i]) {
            if (iw->image.gr_objects[i]->any.gc == gc) {
               switch (iw->image.gr_objects[i]->type) {
                  case GrImageString:
                  case GrImageString16:
                  case GrString:
                  case GrString16:
                  case GrText:
                  case GrText16:
                     CalcStringBounds(iw, iw->image.gr_objects[i]);
                     break;
                  default:
                     break;
               }
            }
         }
      }
   }

   /* Add in the new region, if different */

   rgn = GetBoundingRgnForGC(iw, gc, rgn);

   /* Now redisplay the area covered by the region. */

   RepaintOverlayRegion(iw, rgn);

   _XvicRegionDestroy(rgn);
}

/************************************************************************/
/* XvicImageCreateGC							*/
/*									*/
/* Creates a GrGC for use by the graphics routines.			*/
/************************************************************************/

XvicGC
#ifdef _NO_PROTO
XvicImageCreateGC(w, valuemask, values)
   Widget w;
   unsigned long valuemask;
   XGCValues *values;
#else
XvicImageCreateGC(
   Widget w,
   unsigned long valuemask,
   XGCValues *values)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int i, n;
   GrGC *tmp;

   /* Check to see if we can reuse an old GC */

   for (i=0; i<iw->image.num_gr_gc; i++) {
      if (!iw->image.gr_gc_list[i].active)		/* found one */
         break;
   }

   if (i == iw->image.num_gr_gc) {	/* none found, expand the table */

      n = iw->image.num_gr_gc + 10;
      tmp = _XvicMalloc(biw, n * sizeof(GrGC));
      if (iw->image.num_gr_gc > 0)
         memcpy((void *)tmp, (void *)iw->image.gr_gc_list,
		iw->image.num_gr_gc * sizeof(GrGC));
      if (iw->image.gr_gc_list)
         _XvicFree(biw, iw->image.gr_gc_list,
		iw->image.num_gr_gc * sizeof(GrGC));
      iw->image.gr_gc_list = tmp;

      for (i=iw->image.num_gr_gc; i<n; i++) {
         iw->image.gr_gc_list[i].active = False;
         iw->image.gr_gc_list[i].num_dashes = 0;
         iw->image.gr_gc_list[i].dash_list = NULL;
         iw->image.gr_gc_list[i].gc = NULL;
      }
      i = iw->image.num_gr_gc;
      iw->image.num_gr_gc = n;
   }

   iw->image.gr_gc_list[i].active = True;
   memcpy((void *)&iw->image.gr_gc_list[i].values, (void *)values,
				sizeof(XGCValues));
   iw->image.gr_gc_list[i].mask = valuemask;
   iw->image.gr_gc_list[i].dash_offset = 0;
   iw->image.gr_gc_list[i].dash_list = NULL;
   iw->image.gr_gc_list[i].num_dashes = 0;
   iw->image.gr_gc_list[i].is_rubber = False;
   iw->image.gr_gc_list[i].active = True;
   iw->image.gr_gc_list[i].gc = NULL;

   return i;

}

/************************************************************************/
/* XvicImageCreateRubberGC						*/
/*									*/
/* Creates a GrGC for rubber-banding operations.			*/
/************************************************************************/

XvicGC
#ifdef _NO_PROTO
XvicImageCreateRubberGC(w, valuemask, values)
   Widget w;
   unsigned long valuemask;
   XGCValues *values;
#else
XvicImageCreateRubberGC(
   Widget w,
   unsigned long valuemask,
   XGCValues *values)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicGC gc;

   gc = XvicImageCreateGC(w, valuemask, values);

   iw->image.gr_gc_list[gc].is_rubber = True;

   return gc;

}

/************************************************************************/
/* XvicImageDrawArc							*/
/*									*/
/* Draw an arc in the graphics overlay.					*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawArc(w, id, gc, color, x, y, width, height, angle1, angle2)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
   int angle1;
   int angle2;
#else
XvicImageDrawArc(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height,
   int angle1,
   int angle2)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddArc(iw, id, gc, color, x, y, width, height, angle1, angle2,
		GrArc);
}

/************************************************************************/
/* XvicImageDrawArcs							*/
/*									*/
/* Draw a set of arcs in the graphics overlay.				*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawArcs(w, id, gc, color, arcs, narcs)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicArc *arcs;
   int narcs;
#else
XvicImageDrawArcs(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicArc *arcs,
   int narcs)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddArcs(iw, id, gc, color, arcs, narcs,
		GrArcs);
}

/************************************************************************/
/* XvicImageDrawBitmap							*/
/*									*/
/* Draw a bitmap in the graphics overlay.				*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawBitmap(w, id, gc, color, x, y, bitmap, width, height, hot_x, hot_y)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   Pixmap bitmap;
   unsigned int width;
   unsigned int height;
   int hot_x;
   int hot_y;
#else
XvicImageDrawBitmap(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   Pixmap bitmap,
   unsigned int width,
   unsigned int height,
   int hot_x,
   int hot_y)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrBitmapObject));
   if (!obj)
      return 0;
   obj->type = GrBitmap;

   obj->bitmap.x = x;
   obj->bitmap.y = y;
   obj->bitmap.bitmap = bitmap;
   obj->bitmap.width = width;
   obj->bitmap.height = height;
   obj->bitmap.hot_x = hot_x;
   obj->bitmap.hot_y = hot_y;

   obj->any.bounds.x1 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageDrawImageString						*/
/*									*/
/* Draw a string with opaque background in the graphics overlay.	*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawImageString(w, id, gc, fg, bg, x, y, string, length, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor fg;
   XvicColor bg;
   double x;
   double y;
   char *string;
   int length;
   int justify;
#else
XvicImageDrawImageString(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor fg,
   XvicColor bg,
   double x,
   double y,
   char *string,
   int length,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddString(iw, id, gc, fg, bg, x, y, string, length, justify,
		GrImageString);
}

/************************************************************************/
/* XvicImageDrawImageString16						*/
/*									*/
/* Draw a 16-bit string with opaque background in the graphics overlay.	*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawImageString16(w, id, gc, fg, bg, x, y, string, length, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor fg;
   XvicColor bg;
   double x;
   double y;
   XChar2b *string;
   int length;
   int justify;
#else
XvicImageDrawImageString16(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor fg,
   XvicColor bg,
   double x,
   double y,
   XChar2b *string,
   int length,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddString16(iw, id, gc, fg, bg, x, y, string, length, justify,
		GrImageString16);
}

/************************************************************************/
/* XvicImageDrawLine							*/
/*									*/
/* Draw a line in the graphics overlay.					*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawLine(w, id, gc, color, x1, y1, x2, y2)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x1;
   double y1;
   double x2;
   double y2;
#else
XvicImageDrawLine(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x1,
   double y1,
   double x2,
   double y2)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrLineObject));
   if (!obj)
      return 0;
   obj->type = GrLine;

   obj->line.x1 = x1;
   obj->line.y1 = y1;
   obj->line.x2 = x2;
   obj->line.y2 = y2;

   obj->any.bounds.x1 = MIN(x1, x2);
   obj->any.bounds.y1 = MIN(y1, y2);
   obj->any.bounds.x2 = MAX(x1, x2);
   obj->any.bounds.y2 = MAX(y1, y2);

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageDrawLines							*/
/*									*/
/* Draw a set of connected lines in the graphics overlay.		*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawLines(w, id, gc, color, points, npoints, mode)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicPoint *points;
   int npoints;
   int mode;
#else
XvicImageDrawLines(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicPoint *points,
   int npoints,
   int mode)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddLines(iw, id, gc, color, points, npoints, 0, mode, GrLines);
}

/************************************************************************/
/* XvicImageDrawPoint							*/
/*									*/
/* Draws a point in the graphics overlay.				*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawPoint(w, id, gc, color, x, y)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
#else
XvicImageDrawPoint(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrPointObject));
   if (!obj)
      return 0;
   obj->type = GrPoint;

   obj->point.x = x;
   obj->point.y = y;

   obj->any.bounds.x1 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageDrawPoints							*/
/*									*/
/* Draw a set of points in the graphics overlay.			*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawPoints(w, id, gc, color, points, npoints, mode)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicPoint *points;
   int npoints;
   int mode;
#else
XvicImageDrawPoints(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicPoint *points,
   int npoints,
   int mode)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddLines(iw, id, gc, color, points, npoints, 0, mode, GrPoints);
}

/************************************************************************/
/* XvicImageDrawRectangle						*/
/*									*/
/* Draw a rectangle in the graphics overlay.				*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawRectangle(w, id, gc, color, x, y, width, height)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
#else
XvicImageDrawRectangle(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddRectangle(iw, id, gc, color, x, y, width, height, GrRectangle);
}

/************************************************************************/
/* XvicImageDrawRectangles						*/
/*									*/
/* Draw a set of rectangles in the graphics overlay.			*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawRectangles(w, id, gc, color, rectangles, nrectangles)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicRectangle *rectangles;
   int nrectangles;
#else
XvicImageDrawRectangles(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicRectangle *rectangles,
   int nrectangles)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddRectangles(iw, id, gc, color, rectangles, nrectangles,
		GrRectangles);
}

/************************************************************************/
/* XvicImageDrawSegments						*/
/*									*/
/* Draw a set of disjoint line segments in the graphics overlay.	*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawSegments(w, id, gc, color, segments, nsegments)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicSegment *segments;
   int nsegments;
#else
XvicImageDrawSegments(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicSegment *segments,
   int nsegments)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;
   int i;

   obj = NewObject(iw, id, gc, color, sizeof(GrSegmentsObject));
   if (!obj)
      return 0;
   obj->type = GrSegments;

   obj->segments.nsegments = nsegments;
   obj->segments.segments = (XvicSegment *)_XvicMalloc(biw,
			nsegments * sizeof(XvicSegment));
   memcpy((void *)obj->segments.segments, (void *)segments,
			nsegments * sizeof(XvicSegment));

   /* Calculate the bounding box */

   if (nsegments == 0) {			/* huh? */
      obj->any.bounds.x1 = 0.0;
      obj->any.bounds.y1 = 0.0;
      obj->any.bounds.x2 = 0.0;
      obj->any.bounds.y2 = 0.0;
   }
   else {
      obj->any.bounds.x1 = MIN(segments[0].x1, segments[0].x2);
      obj->any.bounds.y1 = MIN(segments[0].y1, segments[0].y2);
      obj->any.bounds.x2 = MAX(segments[0].x1, segments[0].x2);
      obj->any.bounds.y2 = MAX(segments[0].y1, segments[0].y2);

      for (i=1; i<nsegments; i++) {
         if (MIN(segments[i].x1, segments[i].x2) < obj->any.bounds.x1)
            obj->any.bounds.x1 = MIN(segments[i].x1, segments[i].x2);
         if (MAX(segments[i].x1, segments[i].x2) > obj->any.bounds.x2)
            obj->any.bounds.x2 = MAX(segments[i].x1, segments[i].x2);
         if (MIN(segments[i].y1, segments[i].y2) < obj->any.bounds.y1)
            obj->any.bounds.y1 = MIN(segments[i].y1, segments[i].y2);
         if (MAX(segments[i].y1, segments[i].y2) > obj->any.bounds.y2)
            obj->any.bounds.y2 = MAX(segments[i].y1, segments[i].y2);
      }
   }

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageDrawString							*/
/*									*/
/* Draw a string with transparent background in the graphics overlay.	*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawString(w, id, gc, color, x, y, string, length, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   char *string;
   int length;
   int justify;
#else
XvicImageDrawString(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   char *string,
   int length,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddString(iw, id, gc, 0, color, x, y, string, length, justify,
		GrString);
}

/************************************************************************/
/* XvicImageDrawString16						*/
/*									*/
/* Draw a 16-bit string with transparent background in the graphics	*/
/* overlay.								*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawString16(w, id, gc, color, x, y, string, length, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   XChar2b *string;
   int length;
   int justify;
#else
XvicImageDrawString16(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   XChar2b *string,
   int length,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddString16(iw, id, gc, 0, color, x, y, string, length, justify,
		GrString16);
}

/************************************************************************/
/* XvicImageDrawText							*/
/*									*/
/* Draw multiple text strings with transparent background in the	*/
/* graphics overlay.							*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawText(w, id, gc, color, x, y, items, nitems, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   XTextItem *items;
   int nitems;
   int justify;
#else
XvicImageDrawText(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   XTextItem *items,
   int nitems,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrTextObject));
   if (!obj)
      return 0;
   obj->type = GrText;

   obj->anystr.justify = justify;
   obj->text.x = x;
   obj->text.y = y;

   obj->text.items = _XvicMalloc(biw, nitems * sizeof(XTextItem));
   memcpy((void *)obj->text.items, (void *)items, nitems * sizeof(XTextItem));

   /* Calculate the bounding box */

   CalcStringBounds(iw, obj);

   obj->any.bounds.x1 = x;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageDrawText16							*/
/*									*/
/* Draw multiple 16-bit text strings with transparent background in the	*/
/* graphics overlay.							*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageDrawText16(w, id, gc, color, x, y, items, nitems, justify)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   XTextItem16 *items;
   int nitems;
   int justify;
#else
XvicImageDrawText16(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   XTextItem16 *items,
   int nitems,
   int justify)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrText16Object));
   if (!obj)
      return 0;
   obj->type = GrText16;

   obj->anystr.justify = justify;
   obj->text16.x = x;
   obj->text16.y = y;

   obj->text16.items = _XvicMalloc(biw, nitems * sizeof(XTextItem16));
   memcpy((void *)obj->text16.items, (void *)items, nitems*sizeof(XTextItem16));

   /* Calculate the bounding box */

   CalcStringBounds(iw, obj);

   obj->any.bounds.x1 = x;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* XvicImageEraseObject							*/
/*									*/
/* Erase an object in the graphics overlay.  The object is deleted and	*/
/* cannot be drawn again.						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageEraseObject(w, id)
   Widget w;
   XvicID id;
#else
XvicImageEraseObject(
   Widget w,
   XvicID id)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   _XvicRegion *rgn;
   XvicGC gc;
   int i, j;

   /* Check for rubber-banded objects and erase them first */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.id == id) {
            gc = iw->image.gr_objects[i]->any.gc;
            if (CheckValidGrGC(iw, gc, False)) {
               if (iw->image.gr_gc_list[gc].is_rubber) {

                  /* Finally.  Found a rubber object to delete. */

                  DrawObject(iw, iw->image.gr_objects[i], NULL);

               }
            }
         }
      }
   }

   /* Gather the area to erase in Image coords (excludes rubber areas) */

   rgn = GetBoundingRgnForID(iw, id, NULL);

   /* Now delete the objects */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.id == id) {

            /* Found a match, now delete the object and move everything	*/
            /* else down to fill in the spot.				*/

            FreeOneGrObject(iw, iw->image.gr_objects[i]);

            for (j=i; j<iw->image.num_gr_objects-1; j++)
               iw->image.gr_objects[j] = iw->image.gr_objects[j+1];
            iw->image.gr_objects[iw->image.num_gr_objects-1] = NULL;
            i--;	/* Check this index again since it's a new object */
         }
      }
   }

   if (id == iw->image.max_gr_id)
      iw->image.max_gr_id--;		/* Reuse this ID if it's the last one */

   /* Now redisplay the area covered by the region. */

   RepaintOverlayRegion(iw, rgn);

   _XvicRegionDestroy(rgn);

}

/************************************************************************/
/* XvicImageEraseOverlay						*/
/*									*/
/* Erase the entire graphics overlay.  The objects are deleted and	*/
/* cannot be drawn again.						*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageEraseOverlay(w)
   Widget w;
#else
XvicImageEraseOverlay(
   Widget w)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   _XvicRegion *rgn;
   _XvicRect rect;
   int i;

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         FreeOneGrObject(iw, iw->image.gr_objects[i]);
         iw->image.gr_objects[i] = NULL;
      }
   }

   /* Now redisplay the area covered by the region. */

   XvicImageDisplayBounds((Widget)iw, &rect.x1, &rect.y1, &rect.x2, &rect.y2);
   do {
      rgn = _XvicRegionCreateRect(&rect);
      if (!rgn) _XvicMemoryPanic(biw);
   } while (rgn == NULL);

   RepaintOverlayRegion(iw, rgn);

   _XvicRegionDestroy(rgn);

}

/************************************************************************/
/* XvicImageFillArc							*/
/*									*/
/* Draw a filled arc in the graphics overlay.				*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageFillArc(w, id, gc, color, x, y, width, height, angle1, angle2)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
   int angle1;
   int angle2;
#else
XvicImageFillArc(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height,
   int angle1,
   int angle2)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddArc(iw, id, gc, color, x, y, width, height, angle1, angle2,
		GrFillArc);
}

/************************************************************************/
/* XvicImageFillArcs							*/
/*									*/
/* Draw a set of filled arcs in the graphics overlay.			*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageFillArcs(w, id, gc, color, arcs, narcs)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicArc *arcs;
   int narcs;
#else
XvicImageFillArcs(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicArc *arcs,
   int narcs)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddArcs(iw, id, gc, color, arcs, narcs,
		GrFillArcs);
}

/************************************************************************/
/* XvicImageFillPolygon							*/
/*									*/
/* Draw a filled polygon in the graphics overlay.			*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageFillPolygon(w, id, gc, color, points, npoints, shape, mode)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicPoint *points;
   int npoints;
   int shape;
   int mode;
#else
XvicImageFillPolygon(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicPoint *points,
   int npoints,
   int shape,
   int mode)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddLines(iw, id, gc, color, points, npoints, 0, mode, GrFillPolygon);
}

/************************************************************************/
/* XvicImageFillRectangle						*/
/*									*/
/* Draw a filled rectangle in the graphics overlay.			*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageFillRectangle(w, id, gc, color, x, y, width, height)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
#else
XvicImageFillRectangle(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddRectangle(iw, id, gc, color, x, y, width, height, GrFillRectangle);
}

/************************************************************************/
/* XvicImageFillRectangles						*/
/*									*/
/* Draw a set of filled rectangles in the graphics overlay.		*/
/************************************************************************/

XvicID
#ifdef _NO_PROTO
XvicImageFillRectangles(w, id, gc, color, rectangles, nrectangles)
   Widget w;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicRectangle *rectangles;
   int nrectangles;
#else
XvicImageFillRectangles(
   Widget w,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicRectangle *rectangles,
   int nrectangles)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   return AddRectangles(iw, id, gc, color, rectangles, nrectangles,
		GrFillRectangles);
}

/************************************************************************/
/* XvicImageFreeGC							*/
/*									*/
/* Frees a GrGC.  It is the caller's responsibilty to make sure that	*/
/* all graphics objects that use this GC have been erased.		*/
/*!!!! Should we make *sure* no graphics objects use this?? !!!!	*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageFreeGC(w, gc)
   Widget w;
   XvicGC gc;
#else
XvicImageFreeGC(
   Widget w,
   XvicGC gc)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;

   /* Make sure the GC is valid. */

   if (!CheckValidGrGC(iw, gc, False))
      return;

   if (gc == 0)		/* Don't allow freeing the default!! */
      return;

   FreeOneGrGC(iw, &iw->image.gr_gc_list[gc]);

}

/************************************************************************/
/* XvicImageGetGrColor							*/
/*									*/
/* Returns a graphics color index for the given color.			*/
/************************************************************************/

XvicColor
#ifdef _NO_PROTO
XvicImageGetGrColor(w, xcolor)
   Widget w;
   XColor *xcolor;
#else
XvicImageGetGrColor(
   Widget w,
   XColor *xcolor)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrColor *tmp;
   int i, n;

   /* Check to see if we can reuse an old color */

   for (i=0; i<iw->image.num_gr_colors; i++) {
      if (!iw->image.gr_colors[i].active)		/* found one */
         break;
   }

   if (i == iw->image.num_gr_colors) {	/* none found, expand the table */

      n = iw->image.num_gr_colors + 10;
      tmp = _XvicMalloc(biw, n * sizeof(GrColor));
      if (iw->image.num_gr_colors > 0)
         memcpy((void *)tmp, (void *)iw->image.gr_colors,
		iw->image.num_gr_colors * sizeof(GrColor));
      if (iw->image.gr_colors)
         _XvicFree(biw, iw->image.gr_colors,
		iw->image.num_gr_colors * sizeof(GrColor));
      iw->image.gr_colors = tmp;

      for (i=iw->image.num_gr_colors; i<n; i++) {
         iw->image.gr_colors[i].active = False;
         iw->image.gr_colors[i].gc_tile = None;
         iw->image.gr_colors[i].alloc_def = False;
         iw->image.gr_colors[i].alloc_pvt = False;
      }
      i = iw->image.num_gr_colors;
      iw->image.num_gr_colors = n;
   }

   iw->image.gr_colors[i].active = True;
   memcpy((void *)&iw->image.gr_colors[i].xcolor,(void *)xcolor,sizeof(XColor));
   iw->image.gr_colors[i].gc_tile = None;
   iw->image.gr_colors[i].width = 0;
   iw->image.gr_colors[i].height = 0;
   iw->image.gr_colors[i].alloc_def = False;
   iw->image.gr_colors[i].alloc_pvt = False;

   DPR(("trying to match %4x,%4x,%4x\n", xcolor->red,xcolor->green,xcolor->blue));

   GetOneGrColor(iw, &iw->image.gr_colors[i], NULL);

   return i;
}

/************************************************************************/
/* XvicImageGetGrColorRGB						*/
/*									*/
/* Returns a graphics color index given 8-bit RGB color values.		*/
/************************************************************************/

XvicColor
#ifdef _NO_PROTO
XvicImageGetGrColorRGB(w, red, green, blue)
   Widget w;
   int red;
   int green;
   int blue;
#else
XvicImageGetGrColorRGB(
   Widget w,
   int red,
   int green,
   int blue)
#endif /* _NO_PROTO */
{
   XColor xcolor;

   xcolor.red = red << 8;
   xcolor.green = green << 8;
   xcolor.blue = blue << 8;

   return XvicImageGetGrColor(w, &xcolor);
}

/************************************************************************/
/* XvicImageMoveObject							*/
/*									*/
/* Move an object in the graphics overlay using the supplied deltas.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageMoveObject(w, id, delta_x, delta_y)
   Widget w;
   XvicID id;
   double delta_x;
   double delta_y;
#else
XvicImageMoveObject(
   Widget w,
   XvicID id,
   double delta_x,
   double delta_y)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   _XvicRegion *rgn;
   XvicGC gc;
   int i;

   /* Check for rubber-banded objects and move them first */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.id == id) {
            gc = iw->image.gr_objects[i]->any.gc;
            if (CheckValidGrGC(iw, gc, False)) {
               if (iw->image.gr_gc_list[gc].is_rubber) {

                  /* Finally.  Found a rubber object to move. */

                  DrawObject(iw, iw->image.gr_objects[i], NULL);
                  MoveOneObject(iw, iw->image.gr_objects[i], delta_x, delta_y);
                  DrawObject(iw, iw->image.gr_objects[i], NULL);
               }
            }
         }
      }
   }

   /* Gather the area to move in Image coords (excludes rubber areas) */

   rgn = GetBoundingRgnForID(iw, id, NULL);

   /* Now move the objects that aren't rubber */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.id == id) {
            gc = iw->image.gr_objects[i]->any.gc;
            if (!CheckValidGrGC(iw, gc, False) ||
		!iw->image.gr_gc_list[gc].is_rubber) {
               MoveOneObject(iw, iw->image.gr_objects[i], delta_x, delta_y);
            }
         }
      }
   }

   /* Gather the additional area that the objects now cover */

   rgn = GetBoundingRgnForID(iw, id, rgn);

   /* Now redisplay the area covered by the region. */

   RepaintOverlayRegion(iw, rgn);

   _XvicRegionDestroy(rgn);

}

/************************************************************************/
/* XvicImageSetDashes							*/
/*									*/
/* Changes the dash mode for an existing GrGC, just like XSetDashes()	*/
/* does.  Needed because dashes cannot be set entirely via the		*/
/* XGCValues struct.  Passing n as 0 disables the XSetDashes() call.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetDashes(w, gc, dash_offset, dash_list, n)
   Widget w;
   XvicGC gc;
   int dash_offset;
   char dash_list[];
   int n;
#else
XvicImageSetDashes(
   Widget w,
   XvicGC gc,
   int dash_offset,
   char dash_list[],
   int n)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   /* Make sure the GC is valid */

   if (!CheckValidGrGC(iw, gc, True))
      return;

   if (iw->image.gr_gc_list[gc].dash_list)
      _XvicFree(biw, iw->image.gr_gc_list[gc].dash_list,
			iw->image.gr_gc_list[gc].num_dashes);

   if (n == 0) {		/* 0 means to not use XSetDashes() */
      iw->image.gr_gc_list[gc].dash_list = NULL;
      iw->image.gr_gc_list[gc].num_dashes = 0;
      iw->image.gr_gc_list[gc].dash_offset = 0;
      /* Freeing the list is the only way I know to undo XSetDashes() */
      if (iw->image.gr_gc_list[gc].gc)
         XFreeGC(XtDisplay((Widget)iw), iw->image.gr_gc_list[gc].gc);
         iw->image.gr_gc_list[gc].gc = NULL;
   }
   else {
      iw->image.gr_gc_list[gc].dash_list = _XvicMalloc(biw, n);
      memcpy((void *)iw->image.gr_gc_list[gc].dash_list, (void *)dash_list, n);
      iw->image.gr_gc_list[gc].num_dashes = n;
      iw->image.gr_gc_list[gc].dash_offset = dash_offset;
      if (iw->image.gr_gc_list[gc].gc)
         XSetDashes(XtDisplay((Widget)iw), iw->image.gr_gc_list[gc].gc,
			dash_offset, dash_list, n);
   }

}

/************************************************************************/
/* XvicImageSetFontCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the standard cursor	*/
/* font.  This is just a wrapper around _XvicImageSetFontCursor that	*/
/* nulls out the XvicNcursor resource first.				*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetFontCursor(w, shape)
   Widget w;
   unsigned int shape;
#else
XvicImageSetFontCursor(
   Widget w,
   unsigned int shape)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor != NULL) {
      _XvicFree(biw, iw->image.cursor, strlen(iw->image.cursor)+1);
      iw->image.cursor = NULL;
   }
   _XvicImageSetFontCursor(iw, shape);
}

/************************************************************************/
/* XvicImageSetGlyphCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the given font(s).	*/
/* This is just a wrapper around _XvicImageSetGlyphCursor that nulls	*/
/* out the XvicNcursor resource first.					*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetGlyphCursor(w, source_font, mask_font, source_char, mask_char)
   Widget w;
   Font source_font;
   Font mask_font;
   unsigned int source_char;
   unsigned int mask_char;
#else
XvicImageSetGlyphCursor(
   Widget w,
   Font source_font,
   Font mask_font,
   unsigned int source_char,
   unsigned int mask_char)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor != NULL) {
      _XvicFree(biw, iw->image.cursor, strlen(iw->image.cursor)+1);
      iw->image.cursor = NULL;
   }
   _XvicImageSetGlyphCursor(iw, source_font, mask_font, source_char, mask_char);
}

/************************************************************************/
/* XvicImageSetPixmapCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the given pixmaps.	*/
/* This is just a wrapper around _XvicImageSetPixmapCursor that nulls	*/
/* out the XvicNcursor resource first.					*/
/************************************************************************/

void
#ifdef _NO_PROTO
XvicImageSetPixmapCursor(w, source, mask, x, y)
   Widget w;
   Pixmap source;
   Pixmap mask;
   unsigned int x;
   unsigned int y;
#else
XvicImageSetPixmapCursor(
   Widget w,
   Pixmap source,
   Pixmap mask,
   unsigned int x,
   unsigned int y)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget) w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor != NULL) {
      _XvicFree(biw, iw->image.cursor, strlen(iw->image.cursor)+1);
      iw->image.cursor = NULL;
   }
   _XvicImageSetPixmapCursor(iw, source, mask, x, y);
}

/************************************************************************/
/************************************************************************/
/*  P R I V A T E   E X T E R N A L   F U N C T I O N S			*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* _XvicImageSetFontCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the standard cursor	*/
/* font.  This merely loads the cursor font then calls GlyphCursor.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicImageSetFontCursor(iw, shape)
   XvicImageWidget iw;
   unsigned int shape;
#else
_XvicImageSetFontCursor(
   XvicImageWidget iw,
   unsigned int shape)
#endif /* _NO_PROTO */
{
   Font cursor_font;

   cursor_font = XLoadFont(XtDisplay((Widget)iw), CURSORFONT_NAME);

   /* shape+1 is where the mask lives */

   _XvicImageSetGlyphCursor(iw, cursor_font, cursor_font, shape, shape+1);

   XUnloadFont(XtDisplay((Widget)iw), cursor_font);

}

/************************************************************************/
/* _XvicImageSetGlyphCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the given font(s).	*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicImageSetGlyphCursor(iw, source_font, mask_font, source_char, mask_char)
   XvicImageWidget iw;
   Font source_font;
   Font mask_font;
   unsigned int source_char;
   unsigned int mask_char;
#else
_XvicImageSetGlyphCursor(
   XvicImageWidget iw,
   Font source_font,
   Font mask_font,
   unsigned int source_char,
   unsigned int mask_char)
#endif /* _NO_PROTO */
{
   Boolean status;
   int width, height;
   int mask_width, mask_height;
   unsigned int hot_x, hot_y;
   unsigned int mask_hot_x, mask_hot_y;
   int offset_x, offset_y, mask_offset_x, mask_offset_y;
   char str[1];
   XGCValues gc_values;
   GC gc;
   XFontStruct *fs;

   fs = XQueryFont(XtDisplay((Widget)iw), source_font);

   status = GetCursorSizeFromFont(iw, fs, source_char,
		&width, &height, &hot_x, &hot_y);
   if (!status)
      return;
   offset_x = 0;
   offset_y = 0;

   if (mask_font != None) {

      if (mask_font != source_font) {
         /* !!!! How do we free the XFontStruct structure?? !!!! */
         fs = XQueryFont(XtDisplay((Widget)iw), mask_font);
      }
      status = GetCursorSizeFromFont(iw, fs, mask_char,
		&mask_width, &mask_height, &mask_hot_x, &mask_hot_y);
      if (!status)
         return;
      mask_offset_x = 0;
      mask_offset_y = 0;

      if (mask_width != width || mask_height != height ||
		mask_hot_x != hot_x || mask_hot_y != hot_y) {

         /* The mask and cursor glyphs are not the same size, or don't	*/
         /* overlap hotspots.  Since PixmapCursor requires this, make	*/
         /* it happen.							*/

         if (hot_x < mask_hot_x) {
            offset_x = (mask_hot_x - hot_x);
            hot_x = mask_hot_x;
         }
         else if (hot_x > mask_hot_x)
            mask_offset_x = (hot_x - mask_hot_x);

         if (hot_y < mask_hot_y) {
            offset_y = (mask_hot_y - hot_y);
            hot_y = mask_hot_y;
         }
         else if (hot_y > mask_hot_y)
            mask_offset_y = (hot_y - mask_hot_y);

         width = MAX(offset_x + width, mask_offset_x + mask_width);
         height = MAX(offset_y + height, mask_offset_y + mask_height);
      }
   }

   if (iw->image.cursor_mode == XvicPLANTED)
      ErasePlantedCursor(iw);

   if (iw->image.curs_source)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_source);
   if (iw->image.curs_mask)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_mask);
   iw->image.curs_mask = None;

   /* !!!! How do we free the XFontStruct structure?? !!!! */

   /* Now, create the pixmaps and render the font glyphs into them */

   iw->image.curs_width = width;
   iw->image.curs_height = height;
   iw->image.curs_hot_x = hot_x;
   iw->image.curs_hot_y = hot_y;
   iw->image.curs_source = XCreatePixmap(XtDisplay((Widget)iw),
			RootWindowOfScreen(XtScreen((Widget)iw)),
			width, height, 1);

   /* Create a temporary GC to use for rendering the text */

   gc_values.font = source_font;
   gc_values.foreground = 0;		/* set to 1 after the clear */
   gc_values.background = 0;		/* 1-bit pixmap, so these are okay */
   gc = XCreateGC(XtDisplay((Widget)iw), iw->image.curs_source,
		GCFont | GCForeground | GCBackground, &gc_values);

   /* Clear the pixmap in case the font doesn't cover it all.		*/
   /* There appears to be no way to clear a pixmap!  XClearWindow and	*/
   /* XClearArea work only on Windows, not Pixmaps!  Give me a break!!	*/

   XFillRectangle(XtDisplay((Widget)iw), iw->image.curs_source, gc,
		0, 0, width, height);
   XSetForeground(XtDisplay((Widget)iw), gc, 1);

   /* Render the text into the pixmap.  A little work must be done here	*/
   /* in order to support multibyte fonts, as well.			*/

   str[0] = (char) source_char;
   XDrawImageString(XtDisplay((Widget)iw), iw->image.curs_source, gc,
		hot_x, hot_y, str, 1);

   /* Now do the same for the mask, if present */

   if (mask_font != None) {
      iw->image.curs_mask = XCreatePixmap(XtDisplay((Widget)iw),
			RootWindowOfScreen(XtScreen((Widget)iw)),
			width, height, 1);
      /* Clear the pixmap in case the font doesn't cover it all.	*/
      XSetForeground(XtDisplay((Widget)iw), gc, 0);
      XFillRectangle(XtDisplay((Widget)iw), iw->image.curs_mask, gc,
		0, 0, width, height);
      XSetForeground(XtDisplay((Widget)iw), gc, 1);

      XSetFont(XtDisplay((Widget)iw), gc, mask_font);

      str[0] = (char) mask_char;
      XDrawImageString(XtDisplay((Widget)iw), iw->image.curs_mask, gc,
		hot_x, hot_y, str, 1);
   }

   MaskPixmap(iw, iw->image.curs_mask, iw->image.curs_source, width, height);

   CreateXCursor(iw);
   iw->image.plant_set = False;

   if (iw->image.cursor_mode == XvicPLANTED)
      DrawPlantedCursor(iw);

}

/************************************************************************/
/* _XvicImageSetPixmapCursor						*/
/*									*/
/* Sets the cursor to the specified shape from the given pixmaps.	*/
/* We must copy the pixmaps to internal storage so the caller can free	*/
/* it.  The widget does not have to be realized to call this function.	*/
/* We don't bother with _XvicMemoryGrab et al because these pixmaps are	*/
/* too small to worry about.  The mask pixmap may be NULL (None), and	*/
/* both pixmaps may be freed by the caller after this routine returns.	*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicImageSetPixmapCursor(iw, source, mask, x, y)
   XvicImageWidget iw;
   Pixmap source;
   Pixmap mask;
   unsigned int x;
   unsigned int y;
#else
_XvicImageSetPixmapCursor(
   XvicImageWidget iw,
   Pixmap source,
   Pixmap mask,
   unsigned int x,
   unsigned int y)
#endif /* _NO_PROTO */
{
   Window root;
   int xcoord, ycoord;
   unsigned int width, height, border_width, depth;
   GC gc;
   XGCValues gc_values;

   if (iw->image.cursor_mode == XvicPLANTED)
      ErasePlantedCursor(iw);

   /* Create a temporary GC to use for copying the pixmap */

   gc = XCreateGC(XtDisplay((Widget)iw), source, 0, &gc_values);

   if (iw->image.curs_source)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_source);

   /* Create the pixmap to copy into */

   XGetGeometry(XtDisplay((Widget)iw), source,
	&root, &xcoord, &ycoord, &width, &height, &border_width, &depth);
   iw->image.curs_width = width;
   iw->image.curs_height = height;
   iw->image.curs_hot_x = x;
   iw->image.curs_hot_y = y;
   iw->image.curs_source = XCreatePixmap(XtDisplay((Widget)iw),
			source, width, height, 1);

   /* Copy the pixmap */

   XCopyArea(XtDisplay((Widget)iw), source, iw->image.curs_source, gc,
			0, 0, width, height, 0, 0);

   /* Now do the same for the mask, if present */

   if (iw->image.curs_mask)
      XFreePixmap(XtDisplay((Widget)iw), iw->image.curs_mask);
   iw->image.curs_mask = None;

   if (mask != None) {
      iw->image.curs_mask = XCreatePixmap(XtDisplay((Widget)iw),
			mask, width, height, 1);

      XCopyArea(XtDisplay((Widget)iw), mask, iw->image.curs_mask, gc,
			0, 0, width, height, 0, 0);
   }

   XFreeGC(XtDisplay((Widget)iw), gc);

   MaskPixmap(iw, iw->image.curs_mask, iw->image.curs_source, width, height);

   CreateXCursor(iw);
   iw->image.plant_set = False;

   if (iw->image.cursor_mode == XvicPLANTED)
      DrawPlantedCursor(iw);

}

/************************************************************************/
/************************************************************************/
/*  A C T I O N   P R O C E D U R E S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* CursorModeAction							*/
/*									*/
/* Sets the cursor planting mode based on the (single) argument to	*/
/* "plant", "float", or "toggle".  The default if no argument is given	*/
/* is "toggle".  The second argument, if "true" and we are going from	*/
/* planted to floating, causes the mouse pointer to be warped to the	*/
/* last location of the planted cursor.  Use with discretion.  The	*/
/* default is "false".							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CursorModeAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
CursorModeAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   unsigned char mode, warp, old_mode=0;
   double x, y;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;

   if (*num_params < 1)
      mode = 't';		/* toggle */
   else
      mode = (unsigned char) *params[0];

   if (*num_params < 2)
      warp = 'f';		/* false */
   else
      warp = (unsigned char) *params[1];

   if (warp == 't' || warp == 'T') {
      x = iw->image.plant_curs_x;
      y = iw->image.plant_curs_y;
      old_mode = iw->image.cursor_mode;
   }

   switch (mode) {

      case 'p':			/* Planted */
      case 'P':
         XtVaSetValues((Widget)iw, XvicNcursorMode, XvicPLANTED, NULL);
         break;

      case 'f':			/* Floating */
      case 'F':
         XtVaSetValues((Widget)iw, XvicNcursorMode, XvicFLOATING, NULL);
         break;

      case 't':			/* Toggle */
      case 'T':
         if (iw->image.cursor_mode == XvicFLOATING)
            mode = XvicPLANTED;
         else
            mode = XvicFLOATING;
         XtVaSetValues((Widget)iw, XvicNcursorMode, mode, NULL);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */
   }

   if (warp == 't' || warp == 'T') {
      if (old_mode == XvicPLANTED || iw->image.cursor_mode == XvicFLOATING) {
         XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(x),
				   XvicNcursorYfp, XvicDOUBLE_ARG(y), NULL);
      }
   }
}

/************************************************************************/
/* InputAction								*/
/*									*/
/* Calls the inputCallback routine after translating the position to	*/
/* Image coordinates.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
InputAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
InputAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XvicImageCallbackStruct cb;
   int x, y;
   Boolean use_location;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (!iw->image.input_callback)
      return;				/* No callback; nothing to do */

   cb.reason = XvicCR_INPUT;
   cb.event = event;
   cb.input_params = params;
   cb.input_num_params = *num_params;

   use_location = GetXYFromEvent(iw, w, event, &x, &y);

   if (use_location) {
      cb.x_fp = XC_Scr2Img(x - biw->bim.x_dpy_off);
      cb.y_fp = YC_Scr2Img(y - biw->bim.y_dpy_off);
      cb.x = ROUND(cb.x_fp);
      cb.y = ROUND(cb.y_fp);

      cb.on_screen = True;
      if ((x - biw->bim.x_dpy_off) < 0 ||
	  (x - biw->bim.x_dpy_off) >= (int)biw->bim.view_width)
         cb.on_screen = False;
      if ((y - biw->bim.y_dpy_off) < 0 ||
	  (y - biw->bim.y_dpy_off) >= (int)biw->bim.view_height)
         cb.on_screen = False;
      if (event->type == LeaveNotify)
         cb.on_screen = False;
   }
   else {
      cb.x = 0;
      cb.y = 0;
      cb.x_fp = 0.0;
      cb.y_fp = 0.0;
      cb.on_screen = False;
   }

   XtCallCallbackList((Widget)iw, iw->image.input_callback, (XtPointer) &cb);

}

/************************************************************************/
/* MousePanAction							*/
/*									*/
/* Implements mouse-based panning.  This action may be triggered by any	*/
/* event with an (x,y) positon, and must be preceded by a MousePanStart	*/
/* action.  There is no termination action; panning happens as long as	*/
/* MousePan is called.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MousePanAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
MousePanAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int x, y;
   int new_x, new_y;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (!XtIsRealized((Widget)iw))
      return;

   if (!GetXYFromEvent(iw, w, event, &x, &y))
      return;			/* If a bad event, do nothing */

   /* To get the new pan value, we take the difference between this	*/
   /* location and the last one, and add it to the current pan value.	*/
   /* We call SetValues here so we don't have to make DoPan externally	*/
   /* visible (it's a static function in BasicImage).  There shouldn't	*/
   /* be too much overhead.						*/
   /* Note that the mouse position is saved in Scr coordinates (i.e.	*/
   /* what we get directly from the event) so that it will be invariant	*/
   /* under pans (if we kept it in Image coordinates, the pan would	*/
   /* be subtracted out.  Since we're changing the pan, race conditions	*/
   /* develop and the pan goes all haywire).  We actually use Dpy2Img	*/
   /* because all Scr does is remove the pan.  Since we're doing	*/
   /* deltas between two positions, the pan doesn't matter.		*/

   new_x = iw->bim.x_pan + (int)(XC_Dpy2Img((double)iw->image.x_mouse_scr_pan) -
				 XC_Dpy2Img((double)x));
   new_y = iw->bim.y_pan + (int)(YC_Dpy2Img((double)iw->image.y_mouse_scr_pan) -
				 YC_Dpy2Img((double)y));
   if (new_x != iw->bim.x_pan)	/* don't update pos unless pan changed */
      iw->image.x_mouse_scr_pan = x;
   if (new_y != iw->bim.y_pan)
      iw->image.y_mouse_scr_pan = y;

   XtVaSetValues((Widget)iw, XvicNxPan, new_x, XvicNyPan, new_y, NULL);

   CallPanCallback(iw);

}

/************************************************************************/
/* MousePanStartAction							*/
/*									*/
/* Sets the starting point for a mouse pan operation.  This action	*/
/* currently may be triggered by any event with an (x,y) position.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MousePanStartAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
MousePanStartAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   int x, y;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;

   if (!XtIsRealized((Widget)iw))
      return;

   if (!GetXYFromEvent(iw, w, event, &x, &y))
      return;			/* If a bad event, do nothing */

   iw->image.x_mouse_scr_pan = x;
   iw->image.y_mouse_scr_pan = y;

}

/************************************************************************/
/* MoveCursorAction							*/
/*									*/
/* Moves the cursor (if planted) one Image pixel in the indicated	*/
/* direction ("left", "right", "up", "down").  If the cursor is free	*/
/* floating, there is no effect.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MoveCursorAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
MoveCursorAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   double x, y;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor_mode != XvicPLANTED)
      return;

   if (*num_params < 1)
      return;

   x = iw->image.plant_curs_x;
   y = iw->image.plant_curs_y;

   switch (*params[0]) {

      case 'l':
      case 'L':
         x = x - 1.0;
         XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(x), NULL);
         break;

      case 'r':
      case 'R':
         x = x + 1.0;
         XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(x), NULL);
         break;

      case 'u':
      case 'U':
         y = y - 1.0;
         XtVaSetValues((Widget)iw, XvicNcursorYfp, XvicDOUBLE_ARG(y), NULL);
         break;

      case 'd':
      case 'D':
         y = y + 1.0;
         XtVaSetValues((Widget)iw, XvicNcursorYfp, XvicDOUBLE_ARG(y), NULL);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */

   }
}

/************************************************************************/
/* MoveCursorMouseAction						*/
/*									*/
/* Moves the cursor (if planted) based on an event with an (x,y)	*/
/* position.  If the cursor is free floating, there is no effect.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MoveCursorMouseAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
MoveCursorMouseAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int x, y;
   double x_fp, y_fp;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor_mode != XvicPLANTED)
      return;

   if (!GetXYFromEvent(iw, w, event, &x, &y))
      return;			/* If a bad event, do nothing */

   x_fp = XC_Scr2Img(x - biw->bim.x_dpy_off);
   y_fp = YC_Scr2Img(y - biw->bim.y_dpy_off);

   XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(x_fp),
			     XvicNcursorYfp, XvicDOUBLE_ARG(y_fp),NULL);

}

/************************************************************************/
/* MoveCursorScreenAction						*/
/*									*/
/* Moves the cursor (if planted) at least one Screen pixel in the	*/
/* indicated direction ("left", "right", "up", "down").  If the cursor	*/
/* is free floating, there is no effect.  Depending on the zoom, this	*/
/* may be more than one, or a fraction of, an Image pixel.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MoveCursorScreenAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
MoveCursorScreenAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   double img_x, img_y;
   int scr_x, scr_y;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (iw->image.cursor_mode != XvicPLANTED)
      return;

   if (*num_params < 1)
      return;

   img_x = iw->image.plant_curs_x;
   img_y = iw->image.plant_curs_y;
   scr_x = XC_Img2Scr(img_x);
   scr_y = YC_Img2Scr(img_y);

   switch (*params[0]) {

      case 'l':
      case 'L':
         img_x = XC_Scr2Img(scr_x - 1);
         XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(img_x), NULL);
         break;

      case 'r':
      case 'R':
         img_x = XC_Scr2Img(scr_x + 1);
         XtVaSetValues((Widget)iw, XvicNcursorXfp, XvicDOUBLE_ARG(img_x), NULL);
         break;

      case 'u':
      case 'U':
         img_y = YC_Scr2Img(scr_y - 1);
         XtVaSetValues((Widget)iw, XvicNcursorYfp, XvicDOUBLE_ARG(img_y), NULL);
         break;

      case 'd':
      case 'D':
         img_y = YC_Scr2Img(scr_y + 1);
         XtVaSetValues((Widget)iw, XvicNcursorYfp, XvicDOUBLE_ARG(img_y), NULL);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */

   }
}

/************************************************************************/
/* PanEdgeAction							*/
/*									*/
/* Pans all the way to the edge of the image in the direction indicated	*/
/* by the argument, which is one of the strings "left", "right", "up",	*/
/* or "down".  Case doesn't matter, and only the first character is	*/
/* significant.  Invalid arguments (or lack of an argument) is quietly	*/
/* ignored.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
PanEdgeAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
PanEdgeAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (!XtIsRealized((Widget)iw))
      return;

   if (*num_params < 1)
      return;

   switch (*params[0]) {

      case 'l':
      case 'L':
         XtVaSetValues((Widget)iw, XvicNxPan, 0, NULL);
         CallPanCallback(iw);
         break;

      case 'r':
      case 'R':
         XtVaSetValues((Widget)iw, XvicNxPan,
		iw->bim.image_width - X_Dpy2Img(iw->bim.view_width), NULL);
         CallPanCallback(iw);
         break;

      case 'u':
      case 'U':
         XtVaSetValues((Widget)iw, XvicNyPan, 0, NULL);
         CallPanCallback(iw);
         break;

      case 'd':
      case 'D':
         XtVaSetValues((Widget)iw, XvicNyPan,
		iw->bim.image_height - Y_Dpy2Img(iw->bim.view_height), NULL);
         CallPanCallback(iw);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */
   }
}

/************************************************************************/
/* PanHalfViewAction							*/
/*									*/
/* Pans by half of the view size in the direction indicated by the	*/
/* argument, which is one of the strings "left", "right", "up", or	*/
/* "down".  Case doesn't matter, and only the first character is	*/
/* significant.  Invalid arguments (or lack of an argument) is quietly	*/
/* ignored.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
PanHalfViewAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
PanHalfViewAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;
   biw = (XvicBasicImageWidget)iw;

   if (!XtIsRealized((Widget)iw))
      return;

   if (*num_params < 1)
      return;

   switch (*params[0]) {

      case 'l':
      case 'L':
         XtVaSetValues((Widget)iw, XvicNxPan,
		iw->bim.x_pan - X_Dpy2Img(iw->bim.view_width) / 2, NULL);
         CallPanCallback(iw);
         break;

      case 'r':
      case 'R':
         XtVaSetValues((Widget)iw, XvicNxPan,
		iw->bim.x_pan + X_Dpy2Img(iw->bim.view_width) / 2, NULL);
         CallPanCallback(iw);
         break;

      case 'u':
      case 'U':
         XtVaSetValues((Widget)iw, XvicNyPan,
		iw->bim.y_pan - Y_Dpy2Img(iw->bim.view_height) / 2, NULL);
         CallPanCallback(iw);
         break;

      case 'd':
      case 'D':
         XtVaSetValues((Widget)iw, XvicNyPan,
		iw->bim.y_pan + Y_Dpy2Img(iw->bim.view_height) / 2, NULL);
         CallPanCallback(iw);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */
   }
}

/************************************************************************/
/* PanOneAction								*/
/*									*/
/* Pans by one pixel in the direction indicated by the argument, which	*/
/* is one of the strings "left", "right", "up", or "down".  Case doesn't*/
/* matter, and only the first character is significant.  Invalid	*/
/* arguments (or lack of an argument) is quietly ignored.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
PanOneAction(w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params;
#else
PanOneAction(
   Widget w,
   XEvent *event,
   String *params,
   Cardinal *num_params)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;

   /* Make sure we've got the right kind of widget.  If the HW Overlay	*/
   /* gets a translation, it'll return the wrong widget so we search	*/
   /* all parents until we find an XvicImage or give up.		*/

   while (iw && !XtIsSubclass((Widget)iw, xvicImageWidgetClass))
      iw = (XvicImageWidget)XtParent((Widget)iw);
   if (!iw)
      return;

   if (!XtIsRealized((Widget)iw))
      return;

   if (*num_params < 1)
      return;

   switch (*params[0]) {

      case 'l':
      case 'L':
         XtVaSetValues((Widget)iw, XvicNxPan, iw->bim.x_pan-1, NULL);
         CallPanCallback(iw);
         break;

      case 'r':
      case 'R':
         XtVaSetValues((Widget)iw, XvicNxPan, iw->bim.x_pan+1, NULL);
         CallPanCallback(iw);
         break;

      case 'u':
      case 'U':
         XtVaSetValues((Widget)iw, XvicNyPan, iw->bim.y_pan-1, NULL);
         CallPanCallback(iw);
         break;

      case 'd':
      case 'D':
         XtVaSetValues((Widget)iw, XvicNyPan, iw->bim.y_pan+1, NULL);
         CallPanCallback(iw);
         break;

      default:
         break;		/* Do nothing, quietly ignore errors */
   }
}

/************************************************************************/
/************************************************************************/
/*  C A L L B A C K   F U N C T I O N S					*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* HScrollCallback							*/
/*									*/
/* Callback routine for changes in the horizontal scrollbar position.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
HScrollCallback(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XtPointer call_data;
#else
HScrollCallback(
   Widget w,
   XtPointer client_data,
   XtPointer call_data)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)client_data;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)call_data;

   iw->image.in_scrollbar = TRUE;
   XtVaSetValues((Widget)biw, XvicNxPan, cb->value, NULL);
   iw->image.in_scrollbar = FALSE;

   CallPanCallback(iw);

}

/************************************************************************/
/* Unrealize								*/
/*									*/
/* Callback routine for unrealizing the widget.  We must unrealize the	*/
/* overlay child here before our window gets nuked because that will	*/
/* blow away its window, and XtUnrealizeWidget() gets confused if the	*/
/* window goes away but the window handle in the widget doesn't.	*/
/* ***NOTE*** This exploits an (apparently) undocumented feature in Xt.	*/
/* XtUnrealizeWidget() calls an XtNunrealizeCallback if it exists.	*/
/* I can't find this documented but it's used in Xaw(text) and		*/
/* PEX(Workstation) so it's probably safe.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
Unrealize(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XtPointer call_data;
#else
Unrealize(
   Widget w,
   XtPointer client_data,
   XtPointer call_data)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;

   if (iw->image.overlay_widget) {
      if (XtIsRealized(iw->image.overlay_widget))
         XtUnrealizeWidget(iw->image.overlay_widget);
   }
}

/************************************************************************/
/* VScrollCallback							*/
/*									*/
/* Callback routine for changes in the vertical scrollbar position.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
VScrollCallback(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XtPointer call_data;
#else
VScrollCallback(
   Widget w,
   XtPointer client_data,
   XtPointer call_data)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)client_data;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)call_data;

   iw->image.in_scrollbar = TRUE;
   XtVaSetValues((Widget)biw, XvicNyPan, cb->value, NULL);
   iw->image.in_scrollbar = FALSE;

   CallPanCallback(iw);

}

/************************************************************************/
/************************************************************************/
/*  E V E N T   H A N D L E R S						*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* MotionEventHandler							*/
/*									*/
/* Tracks the pointer and calls the cursorCallback when it moves.	*/
/* This should only be enabled when cursorMode is XvicFLOATING.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MotionEventHandler(w, client_data, event, continue_to_dispatch)
   Widget w;
   XtPointer client_data;
   XEvent *event;
   Boolean *continue_to_dispatch;
#else
MotionEventHandler(
   Widget w,
   XtPointer client_data,
   XEvent *event,
   Boolean *continue_to_dispatch)
#endif /* _NO_PROTO */
{
   XvicImageWidget iw = (XvicImageWidget)w;
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XMotionEvent *mev = (XMotionEvent *)event;
   XCrossingEvent *cev = (XCrossingEvent *)event;
   int x, y;

   if (event->type == MotionNotify) {
      x = mev->x;
      y = mev->y;
   }
   else if (event->type == EnterNotify || event->type == LeaveNotify) {
      x = cev->x;
      y = cev->y;
   }
   else
      return;			/* Unrecognized event type */

   CallCursorCallback(iw, XC_Scr2Img(x - biw->bim.x_dpy_off),
			  YC_Scr2Img(y - biw->bim.y_dpy_off),
			  (event->type == LeaveNotify));
}

/************************************************************************/
/************************************************************************/
/*  M I S C E L L A N E O U S   F U N C T I O N S			*/
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* AddArc								*/
/*									*/
/* Does the work for XvicImageDrawArc and XvicImageFillArc.		*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddArc(iw, id, gc, color, x, y, width, height, angle1, angle2, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
   int angle1;
   int angle2;
   GrType type;
#else
AddArc(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height,
   int angle1,
   int angle2,
   GrType type)
#endif /* _NO_PROTO */
{
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrArcObject));
   if (!obj)
      return 0;
   obj->type = type;

   if (width < 1.0)
      width = 1.0;
   if (height < 1.0)
      height = 1.0;

   obj->arc.x = x;
   obj->arc.y = y;
   obj->arc.width = width;
   obj->arc.height = height;
   obj->arc.angle1 = angle1;
   obj->arc.angle2 = angle2;

   obj->any.bounds.x1 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.x2 = x + width - 1.0;
   obj->any.bounds.y2 = y + height - 1.0;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddArcs								*/
/*									*/
/* Does the work for XvicImageDrawArcs and XvicImageFillArcs.		*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddArcs(iw, id, gc, color, arcs, narcs, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicArc *arcs;
   int narcs;
   GrType type;
#else
AddArcs(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicArc *arcs,
   int narcs,
   GrType type)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;
   int i;

   obj = NewObject(iw, id, gc, color, sizeof(GrArcsObject));
   if (!obj)
      return 0;
   obj->type = type;

   obj->arcs.narcs = narcs;
   obj->arcs.arcs = (XvicArc *)_XvicMalloc(biw, narcs * sizeof(XvicArc));
   memcpy((void *)obj->arcs.arcs, (void *)arcs, narcs * sizeof(XvicArc));

#define ARC(i) obj->arcs.arcs[i]

   /* Calculate the bounding box */

   if (narcs == 0) {				/* huh? */
      obj->any.bounds.x1 = 0;
      obj->any.bounds.y1 = 0;
      obj->any.bounds.x2 = 0;
      obj->any.bounds.y2 = 0;
   }
   else {
      if (ARC(0).width < 1.0)
         ARC(0).width = 1.0;
      if (ARC(0).height < 1.0)
         ARC(0).height = 1.0;
      obj->any.bounds.x1 = ARC(0).x;
      obj->any.bounds.y1 = ARC(0).y;
      obj->any.bounds.x2 = ARC(0).x + ARC(0).width - 1.0;
      obj->any.bounds.y2 = ARC(0).y + ARC(0).height - 1.0;

      for (i=1; i<narcs; i++) {
         if (ARC(i).width < 1.0)
            ARC(i).width = 1.0;
         if (ARC(i).height < 1.0)
            ARC(i).height = 1.0;
         if (ARC(i).x < obj->any.bounds.x1)
            obj->any.bounds.x1 = ARC(i).x;
         if (ARC(i).x + ARC(i).width - 1.0 > obj->any.bounds.x2)
            obj->any.bounds.x2 = ARC(i).x + ARC(i).width - 1.0;
         if (ARC(i).y < obj->any.bounds.y1)
            obj->any.bounds.y1 = ARC(i).y;
         if (ARC(i).y + ARC(i).height - 1.0 > obj->any.bounds.y2)
            obj->any.bounds.y2 = ARC(i).y + ARC(i).height - 1.0;
      }
   }

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddString								*/
/*									*/
/* Does the work for XvicImageDrawString and XvicImageDrawImageString.	*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddString(iw, id, gc, fg, color, x, y, string, length, justify, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor fg;
   XvicColor color;
   double x;
   double y;
   char *string;
   int length;
   int justify;
   GrType type;
#else
AddString(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor fg,
   XvicColor color,
   double x,
   double y,
   char *string,
   int length,
   int justify,
   GrType type)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;

   if (type == GrImageString) {
      if (!CheckValidGrColor(iw, fg))
         return 0;
   }

   obj = NewObject(iw, id, gc, color, sizeof(GrStringObject));
   if (!obj)
      return 0;
   obj->type = type;

   obj->anystr.justify = justify;
   obj->string.fg = fg;
   obj->string.x = x;
   obj->string.y = y;
   obj->string.length = length;

   obj->string.string = _XvicMalloc(biw, length);
   memcpy((void *)obj->string.string, (void *)string, length);

   /* Calculate the bounding box */

   CalcStringBounds(iw, obj);

   obj->any.bounds.x1 = x;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddString16								*/
/*									*/
/* Does the work for XvicImageDrawString16 and				*/
/* XvicImageDrawImageString16.						*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddString16(iw, id, gc, fg, color, x, y, string, length, type, justify)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor fg;
   XvicColor color;
   double x;
   double y;
   XChar2b *string;
   int length;
   int justify;
   GrType type;
#else
AddString16(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor fg,
   XvicColor color,
   double x,
   double y,
   XChar2b *string,
   int length,
   int justify,
   GrType type)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;

   if (type == GrImageString16) {
      if (!CheckValidGrColor(iw, fg))
         return 0;
   }

   obj = NewObject(iw, id, gc, color, sizeof(GrString16Object));
   if (!obj)
      return 0;
   obj->type = type;

   obj->anystr.justify = justify;
   obj->string16.fg = fg;
   obj->string16.x = x;
   obj->string16.y = y;
   obj->string16.length = length;

   obj->string16.string = _XvicMalloc(biw, length * sizeof(XChar2b));
   memcpy((void *)obj->string16.string, (void *)string, length*sizeof(XChar2b));

   /* Calculate the bounding box */

   CalcStringBounds(iw, obj);

   obj->any.bounds.x1 = x;
   obj->any.bounds.x2 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.y2 = y;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddLines								*/
/*									*/
/* Does the work for XvicImageDrawLines, XvicImageFillPolygon, and	*/
/* XvicImageDrawPoints.							*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddLines(iw, id, gc, color, points, npoints, shape, mode, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicPoint *points;
   int npoints;
   int shape;
   int mode;
   GrType type;
#else
AddLines(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicPoint *points,
   int npoints,
   int shape,
   int mode,
   GrType type)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;
   double x, y;
   int i;

   obj = NewObject(iw, id, gc, color, sizeof(GrLinesObject));
   if (!obj)
      return 0;
   obj->type = type;

   obj->lines.mode = mode;
   obj->lines.shape = shape;
   obj->lines.npoints = npoints;
   obj->lines.points = (XvicPoint *)_XvicMalloc(biw, npoints*sizeof(XvicPoint));
   memcpy((void *)obj->lines.points, (void *)points, npoints*sizeof(XvicPoint));

   /* Calculate the bounding box */

   if (npoints == 0) {			/* huh? */
      obj->any.bounds.x1 = 0;
      obj->any.bounds.y1 = 0;
      obj->any.bounds.x2 = 0;
      obj->any.bounds.y2 = 0;
   }
   else {
      x = points[0].x;
      y = points[0].y;
      obj->any.bounds.x1 = x;
      obj->any.bounds.y1 = y;
      obj->any.bounds.x2 = x;
      obj->any.bounds.y2 = y;

      for (i=1; i<npoints; i++) {
         if (mode == CoordModePrevious) {
            x += points[i].x;
            y += points[i].y;
         }
         else {
            x = points[i].x;
            y = points[i].y;
         }
         if (x < obj->any.bounds.x1)
            obj->any.bounds.x1 = x;
         if (x > obj->any.bounds.x2)
            obj->any.bounds.x2 = x;
         if (y < obj->any.bounds.y1)
            obj->any.bounds.y1 = y;
         if (y > obj->any.bounds.y2)
            obj->any.bounds.y2 = y;
      }
   }

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddRectangle								*/
/*									*/
/* Does the work for XvicImageDrawRectangle and XvicImageFillRectangle.	*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddRectangle(iw, id, gc, color, x, y, width, height, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   double x;
   double y;
   double width;
   double height;
   GrType type;
#else
AddRectangle(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   double x,
   double y,
   double width,
   double height,
   GrType type)
#endif /* _NO_PROTO */
{
   GrObject *obj;

   obj = NewObject(iw, id, gc, color, sizeof(GrRectangleObject));
   if (!obj)
      return 0;
   obj->type = type;

   if (width < 1.0)
      width = 1.0;
   if (height < 1.0)
      height = 1.0;

   obj->rectangle.x = x;
   obj->rectangle.y = y;
   obj->rectangle.width = width;
   obj->rectangle.height = height;

   obj->any.bounds.x1 = x;
   obj->any.bounds.y1 = y;
   obj->any.bounds.x2 = x + width - 1.0;
   obj->any.bounds.y2 = y + height - 1.0;

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AddRectangles							*/
/*									*/
/* Does the work for XvicImageDrawRectangles and			*/
/* XvicImageFillRectangles.						*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
AddRectangles(iw, id, gc, color, rectangles, nrectangles, type)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   XvicRectangle *rectangles;
   int nrectangles;
   GrType type;
#else
AddRectangles(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   XvicRectangle *rectangles,
   int nrectangles,
   GrType type)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GrObject *obj;
   int i;

   obj = NewObject(iw, id, gc, color, sizeof(GrRectanglesObject));
   if (!obj)
      return 0;
   obj->type = type;

   obj->rectangles.nrectangles = nrectangles;
   obj->rectangles.rectangles = (XvicRectangle *)_XvicMalloc(biw,
			nrectangles * sizeof(XvicRectangle));
   memcpy((void *)obj->rectangles.rectangles, (void *)rectangles,
			nrectangles * sizeof(XvicRectangle));

#define RECT(i) obj->rectangles.rectangles[i]

   /* Calculate the bounding box */

   if (nrectangles == 0) {			/* huh? */
      obj->any.bounds.x1 = 0;
      obj->any.bounds.y1 = 0;
      obj->any.bounds.x2 = 0;
      obj->any.bounds.y2 = 0;
   }
   else {
      if (RECT(0).width < 1.0)
         RECT(0).width = 1.0;
      if (RECT(0).height < 1.0)
         RECT(0).height = 1.0;
      obj->any.bounds.x1 = RECT(0).x;
      obj->any.bounds.y1 = RECT(0).y;
      obj->any.bounds.x2 = RECT(0).x + RECT(0).width - 1.0;
      obj->any.bounds.y2 = RECT(0).y + RECT(0).height - 1.0;

      for (i=1; i<nrectangles; i++) {
         if (RECT(i).width < 1.0)
            RECT(i).width = 1.0;
         if (RECT(i).height < 1.0)
            RECT(i).height = 1.0;
         if (RECT(i).x < obj->any.bounds.x1)
            obj->any.bounds.x1 = RECT(i).x;
         if (RECT(i).x + RECT(i).width - 1.0 > obj->any.bounds.x2)
            obj->any.bounds.x2 = RECT(i).x + RECT(i).width - 1.0;
         if (RECT(i).y < obj->any.bounds.y1)
            obj->any.bounds.y1 = RECT(i).y;
         if (RECT(i).y + RECT(i).height - 1.0 > obj->any.bounds.y2)
            obj->any.bounds.y2 = RECT(i).y + RECT(i).height - 1.0;
      }
   }

   DrawObject(iw, obj, NULL);

   return obj->any.id;

}

/************************************************************************/
/* AllocOverlayColor							*/
/*									*/
/* Allocates a color from the overlay colormap and returns it in	*/
/* color->xcolor.pixel.  If no colors are left unallocated, False is	*/
/* returned.  This routine does not attempt to find the closest color	*/
/* but will return an already-allocated exact match.  cells and cmap	*/
/* are only included to keep them updated for GetClosestColor.		*/
/* This routine must not be called if a hardware overlay is not in use.	*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
AllocOverlayColor(iw, color, cells_arg, cmap)
   XvicImageWidget iw;
   GrColor *color;
   XColor *cells_arg;
   Colormap cmap;
#else
AllocOverlayColor(
   XvicImageWidget iw,
   GrColor *color,
   XColor *cells_arg,
   Colormap cmap)
#endif /* _NO_PROTO */
{
   int i, index;
   XColor cells_local[CMAP_SIZE_MAX];
   XColor *cells;

   cells = cells_arg;
   if (cells == NULL) {
      for (i=0; i<iw->image.overlay_cmap_size; i++)
         cells_local[i].pixel = i;
      XQueryColors(XtDisplay(iw), cmap, cells_local,
		iw->image.overlay_cmap_size);
      cells = cells_local;
   }

   index = -1;

   for (i=0; i<iw->image.overlay_cmap_size; i++) {

      /* If this is the transparent pixel, skip it */

      if (((iw->image.overlay_type == TransparentPixel) &&
	   (i == iw->image.overlay_value)) ||
	  ((iw->image.overlay_type == TransparentMask) &&
	   ((i & iw->image.overlay_value) != 0)))
         continue;

      if (iw->image.private_cmap_refcnt[i] == 0)
         index = i;
      else {
         if (cells[i].red == color->xcolor.red &&
             cells[i].green == color->xcolor.green &&
             cells[i].blue == color->xcolor.blue) {
            color->xcolor.pixel = i;		/* exact match w/existing */
            iw->image.private_cmap_refcnt[i]++;
            color->alloc_pvt = True;
            return True;
         }
      }
   }

   if (index < 0)		/* No empty slots */
      return False;

   color->xcolor.pixel = index;
   color->xcolor.flags = DoRed | DoGreen | DoBlue;
   XStoreColors(XtDisplay(iw), cmap, &color->xcolor, 1);
   memcpy((void *)&cells[color->xcolor.pixel], (void *)&color->xcolor,
		sizeof(XColor));

   iw->image.private_cmap_refcnt[color->xcolor.pixel]++;
   color->alloc_pvt = True;

   return True;

}

/************************************************************************/
/* AllocPrivateColor							*/
/*									*/
/* Allocates a color from the private colormap and returns it in	*/
/* color->xcolor.pixel.  It must come from the lower half of the	*/
/* colormap, since this is only used for colormapPolicy of HALF.	*/
/* If no colors are left unallocated, False is returned.		*/
/* This routine does not attempt to find the closest color but will	*/
/* return an already-allocated exact match.  cells and cmap are		*/
/* only included to keep them updated for GetClosestColor.		*/
/* This routine must not be called if a hardware overlay is in use.	*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
AllocPrivateColor(iw, color, cells_arg, cmap)
   XvicImageWidget iw;
   GrColor *color;
   XColor *cells_arg;
   Colormap cmap;
#else
AllocPrivateColor(
   XvicImageWidget iw,
   GrColor *color,
   XColor *cells_arg,
   Colormap cmap)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int i, index;
   XColor cells_local[CMAP_SIZE_MAX/2];
   XColor *cells;

   cells = cells_arg;
   if (cells == NULL) {
      for (i=0; i<CMAP_SIZE/2; i++)
         cells_local[i].pixel = i;
      XQueryColors(XtDisplay(iw), cmap, cells_local, CMAP_SIZE/2);
      cells = cells_local;
   }

   index = -1;

   for (i=0; i<CMAP_SIZE/2; i++) {

      /* Doing it this way ensures that we will get the highest index	*/
      /* available in the colormap, which is what we really want.	*/

      if (iw->image.private_cmap_refcnt[i] == 0)
         index = i;
      else {
         if (cells[i].red == color->xcolor.red &&
             cells[i].green == color->xcolor.green &&
             cells[i].blue == color->xcolor.blue) {
            color->xcolor.pixel = i;		/* exact match w/existing */
            iw->image.private_cmap_refcnt[i]++;
            color->alloc_pvt = True;
            return True;
         }
      }
   }

   if (index < 0)		/* No empty slots */
      return False;

   color->xcolor.pixel = index;
   color->xcolor.flags = DoRed | DoGreen | DoBlue;
   XStoreColors(XtDisplay(iw), cmap, &color->xcolor, 1);
   memcpy((void *)&cells[color->xcolor.pixel], (void *)&color->xcolor,
		sizeof(XColor));

   iw->image.private_cmap_refcnt[color->xcolor.pixel]++;
   color->alloc_pvt = True;

   return True;

}

/************************************************************************/
/* AllocSystemColor							*/
/*									*/
/* Allocates a color from the system colormap and returns it in		*/
/* color->xcolor.pixel.  If lower_half is True, color must come from	*/
/* the bottom half of the colormap.  cells is updated if non-NULL in	*/
/* order to keep GetClosestColor() happy.				*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
AllocSystemColor(iw, color, cells, lower_half)
   XvicImageWidget iw;
   GrColor *color;
   XColor *cells;
   Boolean lower_half;
#else
AllocSystemColor(
   XvicImageWidget iw,
   GrColor *color,
   XColor *cells,
#if NeedWidePrototypes
   int lower_half)
#else
   Boolean lower_half)
#endif
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   Display *dpy;
   Boolean status;

   dpy = XtDisplay(iw);

   status = XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)),
			&color->xcolor);
   if (!status)				/* Couldn't get it */
      return False;

   /* If it's supposed to be in the lower half, make sure it is */

   if (lower_half && color->xcolor.pixel >= CMAP_SIZE/2) {
      XFreeColors(dpy, DefaultColormap(dpy, DefaultScreen(dpy)),
		&color->xcolor.pixel, 1, 0);
      return False;
   }

   iw->image.default_cmap_refcnt[color->xcolor.pixel]++;
   color->alloc_def = True;

   if (cells != NULL)
      memcpy((void *)&cells[color->xcolor.pixel], (void *)&color->xcolor,
		sizeof(XColor));

   return True;

}

/************************************************************************/
/* BoundingRect								*/
/*									*/
/* Returns the bounding rectangle of a graphics object in Image		*/
/* coordinates.  The bounding rectangle is basically determined when	*/
/* the object is created, but might need to be expanded to handle	*/
/* line widths, etc.  These are in Scr coordinates so they must be	*/
/* converted to a worst-case Image coordinate size before applying them	*/
/* to the bounding box.  Note that in some cases, a tighter bounding	*/
/* box could be achieved by e.g. actually calculating the line angle,	*/
/* but this is not worthwhile.  We just pick something that is		*/
/* definitely bigger than the object.					*/
/*									*/
/* A pointer to an _XvicRect is returned; this may point either at	*/
/* obj->bounds or at a static _XvicRect, so this value must be used	*/
/* or copied before BoundingRect() is called again.			*/
/*									*/
/* The returned bounding box is in integer Image coordinates.		*/
/* Fractional coordinates are rounded to the integer pixel in which	*/
/* they appear.  This does not cause a problem with bounding box	*/
/* calculation because the callers assume the bounding box goes from	*/
/* the left side of the left pixel to the right side of the right	*/
/* pixel, thereby covering the entire Image-coordinate pixel (the same	*/
/* happens vertically).							*/
/************************************************************************/

static _XvicRect *
#ifdef _NO_PROTO
BoundingRect(iw, obj)
   XvicImageWidget iw;
   GrObject *obj;
#else
BoundingRect(
   XvicImageWidget iw,
   GrObject *obj)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   static _XvicRect rect;
   int w;
   int x_delta, y_delta;

   rect.x1 = ROUND(obj->any.bounds.x1);
   rect.x2 = ROUND(obj->any.bounds.x2);
   rect.y1 = ROUND(obj->any.bounds.y1);
   rect.y2 = ROUND(obj->any.bounds.y2);

   switch (obj->type) {

      /* No extension; the bounding rectangle doesn't change.  However,	*/
      /* we must subtract 1 from the top to make sure we cover the edge	*/
      /* at all different zooms.					*/

      case GrFillArc:
      case GrFillArcs:
      case GrFillRectangle:
      case GrFillRectangles:
      case GrFillPolygon:
      case GrPoint:
      case GrPoints:
         rect.x1 -= MAX(UNZOOMX(1),1); /*-1 cuz X draws boundaries on top/left*/
         rect.y1 -= MAX(UNZOOMY(1),1);
         return &rect;

      /* Extend bounding rectangle by half of line width */

      case GrArc:
      case GrLine:
      case GrRectangle:
      case GrRectangles:
      case GrSegments:
         w = 0;
         CheckValidGrGC(iw, obj->any.gc, True);
         if (iw->image.gr_gc_list[obj->any.gc].mask & GCLineWidth)
            w = iw->image.gr_gc_list[obj->any.gc].values.line_width;
         if (w == 0)
            w = 1;
         x_delta = MAX(UNZOOMX(((w+1)/2)+1),1);
         y_delta = MAX(UNZOOMY(((w+1)/2)+1),1);
         rect.x1 -= x_delta;
         rect.y1 -= y_delta;
         rect.x2 += x_delta;
         rect.y2 += y_delta;

         return &rect;

      /* Extend bounding rectangle for possible mitered corners.	*/
      /* Maximum miter value is line_width/(2*sin(theta/2)) where	*/
      /* theta is the minimum angle specified in the X docs, 11 degrees.*/
      /* This works out to 5.2167*line_width, or to avoid float, 6.	*/

      case GrArcs:
      case GrLines:
         w = 0;
         CheckValidGrGC(iw, obj->any.gc, True);
         if (iw->image.gr_gc_list[obj->any.gc].mask & GCLineWidth)
            w = iw->image.gr_gc_list[obj->any.gc].values.line_width;
         if (w == 0)
            w = 1;
         x_delta = MAX(UNZOOMX(w*6),1);
         y_delta = MAX(UNZOOMY(w*6),1);
         rect.x1 -= x_delta;
         rect.y1 -= y_delta;
         rect.x2 += x_delta;
         rect.y2 += y_delta;

         return &rect;

      /* Extend bounding rectangle by size of bitmap */

      case GrBitmap:
         rect.x1 -= MAX(UNZOOMX(obj->bitmap.hot_x+1),1);
         rect.y1 -= MAX(UNZOOMY(obj->bitmap.hot_y+1),1);
         rect.x2 += MAX(UNZOOMX(obj->bitmap.width - obj->bitmap.hot_x + 1),1);
         rect.y2 += MAX(UNZOOMY(obj->bitmap.height - obj->bitmap.hot_y + 1),1);

         return &rect;

      /* Extend bounding rectangle by size of string */

      case GrImageString:
      case GrImageString16:
      case GrString:
      case GrString16:
      case GrText:
      case GrText16:
         switch (obj->anystr.justify) {
            case XvicJUST_RIGHT:
               w = obj->anystr.width;
               if (obj->anystr.lbearing < 0)
                  w -= obj->anystr.lbearing;
               rect.x1 -= MAX(UNZOOMX(w+1),1);
               rect.x2 += MAX(UNZOOMX(
			MAX(obj->anystr.rbearing-obj->anystr.width+1, 0)),1);
               break;
            case XvicJUST_CENTER:
               w = obj->anystr.width / 2;
               if (obj->anystr.lbearing < 0)
                  w -= obj->anystr.lbearing;
               rect.x1 -= MAX(UNZOOMX(w+1),1);
               rect.x2 += MAX(UNZOOMX((obj->anystr.width / 2) + 1 +
			MAX(obj->anystr.rbearing-obj->anystr.width,0)),1);
               break;
            default:		/* XvicJUST_LEFT */
               rect.x1 -= MAX(UNZOOMX(1),1);
               rect.x2 += MAX(UNZOOMX(
			MAX(obj->anystr.rbearing,obj->anystr.width)+1),1);
               break;
         }
         rect.y1 -= MAX(UNZOOMY(obj->anystr.ascent+1),1);
         rect.y2 += MAX(UNZOOMY(obj->anystr.descent+1),1);

         return &rect;

      default:
         FatalError(iw, "BadObjectType3",
		  "Internal error: Unknown object type in BoundingRect().");
   }

   return &rect;	/* to make compiler happy; never reached */
}

/************************************************************************/
/* CalcStringBounds							*/
/*									*/
/* Calculates the bounds of a string object (of any type).  This must	*/
/* be done when the object is created, and again if the font changes.	*/
/* Note that the bounds are stored in Screen coordinates; BoundingRect	*/
/* must still be called to get the real bounds for a given zoom.	*/
/* These values are calculated at creation/font change instead of in	*/
/* BoundingRect to avoid having to call XQueryTextExtents() every time	*/
/* we have to do an expose!						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CalcStringBounds(iw, obj)
   XvicImageWidget iw;
   GrObject *obj;
#else
CalcStringBounds(
   XvicImageWidget iw,
   GrObject *obj)
#endif /* _NO_PROTO */
{
   Font font;
   int direct;
   int ascent, descent;
   XCharStruct overall;
   int x, i;
   XTextItem *item;
   XTextItem16 *item16;

   CheckValidGrGC(iw, obj->any.gc, True);
   if (iw->image.gr_gc_list[obj->any.gc].mask & GCFont)
      font = iw->image.gr_gc_list[obj->any.gc].values.font;
   else {
      /* We're missing a font.  This is okay only for Text and Text16,	*/
      /* where the first item specifies the font.			*/
      if ((obj->type == GrText &&
           obj->text.nitems > 0 && obj->text.items[0].font) ||
          (obj->type == GrText16 &&
           obj->text16.nitems > 0 && obj->text16.items[0].font))
         font = None;
      else {
         obj->anystr.ascent = 0;
         obj->anystr.descent = 0;
         obj->anystr.width = 0;
         obj->anystr.rbearing = 0;
         obj->anystr.lbearing = 0;
         return;		/* Unspecified font means do nothing */
      }
   }

   switch (obj->type) {

      case GrImageString:
      case GrString:
         XQueryTextExtents(XtDisplay((Widget)iw), font, obj->string.string,
		obj->string.length, &direct, &ascent, &descent, &overall);
         obj->anystr.ascent = ascent;
         obj->anystr.descent = descent;
         obj->anystr.width = overall.width;
         obj->anystr.rbearing = overall.rbearing;
         obj->anystr.lbearing = overall.lbearing;

         break;

      case GrImageString16:
      case GrString16:
         XQueryTextExtents16(XtDisplay((Widget)iw), font, obj->string16.string,
		obj->string16.length, &direct, &ascent, &descent, &overall);
         obj->anystr.ascent = ascent;
         obj->anystr.descent = descent;
         obj->anystr.width = overall.width;
         obj->anystr.rbearing = overall.rbearing;
         obj->anystr.lbearing = overall.lbearing;

         break;

      case GrText:
         obj->anystr.ascent = 0;
         obj->anystr.descent = 0;
         obj->anystr.width = 0;
         obj->anystr.rbearing = 0;
         x = 0;
         for (i=0; i<obj->text.nitems; i++) {
            item = &obj->text.items[0];
            if (item->font)
               font = item->font;
            XQueryTextExtents(XtDisplay((Widget)iw), font, item->chars,
		item->nchars, &direct, &ascent, &descent, &overall);
            x += item->delta;
            obj->anystr.ascent = MAX(obj->anystr.ascent, ascent);
            obj->anystr.descent = MAX(obj->anystr.descent, descent);
            obj->anystr.width = MAX(obj->anystr.width, x + overall.width);
            if (i == 0)
               obj->anystr.lbearing = x + overall.lbearing;
            else
               obj->anystr.lbearing = MIN(obj->anystr.lbearing,
					  x + overall.lbearing);
            x += overall.rbearing;
            obj->anystr.rbearing = MAX(overall.rbearing, x);
         }
         break;

      case GrText16:
         obj->anystr.ascent = 0;
         obj->anystr.descent = 0;
         obj->anystr.width = 0;
         obj->anystr.rbearing = 0;
         x = 0;
         for (i=0; i<obj->text16.nitems; i++) {
            item16 = &obj->text16.items[0];
            if (item16->font)
               font = item16->font;
            XQueryTextExtents16(XtDisplay((Widget)iw), font, item16->chars,
		item16->nchars, &direct, &ascent, &descent, &overall);
            x += item16->delta;
            obj->anystr.ascent = MAX(obj->anystr.ascent, ascent);
            obj->anystr.descent = MAX(obj->anystr.descent, descent);
            obj->anystr.width = MAX(obj->anystr.width, x + overall.width);
            if (i == 0)
               obj->anystr.lbearing = x + overall.lbearing;
            else
               obj->anystr.lbearing = MIN(obj->anystr.lbearing,
					  x + overall.lbearing);
            x += overall.rbearing;
            obj->anystr.rbearing = MAX(overall.rbearing, x);
         }
         break;

      default:
         break;		/* Do nothing for other types */
   }

}

/************************************************************************/
/* CallCursorCallback							*/
/*									*/
/* Call the cursorCallback callback routine.  Setting force_off_screen	*/
/* to True causes the on_screen flag to be False regardless of what the	*/
/* coordinates are.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CallCursorCallback(iw, x, y, force_off_screen)
   XvicImageWidget iw;
   double x;
   double y;
   Boolean force_off_screen;
#else
CallCursorCallback(
   XvicImageWidget iw,
   double x,
   double y,
#if NeedWidePrototypes
   int force_off_screen)
#else
   Boolean force_off_screen)
#endif
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XvicImageCallbackStruct cb;

   if (iw->image.cursor_callback) {
      cb.reason = XvicCR_CURSOR;
      cb.x = ROUND(x);
      cb.y = ROUND(y);
      cb.x_fp = x;
      cb.y_fp = y;

      cb.on_screen = True;
      if ((X1_Img2Scr(cb.x) < 0) ||
	  (X2_Img2Scr(cb.x) >= (int)biw->bim.view_width))
         cb.on_screen = False;
      if ((Y1_Img2Scr(cb.y) < 0) ||
	  (Y2_Img2Scr(cb.y) >= (int)biw->bim.view_height))
         cb.on_screen = False;

      if (force_off_screen)
         cb.on_screen = False;

      XtCallCallbackList((Widget)iw, iw->image.cursor_callback, (XtPointer)&cb);
   }
}

/************************************************************************/
/* CallPanCallback							*/
/*									*/
/* Call the panCallback callback routine.  Called only when the widget	*/
/* changes the pan value, due to scrollbars or actions, not when the	*/
/* application sets xPan or yPan directly.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CallPanCallback(iw)
   XvicImageWidget iw;
#else
CallPanCallback(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicImageCallbackStruct cb;

   cb.reason = XvicCR_PAN;
   cb.x_pan = iw->bim.x_pan;
   cb.y_pan = iw->bim.y_pan;

   if (iw->image.pan_callback)
      XtCallCallbackList((Widget)iw, iw->image.pan_callback, (XtPointer) &cb);
}

/************************************************************************/
/* CheckValidGrColor							*/
/*									*/
/* Checks whether the given XvicColor value is valid (allocated) and	*/
/* issues a warning message if not.					*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CheckValidGrColor(iw, color)
   XvicImageWidget iw;
   XvicColor color;
#else
CheckValidGrColor(
   XvicImageWidget iw,
   XvicColor color)
#endif /* _NO_PROTO */
{
   if (color >= 0 && color < iw->image.num_gr_colors) {
      if (iw->image.gr_colors[color].active)
         return True;				/* it's okay */
   }

   WarningMsg(iw, "InvalidXvicColor",
	"The specified XvicColor value has not been allocated.");

   return False;
}

/************************************************************************/
/* CheckValidGrGC							*/
/*									*/
/* Checks whether the given XvicGC value is valid (allocated) and,	*/
/* if print is True, issues a warning message if it's not valid.	*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CheckValidGrGC(iw, gr_gc, print)
   XvicImageWidget iw;
   XvicGC gr_gc;
   int print;
#else
CheckValidGrGC(
   XvicImageWidget iw,
   XvicGC gr_gc,
   int print)
#endif /* _NO_PROTO */
{
   if (gr_gc >= 0 && gr_gc < iw->image.num_gr_gc) {
      if (iw->image.gr_gc_list[gr_gc].active)
         return True;				/* it's okay */
   }

   if (print)
      WarningMsg(iw, "InvalidXvicGC",
	"The specified XvicGC value has not been allocated.");

   return False;
}

/************************************************************************/
/* ClearOverlayRegion							*/
/*									*/
/* Clear the overlay window over a given region (in Image coords).	*/
/* The caller must check to make sure the overlay exists and is		*/
/* realized.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ClearOverlayRegion(iw, rgn)
   XvicImageWidget iw;
   _XvicRegion *rgn;
#else
ClearOverlayRegion(
   XvicImageWidget iw,
   _XvicRegion *rgn)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   _XvicRect *img_rect;
   int i, n;
   int x, y, width, height;

   img_rect = _XvicRegionGetRectangles(rgn);
   n = _XvicRegionGetNumRects(rgn);

   for (i=0; i<n; i++) {
      x = X1_Img2Scr(img_rect[i].x1);
      y = Y1_Img2Scr(img_rect[i].y1);
      width = X2_Img2Scr(img_rect[i].x2) - x + 1;
      height = Y2_Img2Scr(img_rect[i].y2) - y + 1;
      X_ViewConstrain(x, width);
      Y_ViewConstrain(y, height);

      if (width > 0 && height > 0)
         XClearArea(XtDisplay((Widget)iw), XtWindow(iw->image.overlay_widget),
			x, y, width, height, False);
   }
}

/************************************************************************/
/* ConfigureScrollbars							*/
/*									*/
/* Called whenever anything that might possibly affect the scrollbars	*/
/* changes.  This routine determines if the scrollbars are needed,	*/
/* creates or destroys them if needed, manages or unmanages them if	*/
/* it's changed, and sets all the relevant scrollbar resources.		*/
/* Note that this routine specifically does *NOT* set the scrollbar's	*/
/* value.  That is affected only by a pan change, and is caught		*/
/* elsewhere.  (It does actually make sure the value is within range,	*/
/* but that is only to avoid an annoying warning message from ScrollBar.*/
/* The value is truly set elsewhere, and may in fact be set out of	*/
/* bounds if the pan is not constrained.  This setting doesn't affect	*/
/* any callbacks or anything).						*/
/*									*/
/* There is a potential problem here if this is called due to the	*/
/* BasicImage's Resize callback.  Basically, when ScrolledWindow's	*/
/* Resize is called, it sets a flag saying that it is doing a resize.	*/
/* It then calls our QueryGeometry routine, where we manage/unmanage	*/
/* the scrollbars appropriately.  SW assumes that's the gospel.  Then,	*/
/* after positioning the scrollbars (if needed), it calls our Resize	*/
/* routine, which calls BasicImage's Resize, which calls the application*/
/* Resize callback.  If the Resize callback sets the zoom factor, our	*/
/* SetValues will call this routine again, which may come up with a	*/
/* different answer and remanage the scrollbars.  But, SW ignores this	*/
/* remanagement because of the flag it set (Resize is supposed to be	*/
/* a command not a request, so it shouldn't generate more configuration	*/
/* calls).  SW has to do this to avoid infinite recursion loops.  The	*/
/* scrollbars can thus appear somewhere they shouldn't (namely, the	*/
/* last position they were placed in).  The practical upshot of all	*/
/* this is that the Resize callback can't change anything that would	*/
/* affect whether or not the scrollbars are displayed.  If this is	*/
/* really needed, the application should set a work proc or a timer	*/
/* with value 0 to fire off back at the event loop, so that we are not	*/
/* buried deep within the Resize code when these things are changed.	*/
/* This is reflected in the docs.  Note that this is only a problem	*/
/* with XvicAS_NEEDED - the other two modes won't change scrollbar	*/
/* display based on SetValues.  Yikes!					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ConfigureScrollbars(iw)
   XvicImageWidget iw;
#else
ConfigureScrollbars(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int view_size, value, slider_size;
   Widget manage_list[2], unmanage_list[2];
   int manage_count, unmanage_count;

   if (!iw->image.scroll_win)
      return;			/* No scrolled window, nothing to do */

   /* Since Xt(Un)ManageChild() will cause a reconfig which will call	*/
   /* this routine again, defer all management to happen at once.	*/

   manage_count = 0;
   unmanage_count = 0;

   /* STATIC display policy, turn the scrollbars on unconditionally */

   if (iw->image.scrollbar_display_policy == XvicSTATIC) {
      if (!iw->image.h_scrollbar || !iw->image.v_scrollbar)
         CreateScrollbars(iw);

      if (!iw->image.hsb_managed) {
         iw->image.hsb_managed = TRUE;
         manage_list[manage_count++] = (Widget) iw->image.h_scrollbar;
      }
      if (!iw->image.vsb_managed) {
         iw->image.vsb_managed = TRUE;
         manage_list[manage_count++] = (Widget) iw->image.v_scrollbar;
      }
   }

   /* AS_NEEDED display policy, check to see if the scrollbars are	*/
   /* needed.  We don't worry about the case where the presence of one	*/
   /* affects the other, because the act of managing or unmanaging a	*/
   /* scrollbar doesn't directly affect the work widget's actual size.	*/
   /* However, it should cause the QueryGeometry method to be called,	*/
   /* which *does* worry about that case.				*/

   else if (iw->image.scrollbar_display_policy == XvicAS_NEEDED) {
      if (!iw->image.h_scrollbar || !iw->image.v_scrollbar)
         CreateScrollbars(iw);

      /* Look at the image and view dimensions and manage/unmanage the	*/
      /* scrollbars accordingly.					*/

      if ((int)iw->bim.view_width < X2_Img2Dpy(iw->bim.image_width)) {
         if (!iw->image.hsb_managed) {			/* HSB needed */
            iw->image.hsb_managed = TRUE;
            manage_list[manage_count++] = (Widget) iw->image.h_scrollbar;
         }
      }
      else {						/* HSB not needed */
         if (iw->image.hsb_managed) {
            iw->image.hsb_managed = FALSE;
            unmanage_list[unmanage_count++] = (Widget) iw->image.h_scrollbar;
         }
      }

      if ((int)iw->bim.view_height < Y2_Img2Dpy(iw->bim.image_height)) {
         if (!iw->image.vsb_managed) {			/* VSB needed */
            iw->image.vsb_managed = TRUE;
            manage_list[manage_count++] = (Widget) iw->image.v_scrollbar;
         }
      }
      else {						/* VSB not needed */
         if (iw->image.vsb_managed) {
            iw->image.vsb_managed = FALSE;
            unmanage_list[unmanage_count++] = (Widget) iw->image.v_scrollbar;
         }
      }
   }

   /* NEVER display policy, get rid of them pesky bars! */

   else {			/* XvicNEVER */
      if (iw->image.h_scrollbar || iw->image.v_scrollbar)
         DestroyScrollbars(iw);
   }

   /* Now, if a scrollbar is managed, set its size-related resources.	*/
   /* We don't use XmScrollBarSetValues because we want to change	*/
   /* min and max simultaneously.					*/

   if (iw->image.hsb_managed) {
      XtVaGetValues((Widget)iw->image.h_scrollbar, XmNvalue, &value, NULL);
      view_size = X_Dpy2Img(iw->bim.view_width);
      slider_size = MAX(MIN(view_size, iw->bim.image_width), 1);
      if (value > iw->bim.image_width - slider_size)
         value = iw->bim.image_width - slider_size;
      XtVaSetValues((Widget)iw->image.h_scrollbar,
		XmNincrement, 1,
		XmNminimum, 0,
		XmNmaximum, iw->bim.image_width,
		XmNpageIncrement, MAX(MIN(view_size/2, iw->bim.image_width),1),
		XmNsliderSize, slider_size,
		XmNvalue, value,
		NULL);
   }
   if (iw->image.vsb_managed) {
      XtVaGetValues((Widget)iw->image.v_scrollbar, XmNvalue, &value, NULL);
      view_size = Y_Dpy2Img(iw->bim.view_height);
      slider_size = MAX(MIN(view_size, iw->bim.image_height), 1);
      if (value > iw->bim.image_height - slider_size)
         value = iw->bim.image_height - slider_size;
      XtVaSetValues((Widget)iw->image.v_scrollbar,
		XmNincrement, 1,
		XmNminimum, 0,
		XmNmaximum, iw->bim.image_height,
		XmNpageIncrement, MAX(MIN(view_size/2, iw->bim.image_height),1),
		XmNsliderSize, slider_size,
		XmNvalue, value,
		NULL);
   }

   /* Actually do the managing now. */

   if (manage_count)
      XtManageChildren(manage_list, manage_count);
   if (unmanage_count)
      XtUnmanageChildren(unmanage_list, unmanage_count);

}

/************************************************************************/
/* CreateGrColorDitherPattern						*/
/*									*/
/* Create the dither pattern for a graphics color.  We must have	*/
/* already determined that the dither pattern is appropriate.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CreateGrColorDitherPattern(iw, color)
   XvicImageWidget iw;
   GrColor *color;
#else
CreateGrColorDitherPattern(
   XvicImageWidget iw,
   GrColor *color)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int type;

   if (biw->bim.colormap_policy == XvicDITHER ||
       (biw->bim.colormap_policy == XvicALLOC &&
        (biw->bim.image_mode==XvicCOLOR || biw->bim.dither_mode==XvicKAGELS)))
      type = XvicKAGELS;
   else
      type = XvicORDERED;

   _XvicGetDitherPixmap(biw, &color->gc_tile, &color->width, &color->height,
	type, color->xcolor.red, color->xcolor.green, color->xcolor.blue);

}

/************************************************************************/
/* CreateOverlayWidget							*/
/*									*/
/* Create the widget for the hardware overlay, if available.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CreateOverlayWidget(iw)
   XvicImageWidget iw;
#else
CreateOverlayWidget(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   unsigned long *pixels;
   int status;

   iw->image.overlay_widget = NULL;

   GetHWOverlayVisual(iw);
   if (!iw->image.overlay_visual)
      return;					/* No HW overlay available */

   /* The SGI server does not allow us to use AllocAll because it	*/
   /* claims that the transparent color is not allocatable, yet it is	*/
   /* included in the colormap.  HP does this better by advertising one	*/
   /* less cell in colormap_size, so that we can in fact use AllocAll.	*/
   /* So, we must use AllocNone then manually allocate cmap_size - 1	*/
   /* cells.  SGI claims all their servers work this way so we can use	*/
   /* ServerVendor.							*/

   if (strcmp(ServerVendor(XtDisplay((Widget)iw)), "Silicon Graphics") == 0) {
      iw->image.overlay_colormap = XCreateColormap(XtDisplay((Widget)iw),
		RootWindowOfScreen(XtScreen((Widget)iw)),
		iw->image.overlay_visual,
		AllocNone);
      pixels = (unsigned long *) _XvicMalloc(biw,
			iw->image.overlay_cmap_size * sizeof(unsigned long));
      status = XAllocColorCells(XtDisplay((Widget)iw),
		iw->image.overlay_colormap, True, NULL, 0,
		pixels, iw->image.overlay_cmap_size - 1);
      _XvicFree(biw, pixels,
			iw->image.overlay_cmap_size * sizeof(unsigned long));
      if (status == 0)
         FatalError(iw, "BadSGIColormap",
		  "Can't allocate colors for overlay colormap on SGI server (shouldn't happen).");

   }
   else
      iw->image.overlay_colormap = XCreateColormap(XtDisplay((Widget)iw),
		RootWindowOfScreen(XtScreen((Widget)iw)),
		iw->image.overlay_visual,
		AllocAll);

   iw->image.overlay_widget = XtVaCreateWidget("overlay",
		xvicImageOverlayWidgetClass, (Widget)iw,
		XtNheight, iw->bim.view_height,
		XtNwidth, iw->bim.view_width,
		XtNborderWidth, 0,
		XtNdepth, iw->image.overlay_depth,
		XtNx, iw->bim.x_dpy_off,
		XtNy, iw->bim.y_dpy_off,
		XtNbackground, iw->image.overlay_value,
		XtNcolormap, iw->image.overlay_colormap,
		XvicNvisual, iw->image.overlay_visual,
		NULL);

   if (XtIsRealized((Widget)iw)) {
      XtRealizeWidget(iw->image.overlay_widget);
      XtMapWidget(iw->image.overlay_widget);
      GetHWOverlayGC(iw);
   }
}

/************************************************************************/
/* CreateScrollbars							*/
/*									*/
/* Create the scrollbar widgets.  Only called from Initialize, or	*/
/* when scrollBarDisplayPolicy changes from XvicNEVER to something	*/
/* else.  Assumes that the parent is, in fact a ScrolledWindow.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CreateScrollbars(iw)
   XvicImageWidget iw;
#else
CreateScrollbars(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{

   if (!iw->image.h_scrollbar) {
      iw->image.h_scrollbar = (XmScrollBarWidget) XtVaCreateWidget(
		"hscroll", xmScrollBarWidgetClass, (Widget)iw->image.scroll_win,
		XmNorientation, XmHORIZONTAL,
		XmNunitType, XmPIXELS,
		NULL);
      iw->image.hsb_managed = FALSE;
   }

   if (!iw->image.v_scrollbar) {
      iw->image.v_scrollbar = (XmScrollBarWidget) XtVaCreateWidget(
		"vscroll", xmScrollBarWidgetClass, (Widget)iw->image.scroll_win,
		XmNorientation, XmVERTICAL,
		XmNunitType, XmPIXELS,
		NULL);
      iw->image.vsb_managed = FALSE;
   }

   /* Tell the ScrolledWindow about its kids */

   if (iw->image.scroll_win)
      XmScrolledWindowSetAreas((Widget)iw->image.scroll_win,
		(Widget)iw->image.h_scrollbar, (Widget)iw->image.v_scrollbar,
		(Widget)iw);

   XtAddCallback((Widget)iw->image.h_scrollbar, XmNvalueChangedCallback,
		HScrollCallback, (XtPointer)iw);
   XtAddCallback((Widget)iw->image.h_scrollbar, XmNdragCallback,
		HScrollCallback, (XtPointer)iw);

   XtAddCallback((Widget)iw->image.v_scrollbar, XmNvalueChangedCallback,
		VScrollCallback, (XtPointer)iw);
   XtAddCallback((Widget)iw->image.v_scrollbar, XmNdragCallback,
		VScrollCallback, (XtPointer)iw);

}

/************************************************************************/
/* CreateXCursor							*/
/*									*/
/* Creates a new X cursor from the pixmaps, freeing the old one if	*/
/* necessary.  This should only be called when the cursor changes	*/
/* shape or color.							*/
/************************************************************************/

static void
#ifdef _NO_PROTO
CreateXCursor(iw)
   XvicImageWidget iw;
#else
CreateXCursor(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XColor fg, bg;
   Colormap colormap;

   if (iw->image.x_cursor) {
      if (XtIsRealized((Widget)iw))
         XUndefineCursor(XtDisplay((Widget)iw), XtWindow((Widget)iw));
      XFreeCursor(XtDisplay((Widget)iw), iw->image.x_cursor);
   }

   if (XtIsRealized((Widget)iw))
      colormap = biw->bim.colormap;
   else
      colormap = DefaultColormapOfScreen(XtScreen((Widget)iw));

   XParseColor(XtDisplay((Widget)iw), colormap,
	iw->image.cursor_foreground, &fg);
   XParseColor(XtDisplay((Widget)iw), colormap,
	iw->image.cursor_background, &bg);

   iw->image.x_cursor =	XCreatePixmapCursor(XtDisplay((Widget)iw),
			iw->image.curs_source, iw->image.curs_mask,
			&fg, &bg, iw->image.curs_hot_x, iw->image.curs_hot_y);

   SetXCursor(iw);

}

/************************************************************************/
/* DeallocGrColor							*/
/*									*/
/* Deallocates the entry from the colormap, if it is alloced.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DeallocGrColor(iw, color)
   XvicImageWidget iw;
   GrColor *color;
#else
DeallocGrColor(
   XvicImageWidget iw,
   GrColor *color)
#endif /* _NO_PROTO */
{
   Display *dpy;

   dpy = XtDisplay(iw);

   if (color->active) {
      if (color->alloc_def) {
         XFreeColors(dpy, DefaultColormap(dpy,DefaultScreen(dpy)),
		&color->xcolor.pixel, 1, 0);
         iw->image.default_cmap_refcnt[color->xcolor.pixel]--;
/*!!!!*/ if (iw->image.default_cmap_refcnt[color->xcolor.pixel] < 0) /*!!!!*/
/*!!!!*/    DPR(("default_cmap_refcnt[%d] < 0!\n",color->xcolor.pixel));/*!!!!*/
         color->alloc_def = False;
      }
      if (color->alloc_pvt) {
         iw->image.private_cmap_refcnt[color->xcolor.pixel]--;
/*!!!!*/ if (iw->image.private_cmap_refcnt[color->xcolor.pixel] < 0) /*!!!!*/
/*!!!!*/    DPR(("private_cmap_refcnt[%d] < 0!\n",color->xcolor.pixel));/*!!!!*/
         color->alloc_pvt = False;
      }
   }
}

/************************************************************************/
/* DestroyScrollbars							*/
/*									*/
/* Destroys the scrollbar widgets.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DestroyScrollbars(iw)
   XvicImageWidget iw;
#else
DestroyScrollbars(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{

   if (iw->image.h_scrollbar) {
      XtDestroyWidget((Widget)iw->image.h_scrollbar);
      iw->image.h_scrollbar = NULL;
      iw->image.hsb_managed = FALSE;
   }
   if (iw->image.v_scrollbar) {
      XtDestroyWidget((Widget)iw->image.v_scrollbar);
      iw->image.v_scrollbar = NULL;
      iw->image.vsb_managed = FALSE;
   }

   /* Tell the ScrolledWindow that its kids don't exist any more */

   if (iw->image.scroll_win)
      XmScrolledWindowSetAreas((Widget)iw->image.scroll_win,
		NULL, NULL, (Widget)iw);

}

/************************************************************************/
/* DrawObject								*/
/*									*/
/* Draws the given object.  The expose_rgn indicates the clipping area	*/
/* and is in Image coordinates.  If the region is NULL, the entire	*/
/* visible area is used.						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DrawObject(iw, obj, expose_rgn)
   XvicImageWidget iw;
   GrObject *obj;
   _XvicRegion *expose_rgn;
#else
DrawObject(
   XvicImageWidget iw,
   GrObject *obj,
   _XvicRegion *expose_rgn)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   GC Xgc;
   Window win;
   _XvicRect rect;
   _XvicRegion *rgn;
   int x_off, y_off;
   int x, y, x1, y1, x2, y2;
   double x_fp, y_fp;
   unsigned int width, height;
   XArc *xarc;
   XPoint *xpoint;
   XRectangle *xrectangle;
   XSegment *xsegment;
   int i;
   _XvicRect *rect_list;
   int n;

   if (iw->image.overlay_widget) {
      if (!XtIsRealized(iw->image.overlay_widget))
         return;
      win = XtWindow(iw->image.overlay_widget);
      x_off = 0;
      y_off = 0;
   }
   else {
      if (!XtIsRealized((Widget)iw))
         return;
      win = XtWindow((Widget)iw);
      x_off = biw->bim.x_dpy_off;
      y_off = biw->bim.y_dpy_off;
   }

   Xgc = GetGrXgc(iw, win, obj->any.gc, obj->any.color);
   if (Xgc == NULL)		/* invalid gc or color */
      return;

   if (expose_rgn == NULL) {
      _XvicGetVisibleRect(biw, &rect);
      do {
         rgn = _XvicRegionCreateRect(&rect);
         if (!rgn) _XvicMemoryPanic(biw);
      } while (rgn == NULL);
   }
   else
      rgn = expose_rgn;

   _XvicSetClipFromRgn(biw, rgn, Xgc, x_off, y_off);

   switch (obj->type) {

      case GrFillArc:
         x = XC_Img2Scr(obj->arc.x);
         y = YC_Img2Scr(obj->arc.y);
         width = XC_Img2Scr(obj->arc.x + obj->arc.width - 1.0) - x + 1;
         height = YC_Img2Scr(obj->arc.y + obj->arc.height - 1.0) - y + 1;

         XFillArc(XtDisplay((Widget)iw), win, Xgc,
			x + x_off, y + y_off, width, height,
			obj->arc.angle1, obj->arc.angle2);
         break;

      case GrFillArcs:
         xarc = _XvicMalloc(biw, obj->arcs.narcs*sizeof(XArc));
         for (i=0; i<obj->arcs.narcs; i++) {
            xarc[i].x = XC_Img2Scr(obj->arcs.arcs[i].x);
            xarc[i].y = YC_Img2Scr(obj->arcs.arcs[i].y);
            xarc[i].width =
			XC_Img2Scr(obj->arcs.arcs[i].x +
				   obj->arcs.arcs[i].width - 1.0)
			- xarc[i].x + 1;
            xarc[i].height =
			YC_Img2Scr(obj->arcs.arcs[i].y +
				   obj->arcs.arcs[i].height - 1.0)
			- xarc[i].y + 1;
            xarc[i].angle1 = obj->arcs.arcs[i].angle1;
            xarc[i].angle2 = obj->arcs.arcs[i].angle2;
            xarc[i].x += x_off;
            xarc[i].y += y_off;
         }
         XFillArcs(XtDisplay((Widget)iw), win, Xgc,
		xarc, obj->arcs.narcs);
         _XvicFree(biw, xarc, obj->arcs.narcs * sizeof(XArc));

         break;

      case GrFillPolygon:
         xpoint = _XvicMalloc(biw, obj->lines.npoints * sizeof(XPoint));
         if (obj->lines.npoints != 0) {
            x_fp = obj->lines.points[0].x;
            y_fp = obj->lines.points[0].y;
            xpoint[0].x = XC_Img2Scr(x_fp) + x_off;
            xpoint[0].y = YC_Img2Scr(y_fp) + y_off;
         }
         for (i=1; i<obj->lines.npoints; i++) {
            if (obj->lines.mode == CoordModePrevious) {
               x_fp += obj->lines.points[i].x;
               y_fp += obj->lines.points[i].y;
            }
            else {
               x_fp = obj->lines.points[i].x;
               y_fp = obj->lines.points[i].y;
            }
            xpoint[i].x = XC_Img2Scr(x_fp) + x_off;
            xpoint[i].y = YC_Img2Scr(y_fp) + y_off;
         }

         /* Always CoordModeOrigin because relative is taken care of above */

         XFillPolygon(XtDisplay((Widget)iw), win, Xgc,
		xpoint, obj->lines.npoints, obj->lines.shape, CoordModeOrigin);
         _XvicFree(biw, xpoint, obj->lines.npoints * sizeof(XPoint));

         break;

      case GrFillRectangle:
         x = XC_Img2Scr(obj->rectangle.x);
         y = YC_Img2Scr(obj->rectangle.y);
         width = XC_Img2Scr(obj->rectangle.x+obj->rectangle.width-1.0) - x + 1;
         height = YC_Img2Scr(obj->rectangle.y+obj->rectangle.height-1.0) - y +1;

         XFillRectangle(XtDisplay((Widget)iw), win, Xgc,
			x + x_off, y + y_off, width, height);
         break;

      case GrFillRectangles:
         xrectangle = _XvicMalloc(biw,
			obj->rectangles.nrectangles*sizeof(XRectangle));
         for (i=0; i<obj->rectangles.nrectangles; i++) {
            xrectangle[i].x = XC_Img2Scr(obj->rectangles.rectangles[i].x);
            xrectangle[i].y = YC_Img2Scr(obj->rectangles.rectangles[i].y);
            xrectangle[i].width =
			XC_Img2Scr(obj->rectangles.rectangles[i].x +
				   obj->rectangles.rectangles[i].width - 1.0)
			- xrectangle[i].x + 1;
            xrectangle[i].height =
			YC_Img2Scr(obj->rectangles.rectangles[i].y +
				   obj->rectangles.rectangles[i].height - 1.0)
			- xrectangle[i].y + 1;
            xrectangle[i].x += x_off;
            xrectangle[i].y += y_off;
         }
         XFillRectangles(XtDisplay((Widget)iw), win, Xgc,
		xrectangle, obj->rectangles.nrectangles);
         _XvicFree(biw, xrectangle,
		obj->rectangles.nrectangles * sizeof(XRectangle));

         break;

      case GrArc:
         x = XC_Img2Scr(obj->arc.x);
         y = YC_Img2Scr(obj->arc.y);
         width = XC_Img2Scr(obj->arc.x + obj->arc.width - 1.0) - x + 1;
         height = YC_Img2Scr(obj->arc.y + obj->arc.height - 1.0) - y + 1;

         XDrawArc(XtDisplay((Widget)iw), win, Xgc,
			x + x_off, y + y_off, width, height,
			obj->arc.angle1, obj->arc.angle2);
         break;

      case GrArcs:
         xarc = _XvicMalloc(biw, obj->arcs.narcs*sizeof(XArc));
         for (i=0; i<obj->arcs.narcs; i++) {
            xarc[i].x = XC_Img2Scr(obj->arcs.arcs[i].x);
            xarc[i].y = YC_Img2Scr(obj->arcs.arcs[i].y);
            xarc[i].width =
			XC_Img2Scr(obj->arcs.arcs[i].x +
				   obj->arcs.arcs[i].width - 1.0)
			- xarc[i].x + 1;
            xarc[i].height =
			YC_Img2Scr(obj->arcs.arcs[i].y +
				   obj->arcs.arcs[i].height - 1.0)
			- xarc[i].y + 1;
            xarc[i].angle1 = obj->arcs.arcs[i].angle1;
            xarc[i].angle2 = obj->arcs.arcs[i].angle2;
            xarc[i].x += x_off;
            xarc[i].y += y_off;
         }
         XDrawArcs(XtDisplay((Widget)iw), win, Xgc,
		xarc, obj->arcs.narcs);
         _XvicFree(biw, xarc, obj->arcs.narcs * sizeof(XArc));

         break;

      case GrBitmap:
         /* Easiest way to draw is to set the clipping mask to the	*/
         /* shape, then use FillRectangle on the area to draw.		*/

         x = XC_Img2Scr(obj->bitmap.x) - obj->bitmap.hot_x;
         y = YC_Img2Scr(obj->bitmap.y) - obj->bitmap.hot_y;

         XSetClipMask(XtDisplay((Widget)iw), Xgc, obj->bitmap.bitmap);
         XSetClipOrigin(XtDisplay((Widget)iw), Xgc, x + x_off, y + y_off);

         rect_list = _XvicRegionGetRectangles(rgn);
         n = _XvicRegionGetNumRects(rgn);
         for (i=0; i<n; i++) {
            x = X1_Img2Scr(rect_list[i].x1);
            y = Y1_Img2Scr(rect_list[i].y1);
            width = X2_Img2Scr(rect_list[i].x2) - x + 1;
            height = Y2_Img2Scr(rect_list[i].y2) - y + 1;
            X_ViewConstrain(x, width);
            Y_ViewConstrain(y, height);

            XFillRectangle(XtDisplay((Widget)iw), win, Xgc,
			x + x_off, y + y_off, width, height);
         }
         break;

      case GrLine:
         x1 = XC_Img2Scr(obj->line.x1);
         y1 = YC_Img2Scr(obj->line.y1);
         x2 = XC_Img2Scr(obj->line.x2);
         y2 = YC_Img2Scr(obj->line.y2);

         XDrawLine(XtDisplay((Widget)iw), win, Xgc,
		x1 + x_off, y1 + y_off, x2 + x_off, y2 + y_off);
         break;

      case GrLines:
         xpoint = _XvicMalloc(biw, obj->lines.npoints * sizeof(XPoint));
         if (obj->lines.npoints != 0) {
            x_fp = obj->lines.points[0].x;
            y_fp = obj->lines.points[0].y;
            xpoint[0].x = XC_Img2Scr(x_fp) + x_off;
            xpoint[0].y = YC_Img2Scr(y_fp) + y_off;
         }
         for (i=1; i<obj->lines.npoints; i++) {
            if (obj->lines.mode == CoordModePrevious) {
               x_fp += obj->lines.points[i].x;
               y_fp += obj->lines.points[i].y;
            }
            else {
               x_fp = obj->lines.points[i].x;
               y_fp = obj->lines.points[i].y;
            }
            xpoint[i].x = XC_Img2Scr(x_fp) + x_off;
            xpoint[i].y = YC_Img2Scr(y_fp) + y_off;
         }

         /* Always CoordModeOrigin because relative is taken care of above */

         XDrawLines(XtDisplay((Widget)iw), win, Xgc,
		xpoint, obj->lines.npoints, CoordModeOrigin);
         _XvicFree(biw, xpoint, obj->lines.npoints * sizeof(XPoint));

         break;

      case GrPoint:
         x = XC_Img2Scr(obj->point.x);
         y = YC_Img2Scr(obj->point.y);

         /* XDrawPoint doesn't pay attention to tiles, so dither fails... */
         XFillRectangle(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y + y_off, 1, 1);
         break;

      case GrPoints:
         xrectangle = _XvicMalloc(biw, obj->lines.npoints * sizeof(XRectangle));
         if (obj->lines.npoints != 0) {
            x_fp = obj->lines.points[0].x;
            y_fp = obj->lines.points[0].y;
            xrectangle[0].x = XC_Img2Scr(x_fp) + x_off;
            xrectangle[0].y = YC_Img2Scr(y_fp) + y_off;
            xrectangle[0].width = 1;
            xrectangle[0].height = 1;
         }
         for (i=1; i<obj->lines.npoints; i++) {
            if (obj->lines.mode == CoordModePrevious) {
               x_fp += obj->lines.points[i].x;
               y_fp += obj->lines.points[i].y;
            }
            else {
               x_fp = obj->lines.points[i].x;
               y_fp = obj->lines.points[i].y;
            }
            xrectangle[i].x = XC_Img2Scr(x_fp) + x_off;
            xrectangle[i].y = YC_Img2Scr(y_fp) + y_off;
            xrectangle[i].width = 1;
            xrectangle[i].height = 1;
         }

         /* XDrawPoint doesn't pay attention to tiles, so dither fails... */
         XFillRectangles(XtDisplay((Widget)iw), win, Xgc,
		xrectangle, obj->lines.npoints);
         _XvicFree(biw, xrectangle, obj->lines.npoints * sizeof(XRectangle));

         break;

      case GrRectangle:
         x = XC_Img2Scr(obj->rectangle.x);
         y = YC_Img2Scr(obj->rectangle.y);
         width = XC_Img2Scr(obj->rectangle.x + obj->rectangle.width-1.0) -x+1;
         height = YC_Img2Scr(obj->rectangle.y + obj->rectangle.height-1.0) -y+1;

         if (width > 0)
            width--;		/* Compensate for X adding 1 to width */
         if (height > 0)
            height--;		/* Compensate for X adding 1 to height */

         XDrawRectangle(XtDisplay((Widget)iw), win, Xgc,
			x + x_off, y + y_off, width, height);
         break;

      case GrRectangles:
         xrectangle = _XvicMalloc(biw,
			obj->rectangles.nrectangles*sizeof(XRectangle));
         for (i=0; i<obj->rectangles.nrectangles; i++) {
            xrectangle[i].x = XC_Img2Scr(obj->rectangles.rectangles[i].x);
            xrectangle[i].y = YC_Img2Scr(obj->rectangles.rectangles[i].y);
            xrectangle[i].width =
			XC_Img2Scr(obj->rectangles.rectangles[i].x +
				   obj->rectangles.rectangles[i].width - 1.0)
			- xrectangle[i].x + 1;
            xrectangle[i].height =
			YC_Img2Scr(obj->rectangles.rectangles[i].y +
				   obj->rectangles.rectangles[i].height - 1.0)
			- xrectangle[i].y + 1;
            xrectangle[i].x += x_off;
            xrectangle[i].y += y_off;
            if (xrectangle[i].width > 0)
               xrectangle[i].width--;	/* Compensate for X adding 1 to width */
            if (xrectangle[i].height > 0)
               xrectangle[i].height--;	/* Compensate for X adding 1 to height*/
         }
         XDrawRectangles(XtDisplay((Widget)iw), win, Xgc,
		xrectangle, obj->rectangles.nrectangles);
         _XvicFree(biw, xrectangle,
		obj->rectangles.nrectangles * sizeof(XRectangle));

         break;

      case GrSegments:
         xsegment = _XvicMalloc(biw, obj->segments.nsegments*sizeof(XSegment));
         for (i=0; i<obj->segments.nsegments; i++) {
            xsegment[i].x1 = XC_Img2Scr(obj->segments.segments[i].x1) + x_off;
            xsegment[i].y1 = YC_Img2Scr(obj->segments.segments[i].y1) + y_off;
            xsegment[i].x2 = XC_Img2Scr(obj->segments.segments[i].x2) + x_off;
            xsegment[i].y2 = YC_Img2Scr(obj->segments.segments[i].y2) + y_off;
         }
         XDrawSegments(XtDisplay((Widget)iw), win, Xgc,
		xsegment, obj->segments.nsegments);
         _XvicFree(biw, xsegment, obj->segments.nsegments * sizeof(XSegment));

         break;

      case GrString:
         if (!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->string.x);
         y = YC_Img2Scr(obj->string.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         XDrawString(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y + y_off, obj->string.string, obj->string.length);
         break;

      case GrString16:
         if (!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->string16.x);
         y = YC_Img2Scr(obj->string16.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         XDrawString16(XtDisplay((Widget)iw), win, Xgc,
		x+x_off, y+y_off, obj->string16.string, obj->string16.length);
         break;

      case GrImageString:
         if (!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->string.x);
         y = YC_Img2Scr(obj->string.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         /* Draw the background box first.  We could use rbearing here	*/
         /* in order to make sure the background covers all of the	*/
         /* text (some italic characters stick out), but the X protocol	*/
         /* specifies width, and that's what XDrawImageString does, so	*/
         /* that's what we'll do too.  Oh well.				*/

         XFillRectangle(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y - obj->anystr.ascent + y_off,
		obj->anystr.width, obj->anystr.ascent + obj->anystr.descent);

         /* Now draw the foreground in a different color */

         Xgc = GetGrXgc(iw, win, obj->any.gc, obj->string.fg);
         if (Xgc == NULL)		/* invalid gc or color */
            break;			/* blow off the string */

         XDrawString(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y + y_off, obj->string.string, obj->string.length);
         break;

      case GrImageString16:
         if (!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->string16.x);
         y = YC_Img2Scr(obj->string16.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         /* Draw the background box first.  We could use rbearing here	*/
         /* in order to make sure the background covers all of the	*/
         /* text (some italic characters stick out), but the X protocol	*/
         /* specifies width, and that's what XDrawImageString does, so	*/
         /* that's what we'll do too.  Oh well.				*/

         XFillRectangle(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y - obj->anystr.ascent + y_off,
		obj->anystr.width, obj->anystr.ascent + obj->anystr.descent);

         /* Now draw the foreground in a different color */

         Xgc = GetGrXgc(iw, win, obj->any.gc, obj->string16.fg);
         if (Xgc == NULL)		/* invalid gc or color */
            break;			/* blow off the string */

         XDrawString16(XtDisplay((Widget)iw), win, Xgc,
		x+x_off, y+y_off, obj->string16.string, obj->string16.length);
         break;

      case GrText:
         if ((!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont)) &&
             (obj->text.nitems <= 0 || obj->text.items[0].font == None))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->text.x);
         y = YC_Img2Scr(obj->text.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         XDrawText(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y + y_off, obj->text.items, obj->text.nitems);
         break;

      case GrText16:
         if ((!(iw->image.gr_gc_list[obj->any.gc].mask & GCFont)) &&
             (obj->text16.nitems <= 0 || obj->text16.items[0].font == None))
            break;		/* Draw nothing if no font */
         x = XC_Img2Scr(obj->text16.x);
         y = YC_Img2Scr(obj->text16.y);
         if (obj->anystr.justify == XvicJUST_RIGHT)
            x -= obj->anystr.width;
         if (obj->anystr.justify == XvicJUST_CENTER)
            x -= obj->anystr.width / 2;

         XDrawText16(XtDisplay((Widget)iw), win, Xgc,
		x + x_off, y + y_off, obj->text16.items, obj->text16.nitems);
         break;

      default:
         FatalError(iw, "BadObjectType2",
		  "Internal error: Unknown object type in DrawObject().");
   }

   if (expose_rgn == NULL)
      _XvicRegionDestroy(rgn);

}

/************************************************************************/
/* DrawPlantedCursor							*/
/*									*/
/* Draws the planted cursor at the location given by plant_curs_x and	*/
/* plant_curs_y.  Note that it is the caller's responsibilty to make	*/
/* sure the cursor should be drawn; i.e. cursorMode is not checked.	*/
/* If the widget is not realized, there's not much point in trying to	*/
/* draw, so this routine just returns.					*/
/************************************************************************/

static void
#ifdef _NO_PROTO
DrawPlantedCursor(iw)
   XvicImageWidget iw;
#else
DrawPlantedCursor(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{

   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XGCValues values;
   unsigned long mask = 0;
   int x, y;
   unsigned int width, height;
   Window win;			/* window to draw in */
   int x_off, y_off;

   if (!XtIsRealized((Widget)iw))
      return;

   if (iw->image.overlay_widget) {
      win = XtWindow(iw->image.overlay_widget);
      x_off = 0;
      y_off = 0;
   }
   else {
      win = XtWindow((Widget)iw);
      x_off = biw->bim.x_dpy_off;
      y_off = biw->bim.y_dpy_off;
   }

   if (!iw->image.plant_set) {		/* Set up the cursor stuff */

      XParseColor(XtDisplay((Widget)iw), biw->bim.colormap,
		iw->image.cursor_foreground, &iw->image.curs_fg.xcolor);
      XParseColor(XtDisplay((Widget)iw), biw->bim.colormap,
		iw->image.cursor_background, &iw->image.curs_bg.xcolor);
      iw->image.curs_fg.active = True;
      iw->image.curs_bg.active = True;

      GetOneGrColor(iw, &iw->image.curs_fg, NULL);
      GetOneGrColor(iw, &iw->image.curs_bg, NULL);

      /* Now set up the GC's for drawing the cursor */

      if (iw->image.curs_fg_gc)
         XFreeGC(XtDisplay((Widget)iw), iw->image.curs_fg_gc);
      if (iw->image.curs_bg_gc)
         XFreeGC(XtDisplay((Widget)iw), iw->image.curs_bg_gc);

      SetUpGraphicsGC(iw, &iw->image.curs_fg, &mask, &values);
      values.clip_mask = iw->image.curs_source;	/* Use shape as clip mask */
      mask |= GCClipMask;
      iw->image.curs_fg_gc = XCreateGC(XtDisplay((Widget)iw),
		win, mask, &values);

      mask = 0;
      SetUpGraphicsGC(iw, &iw->image.curs_bg, &mask, &values);
      values.clip_mask = iw->image.curs_mask; /* Use shape as clip mask */
      mask |= GCClipMask;
      iw->image.curs_bg_gc = XCreateGC(XtDisplay((Widget)iw),
		win, mask, &values);

      iw->image.plant_set = True;
   }

   /* Now draw the cursor.  This is accomplished by drawing the mask	*/
   /* first in the background color, then drawing the cursor in the	*/
   /* foreground color.  The clip mask is set to the cursor shape, so	*/
   /* we just do a FillRectangle on the bounding box.  If the mask	*/
   /* doesn't exist, the clip mask for it is set to NULL, thus the	*/
   /* whole bounding box is drawn.  This matches X behavior when there	*/
   /* is no cursor mask.						*/

   x = XC_Img2Scr(iw->image.plant_curs_x) - iw->image.curs_hot_x;
   width = iw->image.curs_width;
   y = YC_Img2Scr(iw->image.plant_curs_y) - iw->image.curs_hot_y;
   height = iw->image.curs_height;

   values.clip_x_origin = x + x_off;
   values.clip_y_origin = y + y_off;
   mask = GCClipXOrigin | GCClipYOrigin;

   X_ViewConstrain(x, width);
   Y_ViewConstrain(y, height);
   x += x_off;
   y += y_off;

   /* We have to set up TS origin and clip every time, so might as well	*/
   /* set up the rest (there's not much more).				*/
   SetUpGraphicsGC(iw, &iw->image.curs_bg, &mask, &values);

   XChangeGC(XtDisplay((Widget)iw), iw->image.curs_bg_gc, mask, &values);

   XFillRectangle(XtDisplay((Widget)iw), win, iw->image.curs_bg_gc,
		x, y, width, height);

   mask = GCClipXOrigin | GCClipYOrigin;

   SetUpGraphicsGC(iw, &iw->image.curs_fg, &mask, &values);
   XChangeGC(XtDisplay((Widget)iw), iw->image.curs_fg_gc, mask, &values);

   XFillRectangle(XtDisplay((Widget)iw), win, iw->image.curs_fg_gc,
		x, y, width, height);
}

/************************************************************************/
/* ErasePlantedCursor							*/
/*									*/
/* Erases the planted cursor from the location given by plant_curs_x	*/
/* and plant_curs_y.  Note that it is the caller's responsibilty to	*/
/* make sure the cursor should be erased; i.e. cursorMode is not	*/
/* checked.								*/
/************************************************************************/

static void
#ifdef _NO_PROTO
ErasePlantedCursor(iw)
   XvicImageWidget iw;
#else
ErasePlantedCursor(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   XExposeEvent ev;
   unsigned char save_work_proc_policy;

   ev.x = XC_Img2Scr(iw->image.plant_curs_x) - iw->image.curs_hot_x;
   ev.y = YC_Img2Scr(iw->image.plant_curs_y) - iw->image.curs_hot_y;
   ev.width = iw->image.curs_width;
   ev.height = iw->image.curs_height;

   /* Turn off work procs for this so the cursor erases look right */
   /* Also, turn off redisplay of cursor during expose */

   iw->image.erasing_cursor = True;
   save_work_proc_policy = biw->bim.work_proc_policy;
   biw->bim.work_proc_policy = XvicNONE;
   _XvicRedisplay_Internal(biw, &ev, NULL);
   biw->bim.work_proc_policy = save_work_proc_policy;
   iw->image.erasing_cursor = False;

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
FatalError(iw, name, def)
   XvicImageWidget iw;
   char *name;
   char *def;
#else
FatalError(
   XvicImageWidget iw,
   char *name,
   char *def)
#endif /* _NO_PROTO */
{
   XtAppErrorMsg(XtWidgetToApplicationContext((Widget)iw),
	name, "XvicImage", "XvicImageWidgetError", def,
	(String *)NULL, (Cardinal *)NULL);
   exit(1);	/* XtAppErrorMsg should never return, but just in case... */
}

/************************************************************************/
/* FreeGrColorTile							*/
/*									*/
/* Frees up the tile pixmap for a graphics color, if it is allocated.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeGrColorTile(iw, color)
   XvicImageWidget iw;
   GrColor *color;
#else
FreeGrColorTile(
   XvicImageWidget iw,
   GrColor *color)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;

   if (color->active) {
      if (color->gc_tile) {
         _XvicMemoryReturn(biw, color->width * color->height);
         XFreePixmap(XtDisplay(iw), color->gc_tile);
         color->gc_tile = None;
         color->width = 0;
         color->height = 0;
      }
   }
}

/************************************************************************/
/* FreeOneGrGC								*/
/*									*/
/* Frees resources for one GrGC struct (includes GC and dash list).	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeOneGrGC(iw, gr_gc)
   XvicImageWidget iw;
   GrGC *gr_gc;
#else
FreeOneGrGC(
   XvicImageWidget iw,
   GrGC *gr_gc)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;

   if (gr_gc == NULL)
      return;

   if (gr_gc->dash_list) {
      _XvicFree(biw, gr_gc->dash_list, gr_gc->num_dashes);
      gr_gc->dash_list = 0;
      gr_gc->num_dashes = 0;
   }

   if (gr_gc->gc) {
      XFreeGC(XtDisplay((Widget)iw), gr_gc->gc);
      gr_gc->gc = NULL;
   }

   gr_gc->active = False;
}

/************************************************************************/
/* FreeOneGrObject							*/
/*									*/
/* Frees resources for one GrObject, including the object itself	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
FreeOneGrObject(iw, object)
   XvicImageWidget iw;
   GrObject *object;
#else
FreeOneGrObject(
   XvicImageWidget iw,
   GrObject *object)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int size=0;

   if (object == NULL)
      return;

   /* Free any special lists and get size */

   switch (object->type) {

      case GrArc:
      case GrFillArc:
         size = sizeof(GrArcObject);
         break;

      case GrArcs:
      case GrFillArcs:
         if (object->arcs.arcs)
            _XvicFree(biw, object->arcs.arcs,
			object->arcs.narcs * sizeof(XvicArc));
         size = sizeof(GrArcsObject);
         break;

      case GrBitmap:
         size = sizeof(GrBitmapObject);
         break;

      case GrLine:
         size = sizeof(GrLineObject);
         break;

      case GrLines:
      case GrPoints:
      case GrFillPolygon:
         if (object->lines.points)
            _XvicFree(biw, object->lines.points,
			object->lines.npoints * sizeof(XvicPoint));
         size = sizeof(GrLinesObject);
         break;

      case GrPoint:
         size = sizeof(GrPointObject);
         break;

      case GrRectangle:
      case GrFillRectangle:
         size = sizeof(GrRectangleObject);
         break;

      case GrRectangles:
      case GrFillRectangles:
         if (object->rectangles.rectangles)
            _XvicFree(biw, object->rectangles.rectangles,
			object->rectangles.nrectangles * sizeof(XvicRectangle));
         size = sizeof(GrRectanglesObject);
         break;

      case GrSegments:
         if (object->segments.segments)
            _XvicFree(biw, object->segments.segments,
			object->segments.nsegments * sizeof(XvicSegment));
         size = sizeof(GrSegmentsObject);
         break;

      case GrImageString:
      case GrString:
         if (object->string.string)
            _XvicFree(biw, object->string.string, object->string.length);
         size = sizeof(GrStringObject);
         break;

      case GrImageString16:
      case GrString16:
         if (object->string16.string)
            _XvicFree(biw, object->string16.string,
				object->string16.length * sizeof(XChar2b));
         size = sizeof(GrString16Object);
         break;

      case GrText:
         if (object->text.items)
            _XvicFree(biw, object->text.items,
				object->text.nitems * sizeof(XTextItem));
         size = sizeof(GrTextObject);
         break;

      case GrText16:
         if (object->text16.items)
            _XvicFree(biw, object->text16.items,
				object->text16.nitems * sizeof(XTextItem16));
         size = sizeof(GrText16Object);
         break;

      default:
         FatalError(iw, "BadObjectType",
		  "Internal error: Unknown object type in FreeOneGrObject().");

   }

   /* Now free the object itself */

   _XvicFree(biw, object, size);
}

/************************************************************************/
/* GetBoundingRgnForGC							*/
/*									*/
/* Returns a region (in Image coordinates) containing a union of the	*/
/* bounding rectangles for all graphics objects using the given GC.	*/
/* The caller is responsible for freeing the region.  It's quite	*/
/* possible that the returned region might be empty.  Objects drawn	*/
/* with a rubber GC *are* included in the region.  If a region is	*/
/* passed in, it is used (new areas are added to it), otherwise, a	*/
/* new region is created.						*/
/************************************************************************/

static _XvicRegion *
#ifdef _NO_PROTO
GetBoundingRgnForGC(iw, gc, rgn)
   XvicImageWidget iw;
   XvicGC gc;
   _XvicRegion *rgn;
#else
GetBoundingRgnForGC(
   XvicImageWidget iw,
   XvicGC gc,
   _XvicRegion *rgn)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int i;
   Boolean status;

   if (rgn == NULL) {
      do {
         rgn = _XvicRegionCreate();
         if (!rgn) _XvicMemoryPanic(biw);
      } while (rgn == NULL);
   }

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.gc == gc) {
            do {
               status = _XvicRegionUnion(
				BoundingRect(iw, iw->image.gr_objects[i]), rgn);
               if (!status) _XvicMemoryPanic(biw);
            } while (!status);
         }
      }
   }

   return rgn;
}

/************************************************************************/
/* GetBoundingRgnForID							*/
/*									*/
/* Returns a region (in Image coordinates) containing a union of the	*/
/* bounding rectangles for all graphics objects with the given ID.	*/
/* The caller is responsible for freeing the region.  It's quite	*/
/* possible that the returned region might be empty.  Objects drawn	*/
/* with a rubber GC are *not* included in the region.  If a region is	*/
/* passed in, it is used (new areas are added to it), otherwise, a	*/
/* new region is created.						*/
/************************************************************************/

static _XvicRegion *
#ifdef _NO_PROTO
GetBoundingRgnForID(iw, id, rgn)
   XvicImageWidget iw;
   XvicID id;
   _XvicRegion *rgn;
#else
GetBoundingRgnForID(
   XvicImageWidget iw,
   XvicID id,
   _XvicRegion *rgn)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   XvicGC gc;
   int i;
   Boolean status;

   if (rgn == NULL) {
      do {
         rgn = _XvicRegionCreate();
         if (!rgn) _XvicMemoryPanic(biw);
      } while (rgn == NULL);
   }

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i]) {
         if (iw->image.gr_objects[i]->any.id == id) {
            gc = iw->image.gr_objects[i]->any.gc;
            if ( !(CheckValidGrGC(iw,gc,False) &&
		  iw->image.gr_gc_list[gc].is_rubber)) {
               do {
                  status = _XvicRegionUnion(BoundingRect(iw,
						      iw->image.gr_objects[i]),
					    rgn);
                  if (!status) _XvicMemoryPanic(biw);
               } while (!status);
            }
         }
      }
   }

   return rgn;
}

/************************************************************************/
/* GetClosestColor							*/
/*									*/
/* Determines the pixel value that is the "closest" color in the	*/
/* colormap to the desired color and returns it in the "color"		*/
/* argument.  The cells argument is an array of XColor structs as	*/
/* returned by XQueryColor; it may be passed in as NULL (in which case	*/
/* this routine does XQueryColor itself on "colormap"), but if this	*/
/* routine is called often, many server round-trips may be saved by	*/
/* passing in "cells".  The algoritm used to determine "closest" color	*/
/* is simply the minimum Cartesian distance in RGB space.  To prevent	*/
/* overflow, only 8 bits are used for the colors.  A better algorithm	*/
/* could be used if desired.						*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetClosestColor(iw, color, cells_arg, cmap, cmap_size)
   XvicImageWidget iw;
   XColor *color;
   XColor *cells_arg;
   Colormap cmap;
   int cmap_size;
#else
GetClosestColor(
   XvicImageWidget iw,
   XColor *color,
   XColor *cells_arg,
   Colormap cmap,
   int cmap_size)
#endif /* _NO_PROTO */
{
   XColor cells_local[CMAP_SIZE_MAX];
   XColor *cells;
   int best_index, best_distance, distance;
   int red, grn, blu;
   int i;

   cells = cells_arg;
   if (cells == NULL) {
      for (i=0; i<cmap_size; i++)
         cells_local[i].pixel = i;
      XQueryColors(XtDisplay(iw), cmap, cells_local, cmap_size);
      cells = cells_local;
   }

   red = color->red >> 8;
   grn = color->green >> 8;
   blu = color->blue >> 8;
   best_index = 0;
   best_distance = ((cells[0].red>>8) - red)   * ((cells[0].red>>8) - red)   +
		   ((cells[0].green>>8) - grn) * ((cells[0].green>>8) - grn) +
		   ((cells[0].blue>>8) - blu)  * ((cells[0].blue>>8) - blu);

   for (i=1; i<cmap_size; i++) {
      distance = ((cells[i].red>>8) - red)   * ((cells[i].red>>8) - red)   +
		 ((cells[i].green>>8) - grn) * ((cells[i].green>>8) - grn) +
		 ((cells[i].blue>>8) - blu)  * ((cells[i].blue>>8) - blu);
      if (distance < best_distance) {
         best_index = i;
         best_distance = distance;
      }
   }

   color->pixel = best_index;

}

/************************************************************************/
/* GetCursorSizeFromFont						*/
/*									*/
/* Gets the size and hot spot location from a character in a font for	*/
/* use as a cursor.  Return True if it's okay, False for bad font or	*/
/* bad character.  An XFontStruct is passed in, not a Font.  The caller	*/
/* is responsible for querying the font and freeing the XFontStruct.	*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
GetCursorSizeFromFont(iw, fs, ch, width, height, hot_x, hot_y)
   XvicImageWidget iw;
   XFontStruct *fs;
   unsigned int ch;
   int *width;
   int *height;
   unsigned int *hot_x;
   unsigned int *hot_y;
#else
GetCursorSizeFromFont(
   XvicImageWidget iw,
   XFontStruct *fs,
   unsigned int ch,
   int *width,
   int *height,
   unsigned int *hot_x,
   unsigned int *hot_y)
#endif /* _NO_PROTO */
{
   XCharStruct *char_info;

   if (fs->min_byte1 != 0) {
      /* All that really has to be done here to support multibyte fonts	*/
      /* is to figure out how to get the index into the per_char array	*/
      /* and test to make sure the character is within bounds.		*/
      WarningMsg(iw, "NoMultiByteCursorFont",
	"Multibyte fonts are not currently supported by the cursor routines.");
      return False;		/* don't change anything */
   }

   if (ch < fs->min_char_or_byte2 || ch > fs->max_char_or_byte2) {
      WarningMsg(iw, "BadCursorFontChar",
	"The specified character for the cursor does not exist in the font.");
      return False;
   }

   if (fs->per_char == NULL)
      char_info = &fs->min_bounds;
   else
      char_info = &fs->per_char[ch - fs->min_char_or_byte2];

   *height = char_info->ascent + char_info->descent;
   *width = char_info->rbearing - char_info->lbearing;
   if (char_info->lbearing > 0)
      *width += char_info->lbearing;	/* hot spot outside bounding box (!) */

   /* X hotspot is negative of lbearing unless lbearing is positive	*/
   /* (meaning the hotspot is outside the bounding box(!)), in which	*/
   /* case we set it to 0.						*/

   *hot_y = char_info->ascent;
   *hot_x = (char_info->lbearing > 0) ? 0 : (- char_info->lbearing);

   return True;

}

/************************************************************************/
/* GetFloatingCursorLoc							*/
/*									*/
/* Get the cursor location by querying the pointer, as if it were a	*/
/* floating cursor.  Since this is called by SetValues when the mode	*/
/* changes, the cursor mode does not actually have to be floating to	*/
/* call this routine.  The cursor location is returned in Image coords.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetFloatingCursorLoc(iw, x, y)
   XvicImageWidget iw;
   double *x;
   double *y;
#else
GetFloatingCursorLoc(
   XvicImageWidget iw,
   double *x,
   double *y)
#endif
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   Window root, child;
   int root_x, root_y, xloc, yloc;
   unsigned int mask;

   if (!XtIsRealized((Widget)iw)) {
      *x = 0.0;
      *y = 0;
      return;
   }

   XQueryPointer(XtDisplay((Widget)iw), XtWindow((Widget)iw),
		&root, &child, &root_x, &root_y, &xloc, &yloc, &mask);

   *x = XC_Scr2Img(xloc - biw->bim.x_dpy_off);
   *y = YC_Scr2Img(yloc - biw->bim.y_dpy_off);
}

/************************************************************************/
/* GetGrXgc								*/
/*									*/
/* Returns the GC associated with the provided XvicGC.  The GC is	*/
/* created if needed.  The provided GC is ready to go except for the	*/
/* clipping area, which is undefined and must be set by the caller.	*/
/************************************************************************/

static GC
#ifdef _NO_PROTO
GetGrXgc(iw, win, gr_gc, color)
   XvicImageWidget iw;
   Window win;
   XvicGC gr_gc;
   XvicColor color;
#else
GetGrXgc(
   XvicImageWidget iw,
   Window win,
   XvicGC gr_gc,
   XvicColor color)
#endif /* _NO_PROTO */
{
   unsigned long valueMask;
   XGCValues values;
   GrGC *gcptr;

   if (!CheckValidGrGC(iw, gr_gc, True))
      return NULL;

   if (!CheckValidGrColor(iw, color))
      return NULL;

   gcptr = &iw->image.gr_gc_list[gr_gc];

   valueMask = 0;
   memcpy((void *)&values, (void *)&gcptr->values,
			sizeof(XGCValues));

   SetUpGraphicsGC(iw, &iw->image.gr_colors[color], &valueMask, &values);

   if (gcptr->is_rubber) {
      values.foreground = 0xffffffff;	/* all bits on */
      values.function = GXxor;
      values.fill_style = FillSolid;
      valueMask |= GCForeground | GCFunction | GCFillStyle;
   }

   if (gcptr->gc) {			/* The X GC exists */
      XChangeGC(XtDisplay((Widget)iw), gcptr->gc, valueMask, &values);
   }
   else {
      valueMask |= gcptr->mask;
      gcptr->gc = XCreateGC(XtDisplay((Widget)iw), win, valueMask, &values);
      if (gcptr->num_dashes)
         XSetDashes(XtDisplay((Widget)iw), gcptr->gc,
		gcptr->dash_offset, gcptr->dash_list, gcptr->num_dashes);
   }

   return gcptr->gc;
}

/************************************************************************/
/* GetHWOverlayGC							*/
/*									*/
/* Gets a GC to use for panning the overlay.  The overlay widget must	*/
/* exist, and must be realized, before calling this function.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetHWOverlayGC(iw)
   XvicImageWidget iw;
#else
GetHWOverlayGC(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   unsigned long valueMask;
   XGCValues values;

   values.graphics_exposures = FALSE;	/*!!!! Are these needed? !!!!*/
   valueMask = GCGraphicsExposures;

   values.function = GXcopy;
   values.plane_mask = 0xffffffff;
   values.subwindow_mode = IncludeInferiors;
   values.clip_x_origin = 0;
   values.clip_y_origin = 0;
   values.clip_mask = None;
   valueMask |= GCFunction|GCPlaneMask|GCSubwindowMode|GCClipXOrigin|GCClipYOrigin|GCClipMask;	/*!!!!*/

   if (iw->image.overlay_pan_gc)
      XFreeGC(XtDisplay((Widget)iw), iw->image.overlay_pan_gc);

   iw->image.overlay_pan_gc = XCreateGC(XtDisplay(iw->image.overlay_widget),
		XtWindow(iw->image.overlay_widget), valueMask, &values);
}

/************************************************************************/
/* GetHWOverlayVisual							*/
/*									*/
/* Determines whether a hardware overlay is available, and if so, what	*/
/* visual to use for it.  Sets the overlay_visual, overlay_depth,	*/
/* overlay_type, and overlay_value fields of the widget.  If no overlay	*/
/* is available, overlay_visual is set to NULL.  Quietly returns no	*/
/* overlay available for most kinds of errors.  NOTE:  Overlays deeper	*/
/* than 8 bits or not of PsuedoColor class are ignored since the rest	*/
/* of the code can't really deal with them!!!!  Also, if the overlay	*/
/* is actually the server *default* visual (as is the case on the HP),	*/
/* we disallow it.  It causes problems in Alloc mode (the same visual	*/
/* type apparently can't overlay itself) and also the pan repaints	*/
/* don't work right if the window is partially obscured.		*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetHWOverlayVisual(iw)
   XvicImageWidget iw;
#else
GetHWOverlayVisual(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   OverlayInfo *oinfo;
   Atom overlayAtom, actual_type;
   int actual_format;
   unsigned long overlay_count, bytes_left;
   XVisualInfo vinfo, *visuals;
   int nvisuals, overlay, i;

   iw->image.overlay_visual = NULL;

   /* If it's not enabled, just return */

   if (!iw->image.enable_hw_overlay)
      return;

   /*!!!! ViTEC server on a Sun has a problem with the overlay plane	*/
   /* where expose events don't occur, XClearArea() is ignored, and	*/
   /* other niceties.  So, TEMPORARILY, we disable hardware overlay for	*/
   /* the ViTEC.						    !!!!*/
   if (strcmp(ServerVendor(XtDisplay((Widget)iw)),
		"VIT-Visual Information Technologies, Inc. (VITec)") == 0)
      return;		/*!!!! No overlay !!!!*/

   /*!!!! DEC Unix client library (yes, client, not server, hence the	*/
   /* #ifdef) will complain: "Error: Attempt to unmanage a child when	*/
   /* parent is not Composite" when unrealizing the overlay widget,	*/
   /* despite the fact that the overlay widget is never actually	*/
   /* managed!!  So, just disable the HW overlay there for now.		*/
   /* Hopefully, this can be fixed eventually!!!!			*/

#if defined(__alpha) && !defined(__VMS)
   return;
#else
   /* Get the list of overlays available */

   overlayAtom = XInternAtom(XtDisplay((Widget)iw),
				"SERVER_OVERLAY_VISUALS",True);
   if (!overlayAtom) {
      return;		/* No overlay */
   }

   /* 400 below is enough for 100 overlay visuals.  Since X is allocating */
   /* the memory for us anyway, why do we need to pass in a length??	*/
   /* (this was 50 but some SGI servers have more than that!!)		*/
   XGetWindowProperty(XtDisplay((Widget)iw),
		RootWindowOfScreen(XtScreen((Widget)iw)),
		overlayAtom, 0, 400, False,
		AnyPropertyType, &actual_type, &actual_format,
		&overlay_count, &bytes_left, (unsigned char **)&oinfo);
   if (overlay_count == 3)		/* VITec has only 3 elements */
      overlay_count = 1;
   else
      overlay_count /= (sizeof(OverlayInfo) / sizeof(long));

   if (overlay_count == 0) {
      XFree(oinfo);
      return;		/* No overlay */
   }

   /* Get the list of all visuals on the screen */

   vinfo.screen = DefaultScreen(XtDisplay((Widget)iw));
   visuals = XGetVisualInfo(XtDisplay((Widget)iw), VisualScreenMask,
			&vinfo, &nvisuals);
   if (nvisuals == 0) {
      XFree(oinfo);
      return;		/* Shouldn't happen */
   }

   /* Now, for each overlay visual, find the correspinding XVisualInfo	*/
   /* structure and get the "best" overlay visual to use; "best" means	*/
   /* the deepest PseudoColor one available (up to 8 bits).  Preference	*/
   /* is given to TransparentPixel over TransparentMask if they have	*/
   /* the same depth (since TransparentMask takes a bit away from the	*/
   /* depth).								*/

   iw->image.overlay_visual = NULL;
   iw->image.overlay_depth = 0;

   for (overlay = 0; overlay < overlay_count; overlay++) {

      if (oinfo[overlay].type == NotTransparent)
         continue;	/* Some SGI's actually have this! */

      for (i=0; i<nvisuals; i++) {
         if (oinfo[overlay].vid == visuals[i].visualid)
            break;
      }
      if (i == nvisuals) {
         continue;	/* No match found for this overlay (!) */
      }

      /* Don't use it if it's the default visual */
      if (visuals[i].visual == XDefaultVisualOfScreen(XtScreen(iw)))
         continue;

      /* Can't deal with it if it's not PsuedoColor!!!! */
      if (visuals[i].class != PseudoColor)
         continue;

      /* If it's more than 8 bits, toss it out - we currently can't	*/
      /* really deal with something bigger.  Better to use SW overlay.	*/
      if (visuals[i].depth > 8)
         continue;

      /* Throw it out if it's less deep than what we already have */
      if (visuals[i].depth < iw->image.overlay_depth)
         continue;

      /* Throw it out if it's the same depth but not TransparentPixel */
      if (visuals[i].depth == iw->image.overlay_depth &&
          oinfo[overlay].type != TransparentPixel)
         continue;

      /* Finally, we found one we like.  Keep track of it. */

      iw->image.overlay_depth = visuals[i].depth;
      iw->image.overlay_visual = visuals[i].visual;
      iw->image.overlay_cmap_size = visuals[i].colormap_size;
      iw->image.overlay_type = oinfo[overlay].type;
      iw->image.overlay_value = oinfo[overlay].value;
   }

   /* If there's no match, overlay_visual will still be NULL,	*/
   /* indicating no overlay.					*/

   XFree(visuals);
   XFree(oinfo);

#endif
}

/************************************************************************/
/* GetOneGrColor							*/
/*									*/
/* Figures out what to do with one graphics color.  It either sets the	*/
/* dither pattern, allocates a colorcell, or finds the closest color in	*/
/* the existing colormap, depending on the mode.  The "cells" argument	*/
/* is a pointer to an array of XColor structures as returned by		*/
/* XQueryColors.  If NULL, this routine will call XQueryColors itself	*/
/* (actually a subroutine does this), but if this routine is called	*/
/* many times it will save a bunch of server round-trips to call	*/
/* XQueryColors once and pass the array in.				*/
/************************************************************************/

static void
#ifdef _NO_PROTO
GetOneGrColor(iw, color, cells)
   XvicImageWidget iw;
   GrColor *color;
   XColor *cells;
#else
GetOneGrColor(
   XvicImageWidget iw,
   GrColor *color,
   XColor *cells)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   int red, grn, blu;
   int status;

   if (!color->active)
      return;			/* Not active, nothing to do */

   red = color->xcolor.red >> 8;
   grn = color->xcolor.green >> 8;
   blu = color->xcolor.blue >> 8;

   DeallocGrColor(iw, color);

   /* If the overlay widget is available, use it */

   if (iw->image.overlay_widget) {

      FreeGrColorTile(iw, color);
      status = AllocOverlayColor(iw, color, cells, iw->image.overlay_colormap);
      if (!status)
         GetClosestColor(iw, &color->xcolor, cells, iw->image.overlay_colormap,
				iw->image.overlay_cmap_size);
   }

   else {

      /* Color mode (or pseudocolor treated as color) */

      if (biw->bim.image_mode == XvicCOLOR || biw->bim.ps_as_color) {

         switch (biw->bim.colormap_policy) {

            case XvicFULL_COLOR:
               FreeGrColorTile(iw, color);
               color->xcolor.pixel = (red << biw->bim.vis.red_shift) |
				     (grn << biw->bim.vis.green_shift) |
				     (blu << biw->bim.vis.blue_shift);
               break;

            case XvicFULL:
               if (biw->bim.dither_mode == XvicNONE) {
                  FreeGrColorTile(iw, color);
                  color->xcolor.pixel = (red&0xe0) | ((grn>>3)&0x1c) | (blu>>6);
               }
               else
                  CreateGrColorDitherPattern(iw, color);
               break;

            case XvicHALF:
               if (biw->bim.dither_mode == XvicNONE) {
                  FreeGrColorTile(iw, color);
                  color->xcolor.pixel = (((red>>1) & 0x60) | ((grn>>3) & 0x1c) |
					   (blu>>6)) | 0x80;
               }
               else
                  CreateGrColorDitherPattern(iw, color);
               break;

            case XvicDITHER:
            case XvicALLOC:
               CreateGrColorDitherPattern(iw, color);
               break;

            default:			/* shouldn't happen! */
               FatalError(iw, "BadColormapPolicy",
		  "Internal error: Unknown colormap policy in GetOneGrColor()");
         }
      }

      /* BW mode */

      else {

         switch (biw->bim.colormap_policy) {

            case XvicFULL_COLOR:
               FreeGrColorTile(iw, color);
               color->xcolor.pixel = (red << biw->bim.vis.red_shift) |
				     (grn << biw->bim.vis.green_shift) |
				     (blu << biw->bim.vis.blue_shift);
               break;

            case XvicFULL:
               FreeGrColorTile(iw, color);
               GetClosestColor(iw, &color->xcolor, cells, biw->bim.colormap,
				CMAP_SIZE);
               break;

            case XvicHALF:
               FreeGrColorTile(iw, color);
               status = AllocSystemColor(iw, color, NULL, TRUE);
               if (status)				/* got one */
                  MirrorSystemColor(iw, color, cells, biw->bim.colormap);
               else {
                  status=AllocPrivateColor(iw, color, cells, biw->bim.colormap);
                  if (!status)
                     GetClosestColor(iw,&color->xcolor,cells,biw->bim.colormap,	
					CMAP_SIZE);
               }
               break;

            case XvicDITHER:
               CreateGrColorDitherPattern(iw, color);
               break;

            case XvicALLOC:
               if (biw->bim.dither_mode == XvicKAGELS) {
                  CreateGrColorDitherPattern(iw, color);
               }
               else {
                  FreeGrColorTile(iw, color);
                  status = AllocSystemColor(iw, color, cells, FALSE);
                  if (!status)
                     GetClosestColor(iw,&color->xcolor,cells,biw->bim.colormap,
					CMAP_SIZE);
               }
               break;

            default:			/* shouldn't happen! */
               FatalError(iw, "BadColormapPolicy",
		  "Internal error: Unknown colormap policy in GetOneGrColor()");
         }
      }
   }
}

/************************************************************************/
/* GetGrID								*/
/*									*/
/* Return an XvicID to use for a graphics object.  This is equal to	*/
/* a the input ID if it is valid, or a new one if it is 0 or invalid.	*/
/* This code might break if the ID overflows, but that would mean	*/
/* creating >2 billion objects.  Not likely.				*/
/************************************************************************/

static XvicID
#ifdef _NO_PROTO
GetGrID(iw, id)
   XvicImageWidget iw;
   XvicID id;
#else
GetGrID(
   XvicImageWidget iw,
   XvicID id)
#endif /* _NO_PROTO */
{
   XvicID out_id;

   if (id <= 0 || id > iw->image.max_gr_id) {	/* invalid or new ID */
      if (id != 0) {			/* Warn if invalid */
         WarningMsg(iw, "BadObjectID",
			"The specified object ID is not valid.  0 assumed.");
      }
      out_id = iw->image.max_gr_id + 1;
      iw->image.max_gr_id = out_id;
   }
   else
      out_id = id;

   return out_id;
}

/************************************************************************/
/* GetXYFromEvent							*/
/*									*/
/* Return the (x,y) coordinates from the given event.  The event may be	*/
/* one of ButtonPress, ButtonRelease, EnterNotify, LeaveNotify,		*/
/* KeyPress, KeyRelease, or MotionNotify.  The function returns True	*/
/* if one of those events is passed in, False otherwise (in which case	*/
/* the x and y returned values are garbage).				*/
/*									*/
/* It is possible that the event may have been triggered by a widget	*/
/* other than the Image widget.  This should only happen if someone	*/
/* sets translations on the overlay widget (usually accidentally via	*/
/* "*translations").  However, we deal with it here.  "iw" is the	*/
/* XvicImage widget, and "w" is the widget the event is from.  If they	*/
/* are not equal, the coordinates are translated from w's coordinate	*/
/* system into iw's.  Note that the caller must find iw given w.	*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
GetXYFromEvent(iw, w, event, x, y)
   XvicImageWidget iw;
   Widget w;
   XEvent *event;
   int *x;
   int *y;
#else
GetXYFromEvent(
   XvicImageWidget iw,
   Widget w,
   XEvent *event,
   int *x,
   int *y)
#endif /* _NO_PROTO */
{
   Position w_x, w_y, iw_x, iw_y;

   switch (event->type) {
      case ButtonPress:
      case ButtonRelease:
         *x = ((XButtonEvent *)event)->x;
         *y = ((XButtonEvent *)event)->y;
         break;
      case EnterNotify:
      case LeaveNotify:
         *x = ((XCrossingEvent *)event)->x;
         *y = ((XCrossingEvent *)event)->y;
         break;
      case KeyPress:
      case KeyRelease:
         *x = ((XKeyEvent *)event)->x;
         *y = ((XKeyEvent *)event)->y;
         break;
      case MotionNotify:
         *x = ((XMotionEvent *)event)->x;
         *y = ((XMotionEvent *)event)->y;
         break;
      default:
         return False;		/* Unknown event */
   }

   if ((Widget)iw != w) {
      XtTranslateCoords(w, 0, 0, &w_x, &w_y);
      XtTranslateCoords((Widget)iw, 0, 0, &iw_x, &iw_y);
      *x += (w_x - iw_x);
      *y += (w_y - iw_y);
   }

   return True;
}

/************************************************************************/
/* MaskPixmap								*/
/*									*/
/* Perform a logical AND operation between the pixmaps "mask" and	*/
/* "source", and leave the result in "source".  This is used e.g. by	*/
/* the cursor routines to make sure the cursor shape doesn't extend	*/
/* beyond the cursor mask.  If the mask is NULL, nothing is done.	*/
/* Both pixmaps must be the same size and depth.			*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MaskPixmap(iw, mask, source, width, height)
   XvicImageWidget iw;
   Pixmap mask;
   Pixmap source;
   unsigned int width;
   unsigned int height;
#else
MaskPixmap(
   XvicImageWidget iw,
   Pixmap mask,
   Pixmap source,
   unsigned int width,
   unsigned int height)
#endif /* _NO_PROTO */
{
   XGCValues values;
   GC gc;

   if (mask == None)
      return;			/* nothing to do */

   values.function = GXand;

   gc = XCreateGC(XtDisplay((Widget)iw), source, GCFunction, &values);

   XCopyArea(XtDisplay((Widget)iw), mask, source, gc, 0, 0, width, height, 0,0);

   XFreeGC(XtDisplay((Widget)iw), gc);

}

/************************************************************************/
/* MirrorSystemColor							*/
/*									*/
/* Given a color that has just been allocated from the system, mirror	*/
/* it in the private colormap.  The same pixel index is set to the same	*/
/* color, unless the index is already used in the private colormap, in	*/
/* which case we assume that someone already set it to this color	*/
/* (which is not necessarily always true, since an earlier sys alloc	*/
/* could have failed, this pixel position got used w/o system support,	*/
/* then it got freed up.  But that should be a very rare case.)		*/
/* This routine must not be called if a hardware overlay is in use.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MirrorSystemColor(iw, color, cells, cmap)
   XvicImageWidget iw;
   GrColor *color;
   XColor *cells;
   Colormap cmap;
#else
MirrorSystemColor(
   XvicImageWidget iw,
   GrColor *color,
   XColor *cells,
   Colormap cmap)
#endif /* _NO_PROTO */
{
   Display *dpy;

   dpy = XtDisplay(iw);

   /* If the reference count is non-0, assume the color is already	*/
   /* set.  Note that it may be set to a *different* color than the	*/
   /* one we want, but if so, it's a weird situation.			*/

   if (iw->image.private_cmap_refcnt[color->xcolor.pixel] == 0) {
      color->xcolor.flags = DoRed | DoGreen | DoBlue;
      XStoreColors(dpy, cmap, &color->xcolor, 1);
      if (cells != NULL)
         memcpy((void *)&cells[color->xcolor.pixel], (void *)&color->xcolor,
		sizeof(XColor));
   }

   iw->image.private_cmap_refcnt[color->xcolor.pixel]++;
   color->alloc_pvt = True;

   return;

}

/************************************************************************/
/* MoveCursor								*/
/*									*/
/* Moves the cursor to the user-specified coordinates in cursor_x and	*/
/* cursor_y (or _fp versions).  This can actually warp the pointer if	*/
/* the mode is XvicFLOATING, which is generally considered bad style in	*/
/* Motif.  The cursor location is set back to unspecified so we don't	*/
/* change it again next time through.  If the mode is XvicPLANTED, the	*/
/* cursor callback called as well (it is not called for XvicFLOATING	*/
/* because the motion event handler will do it - and XWarpPointer	*/
/* generates a motion event).  If both the integer and _fp resources	*/
/* are set simultaneously, the floating-point version is used.		*/
/************************************************************************/
 
static void
#ifdef _NO_PROTO
MoveCursor(iw)
   XvicImageWidget iw;
#else
MoveCursor(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget)iw;
   Window root, child;
   int root_x, root_y, x, y;
   unsigned int mask;

   if (iw->image.cursor_mode == XvicPLANTED) {
      ErasePlantedCursor(iw);
      if (iw->image.cursor_x != CURSOR_NO_LOC)
         iw->image.plant_curs_x = (double)iw->image.cursor_x;
      if (iw->image.cursor_y != CURSOR_NO_LOC)
         iw->image.plant_curs_y = (double)iw->image.cursor_y;
      if (iw->image.cursor_x_fp != CURSOR_NO_LOC)	/* may override above */
         iw->image.plant_curs_x = iw->image.cursor_x_fp;
      if (iw->image.cursor_y_fp != CURSOR_NO_LOC)
         iw->image.plant_curs_y = iw->image.cursor_y_fp;
      DrawPlantedCursor(iw);

      CallCursorCallback(iw, iw->image.plant_curs_x, iw->image.plant_curs_y,
			 False);
   }
   else {		/* Floating */
      if (!XtIsRealized((Widget)iw)) {
         iw->image.cursor_x = CURSOR_NO_LOC;
         iw->image.cursor_y = CURSOR_NO_LOC;
         iw->image.cursor_x_fp = CURSOR_NO_LOC;
         iw->image.cursor_y_fp = CURSOR_NO_LOC;
         return;			/* Can't move it if not realized */
      }
      if ((iw->image.cursor_x == CURSOR_NO_LOC &&
	   iw->image.cursor_x_fp == CURSOR_NO_LOC) ||
	  (iw->image.cursor_y == CURSOR_NO_LOC &&
	   iw->image.cursor_y_fp == CURSOR_NO_LOC)) { /*Didn't move both. Sigh*/
         XQueryPointer(XtDisplay((Widget)iw), XtWindow((Widget)iw),
			&root, &child, &root_x, &root_y, &x, &y, &mask);
      }
      if (iw->image.cursor_x != CURSOR_NO_LOC)
         x = XC_Img2Scr((double)iw->image.cursor_x) + biw->bim.x_dpy_off;
      if (iw->image.cursor_y != CURSOR_NO_LOC)
         y = YC_Img2Scr((double)iw->image.cursor_y) + biw->bim.y_dpy_off;
      if (iw->image.cursor_x_fp != CURSOR_NO_LOC)	/* may override above */
         x = XC_Img2Scr(iw->image.cursor_x_fp) + biw->bim.x_dpy_off;
      if (iw->image.cursor_y_fp != CURSOR_NO_LOC)
         y = YC_Img2Scr(iw->image.cursor_y_fp) + biw->bim.y_dpy_off;

      XWarpPointer(XtDisplay((Widget)iw), None, XtWindow((Widget)iw),
		0, 0, 0, 0, x, y);
   }
   iw->image.cursor_x = CURSOR_NO_LOC;
   iw->image.cursor_y = CURSOR_NO_LOC;
   iw->image.cursor_x_fp = CURSOR_NO_LOC;
   iw->image.cursor_y_fp = CURSOR_NO_LOC;
}
 
/************************************************************************/
/* MoveOneObject							*/
/*									*/
/* Moves the coordinates for the given object by the given deltas.	*/
/* The object itself is not redrawn; only the coordinates are changed.	*/
/************************************************************************/

static void
#ifdef _NO_PROTO
MoveOneObject(iw, obj, delta_x, delta_y)
   XvicImageWidget iw;
   GrObject *obj;
   double delta_x;
   double delta_y;
#else
MoveOneObject(
   XvicImageWidget iw,
   GrObject *obj,
   double delta_x,
   double delta_y)
#endif /* _NO_PROTO */
{
   int i;

   obj->any.bounds.x1 += delta_x;
   obj->any.bounds.x2 += delta_x;
   obj->any.bounds.y1 += delta_y;
   obj->any.bounds.y2 += delta_y;

   switch (obj->type) {

      case GrArc:
      case GrFillArc:
         obj->arc.x += delta_x;
         obj->arc.y += delta_y;
         break;

      case GrArcs:
      case GrFillArcs:
         for (i=0; i<obj->arcs.narcs; i++) {
            obj->arcs.arcs[i].x += delta_x;
            obj->arcs.arcs[i].y += delta_y;
         }
         break;

      case GrBitmap:
         obj->bitmap.x += delta_x;
         obj->bitmap.y += delta_y;
         break;

      case GrLine:
         obj->line.x1 += delta_x;
         obj->line.y1 += delta_y;
         obj->line.x2 += delta_x;
         obj->line.y2 += delta_y;
         break;

      case GrLines:
      case GrPoints:
      case GrFillPolygon:
         if (obj->lines.npoints > 0) {
            obj->lines.points[0].x += delta_x;
            obj->lines.points[0].y += delta_y;
            if (obj->lines.mode != CoordModePrevious) {

               /* Not relative, so fix all the coordinates */

               for (i=1; i<obj->lines.npoints; i++) {
                  obj->lines.points[i].x += delta_x;
                  obj->lines.points[i].y += delta_y;
               }
            }
         }
         break;

      case GrPoint:
         obj->point.x += delta_x;
         obj->point.y += delta_y;
         break;

      case GrRectangle:
      case GrFillRectangle:
         obj->rectangle.x += delta_x;
         obj->rectangle.y += delta_y;
         break;

      case GrRectangles:
      case GrFillRectangles:
         for (i=0; i<obj->rectangles.nrectangles; i++) {
            obj->rectangles.rectangles[i].x += delta_x;
            obj->rectangles.rectangles[i].y += delta_y;
         }
         break;

      case GrSegments:
         for (i=0; i<obj->segments.nsegments; i++) {
            obj->segments.segments[i].x1 += delta_x;
            obj->segments.segments[i].x2 += delta_x;
            obj->segments.segments[i].y1 += delta_y;
            obj->segments.segments[i].y2 += delta_y;
         }
         break;

      case GrImageString:
      case GrString:
         obj->string.x += delta_x;
         obj->string.y += delta_y;
         break;

      case GrImageString16:
      case GrString16:
         obj->string16.x += delta_x;
         obj->string16.y += delta_y;
         break;

      case GrText:
         obj->text.x += delta_x;
         obj->text.y += delta_y;
         break;

      case GrText16:
         obj->text16.x += delta_x;
         obj->text16.y += delta_y;
         break;

      default:
         FatalError(iw, "BadObjectType4",
		  "Internal error: Unknown object type in MoveOneObject().");
   }
}

/************************************************************************/
/* NewObject								*/
/*									*/
/* Allocates a new graphics object structure and adds it to the iw's	*/
/* list.  The arguments are checked for validity and a NULL is returned	*/
/* if they are bad.  All "any" fields are filled in except for type and	*/
/* bounds - the	caller *must* fill in both of those.			*/
/************************************************************************/
 
static GrObject *
#ifdef _NO_PROTO
NewObject(iw, id, gc, color, size)
   XvicImageWidget iw;
   XvicID id;
   XvicGC gc;
   XvicColor color;
   int size;
#else
NewObject(
   XvicImageWidget iw,
   XvicID id,
   XvicGC gc,
   XvicColor color,
   int size)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   GrObject *obj;
   int i, n;
   GrObject **tmp;

   if (!CheckValidGrGC(iw, gc, True))
      return NULL;

   if (!CheckValidGrColor(iw, color))
      return NULL;

   id = GetGrID(iw, id);

   obj = _XvicMalloc(biw, size);

   obj->any.id = id;
   obj->any.color = color;
   obj->any.gc = gc;

   /* Check for empty slots in the table */

   for (i=0; i<iw->image.num_gr_objects; i++) {
      if (iw->image.gr_objects[i] == NULL)		/* found one */
         break;
   }

   if (i == iw->image.num_gr_objects) {	/* None found, expand the table */
      n = iw->image.num_gr_objects + 10;
      tmp = _XvicMalloc(biw, n * sizeof(GrObject *));
      if (iw->image.num_gr_objects > 0)
         memcpy((void *)tmp, (void *)iw->image.gr_objects,
		iw->image.num_gr_objects * sizeof(GrObject *));
      if (iw->image.gr_objects)
         _XvicFree(biw, iw->image.gr_objects,
		iw->image.num_gr_objects * sizeof(GrObject *));
      iw->image.gr_objects = tmp;

      for (i = iw->image.num_gr_objects; i<n; i++) {
         iw->image.gr_objects[i] = NULL;
      }
      i = iw->image.num_gr_objects;
      iw->image.num_gr_objects = n;
   }

   /* Install this object in the table */

   iw->image.gr_objects[i] = obj;

   return obj;

}

/************************************************************************/
/* RepaintOverlayRegion							*/
/*									*/
/* Redraws the overlay in the area specified by rgn (Image coords).	*/
/* For hardware overlay, we simply call ExposeOverlay for each		*/
/* rectangle in the region (ExposeOverlay will clear the area first).	*/
/* For software, we call _XvicRedisplay_Internal to redraw the image,	*/
/* which will itself call ExposeOverlay to redraw the graphics.		*/
/************************************************************************/
 
static void
#ifdef _NO_PROTO
RepaintOverlayRegion(iw, rgn)
   XvicImageWidget iw;
   _XvicRegion *rgn;
#else
RepaintOverlayRegion(
   XvicImageWidget iw,
   _XvicRegion *rgn)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;
   int i, n;
   _XvicRect *rects;
   _XvicRect bounds;
   XExposeEvent ev;

   /* Make sure overlay/widget is realized before drawing! */

   if (iw->image.overlay_widget) {
      if (!XtIsRealized(iw->image.overlay_widget))
         return;
   }
   else {
      if (!XtIsRealized((Widget)iw))
         return;
   }

   rects = _XvicRegionGetRectangles(rgn);
   n = _XvicRegionGetNumRects(rgn);

   for (i=0; i<n; i++) {

      /* If we have an overlay, erase and redisplay only it.  Otherwise,*/
      /* redisplay the image too.  ExposeOverlay wants Dpy coordinates	*/
      /* while Redisplay_Internal wants Scr.  Oh well.			*/

      if (iw->image.overlay_widget) {
         bounds.x1 = X1_Img2Dpy(rects[i].x1);
         bounds.x2 = X2_Img2Dpy(rects[i].x2);
         bounds.y1 = Y1_Img2Dpy(rects[i].y1);
         bounds.y2 = Y2_Img2Dpy(rects[i].y2);
         ExposeOverlay((Widget)iw, NULL, &bounds);
      }
      else {
         bounds.x1 = X1_Img2Scr(rects[i].x1);
         bounds.x2 = X2_Img2Scr(rects[i].x2);
         bounds.y1 = Y1_Img2Scr(rects[i].y1);
         bounds.y2 = Y2_Img2Scr(rects[i].y2);
         ev.x = bounds.x1;
         ev.y = bounds.y1;
         ev.width = bounds.x2 - bounds.x1 + 1;
         ev.height = bounds.y2 - bounds.y1 + 1;
         _XvicRedisplay_Internal(biw, &ev, NULL);
      }
   }

}

/************************************************************************/
/* SetMotionEventHandler						*/
/*									*/
/* Register or remove the mouse motion event handler based on the	*/
/* setting of the cursor_mode and track_floating_cursor resources.	*/
/* Since XtRemoveEventHandler returns quietly if there is no handler	*/
/* to remove, and XtAddEventHandler won't add multiple identical	*/
/* handlers, we don't need to worry about if the handler is currently	*/
/* installed or not.  Nice, huh?  We use the same event handler for	*/
/* EnterNotify and LeaveNotify as well.					*/
/************************************************************************/
 
static void
#ifdef _NO_PROTO
SetMotionEventHandler(iw)
   XvicImageWidget iw;
#else
SetMotionEventHandler(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{
   EventMask mask;

   mask = PointerMotionMask | EnterWindowMask | LeaveWindowMask;

   if (iw->image.cursor_mode == XvicFLOATING && iw->image.track_floating_cursor)
      XtAddEventHandler((Widget)iw, mask, False,
		MotionEventHandler, NULL);
   else
      XtRemoveEventHandler((Widget)iw, mask, False,
		MotionEventHandler, NULL);
}

/************************************************************************/
/* SetUpGraphicsGC							*/
/*									*/
/* Sets up an XGCValues struct and the corresponding mask to enable	*/
/* drawing in the graphics color specified by the GrColor argument.	*/
/* The mask is *not* zeroed, so it must be either zeroed by the caller	*/
/* or it must contain useful values (that have already been set in the	*/
/* XGCValues struct).							*/
/************************************************************************/
 
static void
#ifdef _NO_PROTO
SetUpGraphicsGC(iw, color, mask, values)
   XvicImageWidget iw;
   GrColor *color;
   unsigned long *mask;
   XGCValues *values;
#else
SetUpGraphicsGC(
   XvicImageWidget iw,
   GrColor *color,
   unsigned long *mask,
   XGCValues *values)
#endif /* _NO_PROTO */
{
   XvicBasicImageWidget biw = (XvicBasicImageWidget) iw;

   if (color->gc_tile) {
      values->tile = color->gc_tile;
      values->fill_style = FillTiled;
      /* Must use modulo for TS origin because they are short ints in X */
      values->ts_x_origin = IMOD(-XPAN, color->width);
      values->ts_y_origin = IMOD(-YPAN, color->height);
      *mask |= GCTile | GCFillStyle |
			GCTileStipXOrigin | GCTileStipYOrigin;
   }
   else {
      values->foreground = color->xcolor.pixel;
      values->fill_style = FillSolid;
      values->ts_x_origin = 0;
      values->ts_y_origin = 0;
      *mask |= GCForeground | GCFillStyle |
			GCTileStipXOrigin | GCTileStipYOrigin;
   }
   /* We don't want IncludeInferiors here because either the overlay	*/
   /* doesn't exist so it doesn't matter, or it does exist and we're	*/
   /* drawing on it.  These GC's are not used for drawing on the image.	*/

}

/************************************************************************/
/* SetXCursor								*/
/*									*/
/* Sets the X-windows cursor for the window.  It must already have been	*/
/* created.  This routine just returns if the window is not realized.	*/
/* If the cursor is planted (not floating), the defined cursor is	*/
/* removed instead.							*/
/************************************************************************/
 
static void
#ifdef _NO_PROTO
SetXCursor(iw)
   XvicImageWidget iw;
#else
SetXCursor(
   XvicImageWidget iw)
#endif /* _NO_PROTO */
{

   if (!XtIsRealized((Widget)iw))
      return;				/* nothing to do (yet) */

   if (iw->image.cursor_mode == XvicPLANTED) {
      XUndefineCursor(XtDisplay((Widget)iw), XtWindow((Widget)iw));
   }
   else {
      XDefineCursor(XtDisplay((Widget)iw), XtWindow((Widget)iw),
		iw->image.x_cursor);
   }

   return;
}

/************************************************************************/
/* WarningMsg								*/
/*									*/
/* Report a warning message.  "name" is the identifier for the warning  */
/* message, while "def" is the default string to use if the error       */
/* database is not available (which is almost always the case).         */
/* This is just syntactic sugar for XtAppWarningMsg().                  */
/************************************************************************/
 
static void
#ifdef _NO_PROTO
WarningMsg(iw, name, def)
   XvicImageWidget iw;
   char *name;
   char *def;
#else
WarningMsg(
   XvicImageWidget iw,
   char *name,
   char *def)
#endif /* _NO_PROTO */
{
   XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
        name, "XvicImage", "XvicImageWidgetWarning", def,
        (String *)NULL, (Cardinal *)NULL);
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
/* CvtStringToCursorMode						*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToCursorMode(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToCursorMode(
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
	    "wrongParameters", "cvtStringToCursorMode",
	    "XtToolkitError",
	 "String to CursorMode conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "floating"))
      val = XvicFLOATING;
   else if (StringsAreEqual((char *)fromVal->addr, "planted"))
      val = XvicPLANTED;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRCursorMode);
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
/* CvtStringToScrollBarDisplayPolicy					*/
/************************************************************************/

static Boolean
#ifdef _NO_PROTO
CvtStringToScrollBarDisplayPolicy(dpy, args, num_args, fromVal, toVal, data)
   Display *dpy;
   XrmValue *args;
   Cardinal *num_args;
   XrmValue *fromVal;
   XrmValue *toVal;
   XtPointer *data;
#else
CvtStringToScrollBarDisplayPolicy(
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
	    "wrongParameters", "cvtStringToScrollBarDisplayPolicy",
	    "XtToolkitError",
	 "String to ScrollBarDisplayPolicy conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);

   if (StringsAreEqual((char *)fromVal->addr, "static"))
      val = XvicSTATIC;
   else if (StringsAreEqual((char *)fromVal->addr, "as_needed"))
      val = XvicAS_NEEDED;
   else if (StringsAreEqual((char *)fromVal->addr, "never"))
      val = XvicNEVER;
   else {
      XtDisplayStringConversionWarning(dpy, (char *)fromVal->addr,
		XvicRScrollBarDisplayPolicy);
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
			XvicRCursorMode,
			CvtStringToCursorMode,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
   XtSetTypeConverter(XtRString,
			XvicRScrollBarDisplayPolicy,
			CvtStringToScrollBarDisplayPolicy,
			NULL, 0, XtCacheAll, (XtDestructor)NULL);
}

/************************************************************************/

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicImageOverlay.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>	/* for XtParent, XtClass */
#include "XvicImageOverlayP.h"

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

static void Realize();
static void Redisplay();

#else

/* Core Methods */

static void Realize(
		Widget w,
		XtValueMask *value_mask,
		XSetWindowAttributes *attributes);
static void Redisplay(
		Widget w,
		XEvent *event,
		Region region);

#endif /* _NO_PROTO */

/************************************************************************/
/* Translation tables							*/
/************************************************************************/

/* None */

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
	XvicNvisual,
	XvicCVisual,
	XvicRVisual,
	sizeof(Visual *),
	XtOffsetOf(XvicImageOverlayRec, ov.visual),
	XtRImmediate,
	(XtPointer) NULL
    }
};

/************************************************************************/
/* Class record								*/
/************************************************************************/

externaldef(xvicimageoverlayclassrec)
		XvicImageOverlayClassRec xvicImageOverlayClassRec =
{
  { /* core_class record */
    /* superclass         */	(WidgetClass) &widgetClassRec,
    /* class_name         */	"XvicImageOverlay",
    /* widget_size        */	sizeof(XvicImageOverlayRec),
    /* class_initialize   */	NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */	NULL,
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
    /* destroy            */    NULL,
    /* resize             */    NULL,
    /* expose             */    Redisplay,
    /* set_values         */	NULL,
    /* set_values_hook    */	(XtArgsFunc) NULL,
    /* set_values_almost  */	NULL,
    /* get_values_hook    */	(XtArgsProc) NULL,
    /* accept_focus       */	(XtAcceptFocusProc) NULL,
    /* version            */	XtVersion,
    /* callback_private   */	NULL,
    /* tm_table           */	NULL,
    /* query_geometry     */	NULL,
    /* display_accelerator*/	(XtStringProc) NULL,
    /* extension record   */	NULL,
  },
  { /* image_overlay_class record */
    /* extension          */	NULL,
  }

};

externaldef(xvicimageoverlaywidgetclass) WidgetClass xvicImageOverlayWidgetClass=
				(WidgetClass) &xvicImageOverlayClassRec;

/************************************************************************/
/************************************************************************/
/*  M E T H O D S							*/
/************************************************************************/
/************************************************************************/

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
   XvicImageOverlayWidget ow = (XvicImageOverlayWidget) w;

   *value_mask |= CWColormap;
   attributes->colormap = ow->core.colormap;

   *value_mask |= CWBitGravity;
   attributes->bit_gravity = NorthWestGravity;

   *value_mask |= CWBorderPixel;
   attributes->border_pixel = 0;		/*!!!!*/ /* fix this */

   *value_mask |= CWBackPixel;
   attributes->background_pixel = ow->core.background_pixel;

   XtCreateWindow((Widget)ow, InputOutput, ow->ov.visual,
		*value_mask, attributes);

}

/************************************************************************/
/* Redisplay (Expose) method						*/
/*									*/
/* This method just calls the Redisplay method of the parent widget.	*/
/* However, it first offsets the x/y position in the event by the	*/
/* x/y position of this widget's window relative to its parent, so	*/
/* the expose event will cover the same area on the screen.		*/
/* The region is not adjusted, since Image/BasicImage don't use it.	*/
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
   XvicImageOverlayWidget ow = (XvicImageOverlayWidget) w;
   Widget parent;
   XExposeEvent *exp_event = (XExposeEvent *)event;
   XExposeEvent new_event;

   DPR(("overlay redisplay: x=%d, y=%d\n", exp_event->x, exp_event->y));

   /* Copy the event so we don't change Xt's memory */

   memcpy((void *)&new_event, (void *)exp_event, sizeof(new_event));

   new_event.x += ow->core.x;
   new_event.y += ow->core.y;

   /* Now call the parent's Expose method */

   parent = XtParent((Widget)ow);
   (*XtClass(parent)->core_class.expose)(parent, (XEvent *)&new_event, region);

}

/************************************************************************/
/************************************************************************/
/*  P U B L I C   F U N C T I O N S					*/
/************************************************************************/
/************************************************************************/

/* None */

/************************************************************************/
/************************************************************************/
/*  M I S C E L L A N E O U S   F U N C T I O N S			*/
/************************************************************************/
/************************************************************************/

/* None */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicRegion.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* Utilities for handling regions.  The functionality here closely      */
/* mimics the X Region routines, but the X routines use short ints      */
/* for coordinates, and we need full ints.  These routines are intended */
/* for internal use of the Xvic routines only.                          */
/************************************************************************/

#include "XvicRegion.h"
#include <stdlib.h>
#include <string.h>

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#ifdef __cplusplus	/* C++ likes NULL defined as 0 */
#define NULL 0
#else
#define NULL ((void*)0)
#endif
#endif

/************************************************************************/
/* Internal routine to add rectangles to a region			*/
/************************************************************************/

static int
#ifdef _NO_PROTO
more_rects(rgn, num)
   _XvicRegion *rgn;
   int num;
#else
more_rects(
   _XvicRegion *rgn,
   int num)
#endif
{
   _XvicRect *tmp;

   if (rgn->num_rects + num > rgn->rects_alloc) {
      rgn->rects_alloc += MAX(num, 5);
      tmp = (_XvicRect *)malloc(sizeof(_XvicRect) * rgn->rects_alloc);
      if (tmp == NULL) {
         rgn->rects_alloc -= MAX(num, 5);
         return FALSE;
      }
      if (rgn->num_rects > 0)
         memcpy(tmp, rgn->rects, sizeof(_XvicRect) * rgn->num_rects);
      if (rgn->rects)
         free(rgn->rects);
      rgn->rects = tmp;
   }
   return TRUE;
}

/************************************************************************/
/* Intersects two rectangles, returning the result in a third.		*/
/* Returns TRUE if they intersect, FALSE if not.			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRectIntersectRect(r1, r2, dest)
   _XvicRect *r1;
   _XvicRect *r2;
   _XvicRect *dest;
#else
_XvicRectIntersectRect(
   _XvicRect *r1,
   _XvicRect *r2,
   _XvicRect *dest)
#endif
{
   dest->x1 = MAX(r1->x1, r2->x1);
   dest->x2 = MIN(r1->x2, r2->x2);
   dest->y1 = MAX(r1->y1, r2->y1);
   dest->y2 = MIN(r1->y2, r2->y2);

   if (dest->x1 > dest->x2 || dest->y1 > dest->y2)
      return FALSE;			/* no intersection */
   return TRUE;
}

/************************************************************************/
/* Returns the bounding rectangle for a region				*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionBounds(rgn, rect)
   _XvicRegion *rgn;
   _XvicRect *rect;
#else
_XvicRegionBounds(
   _XvicRegion *rgn,
   _XvicRect *rect)
#endif
{
   int i;

   if (rgn->num_rects == 0)
      return FALSE;

   rect->x1 = rgn->rects[0].x1;
   rect->x2 = rgn->rects[0].x2;
   rect->y1 = rgn->rects[0].y1;
   rect->y2 = rgn->rects[0].y2;

   for (i=1; i<rgn->num_rects; i++) {
      if (rgn->rects[i].x1 < rect->x1)
         rect->x1 = rgn->rects[i].x1;
      if (rgn->rects[i].x2 > rect->x2)
         rect->x2 = rgn->rects[i].x2;
      if (rgn->rects[i].y1 < rect->y1)
         rect->y1 = rgn->rects[i].y1;
      if (rgn->rects[i].y2 > rect->y2)
         rect->y2 = rgn->rects[i].y2;
   }
   return TRUE;
}

/************************************************************************/
/* Create an empty region						*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreate()
#else
_XvicRegionCreate()
#endif
{
   _XvicRegion *rgn;

   rgn = (_XvicRegion *)malloc(sizeof(_XvicRegion));
   if (rgn == NULL)
      return NULL;

   rgn->num_rects = 0;
   rgn->rects_alloc = 0;
   rgn->rects = NULL;

   return rgn;
}

/************************************************************************/
/* Intersect a rectangle with a region, returning a new region.		*/
/* We could just create a new region, copy into it, and call		*/
/* RegionIntersect, but that is terribly inefficient for the use this	*/
/* routine is put to (culling out everything except the tile).		*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreateIntersect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionCreateIntersect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   _XvicRegion *new_rgn;
   _XvicRect temp_rect;
   int i;

   new_rgn = _XvicRegionCreate();
   if (new_rgn == NULL)
      return NULL;

   for (i=0; i<rgn->num_rects; i++) {

      /* First clip each box to the rectangle */

      memcpy(&temp_rect, &rgn->rects[i], sizeof(_XvicRect));

      if (temp_rect.x1 < rect->x1)
         temp_rect.x1 = rect->x1;
      if (temp_rect.y1 < rect->y1)
         temp_rect.y1 = rect->y1;
      if (temp_rect.x2 > rect->x2)
         temp_rect.x2 = rect->x2;
      if (temp_rect.y2 > rect->y2)
         temp_rect.y2 = rect->y2;

      /* If the coordinates are not in the wrong order, the box has not	*/
      /* been fully clipped, so copy it into the destination.		*/

      if ((temp_rect.x1 <= temp_rect.x2) &&
          (temp_rect.y1 <= temp_rect.y2)) {

         if (!more_rects(new_rgn, 1)) {
            _XvicRegionDestroy(new_rgn);
            return NULL;
         }
         memcpy(&new_rgn->rects[new_rgn->num_rects],&temp_rect,sizeof(_XvicRect));
         new_rgn->num_rects++;
      }
   }
   return new_rgn;
}

/************************************************************************/
/* Create a region from a single rectangle				*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreateRect(rect)
   _XvicRect *rect;
#else
_XvicRegionCreateRect(
   _XvicRect *rect)
#endif
{
   _XvicRegion *tmp;

   tmp = _XvicRegionCreate();
   if (tmp == NULL)
      return NULL;

   if (!more_rects(tmp, 1)) {
      _XvicRegionDestroy(tmp);
      return NULL;
   }

   memcpy(&tmp->rects[0], rect, sizeof(_XvicRect));
   tmp->num_rects = 1;

   return tmp;
}

/************************************************************************/
/* Destroys a region							*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicRegionDestroy(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionDestroy(
   _XvicRegion *rgn)
#endif
{
   free(rgn->rects);
   free(rgn);
}

/************************************************************************/
/* Returns the number of rectangles in a region				*/
/************************************************************************/

int 
#ifdef _NO_PROTO
_XvicRegionGetNumRects(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionGetNumRects(
   _XvicRegion *rgn)
#endif
{
   return rgn->num_rects;
}

/************************************************************************/
/* Returns the list of rectangles in a region				*/
/************************************************************************/

_XvicRect * 
#ifdef _NO_PROTO
_XvicRegionGetRectangles(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionGetRectangles(
   _XvicRegion *rgn)
#endif
{
   return rgn->rects;
}

/************************************************************************/
/* Returns TRUE if any part of the region is inside the given rectangle.*/
/************************************************************************/

int 
#ifdef _NO_PROTO
_XvicRegionHasRect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionHasRect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i;

   for (i=0; i<rgn->num_rects; i++) {

      if (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2 &&
          rgn->rects[i].y2 >= rect->y1 && rgn->rects[i].y1 <= rect->y2)
         return TRUE;
   }
   return FALSE;
}

/************************************************************************/
/* Intersect a rectangle with a region, discarding any part of the	*/
/* region outside of the rectangle					*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionIntersect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionIntersect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i, j;

   for (i=0; i<rgn->num_rects; i++) {

      /* First clip each box to the rectangle */

      if (rgn->rects[i].x1 < rect->x1)
         rgn->rects[i].x1 = rect->x1;
      if (rgn->rects[i].y1 < rect->y1)
         rgn->rects[i].y1 = rect->y1;
      if (rgn->rects[i].x2 > rect->x2)
         rgn->rects[i].x2 = rect->x2;
      if (rgn->rects[i].y2 > rect->y2)
         rgn->rects[i].y2 = rect->y2;

      /* If the coordinates are in the wrong order, the box has been  */
      /* fully clipped, so discard it by moving everything else down. */

      if ((rgn->rects[i].x1 > rgn->rects[i].x2) ||
          (rgn->rects[i].y1 > rgn->rects[i].y2)) {

         for (j = i+1; j < rgn->num_rects; j++)
            memcpy(&rgn->rects[j-1], &rgn->rects[j], sizeof(_XvicRect));
         rgn->num_rects--;
         i--;			/* compensate for i++ to re-do this index */
      }
   }
   return TRUE;
}

/************************************************************************/
/* Returns TRUE if region is empty, FALSE if it is not			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionIsEmpty(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionIsEmpty(
   _XvicRegion *rgn)
#endif
{
   if (rgn->num_rects == 0)
      return TRUE;
   else
      return FALSE;
}

/************************************************************************/
/* Adds an offset to all coordinates in a region			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionOffset(rgn, xoff, yoff)
   _XvicRegion *rgn;
   int xoff;
   int yoff;
#else
_XvicRegionOffset(
   _XvicRegion *rgn,
   int xoff,
   int yoff)
#endif
{
   int i;

   for (i=0; i<rgn->num_rects; i++) {
      rgn->rects[i].x1 += xoff;
      rgn->rects[i].x2 += xoff;
      rgn->rects[i].y1 += yoff;
      rgn->rects[i].y2 += yoff;
   }
   return TRUE;
}

/************************************************************************/
/* Subtract a rectangle from a region, leaving only parts of the region	*/
/* that are outside the rectangle.  The general idea here is that Y	*/
/* clipping is done first, then X.  Any box that straddles the edge of	*/
/* the rectangle is split into two.  The first is completely clipped;	*/
/* the second is left until its turn in the loop to be clipped.		*/
/* The routine returns TRUE if the operation was successful, and FALSE	*/
/* if it ran out of memory.  This routine is different than others in 	*/
/* this package because a FALSE return leaves the job partially		*/
/* complete (instead of undoing it).  However, the same call may be	*/
/* performed again (once some memory has been freed); in other words,	*/
/* this operation is restartable.					*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionSubtract(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionSubtract(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i, j;

   for (i=0; i<rgn->num_rects; i++) {

      /* First subtract the Y direction */

      if ((rgn->rects[i].y1 < rect->y1 && rgn->rects[i].y2 >= rect->y1) &&
          (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2)) {

         /* Straddles top edge and there is an X intersect, so clip old	*/
         /* box to top part and create new box for the rest		*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].x1 = rgn->rects[i].x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[rgn->num_rects].y1 = rect->y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[i].y2 = rect->y1 - 1;
         rgn->num_rects++;
      }

      if ((rgn->rects[i].y1 <= rect->y2 && rgn->rects[i].y2 > rect->y2) &&
          (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2)) {

         /* Straddled bottom edge and there is an X intersect, so clip	*/
         /* old box to top part and create new box for the bottom	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].x1 = rgn->rects[i].x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[rgn->num_rects].y1 = rect->y2 + 1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[i].y2 = rect->y2;
         rgn->num_rects++;
      }

      /* Now subtract the X direction.  We know that Y is already clipped */

      if ((rgn->rects[i].x1 < rect->x1 && rgn->rects[i].x2 >= rect->x1) &&
          (rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2)) {

         /* Straddles left edge and there is a Y intersect, so clip	*/
         /* old box to left part and create new box for the rest	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].y1 = rgn->rects[i].y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[rgn->num_rects].x1 = rect->x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[i].x2 = rect->x1 - 1;
         rgn->num_rects++;
      }

      if ((rgn->rects[i].x1 <= rect->x2 && rgn->rects[i].x2 > rect->x2) &&
          (rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2)) {

         /* Straddled right edge and there is a Y intersect, so clip	*/
         /* old box to left part and create new box for the right	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].y1 = rgn->rects[i].y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[rgn->num_rects].x1 = rect->x2 + 1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[i].x2 = rect->x2;
         rgn->num_rects++;
      }

      /* Now that the box is completely clipped, see if it is	*/
      /* inside the rectangle.  If so, delete it.		*/

      if (rgn->rects[i].x1 >= rect->x1 && rgn->rects[i].x2 <= rect->x2 &&
          rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2) {

         for (j = i+1; j < rgn->num_rects; j++)
            memcpy(&rgn->rects[j-1], &rgn->rects[j], sizeof(_XvicRect));
         rgn->num_rects--;
         i--;		/* compensate for i++ to re-do this index */
      }
   }

   return TRUE;
}

/************************************************************************/
/* Add (logical union) a rectangle into a region			*/
/* **** TO DO ****!!!!							*/
/* Possible optimization not yet added: merge rects with matching y2	*/
/* but different y1's, and put the excess y into a new rect.  This	*/
/* would help with the ExposeInWork pan case, since a few tiles at the	*/
/* top have already been removed from the damage list.			*/
/* This routine returns FALSE on memory error, and leaves the region	*/
/* in a partially-completed state.  Like _XvicRegionSubtract, however,	*/
/* the operation is restartable (after some memory is freed).		*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionUnion(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionUnion(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i;

   /* Check for an easy merge: if y's match and x's overlap for any	*/
   /* rect, just merge the rects and we're done.  This is a common case	*/
   /* due to the way the tiles work.					*/

   for (i=0; i<rgn->num_rects; i++) {		/* Check for an easy merge */
      if (rgn->rects[i].y1 == rect->y1 && rgn->rects[i].y2 == rect->y2) {
	 /* Y height matches, check for L side */
         if (rect->x2+1 >= rgn->rects[i].x1 && rect->x1 <= rgn->rects[i].x1) {
            rgn->rects[i].x1 = rect->x1;	/* Left side merge */
            rgn->rects[i].x2 = MAX(rgn->rects[i].x2, rect->x2);
            return TRUE;
         }
         /* Now check for R side */
         if (rect->x1-1 <= rgn->rects[i].x2 && rect->x2 >= rgn->rects[i].x2) {
            rgn->rects[i].x2 = rect->x2;	/* Right side merge */
            rgn->rects[i].x1 = MIN(rgn->rects[i].x1, rect->x1);
            return TRUE;
         }
      }
   }

   /* Not easy, do it the hard way by subtracting the rect from the	*/
   /* region (to prevent overlaps), then adding the new rect.		*/

   if (!_XvicRegionSubtract(rect, rgn))
      return FALSE;

   if (!more_rects(rgn, 1))
      return FALSE;

   memcpy(&rgn->rects[rgn->num_rects], rect, sizeof(_XvicRect));
   rgn->num_rects++;

   return TRUE;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicStringCursor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "XvicImageP.h"
#include <X11/cursorfont.h>
#include <stdlib.h>

#ifndef NO_XMU
#if defined(vms) || defined(__VMS)
#include <Xmu/CurUtil.h>
#include <Xmu/Drawing.h>
#else
#include <X11/Xmu/CurUtil.h>
#include <X11/Xmu/Drawing.h>
#endif
#endif

#if XtSpecificationRelease <= 4
#include <X11/StringDefs.h>		/* for XtRString */
#endif

#define xvicPATH_MAX 1024	/* xvic prefix to avoid name collision */

#define FONTSPECIFIER		"FONT "

/************************************************************************/
/* _XvicImageSetStringCursor						*/
/*									*/
/* Converts a String (from a resource, generally) into a cursor for	*/
/* the image widget.  The cursor is not returned; rather, the cursor	*/
/* setting routines are directly called.  This code is lifted largely	*/
/* intact from XmuCvtStringToCursor.					*/
/*									*/
/* The allowed formats are the same as for XmuCvtStringToCursor:	*/
/* 1) Standard cursor name (from cursorfont.h)				*/
/* 2) Font name and glyph index of the form				*/
/*    "FONT fontname index [[font] index]", (where the first pair is	*/
/*    the cursor shape and the second is the mask)			*/
/* 3) Bitmap file name (absolute, or relative to the global resource	*/
/*    bitmapFilePath, class BitmapFilePath).  If the resource is not	*/
/*    defined, the default value is the build symbol BITMAPDIR.  The	*/
/*    mask, if present, has the same name but has "Mask" or "msk"	*/
/*    appended.								*/
/*									*/
/* If the Xmu library is not available, define NO_XMU on the compile	*/
/* line.  In that case, only the second format is allowed, or the	*/
/* value DEFAULT_CURSOR (usually "crosshair") (which is special because	*/
/* it is the default).							*/
/*									*/
/* This routine is in a separate file because of 1) Xmu dependency, and	*/
/* 2) the code is largely lifted from Xmu.				*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicImageSetStringCursor(iw, name)
   XvicImageWidget iw;
   char *name;
#else
_XvicImageSetStringCursor(
   XvicImageWidget iw,
   char *name)
#endif /* _NO_PROTO */
{
   Display *dpy;
   char source_name[xvicPATH_MAX], mask_name[xvicPATH_MAX];
   int source_char, mask_char, fields;
   Font source_font, mask_font;
   XrmValue fromString, toFont;
   Boolean success;
#ifndef NO_XMU
   int i;
   unsigned int shape;
   Pixmap source, mask;
   int xhot, yhot;
   int len;
#endif

   dpy = XtDisplay((Widget)iw);

   /* Check for font name/glyph index form */

   if (strncmp(FONTSPECIFIER, name, strlen(FONTSPECIFIER)) == 0) {

      fields = sscanf(name, "FONT %s %d %s %d", source_name, &source_char,
			mask_name, &mask_char);
      if (fields < 2 || fields > 4) {
         XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
		"BadCursorString", "XvicImage", "XvicImageWidgetError",
		"Invalid format for Cursor string resource, cursor ignored",
		(String *)NULL, (Cardinal *)NULL);
         return;
      }

      fromString.addr = source_name;
      fromString.size = strlen(source_name) + 1;

#if XtSpecificationRelease <= 4
      XtConvert((Widget)iw, XtRString, &fromString, XtRFont, &toFont);
      success = (toFont.addr != NULL);
      if (success)
         source_font = *(Font *)toFont.addr;
#else
      toFont.addr = (XtPointer) &source_font;
      toFont.size = sizeof(Font);
      success = XtConvertAndStore((Widget)iw, XtRString, &fromString,
			XtRFont, &toFont);
#endif
      if (!success) {
         XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
		"BadCursorFont", "XvicImage", "XvicImageWidgetError",
		"Invalid font for Cursor string resource, cursor ignored",
		(String *)NULL, (Cardinal *)NULL);
         return;
      }

      switch (fields) {
         case 2:		/* defaulted mask font & char */
            mask_font = None;		/* thus there is no mask, really */
            mask_char = 0;
            break;

         case 3:		/* defaulted mask font */
            mask_font = source_font;
            mask_char = atoi(mask_name);
            break;

         case 4:		/* specified mask font & char */
            fromString.addr = mask_name;
            fromString.size = strlen(mask_name) + 1;

#if XtSpecificationRelease <= 4
            XtConvert((Widget)iw, XtRString, &fromString, XtRFont, &toFont);
            success = (toFont.addr != NULL);
            if (success)
               mask_font = *(Font *)toFont.addr;
#else
            toFont.addr = (XtPointer) &mask_font;
            toFont.size = sizeof(Font);
            success = XtConvertAndStore((Widget)iw, XtRString, &fromString,
				XtRFont, &toFont);
#endif
            if (!success) {
               XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
		"BadCursorMaskFont", "XvicImage", "XvicImageWidgetError",
		"Invalid mask font for Cursor string resource, cursor ignored",
		(String *)NULL, (Cardinal *)NULL);
               return;
            }
      }

      _XvicImageSetGlyphCursor(iw, source_font, mask_font,
		source_char, mask_char);
/*!!!! Do we need to free fonts here???? !!!!*/

      return;
   }

#ifdef NO_XMU

   if (strcmp(name, DEFAULT_CURSOR) == 0) {
      _XvicImageSetFontCursor(iw, DEFAULT_CURSOR_SHAPE);  /* default value */
      return;
   }
   XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
		"BadCursorString", "XvicImage", "XvicImageWidgetError",
		"Invalid format for Cursor string resource, cursor ignored",
		(String *)NULL, (Cardinal *)NULL);
   return;

#else			/* Xmu is available */

   /* Check for cursor shape name */

   shape = XmuCursorNameToIndex(name);
   if (shape != -1) {
      _XvicImageSetFontCursor(iw, shape);
      return;
   }

   /* Check for bitmap file name */

   source = XmuLocateBitmapFile(XtScreen((Widget)iw), name, 
		mask_name, (sizeof mask_name) - 4, NULL, NULL, &xhot, &yhot);
   if (source == None) {
      XtAppWarningMsg(XtWidgetToApplicationContext((Widget)iw),
		"BadCursorString", "XvicImage", "XvicImageWidgetError",
		"Invalid format for Cursor string resource, cursor ignored",
		(String *)NULL, (Cardinal *)NULL);
      return;
   }
   len = strlen (mask_name);
   for (i = 0; i < 2; i++) {
      strcpy (mask_name + len, i == 0 ? "Mask" : "msk");
      mask = XmuLocateBitmapFile (XtScreen((Widget)iw), mask_name, NULL, 0, 
				NULL, NULL, NULL, NULL);
      if (mask != None)
         break;
   }

   _XvicImageSetPixmapCursor(iw, source, mask, xhot, yhot);

   XFreePixmap(XtDisplay((Widget)iw), source);
   if (mask != None)
      XFreePixmap(XtDisplay((Widget)iw), mask);

   return;

#endif

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRawCall_zoom.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* XvicCopyRawCall_zoom.h						*/
/* This code is repeated for easy and hard zooms.  It generates calls	*/
/* to separate functions (generated by XvicCopyRawFn_zoom.h) based on	*/
/* the data type (byte vs. half/uhalf vs. other) and display mode	*/
/* color vs. pseudo vs. bw).						*/
/************************************************************************/

#ifdef DTYPE
#undef DTYPE
#endif
#ifdef MODE
#undef MODE
#endif

   if (biw->bim.data_type == XvicBYTE && biw->bim.lut16_type == XvicRAW) {

#define DTYPE DTYPE_BYTE

      if (biw->bim.image_mode == XvicCOLOR) {

#define MODE MODE_COLOR

#include "XvicCopyRaw_name.h"
         FN_NAME(3band)(ARG_LIST);

      }
      else {				/* BW, pseudo */

#undef MODE
#define MODE MODE_BW

#include "XvicCopyRaw_name.h"
         FN_NAME(1band)(ARG_LIST);

      }
   }
   else if (biw->bim.data_type == XvicHALF || biw->bim.data_type == XvicUHALF) {

#undef DTYPE
#define DTYPE DTYPE_HALF

      if (biw->bim.image_mode == XvicCOLOR) {

#undef MODE
#define MODE MODE_COLOR

#include "XvicCopyRaw_name.h"
         FN_NAME(3band)(ARG_LIST);

      }
      else if (biw->bim.lut16_type == XvicPSEUDO ||
               biw->bim.lut16_type == XvicPSEUDO_ONLY) {

#undef MODE
#define MODE MODE_PSEUDO

#include "XvicCopyRaw_name.h"
         FN_NAME(3band)(ARG_LIST);

      }
      else {

#undef MODE
#define MODE MODE_BW

#include "XvicCopyRaw_name.h"
         FN_NAME(1band)(ARG_LIST);

      }
   }
   else {						/* other data type */

#undef DTYPE
#define DTYPE DTYPE_OTHER

      if (biw->bim.image_mode == XvicCOLOR) {

#undef MODE
#define MODE MODE_COLOR

#include "XvicCopyRaw_name.h"
         FN_NAME(3band)(ARG_LIST);

      }
      else if (biw->bim.lut16_type == XvicPSEUDO ||
	       biw->bim.lut16_type == XvicPSEUDO_ONLY) {

#undef MODE
#define MODE MODE_PSEUDO

#include "XvicCopyRaw_name.h"
         FN_NAME(3band)(ARG_LIST);

      }
      else {

#undef MODE
#define MODE MODE_BW

#include "XvicCopyRaw_name.h"
         FN_NAME(1band)(ARG_LIST);

      }
   }

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRawFn_zoom.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRawFn_1band.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRawFn_3band.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRaw_bw.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* XvicCopyRaw_bw.h							*/
/* This code, called by XvicCopyRawFn_1band.h, creates the actual	*/
/* processing loops based on how the data is being displayed for bw	*/
/* data.  This is the first time the type of X display comes into play.	*/
/************************************************************************/

   switch (biw->bim.DN_transform) {

      case TRANS_FULLCOLOR:

         if (ximage->bits_per_pixel == 24) {
            /* Strange 3-byte format on some PC servers */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is RED GREEN BLUE or BLUE GREEN RED */

                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
               X_LOOP_END
            Y_LOOP_END
         }
         else if (ximage->byte_order == MSBFirst) {	/* 123 or 321 */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is NULL RED GREEN BLUE or NULL BLUE GREEN RED */

                  *dest_ptr++ = 0;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
               X_LOOP_END
            Y_LOOP_END
         }
         else {					/* LSBfirst - 210 or 012 */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is RED GREEN BLUE NULL or BLUE GREEN RED NULL */

                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = 0;
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_DIRECT:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               *dest_ptr++ = dn;
            X_LOOP_END
         Y_LOOP_END

         break;

      case TRANS_HALF:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               *dest_ptr++ = (dn>>1) | 0x80;
            X_LOOP_END
         Y_LOOP_END

         break;

      case TRANS_CMAP:

         if (biw->bim.dither_mode == XvicKAGELS) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  KAGELS_DITHER(*dest_ptr++, dn, dn, dn, x_dpy, y_dpy);
               X_LOOP_END
            Y_LOOP_END
         }
         else if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn,dn,biw->bim.gray_levels,x_dpy,y_dpy);
                  *dest_ptr++=biw->bim.cmap_gray[dn];
               X_LOOP_END
            Y_LOOP_END
         }
         else {				/* XvicNONE */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ =
			biw->bim.cmap_gray[((int)dn*biw->bim.gray_levels)/256];
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      default:			/* shouldn't happen */
         XtAppErrorMsg(XtWidgetToApplicationContext((Widget)biw),
		"BadDNTrans", "XvicBasicImage", "XvicBasicImageWidgetError",
		"Internal error: Unknown DN_transform in _XvicCopyRawXimage()",
		(String *)NULL, (Cardinal *)NULL);
   }

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRaw_color.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* XvicCopyRaw_color.h							*/
/* This code, called by XvicCopyRawFn_{1|3}band.h, creates the actual	*/
/* processing loops based on how the data is being displayed for color	*/
/* data (pseudocolor is color by this point).  This is the first time	*/
/* the type of X display comes into play.				*/
/************************************************************************/

   switch (biw->bim.DN_transform) {

      case TRANS_FULLCOLOR:

         if (ximage->bits_per_pixel == 24) {
            /* Strange 3-byte format on some PC servers */
            if (ximage->blue_mask == 0x0000ff) {		/* 123 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is RED GREEN BLUE */

                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {						/* 321 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is BLUE GREEN RED */

                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         else if (ximage->byte_order == MSBFirst) {
            if (ximage->blue_mask == 0x0000ff) {		/* 123 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is NULL RED GREEN BLUE */

                     *dest_ptr++ = 0;
                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {						/* 321 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is NULL BLUE GREEN RED */

                     *dest_ptr++ = 0;
                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         else {				/* LSBfirst */
            if (ximage->blue_mask == 0x0000ff) {		/* 210 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is BLUE GREEN RED NULL */

                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red; 
                     *dest_ptr++ = 0;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {					/* 012 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is RED GREEN BLUE NULL */

                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = 0;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         break;

      case TRANS_332:

         if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn_red, dn_red, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_grn, dn_grn, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_blu, dn_blu, 4, x_dpy, y_dpy);
                  *dest_ptr++ = (dn_red << 5) | (dn_grn<<2) | dn_blu;
               X_LOOP_END
            Y_LOOP_END
         }
         else {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ =(dn_red&0xe0) | ((dn_grn>>3)&0x1c) | (dn_blu>>6);
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_232:

         if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn_red, dn_red, 4, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_grn, dn_grn, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_blu, dn_blu, 4, x_dpy, y_dpy);
                  *dest_ptr++ = (dn_red << 5) | (dn_grn << 2) | dn_blu | 0x80;
               X_LOOP_END
            Y_LOOP_END
         }
         else {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ = (((dn_red>>1) & 0x60) | ((dn_grn>>3) & 0x1c) |
				(dn_blu>>6)) | 0x80;
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_CMAP:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               KAGELS_DITHER(*dest_ptr++, dn_red,dn_grn,dn_blu,x_dpy,y_dpy);
            X_LOOP_END
         Y_LOOP_END

         break;

      default:			/* shouldn't happen */
         XtAppErrorMsg(XtWidgetToApplicationContext((Widget)biw),
		"BadDNTrans", "XvicBasicImage", "XvicBasicImageWidgetError",
		"Internal error: Unknown DN_transform in _XvicCopyRawXimage()",
		(String *)NULL, (Cardinal *)NULL);
   }

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XvicCopyRaw_name.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* This include generates function names for the split-out functions	*/
/* (based on zoom type, data type, and color/ps/bw mode).  It is	*/
/* extraordinarily messy because the stupid preprocessor won't allow	*/
/* this kind of construct:						*/
/* #define NAME(x,y) xxx_##x##_##y					*/
/* #define TYPE qqq							*/
/* NAME(TYPE,abc)							*/
/* This should generate "xxx_qqq_abc" but it doesn't work.  Token paste	*/
/* won't substitute the values it's pasting!!  Stupid.			*/
/************************************************************************/

#ifdef FN_NAME
#undef FN_NAME
#endif

#ifdef EASYZOOM

#if DTYPE == DTYPE_BYTE

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_byte_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_byte_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_byte_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_BYTE */
#if DTYPE == DTYPE_HALF

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_half_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_half_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_half_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_HALF */

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_othr_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_othr_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_othr_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#endif /* DTYPE == DTYPE_HALF */
#endif /* DTYPE == DTYPE_BYTE */



#else /* EASYZOOM */



#if DTYPE == DTYPE_BYTE

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_byte_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_byte_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_byte_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_BYTE */
#if DTYPE == DTYPE_HALF

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_half_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_half_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_half_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_HALF */

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_othr_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_othr_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_othr_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#endif /* DTYPE == DTYPE_HALF */
#endif /* DTYPE == DTYPE_BYTE */



#endif /* EASYZOOM */

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvicimage.imake
#define SUBROUTINE xvicimage

#define MODULE_LIST XvicRegion.c XvicBasicImage.c XvicImage.c XvicCopyRaw.c \
   XvicImageOverlay.c XvicStringCursor.c

#define INCLUDE_LIST XvicCopyRawCall_zoom.h XvicCopyRawFn_zoom.h \
   XvicCopyRawFn_1band.h XvicCopyRawFn_3band.h \
   XvicCopyRaw_bw.h XvicCopyRaw_color.h XvicCopyRaw_name.h

#define USES_ANSI_C

#define GUI_SUBLIB

#define LIB_MOTIF
#define LIB_XMU

#if VAX_ARCH
#define C_OPTIONS /define=NO_XMU
#undef LIB_XMU
#endif

$ Return
$!#############################################################################
$Other_File:
$ create XvicImage.doc
##############################################################################
		SPECIFICATIONS FOR IMAGING WIDGET
##############################################################################
==============================================================================
		Inheritance Hierarchy and Functions
==============================================================================

BasicImage : XmPrimitive
	basic display, dithering
	view, image, tile size
	zoom, pan, stretch
	bw, color BYTE data
	handling of non-byte data
	maintains colormap

Image : BasicImage
	overlay support (instantiates, not inherits, ImageOverlay)
	optional scrollbars
	mouse-based panning (off an action)
	cursor planting (i.e. sw draw cursor in image) (action-based)

ImageOverlay : core
	placeholder for hardware overlay window
	completely controlled by Image
	no public interface

All of this is subject to change in the next revision, but the changes
should be minor and generally backward-compatible.

##############################################################################
		BasicImage
##############################################################################
==============================================================================
		BasicImage Resources
==============================================================================
------------------------------------------------------------------------------
		BasicImage Resources - General Info
------------------------------------------------------------------------------

There are three basic display modes: color (3-band), black & white (1-band
gray scale), and pseudocolor (1-band with a color lookup table).  It is
usually desirable to display data in these three modes in different ways
(based on the hardware capabilities), and to allow the user to specify the
preferred display options for each mode separately.  This is supported by
having four resources for each of the four display options - colormapPolicy,
stretchPolicy, ditherMode, and visualType.

These four resources represent the current state of the display, and the
saved states for each of the four display modes.  For example,
XvicNcolormapPolicy contains the current colormap policy regardless of what
display mode is being used.  XvicNcolorColormapPolicy, XvicNbwColormapPolicy,
and XvicNpseudoColormapPolicy contain the "saved" state of the colormap
policy for each of the three modes.

When the display mode is switched, the current settings are saved into the
appropriate set of resources, and another set is copied into current.  For
example, if the mode were switched from bw to color, first colormapPolicy
would be saved in bwColormapPolicy, and then colorColormapPolicy would be
copied into colormapPolicy.  Same with stretchPolicy, ditherMode, and
visualType.  This allows the user to specify in a resource file what settings
to use for each display mode separately, and yet still allow for easy changes
via the current resource set.  Of course, the user may override a setting by
setting either the current resource, or the saved resource corresponding to the
display mode, at any time.

Essentially, the saved set of resources is indistinguishable from the current
set, as long as the display mode matches.  Changes in either one affect the
other, and the current display.  If both the current and the active saved
resources are changed simultaneously (in one SetValues call), the current
resource setting will take precedence.  When the display mode changes, a
different saved set becomes current.

At widget initialization time, if values are explicitly set for the current
resources, they are used in lieu of the saved values, in order to provide
backward compatibility.  These values are then copied into the saved set.
If values are not explicitly set in the current resources, then the appropriate
saved resources are used instead.

------------------------------------------------------------------------------
		BasicImage Resources - Modes and Policies
------------------------------------------------------------------------------

XvicNimageMode		XvicCImageMode	unsigned char	XvicCOLOR	CSG
	XvicCOLOR, XvicBW
Sets the mode of the original image data.  XvicCOLOR means 3-band color data
will be supplied.  XvicBW means that 1-band color data will be supplied (even
if that data is displayed with pseudocolor).  This resource, along with
XvicNlutType and XvicNlut16Type, determine the "display mode" described in
General Info above.

XvicNdataType		XvicCDataType	unsigned char	XvicBYTE	CSG
	XvicBYTE, XvicHALF, XvicUHALF, XvicFULL, XvicUFULL, XvicREAL, XvicDOUBLE
Specifies the data type of the image data.  The valid values correspond to
C typedef's of the same name.  All data supplied via XvicImageWrite must be
of this type.  Some performance degradation will occur for any data type other
than XvicBYTE, (or even with XvicBYTE if the 16-bit LUT is used).  For
any data type other than XvicBYTE, the scaling factors XvicNrawDataMin and
XvicNrawDataMax must be used (they may be used for BYTE as well).  See the
section on "Data Type Transformation" below for details on the algorithm used
to convert non-byte data to bytes for display.  Changing the value of
XvicNdataType will reset XvicNrawDataMin and XvicNrawDataMax to their default
values, unless they are set in the same Xt(Va)SetValues call.  These data
types are defined in terms of their sizes, *not* any particular C data type,
so the Xvic* typedefs should be used.  The size of a pixel may be determined
via sizeof(Xvic*) where Xvic* is one of the C typedefs.  The C data type may
vary per platform, but all current implementations use the sample types
described below.  Complex data is not supported.  The names HALF and FULL are
used for historical reasons.

Resource value	C typedef	Description		Sample C type
--------------	---------	-----------		-------------
XvicBYTE	XvicByte	8-bit unsigned integer	unsigned char
XvicHALF	XvicHalf	16-bit signed integer	short int
XvicUHALF	XvicUHalf	16-bit unsigned integer	unsigned short int
XvicFULL	XvicFull	32-bit signed integer	int
XvicUFULL	XvicUFull	32-bit unsigned integer	unsigned int
XvicREAL	XvicReal	32-bit floating point	float
XvicDOUBLE	XvicDouble	64-bit floating point	double

XvicNditherMode		XvicCDitherMode	unsigned char	dynamic		CSG
	XvicNONE, XvicORDERED, XvicKAGELS
Specifies how to perform dithering for the current mode.  Note that dithering
is only performed on 8-bit displays; with a 24-bit visual type (colormapPolicy
of FULL_COLOR), ditherMode is ignored.  XvicNONE turns off dithering altogether.
In color mode, this forces a straight 3/3/2 or 2/3/2 RGB display (3 or 2 bits
in the lookup table are used for red, 3 for green, and 2 for blue).
XvicORDERED uses a simple 4x4 ordered dither to reduce the number of colormap
cells needed to display the image (the 3/3/2 or 2/3/2 RGB colormap is used for
color).  XvicKAGELS uses a dither algorithm developed by Dave Kagels of
JPL/DIAL.  This algorithm dithers green independently and combines red and
blue in a separate dither pattern.  The green and red-blue pixels are
arranged in a checkerboard pattern.  This mode is not generally recommended
for BW images (since there is no grayscale), but can be used for colormap
compatibility with other color images.  XvicKAGELS can only be used when
colormapPolicy is DITHER or ALLOC.  XvicNONE and XvicORDERED can be used with
any colormapPolicy in bw mode or FULL or HALF in color mode.  Pseudocolor
with stretchPolicy set to XvicUSE_HW follows the rules of BW above, while
stretchPolicy of XvicUSE_SW follows the rules of color above.  There is no
specific default value; it is derived from whichever display mode is current.

XvicNbwDither		XvicCDitherMode	unsigned char	XvicNONE	CSG
XvicNcolorDither	XvicCDitherMode	unsigned char	XvicORDERED	CSG
XvicNpseudoDither	XvicCDitherMode	unsigned char	XvicORDERED	CSG
	XvicNONE, XvicORDERED, XvicKAGELS
Saved settings for XvicNditherMode for each display mode.  See "General Info"
above for details.  These resources should be named "bwDitherMode",
"colorDitherMode", etc. but the "Mode" is absent for historical reasons.

XvicNlutType		XvicCLutType	 unsigned char	XvicSTRETCH	CSG
	XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY
Specifies how the 8-bit lookup tables (LUTs) are to be used.  XvicSTRETCH
causes the currently defined stretch tables to be used.  XvicRAW causes the
data to be displayed straight, without a lookup table (the same effect as
setting the lookup tables to straight ramps).  XvicPSEUDO causes both the b&w
stretch and the pseudocolor lookup tables to be used, and is valid only when
imageMode is set to XvicBW.  The data is first sent through the stretch, and
then through the pseudocolor table.  XvicPSEUDO_ONLY uses only the pseudocolor
table; the stretch is not used (it is treated as a ramp).  Both XvicPSEUDO and
XvicPSEUDO_ONLY set a "pseudocolor mode", which change the current settings
for the display options (see General Info above for details).  The 8-bit
pseudocolor modes may not be used if XvicNlut16Type sets a pseudocolor mode,
as the data appears to be color.  In this case, the 8-bit color LUTs are used
on the pseudocolored data, if XvicNlutType is set to XvicSTRETCH.

XvicNlut16Type		XvicCLut16Type	 unsigned char	XvicRAW		CSG
	XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY
Specifies how the 16-bit lookup tables (LUTs) are to be used.  XvicSTRETCH
causes the currently defined stretch tables to be used.  XvicRAW causes the
data to bypass the 16-bit LUTs (the same effect as setting the lookup tables
to straight ramps).  In the case of byte data, it also means to skip the entire
linear scaling operation.  XvicPSEUDO causes both the b&w stretch and the
pseudocolor lookup tables to be used, and is valid only when imageMode is set
to XvicBW.  The data is first sent through the stretch, and then through the
pseudocolor table.  XvicPSEUDO_ONLY uses only the pseudocolor table; the
stretch is not used (it is treated as a ramp).  Both XvicPSEUDO and
XvicPSEUDO_ONLY set a "pseudocolor mode", which change the current settings
for the display options (see General Info above for details).  If pseudocolor
mode is set using the 16-bit LUTs, pseudocolor may not be used for XvicNlutType
(the data already appears to be color when it gets to the 8-bit LUTs).
XvicNlutType will be reset to RAW or STRETCH in this case.  See the section
on "Data Type Transformation" below for details on the algorithm used to
convert non-byte data to bytes for display.

XvicNstretchPolicy	XvicCStretchPolicy unsigned char dynamic	CSG
	XvicUSE_HW, XvicUSE_SW
Specifies whether stretches are to be performed using the hardware colormap
or in software for the current display mode.  Hardware stretches are much
faster, but can cause colormap flashing if there are not enough hardware
colormaps available.  If a hardware stretch is not available (such as a
TrueColor-only display), then software is always used.  A stretchPolicy of
XvicUSE_HW is only valid on a 24-bit DirectColor display, or on an 8-bit
display when imageMode is BW and colormapPolicy is FULL, HALF, or ALLOC, and
the colormap is changeable.  If XvicUSE_HW is specified when only XvicUSE_SW
is valid, the resource is quietly reset to XvicUSE_SW and no error results.
A BW image with lutType or lut16Type set to PSEUDO or PSEUDO_ONLY will be
treated as a color image for purposes of display in most cases when
stretchPolicy is USE_SW (see colormapPolicy for more details).  There is no
specific default value; it is derived from whichever display mode is current.

XvicNbwStretchPolicy	XvicCStretchPolicy unsigned char	XvicHW	CSG
XvicNcolorStretchPolicy	XvicCStretchPolicy unsigned char	XvicSW	CSG
XvicNpseudoStretchPolicy XvicCStretchPolicy unsigned char	XvicSW	CSG
	XvicUSE_HW, XvicUSE_SW
Saved settings for XvicNstretchPolicy for each display mode.  See "General
Info" above for details.

XvicNcolormapPolicy	XvicCColormapPolicy unsigned char dynamic	CSG
	XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR
Specifies how the colormap is managed for an 8-bit visual type for the current
display mode.  8-bit visuals are normally used on 8-bit displays, or when
imageMode is BW.  This resource is ignored (and quietly set to XvicFULL_COLOR)
when a 24-bit visual is in use (see XvicNvisualType).  Colormap selection is
an important topic.  Since each window potentially has its own colormap, a bad
choice of colormap can create undesirable "color flashing" on the display as
the cursor is moved between windows.  To reduce this, a (mostly) standardized
set of colormaps are used.  If two instances of the widget use the same
settings for the colormap-related resources (colormapPolicy, imageMode,
ditherMode, lutType, stretchPolicy, and *Levels), then the same colormap
structure will be used as well, and there will be no flashing between the
windows (except in some cases with a colormapPolicy of XvicALLOC).  The setting
of colormapPolicy sets some constraints on the values of other resources; these
are defined below.  The setting of colormapPolicy takes precedence over
everything except visualType, so any values that conflict with it are quitely
changed so they don't conflict.  Also, colormapPolicy means slightly different
things for BW vs. COLOR images, so they are separated out below.  "Pseudocolor
mode" below means that lutType or lut16Type is set to PSEUDO or PSEUDO_ONLY.
In all cases, if stretchPolicy is USE_HW in pseudocolor mode, the data is
displayed using the BW colormap rules.  If it is USE_SW, the pseudocolor data
is displayed like color data (after going through the pseudocolor LUT), using
the COLOR rules.  There is no specific default value for colormapPolicy; it is
derived from whichever display mode is current.

FULL, BW:	Private colormap, all entries are used for a gray scale ramp.
	If stretchPolicy is USE_HW, the stretched gray LUT or the pseudocolor
	LUT are put in the colormap instead of the gray ramp.  Graphics
	overlay colors are chosen from the colormap.
	ditherMode:	NONE or ORDERED (ORDERED forces USE_SW).
	stretchPolicy:	USE_HW or USE_SW

FULL, COLOR:	Private colormap, all entries are used for a 3/3/2 RGB
	pattern, with 3 bits each of red and green ramps, and 2 bits of blue
	ramp.  Graphics overlay colors are treated like any other colors.
	ditherMode:	NONE or ORDERED
	stretchPolicy:	USE_SW

HALF, BW:	Private colormap, like FULL,BW except only the uppermost 128
	entries are used in the colormap.  The low-order bit is truncated if
	ditherMode is NONE.  If stretchPolicy is USE_HW, the stretched gray LUT
	or the pseudocolor LUT are decimated and put in the colormap instead
	of the gray ramp (only every other entry is used).  The bottom half of
	the colormap is copied from the system colormap, and graphics overlay
	colors are allocated from the system colormap as long as they are in
	the bottom half, otherwise they are allocated from the lower 128
	entries of the private colormap.
	ditherMode:	NONE or ORDERED (ORDERED forces USE_SW).
	stretchPolicy:	USE_HW or USE_SW

HALF, COLOR:	Private colormap, like FULL,COLOR except only the uppermost
	128 entries are used in the colormap, in a 2/3/2 RGB pattern.  Graphics
	overlay colors are treated like any other colors.
	ditherMode:	NONE or ORDERED
	stretchPolicy:	USE_SW

DITHER, BW:	Private colormap with the Kagels dithering pattern.  The
	uppermost greenLevels cells contain a green ramp, the next lowest
	redLevels * blueLevels cells contain a red/blue mixture, and the
	next lowest grayLevels cells contain a gray scale ramp.  Any cells
	left over at the bottom of the colormap are copied from the system
	colormap.  If ditherMode is NONE or ORDERED the gray scale ramp is
	used, while if it is KAGELS the red/blue and green ramps are used.
	KAGELS dithering should not generally be used for BW images, but can
	be if needed for colormap matching.  Graphics overlay colors are
	dithered using KAGELS.  Since only SW stretches are allowed, all
	pseudocolor is displayed like color mode.
	ditherMode:	NONE, ORDERED, or KAGELS
	stretchPolicy:	USE_SW

DITHER, COLOR:	Private colormap with the Kagels dithering pattern.  The
	uppermost greenLevels cells contain a green ramp, the next lowest
	redLevels * blueLevels cells contain a red/blue mixture, and the
	next lowest grayLevels cells contain a gray scale ramp.  Any cells
	left over at the bottom of the colormap are copied from the system
	colormap.  Graphics overlay colors are treated like any other colors.
	ditherMode:	KAGELS
	stretchPolicy:	USE_SW

ALLOC, BW:	Color cells are allocated from the system colormap.  If
	ditherMode is NONE or ORDERED, a gray scale ramp of grayLevels cells is
	allocated. If ditherMode is KAGELS, a Kagels dither pattern of
	greenLevels + (redLevels * blueLevels) cells is allocated.
	A warning is issued if not enough colors are available, and the number
	of levels requested is reduced until it fits in the colormap.  If
	stretchPolicy is USE_HW, the cells are allocated private, while for
	USE_SW they are allocated shared.  For KAGELS or ORDERED dithering,
	stretchPolicy must be USE_SW.  Graphics overlay colors are allocated
	from the system as well, unless ditherMode is KAGELS, in which case
	they are dithered.
	ditherMode:	NONE, ORDERED, KAGELS (all but NONE force USE_SW)
	stretchPolicy:	USE_HW or USE_SW

ALLOC, COLOR:	Color cells are allocated from the system colormap.  A
	Kagels dither pattern of greenLevels + (redLevels * blueLevels) cells
	is allocated.  A warning is issued if not enough colors are available,
	and the number of levels requested is reduced until it fits in the
	colormap.  Graphics overlay colors are treated like any other colors.
	This mode is not recommended; use DITHER,COLOR instead.
	ditherMode:	KAGELS
	stretchPolicy	USE_SW

FULL_COLOR:	This value should never be set by application code.  If the
	visual type in use is a 24-bit visual (see XvicNvisualType), then this
	resource is internally set to XvicFULL_COLOR.  You may retrieve
	colormapPolicy and check for this value to see if a full 24-bit
	display is in use, but you should always set colormapPolicy to one of
	the other four values, because you never know when you might be
	displaying on an 8-bit screen.
	ditherMode:	NONE
	stretchPolicy:	USE_HW or USE_SW (USE_HW only with DirectColor visual)

XvicNbwColormapPolicy	XvicCColormapPolicy	unsigned char	XvicHALF CSG
XvicNcolorColormapPolicy XvicCColormapPolicy	unsigned char	XvicHALF CSG
XvicNpseudoColormapPolicy XvicCColormapPolicy	unsigned char	XvicHALF CSG
	XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR
Saved settings for XvicNcolormapPolicy for each display mode.  See "General
Info" above for details.

XvicNvisualType		XvicCVisualType	unsigned char	XvicUSE_DEFAULT	CSG
	XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT
Suggests what kind of visual to use for the display.  If set to XvicUSE_DEFAULT,
then the widget will attempt to use a 24-bit visual for color images and
an 8-bit visual for BW and pseudocolor images.  Setting this resource to
USE_8BIT or USE_24BIT causes the widget to try to use that kind of visual,
regardless of what the imageMode is.  However, in all cases, if the requested
visual is not present, and another kind is (say USE_24BIT is specified on an
8-bit-only screen), then the available visual is used instead with no error
message.  Changing the value of this resource could change XvicNcolormapPolicy
to or away from XvicFULL_COLOR, with possible corresponding effects on other
resources.

XvicNbwVisualType	XvicCVisualType	unsigned char	XvicUSE_DEFAULT	CSG
XvicNcolorVisualType	XvicCVisualType	unsigned char	XvicUSE_DEFAULT	CSG
XvicNpseudoVisualType	XvicCVisualType	unsigned char	XvicUSE_DEFAULT	CSG
	XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT
Saved settings for XvicNvisualType for each display mode.  See "General
Info" above for details.

XvicNenableDirectColor	XvicCEnableDirectColor Boolean	True		CSG
If True, then the DirectColor visual type (24-bits with hardware colormap)
is preferred for 24-bit color.  If False, then DirectColor is not used, and
TrueColor (no hardware colormap) is used instead.  This setting is useful
on e.g. Sun UltraSPARC workstations, where there is only one colormap total,
so using DirectColor takes away the default (root) colormap, producing
undesirable and annoying colormap flashing.  In TrueColor mode, however,
hardware stretches are not available (stretchPolicy is quietly reset to
USE_SW if TrueColor is used).  Note that setting this resource does not ensure
TrueColor will be used, only that it will be used if a 24-bit display is
needed.  See also visualType, which can force the widget into 24-bit mode.

XvicNworkProcPolicy	XvicCWorkProcPolicy unsigned char XvicALL	CSG
	XvicNONE, XvicREAD, XvicALL
Specifies the policy for using a work procedure for handling expose events.
A work procedure is a function that is called whenever there are no events
to process.  It is designed for functions that take a large amount of time,
so the user interface does not hang.  Expose events may be handled inside
a work proc by the widget, which allows the interface to be active while the
image is being repainted.  If the user does something (pan, etc.), then the
action is immediately processed and the repaints are deferred until after
the action is handled.  The value XvicNONE means that the work proc is
never used, and all exposes are handled in-line.  This option should be
avoided.  The value XvicREAD means that normal exposes are handled in-line,
but exposes that require an application callback (exposeCallback) are handled
in a work proc.  The value XvicALL means that all exposes are handled
in the work proc.  XvicALL allows the best overall performance, but the
way the screen is updated during pans could be objectionable to some people.

XvicNdataSavePolicy	XvicCDataSavePolicy unsigned char XvicRAW	CSG
	XvicNONE, XvicRAW, XvicXIMAGE, XvicPIXMAP
Specifies at what level the data needed to handle expose should be saved.
The widget normally maintains as much data as possible internally (constrained
by memory limits).  The level of data chosen affects the performance of
various features.  XvicNONE means that the widget does not save any data,
so the exposeCallback is always called.  This option is not recommended.
If XvicNONE is used, then XvicImageWrite() must always be called with
full tiles; partial tiles are not allowed.  XvicRAW means that the
original data from the application is saved.  This provides the best
performance for changing zooms or software stretches, but causes pans to
take longer.  At highly subsampled zooms, much data may be saved that is
not needed, since the data is saved before the subsampling step.  XvicXIMAGE
saves the XImage structures right before the data is sent to the X server.
The data has already been zoomed, software stretched, and dithered.  This
provides better pan performance, but significantly slows the changing of
zoom or software stretch, as the raw data must be obtained from the
application again.  The memory requirements are much less for highly
subsampled zooms, however, since the data is saved after subsampling
(conversely, memory requirements are of course higher for replication zooms).
XvicPIXMAP saves the Pixmap structure on the X server.  This provides the
absolute best pan performance, especially when the X connection is over a
network.  However, many X servers do not release pixmap memory back to the
system, so the memory taken by the X server cannot be reused until the server
is re-started.  It can be used for more pixmaps, but not for anything else.
Also, some X terminals are memory-limited, and may not have enough memory for
the pixmaps.  Pixmap memory is counted in the maximumMemory limit.

XvicNconstrainPan	XvicCConstrainPan unsigned char dynamic		CSG
	XvicNONE, XvicX_ONLY, XvicY_ONLY, XvicBOTH
Specifies whether or not the image is constrained to fit in the view during
a pan operation.  Constraining the pan means the value must be between 0 and
the (zoomed) image size minus the view size.  Thus, if the view size is
smaller than the (zoomed) image size, there will never be empty space around
the image.  If the view size is larger than the (zoomed) image size, then
the pan value is forced to 0 and there is extra space at the bottom or right.
An unconstrained pan may shift the image so there is empty space (off the edge
of the image) in either direction, or the image may be panned completely off
the view.  The value XvicX_ONLY constrains the X direction only, XvicY_ONLY
constrains the Y direction only, XvicBOTH constrains both, and XvicNONE
doesn't constrain either direction.  The default value for this resource
is XvicNONE for BasicImage, and is XvicBOTH for Image.  The reason is that
scrollbars require a constrained pan to work properly.  If scrollbars are
enabled in Image, this resource be XvicBOTH, although this is not enforced
by the widget (you may get warnings from ScrollBar or unexpected results if
this is set wrong).

------------------------------------------------------------------------------
		BasicImage Resources - Sizes
------------------------------------------------------------------------------

XvicNimageHeight	XtCHeight	int		n/a		CSG
XvicNimageWidth		XtCWidth	int		n/a		CSG
Sets actual height and width of the image, independent of how it is viewed
(i.e. size in image space not in display space).

XvicNtileHeight		XtCHeight	int		dynamic		CSG
XvicNtileWidth		XtCWidth	int		dynamic		CSG
Sets height and width of the tiles the application wishes to use.  All data
from the application will be requested in multiples of this size.  These
sizes default to imageHeight and imageWidth.  These sizes are in image
coordinates, so they are not affected by the prezoom factor.  As a result of
this, the physical tile buffer may be different than this size if a prezoom
is in effect.  See the section on prezoomed data, below, for details.

XvicNviewHeight		XtCHeight	Dimension	dynamic		CSG
XvicNviewWidth		XtCWidth	Dimension	dynamic		CSG
Sets the height and width of the display in pixels.  The size is expressed
in screen pixels, not image pixels, so the size is unaffected by a zoom.
This size does not include the scrollbar (from Image) or border, if present.
Note that specifying the Core resources height or width will override these
resources.  The Core resources specify the entire width or height, including
the border and scrollbars, if present.  These resources specify the size of
the actual image display area itself.  Either set of resources may be retrieved
no matter which is used to set the size.  If neither the view nor the core
size is set at initialization time, the size is determined from the image size
and zoom factor, up to a maximum of the screen size.  Please note that the
data type of these resources is "Dimension", since it is an X dimension, rather
than "int" (like the image and tile sizes, which could be bigger than a
Dimension can hold).  The distinction is very important in a GetValues call.

------------------------------------------------------------------------------
		BasicImage Resources - Miscellaneous
------------------------------------------------------------------------------

XvicNxPan		XvicCPan	int		0		CSG
XvicNyPan		XvicCPan	int		0		CSG
Sets the pan value.  The specified pixel (in image coordinates) is the pixel
that appears at the upper-left hand corner of the view.  If the pan is
negative, then the absolute value of the pan specifies how many blank pixels
there are between the edge of the view and the beginning of the image (i.e.
a pan of -2 means that the image is shifted down/right by 2 pixels so there
are 2 pixels of background around the image).  For subsampled images, a change
in pan value may have no visible effect, since several image pixels make up
one screen pixel.  The subsample grid is never changed by setting the pan.
The requested pan value may be modified if the pan is constrained; see
XvicNconstrainPan for details.

XvicNxSubpixelPan	XvicCSubpixelPan int		0		CSG
XvicNySubpixelPan	XvicCSubpixelPan int		0		CSG
Sets the subpixel pan value.  This value is normally 0 and should only be
changed in unusual circumstances.  Setting this value has two distinct effects,
depending on the zoom.  When zooming up by an integer amount (*ZoomOut is 1),
the subpixel pan value pans by fractions of a pixel.  So, instead of the entire
leftmost pixel being displayed, only a part of it (ZoomIn - subpixel pan) is
displayed).  This allows fine control over the positioning of large, zoomed-in
pixels.  When zooming down by an integer amount (*ZoomIn is 1), the subpixel
pan value controls how the subsampling grid is laid over the image.  Normally,
the subsampling grid is fixed so that screen pixel 0 gets image pixel 0, and
pan values do not change the relationship (so pans don't have to re-subsample
the entire image).  A subsample pan changes this relationship so that screen
pixel 0 gets image pixel *SubsamplePan.  For example, if *ZoomOut is 3 and
*ZoomIn is 1, with no subsample pan, the image pixels that get displayed are
always (0,3,6,9,12,...).  With a subsample pan of 1, the image pixels that
get displayed are always (1,4,7,10,13,...).  Changing the subsample pan
forces a redisplay of the entire image, since the subsample grid has changed.
When both *ZoomIn and *ZoomOut are greater than 1 (non-integral zooms), the
effect of a subpixel pan is a complex combination of the two effects, which
is almost impossible to describe (you must determine the effect via
experimentation).  The value of the subpixel pan is not checked, but should
normally range between 0 and *ZoomIn * *ZoomOut.

XvicNxZoomIn		XvicCZoomNumer	int		1		CSG
XvicNxZoomOut		XvicCZoomDenom	int		1		CSG
XvicNyZoomIn		XvicCZoomNumer	int		1		CSG
XvicNyZoomOut		XvicCZoomDenom	int		1		CSG
Specifies the components of the zoom factor in the X and Y directions.  The
zoom factor is expressed as a rational number equal to ZoomIn / ZoomOut.
So, a ZoomIn of 1 and a ZoomOut of 3 would result in a zoom factor of 1/3,
or a zoom of -3.  Non-integral zooms are allowed; for example zoom factors of
4/3 or 631/1000 are acceptable.  A way to visualize the zoom factor is to
first take the original image, and zoom in by ZoomIn, so you get many more
pixels and a much larger image.  This zooming in is done by simple pixel
replication.  Then, take this intermediate result, and zoom out by ZoomOut to
get the final displayed pixels.  This zooming is done by simple subsampling of
the intermediate image.  No interpolation or averaging is performed.  The
subsampling grid is fixed so that the image will not be re-subsampled when
the pan changes (see *SubpixelPan for more details).  All zoom components must
always be strictly positive (zero is not allowed).  There is no hard limit on
how big the zoom components may get; however, the product of the ZoomIn value
and the image size in that dimension must fit into an integer (with some
margin).  These resources control the actual zoom factor; xZoom, yZoom, and
imageZoom are merely convenient ways of setting these resources.  The zoom
factor is always expressed relative to the original image data, regardless
of any prezoom that may be in effect.  The fraction represented by the given
zoom factors is reduced to eliminate common factors, so the values obtained via
XtGetValues may not be the same as set via XtSetValues, although the ratio will
always be the same.

XvicNxZoom		XvicCZoom	int		special		CS
XvicNyZoom		XvicCZoom	int		special		CS
XvicNimageZoom		XvicCZoom	int		special		CS
Specifies an integral zoom factor in the X and Y directions (xZoom, yZoom),
or for both directions at once (imageZoom).  All three of these resources are
merely convenient ways of setting *ZoomIn and *ZoomOut.  As such, these
resources cannot be read, only set (this is because the zoom factor may not be
integral, in which case it cannot be represented by these resources).  Read
the *ZoomIn and *ZoomOut resources directly if you need to get the zoom factor.
If set, xZoom and yZoom override the settings for *ZoomIn and *ZoomOut, while
imageZoom overrides all zoom settings.  For all three resource, positive zooms
are magnifications, which set *ZoomOut to 1 and *ZoomIn to the given value,
while negative zooms are reductions, which set *ZoomIn to 1 and *ZoomOut to the
absolute value of the given value.  Zooms of -1 are the same as zooms of 1,
while zooms of 0 are ignored (and do not override the other settings).

XvicNxPreZoomIn		XvicCZoomNumer	int		1		CSG
XvicNxPreZoomOut	XvicCZoomDenom	int		1		CSG
XvicNyPreZoomIn		XvicCZoomNumer	int		1		CSG
XvicNyPreZoomOut	XvicCZoomDenom	int		1		CSG
XvicNxPreSubpixelPan	XvicCSubpixelPan int		0		CSG
XvicNyPreSubpixelPan	XvicCSubpixelPan int		0		CSG
Specifies the parameters for providing prezoomed data.  See the section on
prezoomed data, below, for a complete description.  The application provides
data that has already been zoomed using these resources (which are interpreted
in the same way as the corresponding non-pre resources).  It is then further
zoomed (if necessary) to meet the *ZoomIn and *ZoomOut values, using an
"effective" zoom computed using the ratio between the normal zooms and prezooms,
and *SubpixelPan.  The existence of *PreSubpixelPan implies that there are
actually two subsample grids involved in handling zoomed data.  While this is
true, *PreSubpixelPan should be used even less frequently than *SubpixelPan,
which is itself used only rarely.  The *PreSubpixelPan is included mainly for
orthogonality between the two zoom operations.  If the application changes any
of these resources, then any data the widget is saving will be invalidated,
and the entire image will be re-exposed.  There are no convenience resources
like xZoom, yZoom, or imageZoom to set the prezoom factors.

XvicNmaximumMemory	XvicCMemory	int		0		CSG
This value specifies the maximum amount of memory in bytes the widget is allowed
to allocate.  Normally, the widget maintains more memory than it is currently
using, because it caches tiles (see dataSavePolicy).  This allows for much
more efficient operation, but can be wasteful of memory for large images.
maximumMemory allows the application to set a limit, balancing speed of
updates vs. memory usage.  If the widget tries to allocate memory that exceeds
this limit, it frees up cached tiles in Least-Recently-Used order (and any
other cached memory it may have) until the memory usage falls below the limit.
If the widget is already using the absolute minimum amount of memory
(approximately one zoomed XImage plus a tile if dataSavePolicy is XvicRAW or
a zoomed Pixmap if it is XvicPIXMAP), then the allocation is performed anyway
and maximumMemory is exceeded, on the theory that it's better to exceed the
memory limits than to crash.  If an allocation actually fails, the widget will
also free cached memory and try again.  If at the minimum, and the allocation
still fails, a fatal error occurs and the widget aborts (it may be possible in
the future to call back the application before this happens to allow it a
chance to free memory, but since there are so many ways X can crash when
memory is exhausted that can't be easily caught, a callback is not considered
worthwhile).  If maximumMemory is 0, then there is no memory limit.

XvicNgrayLevels		XvicCCmapCells	int		16		CSG
XvicNredLevels		XvicCCmapCells	int		16		CSG
XvicNgreenLevels	XvicCCmapCells	int		16		CSG
XvicNblueLevels		XvicCCmapCells	int		13		CSG
Specifies the number of intensity levels to use in the colormap for dither
patterns when colormapPolicy is DITHER or ALLOC.  grayLevels specifies the
number of gray-scale levels for BW dithers, while redLevels, greenLevels, and
blueLevels specify the number of intensity levels for red, green, and blue
for Kagels color dithering.  There is a limit on the number of cells in
the colormap (normally 256 for 8-bit displays).  The value (grayLevels +
greenLevels + (redLevels * blueLevels)) must be less than or equal to this
limit.  If not, *Levels are quietly reset to their default values.  The number
of cells should generally be less than the limit by several cells to allow
some system colors in the colormap (the default settings allow for 16 system
colors).  Note that when colormapPolicy is ALLOC, only grayLevels or the
triplet redLevels, greenLevels, blueLevels are used, depending on the setting
of other resources; the unused values are ignored (so they don't affect the
limit calculation).  See colormapPolicy for more details.  A case could be
made for separating these into bwGrayLevels, colorGrayLevels, pseudoGrayLevels,
etc., as is done with the other display options (see General Info above), but
it was decided not to do that in this case because it would be of very limited
utility.

XvicNrawDataMin		XvicCDataRange	double		special		CSG
XvicNrawDataMax		XvicCDataRange	double		special		CSG
Specifies the minimum and maximum data values for non-byte data (or byte
data if the 16-bit LUT is in use).  Any pixels outside this range are set to
the min or max value.  See the section on "Data Type Transformation" below for
details on the algorithm used to convert non-byte data to bytes for display.
The values of these resources are double precision regardless of the data
type of the pixels.  For integral data types, XvicNrawDataMax is the maximum
pixel value (for example, 255 for byte data).  For non-integral types, 
XvicNrawDataMax is just above the maximum pixel value.  The defaults for
these resources are the maximum range of data for integral types (e.g.
-32768..32767 for HALF, 0..4294967295 for UFULL, etc.) and 0 to 1.0 for
REAL and DOUBLE.  These values are reset to their defaults any time
XvicNdataType is changed, unless one or both are also changed in the same
Xt(Va)SetValues call.  Setting their initial values via a resource file is
allowed.  rawDataMax must be greater than rawDataMin.  NOTE:  Because these
are doubles, they must be passed to SetValues (or a VarArgs routine) via the
XvicDOUBLE_ARG macro, e.g. XtVaSetValues(w,XvicNrawDataMin,XvicDOUBLE_ARG(min),
NULL);.  The macro argument must be a simple variable for which &arg is valid,
and must be a double, not a float.  This macro compensates for different
calling mechanisms on machines with 64-bit pointers.

XvicNscaledDataMax	XvicCDataRange	int		65535		CSG
Specifies the maximum value after the first linear scaling of non-byte
data.  Data will range between 0 and this value (inclusive) after the linear
scaling and before the 16-bit LUT.  This value also determines the size of the
16-bit lookup table.  It must be less than or equal to 65535, and greater than
0.  It does not normally need to be changed, but could be useful for e.g. 12
bit data, in which case it could be set to 4095 in order to use a smaller LUT.
See the section on "Data Type Transformation" below for details on the
algorithm used to convert non-byte data to bytes for display.

XvicNoutputDataMax	XvicCDataRange	int		65535		CSG
Specifies the maximum value to be output from the 16-bit LUT.  If the 16-bit
LUT is not in use, this value must equal XvicNscaledDataMax or unpredictable
results will occur.  This maximum value is used in a second linear scaling to
convert the output of the 16-bit LUT to 8-bit data in the range 0..255.
If the LUT outputs anything higher than this value, unpredictable results
will occur.  This value must be less than or equal to 65535, and greater than
0.  It does not normally need to be changed, but could be useful for e.g. 12
bit data, in which case it could be set to 4095 in order to have 12-bit output
from the LUT.  See the setion on "Data Type Transformation" below for details
on the algorithm used to convert non-byte data to bytes for display.

------------------------------------------------------------------------------
		BasicImage Resources - Callbacks
------------------------------------------------------------------------------

XvicNvisibleAreaCallback XtCCallback	XtCallbackList	NULL		CS
This callback is called when certain resources are modified that affect what
area of the image is displayed on the screen, or how it is displayed.  The
resources affecting the area displayed are: XvicNimageHeight, XvicNimageWidth,
XvicNxSubpixelPan, XvicNySubpixelPan, XvicNxPan, XvicNyPan, XvicNxZoomIn,
XvicNxZoomOut, XvicNyZoomIn, and XvicNyZoomOut.  The resources affecting how
the image is displayed are: XvicNimageMode, XvicNdataType, XvicNlutType,
XvicNlut16Type, XvicNditherMode, XvicNstretchPolicy, XvicNcolormapPolicy,
XvicNgrayLevels, XvicNredLevels, XvicNgreenLevels, XvicNblueLevels,
XvicNrawDataMin, XvicNrawDataMax, XvicNscaledDataMax, XvicNoutputDataMax,
(this callback is triggered by current resource changes, not by inactive
saved resource changes).  Note that for pans, the callback will be called
only if the resources are directly modified.  Any pan changes covered by
Image's XvicNpanCallback will not trigger this callback (i.e. between the
two callbacks, all possible pans will be covered once and only once).
This callback is intended mainly for two purposes.  First, to notify
applications that try to track what is being displayed, such as a panner
or browser, so they can update their own display to indicate what area
of the image is being shown.  Note that this callback is not in itself
sufficient for the first use - XvicNpanCallback and XvicNresizeCallback must
also be used to fully track the displayed area.  Second, the callback allows
another image widget to use the same display modes in order to share a colormap
to prevent flashing.  The only item returned in the callback structure
(besides the reason) is a set of flags, which indicate which class(es) of
resources changed to trigger this callback.  The actual resources are not
returned.  The application should use Xt(Va)GetValues() or
XvicImageDisplayBounds() to get the information it needs.  Note that this
callback is called from within the widget's SetValues() routine, so you
should be careful about calling Xt(Va)SetValues again on this widget from
within the callback function.  Specifically, changing one of the trigger
resources could cause an infinite recursive loop.

XvicNexposeCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called when the widget needs data from the application.
The widget tries to maintain the image in memory, but if this is not possible
(due to memory constraints or the first display of an area), then it will
ask the application to provide the data.  The requested data will always be
a single tile (defined by the tile size).  The application is expected to
call XvicImageWrite() (usually once but may be several times) to provide the
requested data.  Any requested data that is not supplied via XvicImageWrite()
will cause undefined results, including possible display of uninitialized
memory.  If XvicImageWrite() is not called at all, the data may be requested
over and over again (depending on the other resource settings), causing a
CPU-burning loop as the callback is called repeatedly.  If no exposeCallback
is registered, then the application is not expected to respond to exposes and
the widget will attempt to maintain the entire image in memory.  dataSavePolicy
must be XvicRAW in this case, and the application must not change any resources
that will affect the raw data, such as image mode, image or tile size, or
prezoom factors (unless it subsequently calls XvicImageWrite() on the entire
image).  The application must call XvicImageWrite() to provide the initial
data.  This approach is not recommended; an exposeCallback should almost
always be registered.

XvicNresizeCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called whenever the view size (viewHeight, viewWidth) of the
widget changes due to user or application action.  It is called even when
the size changes due to an application request (by changing viewHeight or
viewWidth), since the request may or may not have been granted by the window
manager.  NOTE:  If this widget is actually an Image widget, with
XvicNscrollBarDisplayPolicy set to XvicAS_NEEDED, then you must be careful
what you do in the resize callback.  Due to recursion problems with the
ScrolledWindow parent, you must not change anything that would affect whether
or not the scrollbars are displayed.  Most of the time this involves the zoom
factor, but it could also involve the image size and view size (changing the
view size from within the resize callback is not a good idea in any case!).
This is most often an issue with a Zoom-to-Fit model, where the zoom factor is
adjusted to based on the view size so the entire image is always displayed.
In this case, set the scrollbar display policy to XvicNEVER.  If you find you
need to use XvicAS_NEEDED and you must change something from the Resize
callback, then register a timer proc with a time of 0 and do the SetValues call
from there.  That way, the affected values are changed outside of the Resize
code, and the scrollbars will be updated properly.

XvicNworkProcActiveCallback XtCCallback	XtCallbackList	NULL		CS
This callback is called once when the widget's WorkProc (used for repainting
the image) is activated, and again when the WorkProc is complete (i.e. no
more repaints are queued up).  The flags field of the callback structure is
set to True (non-0) when the WorkProc is active, and False (0) when it is not.
This callback could be used to post a "busy" cursor or some such, although this
is not recommended (since the widget is capable of doing other things during
repaints, it is not a good idea to block out those other things).  The main
use for this is so an application can determine when the WorkProc is active
so it can set a "priority" on its own WorkProcs.  For example, if the
application uses a WorkProc to fill in a histogram, that might be considered
less important than repainting the image.  Since Xt does not support priorities
for WorkProcs, the application could simulate them by monitoring this callback
and posting its own WorkProc only when the widget's is inactive.  Note that
this callback will not be called at all if XvicNworkProcPolicy is XvicNONE, and
it is not guaranteed that this callback will be called for any given expose
event (since the server may restore it from backing store).  All that is
guaranteed is that one and only one False (inactive) callback will be generated
for each True (active) callback, and they will be in order (i.e. it will not
generate two True's or two False's in a row).

==============================================================================
		BasicImage Structures and Functions
==============================================================================
------------------------------------------------------------------------------
		BasicImage Callback Structure
------------------------------------------------------------------------------
typedef struct
{
	int	reason;
	XEvent	*event;
	int	x, y;
	int	height, width;
	int	prezoom_x, prezoom_y;
	int	prezoom_height, prezoom_width;
	int	new_view_width;
	int	new_view_height;
	unsigned int	flags;
	String	*input_params;
	Cardinal input_num_params;
	int	x_pan;
	int	y_pan;
	Boolean	on_screen;
	double	x_fp, y_fp;
} XvicImageCallbackStruct;

Note:  This is called XvicImageCallbackStruct rather than
XvicBasicImageCallbackStruct because the Image widget uses the same
callback structure, and most clients will instantiate Image rather than
BasicImage.

int	reason;
Defines the reason for the callback.  The valid reasons, and the fields that
are relevant for each, are listed below.  Fields not relevant for a given
callback may be uninitialized.
XvicCR_EXPOSE - Set for exposeCallback.  Valid fields are:
	x, y, width, height, prezoom_x, prezoom_y, prezoom_height,
	prezoom_width.  Note that event is not valid.
XvicCR_RESIZE - Set for resizeCallback.  Valid fields are:
	new_view_width, new_view_height.  Note that event is not valid.
XvicCR_VISIBLE_AREA - Set for visibleAreaCallback.  Valid fields are:
	flags.  Note that event is not valid.
XvicCR_WORK_PROC_ACTIVE - Set for workProcActiveCallback.  Valid fields are:
	flags.  Note that event is not valid.

XEvent	*event;
The event that (possibly indirectly) caused the callback.  It is present
to comply with the Motif calling standard; it should not be needed
in any of the BasicImage callbacks and may be NULL.  It is used in Image,
however.

int	x, y, width, height;
Defines the area needing to be exposed for XvicCR_EXPOSE.  The application may
increase the size if desired (although that just means extra work for the
widget), but the application must return the data requested by calling
XvicImageWrite().  The area requested is in image coordinates, and is always
a multiple of, and aligned with, the size of a tile.  These fields are
valid only for XvicCR_EXPOSE.  The x and y fields are also used by Image.

int	prezoom_x, prezoom_y, prezoom_width, prezoom_height;
Defines the area needing to be exposed for XvicCR_EXPOSE in terms of prezoomed
pixels.  See the section on prezoomed data, below, for a complete description.
These values are calculated from x, y, width, height, using the formulas in
that section, and are provided merely as a convenience.  These fields are
valid only for XvicCR_EXPOSE.

int	new_view_width;
int	new_view_height;
Supplies the new height and width (in screen coordinates) of the view window
after a resize.  These fields are valid only for XvicCR_RESIZE.

unsigned int	flags;
Specifies which set of resources changed to trigger the visibleAreaCallback,
or the state of the workProcActiveCallback.  For visibleAreaCallback, the
flags are intended to allow the application to more easily decide how to
handle the callback, so that for example fast pan changes don't continually
update a zoom display.  The value is a logical-OR of one or more of the
following values:
	XvicPAN_CHANGED		XvicNxPan or XvicNyPan
	XvicSUBPIXEL_CHANGED	XvicNxSubpixelPan or XvicNySubpixelPan
	XvicSIZE_CHANGED	XvicNimageWidth or XvicNimageHeight
	XvicZOOM_CHANGED	XvicN(x|y)ZoomIn or XvicN(x|y)ZoomOut
	XvicMODE_CHANGED	XvicNimageMode, XvicNdataType
	XvicDITHER_CHANGED	XvicNditherMode, XvicNstretchPolicy,
				XvicNcolormapPolicy, XvicNlutType,
				XvicNlut16Type, XvicN(gray|red|green|blue)Levels
	XvicRANGE_CHANGED	XvicNrawDataMin, XvicNrawDataMax,
				XvicNscaledDataMax, XvicNoutputDataMax
For workProcActiveCallback, this field is a simple True (non-0) or False (0)
value indicating whether or not the WorkProc is active.  This field is valid
only for XvicCR_VISIBLE_AREA and XvicCR_WORK_PROC_ACTIVE.

String *input_params;
Cardinal input_num_params;
int x_pan;
int y_pan;
Boolean on_screen;
double	x_fp, y_fp;
Only used by Image (see Image docs for details).

------------------------------------------------------------------------------
		BasicImage Public Structures
------------------------------------------------------------------------------

#define XvicMEMORY_APPLIC	0
#define XvicMEMORY_WIDGET	1
#define XvicMEMORY_SHARED	2

typedef struct {
	unsigned char	*bw_pixels;
	unsigned char	*red_pixels;
	unsigned char	*grn_pixels;
	unsigned char	*blu_pixels;
	int		x;
	int		y;
	int		width;
	int		height;
	int		memory_control;
	int		line_width;
	int		start_offset;

} XvicImageData;

The XvicImageData structure is used to supply data to the widget via
XvicImageWrite().

Data Format
-----------
There is one function that uses the XvicImageData structure: XvicImageWrite().
The format of the pixel data is described below.  The format is the same for
each of the bw, red, green, and blue pixel pointers.

The simplest case for the data is when line_width == width * sizeof(pixel type)
and start_offset == 0, and no prezoom.  In this case, the data must be a packed
array of pixels, with "height" lines, each line of length "width" pixels, with
no padding bytes.  Subsequent lines must immediately follow the prior ones in
memory.

The data for XvicImageWrite() may be more complex.  In the general case, the
data is still an array of pixels, with "height" lines (this may be modified
by a Y zoom factor with prezoomed data, see below).  Each line is offset from
the previous one by "line_width" bytes (note: not pixels!), allowing lines in
memory to be larger than what the widget requires.  The pixel data for each
line starts at "start_offset" bytes (not pixels) into the line.  Note that
with prezoomed data, line_width and start_offset are in terms of bytes, while
x, y, width, and height are in terms of un-zoomed pixel (image) coordinates.

Prezoomed Data
--------------
The application program has the option of supplying prezoomed data.  This
means that the application does the zooming (or part of it) itself, and
supplies only the results to the widget.  For example, if a postage-stamp file
or a set or pyramid files existed, the application could supply this prezoomed
data rather than the entire image, resulting in a substantial time and memory
savings for highly subsampled zooms.  Or, the application may want to use
zoom algorithms other than pixel replication and subsampling, such as
interpolation or averaging.  Only the zoomed results need be supplied to the
widget.

It is important to note that prezoomed data is not a requirement.  Simple
applications can always supply the original (unzoomed) image data, and
may safely ignore this entire section.  The capability is present, however,
for more advanced applications.

The prezoom factor does not have to match the zoom factor; the widget will
do any additional zooms necessary to convert the prezoomed data to the desired
zoom factor (*ZoomIn, *ZoomOut).  For example, the application may have pyramid
files available at full size, 1/4, and 1/16 scale.  In that case, it may wish
to use the full-size image for zooms > 1/4, use the 1/4 scale for zooms between
1/4 and 1/16, and use the 1/16 scale for zooms < 1/16.

If the prezoom factor and the actual zoom factor are not simply related to
one another (i.e. if the additional zoom the widget must do is not n/1 or 1/n),
then the pixels chosen for the screen may be different than the ones chosen if
there were no prezoom.  There will be no discontinuities in the image, but the
image pixels actually displayed may be shifted a little with respect to an
image of the same zoom factor with no prezoom.  This is a necessary result of
the math involved for multiple non-integral zooms, and should not normally be
a problem.  If it is, don't use prezooms, or use prezooms that are simply
related to the actual zoom.

If the prezoom factor changes (via the application setting the *PreZoom* or
*PreSubpixelPan resources), then any data the widget is saving will be
invalidated, and the entire image will be re-exposed.

In order to provide prezoomed data, the data must correspond to a specific
format.  If it doesn't, the pixel replications and subsample grids will not
match, creating discontinuities in the image.  The formulas for creating
this data format are described below.  Note that the formulas reduce to the
simple case described in Data Format above if there is no prezoom.

All calculations in this entire section must be performed using integer math.
Also, division must be done such that the result is always rounded down.  This
is not normally the case in C if the numerator is negative (the denominators
will always be positive in these formulas), so care must be used with the
division.  A useful macro for this is:

/* Integer division that truncates downwards always.  Assumes y > 0 */
#define IDIV(x,y) ((x)>=0 ? (x)/(y) : ((x)-(y)+1)/(y))

Also note that all calculations in this section deal with pixel coordinates
only, not bytes.  If you have non-byte data, the position coordinates for a
buffer must be multiplied by the pixel size in order to get a byte offset into
the buffer.

The basic formulas for converting coordinates are:

zoomed_coord = (unzoomed_coord * zoom_in - subpixel_pan) / zoom_out
unzoomed_coord = (zoomed_coord * zoom_out + subpixel_pan) / zoom_in

Note that these two formulas are not invertible (converting an unzoomed
coordinate to a zoomed one and back again may not give you the same unzoomed
coordinate).

When prezoomed data is supplied, two independent zooms actually take place.
The first is performed by the application, using *PreZoomIn, *PreZoomOut, and
*PreSubpixelPan.  The unzoomed coordinates are the original image coordinates,
and the zoomed coordinates are relative to the prezoomed image.  The second
zoom is performed by the widget, using *SubpixelPan and the "effective" zoom
factors.  The effective zooms are computed by:

effective_zoom_in = ZoomIn * PreZoomOut
effective_zoom_out = ZoomOut * PreZoomIn

Common integer factors are then eliminated from the effective zoom values.
The effective zoom represents the difference between the actual zoom factor
(*ZoomIn, *ZoomOut), and the supplied prezoomed data (*PreZoomIn, *PreZoomOut),
and is 1:1 if the prezoom matches the actual zoom.

In the following discussion, the x (sample) coordinate is used.  The exact same
formulas apply to the y (line) coordinate as well, by simply substituting y
for x, and height for width.

The prezoom coordinates of the first sample in the buffer is calculated
using the formula:

x_start = (x * xPreZoomIn - xPreSubpixelPan + xPreZoomOut - 1) / xPreZoomOut

The prezoomed coordinates of the last sample in the buffer is calculated by:

x_end = ((x+width) * xPreZoomIn - xPreSubpixelPan - 1) / xPreZoomOut

The buffer is thus of size (x_end-x_start+1).  Note that the "x+width" factor
is actually the end coordinate + 1 (normally, an end coordinate is computed by
start + width - 1).  These formulas differ from the basic zoom formulas above
in order to compensate for roundoff, which is needed to make sure that the
start and end of adjacent tiles are completely covered by prezoomed pixels.
They basically use the left-hand edge of the pixel for the start coordinate,
and the right-hand edge of the pixel for the end coordinate.

These values are provided for you in the callback structure as prezoom_x and
prezoom_width, so you can derive x_start and x_end by:

x_start = prezoom_x
x_end = prezoom_x + prezoom_width - 1

The x_start and x_end coordinates may be used directly to read a prezoomed
file of the appropriate zoom factor, or the data may be obtained from the
unzoomed image on the fly.  The widget itself zooms by using the same formulas
to compute the zoomed coordinates (from the prezoomed coordinates, the
effective zoom factors, and xSubpixelPan).  It then loops between the zoomed
x_start and x_end, using the coordinate conversion formulas above (actually,
incremental forms of them) to convert from zoomed coordinates to prezoomed
coordinates, taking the pixel at the given prezoomed coordinates, and putting
it in the buffer.  The application could do the same thing, looping between
the prezoomed x_start and x_end, and converting from prezoomed to image
coordinates, and taking the pixel at the given image coordinates.

Different zoom algorithms may be used (such as averaging or interpolation)
based on the given formulas, but the x_start and x_end coordinates must be
calculated *exactly* as shown here.  The widget's subsampling algorithm
(taking the upper-left pixel of the area covered by the larger pixel)
guarantees that only image data within the bounds of (x, y, x+width-1,
y+height-1) will be used to generate the prezoomed data (x_start, y_start,
x_end, y_end).  However, other zoom algorithms may need data outside the image
bounding box.  If this is the case, do not modify x, y, width, or height.
They must relate to x_start, y_start, x_end, and y_end using the given
formulas, and x_start et al defines the region of data that needs to be sent
to the widget.  If you need to read extra image data, go ahead, but this fact
is not reflected in the fields of this structure.

Implied in the above discussion is the fact that, with a prezoom in effect,
the actual size of the data in a tile supplied to the widget is potentially
different from the tileWidth and tileHeight specified in the resources.
tileWidth and tileHeight are defined in image (unzoomed) coordinates, while
the buffer has a prezoom.  So, the actual amount of data in the buffer is
computed using the x_start, x_end formulas above.  These must be recomputed
for every tile, as the actual buffer size depends on the coordinates of the
tile.  For example, at a prezoom of 1/3, every third tile will be one pixel
longer than the rest (e.g. if the tile size is 100, the first tile will be
be 34 pixels and the next two will be 33).  If you wish to allocate a buffer
once for a tile, the following formula may be used to calculate the maximum
size of any tile.  In the preceding example, 34 would be returned, although
most of the tiles are actually only 33 pixels wide.  You could allocate a
buffer of width 34 and use it for all tiles (being careful of the actual
size for each tile, of course).

x_max_tile_width = ((tileWidth + 1) * xPreZoomIn - 1) / xPreZoomOut

Note that the subpixel pan has no effect on the maximum size.

Data Type Transformation
------------------------
Non-byte data must be transformed to 8-bit data for display, since all current
display technologies supported by X are 8 bits (per color channel).  Each pixel
goes through this transformation independently, although the widget optimizes
this as much as possible via look-up tables.  Byte data may also go through
the same transformation, but does not normally do so for efficiency.

Summary

In short, the raw pixel is first linearly scaled to a 16-bit unsigned value.
This value goes through a 16-bit lookup table, producing another 16-bit
unsigned value.  This value is linearly scaled to an 8-bit unsigned value,
which is sent through the normal 8-bit lookup table and then dithered or
otherwise manipulated for display (as with any byte data).  Each of these
steps is controllable.

Transformation Detail

The first step is a linear scale to a 16-bit unsigned value.  The resources
XvicNrawDataMin and XvicNrawDataMax specify the minimum and maximum values
that the raw data will have (any data outside these ranges are set to the
min or max value).  The min value will be translated to 0, and the max
value will be translated to XvicNscaledDataMax (which is normally 65535, and
must be less than or equal to that value).

The next step is to pass the data through a 16-bit lookup table if
XvicNlut16Type is set to anything other than XvicNONE.  Stretch,
pseudocolor, or both LUT's may be applied, as with 8-bit data (pseudocolor
only applies to XvicBW data).  However, if pseudocolor is applied via the
16-bit LUT, then it may not be applied via the 8-bit LUT; from the 8-bit LUT's
point of view, the data is already color.  The 16-bit LUT must be filled
up to XvicNscaledDataMax (normally 65535).  The output values must range
between 0 and XvicNoutputDataMax (normally 65535).  If not, unexpected results
may occur.  If the 16-bit LUT is not enabled, then XvicNscaledDataMax must
equal XvicNoutputDataMax, or unexpected results may occur.

The output of the 16-bit LUT is then in the range 0 to XvicNoutputDataMax
(normally 65535).  This data is linearly scaled to the range 0 to 255
to generate byte data for display.  This byte data is then treated like any
other byte data, going through the 8-bit LUTs if XvicNlutType specifies,
and getting zoomed, dithered, and otherwise prepared for display.

Note that zooming and dithering happen only on the 8-bit data.

Usage

Applications at a minimum need to set XvicNrawDataMin and XvicNrawDataMax
in order to handle non-byte data.  Often, that's all that is needed, or
perhaps using a 16-bit LUT as well.  The XvicNscaledDataMax and
XvicNoutputDataMax will rarely be needed.  Where they might come in handy,
however, is if you were for example dealing with 12-bit data.  In that
case, you could set both of these to 4095 and treat the 16-bit LUT as a
true 12-bit one, setting only 4096 values and outputting 12-bit data as
well.

Note that byte data does not go through any of this transformation unless
XvicNlut16Type is set to something other than XvicNONE.  In other words,
if a 16-bit LUT is in use, the byte data is scaled up to 16-bits, goes through
the LUT, and is scaled back down.  If a 16-bit LUT is not in use, then no
scaling is done (even if XvicNrawDataMin/Max are set).  This behavior is
slightly different than other data types and applications should be aware
of it.

Formulas

Linear scaling is done via a binning process.  The output range is treated
as a series of equal-sized "bins" which are mapped to the input range.
The minimum input value specifies the bottom of a bin, while the maximum
input value specifies the top of a bin.  Anything outside of the min or max
data value is saturated to the min or max output bin.  The range for integral
data is inclusive, meaning that the max value can actually appear in the
input data.  The range for non-integral data is exclusive, meaning that the
values go just up to the max, but shouldn't actually reach tha max.  This is
not really an issue since data equalling the max will be saturated to the
max bin anyway, but mathematically the max is outside the range for non-
integral data.

The formula for scaling non-integral data to the 16-bit range is:

16bit = floor((value - rawDataMin) * ((scaledDataMax+1) /
					(rawDataMax - rawDataMin)))

All calculations are performed using doubles.

The formula for scaling integral data to the 16-bit range is:

16bit = floor((value - rawDataMin) * ((scaledDataMax+1) /
					(rawDataMax - rawDataMin + 1)))

The only difference is the +1 in the rawDataMax-Min calculation (which
compensates for the inclusive instead of exclusive range).  All calculations
are performed using doubles.

In both cases the floor() function is in the standard math library and
rounds the value towards negative infinity.

The formula for scaling 16-bit data to the 8-bit range is:

8bit = 16bit * 256 / (outputDataMax+1)

The floor() function is not needed since all values are positive, and integer
math should be used.

Field Descriptions
------------------
unsigned char	*bw_pixels;
unsigned char	*red_pixels;
unsigned char	*grn_pixels;
unsigned char	*blu_pixels;
These elements are pointers to buffers used to hold the data.  If imageMode
is XvicCOLOR, then red_pixels, grn_pixel, and blu_pixels are used.  If imageMode
is XvicBW, then bw_pixels is used.  The unused pointers are ignored and may
be uninitialized.  Even though these are declared as unsigned char pointers,
the actual data must be of the type specified by XvicNdataType.

int x, y, width, height;
These fields define the location of the data being supplied, in image
coordinates.  The widget will operate much more efficiently if these match
the size and position of the image tiles.  However, this is not a requirement;
the data being sent may be anything from a single pixel to the entire image.

int memory_control;
Defines who maintains control of the memory pointed at by the pixel pointers.
The widget maintains a copy of the data when possible in order to handle
expose events without bothering the application.  Normally, the data is
copied into a private memory area (XvicMEMORY_APPLIC).  However, in some
circumstances, the extra copy may be avoided for efficiency.  This field only
applies if dataSavePolicy is XvicRAW; for any other setting, memory_control
is assumed to be XvicMEMORY_APPLIC (take note of this in your application, so
you don't leak memory!).  The allowed values are:

XvicMEMORY_APPLIC:  The application retains control of the memory.  The widget
makes a copy of the data into its own allocated buffers.  Once the
XvicImageWrite() call is complete, the application may free or re-use
the memory as it wishes.  This is the appropriate value to use if a single
buffer is used over and over again for each XvicImageWrite() call.  This is
the only valid setting if dataSavePolicy is not XvicRAW.

XvicMEMORY_WIDGET:  The widget gets control of the memory.  The widget can
use the provided buffer directly, without copying it.  The widget will free
the memory using free() when it is done using it.  If the application calls
malloc() to get the buffer for each XvicImageWrite() call, it should set
memory_control to XvicMEMORY_WIDGET for efficiency.  The application must
not touch the memory after the XvicImageWrite() call, as it could be
deallocated at any time (including during the call itself).  This memory
counts against the maximumMemory limit, and may cause the widget to free
something else.  This setting is valid only if dataSavePolicy is XvicRAW.
Note that only a single full tile should be provided to XvicImageWrite()
in this mode.  Any partial tiles, or full tiles past the first one, will be
copied into widget-allocated memory (and the provided buffer freed), which
defeats the whole purpose of XvicMEMORY_WIDGET.  Worse yet, if you provide
more than one tile, only one tile will use your memory area (the others use
their own memory), but the entire area is still allocated, which can
significantly raise the memory usage.

XvicMEMORY_SHARED:  The widget and application share the memory.  After the
XvicImageWrite() call, the widget may not modify the memory.  The
application should not modify it either, although it can if the modification
is immediately followed by another XvicImageWrite() call, with new_data set
to True.  The widget will never free the memory; the application may not do
so until the widget is destroyed or until another buffer is supplied for the
exact same area of the image via another call to XvicImageWrite().  This
mode is mainly intended for displaying a read-only Array I/O file (i.e. a file
that is mapped into memory, using mmap() on Unix or the VMS equivalent).  This
memory does not count against the maximumMemory limit.  This setting is valid
only if dataSavePolicy is XvicRAW.  Note that only full tiles should be
provided to XvicImageWrite() in this mode (although you can provide
more than one tile in a single call with no memory waste).  Any partial tiles
will be copied into widget memory (thus not sharing the buffer), which defeats
the whole purpose of XvicMEMORY_SHARED.

int line_width;
Defines the length, in *bytes* (not pixels), of each image line.  The address
of the next line is determined by adding line_width to the address of the
previous one.

int start_offset;
Defines the offset, in *bytes* (not pixels), from the start of each image line
to the start of the valid data.  This allows the data to start in the middle
of a line.  This field, and line_width, are most useful when you have a large
buffer containing the entire image (such as a memory-mapped file), and you wish
to provide a small chunk of the image for the display.

------------------------------------------------------------------------------
		BasicImage Public Functions
------------------------------------------------------------------------------
Widget XvicCreateBasicImage(parent, name, args, argCount)
	Widget		parent;
	char		*name;
	ArgList		args;
	Cardinal	argCount;
Creates a basic image widget using the standard Motif creation style.  This
form is rarely used; XtVaCreateManagedWidget is usually used instead.

void XvicImageWrite(widget, image, new_data)
	Widget		widget;
	XvicImageData	*image;
	Boolean		new_data;
Sends image data to the widget.  This function is normally called in response
to an exposeCallback to supply the requested data to the widget.  The image
data structure is described under Public Structures.  The new_data flag
should be True if this data is new or has been changed by the application,
and False if the data is unchanged.  This flag is used to determine how much
is to be displayed.  The widget always requests whole tiles of data (defined
by tileWidth and tileHeight), even if only a portion of the tile needs
updating.  If new_data is False, the widget will display only the portions
needed for the expose event.  If new_data is True, the widget will re-display
all the data supplied, even if not called for by an expose event (this would
be used when displaying a different image, for example).  The response to
the initial expose events (which display the data for the first time) may
have new_data set to False.  This is because the initial expose will cover
the entire display window, so there is no need to force any repaints.
Most applications will set new_data to False permanently.  Any unsolicited
calls to XvicImageWrite (calls not triggered by an exposeCallback) should set
new_data to True for them to have much effect, since unsolicited calls are
only needed to supply new data.  Note that the data supplied to XvicImageWrite
does not have to be the size of a tile (unless dataSavePolicy is XvicNONE),
but the widget will operate more efficiently if it is.  Sending data that is
not a multiple of the tile size may have implications for memory usage; see
the memory_control field of XvicImageData for details.

void XvicImageSetColorLUT(widget, red_lut, green_lut, blue_lut)
	Widget		widget;
	int		*red_lut;
	int		*green_lut;
	int		*blue_lut;
Sets the color or psuedocolor lookup tables for the widget.  The lut arguments
are arrays of 256 integers, where the values are all in the range 0 to 255.
If any of the lut arguments is NULL, the corresponding lookup table is
not modified.  Note that when lutType is XvicPSEUDO, both the pseudocolor
LUTs and the mono LUT are used.  The lookup tables default to straight
linear ramps.

void XvicImageGetColorLUT(widget, red_lut, green_lut, blue_lut)
	Widget		widget;
	int		*red_lut;
	int		*green_lut;
	int		*blue_lut;
Returns the color or pseudocolor lookup table for the widget.  The lut
arguments are pointers to arrays of 256 integers.  The arrays must be
allocated by the caller; this routine does not allocate storage.
The returned values are all in the range of 0 to 255.  If any of the
lut arguments is NULL, the corresponding lookup table is not returned.

void XvicImageSetMonoLUT(widget, lut)
	Widget		widget;
	int		*lut;
Sets the monochrome (single-band) lookup table for the widget.  The lut
argument is an array of 256 integers, where the values are all in the
range 0 to 255.  If the lut argument is NULL, the function is essentially
a no-op.  Note that when lutType is XvicPSEUDO, both the mono LUT and
the pseudocolor LUTs are used.  The lookup table defaults to a straight
linear ramp.  The mono LUT is ignored if imageMode is COLOR.

void XvicImageGetMonoLUT(widget, lut)
	Widget		widget;
	int		*lut;
Returns the monochrome (single-band) lookup table for the widget.  The lut
argument is a pointer to an array of 256 integers.  The array must be
allocated by the caller; this routine does not allocate storage.  The
returned values are all in the range of 0 to 255.  If the lut argument
is NULL, the function is essentially a no-op.  The mono LUT is ignored
if imageMode is COLOR.

void XvicImageSetColorLUT16(widget, red_lut, green_lut, blue_lut, lut_size)
	Widget		widget;
	int		*red_lut;
	int		*green_lut;
	int		*blue_lut;
	int		lut_size;
Sets the color or psuedocolor 16-bit lookup tables for the widget.  The lut
arguments are arrays of lut_size integers, where the values are all in the
range 0 to XvicNoutputDataMax.  lut_size should normally equal
XvicNscaledDataMax.  If any of the lut arguments is NULL, the corresponding
lookup table is not modified.  Note that when lut16Type is XvicPSEUDO, both the
pseudocolor LUTs and the mono LUT are used.  The lookup tables default to
straight linear ramps.

void XvicImageGetColorLUT16(widget, red_lut, green_lut, blue_lut, lut_size)
	Widget		widget;
	int		*red_lut;
	int		*green_lut;
	int		*blue_lut;
	int		lut_size
Returns the color or pseudocolor 16-bit lookup table for the widget.  The lut
arguments are pointers to arrays of lut_size integers.  The arrays must be
allocated by the caller; this routine does not allocate storage.
The returned values should all be in the range of 0 to XvicNoutputDataMax.
lut_size should normally equal XvicNscaledDataMax.  If any of the lut
arguments is NULL, the corresponding lookup table is not returned.

void XvicImageSetMonoLUT16(widget, lut, lut_size)
	Widget		widget;
	int		*lut;
	int		lut_size;
Sets the monochrome (single-band) 16-bit lookup table for the widget.  The lut
argument is an array of lut_size integers, where the values are all in the
range 0 to XvicNoutputDataMax.  lut_size should normally equal
XvicNscaledDataMax.  If the lut argument is NULL, the function is essentially
a no-op.  Note that when lut16Type is XvicPSEUDO, both the mono LUT and
the pseudocolor LUTs are used.  The lookup table defaults to a straight
linear ramp.  The mono LUT is ignored if imageMode is COLOR.

void XvicImageGetMonoLUT16(widget, lut, lut_size)
	Widget		widget;
	int		*lut;
	int		lut_size;
Returns the monochrome (single-band) 16-bit lookup table for the widget.  The
lut argument is a pointer to an array of lut_size integers.  The array must be
allocated by the caller; this routine does not allocate storage.  The
returned values should all be in the range of 0 to XvicNoutputDataMax.
lut_size should normally equal XvicNscaledDataMax.  If the lut argument
is NULL, the function is essentially a no-op.  The mono LUT is ignored
if imageMode is COLOR.

void XvicImageClear(widget)
	Widget		widget;
Invalidates all saved data and clears the window, causing expose events
to occur for the entire displayed area.  This function is normally used
to change the image being displayed.  One option is to call XvicImageWrite
on the new data with new_data set to True, but this is not always practical,
and the application has to figure out what is being displayed so it doesn't
send too much data.  Usually, it is easier to tell the widget to invalidate
all its data and allow the normal Expose callback to repaint the screen.
This function accomplishes that task.

void XvicImageDisplayBounds(widget, x1, y1, x2, y2)
	Widget		widget;
	int		*x1;
	int		*y1;
	int		*x2;
	int		*y2;
Returns the Image coordinates of the edges of the displayed window.  The
coordinates may be off the edge of the image if the window is bigger than
the image.  This is just a convenience routine; the application could get
XvicNviewWidth and XvicNviewHeight and figure it out for itself.  Note that
at some zooms, the returned coordinates may be slightly larger than the window,
but they will never be smaller.

==============================================================================
		BasicImage Actions
==============================================================================

None.

##############################################################################
		Image
##############################################################################
==============================================================================
		Image Resources
==============================================================================
------------------------------------------------------------------------------
		Image Resources - Modes and Policies
------------------------------------------------------------------------------

XvicNcursor		XvicCCursor	String		"crosshair"	CS(G)
Sets the shape of the cursor, a la XmuCvtStringToCursor.  The value for this
may be queried via GetValues.  However, if the cursor shape is explicitly set
using one of the XvicImageSet...Cursor() routines, the queried value will be
NULL (there is no way to query the cursor shape if those routines are used).
The three valid forms for the cursor name are:
1) Standard cursor name (from cursorfont.h), e.g. "arrow"
2) Font name and glyph index of the form
   "FONT fontname index [[fontname] index]", where the first pair is the
   cursor shape and the second is the mask.  For example, "FONT cursor 0"
   or "FONT cursor 0 1" or "FONT cursor 0 cursor 1" all use the font named
   "cursor".
3) Bitmap file name.  This may be an absolute name, or relative to the global
   resource bitmapFilePath, class BitmapFilePath.  If the resource is not
   defined, the default value is the Xmu build symbol BITMAPDIR.  The mask,
   if present, has the same name but with "Mask" or "msk" appended.
If the widget is compiled without Xmu support (i.e. NO_XMU is defined on the
compile line for the widget), then only form #2 is allowed (or the default
value of "crosshair").  This resource must not be set to NULL.

XvicNcursorBackground	XvicCCursorBackground
					String		"white"		CSG
XvicNcursorForeground	XvicCCursorForeground
					String		"black"		CSG
Defines the foreground and background colors of the cursor.  These resources
must be set, rather than using XRecolorCursor() directly, so the cursor
planting routines can use the right color.  The colors are Strings rather
than Pixels because potentially several different colormaps and visual types
may be involved.  Any standard X color string specifier may be used.

XvicNcursorMode		XvicCCursorMode	unsigned char	XvicFLOATING	CSG
	XvicFLOATING, XvicPLANTED
Defines how the image cursor behaves.  XvicFLOATING means that the image cursor
floats free and moves with the mouse pointer.  This is the default case.
XvicPLANTED means that the image cursor is "planted" into a spot on the image.
The image cursor is separate from the mouse cursor, and stays planted until
explicitly moved (which can happen via an action or other method).  The
shape of the mouse cursor is inherited from the parent window in this case.

XvicNcursorX		XvicCCursorLoc	int		dynamic		CSG
XvicNcursorY		XvicCCursorLoc	int		dynamic		CSG
These two resources set or return the image cursor location in image
coordinates.  These resources are most useful when cursorMode is XvicPLANTED,
in which case they are the only way to move the image cursor (some of the
actions move the cursor via these resources).  When the cursorMode is
XvicFLOATING, the position may be read from these resources, but it could
change immediately.  An application that needs the floating cursor
position is much better off using events or actions.  If the position is
set while cursorMode is XvicFLOATING, then the actual mouse pointer is warped
to the given position, which is generally considered very bad style in Motif.
However, this is useful when switching from planted to floating cursor.
The default values depend on the location of the cursor.  Note that for
XvicFLOATING, the returned coordinates may be off the edge of the image if
the cursor is not over the widget's window (i.e. they may be negative or
greater than the image size).  Also, if the cursor is not on the same screen
as the widget, the upper-left corner of the displayed image is returned.

XvicNcursorXfp		XvicCCursorLocFp double		dynamic		CSG
XvicNcursorYfp		XvicCCursorLocFp double		dynamic		CSG
These two resources set or return the image cursor location in floating-point
image coordinates.  This allows for the cursor to be set to fractional-pixel
locations (which is only of any use with high zoom factors).  Otherwise, they
are identical to cursorX and cursorY above.  The integral values are defined
to be the center of a pixel, so the range of locations for pixel 10 is actually
9.5 to 10.4999..., not 10.0 to 10.9999....  If both cursorX/Y and cursorX/Yfp
are set, the fp versions take precedence.  NOTE:  Because these are doubles,
they must be passed to SetValues (or a VarArgs routine) via the XvicDOUBLE_ARG
macro, e.g. XtVaSetValues(w,XvicNcursorXfp,XvicDOUBLE_ARG(xpos),NULL);.  The
macro argument must be a simple variable for which &arg is valid, and must be
a double, not a float.  This macro compensates for different calling
mechanisms on machines with 64-bit pointers.

XvicNenableHWOverlay	XvicCEnableHWOverlay Boolean	True		CG
Enables or disables the use of the hardware overlay plane.  If no hardware
overlay is available, this resource is ignored (implicitly set to False,
although it does not read back that way).  Note that this resource cannot be
read to determine whether a hardware overlay is actually in use; a value of
True only means that it will be used if the hardware supports it.  The overlay
plane should normally be enabled but can be disabled if the widget has trouble
using it (since there is very little standardization in how overlays work,
this is quite possible on hardware that the widget has not been tested on).
This resource can only be set at widget creation time; changes to it after
the widget has been created will be quietly ignored.

XvicNscrollBarDisplayPolicy	XvicCScrollBarDisplayPolicy
					unsigned char	XvicAS_NEEDED	CSG
	XvicSTATIC, XvicAS_NEEDED, XvicNEVER
Specifies when scrollbars should be added to the image.  XvicSTATIC means that
scrollbars are always present.  XvicAS_NEEDED means that scrollbars are present
only if needed, i.e. if the view size is less than the (zoomed) image size
in either direction.  This may cause the scrollbars to be displayed in one
direction but not the other.  XvicNEVER means that scrollbars are never
displayed.  Scrollbars, if displayed, are normally on the bottom and left of
the image (but this can be modified via the XmNscrollBarPlacement resource
of the parent XmScrolledWindow widget).  Note that the use of scrollbars
(XvicSTATIC or XvicAS_NEEDED) will set the XvicNconstrainPan resource of
BasicImage to XvicBOTH.  If you want unconstrained pans, you must disable the
scrollbars by setting this resource to XvicNEVER.  If the parent widget of
Image is not XmScrolledWindow (in APPLICATION_DEFINED mode), scrollbars will
never be used and this resource is set to XvicNEVER.  The convenience creation
routines will create the XmScrolledWindow for you.  If this resource is set
to XvicAS_NEEDED, the view/core sizes of the image area may not be what you
expect around the boundary conditions (they may shrink or expand to make room
for or take up space left by the scrollbars as they appear and disappear).
Also, using XvicAS_NEEDED has some implications for what the application can
do from within the Resize callback.  See XvicNresizeCallback for details.

XvicNtrackFloatingCursor	XvicCTrackFloatingCursor
					Boolean		False		CSG
Specifies whether the cursorCallback is called on pointer motion when
XvicNcursorMode is XvicFLOATING.  This is generally set to True if a
cursorCallback is registered and False otherwise.  Setting this to False
allows the widget to remove the mouse motion event handler that tracks the
floating cursor.  This should improve performance when the cursor location
is not needed, since motion events will not be selected.

------------------------------------------------------------------------------
		Image Resources - Callbacks
------------------------------------------------------------------------------

XvicNcursorCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called when the image cursor moves.  The image cursor
is the mouse pointer when cursorMode is XvicFLOATING, and is independent
(and moved via actions and/or XvicNcursorX/Y or XvicNcursorX/Yfp) when
cursorMode is XvicPLANTED.  This callback allows the application to track the
image cursor regardless of the mode it is in.  When cursorMode is XvicPLANTED,
the callback is called any time the cursor is moved (whether via actions or
setting the XvicNcursorX/Y(fp) resources).  However, when cursorMode
isXvicFLOATING, whether the callback is called depends on the setting of
XvicNtrackFloatingCursor for performance reasons.  If this resource is True,
the callback is called for any cursor motion.  If it is False, the callback
is not called.  Generally, when a cursorCallback is registered, the application
should also set XvicNtrackFloatingCursor to True.  If the callback is removed,
set it to False.

XvicNinputCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called from the Input() action routine.  It allows
application-specific behavior on keypresses or mouse events, without having
to register an event handler.  The event causing the action, the action
argument, and the event location in image coordinates (for certain types of
events)  are available in the callback structure.  The x, y, x_fp, y_fp, and
on_screen fields of the callback structure are set to the image coordinates of
the event for the following event types:  ButtonPress, ButtonRelease,
EnterNotify, LeaveNotify, KeyPress, KeyRelease, MotionNotify.  For other event
types, x, y, x_fp, y_fp, and on_screen are all 0.  Note that if translations
are specified with "*translations" instead of ".translations", then the
translations will be set on the hardware overlay widget as well (if present).
If this happens, the event will be for the window associated with *that*
widget, not the XvicImage widget.  So, be careful using the event x and y
coordinates.  The callback structure x and y (and x_fp, y_fp) coordinates
don't have this problem; they are corrected for this effect.

XvicNpanCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called whenever the widget changes the pan value.  It is
not called if the pan changes due to the application setting xPan or yPan
(but XvicNvisibleAreaCallback will be, so every possible pan operation will
cause one and only one of these two callbacks to be triggered).  This is
normally called due to the scrollbars being moved, or due to one of the
various pan action routines (usually mouse or keyboard-controlled panning).

XtNunrealizeCallback	XtCCallback	XtCallbackList	NULL		CS
This callback is called when the widget is unrealized.  It is for internal
implementation use only, and should not be used by application code.

==============================================================================
		Image Structures and Functions
==============================================================================
------------------------------------------------------------------------------
		Image Callback Structure
------------------------------------------------------------------------------

The same structure, XvicImageCallbackStruct, is used for both the BasicImage
and the Image callbacks.  See BasicImage for the full structure definition.
Only the additions for Image are listed below.

int	reason;
Defines the reason for the callback.  The valid reasons added by Image,
and the fields that are relevant for each, are listed below.

XvicCR_CURSOR - Set for cursorCallback.  Valid fields are:
	x, y, on_screen, x_fp, y_fp.
XvicCR_INPUT - Set for inputCallback.  Valid fields are:
	event, input_params, input_num_params, x, y, on_screen, x_fp, y_fp.
XvicCR_PAN - Set for panCallback.  Valid fields are:
	x_pan, y_pan.  Note that event is not valid.

XEvent	*event;
The event that (possibly indirectly) caused the callback.  It is present
in most callbacks, but only really used for XvicCR_INPUT.  The application can
find out what event triggered the input by examining the event, or
alternatively it can use input_params.  For the Input callback, note that if
translations are specified with "*translations" instead of ".translations",
then the translations will be set on the hardware overlay widget as well (if
present).  If this happens, the event will be for the window associated with
*that* widget, not the XvicImage widget.  So, be careful using the event x
and y coordinates.  The callback structure x and y (and x_fp, y_fp) coordinates
don't have this problem; they are corrected for this effect.

int x, y;
The location, in image coordinates, of the image cursor (for XvicCR_CURSOR)
or the event (for XvicCR_INPUT).  These fields are valid only for
XvicCR_CURSOR and XvicCR_INPUT.  The cursor location may also be retrieved
via Xt(Va)GetValues, but if the cursor is free-floating the location may
have changed since the event was generated (or pans might have changed the
mapping between screen and image coordinates).  These fields should generally
be used instead of GetValues because of how the event queue works.  The
cursor or event location is reported whether or not it is actually visible
in the image (see on_screen).  For XvicCR_INPUT, the coordinates are
set for ButtonPress, ButtonRelease, EnterNotify, LeaveNotify, KeyPress,
KeyRelease, and MotionNotify events.  For other event types, these fields
are 0.

String *input_params;
Cardinal input_num_params;
A string array, and number of elements, representing the argument(s) passed to
the action routine Input() from the translation table.  It is intended to
allow the application to easily determine what action to take based on user
input, without having to examine the event directly (thus violating the
translation-action model).  These fields are exactly the same as the
corresponding fields in the action routine.  The application may of course
register its own action routines, but this facility should be easier to use.
This field is valid only for XvicCR_INPUT.  The strings may be deallocated
upon return from the callback, so if they need to be saved for later use, the
application must copy them, not just save pointers to them.

int x_pan, y_pan;
The new pan value, in image coordinates.  These values could be obtained
via GetValues on the widget, but are provided in the callback structure as
a convenience.  This field is valid only for XvicCR_PAN.

Boolean on_screen;
Indicates whether the cursor (for XvicCR_CURSOR) or event location (for
XvicCR_INPUT) is in the visible area of the image or not.  This field is
valid only for XvicCR_CURSOR and XvicCR_INPUT.  If this value is True,
the location is within the current view area.  If it is False, the
location is either off the window, or possibly in the window border.  Note
that this value may be False even if the coordinates indicate that it
should be on-screen, because a LeaveNotify event may report the last
cursor/event position (which was probably on screen), but this flag will be
set to False because the position is no longer on the screen.  This flag may
be used to blank out coordinate displays when the cursor is off the image,
or to implement automatic panning if something is dragged off the visible area.
Note that on_screen is valid only at the time the callback is issued.  For
example, if a subsequent pan causes the cursor to go off the screen without
actually moving it, a cursorCallback will not be generated even though
on_screen might have changed.  For XvicCR_INPUT, this field is set for
ButtonPress, ButtonRelease, EnterNotify, LeaveNotify, KeyPress, KeyRelease,
and MotionNotify events.  For other event types, this field is set to False.

double x_fp, y_fp;
Identical to x, y above in all respects, except that the coordinates are in
floating-point image coordinates rather than integers.  This allows for the
cursor or event location to be read to fractional-pixel accuracy (which is
only of any use with high zoom factors).  The integral values are defined to
be the center of a pixel, so the range of locations for pixel 10 is actually
9.5 to 10.4999..., not 10.0 to 10.9999....

------------------------------------------------------------------------------
		Image Public Structures
------------------------------------------------------------------------------

typedef struct {
	double	x;
	double	y;
	double	width;
	double	height;
	int	angle1;
	int	angle2;
} XvicArc;

XvicArc is used in XvicImageDrawArcs() and XvicImageFillArcs() to specify
arrays of arcs.  This structure is exactly like XArc, except that the
coordinates in XvicArc are doubles instead of shorts (since Image coordinates
can be larger than a short, and fractional pixels are supported).  See
"Important note on height/width", under "Overlay Drawing Routines", below.

typedef struct {
	double	x;
	double	y;
} XvicPoint;

XvicPoint is used in XvicImageDrawLines(), XvicImageDrawPoints(),
XvicImageFillPolygon() to specify arrays of coordinates.  This structure
is exactly like XPoint, except that the coordinates in XvicPoint are doubles
instead of shorts (since Image coordinates can be larger than a short, and
fractional pixels are supported).

typedef struct {
	double	x;
	double	y;
	double	width;
	double	height;
} XvicRectangle;

XvicRectangle is used in XvicImageDrawRectangles() and XvicImageFillRectangles()
to specify arrays of rectangles.  This structure is exactly like XRectangle,
except that the coordinates in XvicRectangle are doubles instead of shorts
(since Image coordinates can be larger than a short, and fractional pixels are
supported).  See "Important note on height/width", under "Overlay Drawing
Routines", below.

typedef struct {
	double	x1;
	double	y1;
	double	x2;
	double	y2;
} XvicSegment;

XvicSegment is used in XvicImageDrawSegments() to specify arrays of line
segments.  This structure is exactly like XSegment, except that the coordinates
in XvicSegment are doubles instead of shorts (since Image coordinates can be
larger than a short, and fractional pixels are supported).

------------------------------------------------------------------------------
		Image Public Functions
------------------------------------------------------------------------------

XvicCreateImage(parent, name, args, argCount)
	Widget		parent;
	char		*name;
	ArgList		args;
	Cardinal	argCount;
Creates an image widget using the standard Motif creation style.  This
routine also creates a ScrolledWindow parent widget, so scrollbars can
work properly.  For this reason, this routine should normally be used
instead of Xt(Va)Create(Managed)Widget.  Resources for the ScrolledWindow
can be set in args as well as Image resources.  Note that the Image widget is
NOT managed by this routine (to be consistent with Motif); you must manage
the widget yourself (the automatically created ScrolledWindow widget is
managed automatically).  If you want to use the VarArgs interface for creation,
you can do what this routine does yourself.  Create a ScrolledWindow with
XmNscrollingPolicy = XmAPPLICATION_DEFINED, XmNvisualPolicy = XmVARIABLE, and
XmNscrollBarDisplayPolicy = XmSTATIC.  Then, use XtVaCreate(Managed)Widget to
create the Image widget as a child of the ScrolledWindow.

Overlay Color Routines
----------------------
These routines must be called to get a color to use before any graphics
drawing can be done.  The colors are not set directly in the GC in order to
cut down on the number of GC's needed.  You must use these routines instead
of allocating colors manually so that the colors will stay "correct" (as close
as possible) when the visual type or dither type changes.  Graphics colors are
dithered if needed in order to get the closest possible color to the request.

XvicColor XvicImageGetGrColor(iw, xcolor)
	Widget		iw;
	XColor		*xcolor;
Returns a color index to use for graphics or cursor calls.  The color is
specified by the red, green, and blue components of the xcolor structure
(which are unsigned short ints, meaning the values range from 0 to 0xFFFF, not
0 to 255).  Other elements of xcolor are ignored.  The xcolor struct may
be created directly, or it may be returned by a function such as XParseColor()
if you wish to specify the color using string names.  Note that the String
to Pixel resource converter is not sufficient for returning XvicColor indices,
so you must get the value as a string, pass it through XParseColor() (or
equivalent), and then call this routine, if you want to specify graphics
colors in a resource file (or you could write your own converter).  The
returned value can be used for any overlay call requiring a color.  There
is no guarantee you will receive the color requested; however, the widget
will do the best it can.  Colors are preserved (and may get better) when
display modes are changed.

XvicColor XvicImageGetGrColorRGB(iw, red, green, blue)
	Widget		iw;
	int		red;
	int		green;
	int		blue;
Same as XvicImageGetGrColor(), except the color components are specified as
separate arguments in the range 0 to 255.

Overlay GC Routines
-------------------
These routines allow you to allocate, change, and free graphics contexts
for use with the graphics overlay routines.  You must use these routines
instead of getting GC's directly because the widget manages the GC's and will
replace them if the visual type or depth changes (so it is transparent to
the user).  You may pass NULL for the XvicGC argument to the overlay routines
to get a default GC.

The settable items in the GC are:
   arc_mode, cap_style, dashes, dash_offset, fill_rule, font, function,
   join_style, line_style, line_width, plane_mask
Note that setting "function" or "plane_mask" may produce unpredictable results
depending on the type of overlay visual used.  Also, line_width should not be
set to 0; such so-called "fast" lines will not be dithered correctly.  Use a
line_width of at least 1.  Finally, for the text-related routines, the font
must be set; there is no way to default it (largely because there appears to
be no way to get the font ID of the default font back from the X server!).
If you don't set the font, no text will be displayed.

The following are reserved for use by the widget (in order to render the
graphics correctly); any settings for these will be ignored or will have no
effect:
   background, clip_mask, clip_(x|y)_origin, fill_style, foreground,
   graphics_exposures, tile, stipple, subwindow_mode, ts_(x|y)_origin

void XvicImageChangeGC(iw, gc, valuemask, values)
	Widget		iw;
	XvicGC		gc;
	unsigned long	valuemask;
	XGCValues	*values;
Modifies the (already created) GC.  The given GC must be an XvicGC id returned
by XvicImageCreateGC(); it cannot be a standard X GC.  See the description
above for details on what fields are valid.  Any existing graphics that
use this GC will be redisplayed with the new GC.

XvicGC XvicImageCreateGC(iw, valuemask, values)
	Widget		iw;
	unsigned long	valuemask;
	XGCValues	*values;
Creates a GC for use with the graphics routines and returns a handle to it.
The returned handle should be used with all graphics routines.  The GC can
be modified with XvicImageChangeGC() or XvicImageSetDashes(), and can be
freed with XvicImageFreeGC().  See the description above for details on what
fields are valid.

XvicGC XvicImageCreateRubberGC(iw, valuemask, values)
	Widget		iw;
	unsigned long	valuemask;
	XGCValues	*values;
Creates a GC like XvicImageCreateGC, except that the GC will be set up for
rubber-banding operations.  Graphics drawn using the rubber-band mode can be
erased much faster than normal graphics, because the image does not have to
be redrawn.  However, you have no control over the color of the rubber band.
Erasing the rubber band should be done via XvicImageEraseObject(), *not* by
drawing the object again in the same place.

void XvicImageFreeGC(iw, gc)
	Widget		iw;
	XvicGC		gc;
Frees the specified XvicGC.  If it has not been allocated, this call is
quietly ignored.  Any graphics objects using a GC must be erased before
freeing the GC.

void XvicImageSetDashes(iw, gc, dash_offset, dash_list, n)
	Widget		iw;
	XvicGC		gc;
	int		dash_offset;
	char		dash_list[];
	int		n;
Just like XSetDashes(), this routine will set the dashes for an XvicGC.
This routine is needed because the full dash functionality cannot be set
via a XGCValues struct.  The dash_list is copied, so it may be freed when
this call returns.  Setting n to 0 will disable setting the dash list, which
will cause whatever settings may be in the XGCValues struct to take effect.

Overlay Drawing Routines
------------------------
The graphics drawing routines closely mirror the X drawing primitives.
However, once drawn, the widget keeps track of all objects and redraws
them as appropriate without application intervention.  Floating-point Image
coordinates are used, so that the graphics are attached to certain locations
in the image to subpixel accuracy.  If the image is panned or zoomed, the
graphics will be moved or resized to match.  Fractional coordinates only
really matter for zooms > 1.  The integral values are defined to be the centers
of the image pixels, so the range of locations for pixel 10 is actually 9.5 to
10.4999..., not 10.0 to 10.9999....

Important note on height/width:  Several of the drawing routines (e.g.
arc and rectangle) take a floating-point height and width either in arguments
or in the structure passed in.  These fields behave oddly.  The X drawing
routines define height and width to be the number of pixels that are covered
by an object.  So if an object covers coordinates 3 though 6, the width is 4
(the same applies to height of course).  The formula is width = end - start + 1.
This works and makes sense for integer coordinates.  However, it breaks down
for floating-point coordinates.  Since integral coordinates are defined to be
the centers of the pixels, the true width of the 3.0 through 6.0 rectangle is
really 3.0, not 4.  So the natural formula is width = end - start.  However,
this usage is inconsistent with the X drawing routines which the widget is
emulating.  Since most drawing will be done with integral coordinates, the
width in these routines is defined in the integer sense to match X, i.e.
end - start + 1.  This means that when dealing with floating-point coordinates,
you have to add an artificial 1.0 to the width you would expect, and the
minimum width is 1.0 (values less than 1.0 are quietly reset to 1.0).  So an
object starting at 3.4 with a specified width of 2.3 would end at 4.7, not
5.7 as you would expect.

Each object in the overlay is assigned an ID.  This ID can be used to move
or erase the object at a later time.  More than one primitive can share the
same ID, in which case they are treated as a single object.  For example,
two lines in a "+" shape, together with a text number, might be a single
object representing a tiepoint location.  This object can be moved or
erased as a unit, but it cannot be split back into its component parts.

Each drawing routine accepts an ID as an argument, and returns an ID as the
function return.  If the argument is specified as 0, the item being drawn
becomes a new object, and its ID is returned.  It the argument is non-0, the
item being drawn is added to the specified object (which must already exist),
and that ID is returned.  The returned ID can be discarded if you never want
to manipulate the object; the widget will take care of redisplays.  In
case of error (which normally is only an invalid GC, Color, or ID), the
returned object ID is 0.

Each drawing routine also requires XvicGC and XvicColor arguments in order
to specify how to draw the object, and what color to use.  These must be
created using widget functions; see "Overlay GC Routines" and "Overlay Color
Routines" above.  Standard X GC's and colors cannot be used with the widget.

If the X server has a hardware overlay, it will be used for all graphics
functions (including the planted cursor).  If not, the overlay will be
emulated in software, which means that parts of the image itself must be
redrawn whenever a graphics object is moved or erased.  For this reason,
interactive "rubber-banding" effects should be done using a special Rubber GC
(using XvicImageCreateRubberGC()) for efficiency.  Rubber-banding may be
performed without using a rubber GC but the animation will not be as smooth.

Graphics objects are always stacked with the oldest on the bottom.  So, newer
objects will sit on top of older ones.  There is no way to change this
stacking order.  Note that objects sharing a common ID may be at different
places in the stacking order, allowing a simultaneous "over-under" effect if
the objects are moved by other objects in between the components in the
stacking order.

All graphics are clipped to the size of the image.  However, coordinates are
maintained, so that if the image is expanded, or the object moved into the
image area, it will still look correct.

The widget does not provide any capability for matching a coordinate to
the "closest" graphic object (in other words, picking an object off the
screen).  This is the responsibility of the application.

XvicID XvicImageDrawArc(iw, id, gc, color, x, y, width, height, angle1, angle2)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	double		width, height;
	int		angle1, angle2;
Draws an arc inside the box specified by (x,y,width,height).  Similar to
XDrawArc().  The angle1 and angle2 parameters are as specified in XDrawArc().
See "Important note on height/width", under "Overlay Drawing Routines", above.

XvicID XvicImageDrawArcs(iw, id, gc, color, arcs, narcs)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicArc		*arcs;
	int		narcs;
Draws multiple arcs.  Similar to XDrawArcs().  Note that a sequence of
XvicImageDrawArc() calls may be more efficient in some cases than
XvicImageDrawArcs(), because in order to erase or move a graphics primitive,
the widget uses the bounding box of the entire primitive.  Separate Arc's may
have a smaller overall bounding box than one set of Rectangles.  They will
not join correctly, though, if you are depending on line joins where the
arcs meet.  On the other hand, it takes more server commands to draw the arcs
separately.  Generally, if the arcs are together in a small area, use
Arcs.  If they are spread out over the image, use separate Arc's.  See
"Important note on height/width", under "Overlay Drawing Routines", above.


XvicID XvicImageDrawBitmap(iw, id, gc, color, x, y, bitmap, width, height,
			   hot_x, hot_y)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	Pixmap		bitmap;
	unsigned int	width, height;
	int		hot_x, hot_y;
Draws a bitmap.  The given Pixmap must be of depth 1 and should use the same
root as the widget.  The hot_x and hot_y parameters are offsets into the bitmap
(in Screen coordinates) which define the "hotspot".  The bitmap will be drawn
with this hotspot at the given (x,y) coordinates (in Image coordinates).  For
example, if hot_x and hot_y are 0, then the upper left corner of the bitmap
will be at (x,y).  If hot_x and hot_y are (width/2, height/2), the center of
the bitmap will be drawn at (x,y).  The hotspot should be used rather than
offsetting (x,y) so that the correct point is still at the given coordinates
regardless of the zoom factor.  If you wish to have a border ("mask") around
the shape, like for a cursor, you should first draw the mask bitmap in the
background color, then draw the shape bitmap in the foreground color using
the same ID.  The same technique can be used to build up multicolored icons,
by drawing each color as a separate bitmap and overlaying them using the same
ID.  The given Pixmap is not copied by the widget, so you must not free it
until after erasing all graphic objects that use it.  Similarly, erasing
the object does not free the Pixmap; it is the application's responsibility.
Some simple animation may be achieved by changing the pixmap, then moving the
object with a delta of (0,0).  This will cause the object to be redrawn using
the new shape.  The size and hotspot cannot be changed, however.  Note that
the height and width are in Screen coordinates, and the size of the drawn
bitmap does not change with zoom factors (only the hotspot location is
"attached" to the image during zooms).

XvicID XvicImageDrawImageString(iw, id, gc, fg, bg, x, y, string, length,
				justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	fg;
	XvicColor	bg;
	double		x, y;
	char		*string;
	int		length;
	int		justify;
Draws a string, filling the bounding box with the specified background first.
Similar to XDrawImageString().  The string ends up with an opaque background.
Unlike XDrawImageString(), tiling will be used if necessary to dither the
colors.  The justify parameter specifies where in the text the (x,y) location
is.  XvicJUST_LEFT specifies the left side of the baseline, which is how
XDrawImageString works.  XvicJUST_RIGHT specifies the right side of the
baseline, while XvicJUST_CENTER specifies the center of the baseline.  Note
that text is not resized when the zoom changes.

XvicID XvicImageDrawImageString16(iw, id, gc, fg, bg, x, y, string, length,
				  justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	fg;
	XvicColor	bg;
	double		x, y;
	XChar2b		*string;
	int		length;
	int		justify;
Draws a string using 16-bit characters, filling the bounding box with the
specified background first.  Similar to XDrawImageString16().  The string ends
up with an opaque background.  Unlike XDrawImageString16(), tiling will be
used if necessary to dither the colors.  The justify parameter specifies where
in the text the (x,y) location is.  XvicJUST_LEFT specifies the left side of
the baseline, which is how XDrawImageString16 works.  XvicJUST_RIGHT specifies
the right side of the baseline, while XvicJUST_CENTER specifies the center of
the baseline.  Note that text is not resized when the zoom changes.

XvicID XvicImageDrawLine(iw, id, gc, color, x1, y1, x2, y2)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x1, y1;
	double		x2, y2;
Draws a line from (x1,y1) to (x2,y2).  Similar to XDrawLine().

XvicID XvicImageDrawLines(iw, id, gc, color, points, npoints, mode)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicPoint	*points;
	int		npoints;
	int		mode;
Draws a series of lines joined end-to-end, joining them as specified in
the XvicGC.  Similar to XDrawLines().  If the last point matches the first,
the first and last lines will join correctly.  If mode is CoordModeOrigin,
all points are in absolute Image coordinates.  If mode is CoordModePrevious,
all points after the first are relative to the previous point (they are still
in Image coordinates but specify an offset instead of an absolute location).
The first point is always in absolute Image coordinates.

XvicID XvicImageDrawPoint(iw, id, gc, color, x, y)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
Draws a point at (x,y).  Similar to XDrawPoint().  Note, however, that
XDrawPoint() is not actually used since it does not pay attention to
the tiling patterns used for dither.  Instead, XFillRectangle() is used
with a width and height of 1.  Also, single points may or may not be visible if
dithering is in effect, depending on how they hit the dither pattern.  They
most likely won't be the right color in any case.

XvicID XvicImageDrawPoints(iw, id, gc, color, points, npoints, mode)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicPoint	*points;
	int		npoints;
	int		mode;
Draws a series of points.  Similar to XDrawPoints().  If mode is
CoordModeOrigin, all points are in absolute Image coordinates.  If mode is
CoordModePrevious, all points after the first are relative to the previous
point (they are still in Image coordinates but specify an offset instead of
an absolute location).  The first point is always in absolute Image coordinates.
Note that XDrawPoints() is not actually used since it does not pay attention to
the tiling patterns used for dither.  Instead, XFillRectangles() is used
with a width and height of 1.  Also, single points may or may not be visible if
dithering is in effect, depending on how they hit the dither pattern.  They
most likely won't be the right color in any case.

XvicID XvicImageDrawRectangle(iw, id, gc, color, x, y, width, height)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	double		width, height;
Draws a rectangle with upper-left corner at (x,y) and the specified width
and height.  Similar to XDrawRectangle().  NOTE:  Unlike XDrawRectangle(),
the specified width and height are used exactly.  XDrawRectangle() actually
uses a width and height one greater than what is specified.
XvicImageDrawRectangle, on the other hand, uses the supplied width and
height without adding one, so it draws a rectangle the same size as
XvicImageFillRectangle() does.  This was done because the coordinates are
not really pixel coordinates, they are Image coordinates (which are not
constant pixels, if the zoom changes), so adding one screen pixel to the
size did not make much sense.  Note also that when a rectangle is moved
or erased, the entire bounding box is redrawn.  For software overlay, this
means re-exposing the entire image area covered by the rectangle.  For this
reason, it may be more efficient for large rectangles to use four separate
XvicImageDrawLine() calls (attaching them all to the same ID) so that only
the lines themselves are redrawn, not the area in the center.
(XvicImageDrawLines() can't be used becuase it has the same bounding-box
problem).  See "Important note on height/width", under "Overlay Drawing
Routines", above.  Both the height/width adjustments above, and in the
Important Note, apply.  Think in terms of ending pixel coordinate - starting
pixel coordinate + 1.0.

XvicID XvicImageDrawRectangles(iw, id, gc, color, rectangles, nrectangles)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicRectangle	*rectangle;
	int		nrectangles;
Draws multiple rectangles.  Similar to XDrawRectangles().  Note that a
sequence of XvicImageDrawRectangle() calls may be more efficient in some
cases than XvicImageDrawRectangles(), because in order to erase or move a
graphics primitive, the widget uses the bounding box of the entire primitive.
Separate Rectangle's may have a smaller overall bounding box than one set of
Rectangles.  On the other hand, it takes more server commands to draw the rects
separately.  Generally, if the rectangles are together in a small area, use
Rectangles.  If they are spread out over the image, use separate Rectangle's.
XvicImageDrawRectangles() uses the same width and height conventions as
XvicImageDrawRectangle() (which is different from XDrawRectangles()).  Also
see "Important note on height/width", under "Overlay Drawing Routines", above.

XvicID XvicImageDrawSegments(iw, id, gc, color, segments, nsegments)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicSegment	*segments;
	int		nsegments;
Draws multiple disjoint line segments.  Similar to XDrawSegments().  Note
that a sequence of XvicImageDrawLine() calls may be more efficient in some
cases than XvicImageDrawSegments(), because in order to erase or move a
graphics primitive, the widget uses the bounding box of the entire primitive.
Separate Line's may have a smaller overall bounding box than one set of
Segments.  On the other hand, it takes more server commands to draw the lines
separately.  Generally, if the lines are together in a small area, use
Segments.  If they are spread out over the image, use separate Line's.

XvicID XvicImageDrawString(iw, id, gc, color, x, y, string, length, justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	char		*string;
	int		length;
	int		justify;
Draws a string, but does not fill the bounding box.  Similar to XDrawString().
The string ends up with a transparent background.  The justify parameter
specifies where in the text the (x,y) location is.  XvicJUST_LEFT specifies
the left side of the baseline, which is how XDrawString works.  XvicJUST_RIGHT
specifies the right side of the baseline, while XvicJUST_CENTER specifies the
center of the baseline.  Note that text is not resized when the zoom
changes.

XvicID XvicImageDrawString16(iw, id, gc, color, x, y, string, length, justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	XChar2b		*string;
	int		length;
	int		justify;
Draws a string using 16-bit characters, but does not fill the bounding box.
Similar to XDrawString16().  The string ends up with a transparent background.
The justify parameter specifies where in the text the (x,y) location is.
XvicJUST_LEFT specifies the left side of the baseline, which is how
XDrawString16 works.  XvicJUST_RIGHT specifies the right side of the
baseline, while XvicJUST_CENTER specifies the center of the baseline.  Note
that text is not resized when the zoom changes.

XvicID XvicImageDrawText(iw, id, gc, color, x, y, items, nitems, justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	XTextItem	*items;
	int		nitems;
	int		justify;
Draws multiple strings on the same horizontal line, possibly changing fonts
between each.  Similar to XDrawText().  The strings are specified exactly
as in XDrawText().  The justify parameter specifies where in the text line
the (x,y) location is.  XvicJUST_LEFT specifies the left side of the baseline,
which is how XDrawText works.  XvicJUST_RIGHT specifies the right side of the
baseline, while XvicJUST_CENTER specifies the center of the baseline.  Note
that text is not resized when the zoom changes.

XvicID XvicImageDrawText16(iw, id, gc, color, x, y, items, nitems, justify)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	XTextItem16	*items;
	int		nitems;
	int		justify;
Draws multiple 16-bit strings on the same horizontal line, possibly changing
fonts between each.  Similar to XDrawText16().  The strings are specified
exactly as in XDrawText16().  The justify parameter specifies where in the text
line the (x,y) location is.  XvicJUST_LEFT specifies the left side of the
baseline, which is how XDrawText works.  XvicJUST_RIGHT specifies the right
side of the baseline, while XvicJUST_CENTER specifies the center of the
baseline.  Note that text is not resized when the zoom changes.

XvicID XvicImageFillArc(iw, id, gc, color, x, y, width, height, angle1, angle2)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	double		width, height;
	int		angle1, angle2;
Draws a filled arc inside the box specified by (x,y,width,height).  Similar to
XFillArc().  The angle1 and angle2 parameters are as specified in XFillArc().
See "Important note on height/width", under "Overlay Drawing Routines", above.

XvicID XvicImageFillArcs(iw, id, gc, color, arcs, narcs)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicArc		*arcs;
	int		narcs;
Draws multiple filled arcs.  Similar to XFillArcs().  Note that a sequence of
XvicImageFillArc() calls may be more efficient in some cases than
XvicImageFillArcs(), because in order to erase or move a graphics primitive,
the widget uses the bounding box of the entire primitive.  Separate Arc's may
have a smaller overall bounding box than one set of Rectangles.  On the
other hand, it takes more server commands to draw the arcs separately.
Generally, if the arcs are together in a small area, use Arcs.  If they
are spread out over the image, use separate Arc's.  See "Important note on
height/width", under "Overlay Drawing Routines", above.

XvicID XvicImageFillPolygon(iw, id, gc, color, points, npoints, shape, mode)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicPoint	*points;
	int		npoints;
	int		shape;
	int		mode;
Draws a filled polygon.  Similar to XFillPolygon().  If the first and last
point don't coincide, the path is closed automatically.  If mode is
CoordModeOrigin, all points are in absolute Image coordinates.  If mode
is CoordModePrevious, all points after the first are relative to the previous
point (they are still in Image coordinates but specify an offset instead of
an absolute location).  The first point is always in absolute Image coordinates.
The shape argument is one of Complex, Nonconvex, or Convex.  See XFillPolygon() 
for the meaning of these values.  If in doubt, use Complex.

XvicID XvicImageFillRectangle(iw, id, gc, color, x, y, width, height)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	double		x, y;
	double		width, height;
Draws a filled rectangle with upper-left corner at (x,y) and the specified
width and height.  Similar to XFillRectangle().  See "Important note on
height/width", under "Overlay Drawing Routines", above.

XvicID XvicImageFillRectangles(iw, id, gc, color, rectangles, nrectangles)
	Widget		iw;
	XvicID		id;
	XvicGC		gc;
	XvicColor	color;
	XvicRectangle	*rectangle;
	int		nrectangles;
Draws multiple filled rectangles.  Similar to XFillRectangles().  Note that a
sequence of XvicImageFillRectangle() calls may be more efficient in some
cases than XvicImageFillRectangles(), because in order to erase or move a
graphics primitive, the widget uses the bounding box of the entire primitive.
Separate Rectangle's may have a smaller overall bounding box than one set of
Rectangles.  On the other hand, it takes more server commands to fill the rects
separately.  Generally, if the rectangles are together in a small area, use
Rectangles.  If they are spread out over the image, use separate Rectangle's.
See "Important note on height/width", under "Overlay Drawing Routines", above.

Overlay Object Manipulation Routines
------------------------------------
These functions operate on graphics objects specified by an ID, which is
returned by the drawing primitives.  All objects sharing the same ID will
be affected by these routines (there is no way to split an object into its
component parts).

void XvicImageEraseObject(iw, id)
	Widget		iw;
	XvicID		id;
Erases all graphics objects using the given id.  Rubber objects are simply
undrawn (using an XOR), while normal objects are erased and everything else
is redrawn under them (which may be somewhat expensive).  Note that if you
draw a non-rubber object on top of a rubber object, then erase the rubber
object, the results will be undefined until the next time the area is
redisplayed.

void XvicImageEraseOverlay(iw)
	Widget		iw;
Erases all overlay objects in the widget.  Does not erase XvicColor's or
XvicGC's.

void XvicImageMoveObject(iw, id, delta_x, delta_y)
	Widget		iw;
	XvicID		id;
	double		delta_x;
	double		delta_y;
Moves all graphics objects using the given id.  Rubber objects are undrawn
and redrawn, while normal objects are erased (causing everything else to be
redrawn under them) and then redrawn, which may be somewhat expensive.  Note
that if you move a non-rubber object on top of a rubber object, then move the
rubber object, the results will be undefined until the next time the area is
redisplayed.

Cursor Shape Routines
---------------------
The cursor setting routines are needed because of a deficiency in the
X protocol where the shape of a given cursor cannot be determined.
The shape is needed in order for the cursor to be planted, which means
it is drawn directly onto the image in order to mark a place while the real
cursor is elsewhere.  So, these routines must be used instead of the X
equivalents to set the cursor shape.

XvicImageSetFontCursor(widget, shape)
	Widget		widget;
	unsigned int	shape;
Sets the cursor to the specified shape from the standard cursor font.
This routine must be used instead of XCreateFontCursor() so the cursor
planting routines can get the right shape.  See XCreateFontCursor() for
an explanation of the shape parameter.  Setting the cursor via this function
will cause any queries of XvicNcursor to return NULL.

XvicImageSetGlyphCursor(widget, source_font, mask_font, source_char, mask_char)
	Widget		widget;
	Font		source_font, mask_font;
	unsigned int	source_char, mask_char;
Similar to XvicImageSetFontCursor, but the source and mask bitmaps are obtained
from separate font characters, possibly in separate fonts.  This routine
must be used instead of XCreateGlyphCursor() so the cursor planting routines
can get the right shape.  See XCreateGlyphCursor() for an explanation of the
parameters.  Setting the cursor via this function will cause any queries of
XvicNcursor to return NULL.  Currently, multibyte fonts are not supported.

XvicImageSetPixmapCursor(widget, source, mask, x, y)
	Widget		widget;
	Pixmap		source, mask;
	unsigned int	x, y;
Sets the cursor to the shape specified by the given pixmap and mask.  This
routine must be used instead of XCreatePixmapCursor() so the cursor planting
routines can get the right shape.  See XCreatePixmapCursor() for an explanation
of the parameters.  Setting the cursor via this function will cause any queries
of XvicNcursor to return NULL.

==============================================================================
		Image Actions
==============================================================================
------------------------------------------------------------------------------
		Image Actions - Sample Translations
------------------------------------------------------------------------------

	<Btn1Down>:		MousePanStart() \n\
	<Btn1Motion>:		MousePan() \n\
	Shift<Key>Left:		PanOne(left) \n\
	Shift<Key>Right:	PanOne(right) \n\
	Shift<Key>Up:		PanOne(up) \n\
	Shift<Key>Down:		PanOne(down) \n\
	Ctrl<Key>Left:		PanHalfView(left) \n\
	Ctrl<Key>Right:		PanHalfView(right) \n\
	Ctrl<Key>Up:		PanHalfView(up)	\n\
	Ctrl<Key>Down:		PanHalfView(down) \n\
	Shift Ctrl<Key>Left:	PanEdge(left) \n\
	Shift Ctrl<Key>Right:	PanEdge(right) \n\
	Shift Ctrl<Key>Up:	PanEdge(up)	\n\
	Shift Ctrl<Key>Down:	PanEdge(down) \n\
	<Key>Esc:		CursorMode(toggle) \n\
	<Btn3Down>:		Input(mark_point) \n\

These are only samples; applications may choose different events.
There are some interactions with Motif to be aware of when using keyboard
events.  Apparently, keyboard traversal must be on in order to get the
keypresses, but that means that the user must traverse (tab) to the imaging
widget in order to use the keys.  Also, the traversal mechanism seems to
swallow unmodified arrows, but this may be resolved in the future.

------------------------------------------------------------------------------
		Image Action Routines
------------------------------------------------------------------------------

CursorMode(mode,warp)
Sets the cursor planting mode, corresponding to the cursorMode resource.
The "mode" argument is one of the strings "plant", "float", or "toggle".
The default if mode is not given is "toggle".  The "warp" argument is either
"true" or "false", and applies only when going into floating mode.  True
causes the mouse pointer to be warped (explicitly moved) to the last location
of the planted cursor (assuming the cursor was planted before).  This can
be jarring to a user so should be used with discretion.  False (the default)
leaves the pointer alone.

Input(arg...)
Calls the inputCallback routine.  The "arg..." argument is 0 or more strings
that are passed to the callback routine.  This action routine is intended to
allow application-specific behavior on keypresses or mouse events, without
having to register an event handler or their own action routine.  The event
causing the action and callback is available in the callback structure.

MousePanStart()
Sets the starting point for a mouse pan operation.  This action may be
triggered by any event with an (x,y) position, which includes ButtonPress,
ButtonRelease, EnterNotify, LeaveNotify, KeyPress, KeyRelease, and MotionNotify.

MousePan()
Implements mouse-based panning.  The mouse appears to the user to be stuck to
the image, and the image pans as the mouse moves.  Therefore, pushing the mouse
up actually pans down, exposing areas below what was previously displayed.
This action may be triggered by any event with an (x,y) position (see
MousePanStart), and must be preceded by a MousePanStart() action.  There is
no termination action.  The event is typically a button motion event, but
there is nothing in the implementation requiring a button to be held down,
as long as a MousePanStart() action can be generated first.

MoveCursor(direction)
Moves the planted cursor one Image pixel in the indicated direction.  The
"direction" argument is one of the strings "left", "right", "up", or "down".
If the cursor is free floating, this action has no effect.  This is usually
used to move the cursor based on arrow keys.  Note that at zooms < 1, the
cursor might not actually move if the Image coordinate is changed.  This
allows for finer control over position than can be achieved via MoveCursorMouse.
See MoveCursorScreen for a slightly different method.

MoveCursorMouse()
Moves the planted cursor based on an event that includes an (x,y) position,
which include ButtonPress, ButtonRelease, EnterNotify, LeaveNotify, KeyPress,
KeyRelease, and MotionNotify.  If the cursor is free floating, this action
has no effect.  This allows a translation to specify something like
<Btn1Motion> to move the cursor, since you really don't want to change
the cursor location if it is floating (the cursor is already attached to
the mouse).

MoveCursorScreen(direction)
Moves the planted cursor one Screen pixel in the indicated direction.  The
"direction" argument is one of the strings "left", "right", "up", or "down".
If the cursor is free floating, this action has no effect.  This is usually
used to move the cursor based on arrow keys.  At zooms > 1, this moves the
cursor by a fraction of an Image pixel.  At zooms < 1, the cursor still moves
by one Screen pixel, which may be more than one Image pixel.

PanEdge(direction)
Pans to the edge of the image in the indicated direction.  The "direction"
argument is one of the strings "left", "right", "up", or "down", corresponding
to panning to the left side, right side, top, or bottom of the image,
respectively.

PanHalfView(direction)
Pans in the indicated direction by half the view size.  The "direction"
argument is one of the strings "left", "right", "up", or "down".  See PanOne().

PanOne(direction)
Pans one pixel in the indicated direction.  The "direction" argument is one of
the strings "left", "right", "up", or "down".  Case does not matter, and only
the first character is significant.  An invalid character, or no argument,
will cause the action to be quietly ignored.  The direction is interpreted by
thinking about moving a scrollbar in the same direction.  The data is shifted
in the opposite direction, and new data appears on the indicated side.

##############################################################################
		ImageOverlay
##############################################################################

ImageOverlay is completely controlled by Image, and has no public interface.
All it does is create a window with the requested visual and colormap, and
then funnels Expose events to its parent.  ImageOverlay should never be
instantiated from anything except the Image widget.

==============================================================================
		ImageOverlay Resources
==============================================================================

XvicNvisual		XvicCVisual	Visual *	none		C
Defines the visual type to use for the overlay window.

XtNcolormap		XtCColormap	Colormap	none		C
Defines the colormap to use for the overlay window.  This is actually a Core
resource.

$ Return
$!#############################################################################
$Test_File:
$ create Test_IW
#
# This is a standard X resource file.  Testers should try different values
# in here as well as using the on-screen controls.
#
# You must make this file available to X.  The easiest way in Unix is to
# rename the file to "Test_IW" (note the capitalization!) and set the
# XAPPLRESDIR environment variable to point at the directory it is
# in (with a trailing slash!).  The easiest way in VMS is to rename the file
# to "test_iw.dat" and put it in your sys$login directory.
#

*image.xZoomIn: 1
*image.xZoomOut: 1
*image.yZoomIn: 1
*image.yZoomOut: 1
*image.tileWidth: 100
*image.tileHeight: 100

*image.viewHeight: 300
*image.viewWidth: 300

*image.imageMode: bw
#*image.colormapPolicy: half
*image.colorColormapPolicy: Half
*image.colorDither: ordered
*image.bwColormapPolicy: Half

*Test_IW.allowShellResize: True
#*image.navigationType: tab_group
*image.traversalOn: False

#*image.highlightThickness: 8
*image.shadowThickness: 8

*image.translations: #augment \n\
	~Shift<Btn1Down>:		MousePanStart() \n\
	~Shift<Btn1Motion>:		MousePan() \n\
	~Shift~Ctrl~Meta<Key>osfLeft:	PanOne(left) \n\
	~Shift~Ctrl~Meta<Key>osfRight:	PanOne(right) \n\
	~Shift~Ctrl~Meta<Key>osfUp:	PanOne(up) \n\
	~Shift~Ctrl~Meta<Key>osfDown:	PanOne(down) \n\
	Ctrl~Shift~Meta<Key>osfLeft:	PanEdge(left) \n\
	Ctrl~Shift~Meta<Key>osfRight:	PanEdge(right) \n\
	Ctrl~Shift~Meta<Key>osfUp:	PanEdge(up) \n\
	Ctrl~Shift~Meta<Key>osfDown:	PanEdge(down) \n\
	Shift~Ctrl~Meta<Key>osfLeft:	PanHalfView(left) \n\
	Shift~Ctrl~Meta<Key>osfRight:	PanHalfView(right) \n\
	Shift~Ctrl~Meta<Key>osfUp:	PanHalfView(up) \n\
	Shift~Ctrl~Meta<Key>osfDown:	PanHalfView(down) \n\
	<Key>osfActivate:		Input("Return hit") \n\
	<Btn2Down>:			Input("Draw","start") \n\
	Button2<Key>space:		Input("Draw","mark") \n\
	<Btn2Motion>:			Input("Draw","drag") \n\
	<Btn2Up>:			Input("Draw","end") \n\
	<Key>osfEscape:			CursorMode(toggle) \n\
	~Shift<Key>grave:		CursorMode(toggle) \n\
	<Key>asciitilde:		CursorMode(toggle,true) \n\
	Shift<Key>grave:		CursorMode(toggle,true) \n\
	<Key>plus:			CursorMode(floating) \n\
	<Key>minus:			CursorMode(planted) \n\
	Shift<Motion>:			MoveCursorMouse() \n\
	<Key>c:				MoveCursorMouse() \n\
	Shift Ctrl<Key>osfLeft:		MoveCursor(left) \n\
	Shift Ctrl<Key>osfRight:	MoveCursor(right) \n\
	Shift Ctrl<Key>osfUp:		MoveCursor(up) \n\
	Shift Ctrl<Key>osfDown:		MoveCursor(down) \n\
	Meta<Key>osfLeft:		MoveCursorScreen(left) \n\
	Meta<Key>osfRight:		MoveCursorScreen(right) \n\
	Meta<Key>osfUp:			MoveCursorScreen(up) \n\
	Meta<Key>osfDown:		MoveCursorScreen(down) \n\
	<Visible>:			Input("VisibilityNotify")

$!-----------------------------------------------------------------------------
$ create test_iw.c
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include "XvicImage.h"

#ifndef PI
#define PI 3.14159265358979323846
#endif

/* #define DPR(x)  printf x */
#define DPR(x)

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef ABS
#define ABS(x) ((x)>=0 ? (x) : (-(x)))
#endif

typedef enum {MoveExisting, EraseExisting,
	FillArc, FillArcs, FillPolygon, FillRectangle, FillRectangles,
	Arc, Arcs, Bitmap, ImageString, Line, Lines, Point, Points,
	Rectangle, Rectangles, Segments, DrawString} Shape;
#define MAX_POINTS 100

struct GraphicsStuff {
   XvicColor color, bgcolor;
   XvicGC gc, rubber_gc, new_gc;
   Widget id_widget, last_id_widget, shape_widget, text_widget;
   Widget gc_widget, last_gc_widget;
   XvicID id, rubber_id;
   Shape shape;
   int shape_int;
   XGCValues gc_values;
   unsigned long gc_mask;
   Boolean rubber_mode;
   int justify;
   XtIntervalId timeout_id;
   double x, y;				/* "start" locations */
   double width, height;
   int angle1, angle2;
   int npoints;
   XvicArc arcs[MAX_POINTS+1];
   XvicPoint points[MAX_POINTS+1];
   XvicRectangle rectangles[MAX_POINTS+1];
   XvicSegment segments[MAX_POINTS+1];
   int state;		/* which part of the object is being tracked now */
   unsigned int bitmap_width, bitmap_height;
   Pixmap bitmap;
   int bitmap_hot_x, bitmap_hot_y;
} gbl_gr;

typedef struct _TimerData {
   XEvent event;
   int x, y;
} TimerData;


Widget gbl_iw;
int memory_control;
unsigned char *shared_buf = NULL;
unsigned char *shared_buf2 = NULL;
unsigned char *shared_buf3 = NULL;

double data_min, data_max, data_inc;

Widget w_imageMode;
Widget w_dataType;
Widget w_ditherMode;
Widget w_lutType;
Widget w_lut16Type;
Widget w_stretchPolicy;
Widget w_colormapPolicy;
Widget w_visualType;
Widget w_enableDirectColor;
Widget w_workProcPolicy;
Widget w_dataSavePolicy;
Widget w_constrainPan;
Widget w_bwDither;
Widget w_bwStretchPolicy;
Widget w_bwColormapPolicy;
Widget w_colorDither;
Widget w_colorStretchPolicy;
Widget w_colorColormapPolicy;
Widget w_pseudoDither;
Widget w_pseudoStretchPolicy;
Widget w_pseudoColormapPolicy;
Widget w_xPan;
Widget w_yPan;
Widget w_xSubpixelPan;
Widget w_ySubpixelPan;
Widget w_xZoomIn;
Widget w_xZoomOut;
Widget w_yZoomIn;
Widget w_yZoomOut;
Widget w_maximumMemory;
Widget w_grayLevels;
Widget w_redLevels;
Widget w_greenLevels;
Widget w_blueLevels;
Widget w_rawDataMin;
Widget w_rawDataMax;
Widget w_scaledDataMax;
Widget w_outputDataMax;
Widget w_imageWidth;
Widget w_imageHeight;
Widget w_tileWidth;
Widget w_tileHeight;
Widget w_viewWidth;
Widget w_viewHeight;
Widget w_width;
Widget w_height;
Widget w_xPreSubpixelPan;
Widget w_yPreSubpixelPan;
Widget w_xPreZoomIn;
Widget w_xPreZoomOut;
Widget w_yPreZoomIn;
Widget w_yPreZoomOut;
Widget w_bwVisualType;
Widget w_colorVisualType;
Widget w_pseudoVisualType;
Widget w_cursorMode;
Widget w_scrollBarDisplayPolicy;
Widget w_cursorX;
Widget w_cursorY;
Widget w_cursorXfp;
Widget w_cursorYfp;
Widget w_cursorForeground;
Widget w_cursorBackground;
Widget w_cursor;
Widget w_trackFloatingCursor;
Widget w_input;
Widget w_dataMin;
Widget w_dataMax;
Widget w_dataInc;


Widget gw_color;
Widget gw_bgColor;
Widget gw_text;
Widget gw_textJustify;
Widget gw_bitmap;
Widget gw_erase;
Widget gw_shape;
Widget gw_id;
Widget gw_lastId;
Widget gw_gc;
Widget gw_lastGc;
Widget gw_arcMode;
Widget gw_capStyle;
Widget gw_dashes;
Widget gw_dashOffset;
Widget gw_fillRule;
Widget gw_font;
Widget gw_joinStyle;
Widget gw_lineStyle;
Widget gw_lineWidth;
Widget gw_rubberMode;

/*--------------------------------------------------------------*/

/*!!!! VVVV !!!!*/
void DestroyCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   XtDestroyWidget(gbl_iw);
}
/*!!!! ^^^^ !!!!*/

void UpdateCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   reset_defaults(gbl_iw);
}

void EraseOverlayCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   XvicImageEraseOverlay(gbl_iw);
}

void expose(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   XvicImageData img;
   int x_prezoom_in, x_prezoom_out, y_prezoom_in, y_prezoom_out;
   int x_presub, y_presub;
   int i, j;
   int width, height;
   unsigned char image_mode, data_type;
   int x, y, offs;
   int pixel_size;
   int data_range;

   img.x = cb->x;
   img.y = cb->y;
   img.width = cb->width;
   img.height = cb->height;
   img.memory_control = memory_control;
   img.line_width = cb->prezoom_width;	/* adjust for pixel size later */
   img.start_offset = 0;

   XtVaGetValues(iw,
	XvicNxPreZoomIn, &x_prezoom_in, XvicNxPreZoomOut, &x_prezoom_out,
	XvicNyPreZoomIn, &y_prezoom_in, XvicNyPreZoomOut, &y_prezoom_out,
	XvicNxPreSubpixelPan, &x_presub, XvicNyPreSubpixelPan, &y_presub,
	XvicNimageWidth, &width, XvicNimageHeight, &height,
	XvicNimageMode, &image_mode, XvicNdataType, &data_type,
	NULL);

   if (data_type == XvicREAL || data_type == XvicDOUBLE)
      data_range = (data_max - data_min) / data_inc;
   else
      data_range = (data_max + 1 - data_min) / data_inc;
   if (data_range == 0)
      data_range = 1;		/* avoid divide by 0 */

   switch (data_type) {
      case XvicBYTE: pixel_size = sizeof(XvicByte); break;
      case XvicHALF: pixel_size = sizeof(XvicHalf); break;
      case XvicUHALF: pixel_size = sizeof(XvicUHalf); break;
      case XvicFULL: pixel_size = sizeof(XvicFull); break;
      case XvicUFULL: pixel_size = sizeof(XvicUFull); break;
      case XvicREAL: pixel_size = sizeof(XvicReal); break;
      case XvicDOUBLE: pixel_size = sizeof(XvicDouble); break;
      default: printf("Bad data type in test program!\n"); exit(1);
   }
   img.line_width = cb->prezoom_width * pixel_size;

   if (memory_control == XvicMEMORY_SHARED) {
      width = ((width+1)*x_prezoom_in-1)/x_prezoom_out;
      height = ((height+1)*y_prezoom_in-1)/y_prezoom_out;
      img.line_width = width * pixel_size;;
      if (!shared_buf) {
         shared_buf = (unsigned char *)malloc(width*height*pixel_size);
         shared_buf2 = (unsigned char *)malloc(width*height*pixel_size);
         shared_buf3 = (unsigned char *)malloc(width*height*pixel_size);
         if (shared_buf==NULL || shared_buf2==NULL || shared_buf3==NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }
   }

   if (image_mode == XvicBW) {
      if (memory_control==XvicMEMORY_SHARED) {
         img.bw_pixels=shared_buf + cb->prezoom_y*img.line_width;
         img.start_offset = cb->prezoom_x * pixel_size;
      }
      else {
         img.bw_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         if (img.bw_pixels == NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }

#define BWLOOP(DATATYPE)						   \
      for (i=0; i<cb->prezoom_height; i++) {				   \
         for (j=0; j<cb->prezoom_width; j++) {				   \
            *(DATATYPE *)(img.bw_pixels + i*img.line_width +		   \
				j*pixel_size+img.start_offset) =	   \
	      ((((i+cb->prezoom_y)*y_prezoom_out-y_presub)/y_prezoom_in +  \
		((j+cb->prezoom_x)*x_prezoom_out-x_presub)/x_prezoom_in) % \
                data_range) * data_inc + data_min;			   \
            }								   \
         }

      switch (data_type) {
         case XvicBYTE:
            BWLOOP(XvicByte)
            break;
         case XvicHALF:
            BWLOOP(XvicHalf)
            break;
         case XvicUHALF:
            BWLOOP(XvicUHalf)
            break;
         case XvicFULL:
            BWLOOP(XvicFull)
            break;
         case XvicUFULL:
            BWLOOP(XvicUFull)
            break;
         case XvicREAL:
            BWLOOP(XvicReal)
            break;
         case XvicDOUBLE:
            BWLOOP(XvicDouble)
            break;
      }
   }
   else {			/* Color */
      if (memory_control==XvicMEMORY_SHARED) {
         img.red_pixels=shared_buf + cb->prezoom_y * img.line_width;
         img.grn_pixels=shared_buf2 + cb->prezoom_y * img.line_width;
         img.blu_pixels=shared_buf3 + cb->prezoom_y * img.line_width;
         img.start_offset = cb->prezoom_x;
      }
      else {
         img.red_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         img.grn_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         img.blu_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         if (img.red_pixels==NULL || img.grn_pixels==NULL || img.blu_pixels==NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }

#define COLORLOOP(DATATYPE)						     \
      for (i=0; i<cb->prezoom_height; i++) {				     \
         for (j=0; j<cb->prezoom_width; j++) {				     \
            y=((i+cb->prezoom_y)*y_prezoom_out-y_presub)/y_prezoom_in;	     \
            x=((j+cb->prezoom_x)*x_prezoom_out-x_presub)/x_prezoom_in;	     \
            offs = i*img.line_width+j*pixel_size+img.start_offset;	     \
            *(DATATYPE *)(img.red_pixels+offs) = ((x + y) % data_range) *    \
						data_inc + data_min;	     \
            *(DATATYPE *)(img.grn_pixels+offs) = (abs(y - x) % data_range) * \
						data_inc + data_min;	     \
            *(DATATYPE *)(img.blu_pixels+offs) =			     \
		(((x+y*2)%data_range) * data_inc + data_min +		     \
		 ((x*2+y)%data_range) * data_inc + data_min) / 2;	     \
            /* *(DATATYPE *)(img.blu_pixels+offs) = ((((x+y*2) + (x*2+y)) / 2)*/  \
			/*		% data_range) * data_inc + data_min; */ \
         }								     \
      }

      switch (data_type) {
         case XvicBYTE:
            COLORLOOP(XvicByte)
            break;
         case XvicHALF:
            COLORLOOP(XvicHalf)
            break;
         case XvicUHALF:
            COLORLOOP(XvicUHalf)
            break;
         case XvicFULL:
            COLORLOOP(XvicFull)
            break;
         case XvicUFULL:
            COLORLOOP(XvicUFull)
            break;
         case XvicREAL:
            COLORLOOP(XvicReal)
            break;
         case XvicDOUBLE:
            COLORLOOP(XvicDouble)
            break;
      }

   }

   XvicImageWrite(iw, &img, FALSE);

   if (memory_control==XvicMEMORY_APPLIC) {
      if (image_mode == XvicBW)
         free(img.bw_pixels);
      else {
         free(img.red_pixels);
         free(img.grn_pixels);
         free(img.blu_pixels);
      }
   }
}

void resize(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   reset_defaults(gbl_iw);
}

void pan(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   set_int_default(w_xPan, XvicNxPan);
   set_int_default(w_yPan, XvicNyPan);
}

void work_proc_active(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   Pixel fg, bg;

   if (cb->reason != XvicCR_WORK_PROC_ACTIVE)
      return;
   /* Invert colors on WorkProcPolicy widget to indicate state */
   XtVaGetValues(w_workProcPolicy, XmNbackground, &bg, XmNforeground, &fg, NULL);
   XtVaSetValues(w_workProcPolicy, XmNbackground, fg, XmNforeground, bg, NULL);
}

/************************************************************************/
/* Graphics drawing stuff.  Implements a simple editor.			*/
/************************************************************************/

/* If the timer expires while still dragging, pan a little bit and	*/
/* invoke the Input callback again.					*/

void timer_proc(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
   TimerData *td = (TimerData *)client_data;
   String params[2] = {"Draw", "drag"};
   int x_pan, y_pan;
   int x1, y1, x2, y2;

   XvicImageDisplayBounds(gbl_iw, &x1, &y1, &x2, &y2);

   XtVaGetValues(gbl_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   if (td->x < x1)
      x_pan -= (x1 - td->x);
   else if (td->x > x2)
      x_pan += (td->x - x2);
   if (td->y < y1)
      y_pan -= (y1 - td->y);
   else if (td->y > y2)
      y_pan += (td->y - y2);

   XtVaSetValues(gbl_iw, XvicNxPan, x_pan, XvicNyPan, y_pan, NULL);

   XtCallActionProc(gbl_iw, "Input", (XEvent *)client_data, params, 2);
}

/* Get the angle from the position given the bounding box */

int get_angle(x_left, y_top, width, height, x, y)
double x_left, y_top;
double width, height;
double x, y;
{
   double xc, yc;		/* center */
   double angle;

   xc = x_left + (width/2.0);
   yc = y_top + (height/2.0);

   angle = atan2(yc-y, x-xc);

   return (int) ((angle * 180.0 / PI) * 64.0);
}

void start_object(x, y)
double x, y;
{
   gbl_gr.x = x;
   gbl_gr.y = y;

   switch (gbl_gr.shape) {
      case FillArc:
      case Arc:
         gbl_gr.width = 0;
         gbl_gr.height = 0;
         gbl_gr.angle1 = 0;
         gbl_gr.angle2 = 360*64;
         gbl_gr.state = 1;
         break;

      case FillArcs:
      case Arcs:
         gbl_gr.npoints = 0;
         gbl_gr.arcs[0].x = x;
         gbl_gr.arcs[0].y = y;
         gbl_gr.arcs[0].width = 0;
         gbl_gr.arcs[0].height = 0;
         gbl_gr.arcs[0].angle1 = 0;
         gbl_gr.arcs[0].angle2 = 360*64;
         gbl_gr.state = 1;
         break;

      case FillPolygon:
      case Lines:
      case Points:
         gbl_gr.npoints = 1;
         gbl_gr.points[0].x = x;
         gbl_gr.points[0].y = y;
         break;

      case FillRectangles:
      case Rectangles:
         gbl_gr.npoints = 0;
         gbl_gr.rectangles[0].x = x;
         gbl_gr.rectangles[0].y = y;
         gbl_gr.rectangles[0].width = 0;
         gbl_gr.rectangles[0].height = 0;
         gbl_gr.state = 1;
         break;

      case Segments:
         gbl_gr.npoints = 0;
         gbl_gr.segments[0].x1 = x;
         gbl_gr.segments[0].x2 = x;
         gbl_gr.segments[0].y1 = y;
         gbl_gr.segments[0].y2 = y;
         gbl_gr.state = 1;
         break;

      default:
         break;
   }

}

/* Add a point to an existing object */

void point_object(x, y, use_rubber)
double x, y;
int use_rubber;
{
   switch (gbl_gr.shape) {

      case FillArc:
      case Arc:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.x = x;
               gbl_gr.y = y;
               gbl_gr.width = gbl_gr.height = gbl_gr.angle1 = 0;
               gbl_gr.angle2 = 360*64;
               gbl_gr.state = 1;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.width = x - gbl_gr.x + 1.0;
               gbl_gr.height = y - gbl_gr.y + 1.0;
               gbl_gr.state = 2;
               break;
            case 2:			/* first angle */
               gbl_gr.angle1 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y);
               gbl_gr.angle2 = 360*64;	/* make it easy to get a circle */
               gbl_gr.state = 3;
               break;
            case 3:			/* second angle */
               gbl_gr.angle2 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y) - gbl_gr.angle1;
               if (gbl_gr.angle2 <= 0)
                  gbl_gr.angle2 += 360*64;
               break;
         }
         break;
 
      case FillArcs:
      case Arcs:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.arcs[gbl_gr.npoints].x = x;
               gbl_gr.arcs[gbl_gr.npoints].y = y;
               gbl_gr.arcs[gbl_gr.npoints].width = 0;
               gbl_gr.arcs[gbl_gr.npoints].height = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle1 = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               gbl_gr.state = 1;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.arcs[gbl_gr.npoints].width =
					x - gbl_gr.arcs[gbl_gr.npoints].x + 1.0;
               gbl_gr.arcs[gbl_gr.npoints].height =
					y - gbl_gr.arcs[gbl_gr.npoints].y + 1.0;
               gbl_gr.state = 2;
               break;
            case 2:			/* first angle */
               gbl_gr.arcs[gbl_gr.npoints].angle1 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
				gbl_gr.arcs[gbl_gr.npoints].y,
				gbl_gr.arcs[gbl_gr.npoints].width,
				gbl_gr.arcs[gbl_gr.npoints].height, x, y);
               /* make it easy to get a circle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               gbl_gr.state = 3;
               break;
            case 3:			/* second angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
			gbl_gr.arcs[gbl_gr.npoints].y,
			gbl_gr.arcs[gbl_gr.npoints].width,
			gbl_gr.arcs[gbl_gr.npoints].height, x, y) -
				gbl_gr.arcs[gbl_gr.npoints].angle1;
               if (gbl_gr.arcs[gbl_gr.npoints].angle2 <= 0)
                  gbl_gr.arcs[gbl_gr.npoints].angle2 += 360*64;
               if (gbl_gr.npoints < MAX_POINTS)
                  gbl_gr.npoints++;
               gbl_gr.state = 0;
               break;
         }
         break;
 
      case FillPolygon:
      case Lines:
      case Points:
         if (gbl_gr.npoints < MAX_POINTS-1) {
            gbl_gr.points[gbl_gr.npoints].x = x;
            gbl_gr.points[gbl_gr.npoints].y = y;
            gbl_gr.npoints++;
         }
         break;

      case FillRectangles:
      case Rectangles:
         if (gbl_gr.state) {	/* state==1, finish this rect, prep for next */
            if (x < gbl_gr.rectangles[gbl_gr.npoints].x ||
                y < gbl_gr.rectangles[gbl_gr.npoints].y)
               printf("Warning: Rectangles must be pulled down-right\n");
            gbl_gr.rectangles[gbl_gr.npoints].width =
			x - gbl_gr.rectangles[gbl_gr.npoints].x + 1.0;
            gbl_gr.rectangles[gbl_gr.npoints].height =
			y - gbl_gr.rectangles[gbl_gr.npoints].y + 1.0;
            if (gbl_gr.npoints < MAX_POINTS)
               gbl_gr.npoints++;
            gbl_gr.state = 0;
         }
         else {		/* state==0, starting new rect */
            gbl_gr.rectangles[gbl_gr.npoints].x = x;
            gbl_gr.rectangles[gbl_gr.npoints].y = y;
            gbl_gr.rectangles[gbl_gr.npoints].width = 0;
            gbl_gr.rectangles[gbl_gr.npoints].height = 0;
            gbl_gr.state = 1;
         }
         break;

      case Segments:
         if (gbl_gr.state) {	/* state==1, finish this seg, prep for next */
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
            if (gbl_gr.npoints < MAX_POINTS)
               gbl_gr.npoints++;
            gbl_gr.state = 0;
         }
         else {		/* state==0, starting new segment */
            gbl_gr.segments[gbl_gr.npoints].x1 = x;
            gbl_gr.segments[gbl_gr.npoints].y1 = y;
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
            gbl_gr.state = 1;
         }

      default:
         break;
   }
}

void draw_object(x, y, use_rubber)
double x, y;
int use_rubber;
{
   XvicGC gc;
   XvicID id;
   double h, w;
   double xx, yy;
   char buf[20];
   char *string;

   if (use_rubber) {
      gc = gbl_gr.rubber_gc;
      id = gbl_gr.rubber_id;
   }
   else {
      gc = gbl_gr.gc;
      id = gbl_gr.id;
   }

   switch (gbl_gr.shape) {
      case FillArc:
      case Arc:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.x = x;
               gbl_gr.y = y;
               gbl_gr.width = gbl_gr.height = gbl_gr.angle1 = 0;
               gbl_gr.angle2 = 360*64;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.width = x - gbl_gr.x + 1.0;
               gbl_gr.height = y - gbl_gr.y + 1.0;
               break;
            case 2:			/* first angle */
               gbl_gr.angle1 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y);
               gbl_gr.angle2 = 315*64;	/* let user see the angle */
               break;
            case 3:			/* second angle */
               gbl_gr.angle2 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y) - gbl_gr.angle1;
               if (gbl_gr.angle2 <= 0)
                  gbl_gr.angle2 += 360*64;
               break;
         }
         if (gbl_gr.shape == Arc)
            id = XvicImageDrawArc(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, gbl_gr.width, gbl_gr.height,
		gbl_gr.angle1, gbl_gr.angle2);
         else
            id = XvicImageFillArc(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, gbl_gr.width, gbl_gr.height,
		gbl_gr.angle1, gbl_gr.angle2);
         break;

      case FillArcs:
      case Arcs:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.arcs[gbl_gr.npoints].x = x;
               gbl_gr.arcs[gbl_gr.npoints].y = y;
               gbl_gr.arcs[gbl_gr.npoints].width = 0;
               gbl_gr.arcs[gbl_gr.npoints].height = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle1 = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.arcs[gbl_gr.npoints].width =
					x - gbl_gr.arcs[gbl_gr.npoints].x + 1.0;
               gbl_gr.arcs[gbl_gr.npoints].height =
					y - gbl_gr.arcs[gbl_gr.npoints].y + 1.0;
               break;
            case 2:			/* first angle */
               gbl_gr.arcs[gbl_gr.npoints].angle1 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
				gbl_gr.arcs[gbl_gr.npoints].y,
				gbl_gr.arcs[gbl_gr.npoints].width,
				gbl_gr.arcs[gbl_gr.npoints].height, x, y);
               /* let user see the angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 315*64;
               break;
            case 3:			/* second angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
			gbl_gr.arcs[gbl_gr.npoints].y,
			gbl_gr.arcs[gbl_gr.npoints].width,
			gbl_gr.arcs[gbl_gr.npoints].height, x, y) -
				gbl_gr.arcs[gbl_gr.npoints].angle1;
               if (gbl_gr.arcs[gbl_gr.npoints].angle2 <= 0)
                  gbl_gr.arcs[gbl_gr.npoints].angle2 += 360*64;
               break;
         }
         if (gbl_gr.shape == Arcs)
            id = XvicImageDrawArcs(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.arcs, gbl_gr.npoints+1);
         else
            id = XvicImageFillArcs(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.arcs, gbl_gr.npoints+1);
         break;
 
      case Bitmap:
         id = XvicImageDrawBitmap(gbl_iw, id, gc, gbl_gr.color,
		x, y, gbl_gr.bitmap, gbl_gr.bitmap_width, gbl_gr.bitmap_height,
		gbl_gr.bitmap_hot_x, gbl_gr.bitmap_hot_y);
         break;

      case Line:
         id = XvicImageDrawLine(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, x, y);
         break;

      case FillPolygon:
      case Lines:
      case Points:
         gbl_gr.points[gbl_gr.npoints].x = x;
         gbl_gr.points[gbl_gr.npoints].y = y;
         if (gbl_gr.shape == Lines)
            id = XvicImageDrawLines(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, CoordModeOrigin);
         else if (gbl_gr.shape == FillPolygon)
            id = XvicImageFillPolygon(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, Complex, CoordModeOrigin);
         else		/* Points */
            id = XvicImageDrawPoints(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, CoordModeOrigin);
         break;

      case Point:
         id = XvicImageDrawPoint(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y);
         break;

      case FillRectangle:
      case Rectangle:
         xx = MIN(gbl_gr.x, x);
         yy = MIN(gbl_gr.y, y);
         h = ABS(x - gbl_gr.x) + 1.0;
         w = ABS(y - gbl_gr.y) + 1.0;
         if (gbl_gr.shape == Rectangle)
            id = XvicImageDrawRectangle(gbl_iw, id, gc, gbl_gr.color,
		xx, yy, h, w);
         else
            id = XvicImageFillRectangle(gbl_iw, id, gc, gbl_gr.color,
		xx, yy, h, w);
         break;

      case FillRectangles:
      case Rectangles:
         if (gbl_gr.state) {			/* second part of rect */
            if (x < gbl_gr.rectangles[gbl_gr.npoints].x ||
                y < gbl_gr.rectangles[gbl_gr.npoints].y)
               printf("Warning: Rectangles must be pulled down-right\n");
            gbl_gr.rectangles[gbl_gr.npoints].width =
			x - gbl_gr.rectangles[gbl_gr.npoints].x + 1.0;
            gbl_gr.rectangles[gbl_gr.npoints].height =
			y - gbl_gr.rectangles[gbl_gr.npoints].y + 1.0;
         }
         else {				/* first part of rect */
            gbl_gr.rectangles[gbl_gr.npoints].x = x;
            gbl_gr.rectangles[gbl_gr.npoints].y = y;
            gbl_gr.rectangles[gbl_gr.npoints].width = 0;
            gbl_gr.rectangles[gbl_gr.npoints].height = 0;
         }
         if (gbl_gr.shape == Rectangles)
            id = XvicImageDrawRectangles(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.rectangles, gbl_gr.npoints+1);
         else
            id = XvicImageFillRectangles(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.rectangles, gbl_gr.npoints+1);
         break;

      case Segments:
         if (gbl_gr.state) {			/* second part of line */
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
         }
         else {				/* first part of line */
            gbl_gr.segments[gbl_gr.npoints].x1 = x;
            gbl_gr.segments[gbl_gr.npoints].y1 = y;
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
         }
         id = XvicImageDrawSegments(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.segments, gbl_gr.npoints+1);
         break;

      case DrawString:
         XtVaGetValues(gbl_gr.text_widget, XmNvalue, &string, NULL);
         id = XvicImageDrawString(gbl_iw, id, gc, gbl_gr.color,
		x, y, string, strlen(string), gbl_gr.justify);
         XtFree(string);
         break;

      case ImageString:
         XtVaGetValues(gbl_gr.text_widget, XmNvalue, &string, NULL);
         id = XvicImageDrawImageString(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.bgcolor, x, y, string, strlen(string), gbl_gr.justify);
         XtFree(string);
         break;
   }

   if (use_rubber) {
      gbl_gr.rubber_id = id;
   }
   else {
      sprintf(buf, "%d", id);
      XtVaSetValues(gbl_gr.last_id_widget, XmNvalue, buf, NULL);
   }
}

void input(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   static TimerData td;

   if (cb->input_num_params > 0)
      XtVaSetValues(w_input, XmNvalue, cb->input_params[0], NULL);
   else
      XtVaSetValues(w_input, XmNvalue, "", NULL);

   if (cb->input_num_params > 0) {
      if (strcmp(cb->input_params[0], "Draw") == 0) {
         if (gbl_gr.timeout_id) {
            XtRemoveTimeOut(gbl_gr.timeout_id);
            gbl_gr.timeout_id = 0;
         }
         if (cb->input_num_params > 1) {
            if (strcmp(cb->input_params[1], "start") == 0) {
               if (gbl_gr.shape == MoveExisting) {
                  gbl_gr.x = cb->x_fp;
                  gbl_gr.y = cb->y_fp;
               }
               else
                  start_object(cb->x_fp, cb->y_fp);
               gbl_gr.rubber_id = 0;
            }
            else if (strcmp(cb->input_params[1], "drag") == 0) {
               if (gbl_gr.shape == MoveExisting) {
                  XvicImageMoveObject(iw, gbl_gr.id, (cb->x_fp-gbl_gr.x),
						     (cb->y_fp-gbl_gr.y));
                  gbl_gr.x = cb->x_fp;
                  gbl_gr.y = cb->y_fp;
               }
               else {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, True);
               }
               if (!cb->on_screen) {		/* Set up an auto-pan */
                  memcpy((void *)&td.event, (void *)cb->event, sizeof(XEvent));
                  td.x = cb->x;
                  td.y = cb->y;
                  gbl_gr.timeout_id = XtAppAddTimeOut(XtWidgetToApplicationContext(iw),
			500, timer_proc, &td);
               }
            }
            else if (strcmp(cb->input_params[1], "mark") == 0) {
               if (gbl_gr.shape != MoveExisting) {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, True);
                  point_object(cb->x_fp, cb->y_fp, True);
               }
            }
            else if (strcmp(cb->input_params[1], "end") == 0) {
               if (gbl_gr.shape == MoveExisting)
                  XvicImageMoveObject(iw, gbl_gr.id, (cb->x_fp-gbl_gr.x),
						     (cb->y_fp-gbl_gr.y));
               else {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, False);
               }
            }
         }
      }
   }
}

void cursor(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   char buf[20];
   XmHighlightMode mode;

   sprintf(buf, "%0d", cb->x);
   XtVaSetValues(w_cursorX, XmNvalue, buf, NULL);
   sprintf(buf, "%0d", cb->y);
   XtVaSetValues(w_cursorY, XmNvalue, buf, NULL);

   sprintf(buf, "%g", cb->x_fp);
   XtVaSetValues(w_cursorXfp, XmNvalue, buf, NULL);
   sprintf(buf, "%g", cb->y_fp);
   XtVaSetValues(w_cursorYfp, XmNvalue, buf, NULL);

   if (cb->on_screen)
      mode = XmHIGHLIGHT_NORMAL;
   else
      mode = XmHIGHLIGHT_SELECTED;

   XmTextSetHighlight(w_cursorX, 0, 20, mode);
   XmTextSetHighlight(w_cursorY, 0, 20, mode);
}

/*--------------------------------------------------------------*/

void CB_imageMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicCOLOR, XvicBW};
   XtVaSetValues(gbl_iw, XvicNimageMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void set_data_range(type)
unsigned char type;
{
   char buf[20];

   /* Change data ranges to default */
   switch(type) {
      case XvicBYTE:
         data_min = 0; data_max = 255; data_inc = 1; break;
      case XvicHALF:
         data_min = -32768; data_max = 32767; data_inc = 200; break;
      case XvicUHALF:
         data_min = 0; data_max = 65535; data_inc = 100; break;
      case XvicFULL:
         data_min = -50000; data_max = 50000; data_inc = 100; break;
      case XvicUFULL:
         data_min = 0; data_max = 100000; data_inc = 200; break;
      case XvicREAL:
         data_min = 0.0; data_max = 1.0; data_inc = .001; break;
      case XvicDOUBLE:
         data_min = -100.0; data_max = 100.0; data_inc = .2; break;
   }
   sprintf(buf, "%0.10lf", data_min);
   XtVaSetValues(w_dataMin, XmNvalue, buf, NULL);
   sprintf(buf, "%0.10lf", data_max);
   XtVaSetValues(w_dataMax, XmNvalue, buf, NULL);
   sprintf(buf, "%0.10lf", data_inc);
   XtVaSetValues(w_dataInc, XmNvalue, buf, NULL);
}

void CB_dataType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicBYTE, XvicHALF, XvicUHALF, XvicFULL,
				    XvicUFULL, XvicREAL, XvicDOUBLE};
   set_data_range(values[(int)btn]);
   XtVaSetValues(gbl_iw, XvicNdataType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_ditherMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNditherMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_lutType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY};
   XtVaSetValues(gbl_iw, XvicNlutType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_lut16Type(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY};
   XtVaSetValues(gbl_iw, XvicNlut16Type, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_stretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNstretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNcolormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_visualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNvisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_workProcPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicREAD, XvicALL};
   XtVaSetValues(gbl_iw, XvicNworkProcPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_dataSavePolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicRAW, XvicXIMAGE, XvicPIXMAP};
   XtVaSetValues(gbl_iw, XvicNdataSavePolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_constrainPan(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicX_ONLY, XvicY_ONLY, XvicBOTH};
   XtVaSetValues(gbl_iw, XvicNconstrainPan, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_bwDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNbwDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNcolorDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNpseudoDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNbwStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNcolorStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNpseudoStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNbwColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNcolorColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNpseudoColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNbwVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNcolorVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNpseudoVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_cursorMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFLOATING, XvicPLANTED};
   XtVaSetValues(gbl_iw, XvicNcursorMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_scrollBarDisplayPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTATIC, XvicAS_NEEDED, XvicNEVER};
   XtVaSetValues(gbl_iw, XvicNscrollBarDisplayPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_trackFloatingCursor(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {True, False};
   XtVaSetValues(gbl_iw, XvicNtrackFloatingCursor, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_enableDirectColor(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {True, False};
   XtVaSetValues(gbl_iw, XvicNenableDirectColor, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_int(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   int value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

void CB_int_sh(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   int value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   if (memory_control == XvicMEMORY_SHARED && shared_buf) {
      free(shared_buf);
      free(shared_buf2);
      free(shared_buf3);
      shared_buf = NULL;
   }
   XtFree(buf);
}

void CB_Dim(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   Dimension value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

void CB_double(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   double value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atof(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, XvicDOUBLE_ARG(value), NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

/*--------------------------------------------------------------*/

void CB_string(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *value;
   char *old_value;
   XtVaGetValues(w, XmNvalue, &value, NULL);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (old_value == NULL || strcmp(value, old_value) != 0) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(value);
}

/*--------------------------------------------------------------*/

void CB_data_min(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_min = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

void CB_data_max(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_max = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

void CB_data_inc(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_inc = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_mem_ctl(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicMEMORY_APPLIC, XvicMEMORY_WIDGET, XvicMEMORY_SHARED};
   if (memory_control == XvicMEMORY_SHARED && shared_buf) {
      free(shared_buf);
      free(shared_buf2);
      free(shared_buf3);
      shared_buf = NULL;
   }
   memory_control = values[(int)btn];
}

/*--------------------------------------------------------------*/

void CB_stretchLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetMonoLUT(gbl_iw, lut);
}

void CB_redLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, lut, NULL, NULL);
}

void CB_greenLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, NULL, lut, NULL);
}

void CB_blueLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, NULL, NULL, lut);
}

SetLut(lut, n)
int lut[256];
int n;
{
   int i;

   for (i=0; i<256; i++) {
      switch (n) {
         case 0:
            lut[i] = 255-i;
            break;
         case 1:
            lut[i] = (i/2+64) % 256;
            break;
         case 2:
            lut[i] = (i+128) % 256;
            break;
         case 3:
            lut[i] = (i+64) % 256;
            break;
         case 4:
            lut[i] = i;
            break;
         case 5:
            lut[i] = 0;
            break;
      }
   }
}

void CB_stretchLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetMonoLUT16(gbl_iw, lut, 65536);
}

void CB_redLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, lut, NULL, NULL, 65536);
}

void CB_greenLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, NULL, lut, NULL, 65536);
}

void CB_blueLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, NULL, NULL, lut, 65536);
}

SetLut16(lut, n)
int lut[65536];
int n;
{
   int i;

   for (i=0; i<65536; i++) {
      switch (n) {
         case 0:
            lut[i] = i;
            break;
         case 1:
            lut[i] = (i/2+16384) % 65536;
            break;
         case 2:
            lut[i] = (i%1024) * 64;
            break;
         case 3:
            lut[i] = (i+16384) % 65536;
            break;
         case 4:
            lut[i] = 65535-i;
            break;
         case 5:
            lut[i] = (i<4096)?i:0;
            break;
         case 6:
            lut[i] = 0;
            break;
      }
   }
}

/************************************************************************/
/* Graphics callbacks							*/
/************************************************************************/

void new_gc()
{
   char buf[20];

   if (gbl_gr.rubber_mode) {
      gbl_gr.rubber_gc = XvicImageCreateRubberGC(gbl_iw, gbl_gr.gc_mask,
				&gbl_gr.gc_values);
      if (gbl_gr.new_gc) {
         XvicImageChangeGC(gbl_iw, gbl_gr.new_gc,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
         gbl_gr.gc = gbl_gr.new_gc;
      }
      else
         gbl_gr.gc = gbl_gr.rubber_gc;
   }
   else {
      if (gbl_gr.new_gc) {
         XvicImageChangeGC(gbl_iw, gbl_gr.new_gc,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
         gbl_gr.gc = gbl_gr.new_gc;
      }
      else {
         gbl_gr.gc = XvicImageCreateGC(gbl_iw,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
      }
      gbl_gr.rubber_gc = XvicImageCreateRubberGC(gbl_iw, gbl_gr.gc_mask,
				&gbl_gr.gc_values);
   }
   sprintf(buf, "%d", gbl_gr.gc);
   XtVaSetValues(gbl_gr.last_gc_widget, XmNvalue, buf, NULL);
}

void CBgr_color(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XColor xcolor;

   XtVaGetValues(w, XmNvalue, &string, NULL);
   XParseColor(XtDisplay(w), DefaultColormapOfScreen(XtScreen(w)),
		string, &xcolor);
   gbl_gr.color = XvicImageGetGrColor(gbl_iw, &xcolor);
   XtFree(string);
}

void CBgr_bgcolor(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XColor xcolor;

   XtVaGetValues(w, XmNvalue, &string, NULL);
   XParseColor(XtDisplay(w), DefaultColormapOfScreen(XtScreen(w)),
		string, &xcolor);
   gbl_gr.bgcolor = XvicImageGetGrColor(gbl_iw, &xcolor);
   XtFree(string);
}

void CBgr_text(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   /* Does nothing.  Function simply needed so create_text() will make	*/
   /* it editable.							*/
}

void CBgr_textjustify(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {XvicJUST_LEFT, XvicJUST_CENTER, XvicJUST_RIGHT};
   gbl_gr.justify = values[(int)btn];
}

void CBgr_bitmap(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   if (XReadBitmapFile(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		string, &gbl_gr.bitmap_width, &gbl_gr.bitmap_height,
		&gbl_gr.bitmap, &gbl_gr.bitmap_hot_x, &gbl_gr.bitmap_hot_y)
	!= BitmapSuccess) {
      gbl_gr.bitmap = NULL;
   }
   else {
      if (gbl_gr.bitmap_hot_x == -1)
         gbl_gr.bitmap_hot_x = 0;
      if (gbl_gr.bitmap_hot_y == -1)
         gbl_gr.bitmap_hot_y = 0;
   }
   XtFree(string);
}

void CBgr_shape(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static Shape values[] = {MoveExisting, EraseExisting,
	FillArc, FillArcs, FillPolygon, FillRectangle, FillRectangles,
	Arc, Arcs, Bitmap, ImageString, Line, Lines, Point, Points,
	Rectangle, Rectangles, Segments, DrawString};

   if (values[(int)btn] == EraseExisting) {
      XvicImageEraseObject(gbl_iw, gbl_gr.id);
      set_option_default(gbl_gr.shape_widget, (int)gbl_gr.shape_int);
   }
   else {
      gbl_gr.shape = values[(int)btn];
      gbl_gr.shape_int = (int)btn;
   }
}

void CBgr_id(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.id = atoi(string);
   XtFree(string);
}

void CBgr_gc(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.new_gc = atoi(string);
   XtFree(string);
}

void CBgr_arcmode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {ArcPieSlice, ArcChord};
   gbl_gr.gc_values.arc_mode = values[(int)btn];
   gbl_gr.gc_mask |= GCArcMode;
   new_gc();
}

void CBgr_capstyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {CapNotLast, CapButt, CapRound, CapProjecting};
   gbl_gr.gc_values.cap_style = values[(int)btn];
   gbl_gr.gc_mask |= GCCapStyle;
   new_gc();
}

void CBgr_dashes(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.dashes = (char) atoi(string);
   gbl_gr.gc_mask |= GCDashList;
   new_gc();
   XtFree(string);
}

void CBgr_dashoffset(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.dash_offset = atoi(string);
   gbl_gr.gc_mask |= GCDashOffset;
   new_gc();
   XtFree(string);
}

void CBgr_fillrule(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {EvenOddRule, WindingRule};
   gbl_gr.gc_values.fill_rule = values[(int)btn];
   gbl_gr.gc_mask |= GCFillRule;
   new_gc();
}

void CBgr_font(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   char **names;
   int count;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   names = XListFonts(XtDisplay(w), string, 1, &count);
   XFreeFontNames(names);
   if (count > 0) {
      gbl_gr.gc_values.font = XLoadFont(XtDisplay(w),  string);
      gbl_gr.gc_mask |= GCFont;
      new_gc();
   }
   XtFree(string);
}

void CBgr_joinstyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {JoinMiter, JoinRound, JoinBevel};
   gbl_gr.gc_values.join_style = values[(int)btn];
   gbl_gr.gc_mask |= GCJoinStyle;
   new_gc();
}

void CBgr_linestyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {LineSolid, LineOnOffDash, LineDoubleDash};
   gbl_gr.gc_values.line_style = values[(int)btn];
   gbl_gr.gc_mask |= GCLineStyle;
   new_gc();
}

void CBgr_linewidth(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.line_width = atoi(string);
   gbl_gr.gc_mask |= GCLineWidth;
   new_gc();
   XtFree(string);
}

void CBgr_rubbermode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {False, True};
   gbl_gr.rubber_mode = values[(int)btn];
   new_gc();
}

/************************************************************************/

main(argc,argv)
int argc;
char **argv;
{
   XtAppContext app_context;
   Widget topLevel, form, iw, rc, shell;
   Dimension width, height;
   Arg args[20];
   int n;
   unsigned char data_type;

   topLevel = XtVaAppInitialize(
	&app_context,
	"Test_IW",
	NULL, 0,
	&argc, argv,
	NULL, NULL);

   XSynchronize(XtDisplay(topLevel), True);	/*!!!!*/

   form = XtVaCreateManagedWidget("form", xmFormWidgetClass, topLevel, NULL);

   n = 0;
   XtSetArg(args[n], XmNtopAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNbottomAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNleftAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNrightAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XvicNimageWidth, (XtArgVal)1500); n++;
   XtSetArg(args[n], XvicNimageHeight, (XtArgVal)1500); n++;
   XtSetArg(args[n], XvicNtrackFloatingCursor, (XtArgVal)False); n++;

   iw = XvicCreateImage(form, "image", args, n);
   XtManageChild(iw);

   gbl_iw = iw;
   XtAddCallback(iw, XvicNresizeCallback, resize, NULL);
   XtAddCallback(iw, XvicNexposeCallback, expose, NULL);
   XtAddCallback(iw, XvicNpanCallback, pan, NULL);
   XtAddCallback(iw, XvicNinputCallback, input, NULL);
   XtAddCallback(iw, XvicNcursorCallback, cursor, NULL);
   XtAddCallback(iw, XvicNworkProcActiveCallback, work_proc_active, NULL);

   shell = XtVaCreatePopupShell("control", topLevelShellWidgetClass, topLevel,
	XmNwidth, 720, NULL);
   rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, shell,
	XmNwidth, 720,
	XmNorientation, XmVERTICAL,
	XmNpacking, XmPACK_TIGHT,
	XmNadjustLast, False,
	XmNleftAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	XmNbottomAttachment, XmATTACH_FORM,
	NULL);

   create_menus(rc, iw);
   XtPopup(shell, XtGrabNone);

   shell = XtVaCreatePopupShell("graphics", topLevelShellWidgetClass, topLevel,
	XmNwidth, 225, NULL);
   rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, shell,
	XmNwidth, 225,
	XmNorientation, XmVERTICAL,
	XmNpacking, XmPACK_TIGHT,
	NULL);

   create_graphics_menus(rc, iw);
   XtPopup(shell, XtGrabNone);

   reset_defaults(iw);

   XtVaGetValues(iw, XvicNdataType, &data_type, NULL);
   set_data_range(data_type);

   XtRealizeWidget(topLevel);

   XtVaGetValues(iw, XvicNviewWidth, &width, XvicNviewHeight, &height, NULL);
   DPR(("view width=%d, height=%d\n", width, height));
   XtVaGetValues(iw, XmNwidth, &width, XmNheight, &height, NULL);
   DPR(("core width=%d, height=%d\n", width, height));

   XtAppMainLoop(app_context);
}


/****************************************************************/

Widget create_text(parent, label, cb, data)
Widget parent;
char *label;
XtCallbackProc cb;
XtPointer data;
{
   XmString str;
   Widget rc, lbl, txt;

   str = XmStringCreateSimple(label);
   rc = XtVaCreateWidget("rc", xmRowColumnWidgetClass, parent,
	XmNorientation, XmHORIZONTAL,
	NULL);
   lbl = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc,
	XmNlabelString, str,
	NULL);
   txt = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, rc,
	XmNcolumns, 6, XmNeditable, True, XmNmaxLength, 7,
	NULL);

   if (cb == CB_string || cb == NULL || cb == CBgr_color || cb == CBgr_bgcolor
		|| cb == CBgr_font || cb == CBgr_text || cb == CBgr_bitmap)
      XtVaSetValues(txt, XmNcolumns, 20, XmNmaxLength, 150, NULL);
   if (cb == CB_data_min || cb == CB_data_max || cb == CB_data_inc ||
		cb == CB_double)
      XtVaSetValues(txt, XmNcolumns, 12, XmNmaxLength, 20, NULL);

   if (cb == NULL)
      XtVaSetValues(txt, XmNeditable, False, NULL);
   else
      XtAddCallback(txt, XmNactivateCallback, cb, data);

   XtManageChild(rc);
   XmStringFree(str);
   return txt;
}

/****************************************************************/

Widget create_bool(parent, label, cb)
Widget parent;
char *label;
XtCallbackProc cb;
{
   XmString s1, s2, s3;
   Widget w;

   s1 = XmStringCreateSimple(label);
   s2 = XmStringCreateSimple("True");
   s3 = XmStringCreateSimple("False");
   w = XmVaCreateSimpleOptionMenu(parent, "bool", s1, 0, 0, cb,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w);

   return w;
}

/****************************************************************/

create_menus(rc, iw)
Widget rc, iw;
{
   XmString s1, s2, s3, s4, s5, s6, s7, s8, s9;
   Widget w;

/*--------------------------------------------------------------*/
/* BasicImage resources						*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Image Mode");
   s2 = XmStringCreateSimple("Color");
   s3 = XmStringCreateSimple("BW");
   w_imageMode = XmVaCreateSimpleOptionMenu(rc, "imageMode", s1, 0, 0, CB_imageMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_imageMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Data Type");
   s2 = XmStringCreateSimple("Byte");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Unsigned Half");
   s5 = XmStringCreateSimple("Full");
   s6 = XmStringCreateSimple("Unsigned Full");
   s7 = XmStringCreateSimple("Real");
   s8 = XmStringCreateSimple("Double");
   w_dataType = XmVaCreateSimpleOptionMenu(rc, "dataType", s1, 0, 0, CB_dataType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s8);
   XtManageChild(w_dataType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_ditherMode = XmVaCreateSimpleOptionMenu(rc, "ditherMode", s1, 0, 0, CB_ditherMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_ditherMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Lut Type");
   s2 = XmStringCreateSimple("Stretch");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Pseudo");
   s5 = XmStringCreateSimple("Pseudo Only");
   w_lutType = XmVaCreateSimpleOptionMenu(rc, "lutType", s1, 0, 0, CB_lutType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_lutType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Lut16 Type");
   s2 = XmStringCreateSimple("Stretch");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Pseudo");
   s5 = XmStringCreateSimple("Pseudo Only");
   w_lut16Type = XmVaCreateSimpleOptionMenu(rc, "lut16Type", s1, 0, 0, CB_lut16Type,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_lut16Type);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_stretchPolicy = XmVaCreateSimpleOptionMenu(rc, "stretchPolicy", s1, 0, 0, CB_stretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_stretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_colormapPolicy = XmVaCreateSimpleOptionMenu(rc, "colormapPolicy", s1, 0, 0, CB_colormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_colormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_visualType = XmVaCreateSimpleOptionMenu(rc, "visualType", s1, 0, 0, CB_visualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_visualType);

/*--------------------------------------------------------------*/
   w_enableDirectColor = create_bool(rc, "Enable Direct Color", CB_enableDirectColor);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Work Proc Policy");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Read");
   s4 = XmStringCreateSimple("All");
   w_workProcPolicy = XmVaCreateSimpleOptionMenu(rc, "workProcPolicy", s1, 0, 0, CB_workProcPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_workProcPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Data Save Policy");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Ximage");
   s5 = XmStringCreateSimple("Pixmap");
   w_dataSavePolicy = XmVaCreateSimpleOptionMenu(rc, "dataSavePolicy", s1, 0, 0, CB_dataSavePolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_dataSavePolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Constrain Pan");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("X Only");
   s4 = XmStringCreateSimple("Y Only");
   s5 = XmStringCreateSimple("Both");
   w_constrainPan = XmVaCreateSimpleOptionMenu(rc, "constrainPan", s1, 0, 0, CB_constrainPan,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_constrainPan);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_bwDither = XmVaCreateSimpleOptionMenu(rc, "bwDither", s1, 0, 0, CB_bwDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_bwDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_bwStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "bwStretchPolicy", s1, 0, 0, CB_bwStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_bwStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_bwColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "bwColormapPolicy", s1, 0, 0, CB_bwColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_bwColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_bwVisualType = XmVaCreateSimpleOptionMenu(rc, "bwVisualType", s1, 0, 0, CB_bwVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_bwVisualType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_colorDither = XmVaCreateSimpleOptionMenu(rc, "colorDither", s1, 0, 0, CB_colorDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_colorDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_colorStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "colorStretchPolicy", s1, 0, 0, CB_colorStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_colorStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_colorColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "colorColormapPolicy", s1, 0, 0, CB_colorColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_colorColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_colorVisualType = XmVaCreateSimpleOptionMenu(rc, "colorVisualType", s1, 0, 0, CB_colorVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_colorVisualType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_pseudoDither = XmVaCreateSimpleOptionMenu(rc, "pseudoDither", s1, 0, 0, CB_pseudoDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_pseudoDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_pseudoStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "pseudoStretchPolicy", s1, 0, 0, CB_pseudoStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_pseudoStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_pseudoColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "pseudoColormapPolicy", s1, 0, 0, CB_pseudoColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_pseudoColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_pseudoVisualType = XmVaCreateSimpleOptionMenu(rc, "pseudoVisualType", s1, 0, 0, CB_pseudoVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_pseudoVisualType);

/*--------------------------------------------------------------*/
   w_xPan = create_text(rc, "X Pan", CB_int, XvicNxPan);
   w_yPan = create_text(rc, "Y Pan", CB_int, XvicNyPan);
   w_xSubpixelPan = create_text(rc, "X Subpixel Pan", CB_int, XvicNxSubpixelPan);
   w_ySubpixelPan = create_text(rc, "Y Subpixel Pan", CB_int, XvicNySubpixelPan);
   w_xZoomIn = create_text(rc, "X Zoom In", CB_int, XvicNxZoomIn);
   w_xZoomOut = create_text(rc, "X Zoom Out", CB_int, XvicNxZoomOut);
   w_yZoomIn = create_text(rc, "Y Zoom In", CB_int, XvicNyZoomIn);
   w_yZoomOut = create_text(rc, "Y Zoom Out", CB_int, XvicNyZoomOut);
   w_maximumMemory = create_text(rc, "Maximum Memory", CB_int, XvicNmaximumMemory);
   w_grayLevels = create_text(rc, "Gray Levels", CB_int, XvicNgrayLevels);
   w_redLevels = create_text(rc, "Red Levels", CB_int, XvicNredLevels);
   w_greenLevels = create_text(rc, "Green Levels", CB_int, XvicNgreenLevels);
   w_blueLevels = create_text(rc, "Blue Levels", CB_int, XvicNblueLevels);
   w_rawDataMin = create_text(rc, "Raw Data Min", CB_double, XvicNrawDataMin);
   w_rawDataMax = create_text(rc, "Raw Data Max", CB_double, XvicNrawDataMax);
   w_scaledDataMax = create_text(rc, "Scaled Data Max", CB_int, XvicNscaledDataMax);
   w_outputDataMax = create_text(rc, "Output Data Max", CB_int, XvicNoutputDataMax);
   w_imageWidth = create_text(rc, "Image Width", CB_int_sh, XvicNimageWidth);
   w_imageHeight = create_text(rc, "Image Height", CB_int_sh, XvicNimageHeight);
   w_tileWidth = create_text(rc, "Tile Width", CB_int, XvicNtileWidth);
   w_tileHeight = create_text(rc, "Tile Height", CB_int, XvicNtileHeight);
   w_viewWidth = create_text(rc, "View Width", CB_Dim, XvicNviewWidth);
   w_viewHeight = create_text(rc, "View Height", CB_Dim, XvicNviewHeight);
   w_width = create_text(rc, "Core Width", CB_Dim, XmNwidth);
   w_height = create_text(rc, "Core Height", CB_Dim, XmNheight);
   w_xPreSubpixelPan = create_text(rc,"X Pre Subpixel Pan",CB_int,XvicNxPreSubpixelPan);
   w_yPreSubpixelPan = create_text(rc,"Y Pre Subpixel Pan",CB_int,XvicNyPreSubpixelPan);
   w_xPreZoomIn = create_text(rc, "X Pre Zoom In", CB_int_sh, XvicNxPreZoomIn);
   w_xPreZoomOut = create_text(rc, "X Pre Zoom Out", CB_int_sh, XvicNxPreZoomOut);
   w_yPreZoomIn = create_text(rc, "Y Pre Zoom In", CB_int_sh, XvicNyPreZoomIn);
   w_yPreZoomOut = create_text(rc, "Y Pre Zoom Out", CB_int_sh, XvicNyPreZoomOut);

/*--------------------------------------------------------------*/
/* Image resources						*/
/*--------------------------------------------------------------*/

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

   s1 = XmStringCreateSimple("Cursor Mode");
   s2 = XmStringCreateSimple("Floating");
   s3 = XmStringCreateSimple("Planted");
   w_cursorMode = XmVaCreateSimpleOptionMenu(rc, "cursorMode", s1, 0, 0, CB_cursorMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_cursorMode);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Scrollbar Disp Pol");
   s2 = XmStringCreateSimple("Static");
   s3 = XmStringCreateSimple("As Needed");
   s4 = XmStringCreateSimple("Never");
   w_scrollBarDisplayPolicy = XmVaCreateSimpleOptionMenu(rc, "scrollBarDisplayPolicy", s1, 0, 0
, CB_scrollBarDisplayPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_scrollBarDisplayPolicy);

   w_cursorX = create_text(rc, "Cursor X", CB_int, XvicNcursorX);
   w_cursorY = create_text(rc, "Cursor Y", CB_int, XvicNcursorY);
   w_cursorXfp = create_text(rc, "Cursor X fp", CB_double, XvicNcursorXfp);
   w_cursorYfp = create_text(rc, "Cursor Y fp", CB_double, XvicNcursorYfp);
   w_cursorForeground = create_text(rc, "Cursor Foreground", CB_string, XvicNcursorForeground);
   w_cursorBackground = create_text(rc, "Cursor Background", CB_string, XvicNcursorBackground);
   w_cursor = create_text(rc, "Cursor Shape", CB_string, XvicNcursor);
   w_trackFloatingCursor = create_bool(rc, "Track Floating Cursor", CB_trackFloatingCursor);

/*--------------------------------------------------------------*/

   w_input = create_text(rc, "Input Arg", NULL, NULL);

/*--------------------------------------------------------------*/
/* Test program controls					*/
/*--------------------------------------------------------------*/

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

   w_dataMin = create_text(rc, "Data Minimum", CB_data_min, NULL);
   w_dataMax = create_text(rc, "Data Maximum", CB_data_max, NULL);
   w_dataInc = create_text(rc, "Data Increment", CB_data_inc, NULL);

   s1 = XmStringCreateSimple("Memory Control");
   s2 = XmStringCreateSimple("Applic");
   s3 = XmStringCreateSimple("Widget");
   s4 = XmStringCreateSimple("Shared");
   w = XmVaCreateSimpleOptionMenu(rc, "memory_control", s1, 0, 0, CB_mem_ctl,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   memory_control = XvicMEMORY_APPLIC;
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("BW Stretch");
   s2 = XmStringCreateSimple("Inverse 255-0");
   s3 = XmStringCreateSimple("Linear 64-196");
   s4 = XmStringCreateSimple("Wrap 128-127");
   s5 = XmStringCreateSimple("Wrap 64-63");
   s6 = XmStringCreateSimple("Linear 0-255");
   s7 = XmStringCreateSimple("Black");
   w = XmVaCreateSimpleOptionMenu(rc, "bw_stretch", s1, 0, 4, CB_stretchLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_stretchLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Red Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "red_stretch", s1, 0, 4, CB_redLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_redLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Green Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "green_stretch", s1, 0, 4, CB_greenLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_greenLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Blue Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "blue_stretch", s1, 0, 4, CB_blueLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_blueLUT(w, 4, NULL);
   XmStringFree(s1);

   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("BW Stretch 16");
   s2 = XmStringCreateSimple("Linear 0-64K");
   s3 = XmStringCreateSimple("Linear 16K-48K");
   s4 = XmStringCreateSimple("Ramp 0-1024 rpt");
   s5 = XmStringCreateSimple("Wrap 16K");
   s6 = XmStringCreateSimple("Inverse 64K-0");
   s7 = XmStringCreateSimple("12-bit 0-4K");
   s8 = XmStringCreateSimple("Black");
   w = XmVaCreateSimpleOptionMenu(rc, "bw_stretch16", s1, 0, 0, CB_stretchLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Red Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "red_stretch16", s1, 0, 0, CB_redLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Green Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "green_stretch16", s1, 0, 0,CB_greenLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Blue Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "blue_stretch16", s1, 0, 0, CB_blueLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s7);

/*!!!! VVVV !!!!*/
   w=XtVaCreateManagedWidget("Destroy Widget",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(w, XmNactivateCallback, DestroyCallback, (XtPointer)NULL);
/*!!!! ^^^^ !!!!*/

   w=XtVaCreateManagedWidget("Update Values",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(w, XmNactivateCallback, UpdateCallback, (XtPointer)NULL);

}

/****************************************************************/

create_graphics_menus(rc, iw)
Widget rc, iw;
{
   XmString s1, s2, s3, s4, s5, s6, s7, s8, s9;
   XmString s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20;
   Widget w;

/* Init items in the global gr structure */

   gbl_gr.gc = 0;
   gbl_gr.rubber_gc = 0;
   gbl_gr.id = 0;
   gbl_gr.rubber_id = 0;
   gbl_gr.shape = Line;
   gbl_gr.shape_int = 11;
   gbl_gr.gc_mask = 0;
   gbl_gr.bitmap = NULL;

/*--------------------------------------------------------------*/
/* Colors for graphics						*/
/*--------------------------------------------------------------*/

   gw_color = create_text(rc, "Color", CBgr_color, NULL);
   XtVaSetValues(gw_color, XmNvalue, "white", NULL);
   CBgr_color(gw_color, NULL, NULL);

   gw_bgColor = create_text(rc, "BG Color", CBgr_bgcolor, NULL);
   XtVaSetValues(gw_bgColor, XmNvalue, "black", NULL);
   CBgr_bgcolor(gw_bgColor, NULL, NULL);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* String for text						*/
/*--------------------------------------------------------------*/

   gw_text = create_text(rc, "Text", CBgr_text, NULL);
   gbl_gr.text_widget = gw_text;

   s1 = XmStringCreateSimple("Text Justify");
   s2 = XmStringCreateSimple("Left");
   s3 = XmStringCreateSimple("Center");
   s4 = XmStringCreateSimple("Right");
   gw_textJustify = XmVaCreateSimpleOptionMenu(rc, "textJustify", s1, 0, 0, CBgr_textjustify,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_textJustify);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* Bitmap file							*/
/*--------------------------------------------------------------*/

   gw_bitmap = create_text(rc, "Bitmap File", CBgr_bitmap, NULL);

/*--------------------------------------------------------------*/
/* Erase overlay						*/
/*--------------------------------------------------------------*/

   gw_erase=XtVaCreateManagedWidget("Erase Overlay",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(gw_erase, XmNactivateCallback, EraseOverlayCallback, (XtPointer)NULL);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* Shape for graphics						*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Shape");
   s2 = XmStringCreateSimple("Move Existing");
   s3 = XmStringCreateSimple("Erase Existing");
   s4 = XmStringCreateSimple("Filled Arc");
   s5 = XmStringCreateSimple("Filled Arcs");
   s6 = XmStringCreateSimple("Filled Polygon");
   s7 = XmStringCreateSimple("Filled Rectangle");
   s8 = XmStringCreateSimple("Filled Rectangles");
   s9 = XmStringCreateSimple("Arc");
   s10 = XmStringCreateSimple("Arcs");
   s11 = XmStringCreateSimple("Bitmap");
   s12 = XmStringCreateSimple("Image String");
   s13 = XmStringCreateSimple("Line");
   s14 = XmStringCreateSimple("Lines");
   s15 = XmStringCreateSimple("Point");
   s16 = XmStringCreateSimple("Points");
   s17 = XmStringCreateSimple("Rectangle");
   s18 = XmStringCreateSimple("Rectangles");
   s19 = XmStringCreateSimple("Segments");
   s20 = XmStringCreateSimple("String");
   gw_shape = XmVaCreateSimpleOptionMenu(rc, "grShape", s1, 0, 11, CBgr_shape,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		XmVaPUSHBUTTON, s9, 0, 0, 0,
		XmVaPUSHBUTTON, s10, 0, 0, 0,
		XmVaPUSHBUTTON, s11, 0, 0, 0,
		XmVaPUSHBUTTON, s12, 0, 0, 0,
		XmVaPUSHBUTTON, s13, 0, 0, 0,
		XmVaPUSHBUTTON, s14, 0, 0, 0,
		XmVaPUSHBUTTON, s15, 0, 0, 0,
		XmVaPUSHBUTTON, s16, 0, 0, 0,
		XmVaPUSHBUTTON, s17, 0, 0, 0,
		XmVaPUSHBUTTON, s18, 0, 0, 0,
		XmVaPUSHBUTTON, s19, 0, 0, 0,
		XmVaPUSHBUTTON, s20, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s8);
   XmStringFree(s9);
   XmStringFree(s10);
   XmStringFree(s11);
   XmStringFree(s12);
   XmStringFree(s13);
   XmStringFree(s14);
   XmStringFree(s15);
   XmStringFree(s16);
   XmStringFree(s17);
   XmStringFree(s18);
   XmStringFree(s19);
   XmStringFree(s20);
   XtManageChild(gw_shape);
   gbl_gr.shape_widget = gw_shape;

   gw_id = create_text(rc, "Object ID", CBgr_id, NULL);
   gbl_gr.id_widget = gw_id;
   XtVaSetValues(gw_id, XmNvalue, "0", NULL);
   gw_lastId = create_text(rc, "Last ID", NULL, NULL);
   gbl_gr.last_id_widget = gw_lastId;

   gw_gc = create_text(rc, "GC ID", CBgr_gc, NULL);
   gbl_gr.gc_widget = gw_gc;
   XtVaSetValues(gw_gc, XmNvalue, "0", NULL);
   gw_lastGc = create_text(rc, "Last GC", NULL, NULL);
   gbl_gr.last_gc_widget = gw_lastGc;

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* GC stuff							*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Arc Mode");
   s2 = XmStringCreateSimple("ArcPieSlice");
   s3 = XmStringCreateSimple("ArcChord");
   gw_arcMode = XmVaCreateSimpleOptionMenu(rc, "arcMode", s1, 0, 0, CBgr_arcmode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_arcMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Cap Style");
   s2 = XmStringCreateSimple("CapNotLast");
   s3 = XmStringCreateSimple("CapButt");
   s4 = XmStringCreateSimple("CapRound");
   s5 = XmStringCreateSimple("CapProjecting");
   gw_capStyle = XmVaCreateSimpleOptionMenu(rc, "capStyle", s1, 0, 1, CBgr_capstyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(gw_capStyle);

/*--------------------------------------------------------------*/
   gw_dashes = create_text(rc, "Dashes", CBgr_dashes, NULL);
   gw_dashOffset = create_text(rc, "Dash Offset", CBgr_dashoffset, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Fill Rule");
   s2 = XmStringCreateSimple("EvenOddRule");
   s3 = XmStringCreateSimple("WindingRule");
   gw_fillRule = XmVaCreateSimpleOptionMenu(rc, "fillRule", s1, 0, 0, CBgr_fillrule,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_fillRule);

/*--------------------------------------------------------------*/
   gw_font = create_text(rc, "Font", CBgr_font, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Join Style");
   s2 = XmStringCreateSimple("JoinMiter");
   s3 = XmStringCreateSimple("JoinRound");
   s4 = XmStringCreateSimple("JoinBevel");
   gw_joinStyle = XmVaCreateSimpleOptionMenu(rc, "joinStyle", s1, 0, 0, CBgr_joinstyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_joinStyle);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Line Style");
   s2 = XmStringCreateSimple("LineSolid");
   s3 = XmStringCreateSimple("LineOnOffDash");
   s4 = XmStringCreateSimple("LineDoubleDash");
   gw_lineStyle = XmVaCreateSimpleOptionMenu(rc, "lineStyle", s1, 0, 0, CBgr_linestyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_lineStyle);

/*--------------------------------------------------------------*/
   gw_lineWidth = create_text(rc, "Line Width", CBgr_linewidth, NULL);
   XtVaSetValues(gw_lineWidth, XmNvalue, "1", NULL);
   CBgr_linewidth(gw_lineWidth, NULL, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Draw Mode");
   s2 = XmStringCreateSimple("Normal");
   s3 = XmStringCreateSimple("RubberBand");
   gw_rubberMode = XmVaCreateSimpleOptionMenu(rc, "rubberMode", s1, 0, 0, CBgr_rubbermode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_rubberMode);

}

/****************************************************************/

set_int_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   int value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0d", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_dim_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   Dimension value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0d", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_option_default(w, n)
Widget w;
int n;
{
   WidgetList buttons;
   Cardinal num_buttons;
   int count, i;
   Widget menu;

   XtVaGetValues(XmOptionButtonGadget(w), XmNsubMenuId, &menu, NULL);
   XtVaGetValues(menu, XmNchildren, &buttons, XmNnumChildren, &num_buttons, NULL);
   count = 0;
   for (i=0; i<num_buttons; i++) {
      if (XmIsPushButtonGadget(buttons[i]) || XmIsPushButton(buttons[i])) {
         if (count == n)
            break;
         count++;
      }
   }
   if (i < num_buttons)
      XtVaSetValues(w, XmNmenuHistory, buttons[i], NULL);
}

/****************************************************************/

set_bool_default(w, res)
Widget w;
char *res;
{
   Boolean bvalue;
   int def;

   XtVaGetValues(gbl_iw, res, &bvalue, NULL);
   if (bvalue)
      def = 0;
   else
      def = 1;
   set_option_default(w, def);
}

/****************************************************************/

set_string_default(w, res)
Widget w;
char *res;
{
   char *buf;
   char *old_buf;

   XtVaGetValues(gbl_iw, res, &buf, NULL);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (buf == NULL)
      if (strlen(old_buf) != 0)
         XtVaSetValues(w, XmNvalue, "", NULL);
   else if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_dbl_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   double value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0.10lf", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

reset_defaults(iw)
Widget iw;
{
   unsigned char value, def;
   Boolean bvalue;

   XtVaGetValues(iw, XvicNimageMode, &value, NULL);
   switch (value) {
      case XvicCOLOR:		def = 0; break;
      case XvicBW:		def = 1; break;
   }
   set_option_default(w_imageMode, def);

   XtVaGetValues(iw, XvicNdataType, &value, NULL);
   switch (value) {
      case XvicBYTE:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicUHALF:		def = 2; break;
      case XvicFULL:		def = 3; break;
      case XvicUFULL:		def = 4; break;
      case XvicREAL:		def = 5; break;
      case XvicDOUBLE:		def = 6; break;
   }
   set_option_default(w_dataType, def);

   XtVaGetValues(iw, XvicNditherMode, &value, NULL);
   XtVaGetValues(iw, XvicNditherMode, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_ditherMode, def);

   XtVaGetValues(iw, XvicNlutType, &value, NULL);
   switch (value) {
      case XvicSTRETCH:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicPSEUDO:		def = 2; break;
      case XvicPSEUDO_ONLY:	def = 3; break;
   }
   set_option_default(w_lutType, def);

   XtVaGetValues(iw, XvicNlut16Type, &value, NULL);
   switch (value) {
      case XvicSTRETCH:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicPSEUDO:		def = 2; break;
      case XvicPSEUDO_ONLY:	def = 3; break;
   }
   set_option_default(w_lut16Type, def);

   XtVaGetValues(iw, XvicNstretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_stretchPolicy, def);

   XtVaGetValues(iw, XvicNcolormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_colormapPolicy, def);

   XtVaGetValues(iw, XvicNvisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_visualType, def);

   set_bool_default(w_enableDirectColor, XvicNenableDirectColor);

   XtVaGetValues(iw, XvicNworkProcPolicy, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicREAD:		def = 1; break;
      case XvicALL:		def = 2; break;
   }
   set_option_default(w_workProcPolicy, def);

   XtVaGetValues(iw, XvicNdataSavePolicy, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicXIMAGE:		def = 2; break;
      case XvicPIXMAP:		def = 3; break;
   }
   set_option_default(w_dataSavePolicy, def);

   XtVaGetValues(iw, XvicNconstrainPan, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicX_ONLY:		def = 1; break;
      case XvicY_ONLY:		def = 2; break;
      case XvicBOTH:		def = 3; break;
   }
   set_option_default(w_constrainPan, def);

   XtVaGetValues(iw, XvicNbwDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_bwDither, def);

   XtVaGetValues(iw, XvicNbwStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_bwStretchPolicy, def);

   XtVaGetValues(iw, XvicNbwColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_bwColormapPolicy, def);

   XtVaGetValues(iw, XvicNbwVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_bwVisualType, def);

   XtVaGetValues(iw, XvicNcolorDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_colorDither, def);

   XtVaGetValues(iw, XvicNcolorStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_colorStretchPolicy, def);

   XtVaGetValues(iw, XvicNcolorColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_colorColormapPolicy, def);

   XtVaGetValues(iw, XvicNcolorVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_colorVisualType, def);

   XtVaGetValues(iw, XvicNpseudoDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_pseudoDither, def);

   XtVaGetValues(iw, XvicNpseudoStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_pseudoStretchPolicy, def);

   XtVaGetValues(iw, XvicNpseudoColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_pseudoColormapPolicy, def);

   XtVaGetValues(iw, XvicNpseudoVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_pseudoVisualType, def);

   set_int_default(w_xPan, XvicNxPan);
   set_int_default(w_yPan, XvicNyPan);
   set_int_default(w_xSubpixelPan, XvicNxSubpixelPan);
   set_int_default(w_ySubpixelPan, XvicNySubpixelPan);
   set_int_default(w_xZoomIn, XvicNxZoomIn);
   set_int_default(w_xZoomOut, XvicNxZoomOut);
   set_int_default(w_yZoomIn, XvicNyZoomIn);
   set_int_default(w_yZoomOut, XvicNyZoomOut);
   set_int_default(w_maximumMemory, XvicNmaximumMemory);
   set_int_default(w_grayLevels, XvicNgrayLevels);
   set_int_default(w_redLevels, XvicNredLevels);
   set_int_default(w_greenLevels, XvicNgreenLevels);
   set_int_default(w_blueLevels, XvicNblueLevels);
   set_dbl_default(w_rawDataMin, XvicNrawDataMin);
   set_dbl_default(w_rawDataMax, XvicNrawDataMax);
   set_int_default(w_scaledDataMax, XvicNscaledDataMax);
   set_int_default(w_outputDataMax, XvicNoutputDataMax);
   set_int_default(w_imageWidth, XvicNimageWidth);
   set_int_default(w_imageHeight, XvicNimageHeight);
   set_int_default(w_tileWidth, XvicNtileWidth);
   set_int_default(w_tileHeight, XvicNtileHeight);
   set_dim_default(w_viewWidth, XvicNviewWidth);
   set_dim_default(w_viewHeight, XvicNviewHeight);
   set_dim_default(w_width, XmNwidth);
   set_dim_default(w_height, XmNheight);
   set_int_default(w_xPreSubpixelPan, XvicNxPreSubpixelPan);
   set_int_default(w_yPreSubpixelPan, XvicNyPreSubpixelPan);
   set_int_default(w_xPreZoomIn, XvicNxPreZoomIn);
   set_int_default(w_xPreZoomOut, XvicNxPreZoomOut);
   set_int_default(w_yPreZoomIn, XvicNyPreZoomIn);
   set_int_default(w_yPreZoomOut, XvicNyPreZoomOut);

   XtVaGetValues(iw, XvicNcursorMode, &value, NULL);
   switch (value) {
      case XvicFLOATING:	def = 0; break;
      case XvicPLANTED:		def = 1; break;
   }
   set_option_default(w_cursorMode, def);

   XtVaGetValues(iw, XvicNscrollBarDisplayPolicy, &value, NULL);
   switch (value) {
      case XvicSTATIC:		def = 0; break;
      case XvicAS_NEEDED:	def = 1; break;
      case XvicNEVER:		def = 2; break;
   }
   set_option_default(w_scrollBarDisplayPolicy, def);

   set_int_default(w_cursorX, XvicNcursorX);
   set_int_default(w_cursorY, XvicNcursorY);
   set_dbl_default(w_cursorXfp, XvicNcursorXfp);
   set_dbl_default(w_cursorYfp, XvicNcursorYfp);
   set_string_default(w_cursorForeground, XvicNcursorForeground);
   set_string_default(w_cursorBackground, XvicNcursorBackground);

   set_string_default(w_cursor, XvicNcursor);

   set_bool_default(w_trackFloatingCursor, XvicNtrackFloatingCursor);

}

$!-----------------------------------------------------------------------------
$ create test_iw.imake
#define PROGRAM test_iw

#define MODULE_LIST test_iw.c

#define MAIN_LANG_C
#define USES_C

#define TEST

#define LIB_GUISUB
#define LIB_MOTIF
#define LIB_XMU

#define LIB_LOCAL	/*!!!!*/


$ Return
$!#############################################################################
