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

