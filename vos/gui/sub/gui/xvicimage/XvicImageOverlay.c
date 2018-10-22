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

