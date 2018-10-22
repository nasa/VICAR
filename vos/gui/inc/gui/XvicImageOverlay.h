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

