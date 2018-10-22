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

