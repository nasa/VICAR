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

