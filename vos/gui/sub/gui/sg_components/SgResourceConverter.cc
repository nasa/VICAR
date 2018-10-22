#include "SgResourceConverter.h"
#include "Application.h"
#ifndef NO_XPM
#include <X11/xpm.h>
#else
#include <X11/Xlib.h>
#endif
#include <ctype.h>
#include <stdio.h>

Boolean SgResourceConverter::_firstTime = True;

void SgResourceConverter::registerStringToPixmapConverter()
{
    if (_firstTime == True) {
        _firstTime = False;
 
        XtAppAddConverter(theApplication->appContext(),
                          XmRString, XmRPrimForegroundPixmap,
                          cvtStringToPixmap,
                          NULL, 0);
    }
}

void SgResourceConverter::cvtStringToPixmap(XrmValue *, 
					    Cardinal *,
					    XrmValue *fromVal, 
					    XrmValue *toVal)
{
    static Pixmap pixmap;
    char *image_name = (char *) (fromVal->addr);
    Screen *screen;
    char *name;
    Pixel foreground;
    Pixel background;
 
    if (stringsAreEqual(image_name, "unspecified_pixmap")) {
        pixmap = XmUNSPECIFIED_PIXMAP;
    }
    else {
        screen = XtScreen(theApplication->baseWidget());
        name = XtResolvePathname(XtDisplay(theApplication->baseWidget()),
                                 "bitmap", image_name, NULL,
                                 NULL, NULL, 0, NULL);
#ifndef NO_XPM
        if (name && strstr(image_name, ".xpm")) {
            Pixmap shapemask;
            int status = XpmReadFileToPixmap(theApplication->display(),
                                DefaultRootWindow(theApplication->display()),
                                name,
                                &pixmap, &shapemask,
                                NULL);
	    if (status != XpmSuccess || pixmap == 0) 
		pixmap = XmUNSPECIFIED_PIXMAP;
            if (shapemask != None)
                XFreePixmap(theApplication->display(), shapemask);
            XtFree(name);
        }
        else
#endif
        if (name) {
            XtVaGetValues(theApplication->baseWidget(),
                          XmNforeground, &foreground,
                          XmNbackground, &background,
                          NULL);
            pixmap = XmGetPixmap(screen, name, foreground, background);
        }
        else {
	    XtVaGetValues(theApplication->baseWidget(),
                          XmNforeground, &foreground,
                          XmNbackground, &background,
                          NULL);
            pixmap = XmGetPixmap(screen, image_name, foreground, background);
	}
    }
    (*toVal).size = sizeof (Pixmap);
    (*toVal).addr = (XPointer) &pixmap;
}

Boolean SgResourceConverter::stringsAreEqual(const char *in_str,
						const char *test_str)
{
    register char i ;
 
    if(((in_str[0] == 'X') || (in_str[0] == 'x'))
       && ((in_str[1] == 'M') || (in_str[1] == 'm'))) {
        in_str +=2;
    }
    do {
        i = (char) tolower( *in_str++);
	
        if (i != *test_str++) {
            return( False);
        }
    } while (i);
    
    return(True);
}
