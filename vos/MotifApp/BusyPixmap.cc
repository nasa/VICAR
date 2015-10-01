///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////
// BusyPixmap.C
///////////////////////////////////////////////////
#include "BusyPixmap.h"
#include <Xm/Xm.h>

#define NUMPIXMAPS  8
#define PIXMAPSIZE  50

BusyPixmap::BusyPixmap ( Widget w ) : 
        PixmapCycler ( NUMPIXMAPS, PIXMAPSIZE, PIXMAPSIZE )
{
    _w = w;
}

void BusyPixmap::createPixmaps()
{
    int angle, delta, i;
    XGCValues  gcv;
    
    // Create a graphics context used to draw each pixmap,
    // based on the colors of the given widget
    
    XtVaGetValues ( _w, 
		   XmNforeground, &gcv.foreground,
		   XmNbackground, &gcv.background,
		   NULL );
    
    _gc = XtGetGC ( _w,  GCForeground | GCBackground, &gcv );
    
    // Create a second GC used to fill the pixmap with
    // the background color of the widget
    
    XtVaGetValues ( _w, 
		   XmNforeground, &gcv.background,
		   XmNbackground, &gcv.foreground,
		   NULL );
    
    _inverseGC = XtGetGC ( _w,  GCForeground | GCBackground, &gcv );
    
    // Define the starting increment, and a slice of the pie.
    // The size of the pie slice depends on the number of pixmaps
    // to be created.
    
    angle = 360;
    delta = 360 / NUMPIXMAPS;
    
    for ( i = 0; i < NUMPIXMAPS; i++)
    {
	// Create a pixmap for each slice of the pie. X measures
	// counterclockwise, so subtract the size of each slice
	// so the sequence moves clockwise.
	
	_pixmapList[i] = createBusyPixmap ( angle, delta ); 
	angle -= delta;
    }
    
    // Release the GCs after all pixmaps have been created
    
    XtReleaseGC ( _w, _gc );
    XtReleaseGC ( _w, _inverseGC );
}

Pixmap BusyPixmap::createBusyPixmap ( int    start, 
				     int    end )
{
    Pixmap    pm;
    const int margin = 1;
    
    // Create a pixmap. Use the root window used by the widget,
    // because the widget may not be realized, or may be a gadget
    
    pm = XCreatePixmap ( XtDisplay ( _w ), 
			RootWindowOfScreen ( XtScreen ( _w ) ),
			_width, _height,
			DefaultDepthOfScreen ( XtScreen ( _w ) ) );
    
    // Pixmaps have to be cleared by filling them with a background color
    
    XFillRectangle ( XtDisplay ( _w ), 
		    pm, 
		    _inverseGC, 
		    0, 0, _width, _height );
    
    // Draw a complete circle just inside the bounds of the pxmap
    
    XDrawArc ( XtDisplay ( _w ), 
	      pm,
	      _gc, 
	      margin, margin, 
	      _width - 2 * margin, 
	      _height - 2 * margin, 
	      0, 360 * 64 );
    
    // Draw the pie slice as a solid color
    
    XFillArc ( XtDisplay ( _w ), 
	      pm,
	      _gc, 
	      margin, margin, 
	      _width - 2 * margin, 
	      _height - 2 * margin, 
	      start * 64, end * 64 );
    
    return pm;
}
