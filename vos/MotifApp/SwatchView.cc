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


////////////////////////////////////////////////////////////
// SwatchView.C: Display a swatch of color
/////////////////////////////////////////////////////////////
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include "SwatchView.h"
#include "ColorModel.h"

SwatchView::SwatchView ( Widget parent, const char *name ) : ColorView ( name )
{
    int    status;
    Pixel  pixels[1];
    
    // Put the color swatch in a 3D frame to set it off from 
    // its surroundings
    
    _w = XtCreateWidget ( _name, xmFrameWidgetClass, 
			 parent, NULL, 0 );
    
    installDestroyHandler();    
    
    // Try to allocate a read-write color cell. This function 
    // an array of pixels, of length 1. If sucessful, the allocated
    // color cell will be in pixel[0].    
    
    status = 
	XAllocColorCells ( XtDisplay( _w ),
			  DefaultColormapOfScreen ( XtScreen ( _w ) ),
			  FALSE,
			  NULL, 
			  0,
			  pixels, 1 );
    
    if ( status == FALSE )
    { 
	
	// If the color allocation fails, use the parent's background
	// color as the color of the swatch, and set the frame's
	// shadow thickness to zero, effectively hiding this component
	
	_enabled = FALSE;
	XtVaGetValues ( parent, XmNbackground, &_index , NULL );  
	XtVaSetValues ( _w, XmNshadowThickness, 0, NULL );
    }
    else
    {
	// Flag this object as enabled, and store the allocated 
	// color cell for later use.
	
	_enabled = TRUE;
	_index   = pixels[0];
    }
    
    // Create a widget whose background is set to the allocated color.
    
    _swatch = XtVaCreateManagedWidget ( "swatch", 
				       xmDrawingAreaWidgetClass, 
				       _w, 
				       XmNbackground, _index, 
				       NULL );
}

void SwatchView::update ( ColorModel *model )
{
    // Don't update if the widget has not yet been
    // created or if no color cell was allocated
    
    if ( _swatch && _enabled )
    {   
	XColor color;
	
	// Convert from the 0-256 scale used by the ColorModel
	// to the 0-65535 scale supported by X, and store
	// the result in an XColor structure.
	
	color.red    = model->red()   * 256;
	color.green  = model->green() * 256;
	color.blue   = model->blue()  * 256;
	color.flags  = DoRed | DoBlue | DoGreen;
	color.pixel  = _index;
	
	// Change the values stored in the color cell, thereby
	// changing the background color of the swatch widget
	
	XStoreColor ( XtDisplay ( _w ),
		     DefaultColormapOfScreen ( XtScreen ( _w ) ),
		     &color );
    }
}
