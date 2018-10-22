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
// HSVView.C: Display the contents of a ColorModel as
//            HSV color components
/////////////////////////////////////////////////////////////
#include "HSVView.h"
#include "ColorModel.h"
#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <stdio.h>

HSVView::HSVView ( Widget parent, const char *name ) : 
            TextView ( parent, name )
{
    // Empty
}

void HSVView::update( ColorModel *model )
{
    char     buf[100]; 
    int      hue, value, saturation;
    
    // Compute the hue, saturation, and value components
    // of a color from its RGB values.
    
    RGBToHSV ( model->red(),
	      model->green(),
	      model->blue(),
	      hue, saturation, value );
    
    // Format and display each of the color components
    
    sprintf ( buf, "%3.3d",    hue );        // Hue
    XmTextFieldSetString ( _field1, buf );
    sprintf ( buf, "%3.3d",    saturation ); // Saturation
    XmTextFieldSetString ( _field2, buf );
    sprintf ( buf, "%3.3d",    value  );     // Value
    XmTextFieldSetString ( _field3, buf );
}

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b) ((a) > (b) ? (b) : (a))
#endif

void HSVView::RGBToHSV ( int     red,
			int     green,
			int     blue,
			int&    hue,         // Return value
			int&    saturation,  // Return value
			int&    value )      // Return value
{
    float h, s, v;
    float r, g, b;
    float temp;
    
    // Normalize the rgb values to lie between 0 and 1.0
    
    r =  (float) red   /  255.0;
    g =  (float) green /  255.0;
    b =  (float) blue  /  255.0;
    
    // Compute the value
    
    v = MAX ( MAX ( r, g ), b );
    
    // Compute the saturation
    
    temp = MIN  ( MIN ( r, g ), b );
    
    if  ( v == 0.0 )
	s = 0.0;
    else
	s = ( v - temp ) / v;
    
    // If saturation is not zero, compute the hue
    
    if ( s != 0.0 )
    {
	float Cr = ( v - r ) / ( v - temp );
	float Cg = ( v - g ) / ( v - temp );
	float Cb = ( v - b ) / ( v - temp );
	if ( r == v ) 
	    h = Cb - Cg;
	else if ( g == v )
	    h = 2.0 + Cr - Cb;
	else if ( b == v )
	    h = 4.0 + Cg - Cr;
	
	h = 60.0 * h;
	if ( h < 0.0 )
	    h += 360.0;
    }
    else 
	h = 0.0;
    
    // Convert value and saturation to percentages
    
    value      = ( int ) ( 100 * v );
    saturation = ( int ) ( 100 * s );
    hue = ( int ) h;
}
