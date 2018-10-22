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


///////////////////////////////////////////////////////////
// ColorChooser.C:
/////////////////////////////////////////////////////////////
#include "ColorModel.h"
#include "HSVView.h"
#include "SwatchView.h"
#include "RGBController.h"
#include "RGBView.h"
#include "ColorChooser.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/BulletinB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>

String ColorChooser::_colorChooserResources[] = {
    (char *)"*rgbView.x:                        150",
    (char *)"*rgbView.y:                        20",
    (char *)"*rgbView.orientation:		XmHORIZONTAL",
    (char *)"*rgbView.packing:			XmPACK_COLUMN",
    (char *)"*rgbView.numColumns:		3",
    (char *)"*rgbView*label1*labelString:       Red:",
    (char *)"*rgbView*label2*labelString:       Green:",
    (char *)"*rgbView*label3*labelString:       Blue:",
    (char *)"*hsvView.x:                        300",
    (char *)"*hsvView.y:                        20",
    (char *)"*hsvView*label1*labelString:       Hue:",
    (char *)"*hsvView*label2*labelString:       Saturation:",
    (char *)"*hsvView*label3*labelString:       Value:",
    (char *)"*rgbController.x:                  50",
    (char *)"*rgbController.y:                  150",
    (char *)"*rgbController*scaleWidth:         375",
    (char *)"*rgbController*scaleHeight:        22",
    (char *)"*colorView.x:                      20",
    (char *)"*colorView.y:                      20",
    (char *)"*swatch.width:                     100",
    (char *)"*swatch.height:                    100",
    (char *)"*colorView.shadowType:             shadow_in",
    (char *)"*ok.x:                             20",
    (char *)"*ok.y:                             240",
    (char *)"*ok*labelString:                   OK",
    (char *)"*cancel.x:                         400",
    (char *)"*cancel.y:                         240",
    (char *)"*cancel*labelString:               Cancel",
    //  Debatable use of color
    (char *)"*rgbController*red*troughColor:    red",
    (char *)"*rgbController*green*troughColor:  green",
    (char *)"*rgbController*blue*troughColor:   blue",
    NULL,
};

ColorChooser::ColorChooser ( Widget parent, const char *name ) : 
                UIComponent ( name )
{
    _clientData = NULL;

    // Load the ColorChooser components resources into 
    // resource database
    
    setDefaultResources ( parent , _colorChooserResources );
    
    // The ColorChooser is a dialog, but no existing
    // Motif dialog supports the required layout, so use
    // a generic BulletinBoardDialog.
    
    _w = XmCreateBulletinBoardDialog ( parent, _name, NULL, 0 );
    
    _okButton     = XtCreateManagedWidget ( "ok", 
					   xmPushButtonWidgetClass,
					   _w, NULL, 0 );
    _cancelButton = XtCreateManagedWidget ( "cancel", 
					   xmPushButtonWidgetClass,
					   _w, NULL, 0 );
    
    // Set up the ok and cancel buttons, so the BulletinBoard
    // widget can handle them automatically
    
    XtVaSetValues ( _w, 
		   XmNdefaultButton, _okButton,
		   XmNcancelButton,  _cancelButton, 
		   NULL );
    
    // The OK button allows the user to finalize the selected value
    
    XtAddCallback ( _okButton, 
		   XmNactivateCallback, 
		   &ColorChooser::okCallback,
		   (XtPointer) this );

    XtAddCallback ( _cancelButton, 
		   XmNactivateCallback, 
		   &ColorChooser::cancelCallback,
		   (XtPointer) this );
    
    // Create a ColorModel object, and instantiate one of each 
    // available ColorView class
    
    _model      = new ColorModel();
    _rgbSliders = new RGBController ( _w, _model, (char *)"rgbController" );
    _swatch     = new SwatchView ( _w, (char *)"colorView" );
    _rgbView    = new RGBView ( _w, (char *)"rgbView" );
    _hsvView    = new HSVView ( _w, (char *)"hsvView" );
    
    // Attach each ColorView to the ColorModel object
    
    _model->attachView ( _swatch );
    _model->attachView ( _rgbView );
    _model->attachView ( _hsvView );
    _model->attachView ( _rgbSliders );
    
    // Manage each of the views
    
    _rgbSliders->manage();
    _swatch->manage();
    _rgbView->manage();
    _hsvView->manage();
}

ColorChooser::~ColorChooser()
{
    delete _model;
    delete _rgbSliders;
    delete _swatch;
    delete _rgbView;
    delete _hsvView;
}

void ColorChooser::pickColor ( ColorSelectedCallback okCb, 
			      CancelCallback         cancelCb,
			      void *clientData )
{
    _okCallback     = okCb;
    _cancelCallback = cancelCb;
    _clientData     = clientData;
    manage();
}

void ColorChooser::setColor ( int red, int green, int blue )
{
    _model->setRgb ( red, green, blue);
}

void ColorChooser::okCallback ( Widget, 
			       XtPointer clientData, 
			       XtPointer )
{
    ColorChooser *obj = ( ColorChooser * ) clientData;
    
    obj->ok();
}

void ColorChooser::ok()
{
    if ( _okCallback )
	( *_okCallback )( _model->red(), _model->green(), _model->blue(), _clientData );
}



void ColorChooser::cancelCallback ( Widget, 
				   XtPointer clientData, 
				   XtPointer )
{
    ColorChooser *obj = ( ColorChooser * ) clientData;
    
    obj->cancel();
}

void ColorChooser::cancel()
{
    if ( _cancelCallback )
	( *_cancelCallback )( _clientData );
}

