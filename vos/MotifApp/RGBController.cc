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
// RGBController.C: Control the ColorModel
/////////////////////////////////////////////////////////////
#include "RGBController.h"
#include "ColorModel.h"
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>

RGBController::RGBController ( Widget      parent, 
			      ColorModel *model, 
			      const char       *name ) : ColorView ( name )
{
    Arg wargs[10];
    int n;
    
    _model = model;   // Keep a pointer to the model
    
    // Set up a manager for a single column of controls
    
    _w = XtVaCreateWidget ( "rgbController", 
			   xmRowColumnWidgetClass, 
			   parent, 
			   XmNnumColumns,  1,
			   XmNpacking,     XmPACK_COLUMN,
			   XmNorientation, XmVERTICAL,
			   NULL );
    installDestroyHandler();    
    
    // Create an XmScale to control each color component. Each
    // widget needs to have the same configuration, so create
    // a single Arg list and pass it to all three widgets.
    
    n = 0;
    XtSetArg ( wargs[n], XmNminimum,        0 ) ; n++;
    XtSetArg ( wargs[n], XmNmaximum,      255 ); n++;
    XtSetArg ( wargs[n], XmNorientation,  XmHORIZONTAL ); n++;
    _redSlider   = XtCreateManagedWidget ( "red",   
					  xmScaleWidgetClass, 
					  _w, wargs, n);
    
    _greenSlider = XtCreateManagedWidget ( "green", 
					  xmScaleWidgetClass, 
					  _w, wargs, n);
    
    _blueSlider  = XtCreateManagedWidget ( "blue", 
					  xmScaleWidgetClass, 
					  _w, wargs, n );
    
    // Install callbacks for each widget, to be called if the
    // scale moves suddenly or is dragged.
    
    XtAddCallback ( _redSlider, 
		   XmNvalueChangedCallback,
		   &RGBController::redChangedCallback,
		   ( XtPointer ) this );
    XtAddCallback ( _redSlider, 
		   XmNdragCallback,
		   &RGBController::redChangedCallback,
		   ( XtPointer ) this );
    
    XtAddCallback ( _greenSlider, 
		   XmNvalueChangedCallback,
		   &RGBController::greenChangedCallback,
		   ( XtPointer ) this );
    XtAddCallback ( _greenSlider, 
		   XmNdragCallback,
		   &RGBController::greenChangedCallback,
		   ( XtPointer ) this );
    
    XtAddCallback ( _blueSlider, 
		   XmNvalueChangedCallback,
		   &RGBController::blueChangedCallback,
		   ( XtPointer ) this );
    XtAddCallback ( _blueSlider, 
		   XmNdragCallback,
		   &RGBController::blueChangedCallback,
		   ( XtPointer ) this );
}

void RGBController::redChangedCallback ( Widget, 
					XtPointer clientData, 
					XtPointer callData ) 
{
    RGBController* obj = ( RGBController * ) clientData;  
    XmScaleCallbackStruct *cb  = ( XmScaleCallbackStruct * ) callData;
    
    obj->redChanged ( cb->value );
}

void RGBController::greenChangedCallback ( Widget,
					  XtPointer clientData, 
					  XtPointer callData )
{
    RGBController* obj = ( RGBController * ) clientData;  
    XmScaleCallbackStruct *cb  = ( XmScaleCallbackStruct * ) callData;
    
    obj->greenChanged ( cb->value );
}

void RGBController::blueChangedCallback ( Widget, 
					 XtPointer clientData, 
					 XtPointer callData )
{
    RGBController* obj = ( RGBController * ) clientData;  
    XmScaleCallbackStruct *cb  = ( XmScaleCallbackStruct * ) callData;
    
    obj->blueChanged ( cb->value );
}

void  RGBController::redChanged ( int value )
{
    _model->setRed ( value );
}

void  RGBController::greenChanged ( int value )
{
    _model->setGreen ( value );
}

void  RGBController::blueChanged ( int value )
{
    _model->setBlue ( value );
}

void RGBController::update ( ColorModel *model )
{
    
    XtVaSetValues ( _redSlider,   XmNvalue, model->red(),   NULL );
    XtVaSetValues ( _greenSlider, XmNvalue, model->green(), NULL );
    XtVaSetValues ( _blueSlider,  XmNvalue, model->blue(),  NULL );
}
