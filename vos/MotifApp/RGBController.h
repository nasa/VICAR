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
#ifndef RGBCONTROLLER_H
#define RGBCONTROLLER_H
#include <Xm/Xm.h>
#include "ColorView.h"

class ColorModel;

class RGBController : public ColorView {
    
  private:
    
    Widget _redSlider;      // XmScale widgets for each color component
    Widget _greenSlider;
    Widget _blueSlider;
    
    // Callbacks for when user moves any slider
    
    static void redChangedCallback ( Widget, XtPointer, XtPointer );
    static void greenChangedCallback ( Widget, XtPointer, XtPointer );
    static void blueChangedCallback ( Widget, XtPointer, XtPointer );
    
  protected:
    
    ColorModel *_model;    // ColorModel controlled by this object
    
    // Called when user moves sliders to change a color component
    
    virtual void  redChanged ( int );
    virtual void  greenChanged ( int );
    virtual void  blueChanged ( int );
    
  public:
    
    void update ( ColorModel * ); // Called when the ColorModel changes
    
    RGBController ( Widget , ColorModel *, const char * );
    
    const char *const className() { return "RGBController";}
};
#endif
