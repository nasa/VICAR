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
// SwatchView.h: Display a color "swatch" corresponding to
//                the color in the ColorModel
/////////////////////////////////////////////////////////////
#ifndef SWATCHVIEW_H
#define SWATCHVIEW_H
#include "ColorView.h"

class ColorModel;

class SwatchView : public ColorView {
    
  protected:
    
    Widget    _swatch;
    Pixel     _index;   
    Boolean   _enabled;
    
  public:
    
    SwatchView ( Widget,  const char * );
    virtual void update ( ColorModel * );
    virtual const char *const className() { return "SwatchView"; }
};
#endif
