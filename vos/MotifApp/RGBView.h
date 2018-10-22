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
// RGBView.h: Display the contents of a ColorModel as
//            RGB color components
/////////////////////////////////////////////////////////////
#ifndef RGBVIEW_H
#define RGBVIEW_H
#include "TextView.h"

class RGBView : public TextView {
    
  public:
    
    RGBView ( Widget, const char * );
    
    virtual void update ( ColorModel * );
    virtual const char *const className() { return "RGBView"; }
};
#endif
