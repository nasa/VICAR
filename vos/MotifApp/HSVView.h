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
// HSVView.h: Display the contents of a ColorModel as
//            HSV color components
/////////////////////////////////////////////////////////////
#ifndef HSVVIEW_H
#define HSVVIEW_H
#include "TextView.h"

class ColorModel;

class HSVView : public TextView {
    
  public:
    
    static void RGBToHSV ( int, int, int, int&, int&, int& );
    
    HSVView ( Widget parent, const char * );
    
    virtual void update ( ColorModel * );
    
    virtual const char *const className() { return "HSVView"; }
};
#endif
