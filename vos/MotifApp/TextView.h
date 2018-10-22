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
// TextView.C: Abstract base class for all text (numerical)
//             views of a ColorModel
/////////////////////////////////////////////////////////////
#ifndef TEXTVIEW_H
#define TEXTVIEW_H
#include "ColorView.h"

class TextView : public ColorView {
    
  protected:
    
    static String _defaultResources[]; // resource defaults

    Widget _field1;    // An output area
    Widget _field2;
    Widget _field3;
    
    Widget _label1;    // Labels for each output area
    Widget _label2;
    Widget _label3;
    
  public:

#ifndef CPLUSPLUS2_1  // required for C++ 2.0 
    virtual void  update ( ColorModel * ) = 0;  
#endif
    
    TextView ( Widget,  const char * );
    virtual const char *const className() { return "TextView"; }
};
#endif
