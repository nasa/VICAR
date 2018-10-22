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
// ColorModel.h: An RGB color model for a single color
////////////////////////////////////////////////////////////
#ifndef COLORMODEL_H
#define COLORMODEL_H

class ColorView;

class ColorModel {
    
  private:
    
    int         _numViews;  // Number of dependent views
    ColorView **_views;     // View objects that depend on this model
    
    int      _red;          // RGB representation of a color
    int      _green;
    int      _blue;
    
    // Called whenever the model's data changes
    
    void     updateViews();
    
  public:
    
    ColorModel();
    
    // Add dependent View objects
    
    void attachView ( ColorView * );
    
    // Functions that allow controllers to manipulate the Model
    
    void setRgb   ( int, int, int );   
    void setRed   ( int r ) { setRgb ( r,    _green, _blue ); }
    void setGreen ( int g ) { setRgb ( _red, g,      _blue ); }
    void setBlue  ( int b ) { setRgb ( _red, _green,  b    ); }
    
    // Functions that allow Views to retrieve the Model's current state
    
    int  red()   { return _red;   }
    int  green() { return _green; }
    int  blue()  { return _blue;  }
};
#endif
