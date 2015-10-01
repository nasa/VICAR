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


////////////////////////////////////////////////////////////////////
// PixmapCycler.h: Abstract class that supports a continuous cycle
//                 of pixmaps for short animation sequences.
////////////////////////////////////////////////////////////////////
#ifndef PIXMAPCYCLER_H
#define PIXMAPCYCLER_H
#include <Xm/Xm.h>

class PixmapCycler {
    
  protected:
    
    int       _numPixmaps;     // Total number of pixmaps in cycle
    int       _current;        // Index of the current pixmap
    Pixmap   *_pixmapList;     // The array of pixmaps
    Dimension _width, _height; // Pixmap size
    
    virtual void createPixmaps() = 0; // Derived class must implement
    PixmapCycler ( int, Dimension, Dimension  );   
    
  public:
    
    virtual ~PixmapCycler();
    Dimension width()  { return _width; }
    Dimension height() { return _height; }
    
    Pixmap next();       // Return the next pixmap in the cycle
};
#endif
