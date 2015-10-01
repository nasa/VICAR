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


//////////////////////////////////////////////////////////////////
// PixmapCycler.C: Abstract class that supports a continuous cycle
//                 of pixmaps for short animation sequences.
////////////////////////////////////////////////////////////////////
#include "PixmapCycler.h"

#define INVALID -1

PixmapCycler::PixmapCycler ( int numPixmaps, Dimension w, Dimension h )
{
    _numPixmaps = numPixmaps;
    _current    = INVALID;
    _pixmapList = new Pixmap[_numPixmaps];
    _width      = w;
    _height     = h;
}

PixmapCycler::~PixmapCycler()
{
    delete []_pixmapList;
}

Pixmap PixmapCycler::next()
{
    // The first time, call the createPixmaps() function 
    // implemented by the derived class to create the pixmaps
    
    if ( _current == INVALID )
    {
	createPixmaps();
	_current = 0;    // Initialize to the first pixmap
    }
    
    // If the counter is larger than the index of the 
    // last pixmap, roll it over and restart with zero
    
    if ( _current >= _numPixmaps )
	_current = 0;
    
    // Return the current pixmap and increment the counter
    
    return _pixmapList[_current++];
}
