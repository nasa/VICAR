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


///////////////////////////////////////////////////
// BusyPixmap.h
///////////////////////////////////////////////////
#include "PixmapCycler.h"

class BusyPixmap : public PixmapCycler {
    
  protected:
    
    GC     _gc, _inverseGC; // Used to draw Pixmaps
    Widget _w;              // Widget whose colors are to be used 
    void createPixmaps();   // Overrides base class' pure virtual
    virtual Pixmap createBusyPixmap ( int, int );
    
  public:
    
    BusyPixmap ( Widget );
};
