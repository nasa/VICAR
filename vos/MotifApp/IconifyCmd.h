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


////////////////////////////////////////////////////////////////
// IconifyCmd.h: Iconify all windows in a MotifApp application.
////////////////////////////////////////////////////////////////
#ifndef ICONIFYCMD_H
#define ICONIFYCMD_H
#include "NoUndoCmd.h"

class IconifyCmd : public NoUndoCmd {
    
  protected:
    
    virtual void doit();      // Iconify all windows
    
  public:
    
    IconifyCmd ( const char *, int );
    virtual const char *const className () { return "IconifyCmd"; }
};
#endif
