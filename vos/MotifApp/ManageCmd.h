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


/////////////////////////////////////////////////////////////
// ManageCmd.h: Manage all windows in a MotifApp application.
//////////////////////////////////////////////////////////////
#ifndef MANAGECMD_H
#define MANAGECMD_H
#include "NoUndoCmd.h"

class ManageCmd : public NoUndoCmd {
    
  protected:
    
    virtual void doit();      // Manage all windows
    
  public:
    
    ManageCmd ( const char *, int );
    virtual const char *const className () { return "ManageCmd"; }
};
#endif
