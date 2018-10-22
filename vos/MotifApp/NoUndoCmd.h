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


//////////////////////////////////////////////////////////
// NoUndoCmd.h: Base class for all commands without undo
//////////////////////////////////////////////////////////
#ifndef NOUNDOCMD_H
#define NOUNDOCMD_H
#include "Cmd.h"

class NoUndoCmd : public Cmd {
    
  protected:

#ifndef CPLUSPLUS2_1 
    virtual void doit()   = 0;  // Specific actions must be defined    
#endif
    
    virtual void undoit();
    
  public:
    
    NoUndoCmd ( const char *, int );
};
#endif
