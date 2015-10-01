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
// QuitCmd.h: Exit an application after checking with user.
////////////////////////////////////////////////////////////
#ifndef QUITCMD_H
#define QUITCMD_H
#include "WarnNoUndoCmd.h"

class QuitCmd : public WarnNoUndoCmd {
    
  protected:
    
    virtual void doit();      // Call exit
    
  public:
    
    QuitCmd ( const char *, int );
    QuitCmd ( const char *, int, const char * );
    virtual const char *const className () { return "QuitCmd"; }
};
#endif
