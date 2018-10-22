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
// WarnNoUndoCmd.C: Warns user before executing a command
//////////////////////////////////////////////////////////
#include "WarnNoUndoCmd.h"

#define DEFAULTWARNING "This command cannot be undone. Proceed anyway?"

WarnNoUndoCmd::WarnNoUndoCmd ( const char *name, int active) : 
                     AskFirstCmd ( name, active )
{
    _hasUndo = 0;     // Specify that there is no undo
    
    setQuestion ( DEFAULTWARNING );
}

void WarnNoUndoCmd::undoit()
{
    // Empty
} 
