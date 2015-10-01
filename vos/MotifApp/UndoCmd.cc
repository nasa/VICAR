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
// UndoCmd.C: An interface to undoing the last command
//////////////////////////////////////////////////////////
#include "UndoCmd.h"

// Declare a global object: theUndoCmd

UndoCmd *theUndoCmd = new UndoCmd ( "Undo" ); 

UndoCmd::UndoCmd ( const char *name ) : NoUndoCmd ( name, 0 )
{
    // Empty
}

void UndoCmd::doit()
{
    // If there is a current command, undo it
    
    if ( _lastCmd != NULL )
    {
	// Undo the previous command
	
	_lastCmd->undo();
	
	_lastCmd = NULL; // Make sure we can't undo twice
    }
}
