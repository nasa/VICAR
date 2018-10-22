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


///////////////////////////////////////////////////////////
// ManageCmd.C: Manage all windows in a MotifApp application.
//////////////////////////////////////////////////////////
#include "ManageCmd.h"
#include "Application.h"

ManageCmd::ManageCmd ( const char *name, int active ) : 
                              NoUndoCmd ( name, active )
{
    // Empty
}

void ManageCmd::doit()
{
    theApplication->manage(); // Opens all top-level windows
}
