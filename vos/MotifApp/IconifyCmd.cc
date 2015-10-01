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
// IconifyCmd.C: Iconify all windows in a MotifApp application.
////////////////////////////////////////////////////////////////
#include "IconifyCmd.h"
#include "Application.h"

IconifyCmd::IconifyCmd ( const char *name, int active ) : 
              NoUndoCmd ( name, active )
{
    // Empty
}


void IconifyCmd::doit()
{
    theApplication->iconify(); // Close all top-level windows
}       
