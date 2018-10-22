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
// NoOpCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "NoOpCmd.h"
#include "Application.h"
#include <iostream>

NoOpCmd::NoOpCmd ( const char *name, int active ) : Cmd ( name, active )
{
    // Empty
}

void NoOpCmd::doit()
{
    // Just print a message that allows us to trace the execution

    std::cout <<  name() << ":" << "doit\n" << std::flush;
}      

void NoOpCmd::undoit()
{
    // Just print a message that allows us to trace the execution

    std::cout <<  name() << ":" << "undoit\n" << std::flush;
}       
