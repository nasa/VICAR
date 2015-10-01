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
// BasicComponent.C: Initial version of a class to define 
//                    a protocol for all components
///////////////////////////////////////////////////////////
#include "BasicComponent.h"
#include <assert.h>
#include <stdio.h>

BasicComponent::BasicComponent ( const char *name )
{
    _w = NULL;
    assert ( name != NULL );  // Make sure programmers provide name
    _name = strdup ( name );
}

BasicComponent::~BasicComponent()
{
    if( _w )
	XtDestroyWidget ( _w );
    delete [] _name;
}

void BasicComponent::manage()
{
    assert ( _w != NULL );
    XtManageChild ( _w );
}

void BasicComponent::unmanage()
{
    assert ( _w != NULL );
    XtUnmanageChild ( _w );
}

int BasicComponent::isManaged()
{
    if (!_w)
        return FALSE;
    return XtIsManaged(_w);
}

