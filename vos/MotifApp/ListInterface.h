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


//////////////////////////////////////////////////////////////
// ListInterface.h: A list interface to a Cmd object.  _w is actually
// the ScrolledWindow parent, not the list itself.
///////////////////////////////////////////////////////////////
#ifndef LISTINTERFACE
#define LISTINTERFACE
#include "CmdInterface.h"

class ListInterface : public CmdInterface {
   
protected:

    Widget _list;
   
public:
   
    ListInterface ( Widget, Cmd * );
    Widget listWidget() { return _list; }	// Added access functions - HBM
    int operator== ( const ListInterface & );
};
#endif
