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
// ListInterface.C: A list interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "ListInterface.h"
#include <Xm/ScrolledW.h>
#include <Xm/List.h>
#include <Xm/PushB.h>

ListInterface::ListInterface ( Widget parent, 
                               Cmd *cmd ) : CmdInterface ( cmd )
{
    // The base widget, _w, points to the ScrolledWindow instead of
    // the List.  The List is managed within the ListInterface.
    // Since the scrolled window is created as a managed widget by 
    // XmCreateScrolledList, it is not required to manage the scrolled
    // window widget after creation of the ListInterface, although it doesn't
    // hurt to do so.

    _list = XmCreateScrolledList ( parent, _name, NULL, 0 );
    XtManageChild(_list);
    _w = XtParent ( _list );

    installDestroyHandler();
    
    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.

    if ( _active )
        activate();     
    else
        deactivate();   

#ifdef GNU_CC

    // No idea what the right ifdef is for automatically recognizing g++
    // G++ reportedly doesn't like the form expected by cfront. I'm
    // told this will work, but I haven't tested it myself.

    XtAddCallback ( _list,
                   XmNdefaultActionCallback,
                   executeCmdCallback,
                   (XtPointer) this );
#else

    XtAddCallback ( _list,
                   XmNdefaultActionCallback,
                   &CmdInterface::executeCmdCallback,
                   (XtPointer) this );
#endif

}

int ListInterface::operator== ( const ListInterface &bi )
{
    return this == &bi;
}
