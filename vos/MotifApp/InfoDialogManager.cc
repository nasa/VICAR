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
// InfoDialogManager.C: 
//////////////////////////////////////////////////////////
#include "InfoDialogManager.h"
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

DialogManager *theInfoDialogManager = 
new InfoDialogManager ( "InformationDialog" );

InfoDialogManager::InfoDialogManager ( const char   *name ) : 
                                   DialogManager ( name )
{
    // Empty
}

Widget InfoDialogManager::createDialog ( Widget parent )
{
    Widget dialog = XmCreateInformationDialog ( parent, _name, NULL, 0 );

    Widget w = XmMessageBoxGetChild ( dialog, XmDIALOG_CANCEL_BUTTON );

    XtUnmanageChild ( w );
    
    return dialog;
}
