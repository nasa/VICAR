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
// ErrorDialogManager.C: 
//////////////////////////////////////////////////////////
#include "ErrorDialogManager.h"
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

DialogManager *theErrorDialogManager = 
new ErrorDialogManager ( "ErrorDialog" );

ErrorDialogManager::ErrorDialogManager ( const char   *name ) : 
                                   DialogManager ( name )
{
    // Empty
}

Widget ErrorDialogManager::createDialog ( Widget parent )
{
    Widget dialog = XmCreateErrorDialog ( parent, _name, NULL, 0 );
    Widget w = XmMessageBoxGetChild ( dialog,
                                      XmDIALOG_CANCEL_BUTTON );
   
    XtVaSetValues ( dialog,
                    XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
                    NULL );
    XtUnmanageChild ( w );
   
    return dialog;
}
