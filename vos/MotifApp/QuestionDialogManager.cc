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
// QuestionDialogManager.C: 
//////////////////////////////////////////////////////////
#include "QuestionDialogManager.h"
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

// Define an instance to be available throughout the framework.

QuestionDialogManager *theQuestionDialogManager = 
    new QuestionDialogManager ( "QuestionDialog" );

QuestionDialogManager::QuestionDialogManager ( const char   *name ) : 
                                 DialogManager ( name )
{
    // Empty
}

Widget QuestionDialogManager::createDialog ( Widget parent )
{
    Widget dialog = XmCreateQuestionDialog ( parent, _name, NULL, 0);
    
    XtVaSetValues ( dialog,
		   XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		   NULL );
    
    return dialog;
}
