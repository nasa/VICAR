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
// DialogManager.h: A base class for cached dialogs
//////////////////////////////////////////////////////////
#ifndef DIALOGMANAGER_H
#define DIALOGMANAGER_H

#include "UIComponent.h"
#include "DialogCallbackData.h"

class DialogManager : public UIComponent {
    
  private:
    
    Widget getDialog(); 
    
    static void destroyTmpDialogCallback ( Widget, 
					  XtPointer, 
					  XtPointer );
    static void okCallback ( Widget, 
			    XtPointer, 
			    XtPointer );
    
    static void cancelCallback ( Widget, 
				XtPointer, 
				XtPointer );
    
    static void helpCallback ( Widget, 
			      XtPointer, 
			      XtPointer );
    
    void cleanup ( Widget, DialogCallbackData* );
    
  protected:
    
    // Called to get a new dialog
    
    virtual Widget createDialog ( Widget ) = 0;   
    
  public:
    
    DialogManager ( const char * );
    
    virtual Widget post ( const char *, 
			 void *clientData      = NULL,
			 DialogCallback ok     = NULL,
			 DialogCallback cancel = NULL,
			 DialogCallback help   = NULL );
};
#endif
