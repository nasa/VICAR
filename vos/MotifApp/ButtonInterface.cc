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
// ButtonInterface.C: A push button interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "ButtonInterface.h"
#include <Xm/PushB.h>

ButtonInterface::ButtonInterface ( Widget parent, 
				  Cmd *cmd ) : CmdInterface ( cmd )
{
    _w = XtCreateWidget ( _name, 
			 xmPushButtonWidgetClass,
			 parent,
			 NULL, 0 );
    installDestroyHandler();
    
    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

// No idea what the right ifdef is for automatically recognizing g++
#ifdef GNU_CC
    
    // G++ reportedly doesn't like the form expected by cfront. I'm
    // told this will work, but I haven't tested it myself.
    
    XtAddCallback ( _w,  
		   XmNactivateCallback, 
		   executeCmdCallback,
		   (XtPointer) this );  
#else

    XtAddCallback ( _w,  
		   XmNactivateCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
#endif
    
}

void ButtonInterface::setButtonLabel(const char *label)
{
    XmString string = XmStringCreateLocalized((char *)label);

    XtVaSetValues(_w, XmNlabelString, string, NULL);

    XmStringFree(string);
}

int ButtonInterface::operator== ( const ButtonInterface &bi )
{
    return this == &bi;
}
