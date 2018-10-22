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
// SelectFileCmd.C: 
//////////////////////////////////////////////////////////
#include "SelectFileCmd.h"
#include "Application.h"
#include <Xm/FileSB.h>

SelectFileCmd::SelectFileCmd ( const char  *name, 
			      int          active,
			      FileCallback callback,
			      void        *clientData ) :
			      NoUndoCmd ( name, active )
{
    _callback   = callback;
    _clientData = clientData;
}

void SelectFileCmd::doit()
{
    // Create a FileSelectionBox widget
    
    _fileBrowser =
	XmCreateFileSelectionDialog ( theApplication->baseWidget(),
				     (char *) name(), // Different from the book. cast required
				     NULL, 0 );
    
    // Set up the callback to be called when the user chooses a file
    
    XtAddCallback ( _fileBrowser, 
		   XmNokCallback,
		   &SelectFileCmd::fileSelectedCallback, 
		   (XtPointer) this );

    XtAddCallback ( _fileBrowser,
		   XmNcancelCallback,
		   &SelectFileCmd::fileCancelledCallback,
		   NULL );
    
    // Display the dialog
    
    XtManageChild ( _fileBrowser );
}

void SelectFileCmd::fileCancelledCallback ( Widget w,
					   XtPointer,
					   XtPointer )
{
    XtDestroyWidget ( w );
}

void SelectFileCmd::fileSelectedCallback ( Widget    w,
					  XtPointer clientData,
					  XtPointer callData )
{
    SelectFileCmd * obj = (SelectFileCmd *) clientData;
    XmFileSelectionBoxCallbackStruct *cb = 
	(XmFileSelectionBoxCallbackStruct *) callData;
    char     *name   = NULL;
    XmString  xmstr  = cb->value;  // The selected file
    int       status = 0;
    Boolean   flag;
    
    if ( xmstr )   // Make sure a file was selected
    {
	// Extract the first character string matching the default
	// character set from the compound string
	
	status = XmStringGetLtoR ( xmstr, 
				  XmSTRING_DEFAULT_CHARSET,
				  &name );
	
	// If a string was succesfully extracted, call
	// fileSelected to handle the file.
	
	if ( status )
	    flag = obj->fileSelected ( name );
    }
    
    // Doug Young forgot to do this
    XtFree ( name );
    if (flag)
        XtDestroyWidget ( w );   // Destroy the file selection dialog
}

// A return status of TRUE means to accept the file and make the dialog
// go away.  Returning FALSE leaves the dialog up so the user can try again.

Boolean SelectFileCmd::fileSelected ( char *filename )
{
    if ( _callback )
	return _callback ( _clientData, filename );
    else
        return TRUE;
}
