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
// WorkingDialogManager.C: 
//////////////////////////////////////////////////////////
#include "WorkingDialogManager.h"
#include "Application.h"
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include "BusyPixmap.h"
#include <assert.h>

WorkingDialogManager *theWorkingDialogManager =
        new WorkingDialogManager ( "WorkingDialog" );

WorkingDialogManager::WorkingDialogManager ( const char   *name ) : 
                           DialogManager ( name )
{
    _intervalId  = None;
    _busyPixmaps = NULL;
}

Widget WorkingDialogManager::createDialog ( Widget parent )
{
    Widget dialog = XmCreateWorkingDialog ( parent, _name, NULL, 0 );
    
    XtVaSetValues ( dialog,
		   XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		   NULL );
    
    XtAddCallback ( dialog, 
		   XmNokCallback, 
		   &WorkingDialogManager::unpostCallback,
		   (XtPointer) this );
    
    XtAddCallback ( dialog, 
		   XmNcancelCallback, 
		   &WorkingDialogManager::unpostCallback,
		   (XtPointer) this );
    
    if ( !_busyPixmaps )
	_busyPixmaps = new BusyPixmap ( dialog );    
    
    return dialog;
}

Widget WorkingDialogManager::post ( const char   *text, 
				   void          *clientData,
				   DialogCallback ok,
				   DialogCallback cancel,
				   DialogCallback help )
{
    // The the dialog already exists, and is currently in use,
    // just return this dialog. The WorkingDialogManager
    // only supports one dialog.
    
    if ( _w && XtIsManaged ( _w ) )
	return _w;
    
    // Pass the message on to the base class
    
    DialogManager::post ( text, clientData, ok, cancel, help );
    
    // Call timer to start an animation sequence for this dialog
    
    timer();
    
    return _w;
}

void WorkingDialogManager::timerCallback ( XtPointer clientData,
					  XtIntervalId * )
{
    WorkingDialogManager *obj = ( WorkingDialogManager * ) clientData;
    
    obj->timer();
}

void WorkingDialogManager::timer ()
{
    if ( !_w )
	return;
    
    // Reinstall the time-out callback to be called again
    
    _intervalId = 
	XtAppAddTimeOut ( XtWidgetToApplicationContext ( _w ),
			 250, 
			 &WorkingDialogManager::timerCallback,
			 ( XtPointer ) this );    
    
    // Get the next pixmap in the animation sequence and display
    // it in the dialog's symbol area.
    
    XtVaSetValues ( _w, 
		   XmNsymbolPixmap, _busyPixmaps->next(),
		   NULL );
}

void WorkingDialogManager::unpostCallback ( Widget , 
					   XtPointer clientData, 
					   XtPointer )
{
    WorkingDialogManager *obj = ( WorkingDialogManager* ) clientData;
    
    obj->unpost();
}

void WorkingDialogManager::unpost ()
{
    assert ( _w != NULL );
    
    // Remove the dialog from the screen
    
    XtUnmanageChild ( _w );
    
    // Stop the animation
    
    if ( _intervalId )
	XtRemoveTimeOut ( _intervalId );
}

void WorkingDialogManager::updateMessage ( const char *text )
{
    if ( _w )
    {
    
	// Just change the string displayed in the dialog
    
	XmString xmstr = XmStringCreateSimple ( (char *)text ); 
	XtVaSetValues ( _w, XmNmessageString, xmstr, NULL );
	XmStringFree ( xmstr );
    }
}
