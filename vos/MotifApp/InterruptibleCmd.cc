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


///////////////////////////////////////////////////////////////
// InterruptibleCmd.C: Abstract class that supports lengthy,
//                     user-interruptible activities
//////////////////////////////////////////////////////////////
#include "InterruptibleCmd.h"
#include "WorkingDialogManager.h"
#include "Application.h"
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <assert.h>

InterruptibleCmd::InterruptibleCmd ( const char *name, int active ) :
                        NoUndoCmd ( name, active )
{
    _wpId        = None;   // There is no work procedure yet
    _callback    = NULL;	   // Callbacks are specified in execute()
    _clientData  = NULL;
    _done        = FALSE; 
}

InterruptibleCmd::~InterruptibleCmd()
{
    // Clean up by removing all callbacks
    
    if ( _wpId)    
	XtRemoveWorkProc ( _wpId );
}

void InterruptibleCmd::execute ( TaskDoneCallback callback, void *clientData )
{
    _callback   = callback;
    _clientData = clientData;
    execute();
}

void InterruptibleCmd::execute( CmdValue )
{
    _done  = FALSE;  // Initialize flag
    
    // Call the Cmd execute function to handle the Undo, and other
    // general mechanisms supported by Cmd. Execute calls doit()
    
    Cmd::execute();  
    
    // If the task was completed in a single call,
    // don't bother to set up a work procedure. Just
    // give derived classes a chance to cleanup and
    // call the application's callback function
    
    if ( _done )
    {
	cleanup();
	
	if ( _callback )
	    ( *_callback )( this, FALSE, _clientData );
    }
    
    // If the task is not done, post a WorkingDialog and 
    // install a work procedure to continue the task 
    // as soon as possible.
    
    if ( !_done )
    {
	theWorkingDialogManager->post ( "" , (void *) this,
				       NULL, 
				       &InterruptibleCmd::interruptCallback );
	
	_wpId = XtAppAddWorkProc ( theApplication->appContext(), 
				  &InterruptibleCmd::workProcCallback,
				  (XtPointer) this );
    }
}

Boolean InterruptibleCmd::workProcCallback ( XtPointer clientData )
{
    InterruptibleCmd *obj = (InterruptibleCmd *) clientData;
    
    // The work procedure just returns the value returned by the
    // workProc member function.
    
    return ( obj->workProc() );
}

Boolean InterruptibleCmd::workProc()
{
    doit();
    
    // If the task has been completed, hide the dialog,
    // give the derived class a chance to clean up, and notify
    // the application that instantiated this object.
    
    if ( _done )
    {
	theWorkingDialogManager->unpost();
	cleanup();
	
	if ( _callback )
	    ( *_callback )( this, FALSE, _clientData );
    }
    
    return _done;
}

void InterruptibleCmd::cleanup()
{
    // Empty
}

void InterruptibleCmd::abortCleanup()
{
    cleanup();
}

void InterruptibleCmd::interruptCallback ( void * clientData )
{
    InterruptibleCmd *obj = ( InterruptibleCmd * ) clientData;
    
    // Just set the _interrupt flag to TRUE. The workProc() 
    // function will notice the next time it is called
    
    obj->interrupt();
}

void InterruptibleCmd::interrupt()
{
    // Remove the work procedure
    
    XtRemoveWorkProc ( _wpId );
    
    // Remove the working dialog and give derived 
    // classes a chance to clean up 
    
    theWorkingDialogManager->unpost();
    abortCleanup();
    
    // Notify the application that the task was interrupted
    
    if ( _callback )
	( *_callback )( this, TRUE, _clientData);
}

void InterruptibleCmd::updateMessage ( const char * msg )
{
    theWorkingDialogManager->updateMessage ( msg );   
}
