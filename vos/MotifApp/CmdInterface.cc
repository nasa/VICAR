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


///////////////////////////////////////////////////////
// CmdInterface.C
/////////////////////////////////////////////////////////
#include "CmdInterface.h"
#include "Cmd.h"
#include "CmdList.h"
#include "stdio.h"
#include <assert.h>

CmdInterface::CmdInterface ( Cmd *cmd ) : UIComponent( cmd->name() )
{
    _active = TRUE;
    _cmd    = cmd;
    _deferredList = NULL;
    cmd->registerInterface ( this );
}

CmdInterface::CmdInterface ( const char *name ) : UIComponent( name )
{
    _active = TRUE;
    _cmd = NULL;
    _deferredList = NULL;
}

// This constructor is only needed when the command is optional; i.e.
// sometimes provided and sometimes not.

CmdInterface::CmdInterface ( Cmd *cmd, const char *name ) : UIComponent( name )
{
    _active = TRUE;
    _cmd = cmd;
    _deferredList = NULL;
    if (cmd)
        cmd->registerInterface ( this );
}

CmdInterface::~CmdInterface ()
{
    if (_cmd)
        _cmd->unRegisterInterface ( this );
}

////////////////////////////////////////////////////////////////////////
// Originally, setCommand must only be called if the char* constructor
// was used, and must only be called once!
// Now, it can be called multiple times, since it unregisters with any
// old Cmd's
////////////////////////////////////////////////////////////////////////

void CmdInterface::setCommand ( Cmd *cmd )
{
    if (_cmd != NULL)
        _cmd->unRegisterInterface(this);

    _cmd = cmd;
    cmd->registerInterface ( this );
    setValue(cmd->getValue());
}

////////////////////////////////////////////////////////////////////////
// Calling this function with a command list will cause any attempts by
// this interface to execute the command to be suspended.  Instead of
// executing, the command is put onto the given list for execution later.
// This is typically used in dialog boxes, where OK/Apply runs the list.
// Calling this function with a NULL argument will reset immediate
// execution.
////////////////////////////////////////////////////////////////////////

void CmdInterface::setDeferredExec ( CmdList *list )
{
    // If we're changing lists (or disabling them), make sure we remove
    // the command from it first.

    if (list != _deferredList)
        if (_deferredList != NULL && _cmd != NULL)
            _deferredList->remove(_cmd);

    _deferredList = list;
}

////////////////////////////////////////////////////////////////////////
// executeCmdCallback calls executeCmd() instead of running the command
// directly so that subclasses may override it and still use this
// callback function.
////////////////////////////////////////////////////////////////////////

void CmdInterface::executeCmdCallback ( Widget, 
				       XtPointer clientData,
				       XtPointer callData)
{
    CmdInterface *obj = (CmdInterface *) clientData;
    
    assert ( obj->_cmd != NULL);
    obj->executeCmd(callData);
}

////////////////////////////////////////////////////////////////////////
// This should be overridden by commands needing CmdValue's to retrieve
// the value from the interface (allocating it if necessary) and call
// runCmd(CmdValue).  Do not call _cmd->execute() directly or deferred
// command execution won't work.  The argument is the call_data from
// the callback.
////////////////////////////////////////////////////////////////////////

void CmdInterface::executeCmd ( XtPointer )	// call_data defaults to NULL
{
    runCmd();
}

////////////////////////////////////////////////////////////////////////
// This is where we decide whether to run the command or defer it.
// This class should not generally be overridden by subclasses.
////////////////////////////////////////////////////////////////////////

void CmdInterface::runCmd ( CmdValue value )	// value defaults to NULL
{

    if (_deferredList)			// Save on list instead
        _deferredList->addUnique(_cmd, value);
    else
        _cmd->execute( value );		// Execute normally
}

////////////////////////////////////////////////////////////////////////

void CmdInterface::setValue ( CmdValue )
{
    if (_deferredList && _cmd)
        _deferredList->remove(_cmd);
}

////////////////////////////////////////////////////////////////////////

void CmdInterface::activate()
{
    if ( _w )
	XtSetSensitive ( _w, TRUE );
    _active = TRUE;
}

void CmdInterface::deactivate()
{
    if ( _w )
	XtSetSensitive ( _w, FALSE );
    _active = FALSE;
}
