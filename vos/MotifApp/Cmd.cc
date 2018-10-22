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
// Cmd.C
///////////////////////////////////////////////////////
#include "Cmd.h"
#include "CmdList.h"
#include "CmdInterface.h"

extern Cmd *theUndoCmd;  // External object that reverses the 
// most recent Cmd when executed

Cmd *Cmd::_lastCmd = NULL;  // Pointer to most recent Cmd

Cmd::Cmd ( const char *name, int active )
{
    // Initialize all data members
    
    _name              = name;  
    _active            = active;  
    _numInterfaces     = 0;    
    _ci                = NULL;
    _activationList    = NULL;
    _deactivationList  = NULL;
    _hasUndo           = TRUE;
    _value             = NULL;
}

Cmd::Cmd ( const char *name, int active, CmdValue value )
{
    // Initialize all data members
    
    _name              = name;  
    _active            = active;  
    _numInterfaces     = 0;    
    _ci                = NULL;
    _activationList    = NULL;
    _deactivationList  = NULL;
    _hasUndo           = TRUE;
    _value             = value;
    newValue();
}

Cmd::~Cmd()
{
    delete _activationList;	
    delete _deactivationList;	
    delete _ci;
    freeValue(_value);
}

void Cmd::registerInterface ( CmdInterface *ci )
{
    // Make a new list, large enough for the new object
    
    CmdInterface **newList = new CmdInterface*[_numInterfaces + 1];
    
    // Copy the contents of the previous list to
    // the new list
    
    for( int i = 0; i < _numInterfaces; i++)
	newList[i] = _ci[i];
    
    // Free the old list
    
    delete []_ci;
    
    // Install the new list
    
    _ci =  newList;
    
    // Add the object to the list and update the list size.
    
    _ci[_numInterfaces] = ci;
    
    _numInterfaces++;
    
    if ( ci ) {
	if ( _active )
	    ci->activate();
	else
	    ci->deactivate();      

	ci->setValue(_value);
    }
}

void Cmd::unRegisterInterface ( CmdInterface *ci )
{
    // Make a new list, one item smaller than before

    CmdInterface **newList = new CmdInterface*[_numInterfaces - 1];

    // Copy the contents of the previous list to
    // the new list, except for the one that has to be deleted

    int i, j;
    for( i = 0, j = 0; i < _numInterfaces; i++)
	if ( _ci[i] != ci )
	    newList[j++] = _ci[i];

    // Free the old list

    delete []_ci;

    // Install the new list

    _ci =  newList;

    _numInterfaces--;
}

void Cmd::activate()
{
    // Activate the associated interfaces
    
    for ( int i = 0; i < _numInterfaces; i++ )
	_ci[i]->activate ();
    
    // Save the current value of active before setting the new state
    
    _previouslyActive = _active;
    _active = TRUE;
}

void Cmd::deactivate()
{
    // Deactivate the associated interfaces
    
    for ( int i = 0; i < _numInterfaces; i++ )
	_ci[i]->deactivate ();
    
    // Save the current value of active before setting the new state
    
    _previouslyActive = _active;
    _active = FALSE;
}

void Cmd::revert()
{
    // Activate or deactivate, as necessary, 
    // to return to the previous state
    
    if ( _previouslyActive )
	activate();
    else
	deactivate();
}

void Cmd::addToActivationList ( Cmd *cmd )
{
    if ( !_activationList )
	_activationList = new CmdList();
    
    _activationList->add ( cmd );
}

void Cmd::addToDeactivationList ( Cmd *cmd )
{
    if ( !_deactivationList )
	_deactivationList = new CmdList();
    
    _deactivationList->add ( cmd );
}

void Cmd::execute( CmdValue new_value )		// new_value defaults to NULL
{
    int i;      
    
    // If a command is inactive, it cannot be executed
    
    if ( !_active )
	return;
    
    freeValue(_value);
    _value = new_value;

    // Call the derived class's doit member function to 
    // perform the action represented by this object
    
    doit();
    
    // Activate or deactivate the global theUndoCmd, 
    // and remember the last command, as needed
    
    if ( _hasUndo )
    {
	Cmd::_lastCmd = this;
	theUndoCmd->activate();
    }
    else  
    {      
	Cmd::_lastCmd = NULL;
	theUndoCmd->deactivate();
    }
    
    // Process the commands that depend on this one
    
    if ( _activationList )    
	for ( i = 0; i < _activationList->size(); i++ )
	    (*_activationList)[i]->activate();
    
    if ( _deactivationList )    
	for ( i = 0; i < _deactivationList->size(); i++ )
	    (*_deactivationList)[i]->deactivate();

    // Tell all the interfaces about the new value

    newValue();
}

void Cmd::undo()
{
    int i;
    
    // Call the derived class's undoit() member function.
    
    undoit();
    
    // The system only supports one level of undo, and this is it,
    // so deactivate the undo facility.
    
    theUndoCmd->deactivate();
    
    // Reverse the effects of the execute() member function by 
    // reverting all dependent objects to their previous state
    
    if ( _activationList )        
	for ( i = 0; i < _activationList->size(); i++ )
	    (*_activationList)[i]->revert();
    
    if ( _deactivationList )    
	for ( i = 0; i < _deactivationList->size(); i++ )
	    (*_deactivationList)[i]->revert();
}

void Cmd::newValue()
{
    for ( int i = 0; i < _numInterfaces; i++ )
        _ci[i]->setValue(_value);
}

