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


////////////////////////////////////////////////////////////
// CmdList.C: Maintain a list of Cmd objects
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
// MODIFIED TO INHERIT FROM CMD - not described in Book
///////////////////////////////////////////////////////////


#include "CmdList.h"
#include <string.h>

CmdList::CmdList() : Cmd("CmdList", 1)
{
    _contents = 0;
    _values = 0;
    _numElements = 0;
    _undoContents = 0;
    _numUndoElements = 0;
}

CmdList::CmdList(const char *name ) : Cmd(name, 1)
{
    // The list is initially empty
    
    _contents    = 0;
    _values      = 0;
    _numElements = 0;
    _undoContents = 0;
    _numUndoElements = 0;
}

CmdList::~CmdList()
{
    // free the list
    
    int i;
    for (i = 0; i < _numElements; i++)
        _contents[i]->freeValue(_values[i]);
    delete []_values;
    delete []_contents;
    if (_undoContents)
        delete _undoContents;
}

///////////////////////////////////////////////////////////

void CmdList::add ( Cmd *cmd, CmdValue value )	// value defaults to NULL
{
    int i;
    Cmd **newList;
    CmdValue *newValues;

    // CmdList can only be undone if all Cmds it contains can be undone
    
    if(!cmd->hasUndo())
	_hasUndo = 0;
    
    // Allocate a list large enough for one more element
    
    newList = new Cmd*[_numElements + 1];
    newValues = new CmdValue[_numElements + 1];
    
    // Copy the contents of the previous list to
    // the new list
    
    for( i = 0; i < _numElements; i++) {
	newList[i] = _contents[i];
        newValues[i] = _values[i];
    }
    
    // Free the old list
    
    delete []_contents;
    delete []_values;
    
    // Make the new list the current list
    
    _contents =  newList;
    _values = newValues;
    
    // Add the command to the list and update the list size.
    
    _contents[_numElements] = cmd;
    _values[_numElements] = value;
    
    _numElements++;
}

///////////////////////////////////////////////////////////
// Add the command if it doesn't already exist.  If it does, update the
// value.
///////////////////////////////////////////////////////////

void CmdList::addUnique( Cmd *cmd, CmdValue value )   // value defaults to NULL
{
    int i;

    for (i=0; i<_numElements; i++) {
        if (cmd == _contents[i]) {
            _contents[i]->freeValue(_values[i]);
            _values[i] = value;
            return;
        }
    }

    add(cmd, value);
}

///////////////////////////////////////////////////////////
// Remove all occurrences (should only be one, really) of cmd from the list
///////////////////////////////////////////////////////////

void CmdList::remove( Cmd *cmd )
{
    int i;

    for (i=0; i<_numElements; i++) {
        if (cmd == _contents[i]) {		// Found the one to delete
            for (int j=i; j<_numElements-1; j++) {
                _contents[j] = _contents[j+1];
                _values[j] = _values[j+1];
            }
            _numElements--;
            i--;			// Repeat the check for this index
        }
    }
}

///////////////////////////////////////////////////////////
// Clear the command list, but leave the undo list around.
///////////////////////////////////////////////////////////

void CmdList::clear()
{
    int i;
    for (i = 0; i < _numElements; i++)
        _contents[i]->freeValue(_values[i]);
    delete []_values;
    delete []_contents;
    _values = 0;
    _contents = 0;
    _numElements = 0;
}

///////////////////////////////////////////////////////////
// Call newValue() on all Cmd's in the list so the interfaces get set back
// to the current "saved" values.  Useful for Cancel from dialogs.
///////////////////////////////////////////////////////////

void CmdList::reset()
{
    int i, nEl;
    Cmd **saveList;

    saveList = new Cmd*[_numElements];
    memcpy(saveList, _contents, _numElements * sizeof(Cmd *));
    nEl = _numElements;

    for (i=0; i<nEl; i++) {
        saveList[i]->reset();
    }

    delete []saveList;
}

///////////////////////////////////////////////////////////

Cmd *CmdList::operator[] ( int index )
{
    // Return the indexed element
    
    return _contents[index];
}


///////////////////////////////////////////////////////////
// Because executing commands might modify the list, we must copy the
// list first!  We must also save the list so Undo will use the same set.
// Freeing of values is not a problem because the commands only take
// themselves off the list, so the value is valid until we get to each
// command.  Undo doesn't need values so we don't save them.
///////////////////////////////////////////////////////////

void CmdList::doit()
{
    CmdValue *saveValues;

    if (_undoContents)
        delete []_undoContents;

    _undoContents = new Cmd*[_numElements];
    saveValues = new CmdValue[_numElements];
    memcpy(_undoContents, _contents, _numElements * sizeof(Cmd *));
    memcpy(saveValues, _values, _numElements * sizeof(Cmd *));
    _numUndoElements = _numElements;

    for ( int i = 0; i < _numUndoElements; i++)
	_undoContents[i]->execute(saveValues[i]);

    delete []saveValues;
}

///////////////////////////////////////////////////////////

void CmdList::undoit()
{
    if(_hasUndo) {

	for( int i = _numUndoElements - 1; i >=0; i--)
	    _undoContents[i]->undo();

    }
}
