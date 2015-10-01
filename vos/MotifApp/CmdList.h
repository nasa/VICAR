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
// CmdList.h: Maintain a list of Cmd objects
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
// MODIFIED TO INHERIT FROM CMD - not described in Book
///////////////////////////////////////////////////////////

#ifndef CMDLIST_H
#define CMDLIST_H

#include "Cmd.h"

class CmdList : public Cmd {
    
  protected:
    
    Cmd **_contents;    // The list of objects
    CmdValue *_values;	// The corresponding list of CmdValues
    int   _numElements; // Current size of list
    Cmd **_undoContents; // List of objects for undo
    int _numUndoElements;
    virtual void doit();  
    virtual void undoit();  

  public:
 
    CmdList();    
    CmdList(const char *);           // Construct an empty list

    virtual ~CmdList();  // Destroys list and values, but not Cmds in list
    
    virtual void add ( Cmd *, CmdValue = NULL );  // Add a single Cmd to list
    virtual void addUnique ( Cmd *, CmdValue = NULL ); // Add Cmd to list uniquely
    virtual void remove ( Cmd * );		// Remove Cmd from list
    virtual void clear();	// Remove all Cmd's (but not undo)
    virtual void reset();	// Reset all interfaces to stored values
    
    Cmd **contents() { return _contents; } // Return the list
    int size() { return _numElements; }    // Return list size
    Cmd *operator[]( int );            // Return an element of the list
    virtual const char *const className () { return "CmdList"; }
};

#endif

