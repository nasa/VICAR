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


/////////////////////////////////////////////////////////
// Cmd.h: A base class for all command objects
/////////////////////////////////////////////////////////
#ifndef CMD_H
#define CMD_H

#include "CmdValue.h"

// Note: C++ wants NULL defined as 0, not (void *)0.

#ifndef NULL
#define NULL (0)
#endif

class CmdList;
class CmdInterface;

class Cmd {
    
  private:
    
    // Lists of other commands to be activated or deactivated
    // when this command is executed or "undone"
    
    CmdList       *_activationList;
    CmdList       *_deactivationList;
    void            revert();   // Reverts object to previous state
    int            _previouslyActive; // Previous value of _active
    const char    *_name;             // Name of this Cmd
    CmdInterface **_ci;            
    int            _numInterfaces;

  protected:
    
    int            _active;     // Is this command currently active?
    int           _hasUndo;    // True if this object supports undo
    static Cmd   *_lastCmd;    // Pointer to last Cmd executed

    CmdValue _value;		// Value from interface
    
    virtual void doit()   = 0;  // Specific actions must be defined
    virtual void undoit() = 0;  // by derived classes
    
  public:
    
    Cmd ( const char *,  int );		// Protected constructor
    Cmd ( const char *,  int, CmdValue );	// with starting value
    
    virtual ~Cmd ();                 // Destructor
    
    // public interface for executing and undoing commands
    
    virtual void execute( CmdValue = NULL );
    virtual void undo();
    
    void    activate();   // Activate this object
    void    deactivate(); // Deactivate this object
    
    // Functions to register dependent commands
    
    void    addToActivationList ( Cmd * );
    void    addToDeactivationList ( Cmd * );
    
    // Register an UIComponent used to execute this command
    
    void    registerInterface ( CmdInterface * );
    void    unRegisterInterface ( CmdInterface * );
    
    // Function to free a CmdValue, if needed.  This should be overridden
    // by any subclass that dynamically allocates values.

    virtual void freeValue ( CmdValue )  { }

    // Reset means to send the current value back out to all interfaces,
    // so they reflect the current state.  Useful for Cancel from dialogs.

    virtual void reset() { newValue(); }

    virtual void newValue();

    // Access functions 
    
    int active () { return _active; }
    int hasUndo() { return _hasUndo; }
    virtual void *getValue() { return _value; }
    const char *const name () { return _name; }
    virtual const char *const className () { return "Cmd"; }
};

#endif
