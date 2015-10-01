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
// InterruptibleCmd.h: Abstract class that supports lengthy,
//                     user-interruptible activities
//////////////////////////////////////////////////////////////
#ifndef INTERRUPTIBLECMD_H
#define INTERRUPTIBLECMD_H
#include <Xm/Xm.h>
#include "NoUndoCmd.h"

// Define a type for the callback invoked when the task is finished

class InterruptibleCmd;

typedef void (*TaskDoneCallback) ( InterruptibleCmd *, Boolean, void * );

class InterruptibleCmd : public NoUndoCmd {
    
  protected:
    
    XtWorkProcId     _wpId;         // The ID of the workproc
    TaskDoneCallback _callback;     // Application-defined callback
    void            *_clientData;
    Boolean workProc ();
    static Boolean  workProcCallback ( XtPointer );
    static void     interruptCallback ( void * );
    void interrupt(); 
    
    Boolean      _done;          // TRUE if the task has been completed
    virtual void cleanup();      // Called when task ends
    virtual void abortCleanup(); // Called when task is interrupted
    virtual void updateMessage ( const char * );
    
    // Derived classes must implement doit(), declared by Cmd
    
  public:
    
    InterruptibleCmd ( const char * , int );
    virtual ~InterruptibleCmd();
    
    virtual void execute(CmdValue=NULL); // Overrides base class member function
    virtual void execute ( TaskDoneCallback, void * );
};
#endif
