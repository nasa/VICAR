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
// CmdInterface.h
///////////////////////////////////////////////////////
#ifndef CMDINTERFACE
#define CMDINTERFACE
#include "UIComponent.h"
#include "CmdValue.h"

class Cmd;
class CmdList;

class CmdInterface : public UIComponent {
    
    friend class Cmd;
    
  protected:
    
    Cmd    *_cmd;
    CmdList *_deferredList;
    
    static void executeCmdCallback ( Widget, 
				    XtPointer, 
				    XtPointer );
    virtual void executeCmd( XtPointer = NULL );
    void runCmd ( CmdValue = NULL );
    
    int _active;
    
    CmdInterface ( Cmd * );
    CmdInterface ( const char * );		// For delayed setting of Cmd
    CmdInterface ( Cmd *, const char * );	// For optional Cmd

    virtual void activate();
    virtual void deactivate();

  public:

    virtual ~CmdInterface();

    void setCommand ( Cmd * );		// For delayed setting of Cmd only

    void setDeferredExec ( CmdList * ); // Defers execution for later

    virtual void setValue ( CmdValue );	// Set interface to match another
};
#endif
