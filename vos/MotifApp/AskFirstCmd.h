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
// AskFirstCmd.h: Base class for Cmds that ask for confirmation
////////////////////////////////////////////////////////////////
#ifndef ASKFIRSTCMD_H
#define ASKFIRSTCMD_H
#include "Cmd.h"

class AskFirstCmd : public Cmd {
    
  private:
    
    // Callback for the yes choice on the dialog
    
    static void yesCallback ( void * );
    static void noCallback ( void * );
    
    //  Derived classes should use setQuestion to change
    // the string displayed in the dialog
    
    char *_question;

    CmdValue _saved_value;

#ifndef CPLUSPLUS2_1
  protected:    

    virtual void doit()   = 0;  // Specific actions must be defined    
    virtual void undoit()   = 0;  // Specific actions must be defined    
#endif

  public:
    
    AskFirstCmd ( const char *, int );
    
    void setQuestion ( const char *str );
    
    virtual void execute(CmdValue = NULL); // Overrides the Cmd member function
    
    virtual const char *const className ()  { return "AskFirstCmd"; }
};
#endif
