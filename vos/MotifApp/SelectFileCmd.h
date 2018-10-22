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


///////////////////////////////////////////////////////////////////
// SelectFileCmd.h:  Allow the user to select a file interactively
///////////////////////////////////////////////////////////////////
#ifndef SELECTFILECMD_H
#define SELECTFILECMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>

typedef Boolean (*FileCallback) ( void *, char * );

class SelectFileCmd : public NoUndoCmd {
    
  private:
    
    static void fileSelectedCallback ( Widget, XtPointer, XtPointer );
    static void fileCancelledCallback ( Widget, XtPointer, XtPointer );
    
  protected:
    
    void doit();              // Called by base class
    FileCallback _callback;   // Function to be called 
    // when user selects a file.
    void        *_clientData; // Data provided by caller
    
    Widget      _fileBrowser; // The Motif widget used to get file
    
    virtual Boolean fileSelected ( char * );
    
  public:
    
    SelectFileCmd ( const char *, int , FileCallback, void * );
};
#endif
