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


///////////////////////////////////////////////////////////
// QuitCmd.C: Exit an application after checking with user.
//////////////////////////////////////////////////////////
#include "QuitCmd.h"
#include <stdlib.h>

#define QUITQUESTION "Do you really want to exit?"

QuitCmd::QuitCmd ( const char *name, int active ) : 
                    WarnNoUndoCmd ( name, active )
{
    setQuestion ( QUITQUESTION );
}
QuitCmd::QuitCmd ( const char *name, int active, const char *msg) :
			WarnNoUndoCmd (name, active ) 
{
    setQuestion ( msg );
}

void QuitCmd::doit()
{
    // Just exit
    
    exit ( 0 );
}       



