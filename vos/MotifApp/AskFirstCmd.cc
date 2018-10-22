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


//////////////////////////////////////////////////////////
// AskFirstCmd.C
//////////////////////////////////////////////////////////
#include "AskFirstCmd.h"
#include "QuestionDialogManager.h"
#include "stdio.h"

#define DEFAULTQUESTION "Do you really want to execute this command?"

AskFirstCmd::AskFirstCmd ( const char *name, int active ) : Cmd ( name, active )
{
    _question = NULL;
    setQuestion ( DEFAULTQUESTION );
    _saved_value = NULL;
}

void AskFirstCmd::setQuestion ( const char *str )
{
    delete _question;
    _question = strdup ( str );
}

void AskFirstCmd::execute( CmdValue new_value)
{
    _saved_value = new_value;
    theQuestionDialogManager->post ( _question,
				    (void *) this,
				    &AskFirstCmd::yesCallback, 
				    &AskFirstCmd::noCallback );
}       

void AskFirstCmd::yesCallback ( void *clientData )
{
    AskFirstCmd *obj = (AskFirstCmd *) clientData;

    // Call the base class execute() member function 
    // to do all the usual processing of the command

    obj->Cmd::execute(obj->_saved_value);
}

void AskFirstCmd::noCallback ( void *clientData )
{
    AskFirstCmd *obj = (AskFirstCmd *) clientData;

    if (obj->_saved_value)
	obj->freeValue(obj->_saved_value);
}

