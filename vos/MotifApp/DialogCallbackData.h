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


////////////////////////////////////////////////////////////////
// DialogCallbackData.h: Auxiliary class used by DialogManager
//////////////////////////////////////////////////////////////
#ifndef DIALOGCALLBACKDATA
#define DIALOGCALLBACKDATA

class DialogManager;

typedef void (*DialogCallback)( void * );

class DialogCallbackData {

  private:
    
    DialogManager  *_dialogManager;
    DialogCallback  _ok;
    DialogCallback  _help;
    DialogCallback  _cancel;
    void           *_clientData;
    
  public:
    
    DialogCallbackData ( DialogManager *dialog, 
			void          *clientData,
			DialogCallback ok,
			DialogCallback cancel,
			DialogCallback help)
    {
	_dialogManager = dialog;
	_ok            = ok;
	_help          = help;
	_cancel        = cancel;
	_clientData    = clientData;
    }
    
    DialogManager  *dialogManager() { return _dialogManager; }
    DialogCallback  ok() { return _ok; }
    DialogCallback  help() { return _help; }
    DialogCallback  cancel() { return _cancel; }
    void           *clientData() { return _clientData; }
};
#endif

