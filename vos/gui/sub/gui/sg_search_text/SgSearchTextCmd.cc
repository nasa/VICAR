/////////////////////////////////////////////////////////////////
// SgSearchTextCmd.cc - an abstract base class for text searches;
// This class relies upon the derived class to provide the 
// search() function, which advances the position to the next
// occurance of the text, if any. 
//
// SgSearchTextCmd has a facility for activating/deactivating
// a "search again" cmd (which is optional); the "search again"
// cmd needs to register itself via registerAgainCmd().
//
// textChangedCallback() is provided to allow Motif-based apps
// to notify the searchCmd object when something changes
////////////////////////////////////////////////////////////////

#include "SgSearchTextCmd.h"
#include <string.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include "InfoDialogManager.h"


SgSearchTextCmd::SgSearchTextCmd( const char *name, int active )
                                                    : NoUndoCmd(name, active)
{ 
  _searched_text = NULL;   // derived classes should call getSearchedText()
  _sought_text = NULL;
  _search_again_cmd = NULL;

  _search_type = FromCurrentToBottom;
  _start_pos = 0;         
  _cur_pos = _start_pos;

  _case_sens = 0;
  _already_deactivated = 0;

  _value = (CmdValue) (new SgSearchCmdValue());
  _old_value = NULL;
}


////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////

void SgSearchTextCmd::doit()
{

  _already_deactivated = 0;

  // retrieve search data from the CmdValue's fields
  
  SgSearchCmdValue *currentValue = NULL;
  
  if (_value) { 
    currentValue = new SgSearchCmdValue(*(SgSearchCmdValue *)_value);
    _old_value = currentValue;
  }

  else if (_old_value) {   // _value is null; assume it's a "find next"
                           // and use the previous value, if it exists
    currentValue = _old_value;
  }

  if (currentValue) {

    if( currentValue->text && strlen(currentValue->text) ) {

      _sought_text = strdup(currentValue->text);
      _case_sens = currentValue->case_sens;


      // find the next occurrence, if it exists

      _SearchStatus searchResult = search();

      // FOUND
      if ( searchResult == StatusFound ) { 
	_cur_pos++;                       // found, so exit normally
	if(_search_again_cmd)
	  _search_again_cmd->activate();
      }


      // BOTTOM REACHED
      else if ( searchResult == StatusBottomReached ) {
	_search_type = FromTopToStart;
	_cur_pos = 0;
	searchResult = search();

	if ( searchResult == StatusFound ) {
	  if(_search_again_cmd)
	    _search_again_cmd->activate();
	  _cur_pos++;
	}
      } 

      // ENTIRE TEXT SEARCHED
      if ( searchResult == StatusEntireTextSearched ) {

	char msg[100];
	sprintf(msg, "Entire document searched.");
	theInfoDialogManager->post(msg);

	cleanup();
	_search_type = FromCurrentToBottom;
	_cur_pos = _start_pos;
	
	if (_search_again_cmd)
	  _search_again_cmd->deactivate();
      } 
      
    } 
  } 
}


////////////////////////////////////////////////////////////////
// textChanged() - if the text we're browsing has changed,
//    we can't "search again;" need to start from the beginning
////////////////////////////////////////////////////////////////
void SgSearchTextCmd::textChanged()
{
  // the searchAgainCmd (if present) should have registered itself by now

  if ( !_already_deactivated && _search_again_cmd ) {
    _search_again_cmd->deactivate();
    _already_deactivated = 1;
  }

  cleanup();             
  _sought_text = NULL;    // not useful anymore, since we can't searchAgain
  _cur_pos = _start_pos;  // go back to where we started the search
}


////////////////////////////////////////////////////////////////
// registerAgainCmd() - if a SearchAgainCmd is associated with
//  the search cmd, it will notify us of its existence here
////////////////////////////////////////////////////////////////
void SgSearchTextCmd::registerAgainCmd( Cmd *search_again_cmd )
{
  _search_again_cmd = search_again_cmd;
}


////////////////////////////////////////////////////////////////
// freeValue()
////////////////////////////////////////////////////////////////
void SgSearchTextCmd::freeValue(CmdValue value)
{
  if (value)
    delete (SgSearchCmdValue *)value;
}


////////////////////////////////////////////////////////////////
// textChangedCallback() - allows Motif-based apps to notify us
//                         when something's changed
////////////////////////////////////////////////////////////////
void SgSearchTextCmd::textChangedCallback( Widget, XtPointer clientData,
					   XtPointer )
{
  SgSearchTextCmd *obj = (SgSearchTextCmd *)clientData;
  obj->textChanged();
}
