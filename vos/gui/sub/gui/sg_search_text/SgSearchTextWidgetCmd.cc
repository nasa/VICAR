/////////////////////////////////////////////////////////////////
// SgSearchTextWidgetCmd.cc - A modifed SgSearchTextCmd, with
// facilites for searching Motif text widgets
////////////////////////////////////////////////////////////////

#include "SgSearchTextWidgetCmd.h"
#include <Xm/Text.h>
#include <X11/Xos.h>
#include <string.h>
#include "InfoDialogManager.h"
#include "strncasecmp.h"

SgSearchTextWidgetCmd::SgSearchTextWidgetCmd( const char *name, int active, 
					      Widget textWidget )
  : SgSearchTextCmd(name, active)
{
  _textWidget = textWidget;
  _hlightBegin = 0;
  _hlightEnd = 0;

  _searched_text = strdup( SgSearchTextWidgetCmd::getSearchedText() );

  // register our callback so that we'll be notified when the text changes
  XtAddCallback( _textWidget, XmNvalueChangedCallback,
		 &SgSearchTextWidgetCmd::textChangedCallback,
		 (XtPointer)this );
}


////////////////////////////////////////////////////////////////
// cleanup() -- removes hilighting, if any
////////////////////////////////////////////////////////////////
void SgSearchTextWidgetCmd::cleanup()
{
  if ( _hlightEnd ) {        // if the end pos. isn't 0,
                             // then there *is* a highlight
    
    XmTextSetHighlight(_textWidget, _hlightBegin, _hlightEnd,
		       XmHIGHLIGHT_NORMAL);       // so UN-highlight it
  }
}

////////////////////////////////////////////////////////////////
// search() - finds the next occurance of _sought_text
////////////////////////////////////////////////////////////////
_SearchStatus SgSearchTextWidgetCmd::search()
{

  cleanup();   // remove any previous hilighting
  
  _searched_text = strdup( SgSearchTextWidgetCmd::getSearchedText() );

  char *p;
  int count;
  int sought_strlen = strlen(_sought_text);
  int found = 0;
  int widget_strlen = strlen(_searched_text);

  // set end_pos based on _search_type

  if(_search_type == FromCurrentToBottom)
    _end_pos = widget_strlen;
  else
    _end_pos = _start_pos;    // this is a wrapped search

  
  // begin search at current position

  p = &_searched_text[_cur_pos];     // point to the current pos.
  
  int init_pos = _cur_pos;

  for( count = 0; count < (widget_strlen - init_pos); count++ ) {
    if (_cur_pos == _end_pos) 
      return StatusEntireTextSearched; //return StatusNotFound;
    else if(((_case_sens == 0)&&(!strncasecmp(p, _sought_text, sought_strlen)))
	   ||((_case_sens == 1) && (!strncmp(p, _sought_text, sought_strlen))))
      {
	found = True;
	break;
      }
    p++;
    _cur_pos++;	 
  } 
  
  if (found) 
  {
    XmTextSetInsertionPosition(_textWidget, _cur_pos);
    
    _hlightBegin = _cur_pos;
    _hlightEnd = _cur_pos + sought_strlen;
    
    // ensure that _sought_text is visible in the widget
    XmTextShowPosition(_textWidget, _hlightBegin);
    XmTextShowPosition(_textWidget, _hlightEnd);
    
    XmTextSetHighlight(_textWidget, _hlightBegin, _hlightEnd, 
		       XmHIGHLIGHT_SECONDARY_SELECTED);

    return StatusFound;
  }
  else  // we've reached the bottom and didn't find it
  {
    return StatusBottomReached;// return StatusNotFound;
  }
}


void SgSearchTextWidgetCmd::searchCallback(void *clientData)
{
  SgSearchTextWidgetCmd *obj = (SgSearchTextWidgetCmd *)clientData;
  obj->search();
}

char *SgSearchTextWidgetCmd::getSearchedText()
{
  return XmTextGetString(_textWidget);
}
