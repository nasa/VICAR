//////////////////////////////////////////////////////////////
// SgSearchTextCmdInterface.cc: Derived from CmdInterface,
// this class will fill in the SearchCmdValue structure and
// calls the Cmd with filled-in value.
//////////////////////////////////////////////////////////////

#include "SgSearchTextCmdInterface.h"
#include "SgSearchTextCmd.h"

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>

class SgSearchCmdValue;


SgSearchTextCmdInterface::SgSearchTextCmdInterface ( Widget parent, Cmd *cmd )
  : CmdInterface ( cmd )
{
  _prev_sought_text = NULL;

  _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, NULL );
  installDestroyHandler();

  // get current value from Cmd object

  _searchCmdValue = new SgSearchCmdValue 
                                   (*((SgSearchCmdValue *)(_cmd->getValue())));

  // set up label + textField widgets

  Widget rowCol = XtVaCreateManagedWidget
                     ( "Label Row Col",
		       xmRowColumnWidgetClass,    _w,
		       XmNorientation,            XmHORIZONTAL,
		       XmNtopAttachment,          XmATTACH_FORM,
		       XmNleftAttachment,         XmATTACH_FORM,
		       NULL );
		      
  _textLabel = XtVaCreateManagedWidget
                       ( "Search string",
			 xmLabelWidgetClass,         rowCol,
			 NULL );

  _textField = XtVaCreateManagedWidget
                       ( "Sought Text Field",
			 xmTextFieldWidgetClass,   rowCol,
			 NULL );

  _checkBox = XtVaCreateManagedWidget
                       ( "Case Sensitive",
			 xmToggleButtonWidgetClass, _w,
			 XmNtopAttachment,          XmATTACH_WIDGET,
			 XmNtopWidget,              rowCol,
			 XmNleftAttachment,         XmATTACH_FORM,
			 XmNleftOffset,           10,
			 XmNtopOffset,            5,
			 XmNset,                  False,
			 NULL );

  XtAddCallback ( _textField, XmNactivateCallback,
		  &CmdInterface::executeCmdCallback,
		  (XtPointer)this );
  
  XtAddCallback ( _textField, XmNvalueChangedCallback,
		  &SgSearchTextCmd::textChangedCallback,
		  (XtPointer) cmd );
  
  setValue( _cmd->getValue() );  // update the interface, now that the
                                 // widgets have all been created
}


void SgSearchTextCmdInterface::setValue(CmdValue value)
{
  // we keep track of _prev_sought_text so that we don't unnecessarily
  // reset the text field every time setValue is called--the Motif
  // callback would trigger a call to SgSearchTextCmd::textChanged()

  if (value != NULL)
    *_searchCmdValue = *((SgSearchCmdValue *)value);
  
  // set _textField to reflect the current search string

  if( _searchCmdValue->text && _prev_sought_text &&
      !strcmp(_searchCmdValue->text, _prev_sought_text) ) {

    XmTextFieldSetString( _textField, _searchCmdValue->text );
    
    _prev_sought_text = strdup(_searchCmdValue->text);
  }

  // set _checkBox to reflect the current case-sensitivity state

  XmToggleButtonSetState( _checkBox, _searchCmdValue->case_sens, True );
}


void SgSearchTextCmdInterface::executeCmd(XtPointer)
{
  SgSearchCmdValue *searchVal = new SgSearchCmdValue();

  // fill in the appropriate fields in the specialized CmdValue structure
  
  searchVal->text = XmTextFieldGetString(_textField);
  searchVal->case_sens = (int) XmToggleButtonGetState(_checkBox);

  // run the actual cmd, now that the search info has been specified
  runCmd( (CmdValue) searchVal );
}

char *SgSearchTextCmdInterface::getText()
{
  return XmTextFieldGetString(_textField);
}

int SgSearchTextCmdInterface::getCaseSens()
{
  return (int) XmToggleButtonGetState(_checkBox);
}

void SgSearchTextCmdInterface::triggerCmd()
{
  XtPointer dummy = NULL;
  executeCmd(dummy);
}
