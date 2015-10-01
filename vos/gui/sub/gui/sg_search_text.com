$!****************************************************************************
$!
$! Build proc for MIPL module sg_search_text
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:35
$!
$! Execute by entering:		$ @sg_search_text
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sg_search_text ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sg_search_text.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sg_search_text.imake") .nes. ""
$   then
$      vimake sg_search_text
$      purge sg_search_text.bld
$   else
$      if F$SEARCH("sg_search_text.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sg_search_text
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sg_search_text.bld "STD"
$   else
$      @sg_search_text.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_search_text.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_search_text.com -mixed -
	-s SgSearchTextCmd.cc SgSearchTextWidgetCmd.cc -
	   SgSearchTextCmdInterface.cc SgSearchTextDialog.cc -
	   SgSearchCmdValue.cc SgSearchAgainCmd.cc SgSaveTextWidgetCmd.cc -
	-i sg_search_text.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgSearchTextCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextWidgetCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgSearchTextDialog.cc:  A dialog for SgSearchTextCmd objects;
// overrides CustomDialog::apply for immediate execution of a 
// single command (as opposed to the deferred list)
////////////////////////////////////////////////////////////////

#include "SgSearchTextDialog.h"

String SgSearchTextDialog::_defaults[] = {
  (char *)"*OK*labelString: Find",
  NULL
};


SgSearchTextDialog::SgSearchTextDialog(const char *name, Cmd *cmd)
  : SgCmdDialog(name, cmd, Default, Invisible, Invisible, 
		Visible, Invisible)
{
  _ci = NULL;
  _command = cmd;
}


CmdInterface *SgSearchTextDialog::createCmdInterface(Widget parent, Cmd *cmd)
{
  
  setDefaultResources(parent, _defaults);
  _ci = new SgSearchTextCmdInterface( parent, cmd );

  return _ci;
}  

//////////////////////////////////////////////////////////
// Unlike CustomDialog, our apply() only executes a single
// Cmd, not a cmdList
//////////////////////////////////////////////////////////
 
void SgSearchTextDialog::apply()
{
  if (_command)
    _ci->triggerCmd();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchCmdValue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SgSearchCmdValue.h: Contains parameters needed for a text search.
// Objects of this class are used by SgSearchText* objects
///////////////////////////////////////////////////////////////////

#include "SgSearchCmdValue.h"
#include <iostream>
using namespace std;
#include <stdio.h>
#include <string.h>

///////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::SgSearchCmdValue()
{
  text = NULL;
  case_sens = 0;
}


///////////////////////////////////////////////////////////////////
// Destructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::~SgSearchCmdValue()
{
  if (text)
    delete []text;
} 

///////////////////////////////////////////////////////////////////
// Copy constructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::SgSearchCmdValue(SgSearchCmdValue &val)
{
  text = NULL;
  if (val.text) {
    text = new char[strlen(val.text)+1];
    strcpy(text, val.text);
  }
  case_sens = val.case_sens;
}

///////////////////////////////////////////////////////////////////
// Assignment operator
///////////////////////////////////////////////////////////////////
SgSearchCmdValue &SgSearchCmdValue::operator=(SgSearchCmdValue &val)
{
    if (this == &val)
	return *this;		// assignment to self
    
    if (text)
	delete []text;
    
    text = NULL;

    if (val.text) {
	text = new char[strlen(val.text)+1];
	strcpy(text, val.text);
    }

    case_sens = val.case_sens;
    
    return *this;
}

///////////////////////////////////////////////////////////////////
// Equality operator
///////////////////////////////////////////////////////////////////
int SgSearchCmdValue::operator==(SgSearchCmdValue &val)
{
    if (this == &val)
        return 1;
 
    if (case_sens != val.case_sens) 
      return 0;
    else {
      if (text && val.text) {
	if (strlen(text) != strlen(val.text))
	  return 0;
	else if ((text && !val.text) || (!text && val.text))
	  return 0;
	else if (!strcmp(text, val.text))
	  return 0;
      }
      return 1;
    }
}





$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchAgainCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SgSearchAgainCmd.h - a simple cmd to execute an SgSearchTextCmd
///////////////////////////////////////////////////////////////////

#include "SgSearchAgainCmd.h"

SgSearchAgainCmd::SgSearchAgainCmd( const char *name, int active, 
				    SgSearchTextCmd *search_cmd ) 
  : NoUndoCmd(name, active)
{
  _search_cmd = search_cmd;
  
  // notify the main search cmd of our existence
  _search_cmd->registerAgainCmd(this);
}

void SgSearchAgainCmd::doit()
{
  _search_cmd->execute();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSaveTextWidgetCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SgSaveTextWidgetCmd.cc - writes the contents of a Motif
// text widget to a file
////////////////////////////////////////////////////////////////

#include "SgSaveTextWidgetCmd.h"
#include <Xm/Text.h>
#include <fstream>
#include <iostream>

SgSaveTextWidgetCmd::SgSaveTextWidgetCmd( const char *name, int active, 
		      Widget textWidget ) : NoUndoCmd(name, active)
{
  _textWidget = textWidget;   // the widget whose text we're saving
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void SgSaveTextWidgetCmd::doit()
{
  char *filename = (char *)_value;  
  char *logText = NULL;

  // Get the text from the output window
  
  if ((logText = XmTextGetString(_textWidget))) {
    
    // Save text to the specified file; ios::out will OVERWRITE existing file
    std::ofstream outputFile(filename, std::ios::out);
    outputFile << logText << std::endl;
    outputFile.close();
    
    // free the memory using XtFree()
    XtFree(logText);
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sg_search_text.imake
#define SUBROUTINE sg_search_text 
#define MODULE_LIST SgSearchTextCmd.cc SgSearchTextWidgetCmd.cc \
	SgSearchTextCmdInterface.cc SgSearchCmdValue.cc \
	SgSaveTextWidgetCmd.cc SgSearchTextDialog.cc \
	SgSearchAgainCmd.cc

#define GUI_SUBLIB
 
#define USES_C_PLUS_PLUS
 
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_RTL
$ Return
$!#############################################################################
