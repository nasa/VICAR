$!****************************************************************************
$!
$! Build proc for MIPL module sg_search_text_h
$! VPACK Version 1.9, Friday, January 16, 1998, 11:13:27
$!
$! Execute by entering:		$ @sg_search_text_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sg_search_text_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sg_search_text_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sg_search_text_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sg_search_text_h.bld "STD"
$   else
$      @sg_search_text_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_search_text_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_search_text_h.com -mixed -
	-s SgSearchTextCmd.h SgSearchTextWidgetCmd.h SgSearchTextCmdInterface.h -
	   SgSearchTextDialog.h SgSearchCmdValue.h SgSearchAgainCmd.h -
	   SgSaveTextWidgetCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgSearchTextCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SgSearchTextCmd.h - an abstract base class for text searches;
// This class relies upon the derived class to provide the 
// search() function, which advances the position to the next
// occurance of the text, if any, and returns a value.
//
// SgSearchTextCmd has a facility for activating/deactivating
// a "search again" cmd (which is optional); the "search again"
// cmd needs to register itself via registerAgainCmd().
//
// textChangedCallback() is provided to allow Motif-based apps
// to notify the searchCmd object when something changes
////////////////////////////////////////////////////////////////
#ifndef SGSEARCHTEXTCMD_H
#define SGSEARCHTEXTCMD_H

#include "NoUndoCmd.h"
#include "SgSearchCmdValue.h"
#include <Xm/Xm.h>

enum _SearchStatus { 
  StatusFound, 
  StatusBottomReached,
  StatusEntireTextSearched
};

enum _SearchType {
  FromTopToStart,
  FromCurrentToBottom
};

class Cmd;

class SgSearchTextCmd: public NoUndoCmd {
  

  protected:

    SgSearchCmdValue *_old_value; // used for "search again"

    _SearchType _search_type;     

    char *_sought_text;           // the text we want to find
    char *_searched_text;         // the text *through* which we're searching

    Cmd *_search_again_cmd;       // (optional) the associated SearchAgainCmd

    int  _start_pos, _cur_pos;    // treated as offsets from the beginning 
                                  // of the text

    int _case_sens;               // True if it *is* case-sensitive

    int _already_deactivated;     // indicates whether we need to deactivate
                                  // the _search_again_cmd

    int _end_pos;                 // indicates where the derived class should
                                  // stop searching

    virtual void doit();

    // The following functions are application-specific and must be
    // supplied by derived classes

    virtual char *getSearchedText() = 0;
                                  // fetches/returns a ptr to _searched_text

    virtual _SearchStatus search() = 0;    
                                  // finds the next occurance of _sought_text

    virtual void cleanup() = 0;   // performs app-specific cleanup;
                                  // e.g., un-highlighting

  public:

    static void textChangedCallback(Widget, XtPointer, XtPointer);

    SgSearchTextCmd( const char *, int );

    virtual void freeValue(CmdValue);

    virtual const char *const className() { return ("SgSearchTextCmd"); }

    virtual void textChanged();   // called if the searched text has changed

    virtual void registerAgainCmd(Cmd *);
                                  // if a SearchAgainCmd is used, it registers
                                  // itself via this fcn
    virtual ~SgSearchTextCmd() { }
};


#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextWidgetCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SgSearchTextWidgetCmd.h - A modifed SgSearchTextCmd, with
// facilites for searching Motif text widgets
////////////////////////////////////////////////////////////////

#ifndef SGSEARCHTEXTWIDGETCMD_H
#define SGSEARCHTEXTWIDGETCMD_H

#include "SgSearchTextCmd.h"
#include <Xm/Xm.h>

class Cmd;

class SgSearchTextWidgetCmd: public SgSearchTextCmd {
  
  private:

    static void searchCallback(void *);
    
  protected:

    Widget _textWidget;
    virtual char *getSearchedText();   // returns a ptr to the searched text;
				       // Motif-specific

    virtual _SearchStatus search();    // the "guts" of the search; 
                                       // Motif-specific

    int _hlightBegin, _hlightEnd;  

    virtual void cleanup();            // removes highlighting, if any

  public:

    SgSearchTextWidgetCmd( const char *, int, Widget );

    virtual const char *const className() { return ("SgSearchTextWidgetCmd"); }

    virtual ~SgSearchTextWidgetCmd() { }
}; 
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgSearchTextCmdInterface.h: Derived from CmdInterface,
// this class will fill in the SearchCmdValue structure and
// calls the Cmd with filled-in value.
//////////////////////////////////////////////////////////////
#ifndef SGSEARCHTEXTCMDINTERFACE
#define SGSEARCHTEXTCMDINTERFACE

#include "CmdInterface.h"
#include "SgSearchCmdValue.h"
#include "Cmd.h"

class SgSearchTextCmdInterface : public CmdInterface {

  protected:

    SgSearchCmdValue *_searchCmdValue;
  
    Widget _textLabel;                    // label (for the text field)
    Widget _textField;                    // takes user input

    Widget _checkBox;                     // toggles case-sensitivity

    char *_prev_sought_text; 

    virtual void executeCmd(XtPointer = NULL);
                                          // fills in our specialized
                                          // CmdValue and calls runCmd()
   public:
    
     SgSearchTextCmdInterface(Widget, Cmd *);
     virtual ~SgSearchTextCmdInterface() { }
     
     virtual void setValue(CmdValue);    // update the interface
     virtual char *getText();            // fetches & returns widget contents
     virtual int  getCaseSens();         // fetches & returns case sens state
     virtual void triggerCmd();          // allows other entities to execute
                                         // the Cmd
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchTextDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgSearchTextDialog.h:  A dialog for SgSearchTextCmd objects;
// overrides CustomDialog::apply for immediate execution of a 
// single command (as opposed to the deferred list)
////////////////////////////////////////////////////////////////
 
#ifndef SGSEARCHTEXTDIALOG_H
#define SGSEARCHTEXTDIALOG_H
 
#include "SgCmdDialog.h"
#include "SgSearchTextCmdInterface.h"

class SgSearchTextDialog : public SgCmdDialog {
 
 protected:
   static String _defaults[];
   Cmd *_command;
   SgSearchTextCmdInterface *_ci;

   virtual void apply();         // executes a Cmd, not a CmdList

   virtual CmdInterface *createCmdInterface(Widget, Cmd *);

   virtual void setCmdIfDeferredExec() { }
 
 public:
 
   SgSearchTextDialog(const char *, Cmd *);
 
   virtual ~SgSearchTextDialog() { };
 
   virtual CmdInterface *getCmdInterface() { return _ci; }

};
 
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchCmdValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SgSearchCmdValue.h: Contains parameters needed for a text search.
// Objects of this class are used by SgSearchText* objects
///////////////////////////////////////////////////////////////////

#ifndef SGSEARCHCMDVALUE_H
#define SGSEARCHCMDVALUE_H

struct SgSearchCmdValue {

    int case_sens;       // 1 if it *is* case sensitive; 0 if not

    char *text;          // the text we seek
    
    SgSearchCmdValue();
    SgSearchCmdValue(SgSearchCmdValue &);		// copy ctor
    ~SgSearchCmdValue ();

    SgSearchCmdValue &operator=(SgSearchCmdValue &val);
    int operator==(SgSearchCmdValue &val);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSearchAgainCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SgSearchAgainCmd.h - a simple cmd to execute an SgSearchTextCmd
//////////////////////////////////////////////////////////////////
#ifndef SGSEARCHAGAINCMD
#define SGSEARCHAGAINCMD

#include "NoUndoCmd.h"
#include "SgSearchTextCmd.h"

class SgSearchAgainCmd: public NoUndoCmd {
  
  protected:
    
    virtual void doit();
    SgSearchTextCmd *_search_cmd;

  public:

    SgSearchAgainCmd( const char *, int, SgSearchTextCmd *);
    virtual ~SgSearchAgainCmd() { }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgSaveTextWidgetCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SgSaveTextWidgetCmd.h - writes the contents of a Motif
// text widget to a file
////////////////////////////////////////////////////////////////

#ifndef SGSAVETEXTWIDGETCMD_H
#define SGSAVETEXTWIDGETCMD_H

#include "NoUndoCmd.h"
#include <Xm/Xm.h>

class Cmd;

class SgSaveTextWidgetCmd: public NoUndoCmd {

  protected:

    Widget _textWidget;

  public:

    SgSaveTextWidgetCmd( const char *, int, Widget );
    virtual void doit();
    virtual const char *const className() { return ("SgSaveTextWidgetCmd"); }
    virtual void freeValue(CmdValue value) 
                                      { if (value) delete [] (char *)value; }
};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
