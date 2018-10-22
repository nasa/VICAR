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
