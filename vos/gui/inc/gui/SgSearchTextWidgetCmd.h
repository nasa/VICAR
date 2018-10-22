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
