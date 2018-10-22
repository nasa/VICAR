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
