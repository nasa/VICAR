/////////////////////////////////////////////////////////
// TpPrintCmd.h
/////////////////////////////////////////////////////////
#ifndef TPPRINTCMD_H
#define TPPRINTCMD_H

#include "NoUndoCmd.h"
#include "SgPrintValue.h"

class TpDisplayer;

class TpPrintCmd : public NoUndoCmd {

  protected:

    TpDisplayer *_displayer;

    SgPrintValue *_printValue;
    
  public:

    TpPrintCmd(const char *, int, TpDisplayer *displayer,
	       SgPrintValue *printValue=NULL);
    
    virtual void doit(); 
    
    virtual void freeValue(CmdValue value) 
	{ if (value) delete (SgPrintValue *)value; }
    
    virtual const char *const className () { return "TpPrintCmd"; }
};
#endif
