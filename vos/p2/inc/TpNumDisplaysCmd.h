////////////////////////////////////////////////////////////
// TpNumDisplaysCmd.h: Set number of displays to either one, 
// two, or three.
////////////////////////////////////////////////////////////
#ifndef TpNumDisplaysCmd_H
#define TpNumDisplaysCmd_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpDisplayer;

class TpNumDisplaysCmd : public RadioCmd {

  protected:

    TpDisplayer *_displayer;
    int _numDisplays;

    virtual void doit();
    
  public:

    TpNumDisplaysCmd(const char *, int, CmdValue, CmdList *, 
			TpDisplayer *, int);

    virtual const char *const className () { return "TpNumDisplaysCmd"; }
};
#endif
