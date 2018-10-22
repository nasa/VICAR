///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmd.h: This command class allow user to set the match 
// mode values.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODEVALUESCMD_H
#define TPSETMATCHMODEVALUESCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchModeValuesCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchModeValuesCmd(const char *name, int active, CmdValue value, 
			    TpMatchManager *);
    virtual ~TpSetMatchModeValuesCmd() { }

    virtual const char *const className () {return "TpSetMatchModeValuesCmd";}
};

#endif
