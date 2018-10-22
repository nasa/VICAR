///////////////////////////////////////////////////////////////////////////////
// TpShowPointLabelsCmd.h: This command class allow user to specify whether 
// the program should display point labels.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSHOWPOINTLABELSCMD_H
#define TPSHOWPOINTLABELSCMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpShowPointLabelsCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
            if (_value)
                _matchManager->showPointLabels(True);
            else
                _matchManager->showPointLabels(False);
        }

    void undoit()
	{
            if (_value)
                _matchManager->showPointLabels(False);
            else
                _matchManager->showPointLabels(True);
 
            _value = (CmdValue)(!_value);
            newValue();
        }

  public:

    TpShowPointLabelsCmd(const char *name, int active, TpMatchManager *mm) 
	: Cmd(name, active, (CmdValue)True)
	{ _matchManager = mm; }
    virtual ~TpShowPointLabelsCmd() { }

    virtual const char *const className () { return "TpShowPointLabelsCmd";}
};

#endif
