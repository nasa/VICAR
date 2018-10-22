///////////////////////////////////////////////////////////////////////////////
// TpAutoSyncPointsCmd.h: This command class allow user to set status of 
// points collection to "redo" mode that will redo matching as soon as 
// the location of any of the points change.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOSYNCPOINTSCMD_H
#define TPAUTOSYNCPOINTSCMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpAutoSyncPointsCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
	    if (_value)
		_matchManager->setAutoSyncPoints(True);
	    else
		_matchManager->setAutoSyncPoints(False);
	}

    void undoit()
	{
	    if (_value)
		_matchManager->setAutoSyncPoints(False);
	    else
		_matchManager->setAutoSyncPoints(True);
	    
	    _value = (CmdValue)(!_value);
	    newValue();
	}


  public:

    TpAutoSyncPointsCmd(const char *name, int active, TpMatchManager *mm)
	: Cmd(name, active)
	{ _matchManager = mm; }
    virtual ~TpAutoSyncPointsCmd() { }

    virtual const char *const className () { return "TpAutoSyncPointsCmd"; }
};

#endif
