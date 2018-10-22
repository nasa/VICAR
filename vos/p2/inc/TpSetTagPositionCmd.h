///////////////////////////////////////////////////////////////////////////////
// TpSetTagPositionCmd.h: This command class allow user to set tag position 
// to one of the predefined types (NE, NW, SE, SW, CTR).
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETTAGPOSITIONCMD_H
#define TPSETTAGPOSITIONCMD_H
#include "RadioCmd.h"
#include "TpPoint.h"
#include "TpMatchManager.h"

class TpSetTagPositionCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    const TpTagPosition _tagPosition;

    void doit()
	{ 
	    if (_value) {
		_matchManager->setTagPosition(_tagPosition);
	    }
	}

  public:

    TpSetTagPositionCmd(const char *name, int active, CmdValue value, 
			CmdList *radCmdList, TpTagPosition position, 
			TpMatchManager *mm)
	: RadioCmd(name, active, value, radCmdList), _tagPosition(position)
	{
	    _matchManager = mm;
	}

    virtual ~TpSetTagPositionCmd() { }

    TpTagPosition getTagPosition() const { return _tagPosition; }

    virtual const char *const className () { return "TpSetTagPositionCmd"; }
};

#endif
