///////////////////////////////////////////////////////////////////////////////
// TpCheckGenQualUniqueCmd.h: This command class allow user to specify whether 
// the program should check for general qualifier uniqueness.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPCHECKGENQUALUNIQUECMD_H
#define TPCHECKGENQUALUNIQUECMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpCheckGenQualUniqueCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
            if (_value)
                _matchManager->setCheckingGenQualUnique(True);
            else
                _matchManager->setCheckingGenQualUnique(False);
        }

    void undoit()
	{
            if (_value)
                _matchManager->setCheckingGenQualUnique(False);
            else
                _matchManager->setCheckingGenQualUnique(True);
 
            _value = (CmdValue)(!_value);
            newValue();
        }

  public:

    TpCheckGenQualUniqueCmd(const char *name, int active, TpMatchManager *mm) 
	: Cmd(name, active)
	{ _matchManager = mm; }
    virtual ~TpCheckGenQualUniqueCmd() { }

    virtual const char *const className () { return "TpCheckGenQualUniqueCmd";}
};

#endif
