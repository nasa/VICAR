///////////////////////////////////////////////////////
// RadioCmd.cc: A base class for Cmd objects exhibiting radio bank
// behavior.  The value must be an integer True or False.  The addition
// is a list of commands in the same radio group - when one turns on,
// all the others in the list turn off.  Since all Commands in the
// radio bank share the same list, it is intended that there be only one
// CmdList, and that its pointer be set via the constructor (in which
// case the RadioCmd will add itself to the list) or via setRadioList()
// (in which case you must add the command to the list yourself).
///////////////////////////////////////////////////////
#include "RadioCmd.h"
#include "CmdList.h"
#include "CmdInterface.h"
#include "UndoCmd.h"

RadioCmd::RadioCmd ( const char *name, int active, CmdList *radioList ) :
							// = NULL
		Cmd ( name, active )
{
    _radioList = radioList;
    if (radioList)
        radioList->add(this);
    _lastTrueCmd = NULL;
}

RadioCmd::RadioCmd ( const char *name, int active, CmdValue starting_value,
						CmdList *radioList ) :
							// = NULL
		Cmd ( name, active, starting_value )
{
    _radioList = radioList;
    if (radioList)
        radioList->add(this);
    _lastTrueCmd = NULL;
}

void RadioCmd::execute( CmdValue value )
{
    int i;

    // If a command is inactive, it cannot be executed
    
    if ( !_active )
	return;
    
    // If this one is turned on, turn off all the others in _radioList

    if (value) {
        if (_radioList) {
            _lastTrueCmd = NULL;
            for (i = 0; i < _radioList->size(); i++) {
                if ((*_radioList)[i] != this) {
                    if ((*_radioList)[i]->getValue())
                        _lastTrueCmd = (*_radioList)[i];
                    (*_radioList)[i]->execute((CmdValue) False);
                }
            }
            if (_lastTrueCmd == NULL)
                _lastTrueCmd = this;	// Must have already been selected
        }
    }

    Cmd::execute(value);
    
}

// The only reason this is repeated here is to avoid running through the
// activation/deactivation list.  Since RadioCmd's are undone by executing
// whatever was last True, that execution will reset all the dependent
// sensitivities.

void RadioCmd::undo()
{

    // Call the derived class's undoit() member function.

    undoit();

    // The system only supports one level of undo, and this is it,
    // so deactivate the undo facility.

    theUndoCmd->deactivate();
}

void RadioCmd::undoit()
{ 
    // By executing the saved command with True, it will turn everything
    // else, including ourself, off

    if (_lastTrueCmd)
        _lastTrueCmd->execute((CmdValue) True);
}

void RadioCmd::setRadioList ( CmdList *list )
{
    _radioList = list;
}

// Reset all interfaces of all commands in this radio bank.

void RadioCmd::reset()
{
    if (_radioList) {
        for (int i = 0; i < _radioList->size(); i++) {
            (*_radioList)[i]->newValue();
        }
    }
    else
        newValue();
}

