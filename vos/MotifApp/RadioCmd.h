/////////////////////////////////////////////////////////
// RadioCmd.h: A base class for Cmd objects exhibiting radio bank
// behavior.  The value must be an integer True or False.  The addition
// is a list of commands in the same radio group - when one turns on,
// all the others in the list turn off.  Since all Commands in the
// radio bank share the same list, it is intended that there be only one
// CmdList, and that its pointer be set via setRadioList() or the constructor.
/////////////////////////////////////////////////////////
#ifndef RADIOCMD_H
#define RADIOCMD_H

#include "Cmd.h"

class CmdList;

class RadioCmd : public Cmd {
    
  protected:
    
    // List of other commands in this radio bank
    
    CmdList       *_radioList;

    Cmd *_lastTrueCmd;		// the previous one set; for Undo
    
  public:
    
    RadioCmd(const char *, int, CmdList * = NULL);  // Constructor with no value
    RadioCmd(const char *, int, CmdValue, CmdList * = NULL); // w/starting value

    // public interface for executing and undoing commands
    
    virtual void execute( CmdValue = NULL );  
    virtual void undo();
    
    // Undo (derived classes need not implement this

    virtual void undoit();

    // Function to register the radio list
    
    void    setRadioList ( CmdList * );

    virtual void reset();
    
    virtual const char *const className () { return "RadioCmd"; }
};
#endif
