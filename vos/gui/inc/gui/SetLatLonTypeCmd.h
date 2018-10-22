/////////////////////////////////////////////////////////////
// SetLatLonTypeCmd.h: sets the method of calculating 
//  latitude and longitude; can be either planetocentric or
//  planetodetic
/////////////////////////////////////////////////////////////
#ifndef SETLATLONTYPECMD_H
#define SETLATLONTYPECMD_H
//#include "NoUndoCmd.h"

#include "RadioCmd.h"

//#include "ImageDisplayer.h"
#include "LatLonBar.h"
#include "CursorLatLonView.h"

class SetLatLonTypeCmd : public RadioCmd {

  protected:
  
    // int _oldValue;
    
    CursorLatLonView *_cursorView;
    LatLonType _latLonType;
    
    virtual void doit();   

  public:
    
    SetLatLonTypeCmd ( const char *, int, CursorLatLonView *, LatLonType,
		       CmdList * );

    virtual const char *const className () { return "SetLatLonTypeCmd"; }
};
#endif
