//////////////////////////////////////////////////////////////
// RadioCmdBox.h: A UIComponent that creates a radio bank given a
// list of RadioCmd's.  All items in the list must be RadioCmd's,
// but this is not checked for in the code.  The provided CmdList
// will typically be a MenuList or the actual radio list (the one
// that specifies which commands make up the mutual-exclude set).
// This is merely a convenience class; if you want separators or other
// weird things in the radio bank it can be created manually.
// The returned RowColumn can be manipulated, for example to set the
// number of columns.
///////////////////////////////////////////////////////////////
#ifndef RADIOCMDBOX_H
#define RADIOCMDBOX_H
#include "UIComponent.h"

class CmdList;
class RadioButtonInterface;

class RadioCmdBox : public UIComponent {

  protected:
    int _numElements;
    RadioButtonInterface **_list;

  public:
    
    RadioCmdBox ( Widget, const char *name, CmdList *, CmdList * = 0 );
    virtual ~RadioCmdBox();

};
#endif
