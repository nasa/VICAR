//////////////////////////////////////////////////////////////
// OptionCmdMenu.h: An option menu component that hooks to a
// CmdList or MenuCmdList.  All commands in the list should be
// RadioCmd's in order for things to work correctly.  If a
// MenuCmdList is used, separators may also be used, along with
// setting mnemonics and accelerators, although the RadioCmd's
// must be added using addOption() rather than addRadioButton().
// If a straight CmdList is used, then all items of the list must
// be RadioCmd's and mnemonics or accelerators must be set via the
// resource file.  The CmdList form is convenient because it allows
// the use of the already-existing CmdList used to specify the
// mutual-exclude of the RadioCmd's.
///////////////////////////////////////////////////////////////

#ifndef OPTIONCMDMENU
#define OPTIONCMDMENU
#include "UIComponent.h"

class CmdList;
class MenuCmdList;

class OptionCmdMenu : public UIComponent {

  public:
    
    OptionCmdMenu ( Widget, const char *name, MenuCmdList * );
    OptionCmdMenu ( Widget, const char *name, CmdList *, CmdList * = 0 );
};
#endif
