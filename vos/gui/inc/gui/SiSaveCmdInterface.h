////////////////////////////////////////////////////////////////
// SiSaveCmdInterface.h: Command interface subclass that fills 
// a SiSaveCmdValue structure and executes the command.
////////////////////////////////////////////////////////////////

#ifndef SISAVECMDINTERFACE_H
#define SISAVECMDINTERFACE_H

#include "CmdInterface.h"
#include "SiSaveCmdValue.h"

class KeyinView;
class OptionCmdMenu;
class Cmd;
class CmdList;

class SiSaveCmdInterface : public CmdInterface {

  protected: 

    Boolean _suspend_updates;

    Cmd *_browser_red_cmd, *_post_red_cmd;
    CmdInterface *_browser_red;
    Cmd *_browser_grn_cmd, *_post_grn_cmd;
    CmdInterface *_browser_grn;
    Cmd *_browser_blu_cmd, *_post_blu_cmd;
    CmdInterface *_browser_blu;

    KeyinView *_filename_red;
    KeyinView *_filename_grn;
    KeyinView *_filename_blu;

    CmdList *_extentList;
    OptionCmdMenu *_extentMenu;
    Cmd *_extentDisplay, *_extentFile, *_extentROI;

    CmdList *_lutList;
    OptionCmdMenu *_lutMenu;
    Cmd *_lutRaw, *_lutStretch, *_lutPseudo, *_lutPseudoOnly;

    CmdList *_byteList;
    OptionCmdMenu *_byteMenu;
    Cmd *_byteFalse, *_byteTrue;

    CmdList *_fmtList;
    OptionCmdMenu *_fmtMenu;
    Cmd *_fmtVicar, *_fmtTiff;

    static void runCommandCallback(Widget, XtPointer, XtPointer);

  public:

    SiSaveCmdInterface(Widget, Cmd *);
    virtual ~SiSaveCmdInterface();

    virtual void runCommand();

    virtual void setFilename(char *name, int which);

    virtual void setValue(CmdValue);

};

#endif

