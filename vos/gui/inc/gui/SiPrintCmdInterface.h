////////////////////////////////////////////////////////////////
// SiPrintCmdInterface.h: Command interface subclass that fills 
// a SiPrintCmdValue structure and executes the command.
////////////////////////////////////////////////////////////////

#ifndef SIPRINTCMDINTERFACE_H
#define SIPRINTCMDINTERFACE_H

#include "CmdInterface.h"
#include "SiPrintCmdValue.h"

class KeyinView;
class OptionCmdMenu;
class Cmd;
class CmdList;

class SiPrintCmdInterface : public CmdInterface {

  protected: 

    Boolean _suspend_updates;

    Cmd *_browser_file_cmd, *_post_file_cmd;
    CmdInterface *_browser_file;

    KeyinView *_filename;

    CmdList *_extentList;
    OptionCmdMenu *_extentMenu;
    Cmd *_extentDisplay, *_extentFile, *_extentROI;

    CmdList *_lutList;
    OptionCmdMenu *_lutMenu;
    Cmd *_lutRaw, *_lutStretch, *_lutPseudo, *_lutPseudoOnly;

    CmdList *_printToList;
    OptionCmdMenu *_printToMenu;
    Cmd *_printToPrinter, *_printToFile;

    KeyinView *_printerCommand;
    KeyinView *_printWidth;
    KeyinView *_printHeight;

    CmdList *_printOrientationList;
    OptionCmdMenu *_printOrientationMenu;
    Cmd *_printPortrait, *_printLandscape;

    CmdList *_printTitleList;
    OptionCmdMenu *_printTitleMenu;
    Cmd *_printTitleFilename, *_printTitleCustom;

    KeyinView *_title;

    static void runCommandCallback(Widget, XtPointer, XtPointer);

    void setPrintTo();		// sets sensitivity for file/cmd keyins

  public:

    SiPrintCmdInterface(Widget, Cmd *);
    virtual ~SiPrintCmdInterface();

    virtual void runCommand();

    virtual void setFilename(char *name);

    virtual void setValue(CmdValue);

};

#endif

