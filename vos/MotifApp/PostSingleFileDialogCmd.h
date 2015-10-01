////////////////////////////////////////////////////////////////////////
// PostSingleFileDialogCmd.h:  Handles posting the File dialog box.  The
// caller must supply a command class object loadFileCmd
// that takes dynamically allocated string filename
// as a CmdValue.  The command is expected to free the string in
// freeValue() using XtFree.
////////////////////////////////////////////////////////////////////////
#ifndef POSTSINGLEFILEDIALOGCMD_H
#define POSTSINGLEFILEDIALOGCMD_H
#include "NoUndoCmd.h"

class MainWindow;

class PostSingleFileDialogCmd : public NoUndoCmd {

  private:

    int _created;
    MainWindow *_fileSelWindow;
    Cmd *_loadFileCmd;

  protected:

    virtual void doit();

  public:

    PostSingleFileDialogCmd(const char *, int, Cmd *);
    virtual const char *const className () { return "PostSingleFileDialogCmd"; }

};
#endif
