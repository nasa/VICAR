////////////////////////////////////////////////////////////////////////
// LoadMenuCmd.h: Include file to handle posting the File dialog box.
////////////////////////////////////////////////////////////////////////
#ifndef LOADMENUCMD_H
#define LOADMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Cmd;

class LoadMenuCmd : public NoUndoCmd {

 private:

   MainWindow *_fileSelWindow;
   Cmd *_loadFileCmd;

 protected:

   virtual void doit();

 public:

   LoadMenuCmd(const char *, int, Cmd *);
   virtual const char *const className () { return "LoadMenuCmd"; }
};
#endif
