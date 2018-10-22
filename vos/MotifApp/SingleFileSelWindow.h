////////////////////////////////////////////////////////////////////////
// SingleFileSelWindow.h:  Component that creates a popup window 
// containing a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef SINGLEFILESELWINDOW_H
#define SINGLEFILESELWINDOW_H
#include "MainWindow.h"

class SingleFileSelBox;
class Cmd;

class SingleFileSelWindow : public MainWindow {

  protected:

    Widget _form;
    SingleFileSelBox *_fileSelBox;
    Cmd *_loadFileCmd;

    virtual Widget createWorkArea(Widget);

  public:

    SingleFileSelWindow(const char *name, Cmd *);

};

#endif
