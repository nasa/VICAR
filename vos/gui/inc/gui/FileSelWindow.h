////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef FILESELWINDOW_H
#define FILESELWINDOW_H

#include "MainWindow.h"
#ifdef ENABLE_SAGE
class SptParamMultiFileSel;
#else
class FileSelBox;
#endif
class Cmd;

class FileSelWindow : public MainWindow {

 protected:

   Widget _form;
#ifdef ENABLE_SAGE
   SptParamMultiFileSel *_fileSelBox;
#else
   FileSelBox *_fileSelBox;
#endif
   Cmd *_loadFileCmd;

   virtual Widget createWorkArea(Widget);

 public:

   FileSelWindow(const char *name, Cmd *);

};

#endif

