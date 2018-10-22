////////////////////////////////////////////////////////////////////////
// SptMultiFileSelView - View object for Multiple File Selection dialog
////////////////////////////////////////////////////////////////////////
#ifndef SPTMULTIFILESELVIEW_H
#define SPTMULTIFILESELVIEW_H

#include "SptDirectBase.h"

class SptDirectValueCmd;
class FileSelBox;
class MainWindow;

class DropSite;

class SptMultiFileSelView : public SptDirectBase {
 private:

 protected:
   FileSelBox *_fileSelBox;
   SptDirectValueCmd *_stringValueCmd;

   DropSite *_dropSite;

   virtual void displayValue();
   virtual void displayValue(char *);

 public:
   SptMultiFileSelView(Widget, char *, SptParamBase *, MainWindow *mw = NULL);

   virtual void paramValueChanged();
   virtual void setDeferredExec(CmdList *);

   virtual void manage();
   virtual void unmanage();
};

#endif
