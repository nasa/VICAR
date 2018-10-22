////////////////////////////////////////////////////////////////
// DataRangeDialog.h:  Create the Data Range dialog.
////////////////////////////////////////////////////////////////
#ifndef DATARANGEDIALOG_H
#define DATARANGEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class StringKeyinInterface;

class DataRangeDialog: public CustomDialog {

 protected:

   Cmd *_minAutoCmd;
   Cmd *_maxAutoCmd;
   Cmd *_minValueCmd;
   Cmd *_maxValueCmd;
   StringKeyinInterface *_minInterface;
   StringKeyinInterface *_maxInterface;
   double _saveMin, _saveMax;

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   DataRangeDialog(const char *name, Cmd *, Cmd *, Cmd *, Cmd *);

   virtual Widget createWorkArea(Widget);

   virtual void setDataRange(double min, double max);

};

#endif

