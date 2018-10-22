/////////////////////////////////////////////////////////////////
// ZoomDialog.h - Dialog box for the zoom factor
////////////////////////////////////////////////////////////////
#ifndef ZOOMDIALOG_H
#define ZOOMDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"
#include <Xm/Xm.h>

class ZoomCmdSet;
class ZoomRadioCmdBox;
class ZoomCmdInterface;

class ZoomDialog : public CustomDialog {

 protected:

   ZoomCmdSet *_zoomCmdSet;
   ZoomRadioCmdBox *_zoomRadioCmdBox;
   ZoomCmdInterface *_zoomCmdInterface;

   virtual Widget createWorkArea(Widget);

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   ZoomDialog(const char *name, ZoomCmdSet *);
   virtual ~ZoomDialog();
   virtual const char *const className() { return ("ZoomDialog"); }

};
#endif

