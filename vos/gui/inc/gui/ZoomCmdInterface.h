//////////////////////////////////////////////////////////////
// ZoomCmdInterface.h
///////////////////////////////////////////////////////////////
#ifndef ZOOMCMDINTERFACE
#define ZOOMCMDINTERFACE
#include "CmdInterface.h"
#include <Xm/Xm.h>

class KeyinView;
class BasicImageView;

class ZoomCmdInterface : public CmdInterface {
 private:
   static void newZoomCallback(Widget, XtPointer, XtPointer);

 protected:

   KeyinView *_zoomXIn;
   KeyinView *_zoomXOut;
   KeyinView *_zoomYIn;
   KeyinView *_zoomYOut;

   virtual KeyinView *addOneSubView(Widget parent, const char *name);
   virtual void createAllSubViews(Widget parent, BasicImageView *imageView);
   virtual void executeCmd(XtPointer=NULL);
   virtual void setValue(CmdValue);
   virtual void newZoom(Widget);

 public:

   ZoomCmdInterface(Widget, Cmd *, BasicImageView * = NULL);
   ZoomCmdInterface(Widget, const char *, BasicImageView * = NULL);
   ~ZoomCmdInterface();
};
#endif

