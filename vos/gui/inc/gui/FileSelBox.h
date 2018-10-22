////////////////////////////////////////////////////////////////////////
// Compenent that implements a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef FILESELBOX_H
#define FILESELBOX_H

#include "UIComponent.h"

class FileTextInterface;
class Cmd;

class FileSelBox : public UIComponent {
 private:
   static void newTextCallback(Widget, XtPointer, XtPointer);
   static void okButtonCallback(Widget, XtPointer, XtPointer);
   static void applyButtonCallback(Widget, XtPointer, XtPointer);
   static void cancelButtonCallback(Widget, XtPointer, XtPointer);
   static void helpButtonCallback(Widget, XtPointer, XtPointer);

 protected:
   FileTextInterface *_fileText;
   Widget _applyWidget;
   UIComponent *_mainWindow;

   virtual void newText(Widget w);
   virtual void okButton(Widget, XtPointer);
   virtual void applyButton();
   virtual void cancelButton();
   virtual void helpButton(Widget, XtPointer);

 public:

   FileSelBox(Widget, const char *, Cmd *, UIComponent *mainWindow=NULL);

};

#endif

