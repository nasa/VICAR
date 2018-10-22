////////////////////////////////////////////////////////////////////////
// Compenent that implements the command interface (text keyin areas)
// for the FileSelBox.
////////////////////////////////////////////////////////////////////////
#ifndef FILETEXTINTERFACE_H
#define FILETEXTINTERFACE_H

#include "CmdInterface.h"

class FileTextInterface : public CmdInterface {
 private:
   static void toggleCallback(Widget, XtPointer, XtPointer);
   static void typeToggleCallback(Widget, XtPointer, XtPointer);
   static void fileFocusCallback(Widget, XtPointer, XtPointer);
   static void bandFocusCallback(Widget, XtPointer, XtPointer);

 protected:
   int _numFiles;
   int _activeIndex;
   Boolean _useBands;
   Widget *_toggleWidgetList;
   Widget *_fileWidgetList;
   Widget *_bandWidgetList;

   Widget _toggleForm, _filesForm, _bandsForm;

   Widget _typeForm, _typeLabel, _typeTogglePDS, _typeToggleVICAR;

   virtual void createWidgetRow(int);

   virtual void toggle(Widget, XtPointer);
   virtual void typeToggle(Widget, XtPointer);
   virtual void fileFocus(Widget, XtPointer);
   virtual void bandFocus(Widget, XtPointer);

   virtual void enableFileIndex(int);

 public:

   FileTextInterface(Widget, Cmd *, int numFiles=3, Boolean useBands=True,
						    Boolean useType=True);

   virtual void enterName(char *);
   virtual void nextFile();

   virtual void startCmd();
};

#endif

