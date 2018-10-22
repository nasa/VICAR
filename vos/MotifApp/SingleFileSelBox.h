////////////////////////////////////////////////////////////////////////
// SingleFileSelBox.h: Component that implements a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef SINGLEFILESELBOX_H
#define SINGLEFILESELBOX_H
#include "CmdInterface.h"

class SingleFileSelBox : public CmdInterface {

  private:

    static void okButtonCallback(Widget, XtPointer, XtPointer);
    static void cancelButtonCallback(Widget, XtPointer, XtPointer);
    static void helpButtonCallback(Widget, XtPointer, XtPointer);

  protected:

    UIComponent *_mainWindow;
    Cmd *_loadFileCmd;

    virtual void okButton(Widget, XtPointer);
    virtual void cancelButton(Widget, XtPointer);
    virtual void helpButton(Widget, XtPointer);

    void getPath(char *filename, char *dir);

  public:

    SingleFileSelBox(Widget, Cmd *, UIComponent *mainWindow=NULL);
    void setValue(CmdValue);

};

#endif
