//////////////////////////////////////////////////////////////////////////////
// SgColorChooserInterface.h:  A command interface that allows user to select 
// a color by either typing the value or using color chooser.  It then 
// executes command with value passed as a string (either standard X color 
// name, like 'red', or hex value in the form #RRGGBB).
//////////////////////////////////////////////////////////////////////////////
#ifndef SGCOLORCHOOSERINTERFACE_H
#define SGCOLORCHOOSERINTERFACE_H

#include "CmdInterface.h"

class ColorChooser;

class SgColorChooserInterface : public CmdInterface {

  protected:

    Widget _text, _button;
    ColorChooser *_colorChooser;

    char *_oldValue;		// Avoid repeat execs if string doesn't change

    virtual void copyOldValue(char *string);
    virtual void executeCmd(XtPointer = NULL);

    static void colorSelectedCallback ( int, int, int, void * );
    static void pickColorCallback ( Widget, XtPointer, XtPointer );

  public:

    SgColorChooserInterface(Widget, Cmd *);

    virtual void setValue(CmdValue);

    virtual void setValueAndRun(CmdValue);

};
#endif
