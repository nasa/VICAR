//////////////////////////////////////////////////////////////
// KeyinView.h: A component class to show keyin fields
/////////////////////////////////////////////////////////////
#ifndef KEYINVIEW_H
#define KEYINVIEW_H
#include "UIComponent.h"

class KeyinView : public UIComponent {

  public:	// Should be private but Sun C++ 4.0.1 has a bug

    static void updateCallback ( Widget, XtPointer, XtPointer );

  protected:

    Widget _field;	// Input area

    Widget _label;	// Label

  public:

    KeyinView ( Widget, const char * );

    void installCallback(void (*)(Widget, XtPointer, XtPointer) = updateCallback, XtPointer obj = (XtPointer)NULL);

    Widget getField ( ) { return _field; }
    Widget getLabel ( ) { return _label; }

    virtual void setFieldValue(char *text);
    virtual char *getFieldValue();

    virtual void update( XtPointer = NULL ) { }

    virtual const char *const className() { return "KeyinView"; }
};
#endif

