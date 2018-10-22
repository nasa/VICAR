//////////////////////////////////////////////////////////////
// CurListValue.h: To show current list values
/////////////////////////////////////////////////////////////
#ifndef CURLISTVALUE_H
#define CURLISTVALUE_H
#include "KeyinView.h"

class CurListValue : public KeyinView {

  protected:

    int _defValue;
    int _value;

  public:

    CurListValue ( Widget, const char *, int=0);

    void setValue ( int );
    int getValue();

    virtual void update( XtPointer = NULL );

    virtual const char *const className() { return "CurListValue"; }
};
#endif

