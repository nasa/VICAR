//////////////////////////////////////////////////////////////
// StretchParmList.h: A component class to implement list widget
/////////////////////////////////////////////////////////////
#ifndef STRETCHPARMLIST_H
#define STRETCHPARMLIST_H
#include "UIComponent.h"

class CurListValue;

class StretchParmList : public UIComponent {

  protected:

    int *_in;
    int *_out;
    int _count;

    CurListValue *_inSingleValue;
    CurListValue *_outSingleValue;

    static void selectCallback ( Widget, XtPointer, XtPointer);

  public:

    StretchParmList ( Widget, const char *, CurListValue *, CurListValue * );
    ~StretchParmList ( ) { };

    void update ( int *, int *, int );

    virtual void select ( int );

    virtual const char *const className() { return "StretchParmList"; }
};
#endif

