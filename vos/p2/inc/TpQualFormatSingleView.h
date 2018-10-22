////////////////////////////////////////////////////////////////
// TpQualFormatSingleView.h: 
////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATSINGLEVIEW_H
#define TPQUALFORMATSINGLEVIEW_H
#include "UIComponent.h"
#include "TpQualifier.h"

class TpQualFormatView;

class TpQualFormatSingleView : public UIComponent {

  protected:

    Widget _wname, _wunit;
    Widget _wreal, _wfull, _wtext;

    int _n;
    TpQualFormatView *_view;

    static void setNameCallback(Widget, XtPointer, XtPointer);
    static void setUnitCallback(Widget, XtPointer, XtPointer);
    static void setTypeCallback(Widget, XtPointer, XtPointer);

  public:

    TpQualFormatSingleView(Widget parent, const char *name, 
			   TpQualFormatView *, int n);
    virtual ~TpQualFormatSingleView() { };

    void setNumber(int n) { _n = n; }

    void setName(char *, Boolean doUpdate = True);
    void setUnit(char *, Boolean doUpdate = True);
    void setType();
    void setType(TpQualType);

    virtual const char *const className() { return "TpQualFormatSingleView"; }

};
#endif
