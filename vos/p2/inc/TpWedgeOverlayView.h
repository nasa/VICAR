//////////////////////////////////////////////////////////////////////////////
// TpWedgeOverlayView.h: Provides overlay graphics support for wedge class
// The class allows to manupulate two marks that are used by contrasting 
// tool in TP.
/////////////////////////////////////////////////////////////////////////////
#ifndef TPWEDGEOVERLAYVIEW_H
#define TPWEDGEOVERLAYVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"

class TpContrastValue;
class Cmd;

class TpWedgeOverlayView : public UIComponent {

  private:

    static String _defaults[];
    static XtResource _resources[];

    static void inputCallback(Widget, XtPointer, XtPointer);
    void input(XtPointer callData);

  protected:

    Cmd *_cmd;

    int _min, _max;

    Widget _iw;
    
    XvicGC    _lineGC;
    XvicColor _lineColor;
    
    String    _markColor;
    Dimension _markLength, _markThickness;

  public:

    TpWedgeOverlayView(Widget, const char *, Cmd *);
    
    virtual void update(TpContrastValue *);
    
    virtual const char *const className() { return ("TpWedgeOverlayView"); }
};
#endif
