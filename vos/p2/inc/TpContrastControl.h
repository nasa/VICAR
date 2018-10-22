//////////////////////////////////////////////////////////////////////////////
// TpContrastControl.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPCONTRASTCONTROL_H
#define TPCONTRASTCONTROL_H
#include "CmdInterface.h"

class ImageData;
class TpWedgeOverlayView;

class TpContrastControl : public CmdInterface {

  protected:

    static XtResource _resources[];

    int _min, _max;

    Widget _keyinLowContrast;
    Widget _keyinHighContrast;

    TpWedgeOverlayView *_wedge;

    static void setHighContrastCallback(Widget w, XtPointer clientData,
			XtPointer callData);
    static void setLowContrastCallback(Widget w, XtPointer clientData,
			XtPointer callData);

    void setHighContrast(int i);
    void setLowContrast(int i);

  public:

    TpContrastControl(Widget parent, Cmd *cmd);
    virtual ~TpContrastControl() { }

    void reload(TpContrastControl *);

    void setValue(CmdValue);
};

#endif
