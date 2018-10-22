///////////////////////////////////////////////////////////////////
//  TpZoomControl.h
///////////////////////////////////////////////////////////////////
#ifndef TPZOOMCONTROL_H
#define TPZOOMCONTROL_H
#include "UIComponent.h"

class TpImageView;

class TpZoomControl : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    Widget _keyinZoom;
    TpImageView *_zoomView;

    int _zoomValue;

    static void incZoomCallback(Widget w, XtPointer clientData,
			XtPointer callData);
    static void setZoomCallback(Widget w, XtPointer clientData,
                        XtPointer callData);
    static void decZoomCallback(Widget w, XtPointer clientData,
			XtPointer callData);

    void setZoom(int i);
    void incZoom(int i);

  public:

    TpZoomControl(Widget parent, const char *name, TpImageView *_zoomView);
    virtual ~TpZoomControl() { }

    void reload(TpZoomControl *);
};

#endif
