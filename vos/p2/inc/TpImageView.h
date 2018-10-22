////////////////////////////////////////////////////////////////
// TpImageView.h: This class is responsible for displaying 
// imaging widget with overlayed points.
////////////////////////////////////////////////////////////////
#ifndef TPIMAGEVIEW_H
#define TPIMAGEVIEW_H
#include "ImageOverlayView.h"

class ImageData;
class TpMatchManager;
class TpSubDisplayer;

class TpImageView : public ImageOverlayView {

  private:

    static void inputCallback(Widget, XtPointer, XtPointer);

  protected:

    static Boolean _actionsAdded;

    static void tpMoveUp(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveDown(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveLeft(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveRight(Widget, XEvent *, String *, Cardinal *);

    TpMatchManager *_matchManager;
    TpSubDisplayer *_subDisplayer;
    Boolean _saved;

    Time _lastClickTime;

    void input(XtPointer);

  public:

    TpImageView(Widget parent, const char *name, ImageData *imageData, 
		TpMatchManager *, TpSubDisplayer *);
    virtual ~TpImageView();

    virtual void setCenter(const double x, const double y);
    virtual void setTitle(char *title);

    virtual const char *const className() { return "TpImageView"; }
};
#endif
