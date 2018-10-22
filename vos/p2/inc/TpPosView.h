////////////////////////////////////////////////////////////////
// TpPosView.h: This class displays coordinates of cursor location 
// if no point is selected or point coordinates and point qualifier if 
// any point is selected.  Typing new coordinates should change point's 
// location.
////////////////////////////////////////////////////////////////
#ifndef TPPOSVIEW_H
#define TPPOSVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"

class TpSubDisplayer;
class TpCursorModel;
class KeyinView;
class TpMatchManager;
class ImageData;

class TpPosView : public UIComponent {

  private:

    static void cursorMovedCallback(Widget,
                       XtPointer clientData, XtPointer callData);
    static void changePosCallback(Widget, 
                       XtPointer clientData, XtPointer callData);
    static void changeQualCallback(Widget, 
                       XtPointer clientData, XtPointer callData);

  protected:

    KeyinView *_linePosition;
    KeyinView *_sampPosition;
    KeyinView *_qualPosition;

    TpSubDisplayer *_subDisplayer;
    ImageData *_imageData;
    TpCursorModel *_cursorModel;
    TpMatchManager *_matchManager;

    Boolean _isOnImage;

    virtual void startTracking();
    virtual void stopTracking();

    Boolean isCursorOnImage(XvicImageCallbackStruct * cb);

    void changePos();
    void changeQual();

  public:

    TpPosView(Widget parent, const char *name,
		TpCursorModel *cursorModel, ImageData *imageData,
		TpMatchManager *mm, TpSubDisplayer *subDisplayer);
    virtual ~TpPosView();

    void cursorIsOnImage(Boolean onImage);

    virtual void cursorMoved(XvicImageCallbackStruct * cb );
    virtual void cursorMoved(int x, int y);
    virtual void cursorMoved(double x, double y);

    virtual void blankDisplay();

    void displayValues();

    virtual const char *const className() { return "TpPosView"; }

};
#endif
