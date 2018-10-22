//////////////////////////////////////////////////////////////
// SideBar.h: A component class to show toolbox
/////////////////////////////////////////////////////////////
#ifndef SIDEBAR_H
#define SIDEBAR_H
#include "UIComponent.h"
#include "CursorPositionView.h"
#include "CursorDnView.h"
#include "MagInfo.h"

class Cmd;
class SiHistogram;
class Lut;
class ImageData;
class BasicImageView;
class ZoomCmdSet;
class ImageSizeView;

extern void SiCollectHist(ImageData *,
                SiHistogram *, SiHistogram *, SiHistogram *);

class SideBar : public UIComponent {

  private:

    Widget _histBox;
    static void inputCallback ( Widget, XtPointer, XtPointer );

    CursorPositionView *_cursorPositionView;
    CursorDnView *_cursorDnView;
    ImageSizeView *_imageSizeView;
    MagInfo *_magInfo;

  public:

    SideBar ( Widget, const char *, BasicImageView *,
		ImageData *, Cmd *, SiHistogram *, SiHistogram *, SiHistogram *,
		Cmd *, Lut *, Lut *, Lut *, ZoomCmdSet * );

    // Access functions
    CursorPositionView *getCursorPositionView() { return _cursorPositionView; }
    CursorDnView *getCursorDnView() { return _cursorDnView; }
    MagInfo *getMagInfo() { return _magInfo; }

    virtual const char *const className() { return "SideBar"; }
};
#endif

