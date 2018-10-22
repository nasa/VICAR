///////////////////////////////////////////////////////////////////
// TpSubDisplayer.h: Keeps track of all the image views and all the 
// tiepoint matches in relation to a single image.  One, two or three
// of these will comprise the display.
///////////////////////////////////////////////////////////////////
#ifndef TPSUBDISPLAYER_H
#define TPSUBDISPLAYER_H
#include "UIComponent.h"
#include "TpImageInfo.h"
#include "RotatedImageData.h"

class TpPosView;
class TpImageView;
class TpStripPanTool;
class TpZoomControl;
class TpMatchManager;
class TpContrastControl;
class TpDisplayer;

class TpSubDisplayer : public UIComponent {

  private:

    TpDisplayer *_displayer;
    TpMatchManager *_matchManager;

    char *_filename;
    int _number;

    RotatedImageData *_imageData;
    RotationType _rotationMode;		// left, right, flip
    Boolean _locked;			// for shifts

    Widget _panZoomForm, _form;

    TpPosView *_posView;
    Widget _lock;
    TpImageInfo *_imageInfo;
    TpImageView *_imageView;
    TpStripPanTool *_panView;
    TpImageView *_zoomView;
    TpZoomControl *_mainImageZoom;
    TpZoomControl *_zoomControl;
    TpContrastControl *_contrastControl;

    Boolean _failed;

    // ImageData values set via resources
    static XtResource _resources[];
    Boolean _autoMinRange;
    Boolean _autoMaxRange;
    String _minValue;                  // Strings so we can 
    String _maxValue;                  // detect no-value

    void layoutComponents() const;
    void showComponents() const;
    void hideComponents() const;

  public:

    TpSubDisplayer(Widget parent, const char *name, 
		char *filename, int number, 
		TpMatchManager *matchManager, TpDisplayer *);
    ~TpSubDisplayer();

    char *getFilename() { return _filename; }
    int getNumber() { return _number; }

    void setNumber(int n) { _number = n; _imageInfo->setNumber(n); }

    void setRotationMode(RotationType rotMode);
    void setLock(Boolean);
    void setCursor(String);
    void setCursorColor(String);

    ImageData *getImageData() const { return (ImageData *)_imageData; }
    TpImageView *getImageView() const { return _imageView; }
    TpImageView *getZoomView() const { return _zoomView; }
    TpContrastControl *getContrastControl() const { return _contrastControl; }
    TpPosView *getPosView() const { return _posView; }

    void makeCurrentPointVisible();

    Boolean failed() { return _failed; }

    virtual const char * const className() { return ("TpSubDisplayer"); } 
};
#endif
