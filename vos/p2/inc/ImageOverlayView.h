////////////////////////////////////////////////////////////////
// ImageOverlayView.h: This class is responsible for displaying 
// imaging widget with overlayed objects.
////////////////////////////////////////////////////////////////
#ifndef IMAGEOVERLAYVIEW_H
#define IMAGEOVERLAYVIEW_H
#include "ImageDisplayView.h"
#include "XvicImage.h"
#include "sl_lists.h"

class DrawObject;

class ImageOverlayView : public ImageDisplayView {

  protected:

    SL_List<DrawObject *> *_objects;

    XvicColor getColor(XColor *xcolor);
    XvicColor getColor(int red, int green, int blue);

  public:

    ImageOverlayView(Widget parent, const char *name, ImageData *imageData);
    virtual ~ImageOverlayView();

    XvicID addObject(DrawObject *object);
    void moveObject(DrawObject *object, double newx, double newy);
    void deleteObject(DrawObject *object);

    void colorObject(DrawObject *object, XColor *xcolor);
    void colorObject(DrawObject *object, int red, int green, int blue);

    XvicID groupObjects(SL_List<DrawObject *> *objects);
    void ungroupObjects(XvicID id);

    XvicID raiseObject(DrawObject *object);

    virtual const char *const className() { return "ImageOverlayView"; }
};
#endif
