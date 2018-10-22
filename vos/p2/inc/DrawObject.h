//////////////////////////////////////////////////////////////////////////////
// DrawObject.h: Abstract class for imaging widget graphical overlay object.
// The subclasses have to specify the drawing routine.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef DrawObject_H
#define DrawObject_H
#include "XvicImage.h"

class DrawObject {

  protected:

    Widget _iw;
    XvicID _id;
    XvicID _gid;
    XvicGC _gc;
    XvicColor _color;

    double _x;
    double _y;
    int _width;
    int _height;
    int _offsetx;
    int _offsety;

  public:

    DrawObject(Widget iw, double x=0.0, double y=0.0);
    virtual ~DrawObject();

    virtual XvicID draw()=0;
    virtual void move(double x, double y);
    virtual void erase();
    virtual Boolean isGroup() { return FALSE; }

    void setColor(XColor *color);
    void setColor(int r, int g, int b);
    void setColor(const char *str);
};
#endif
