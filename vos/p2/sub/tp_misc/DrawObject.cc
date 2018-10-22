//////////////////////////////////////////////////////////////////////////////
// DrawObject.cc: 
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#include "DrawObject.h"

DrawObject::DrawObject(Widget iw, double x, double y)
{
    _iw = iw;
    _x = x;
    _y = y;

    _id = 0;
    XGCValues values;
    _gc = XvicImageCreateGC(_iw, 0L, &values);
    setColor("black");
}

DrawObject::~DrawObject()
{
    erase();
}

void DrawObject::setColor(XColor *color)
{
    _color = XvicImageGetGrColor(_iw, color);
}

void DrawObject::setColor(int r, int g, int b)
{
    _color = XvicImageGetGrColorRGB(_iw, r, g, b);
}

void DrawObject::setColor(const char *str)
{
    XColor xcolor;
    XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
		(char *)str, &xcolor);
    setColor(&xcolor);
}

void DrawObject::move(double x, double y)
{
    XvicImageMoveObject(_iw, _id, x, y);
}

void DrawObject::erase()
{
    XvicImageEraseObject(_iw, _id);
    _id = 0;
}
