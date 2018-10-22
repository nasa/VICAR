//////////////////////////////////////////////////////////////////////////////
// TpPoint.h: This class is responsible for drawing a visual representation
// of a tiepoint over the image.  It supports different sizes and shapes.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINT_H
#define TPPOINT_H
#include "DrawObject.h"
#include "TpDefs.h"

class TpPointModel;

class TpPoint : public DrawObject {

  private:

    static int _tpPointInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    TpPointModel *_model;

    XFontStruct *_fontStruct;
    char *_fontname;

    char *_labelString;

    int _width, _height;

    XvicID drawCrossDot();
    XvicID drawRectangle();
    XvicID drawDot();
    XvicID drawCross45();
    XvicID drawCrossWithHole45();

    XvicID drawLabel();

  public:

    TpPoint(TpPointModel *model, Widget iw);
    virtual ~TpPoint();

    virtual XvicID draw();
    void update(TpPointModel *model);

};
#endif
