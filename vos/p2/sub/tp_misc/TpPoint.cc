//////////////////////////////////////////////////////////////////////////////
// TpPoint.cc: This class is responsible for drawing a visual representation 
// of a tiepoint over the image.  It supports different sizes and shapes.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpPoint.h"
#include "TpPointModel.h"
#include "TpQualGroup.h"
#include "TpMatch.h"
#include "TpDefs.h"
#include "ImageData.h"
#include "ErrorManager.h"
#include <Xm/RepType.h>
#include <stdio.h>

int TpPoint::_tpPointInit = False;

XtResource TpPoint::_resources [ ] = {
 {
    (char *)"fontList",
    (char *)"FontList",
    XmRString,
    sizeof(String),
    XtOffset(TpPoint *, _fontname),
    XmRImmediate,
    (XtPointer)"6x10",
 },
};

TpPoint::TpPoint(TpPointModel *model, Widget iw)
	: DrawObject(iw, model->getX(), model->getY())
{
    _model = model;

    XtGetApplicationResources(_iw, (XtPointer)this, 
		_resources, XtNumber(_resources),
                NULL, 0);

    _fontStruct = XLoadQueryFont(XtDisplay(_iw), _fontname);
    if(_fontStruct == NULL) {
        theErrorManager->process(Error, "TpPoint", "No such font", _fontname);
        _fontStruct = XQueryFont(XtDisplay(_iw), 
				 XGContextFromGC(DefaultGCOfScreen(
				     XtScreen(_iw))));
    }

    XGCValues values;
    values.font = _fontStruct->fid;
    XvicImageChangeGC(_iw, _gc, GCFont, &values);

    _model->attachView(this);
}

TpPoint::~TpPoint()
{
    if(_iw && _fontStruct)
	XFreeFont(XtDisplay(_iw), _fontStruct);
}

XvicID TpPoint::draw()
{
    XvicID id;

    _width = _model->getPointWidth();
    _height = _model->getPointHeight();

    if (_model->isCurrent()) 
	setColor(_model->getPointColorSel());
    else 
	setColor(_model->getPointColor());

    switch (_model->getPointShape()) {
	case CrossWithDot: 
	    id = drawCrossDot();
	    break;
	case Rectangle: 
	    id = drawRectangle();
	    break;
        case Dot:
	    id = drawDot();
	    break;
        case Cross45:
	    id = drawCross45();
	    break;
	case CrossWithHole45:
	    id = drawCrossWithHole45();
	    break;
        case RectangleWithCrossesWithDot:
	    id = drawCrossDot();
	    id = drawRectangle();
	    id = drawCrossWithHole45();
	    break;
	default: 
	    id = drawCrossDot();
    }

    TpQualGroup *qualGroup = _model->getMatch()->getGenQual();
    if (qualGroup->getNumQuals() > 0) {
        _labelString = qualGroup->valueToString(0); //!!! customize this
    }
    else
        _labelString = NULL;

    if (_model->getShowPointLabels() && _labelString && strlen(_labelString)) {
	id = drawLabel();
    }

    return id;
}

XvicID TpPoint::drawRectangle()
{
    _id = XvicImageDrawRectangle(_iw, _id, _gc, _color, 
				 _x-(_width/2), _y-(_height/2), 
				 _width, _height);
    return _id;
}

XvicID TpPoint::drawDot()
{
    _id = XvicImageFillRectangle(_iw, _id, _gc, _color,
				 _x, _y, 1, 1);
    return _id;
}

XvicID TpPoint::drawCross45()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y-(_height/2),
                _x+(_width/2), _y+(_height/2));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y+(_height/2),
                _x+(_width/2), _y-(_height/2));

    return _id;
}

XvicID TpPoint::drawCrossDot()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x, _y-(_height/2),
                _x, _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y,
                _x-(_width/4), _y);
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/4), _y,
                _x+(_width/2), _y);
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x, _y+(_height/2),
                _x, _y+(_height/4));
    XvicImageDrawPoint(_iw, _id, _gc, _color,
		_x, _y);

    return _id;
}

XvicID TpPoint::drawCrossWithHole45()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y-(_height/2), 
		_x-(_width/4), _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y+(_height/2),
                _x-(_width/4), _y+(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/2), _y-(_height/2),
                _x+(_width/4), _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/2), _y+(_height/2),
                _x+(_width/4), _y+(_height/4));

    return _id;
}

XvicID TpPoint::drawLabel()
{
    switch (_model->getTagPosition()) {
    case NorthEast:
	_id = XvicImageDrawString(_iw, _id, _gc, _color,
				  _x+(_width/2), _y-(_height/2),
				  _labelString, strlen(_labelString),
				  XvicJUST_LEFT);
	break;
    case NorthWest:
	_id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x-(_width/2), _y-(_height/2),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_RIGHT);
        break;
    case SouthWest:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x-(_width/2), _y+(_height),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_RIGHT);
        break;
    case SouthEast:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x+(_width/2), _y+(_height),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_LEFT);
        break;
    case Center:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x, _y,
                                  _labelString, strlen(_labelString),
                                  XvicJUST_LEFT);
        break;
    }
    return _id;
}

void TpPoint::update(TpPointModel *model)
{
    _model->getImageData()->transImageToDisplayCoords(model->getX(),
                                                      model->getY(),
                                                      &_x, &_y);

    if (_id == 0) {
	draw();
    }
    else {
	erase();
	draw();
    }
}
