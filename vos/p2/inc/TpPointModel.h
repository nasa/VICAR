///////////////////////////////////////////////////////////////
// TpPointModel.h: This class serves as a Model for individual 
// points (observations).  
///////////////////////////////////////////////////////////////
#ifndef TPPOINTMODEL_H
#define TPPOINTMODEL_H
#include "TpDefs.h"
#include <Xm/Xm.h>

class TpPoint;
class TpMatch;
class ImageData;
class TpQualGroup;
class TpQualGroupMgr;

class TpPointModel {

  protected:

    TpPoint **_views;
    int _numViews;

    Boolean _isCurrent;

    double _x;
    double _y;

    ImageData *_image;
    char *_filename;
    int _imageNumber;

    TpMatch *_match;
    TpQualGroup *_pointQual;

    Pixel _size;
    static TpPointSymbolShapeType _pointShape;
    static TpTagPosition _tagPosition;

    static int _pointWidth; 
    static int _pointHeight;
    static char *_pointColor;
    static char *_pointColorSel;

    static Boolean _showLabel;

    static char *_colorCodeFilename;

  public:

    TpPointModel(char *filename, int imageNumber, ImageData *image, 
		 TpMatch *match, TpQualGroupMgr *qualMgr,
		 double x=0.0, double y=0.0);
    virtual ~TpPointModel();

    void attachView(TpPoint *);
    void detachView(TpPoint *);
    void updateViews();

    double getX() { return _x; }
    double getY() { return _y; }

    void setX(double x);// { _x = x; updateViews(); }
    void setY(double y);// { _y = y; updateViews(); }
    void setXY(double x, double y);// { _x = x; _y = y; updateViews(); }

    char *getFilename() { return _filename; }
    ImageData *getImageData() { return _image; }
    int getImageNumber() { return _imageNumber; }

    void setCurrent(Boolean isCurrent);
    Boolean isCurrent() { return _isCurrent; }

    void listPoint();

    void setPointShape(TpPointSymbolShapeType shape);
    static void setPointShapeStatic(TpPointSymbolShapeType shape);
    TpPointSymbolShapeType getPointShape() { return _pointShape; }

    void setTagPosition(TpTagPosition position);
    static void setTagPositionStatic(TpTagPosition position);
    TpTagPosition getTagPosition() { return _tagPosition; }

    void setPointWidth(int);
    static void setPointWidthStatic(int);
    int getPointWidth() { return _pointWidth; }

    void setPointHeight(int);
    static void setPointHeightStatic(int);
    int getPointHeight() { return _pointHeight; }

    void setPointDimensions(int, int);
    static void setPointDimensionsStatic(int, int);
    
    void setPointColor(char *);
    static void setPointColorStatic(char *);
    char *getPointColor() { return _pointColor; }

    void setPointColorSel(char *);
    static void setPointColorSelStatic(char *);
    char *getPointColorSel() { return _pointColorSel; }

    void showPointLabels(Boolean show);
    Boolean getShowPointLabels() { return _showLabel; }

    void rotationChanged();

    void colorCodePointsGen(int n, float a, float b);
    void colorCodePointsPoint(int n, float a, float b);

    void setColorCodeFilename(char *string) { _colorCodeFilename = string; }

    static int pointCmp(TpPointModel *point, void *toPoint);

    TpMatch *getMatch() { return _match; }
    TpQualGroup *getPointQual() { return _pointQual; }
};
#endif        
