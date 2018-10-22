//////////////////////////////////////////////////////////////////////////////
// TpPointModel.cc: This class serves as a Model for individual
// points (observations).
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher     JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpPointModel.h"
#include "TpPoint.h"
#include "TpMatch.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "TpMatchManager.h"
#include "ViewMacros.h"
#include "TpDefs.h"
#include "ErrorManager.h"
#include "file_no_path.h"
#include "ibisfile.h"
#include "zvproto.h"
#include <assert.h>
#include <stdio.h>

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

TpPointSymbolShapeType TpPointModel::_pointShape = Dot;
TpTagPosition TpPointModel::_tagPosition = NorthEast;
int TpPointModel::_pointWidth = 10;
int TpPointModel::_pointHeight = 10;
char *TpPointModel::_pointColor = (char *)"red";
char *TpPointModel::_pointColorSel = (char *)"green";
Boolean TpPointModel::_showLabel = True;
#ifndef __VMS
  char *TpPointModel::_colorCodeFilename = (char *)"$V2DATA/gui/ps1.ibis-2";
#else
  char *TpPointModel::_colorCodeFilename = (char *)"v2$data:[gui]ps1.ibis-2";
#endif

TpPointModel::TpPointModel(char *filename, int imageNumber, ImageData *image, 
			   TpMatch *match, TpQualGroupMgr *qualMgr, 
			   double x, double y)
{
    _filename = sdup(filename);
    _imageNumber = imageNumber;
    _image = image;
    _x = x;
    _y = y;
    _views = NULL;
    _numViews = 0;
    _isCurrent = False;

    assert(match != NULL);
    _match = match;
    _pointQual = new TpQualGroup(qualMgr);

    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0, 
			_match->getMatchManager()->getMatchModeResult(3));

}

TpPointModel::~TpPointModel()
{
    for (int i = 0; i < _numViews; i++)
	delete _views[i];
    delete [] _filename;
}

void TpPointModel::attachView(TpPoint *point)
{
    AttachViewMacro(TpPoint, _views, _numViews, point);
    point->update(this);
}

void TpPointModel::detachView(TpPoint *point)
{
    DetachViewMacro(TpPoint, _views, _numViews, point);
    point->update(this);
}

void TpPointModel::updateViews()
{
    for (int i = 0; i < _numViews; i++) 
	_views[i]->update(this);
}

void TpPointModel::setX(double x) 
{ 
    _x = x; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
}

void TpPointModel::setY(double y) 
{ 
    _y = y; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
 
}

void TpPointModel::setXY(double x, double y) 
{ 
    _x = x; 
    _y = y; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
}

void TpPointModel::setCurrent(Boolean isCurrent)
{
    _isCurrent = isCurrent;
    updateViews();	
}

void TpPointModel::listPoint()
{
    char buf[512];
    sprintf(buf, "\tImage %d: \tL = %.3f\t S = %.3f", _imageNumber, _y, _x);
    theErrorManager->process(Warning, NULL, buf);
}

void TpPointModel::setPointShape(TpPointSymbolShapeType shape)
{
    _pointShape = shape;
    updateViews();
}

void TpPointModel::setTagPosition(TpTagPosition position)
{
    _tagPosition = position;
    updateViews();
}

void TpPointModel::setPointWidth(int width)
{
    _pointWidth = width;
    updateViews();
}

void TpPointModel::setPointHeight(int height)
{
    _pointHeight = height;
    updateViews();
}

void TpPointModel::setPointDimensions(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;
    updateViews();
}

void TpPointModel::setPointColor(char *str)
{
    _pointColor = sdup(str);
    updateViews();
}

void TpPointModel::setPointColorSel(char *str)
{
    _pointColorSel = sdup(str);
    updateViews();
}

void TpPointModel::showPointLabels(Boolean show)
{
    _showLabel = show;
    updateViews();
}

void TpPointModel::rotationChanged()
{
    updateViews();
}

void TpPointModel::colorCodePointsGen(int n, float a, float b)
{
    float value;
    if (_match->getGenQual()->getType(n) == TpText)
        return;
    else if (_match->getGenQual()->getType(n) == TpFull) {
        int valueF;
        _match->getGenQual()->getValue(n, valueF);
        value = (float)valueF;
    }
    else if (_match->getGenQual()->getType(n) == TpReal) {
        _match->getGenQual()->getValue(n, value);
    }
    int y = (int)(a * value + b);
    printf("value=%f, y=%d\n", value, y);

    // Read ibis file
    int unit, ibis, status, record, i;
    int lut[256][3];

    char *filename = _colorCodeFilename;
    // open IBIS file for reading
    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
    if (status != 1) return;
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if (status != 1) return;

    char *stripFilename = sdup(filename);
    file_no_path(stripFilename);

    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) return;

    for (i=1; i<=256; i++) {
	status = IBISRecordRead(record, (char*)lut[i-1], i);
	if (status != 1) return;
    }

    IBISFileClose(ibis, 0);

    delete [] stripFilename;

    int red = lut[y][0];
    int grn = lut[y][1];
    int blu = lut[y][2];

    printf("red = %d, green = %d, blue = %d\n", red, grn, blu);
    char buf[16];
    sprintf(buf, "#%02x%02x%02x", red, grn, blu);
    printf("color = %s\n", buf);
    setPointColor(buf);
}

void TpPointModel::colorCodePointsPoint(int n, float a, float b)
{
    float value;
    if (_pointQual->getType(n) == TpText)
	return;
    else if (_pointQual->getType(n) == TpFull) {
	int valueF;
	_pointQual->getValue(n, valueF);
	value = (float)valueF;
    }
    else if (_pointQual->getType(n) == TpReal) {
	_pointQual->getValue(n, value);
    }
    int y = (int)(a * value + b);
    DPR(("value=%f, y=%d\n", value, y));

    // Read ibis file
    int unit, ibis, status, record, i;
    int lut[256][3];
 
#ifndef __VMS
    char *filename = (char *)"$V2DATA/gui/ps1.ibis-2";
#else
    char *filename = (char *)"v2$data:[gui]ps1.ibis-2";
#endif
    // open IBIS file for reading
    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
 
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if(status!=1) return;

    char *stripFilename = sdup(filename);
    file_no_path(stripFilename);
 
    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
 
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
                            0, 0, (char *)IFMT_FULL);
    if (status != 1) return;
 
    for (i=1; i<=256; i++) {
        status = IBISRecordRead(record, (char*)lut[i-1], i);
        if (status != 1) return;
    }
 
    IBISFileClose(ibis, 0);
 
    delete [] stripFilename;

    int red = lut[y][0];
    int grn = lut[y][1];
    int blu = lut[y][2];
 
    DPR(("red = %d, green = %d, blue = %d\n", red, grn, blu));
    char buf[16];
    sprintf(buf, "#%02x%02x%02x", red, grn, blu);
    DPR(("color = %s\n", buf));
    setPointColor(buf);
}

void TpPointModel::setPointShapeStatic(TpPointSymbolShapeType shape)
{
    _pointShape = shape;
}

void TpPointModel::setTagPositionStatic(TpTagPosition tagPosition)
{
    _tagPosition = tagPosition;
}
 
void TpPointModel::setPointWidthStatic(int width)
{
    _pointWidth = width;
}
 
void TpPointModel::setPointHeightStatic(int height)
{
    _pointHeight = height;
}
 
void TpPointModel::setPointDimensionsStatic(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;
}

void TpPointModel::setPointColorStatic(char *str)
{
    _pointColor = sdup(str);
}

void TpPointModel::setPointColorSelStatic(char *str)
{
    _pointColorSel = sdup(str);
}

int TpPointModel::pointCmp(TpPointModel *point, void *toPoint)
{
    return (point == (TpPointModel *) toPoint);
}
