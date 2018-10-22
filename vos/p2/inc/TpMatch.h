///////////////////////////////////////////////////////////////
// TpMatch.h: This class keeps a list of a single tiepoint set.
///////////////////////////////////////////////////////////////
#ifndef TPMATCH_H
#define TPMATCH_H
#include "sl_lists.h"
#include "TpDefs.h"
#include <Xm/Xm.h>

class TpMatchManager;
class TpPointModel;
class TpSelectionMgr;
class ImageData;
class TpQualGroup;
class TpQualGroupMgr;

class TpMatch {

  protected:

    TpMatchManager *_manager;

    SL_List<TpPointModel *> *_list;
    Boolean _current;

    TpQualGroup *_genQual;

  public:

    TpMatch(TpMatchManager *);
    virtual ~TpMatch();

    void setId(int id);
    void redrawAll();

    Boolean addPoint(char *filename, int n, ImageData *, 
		     const double x, const double y,
		     TpQualGroupMgr *qualMgr,
		     TpPointModel *&added);
    void addPoint(TpPointModel *);
    void deletePoint(TpPointModel *);
    void deletePoint(ImageData *);
    void clear();
    void setCurrent(TpSelectionMgr *);
    void listPoints();
    void setPointShape(TpPointSymbolShapeType shape);
    void setTagPosition(TpTagPosition tagPosition);
    void setPointWidth(int);
    void setPointHeight(int);
    void setPointDimensions(int w, int h);
    void setPointColor(char *);
    void setPointColorSel(char *);
    void showPointLabels(Boolean show);
    void rotationChanged(ImageData *);
    void colorCodePointsGen(int n, float a, float b);
    void colorCodePointsPoint(int n, float a, float b);

    int initScan(SL_ListWatch<TpPointModel *> *);
    int scanDone();
    TpPointModel *next();
    int getNumPoints() { return _list->get_length(); }

    Boolean isEmpty() 
	{ if(_list->get_length() == 0) return TRUE; else return FALSE; }
    SL_List<TpPointModel *> *getList() { return _list; }

    static int matchCmp(TpMatch *match, void *toMatch);

    TpQualGroup *getGenQual() { return _genQual; }
    TpMatchManager *getMatchManager() { return _manager; }

};
#endif
