///////////////////////////////////////////////////////////////
// TpSelectionMgr.h: This class keeps a list of selected points.
///////////////////////////////////////////////////////////////
#ifndef TPSELECTIONMGR_H
#define TPSELECTIONMGR_H

#include "TpMatch.h"
#include "sl_lists.h"
#include <Xm/Xm.h>

class TpMatchManager;
class TpPointModel;

class TpSelectionMgr {

  protected:

    TpMatchManager *_matchManager;
    SL_List<TpPointModel *> *_points;
    TpMatch *_match;

  public:

    TpSelectionMgr(TpMatchManager *);
    virtual ~TpSelectionMgr();

    void addMatch(TpMatch *);
    void addPoint(TpPointModel *);
    void selectPoint(TpPointModel *);
    void selectMatchAndPoints(TpMatch *);
    void deletePoint(TpPointModel *);
    void clear();

    int initScan(SL_ListWatch<TpPointModel *> *);
    TpPointModel *nextPoint();
    int scanDone();

    Boolean isEmpty() 
	{ if (_points->get_length() == 0) 
	    return True; 
	  else
	    return False; }

    int getNumPoints() { return _points->get_length(); }

    Boolean isFullHouse();

    TpMatch *getMatch() { return _match; }

    void listPoints();

};
#endif
