///////////////////////////////////////////////////////////////
// TpSelectionMgr.cc:  Manages points and matches selected by 
// user.  At the core of this class are two elements.  First, a 
// match that is currently being selected.  Because only some 
// points from the match could be selected, it is necessary also 
// to keep a list of points from the match that have been selected.
// A list of points stored by selection manager is a subset of 
// points in the selected match.  
///////////////////////////////////////////////////////////////
#include "TpSelectionMgr.h"
#include "TpMatchManager.h"
#include "TpMatch.h"
#include "TpPointModel.h"

TpSelectionMgr::TpSelectionMgr(TpMatchManager *mm)
{
    _matchManager = mm;
    _match = NULL;
    _points = new SL_List<TpPointModel *>;
}

TpSelectionMgr::~TpSelectionMgr()
{
    // Empty
}

///////////////////////////////////////////////////////////////////
// Add match, no points
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::addMatch(TpMatch *match)
{
    // Save match

    _match = match;

    _matchManager->newMatchSelected();
}

///////////////////////////////////////////////////////////////////
// Add match and all its points.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::selectMatchAndPoints(TpMatch *match)
{
    if (_match != NULL) {
	if (!_matchManager->isExistingMatch(_match)) {
	    delete _match;
	    _points->clear();
	    _match = NULL;
        }
        else {
	    clear();
        }
    }

    if (!match) return;

    // Save match

    addMatch(match);

    // Select all points from selected match

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _match->initScan(&watch);
    while ((apoint = _match->next()) != NULL) {
	selectPoint(apoint);
    }
}

///////////////////////////////////////////////////////////////////
// Add point to the match and to selection manager.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::addPoint(TpPointModel *p)
{
    _match->addPoint(p);
    selectPoint(p);
}

///////////////////////////////////////////////////////////////////
// Select point, don't add it to the match, it's already in the 
// match.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::selectPoint(TpPointModel *p)
{
    p->setCurrent(True);
    _points->add_replace(p);
}

///////////////////////////////////////////////////////////////////
// Delete point
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::deletePoint(TpPointModel *p)
{
    p->setCurrent(False);

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _points->init_scan(&watch);
    while ((toDelete = _points->next()) != NULL) {
	if (toDelete == p)
	    _points->remove_current();
    }

    // Clear match if this was the last point
    if (_points->get_length() == 0)
	addMatch(NULL);
}

///////////////////////////////////////////////////////////////////
// Clear list of points and match.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::clear()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _points->init_scan(&watch);
    while ((toDelete = _points->next()) != NULL) {
	toDelete->setCurrent(False);
	_points->remove_current();
    }

    addMatch(NULL);
}

///////////////////////////////////////////////////////////////////
// The following functions help other classes to interact with 
// the list of points.  They merely imitate the functions of list 
// template class and allow to protect the list itself.
///////////////////////////////////////////////////////////////////
int TpSelectionMgr::initScan(SL_ListWatch<TpPointModel *> *watch)
{
    int status;
    status = _points->init_scan(watch);
    // failed = _points->failed; 
    return status;
}

TpPointModel *TpSelectionMgr::nextPoint()
{
    TpPointModel *p;
    p = _points->next();
    // failed = _points->failed;
    return p;
}

int TpSelectionMgr::scanDone()
{
    int status;
    status = _points->scan_done();
    // failed = _points->failed;
    return status;
}

//////////////////////////////////////////////////////////
// Check if all points in match are selected, or just a 
// subset.
//////////////////////////////////////////////////////////
Boolean TpSelectionMgr::isFullHouse()
{
    if (_points->get_length() == _match->getNumPoints())
	return True;
    else
	return False;
}

//////////////////////////////////////////////////////////
// listPoints: Output info about each point in the match, mostly 
// for testing purposes.  This function should probably be removed 
// or modified for the final delivery.
//////////////////////////////////////////////////////////
void TpSelectionMgr::listPoints()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *point;

    initScan(&watch);
    while ((point = _points->next()) != NULL) {
	point->listPoint();
    }
}
