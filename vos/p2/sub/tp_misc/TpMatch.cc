//////////////////////////////////////////////////////////////////////////////
// TpMatch.cc: This class keeps a list of a single tiepoint set.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher     JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpMatch.h"
#include "TpMatchManager.h"
#include "TpPointModel.h"
#include "TpSelectionMgr.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "ErrorManager.h"
#include "TpDefs.h"
#include <iostream>

TpMatch::TpMatch(TpMatchManager *manager)
{
    _manager = manager;

    _current = FALSE;
    _list = new SL_List<TpPointModel *>;

    _genQual = new TpQualGroup(_manager->getGenQualMgr());

    if (_genQual->getNumQuals() > 0)
	if (_genQual->getType(0) == TpFull) //!!! && !strcmp(name, "ID")
	    _genQual->setValue(0, _manager->getNextId() + 1);
}

TpMatch::~TpMatch()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *current;

    _list->init_scan(&watch);
    while ((current = _list->next()) != NULL) {
	_list->remove_current();
	delete current;
    }
    delete _list;
}

void TpMatch::setId(int id)
{
    if (_genQual->getNumQuals() > 0) {
	if (_genQual->getType(0) == TpFull) {
	    _genQual->setValue(0, id);
	    redrawAll();
	}
    }
}

void TpMatch::redrawAll()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	apoint->updateViews();
    }
}

///////////////////////////////////////////////////////////////////
// Return point model that is being added.  Return True if point is 
// new, False if already exist.
///////////////////////////////////////////////////////////////////
Boolean TpMatch::addPoint(char *filename, int n, ImageData *imageData, 
			  const double x, const double y,
			  TpQualGroupMgr *qualMgr,
			  TpPointModel *&added)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	if (apoint->getImageData() == imageData) {
	    apoint->setXY(x, y);
	    added = apoint;
	    return False;
	}
    }

    // Prepare a new point

    added = new TpPointModel(filename, n, imageData, this, qualMgr, x, y);

    addPoint(added);

    return True;
}

///////////////////////////////////////////////////////////////////
// This is fast and unsafe way to add point.  The caller should 
// make sure that the point doesn't already exist in the match.
///////////////////////////////////////////////////////////////////
void TpMatch::addPoint(TpPointModel *newPoint)
{
    // Copy the list, adding the new point in the right sequence, so that
    // points are sorted in ascending order
 
    SL_List<TpPointModel *> *newList = new SL_List<TpPointModel *>;
    SL_ListWatch<TpPointModel *> watch;
    _list->init_scan(&watch);
    TpPointModel *apoint;
    Boolean isAdded = False;
    int n = newPoint->getImageNumber();
    while ((apoint = _list->next()) != NULL) {
        if (!isAdded && (apoint->getImageNumber() > n)) {
            newList->add(newPoint);
	    isAdded = True;
	}
        newList->add(apoint);
    }
    if (!isAdded)
	newList->add(newPoint);
 
    delete _list;
    _list = newList;
}

void TpMatch::deletePoint(TpPointModel *p)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
	if (toDelete == p) {
	    _list->remove_current();
	    delete p;
	    _list->scan_done();
	    break;
	}
    }
}

void TpMatch::deletePoint(ImageData *id)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
        if (toDelete->getImageData() == id) {
            _list->remove_current();
            delete toDelete;
            _list->scan_done();
            break;
        }
    }
}

void TpMatch::clear()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
	_list->remove_current();
	delete toDelete;
    }
}

void TpMatch::setCurrent(TpSelectionMgr *selectionMgr)
{
    _current = True;

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	selectionMgr->addPoint(apoint);
    }

    selectionMgr->addMatch(this);
}

//////////////////////////////////////////////////////////
// listPoints: Output info about each point in the match
//////////////////////////////////////////////////////////
void TpMatch::listPoints()
{
    if (_genQual->getNumQuals() > 0) {
        if (_genQual->getType(0) == TpFull) { //!!! && !strcmp(name, "ID")
	    char buf[256];
	    sprintf(buf, "Point ID = %s", _genQual->valueToString(0));
            theErrorManager->process(Warning, NULL, buf); 
	}
    }

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *point;

    _list->init_scan(&watch);
    while ((point = _list->next()) != NULL) {
	point->listPoint();
    }
    theErrorManager->process(Warning, NULL, 
		"----------------------------------------------------");
}

void TpMatch::setPointShape(TpPointSymbolShapeType shape)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointShape(shape);
    }
}

void TpMatch::setTagPosition(TpTagPosition tagPosition)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setTagPosition(tagPosition);
    }
}

void TpMatch::setPointWidth(int width)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointWidth(width);
    }
}

void TpMatch::setPointHeight(int height)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointHeight(height);
    }
}

void TpMatch::setPointDimensions(int width, int height)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointDimensions(width, height);
    }
}

void TpMatch::setPointColor(char *str)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointColor(str);
    }
}

void TpMatch::setPointColorSel(char *str)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointColorSel(str);
    }
}

void TpMatch::showPointLabels(Boolean show)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->showPointLabels(show);
    }
}

void TpMatch::rotationChanged(ImageData *id)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	if (id == apoint->getImageData())
	    apoint->rotationChanged();
    }
}

void TpMatch::colorCodePointsGen(int n, float a, float b)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->colorCodePointsGen(n, a, b);
    }
}

void TpMatch::colorCodePointsPoint(int n, float a, float b)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->colorCodePointsPoint(n, a, b);
    }
}

int TpMatch::initScan(SL_ListWatch<TpPointModel *> *watch)
{
    int status;
    status = _list->init_scan(watch);
    // failed = _list->failed;
    return status;
}

TpPointModel *TpMatch::next()
{
    TpPointModel *p = _list->next();
    // failed = _list->failed;
    return p;
}

int TpMatch::scanDone()
{
    int status;
    status = _list->scan_done();
    // failed = _list->failed;
    return status;
}

int TpMatch::matchCmp(TpMatch *match, void *toMatch)
{
    return (match == (TpMatch *) toMatch);
}
