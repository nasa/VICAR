///////////////////////////////////////////////////////////////////
// TpMatchManager.h: Keeps track of all the tiepoint matches.
///////////////////////////////////////////////////////////////////
#ifndef TPMATCHMANAGER_H
#define TPMATCHMANAGER_H
#include "TpMatch.h"
#include "TpDisplayer.h"
#include "TpMatchBrowseControl.h"
#include "TpMatchModeValues.h"
#include "TpDefs.h"
#include "sl_lists.h"
#include <iostream>

class TpWindow;
class ImageData;
class TpPointModel;
class TpSelectionMgr;
class TpSubDisplayer;
class TpQualGroupMgr;

class TpMatchManager {

  private:

    static Boolean _firstTime;
    static XtResource _resources[];

    TpWindow *_window;
    TpDisplayer *_displayer;
    TpMatchBrowseControl *_matchBrowser;

    int _nextId;
    int _startId;
    SL_List<TpMatch *> *_matches;

    TpSelectionMgr *_selectionMgr;
    TpQualGroupMgr *_genQualMgr;
    TpQualGroupMgr *_pointQualMgr;

    int _clickPrecision;
    Boolean _fullMatchIsEnforced;
    Boolean _autoSyncPoints;
    Boolean _checkingGenQualUnique;

    char *_ibisFilename;
    Boolean _dirtyFlag;

    TpAutofindMode _autofindMode;
    TpMatchMode _matchMode;
    TpMatchModeValues *_matchModeValues;
    TpSubDisplayer *_referencedSubDisp;

    TpPointSymbolShapeType _pointShape;
    int _pointWidth;
    int _pointHeight;
    char *_pointColor;
    char *_pointColorSel;
    TpTagPosition _tagPosition;

    double _autofindResults[6];
    float _matchModeResults[5];

    int _pmkCorrParm;
    int _lsmCorrParm;
    int _winCorrParm;
    float _accCorrParm;
    float _corCorrParm;

    void panZoomWindow(int x, int y, TpSubDisplayer *);

    static Boolean isInImage(const double x, const double y, TpSubDisplayer *);
    static TpPointModel *selectedPoint(TpSelectionMgr *, TpSubDisplayer *);

    int manual(const double x, const double y, TpSubDisplayer *, 
		Boolean, Boolean genByAlgorithm = False);
    void affine(const double x, const double y, TpSubDisplayer *, Boolean);
    void assist(const double x, const double y, TpSubDisplayer *, Boolean);
    void automatch(const double x, const double y, TpSubDisplayer *, Boolean);

    // Support routine to do affine transformation

    int doAffine(const double x, const double y,
		 TpSubDisplayer *in, TpSubDisplayer *out,
		 double &xOut, double &yOut);

    // Support routine to de the correlation

    int doMatch(const double x, const double y, TpSubDisplayer *in, 
		const double xs, const double sy, TpSubDisplayer *out,
		double &xOut, double &yOut);

  public:

    TpMatchManager(TpWindow *);
    virtual ~TpMatchManager();

    void reload(TpMatchManager *);

    void setDisplayer(TpDisplayer *displayer) { _displayer = displayer; }
    void setMatchBrowser(TpMatchBrowseControl *b) { _matchBrowser = b; }

    TpSelectionMgr *getSelectionMgr() { return _selectionMgr; }
    TpQualGroupMgr *getGenQualMgr() { return _genQualMgr; }
    TpQualGroupMgr *getPointQualMgr() { return _pointQualMgr; }

    void readInitialPoints();
    void listPoints();
    void writePointsIbis(char *filename=NULL);
    void readPointsIbis(char *filename=NULL);
    void closePointsIbis();

    void processNewPoint(const double x, const double y, 
			 TpSubDisplayer *, Boolean genInMain = True);
    void processSelectPoint(const double x, const double y, 
			    TpSubDisplayer *);
    void processSelectMatch(const double x, const double y, 
			    TpSubDisplayer *);
    void processScrollAll(const double x, const double y,
                            TpSubDisplayer *);
    void processMovePoint(TpSubDisplayer *, const double, const double);

    void newMatchSelected();
    void selectMatch(int number);

    void createPointViews(TpPointModel *, TpSubDisplayer *);

    int getNextId() { return _nextId; }
    void setNextId(int nextId) { _nextId = nextId - 1; }
    int getStartId() { return _startId; }
    void setStartId(int startId) { _startId = startId; redoIds(); }

    int numMatches() { return _matches->get_length(); }
    int getMatchNumber(TpMatch *);
    TpMatch *getNthMatch(int);
    void addMatch();
    void addMatch(TpMatch *match);
    void deleteCurrentSelection();
    void deleteLastAddedMatch();
    void deleteCurrentMatch();
    void deleteAllPoints(ImageData *);
    Boolean isExistingMatch(TpMatch *match);

    char *getIbisFileName() { return _ibisFilename; }
    Boolean isDirty() { return _dirtyFlag; }

    void setAutofindMode(TpAutofindMode mode) { _autofindMode = mode; }
    TpAutofindMode getAutofindMode() { return _autofindMode; }

    double getAutofindResult(int i);
    float getMatchModeResult(int i);

    void setMatchMode(TpMatchMode mode) { _matchMode = mode; }
    TpMatchMode getMatchMode() { return _matchMode; }

    TpMatchModeValues *getMatchModeValues() { return _matchModeValues; }
    void setMatchModeValues(TpMatchModeValues *value)
        { _matchModeValues = value;
	std::cout << _matchModeValues->_pmk << std::endl;
	std::cout << _matchModeValues->_lsm << std::endl;
	std::cout << _matchModeValues->_searchWindow << std::endl;
	std::cout << _matchModeValues->_accuracy << std::endl;
	std::cout << _matchModeValues->_threshold << std::endl; }

    void setPointShape(TpPointSymbolShapeType shape);
    TpPointSymbolShapeType getPointSymbolShape() { return _pointShape; }

    void setPointWidth(int);
    int getPointWidth() { return _pointWidth; }

    void setPointHeight(int);
    int getPointHeight() { return _pointHeight; }

    void setPointDimensions(int w, int h);

    void setPointColor(char *);
    void setPointColorSel(char *);
    char *getPointColor() { return _pointColor; }
    char *getPointColorSel() { return _pointColorSel; }

    void colorCodePointsGen(int n);
    void colorCodePointsPoint(int n);

    void setTagPosition(TpTagPosition tagPosition);
    TpTagPosition getTagPosition() { return _tagPosition; }

    void setPmkCorrParm(int p);
    void setLsmCorrParm(int p);
    void setWinCorrParm(int p);
    void setAccCorrParm(float p);
    void setCorCorrParm(float p);

    void showPointLabels(Boolean show);
    void rotationChanged(ImageData *id);

    void redoIds();

    void setAutoSyncPoints(Boolean v) { _autoSyncPoints = v; }

    Boolean isCheckingGenQualUnique() { return _checkingGenQualUnique; }
    void setCheckingGenQualUnique(Boolean v) { _checkingGenQualUnique = v; }

    virtual const char *const className() { return ("TpMatchManager"); } 
};
#endif
