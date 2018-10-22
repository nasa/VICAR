//////////////////////////////////////////////////////////////////////////////
// TpMatchManager: Keeps track of all the tiepoint matches.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchManager.h"
#include "TpDisplayer.h"
#include "TpSubDisplayer.h"
#include "TpImageView.h"
#include "VicarImageData.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpPoint.h"
#include "TpPosView.h"
#include "TpSelectionMgr.h"
#include "TpQualGroupMgr.h"
#include "TpMatchModeValues.h"
#include "TpMatchManagerPrefView.h"
#include "ErrorManager.h"
#include "TpApplication.h"
#include "TpWindow.h"
#include "TpDefs.h"
#include "PrefManager.h"
#include "file_no_path.h"
#include "ibistiepnt.h"
#include "ibisfile.h"
#include "zvproto.h"
#include "affpar.h"
#include "kqkkor.h"
#include <Xm/RepType.h>
#include <assert.h>
#include <math.h>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

#define TPDPR(x) if (theTpApplication->getVerbose()) printf x

XtResource TpMatchManager::_resources[] = {
  {
    (char *)"autofindMode",
    (char *)"AutofindMode",
    (char *)XtpRAutofindMode,
    sizeof(TpAutofindMode),
    XtOffset(TpMatchManager *, _autofindMode),
    XmRImmediate,
    (XtPointer)AFFINE,
  },
  {
    (char *)"matchMode",
    (char *)"matchMode",
    (char *)XtpRMatchMode,
    sizeof(TpMatchMode),
    XtOffset(TpMatchManager *, _matchMode),
    XmRImmediate,
    (XtPointer)AUTO_CORR,
  },
  {
    (char *)"pointSymbolShape",
    (char *)"PointSymbolShape",
    (char *)XtpRPointSymbolShapeType,
    sizeof(TpPointSymbolShapeType),
    XtOffset(TpMatchManager *, _pointShape),
    XmRImmediate,
    (XtPointer)CrossWithDot,
  },
  {
    (char *)"pointSymbolWidth",
    (char *)"PointSymbolWidth",
    XmRInt,
    sizeof(int),
    XtOffset(TpMatchManager *, _pointWidth),
    XmRImmediate,
    (XtPointer)15,
  },
  {
    (char *)"pointSymbolHeight",
    (char *)"PointSymbolHeight",
    XmRInt,
    sizeof(int),
    XtOffset(TpMatchManager *, _pointHeight),
    XmRImmediate,
    (XtPointer)15,
  },
  {
    (char *)"pointSymbolColor",
    (char *)"PointSymbolColor",
    XmRString,
    sizeof(String),
    XtOffset(TpMatchManager *, _pointColor),
    XmRImmediate,
    (XtPointer)"red",
  },
  {
    (char *)"pointSymbolSelColor",
    (char *)"PointSymbolSelColor",
    XmRString,
    sizeof(String),
    XtOffset(TpMatchManager *, _pointColorSel),
    XmRImmediate,
    (XtPointer)"green",
  },
  {
    (char *)"pmkCorrParm",
    (char *)"PmkCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _pmkCorrParm),
    XmRString,
    (XtPointer) "7",
  },
  {
    (char *)"lsmCorrParm", 
    (char *)"LsmCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _lsmCorrParm),
    XmRString,
    (XtPointer) "15",
  },
  {
    (char *)"winCorrParm", 
    (char *)"WinCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _winCorrParm),
    XmRString,
    (XtPointer) "20",
  },
  {
    (char *)"accCorrParm", 
    (char *)"AccCorrParm",
    XmRFloat,
    sizeof(float),
    XtOffsetOf(TpMatchManager, _accCorrParm),
    XmRString,
    (XtPointer) "0.3",
  },
  {
    (char *)"corCorrParm", 
    (char *)"CorCorrParm",
    XmRFloat,
    sizeof(float),
    XtOffsetOf(TpMatchManager, _corCorrParm),
    XmRString,
    (XtPointer) "0.5",
  },
  {
    (char *)"fullMatchIsEnforced",
    (char *)"FullMatchIsEnforced",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(TpMatchManager, _fullMatchIsEnforced),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"clickPrecision",
    (char *)"ClickPrecision",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _clickPrecision),
    XmRImmediate,
    (XtPointer) 4,
  },
};

////////////////////////////////////////////////////////////////////////

static Boolean CvtFloatToString(Display *dpy,
        XrmValue *, Cardinal *num_args,
        XrmValue *fromVal, XrmValue *toVal, XtPointer *)
{
   if (*num_args != 0)
      XtAppWarningMsg(
            XtDisplayToApplicationContext(dpy),
            "wrongParameters", "cvtFloatToString", "XtToolkitError",
            "Float to String conversion needs no extra arguments",
            (String *)NULL, (Cardinal *)NULL);

   static char out_val[20];

   sprintf(out_val, "%.2f", *((float *)(fromVal->addr)));

   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(char *)) {
         toVal->size = sizeof(char *);
         return FALSE;
      }
      *((char **)(toVal->addr)) = out_val;
   }
   else
      toVal->addr = (XPointer)&out_val;
   toVal->size = sizeof(char *);
   return TRUE;
}

// Reload resources

void TpMatchManager::reload(TpMatchManager *copy)
{
    if (_autofindMode != copy->_autofindMode)
	setAutofindMode(copy->_autofindMode);
    if (_matchMode != copy->_matchMode)
        setMatchMode(copy->_matchMode);
    if (_pointShape != copy->_pointShape)
	setPointShape(copy->_pointShape);
    if (_pointWidth != copy->_pointWidth)
	setPointWidth(copy->_pointWidth);
    if (_pointHeight != copy->_pointHeight)
        setPointHeight(copy->_pointHeight);
    if (strcmp(_pointColor, copy->_pointColor)) {
	TpPointModel::setPointColorStatic(copy->_pointColor);
	setPointColor(copy->_pointColor);
    }
    if (strcmp(_pointColorSel, copy->_pointColorSel)) {
	TpPointModel::setPointColorSelStatic(copy->_pointColorSel);
        setPointColorSel(copy->_pointColorSel);
    }
    if (_pmkCorrParm != copy->_pmkCorrParm)
	setPmkCorrParm(copy->_pmkCorrParm);
    if (_lsmCorrParm != copy->_lsmCorrParm)
        setLsmCorrParm(copy->_lsmCorrParm);
    if (_winCorrParm != copy->_winCorrParm)
        setWinCorrParm(copy->_winCorrParm);
    if (_accCorrParm != copy->_accCorrParm)
        setAccCorrParm(copy->_accCorrParm);
    if (_corCorrParm != copy->_corCorrParm)
        setCorCorrParm(copy->_corCorrParm);
}

Boolean TpMatchManager::_firstTime = False;

TpMatchManager::TpMatchManager(TpWindow *window)
{
    // One-time class initialization
 
    if (!_firstTime) {
	XmRepTypeId id;

        static String AutofindModeNames[] =
			{(char *)"manual", (char *)"affine", (char *)"spice"};
        static unsigned char AutofindModeValues[] = {MANUAL, AFFINE, SPICE};
        id = XmRepTypeRegister((char *)XtpRAutofindMode,
                               AutofindModeNames, AutofindModeValues,
                               XtNumber(AutofindModeNames));
        XmRepTypeAddReverse(id);

        static String MatchModeNames[] =
			{(char *)"auto_corr", (char *)"affine_only"};
        static unsigned char MatchModeValues[] = {AUTO_CORR, AFFINE_ONLY};
        id = XmRepTypeRegister((char *)XtpRMatchMode,
                               MatchModeNames, MatchModeValues,
                               XtNumber(MatchModeNames));
        XmRepTypeAddReverse(id);
 
	static String PointSymbolShapeNames[] = { 
	    (char *)"crosswithdot",
	    (char *)"rectangle",
	    (char *)"dot",
	    (char *)"cross45",
	    (char *)"crosswithhole45",
	    (char *)"rectanglewithcrosseswithdot" };
	static unsigned char PointSymbolShapeValues[] = { 
	    CrossWithDot,
	    Rectangle,
	    Dot, 
	    Cross45,
	    CrossWithHole45, 
	    RectangleWithCrossesWithDot };
        id = XmRepTypeRegister((char *)XtpRPointSymbolShapeType,
			       PointSymbolShapeNames,
                               PointSymbolShapeValues,
                               XtNumber(PointSymbolShapeNames));
	XmRepTypeAddReverse(id);

	XtSetTypeConverter(XmRFloat, XtRString, CvtFloatToString, 
			NULL, 0, XtCacheNone, (XtDestructor)NULL);

        _firstTime = True;
    }

    _window = window;

    XtGetApplicationResources(_window->baseWidget(), 
			      (XtPointer)this,
			      _resources, XtNumber(_resources),
			      NULL, 0);

    char path[100];
    sprintf(path, "*");
    thePrefManager->registerResources(theApplication->baseWidget(), 
				      (XtPointer)this, 
				      "tpMatchManager", "TpMatchManager", 
                                      _resources, XtNumber(_resources) - 2,
                                      path, new TpMatchManagerPrefView());

    _startId = 1;
    _nextId = _startId - 1;
    _autoSyncPoints = False;
    _checkingGenQualUnique = False;
    _matches = new SL_List<TpMatch *>;
    
    _selectionMgr = new TpSelectionMgr(this);
    _genQualMgr = new TpQualGroupMgr();
    _pointQualMgr = new TpQualGroupMgr();

    _ibisFilename = NULL;
    _dirtyFlag = False;

    _matchModeValues = new TpMatchModeValues(_pmkCorrParm, _lsmCorrParm, 
				_winCorrParm, _accCorrParm, _corCorrParm);

    TpPointModel::setPointShapeStatic(_pointShape);
    TpPointModel::setPointDimensionsStatic(_pointWidth, _pointHeight);
    TpPointModel::setPointColorStatic(_pointColor);
    TpPointModel::setPointColorSelStatic(_pointColorSel);
    //!!!TpPointModel::setTagPositionStatic(_tagPosition); //!!! Not resource

    if (theTpApplication->getConfig())
	_window->loadConfig(theTpApplication->getConfig());

    _referencedSubDisp = NULL;

    for (int cnt = 0; cnt < 5; cnt++)
	_matchModeResults[cnt] = 0.0;

    _autofindResults[0] = _autofindResults[4] = 1.0;
    _autofindResults[1] = _autofindResults[2] = 0.0;
    _autofindResults[3] = _autofindResults[5] = 0.0;
}

void TpMatchManager::closePointsIbis()
{
    if (_ibisFilename) delete [] _ibisFilename;
    _ibisFilename = NULL;

    TpMatch *selectedMatch= _selectionMgr->getMatch();
    _selectionMgr->clear();

    // Delete match only if it is so new, it has never been saved

    if (!isExistingMatch(selectedMatch))
	delete selectedMatch;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        _matches->remove_current();
        delete amatch;
    }
    _window->deactivateSavePointCmd();

    char buf[1][6];
    buf[0][0] = '\0';
    _genQualMgr->setFormat(0, buf);
    _pointQualMgr->setFormat(0, buf);

    // Reset dirty flag

    _dirtyFlag = False;
}

void TpMatchManager::readInitialPoints()
{
    char *pfile = theTpApplication->getPfile();
    if (!pfile || (!strlen(pfile)))
	return;

    readPointsIbis(pfile);
}

///////////////////////////////////////////////////////////////////////
// Load points from an IBIS tiepoint file.
///////////////////////////////////////////////////////////////////////

void TpMatchManager::readPointsIbis(char *filename)
{
    closePointsIbis();

    if (filename && strlen(filename)) {
        _ibisFilename = sdup(filename);
	_window->resetSaveCmd(True);
    }
    else if (!_ibisFilename)
        return;

    int unit, ibis;
 
    int status;
    int i;
 
    // Open tiepoint file for read-only

    std::cout << "Reading tiepoint file " << _ibisFilename << std::endl;

    status = zvunit(&unit, (char *)"tp_file_1",  1, "u_name", _ibisFilename,
									NULL);
    if (status != OK) {
        theErrorManager->process(Error, "zvunit", "Cant process file");
        return;
    }

    int numImages;
    char filenames[MAX_noimgs][FNAMLEN];
    int numGenQual;
    char genQualNames[MAX_nogenqlf][STRING_32];
    char genQualFormat[MAX_nogenqlf][IFMT_SIZE];
    char genQualUnits[MAX_nogenqlf][STRING_32];
    int numPointQual;
    char pointQualNames[MAX_noimgqlf][STRING_32];
    char pointQualFormat[MAX_noimgqlf][IFMT_SIZE];
    char pointQualUnits[MAX_noimgqlf][STRING_32];
    int numMatches;
    
    status = zitiepnt_openr(unit, &ibis, &numImages, filenames,
               &numGenQual, genQualNames, genQualFormat, genQualUnits,
               &numPointQual, pointQualNames, pointQualFormat, pointQualUnits,
               &numMatches);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_openw",
                                 "Error opening a file for write");
	zitiepnt_close(unit);
        return;
    }

    // Now that we know the file is in the right format, clear the selection 
    // and delete all of the existing matches.  Then deactivate 'save point'
    // command in case it was active.

    //_closePointsIbis();
    _selectionMgr->clear();
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	_matches->remove_current();
	delete amatch;
    }
    _window->deactivateSavePointCmd(); // end clear

    // Set qualifiers format

    _genQualMgr->setFormat(numGenQual, genQualFormat);
    _pointQualMgr->setFormat(numPointQual, pointQualFormat);

    // Set general qualifiers name and unit

    for (i = 0; i < numGenQual; i++) {
	_genQualMgr->setQualName(i, genQualNames[i]);
	_genQualMgr->setQualUnit(i, genQualUnits[i]);
    }

    // Set point qualifiers name and unit

    for (i = 0; i < numPointQual; i++) {
        _pointQualMgr->setQualName(i, pointQualNames[i]);
	_pointQualMgr->setQualUnit(i, pointQualUnits[i]);
    }

    // Allocate arrays that are necessary for reading one row of data
    // Notice that we allocate space only once and then reuse storage.

    float *lines = new float[numImages];
    float *samps = new float[numImages];

    int *genQualsFull = new int[numGenQual];
    float *genQualsReal = new float[numGenQual];
    char *genQualsText = new char [numGenQual * 257];

    int *pointQualsFull = new int[numPointQual * numImages];
    float *pointQualsReal = new float[numPointQual * numImages];
    char *pointQualsText = new char [numPointQual * 257 * numImages];

    for (i = 1; i <= numMatches; i++) {

    	// Now declare pointers to each of the arrays so that we can do 
    	// pointer arithmetic without affecting storage for the next row of data

    	int *genQualsFullP = genQualsFull;
    	float *genQualsRealP = genQualsReal;
    	int *pointQualsFullP = pointQualsFull; 
    	float *pointQualsRealP = pointQualsReal;

	// Read one line of data

	status = zitiepnt_read(unit, i, 
                               lines, samps,
                               genQualsReal, genQualsFull, genQualsText,
                               pointQualsReal, pointQualsFull, pointQualsText);
	if (status != OK) {
	    theErrorManager->process(Error, "zitiepoint_openw",
				     "Error opening a file for write");
	    zitiepnt_close(unit);
	    return;
	}

	TpMatch *tmpMatch = new TpMatch(this);

	// Set match's general qualifiers according to the values 
	// obtained from the file

	TpQualGroup *genQual = tmpMatch->getGenQual();
	for (int jg = 0, jqg = 0; jg < numGenQual; jg++) {
	    if (genQual->getType(jg) == TpFull) {
		genQual->setValue(jg, genQualsFullP[0]);
		genQualsFullP++;
	    }
	    else if (genQual->getType(jg) == TpReal) {
		genQual->setValue(jg, genQualsRealP[0]);
                genQualsRealP++;
            }
	    else if (genQual->getType(jg) == TpText) {
		genQual->setValue(jg, genQualsText+(jqg*257));
		jqg++;
            }
	    else
		fprintf(stderr, 
			"Error in readPointsIbis (unknown qual type)\n");
	}

	for (int n = 0; n < numImages; n++) {
	    
	    TpSubDisplayer *aSubDisplayer;
	    int textQualPtr = 0;
	    Boolean fileIsLoaded = False;

	    for (int im = 0; im < _displayer->getNumImages(); im++) {
		aSubDisplayer = _displayer->getImage(im);
		char *shortFilename = sdup(aSubDisplayer->getFilename());
		file_no_path(shortFilename);
		
		if (!strcmp(shortFilename, filenames[n])) {
		    fileIsLoaded = True;

		    if (samps[n] == MISSING_POINT_VALUE &&
			lines[n] == MISSING_POINT_VALUE)
			break;
		    TpPointModel *pointModel;
		    tmpMatch->addPoint(aSubDisplayer->getFilename(),
				       aSubDisplayer->getNumber(),
				       aSubDisplayer->getImageData(),
				       samps[n], lines[n], _pointQualMgr, 
				       pointModel);
		    TpQualGroup *pointQual = pointModel->getPointQual();
		    for (int jp = 0; jp < numPointQual; jp++) {
			if (pointQual->getType(jp) == TpFull) {
			    pointQual->setValue(jp, pointQualsFullP[0]);
			    pointQualsFullP++;
			}
			else if (pointQual->getType(jp) == TpReal) {
			    pointQual->setValue(jp, pointQualsRealP[0]);
			    pointQualsRealP++;
			}
			else if (pointQual->getType(jp) == TpText) {
			    pointQual->setValue(jp, pointQualsText + 
						(textQualPtr * 257));
			    textQualPtr++;
			}
			else
			    printf("Error\n");
		    }

		    createPointViews(pointModel, aSubDisplayer);
		    delete [] shortFilename;
		}
	    }

	    // Skip qualifiers if file exists in the ibis file, but
	    // is not loaded into tp
 
	    if (!fileIsLoaded) {
		for (int m = 0; m < numPointQual; m++) {
		    if (!strcmp(pointQualFormat[m], FULL))
			pointQualsFullP++;
		    else if (!strcmp(pointQualFormat[m], REAL))
			pointQualsRealP++;
		    else
			textQualPtr++;
		}
		if (i == 1) {	// only want to print message once
		    char warnMsg[1024];
		    sprintf(warnMsg, 
		        "Image file %s exists in tiepoint file, but is not loaded",
		        filenames[n]);
		    theErrorManager->process(Error, "Open File", warnMsg);
		}
	    }
	}
	addMatch(tmpMatch);
    }

    status = zitiepnt_close(unit);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_close",
                                 "Error closing a file");
        return;
    }

    // Reset the dirty flag

    _dirtyFlag = False;

    // Now select the first point

    TpMatch *firstMatch = _matches->find_first();
    if (firstMatch) {
	_selectionMgr->selectMatchAndPoints(firstMatch);
	newMatchSelected();
    }

    // Set window title to name of file just loaded

    if (_displayer != NULL && _displayer->getNumImages() > 0) {
        TpSubDisplayer *img = _displayer->getImage(0);
        if (img != NULL) {
            img->getImageView()->setTitle("");  // string is unused
        }
    }

}

///////////////////////////////////////////////////////////////////////
// Save points into an IBIS tiepoint file
///////////////////////////////////////////////////////////////////////
void TpMatchManager::writePointsIbis(char *filename)
{
    if (filename && strlen(filename)) {
	_ibisFilename = sdup(filename);
	_window->resetSaveCmd(True);
    }
    else if (!_ibisFilename)
	return;

    int unit, ibis;

    int status;
    int i;

    // Get number of images

    const int numImages = _displayer->getNumImages();

    // Get image filenames

    char filenames[MAX_noimgs][FNAMLEN];
    TpSubDisplayer *aSubDisplayer;
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	sprintf(filenames[i], "%s", aSubDisplayer->getFilename());
    }

    // Get number of General Qualifiers

    const int numGenQual = _genQualMgr->getNumQuals();

    // Get names of General Qualifiers

    char genQualNames[MAX_nogenqlf][STRING_32];
    for (i = 0; i < numGenQual; i++)
	sprintf(genQualNames[i], _genQualMgr->getQualName(i));

    // Get units of General Qualifiers(e.g. pixels, m, cm, ...)
 
    char genQualUnits[MAX_nogenqlf][STRING_32];
    for (i = 0; i < numGenQual; i++)
        sprintf(genQualUnits[i], _genQualMgr->getQualUnit(i));

    // Get format of General Qualifiers

    char genQualFormat[MAX_nogenqlf][IFMT_SIZE];
    for (i = 0; i < numGenQual; i++) {
	if (_genQualMgr->getType(i) == TpFull) 
	    strcpy(genQualFormat[i], FULL);
	else if (_genQualMgr->getType(i) == TpReal)
            strcpy(genQualFormat[i], REAL);
	else
            strcpy(genQualFormat[i], TEXT);
    }

    // Get number of Image Qualifiers

    const int numPointQual = _pointQualMgr->getNumQuals();

    // Get names of Image Qualifiers

    char pointQualNames[MAX_noimgqlf][STRING_32];
    for (i = 0; i < numPointQual; i++)
	sprintf(pointQualNames[i], _pointQualMgr->getQualName(i));

    // Get unit values of Image Qualifiers

    char pointQualUnits[MAX_noimgqlf][STRING_32];
    for (i = 0; i < numPointQual; i++)
        sprintf(pointQualUnits[i], _pointQualMgr->getQualUnit(i));
    
    // Get format of Image Qualifiers
    
    char pointQualFormat[MAX_noimgqlf][IFMT_SIZE];
    for (i = 0; i < numPointQual; i++) { //!!! Implement GUI for this
        if (_pointQualMgr->getType(i) == TpFull)
            strcpy(pointQualFormat[i], FULL);
        else if (_pointQualMgr->getType(i) == TpReal)
            strcpy(pointQualFormat[i], REAL);
        else
            strcpy(pointQualFormat[i], TEXT);
    }

    // Get number of rows (number of tiepoints)

    const int numMatches = _matches->get_length();

    // Get vicar file unit

    status = zvunit(&unit, (char *)"tp_file_2", 1, "u_name",_ibisFilename,NULL);
    if (status != OK) {
        theErrorManager->process(Error, "zvunit", "Cant process file");
	return;
    }

    status = zitiepnt_openw(unit, &ibis, numImages, filenames,
	       numGenQual, genQualNames, genQualFormat, genQualUnits,
	       numPointQual, pointQualNames, pointQualFormat, pointQualUnits,
	       numMatches);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_openw", 
				 "Error opening a file for write");
        return;
    }

    int numFullPointQuals = _pointQualMgr->getNumFullQuals();
    int numRealPointQuals = _pointQualMgr->getNumRealQuals();
    int numTextPointQuals = _pointQualMgr->getNumTextQuals();

    // Get the data and write it row by row

    SL_ListWatch<TpMatch *>wm1;
    _matches->init_scan(&wm1);
    TpMatch *amatch;
    i = 1;
    while ((amatch = _matches->next()) != NULL) {
	float *lines = new float[numImages];
	float *samps = new float[numImages];

	int *fullPointQuals = new int [numImages * numFullPointQuals];
	float *realPointQuals = new float [numImages * numRealPointQuals];
	char *textPointQuals = new char [numImages * numTextPointQuals * 257];

	int n;                                   // Loop variable
	for (n = 0; n < numImages; n++) {
	    lines[n] = MISSING_POINT_VALUE;
	    samps[n] = MISSING_POINT_VALUE;
	}

	int fullPQCounter = 0;
	int realPQCounter = 0;
	int textPQCounter = 0;

	SL_ListWatch<TpPointModel *> wp1;
	amatch->initScan(&wp1);
	TpPointModel *apoint;
	while ((apoint = amatch->next()) != NULL) {
	    for (n = 0; n < numImages; n++) {
		if(!strcmp(apoint->getFilename(), filenames[n])) {
		    lines[n] = apoint->getY();
		    samps[n] = apoint->getX();
		    int k = 0;
		    TpQualGroup *pointQualGroup = apoint->getPointQual();
		    for (k = 0; k < numFullPointQuals; k++)
			fullPointQuals[fullPQCounter++] 
			    = pointQualGroup->getFullQuals()[k];
		    for (k = 0; k < numRealPointQuals; k++)
                        realPointQuals[realPQCounter++]
                            = pointQualGroup->getRealQuals()[k];
		    char *textQuals = pointQualGroup->getTextQuals();
		    if (textQuals) 
			strcat(textPointQuals + textPQCounter, textQuals);
		    textPQCounter += numTextPointQuals * 257;
		}
	    }
	}

	TpQualGroup *genQual = amatch->getGenQual();
	status = zitiepnt_write(unit, i++, lines, samps, 
				genQual->getRealQuals(),
				genQual->getFullQuals(), 
				genQual->getTextQuals(),
				realPointQuals, 
				fullPointQuals,
				textPointQuals);
	if (status != OK) {
	    theErrorManager->process(Error, "zitiepoint_write",
				     "Error writing a file");
	    return;
	}
    }

    status = zitiepnt_close(unit);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_close",
                                 "Error closing a file");
        return;
    }

    _dirtyFlag = False;

    // Set window title to name of file just saved

    if (_displayer != NULL && _displayer->getNumImages() > 0) {
        TpSubDisplayer *img = _displayer->getImage(0);
        if (img != NULL) {
            img->getImageView()->setTitle("");  // string is unused
        }
    }

}

TpMatchManager::~TpMatchManager()
{
    // Empty
}

//////////////////////////////////////////////////////////////////////////////
// Create new point at user's request.  genInMain means that the event was 
// generated in the main image and not the zoomed image.
// There are four possible actions that the 
// software will undertake.  First is pure manual, second is using affine 
// transformation only, third is use correlation only, fourth is use 
// both affine transformation and correlation followed by each other.
//////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processNewPoint(const double x, const double y,
                                     TpSubDisplayer *subDisplayer,
                                     Boolean genInMain)
{
    _dirtyFlag = True;

    for (int i = 0; i < 5; i++) _matchModeResults[i] = 0.0;

    if ((_matchMode == AUTO_CORR) && (_autofindMode == AFFINE))
	automatch(x, y, subDisplayer, genInMain);
    if ((_matchMode == AFFINE_ONLY) && (_autofindMode == AFFINE))
        affine(x, y, subDisplayer, genInMain);
    if ((_matchMode == AUTO_CORR) && (_autofindMode == MANUAL))
        assist(x, y, subDisplayer, genInMain);
    if ((_matchMode == AFFINE_ONLY) && (_autofindMode == MANUAL))
        manual(x, y, subDisplayer, genInMain);
}

//////////////////////////////////////////////////////////////////////////////
// manual(): Place a point in given image at given location.
//////////////////////////////////////////////////////////////////////////////
int TpMatchManager::manual(const double x, const double y, 
			   TpSubDisplayer *subDisplayer,
			   Boolean genInMain, Boolean genByAlgorithm)
{
    if (!isInImage(x, y, subDisplayer))
	return -1;

    if (genInMain == True)
        subDisplayer->getZoomView()->setCenter(x, y);

    if (genByAlgorithm == True)
	subDisplayer->getImageView()->setCenter(x, y);

    //subDisplayer->getPosView()->cursorMoved(x, y);

    // If selection manager already reports point in this image, simply move it

    TpPointModel *apoint = selectedPoint(_selectionMgr, subDisplayer);
    if (apoint) {
	apoint->setXY(x, y);
	subDisplayer->getPosView()->displayValues();
	return 0;
    }
    
    // New point, so enable 'keep point' cmd
 
    _window->activateSavePointCmd();

    // Create a new match if necessary

    if (_selectionMgr->isEmpty())
        _selectionMgr->addMatch(new TpMatch(this));

    // If not every point in the match is already displayed, we may not 
    // need a new point.

    if (!_selectionMgr->isFullHouse()) {
	
	// Search selected match for this point on this display

	Boolean pointAlreadyExists = False;
	TpMatch *selectedMatch = _selectionMgr->getMatch();
	SL_ListWatch<TpPointModel *> wp2;
	selectedMatch->initScan(&wp2);
	while ((apoint = selectedMatch->next()) != NULL)
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		pointAlreadyExists = True;
		selectedMatch->scanDone();
		break;
	    }
	if (pointAlreadyExists) {
	    _selectionMgr->addPoint(apoint);
	    apoint->setXY(x, y);
	    subDisplayer->getPosView()->displayValues();
	    return 0;
	}
    }

    // Finally, we figured that we do need to create a new point

    TpPointModel *newPoint;
    newPoint = new TpPointModel(subDisplayer->getFilename(),
				subDisplayer->getNumber(),
				subDisplayer->getImageData(),
				_selectionMgr->getMatch(),
				_pointQualMgr, x, y);
    createPointViews(newPoint, subDisplayer);
    _selectionMgr->addPoint(newPoint);
    subDisplayer->getPosView()->displayValues();
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// Perform correlation
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::assist(const double x, const double y,
			    TpSubDisplayer *subDisplayer,
			    Boolean genInMain)
{
    // If there were no referenced image - here is the one

    if (_referencedSubDisp == NULL) {
	_referencedSubDisp = subDisplayer;
	manual(x, y, subDisplayer, genInMain);
	return;
    }

    // Return if there is a point already in the selection
 
    Boolean found = False;
    TpMatch *match = _selectionMgr->getMatch();
    SL_ListWatch<TpPointModel *> wp1;
    match->initScan(&wp1);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
        if (subDisplayer->getImageData() == apoint->getImageData())
            found = True;
    }
    if (found) {
	manual(x, y, subDisplayer, genInMain);
	return;
    }

    // Find out the referenced point and match against that point 

    double xCorr, yCorr;
    Boolean createNewPoint = False;
 
    SL_ListWatch<TpPointModel *> wp2;
    match->initScan(&wp2);
    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("%s","\n");		// work around stupid g++ 3.3 bug
    while ((apoint = match->next()) != NULL) {
        if (_referencedSubDisp->getImageData() == apoint->getImageData()) {
            doMatch(apoint->getX(), apoint->getY(),
                    _referencedSubDisp, x, y, subDisplayer, xCorr, yCorr);
            createNewPoint = True;
        }
    }

    if (createNewPoint)
        manual(xCorr, yCorr, subDisplayer, True, True);
}

/////////////////////////////////////////////////////////////////////////////
// foreach subDisplayer except current do
//   foreach match except current do
//     extract point information
//   run affinpar
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::affine(const double x, const double y,
			    TpSubDisplayer *subDisplayer, Boolean genInMain)
{
    // Is this a brand new point?

    TpPointModel *pointExists = selectedPoint(_selectionMgr, subDisplayer);

    // First of all, place a point in a given image in specified location

    int status = manual(x, y, subDisplayer, genInMain);
    if (status == -1)
	return;

    if (pointExists && !_autoSyncPoints)
	return;

    // If this is the first point, we can't do affine transformation yet

    if (numMatches() == 0)
	return;

    // Now adjust all the other points in the match

    double xOut, yOut;

    TpSubDisplayer *aSubDisplayer;
    for (int i = 0; i < _displayer->getNumImages(); i++) {
        aSubDisplayer = _displayer->getImage(i);
        if (subDisplayer == aSubDisplayer)
            continue;
        status = doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);

	// Set the new point only if the new point is within the image

	if (status == 0)
	    manual(xOut, yOut, aSubDisplayer, True, True);
    }
}

/////////////////////////////////////////////////////////////////////////////
// First do affine transformation, then run correlator
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::automatch(const double x, const double y,
			       TpSubDisplayer *subDisplayer, 
			       Boolean genInMain)
{
    // Are there any corresponding points already?

    Boolean otherPoints = False;
    TpSubDisplayer *aSubDisplayer;
    int i;
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	if (selectedPoint(_selectionMgr, aSubDisplayer))
	    otherPoints = True;
    }

    // First of all, place a point in a given image in specified location
 
    int status = manual(x, y, subDisplayer, genInMain);
    if (status == -1)
        return;

    if (otherPoints && !_autoSyncPoints)
        return;
 
    // If this is the first point, we can't do affine transformation yet
 
    if (numMatches() == 0)
        return;
 
    // Now adjust all the other points in the match

    double xOut, yOut;

    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("%s", "\n");		// work around stupid g++ 3.3 bug
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	if (subDisplayer == aSubDisplayer)
	    continue;
	int status = doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);
	if (status == -1)
	    continue;
	
	double korXOut, korYOut;
	status = doMatch(x, y, subDisplayer, xOut, yOut, 
			 aSubDisplayer, korXOut, korYOut);
	if (status == -1)
	    continue;
	
	xOut = korXOut;
	yOut = korYOut;
	
	manual(korXOut, korYOut, aSubDisplayer, True, True);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Support code for running affine transformation.
/////////////////////////////////////////////////////////////////////////////
int TpMatchManager::doAffine(const double x, const double y, 
			     TpSubDisplayer *subDisplayerIn, 
			     TpSubDisplayer *subDisplayerOut, 
			     double &xOut, double &yOut)
{
    TPDPR(("--------------------------------------------------------\n"));
    TPDPR(("Running affpar()...\n"));
    TPDPR(("File ref: %s; File target: %s\n",
	 subDisplayerIn->getFilename(), subDisplayerOut->getFilename()));

    xOut = yOut = 0.0;  // Initialize

    int i = 0;
    int j = 0;
    int n = _matches->get_length();
    double *line0 = new double[n];
    double *samp0 = new double[n];
    double *line1 = new double[n];
    double *samp1 = new double[n];
    SL_ListWatch<TpMatch *> wma;
    _matches->init_scan(&wma);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_ListWatch<TpPointModel *> wpa;
	amatch->initScan(&wpa);
	TpPointModel *apoint;
	Boolean sys0Exist = False;
	Boolean sys1Exist = False;
	while ((apoint = amatch->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayerIn->getImageData()) {
		line0[i] = apoint->getY();
		samp0[i] = apoint->getX();
		sys0Exist = True;
	    }
	    if (apoint->getImageData() == subDisplayerOut->getImageData()) {
		line1[i] = apoint->getY();
		samp1[i] = apoint->getX();
		sys1Exist = True;
	    }
	}
	if (sys0Exist && sys1Exist)
	    i++;
    }

    for (j = 0; j < i; j++) {
	TPDPR(("lref[%d]=%.1f\tsref[%d]=%.1f\tltrg[%d]=%.1f\tstrg[%d]=%.1f\n",
	       j, line0[j], j, samp0[j], j, line1[j], j, samp1[j]));
    }

    affinpar(y, x, i, line0, samp0, line1, samp1, &yOut, &xOut, 
	     _autofindResults);
    TPDPR(("REF: line = %f\t samp = %f\n", y, x));
    TPDPR(("TRG: line = %f\t samp = %f\n", yOut, xOut));

    TPDPR(("Results: %f, %f, %f, %f, %f, %f\n", 
	 _autofindResults[0], _autofindResults[1], _autofindResults[2],
	 _autofindResults[3], _autofindResults[4], _autofindResults[5]));

    _window->getAutofindResultsDialog()->setValues(_autofindResults);

    TPDPR(("End running affpar().\n"));
    TPDPR(("--------------------------------------------------------\n"));

    // Check if the location is off image, in that case return status -1

    if (!isInImage(xOut, yOut, subDisplayerOut))
	return -1;

    return 0;
}

Boolean TpMatchManager::isInImage(const double x, const double y, 
				  TpSubDisplayer *sd)
{
    if ((y < 0) || (x < 0))
	return False;
    if (y > sd->getImageData()->getNumbLines())
	return False;
    if (x > sd->getImageData()->getNumbSamples())
	return False;

    return True;
}

//////////////////////////////////////////////////////////////////////////////
// Search selection manager for the point in given image
//////////////////////////////////////////////////////////////////////////////
TpPointModel *TpMatchManager::selectedPoint(TpSelectionMgr *sm,
                                           TpSubDisplayer *sd)
{
    Boolean pointAlreadyExists = False;
    SL_ListWatch<TpPointModel *> wp1;
    sm->initScan(&wp1);
    TpPointModel *apoint;
    while ((apoint = sm->nextPoint()) != NULL) {
        if (apoint->getImageData() == sd->getImageData()) {
            pointAlreadyExists = True;
            sm->scanDone();
            break;
        }
    }
    if (pointAlreadyExists)
	return apoint;
    else 
	return NULL;
}

double TpMatchManager::getAutofindResult(int i)
{
    assert((i >= 0) && (i < 6));
    return _autofindResults[i];
}

float TpMatchManager::getMatchModeResult(int i)
{
    assert((i >= 0) && (i < 5));
    return _matchModeResults[i];
}

/////////////////////////////////////////////////////////////////////////////
// Support code for running correlator
/////////////////////////////////////////////////////////////////////////////
int TpMatchManager::doMatch(const double x, const double y,
			    TpSubDisplayer *subDisplayerIn,
			    const double sx, const double sy,
			    TpSubDisplayer *subDisplayerOut,
			    double &xOut, double &yOut)
{
    TPDPR(("--------------------------------------------------------\n"));
    TPDPR(("Running correlator routines...\n"));
    TPDPR(("File ref: %s; File target: %s\n",
         subDisplayerIn->getFilename(), subDisplayerOut->getFilename()));

    // Initialize

    xOut = sx;
    yOut = sy;

    KORPAR info;
    info.suchfns = _matchModeValues->_searchWindow;
    info.pmkdim  = _matchModeValues->_pmk;
    info.kqkdim  = _matchModeValues->_lsm;
    info.lsmacu  = _matchModeValues->_accuracy;
    info.minpmk  = _matchModeValues->_threshold;
    info.affin[0] = _autofindResults[0];
    info.affin[1] = _autofindResults[1];
    info.affin[2] = _autofindResults[2];
    info.affin[3] = _autofindResults[3];
    info.affin[4] = _autofindResults[4];
    info.affin[5] = _autofindResults[5];

    INT_MATRIX musterma;
    INT_MATRIX suchma;
 
    dim_muster_such (&info, &musterma, &suchma);

    // Deal with the reference image first

    int *bufIn = new int[musterma.dimz * musterma.dims + 1];
    musterma.ptr_m = bufIn;

    // Read image

    ImageData *imDataIn = subDisplayerIn->getImageData();
    int pixelSize = imDataIn->getPixelSize();
    int numberSamples = imDataIn->getNumbSamples();
    int lineWidth = pixelSize * numberSamples;
    unsigned char *buffer = new unsigned char[lineWidth];

    int startLine = (int)(floor(y + .5) - (musterma.dimz / 2));
    int endLine = (int)(floor(y + .5) + (musterma.dimz / 2));
    int startSamp = (int)(floor(x + .5) - (musterma.dims / 2));
    int endSamp = (int)(floor(x + .5) + (musterma.dims / 2));

    int countBuf = 0;
    int i, j;
    for (i = startLine; i <= endLine; i++) {
	imDataIn->readLine(BWcolor, i, buffer);
	if (imDataIn->getPixelType() == imBYTE) {
	    imByte *bufB = (imByte *)buffer;
	    for (j = startSamp; j <= endSamp; j++)
		bufIn[countBuf++] = bufB[j];
	}
	else if (imDataIn->getPixelType() == imHALF) {
	    imHalf *bufH = (imHalf *)buffer;
	    for (j = startSamp; j <= endSamp; j++)
                bufIn[countBuf++] = bufH[j];
	}
    }
    bufIn[countBuf] = 0;
    delete [] buffer;

    // Now deal with the target image

    int *bufOut = new int[suchma.dimz * suchma.dims + 1];
    suchma.ptr_m = bufOut;
 
    // Read image
 
    ImageData *imDataOut = subDisplayerOut->getImageData();
    pixelSize = imDataOut->getPixelSize();
    numberSamples = imDataOut->getNumbSamples();
    lineWidth = pixelSize * numberSamples;
    buffer = new unsigned char[lineWidth];
 
    startLine = (int)(floor(sy + .5) - (suchma.dimz / 2));
    endLine = (int)(floor(sy + .5) + (suchma.dimz / 2));
    startSamp = (int)(floor(sx + .5) - (suchma.dims / 2));
    endSamp = (int)(floor(sx + .5) + (suchma.dims / 2));

    countBuf = 0;
    for (i = startLine; i <= endLine; i++) {
        imDataOut->readLine(BWcolor, i, buffer);
        if (imDataOut->getPixelType() == imBYTE) {
            imByte *bufBout = (imByte *)buffer;
            for (j = startSamp; j <= endSamp; j++)
                bufOut[countBuf++] = bufBout[j];
        }
        else if (imDataOut->getPixelType() == imHALF) {
            imHalf *bufHout = (imHalf *)buffer;
            for (j = startSamp; j <= endSamp; j++)
                bufOut[countBuf++] = bufHout[j];
        }
    }
    bufOut[countBuf] = 0;
    delete [] buffer;

    // Call the subroutine that does correlation
    // References for the matching algorithm:
    // Ackermann F. (1984): High Precision Digital Image Correlation. 
    //                      Proc 39th Photogrammetric Week, Stuttgart 1984.

    ERG erg; // the result will be returned in this structure
    kqkkor (&info, &musterma, &suchma, &erg);
    delete [] bufIn;
    delete [] bufOut;

    yOut = (double) sy + erg.dz;
    xOut = (double) sx + erg.ds;

    TPDPR(("%f --> %f\n", sy, yOut));
    TPDPR(("%f --> %f\n", sx, xOut));
    
    TPDPR(("Results: %.2f, %.2f, %.2f, %.2f, %.2f\n\n", 
	   erg.dz, erg.ds, erg.pmk, erg.kqk, erg.mp));

    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("Match image %d to %d results: %8.2f, %8.2f, %7.4f, %7.4f, %7.4f\n",
		subDisplayerIn->getNumber(), subDisplayerOut->getNumber(),
		erg.dz, erg.ds, erg.pmk, erg.kqk, erg.mp);

    _matchModeResults[0] = erg.dz;
    _matchModeResults[1] = erg.ds;
    _matchModeResults[2] = erg.pmk;
    _matchModeResults[3] = erg.kqk;
    _matchModeResults[4] = erg.mp;

    _window->getMatchModeResultsDialog()->setValues(_matchModeResults);

    TPDPR(("End correlator routines.\n"));
    TPDPR(("--------------------------------------------------------\n"));

    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// Given point model, we need to create point views for the main image 
// display and for the zoomed image display.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::createPointViews(TpPointModel *model, 
		TpSubDisplayer *subDisplayer)
{
    TpPoint *pointI, *pointZ;
    pointI = new TpPoint(model, subDisplayer->getImageView()->getWidget());
    pointZ = new TpPoint(model, subDisplayer->getZoomView()->getWidget());
}

/////////////////////////////////////////////////////////////////////////////
// Select full match after user clicked in (x, y) on one of the 
// sub displayers.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processSelectMatch(const double x, const double y, 
		TpSubDisplayer *subDisplayer)
{
    _selectionMgr->clear();
    _referencedSubDisp = NULL;

    int minDistX, minDistY;
    minDistX = minDistY = _clickPrecision;
    TpMatch *closestMatch;

    SL_ListWatch<TpMatch *> watch;
    Boolean foundClosest = False;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_List<TpPointModel *> *matchList = amatch->getList();
	SL_ListWatch<TpPointModel *> watch2;
	matchList->init_scan(&watch2);
	TpPointModel *apoint;
	while ((apoint = matchList->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		int distX = abs((int)x - (int)apoint->getX());
		int distY = abs((int)y - (int)apoint->getY());
		if ((distX < minDistX) && (distY < minDistY)) {
		    foundClosest = True;
		    closestMatch = amatch;
		}
	    }
	}
    }

    if (foundClosest) {
	_selectionMgr->selectMatchAndPoints(closestMatch);
	subDisplayer->getZoomView()->setCenter(x, y);
    }

    _dirtyFlag = True;
}

/////////////////////////////////////////////////////////////////////////////
// Select single point after user clicked in (x, y) on one of the
// sub displayers.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processSelectPoint(const double x, const double y, 
		TpSubDisplayer *subDisplayer)
{
    TpMatch *selectedMatch = _selectionMgr->getMatch();
    _selectionMgr->clear();
    _referencedSubDisp = NULL;

    int minDistX, minDistY;
    minDistX = minDistY = _clickPrecision;
    TpPointModel *closestPoint;
    TpMatch *closestMatch;

    SL_ListWatch<TpMatch *> watch;
    Boolean foundClosest = False;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_List<TpPointModel *> *matchList = amatch->getList();
	SL_ListWatch<TpPointModel *> watch2;
	matchList->init_scan(&watch2);
	TpPointModel *apoint;
	while ((apoint = matchList->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		int distX = abs((int)x - (int)apoint->getX());
		int distY = abs((int)y - (int)apoint->getY());
		if ((distX < minDistX) && (distY < minDistY)) {
		    foundClosest = True;
		    closestPoint = apoint;
		    closestMatch = amatch;
		}
	    }
	}
    }

    if (foundClosest) {
	_selectionMgr->addMatch(closestMatch);
	_selectionMgr->selectPoint(closestPoint);
	subDisplayer->getZoomView()->setCenter(closestPoint->getX(), 
					       closestPoint->getY());
    }
    else {

	// Delete match only if it is so new, it has never been saved

	if (!isExistingMatch(selectedMatch))
	    delete selectedMatch;
    }

    _dirtyFlag = True;
}

void TpMatchManager::processMovePoint(TpSubDisplayer *subDisplayer, 
				const double amountX, const double amountY)
{
    TpMatch *match = _selectionMgr->getMatch();
    if (match == NULL)
	return;			// no points, so do nothing
    SL_List<TpPointModel *> *points = match->getList();
    SL_ListWatch<TpPointModel *> watch2;
    points->init_scan(&watch2);
    TpPointModel *apoint;
    while ((apoint = points->next()) != NULL) {
	if (apoint->getImageData() == subDisplayer->getImageData()) {
	    double oldX = apoint->getX();
	    double oldY = apoint->getY();
	    double newX = oldX + amountX;
	    double newY = oldY + amountY;
	    processNewPoint(newX, newY, subDisplayer, False);
	}
    }
    _dirtyFlag = True;
}

void TpMatchManager::processScrollAll(const double x, const double y,
				      TpSubDisplayer *subDisplayer)
{
    if (_autofindMode == AFFINE) {
	TpSubDisplayer *aSubDisplayer;
	for (int i = 0; i < _displayer->getNumImages(); i++) {
	    aSubDisplayer = _displayer->getImage(i);
	    if (subDisplayer == aSubDisplayer)
		continue;
	    double xOut, yOut;
	    doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);
	    aSubDisplayer->getZoomView()->setCenter(xOut, yOut);
	    aSubDisplayer->getImageView()->setCenter(xOut, yOut);
	}
	TPDPR(("Scroll at %f %f\n", x, y));
    }

    else if (_autofindMode == MANUAL) {
	// do nothing
    }
    
    else if (_autofindMode == SPICE) {
	//!!! Spice functions that allow to find a tiepoint go here!
    }

    else {
	fprintf(stderr, "Not a valid TpAutofindMode value!\n");
    }
}

//////////////////////////////////////////////////////////////////////////////
// isExistingMatch: Check if the match is on the list.  Return true is it is 
// on the list.  Return false if the match is not on the list (meaning it is 
// in temporary storage.
//////////////////////////////////////////////////////////////////////////////
Boolean TpMatchManager::isExistingMatch(TpMatch *match)
{
    if (match == NULL) return FALSE;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	if (amatch == match) {
	    _matches->scan_done();
	    return TRUE;
	}
    }
    return FALSE;
}

TpMatch *TpMatchManager::getNthMatch(int n)
{
    TpMatch *match = _matches->get_nth(n-1);
    return match;
}

//////////////////////////////////////////////////////////////////////////
// Return running id of the match or the next available running id if the 
// match was not found
//////////////////////////////////////////////////////////////////////////
int TpMatchManager::getMatchNumber(TpMatch *match)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    int i = 1;
    while ((amatch = _matches->next()) != NULL) {
        if (amatch == match) {
	    _matches->scan_done();
	    return i;
	}
	i++;
    }
    return i;
}

//////////////////////////////////////////////////////////////////////////
// addMatch: Add selected match to the list of matches.
//////////////////////////////////////////////////////////////////////////
void TpMatchManager::addMatch()
{
    addMatch(_selectionMgr->getMatch());
}

//////////////////////////////////////////////////////////////////////////
// addMatch: Add specific match to the list of matches.  Unselect 
// everything.
//////////////////////////////////////////////////////////////////////////
void TpMatchManager::addMatch(TpMatch *match)
{
    if (!match) {
	theErrorManager->process(Error, "Saving operation", 
				 "Nothing to save");
	return;
    }

    if (isExistingMatch(match))
	theErrorManager->process(Error, NULL, 
				 "This tiepoint had already been added!");
    else if (_fullMatchIsEnforced &&
	     (match->getList()->get_length() < _displayer->getNumImages())) {
	theErrorManager->process(Error, NULL, 
				 "Cannot add incomplete tiepoint!");
	return;
    }
    else {
	_matches->add(match);
	_nextId++;
    }

    _selectionMgr->clear();
    _referencedSubDisp = NULL;
}

///////////////////////////////////////////////////////////
// deleteCurrentSelection: Delete currently selected points 
// or the whole match.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteCurrentSelection()
{
    if (_selectionMgr->isEmpty()) {
	theErrorManager->process(Error, "Delete Operation", 
	        "You have to select point or observation before deleting it");
	return;
    }

    if (_selectionMgr->isFullHouse()) {
	deleteCurrentMatch();
	_dirtyFlag = True;
	return;
    }
    else if (_fullMatchIsEnforced) {
	theErrorManager->process(Error, "Delete Point", 
			"This mode only allows to delete COMPLETE matches", 
			"see *fullMatchIsEnforced resource");
	return;
    }

    TpMatch *selectedMatch = _selectionMgr->getMatch();

    SL_ListWatch<TpPointModel *> watch;
    _selectionMgr->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = _selectionMgr->nextPoint()) != NULL) {
	_selectionMgr->deletePoint(apoint);
	selectedMatch->deletePoint(apoint);
    }
    _dirtyFlag = True;
}

///////////////////////////////////////////////////////////
// deleteAllPoints: Delete all points belonging to certain image.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteAllPoints(ImageData *id)
{
    _selectionMgr->clear();

    SL_ListWatch<TpMatch *> watch;

    TpMatch *amatch;
    _matches->init_scan(&watch);
    while ((amatch = _matches->next()) != NULL) {
	amatch->deletePoint(id);
    }	
}

///////////////////////////////////////////////////////////
// deleteLastAddedMatch: Delete last added match
///////////////////////////////////////////////////////////
void TpMatchManager::deleteLastAddedMatch()
{
    _selectionMgr->clear();

    SL_ListWatch<TpMatch *> watch;

    _matches->init_scan(&watch);
    for (int i = 0; i < _matches->get_length(); i++)
	_matches->next();
    TpMatch *match = _matches->remove_current();
    delete match;
    _matches->scan_done();
}

///////////////////////////////////////////////////////////
// deleteCurrentMatch: Delete selected match.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteCurrentMatch()
{
    TpMatch *toDelete = _selectionMgr->getMatch();
    _selectionMgr->clear();

    // Find a match that needs to be deleted

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	if (amatch == toDelete) {
	    _matches->remove_current();
	    _matches->scan_done();
	    delete amatch;
	    toDelete = NULL; //!!! Not sure if this is needed
	    break;
	}
    }

    // If the match was new and not saved, then we have to delete it 
    // explicitly

    if (toDelete) delete toDelete;
}

void TpMatchManager::listPoints()
{
    SL_ListWatch<TpMatch *> watch;
    int i = 0;

    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	char buf[512];
	sprintf(buf, "Tiepoint #%d:", ++i);
	theErrorManager->process(Warning, NULL, buf);
	amatch->listPoints();
    }
    theErrorManager->process(Warning, NULL,
                "====================================================");
}

void TpMatchManager::newMatchSelected()
{
    _displayer->newMatchSelected(_selectionMgr->getMatch());
    _matchBrowser->newMatchSelected(_selectionMgr->getMatch());
}

void TpMatchManager::setPointShape(TpPointSymbolShapeType shape)
{
    _pointShape = shape;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointShape(shape);
    }
}

void TpMatchManager::setTagPosition(TpTagPosition tagPosition)
{
    _tagPosition = tagPosition;
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setTagPosition(_tagPosition);
    }
}

void TpMatchManager::setPointWidth(int width)
{
    _pointWidth = width;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointWidth(width);
    }
}

void TpMatchManager::setPointHeight(int height)
{
    _pointHeight = height;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointHeight(height);
    }
}

void TpMatchManager::setPointDimensions(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointDimensions(width, height);
    }
}

void TpMatchManager::setPointColor(char *str)
{
    _pointColor = sdup(str);
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointColor(str);
    }
}

void TpMatchManager::setPointColorSel(char *str)
{
    _pointColorSel = sdup(str);
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointColorSel(str);
    }
}

void TpMatchManager::showPointLabels(Boolean show)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->showPointLabels(show);
    }    
}

void TpMatchManager::rotationChanged(ImageData *id)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	amatch->rotationChanged(id);
    }
}

void TpMatchManager::setPmkCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_pmk = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setLsmCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_lsm = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setWinCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_searchWindow = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setAccCorrParm(float p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_accuracy = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setCorCorrParm(float p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_threshold = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

// Color code points based on the general qualifier
void TpMatchManager::colorCodePointsGen(int n)
{
    int minValue;
    _genQualMgr->getMinValue(n, minValue);
    int maxValue;
    _genQualMgr->getMaxValue(n, maxValue);

    float a = 255.0 / float(maxValue - minValue);
    float b = 255.0 - (maxValue * a);

    DPR(("min = %d, max = %d, a = %f, b = %f\n", minValue, maxValue, a, b));

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->colorCodePointsGen(n, a, b);
    }
}

// Color code points based on the point qualifier
void TpMatchManager::colorCodePointsPoint(int n)
{
    int minValue;
    int status;
    status = _pointQualMgr->getMinValue(n, minValue);
    if (status == -1)
	return;		// no usable qualifier
    int maxValue;
    status = _pointQualMgr->getMaxValue(n, maxValue);
    if (status == -1)
	return;		// no usable qualifier

    float a = 255.0 / float(maxValue - minValue);
    float b = 255.0 - (maxValue * a);

    DPR(("min = %d, max = %d, a = %f, b = %f\n", minValue, maxValue, a, b));

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->colorCodePointsPoint(n, a, b);
    }
}

void TpMatchManager::redoIds()
{
    _nextId = _startId - 1;
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	amatch->setId(_nextId + 1);
	_nextId++;
    }

    _dirtyFlag = True;
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<TpMatch *>
#pragma define_template SL_ListWatch<TpMatch *>
#pragma define_template SL_List<TpPointModel *>
#pragma define_template SL_ListWatch<TpPointModel *>
#pragma define_template SL_List<TpSubDisplayer *>
#pragma define_template SL_ListWatch<TpSubDisplayer *>
#endif

