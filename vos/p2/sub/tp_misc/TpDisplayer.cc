//////////////////////////////////////////////////////////////////////////////
// TpDisplayer.cc: This class manages multiple subdisplayer windows, of which
// at most three can be displayed on the screen at one time.
//////////////////////////////////////////////////////////////////////////////
#include "TpDisplayer.h"
#include "TpImageReference.h"
#include "TpSubDisplayer.h"
#include "TpMatchManager.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpImageView.h"
#include "TpPosView.h"
#include "Application.h"
#include "ErrorManager.h"
#include "PrefManager.h"
#include "TpDisplayerPrefView.h"
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <assert.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

XtResource TpDisplayer::_resources[] = {
    {
	(char *)"numImagesDisplayed",
	(char *)"NumImagesDisplayed",
	XmRInt,
	sizeof(int),
	XtOffsetOf(TpDisplayer, _numWin),
	XmRImmediate,
	(XtPointer) 3,
    },
    {
	(char *)XvicNcursor,
	(char *)XvicCCursor,
	XtRString,
	sizeof(String),
	XtOffsetOf(TpDisplayer, _cursor),
	XtRImmediate,
	(XtPointer) "crosshair",
    },
    {
        (char *)XvicNcursorBackground,
        (char *)XvicCCursorBackground,
        XtRString,
        sizeof(String),
        XtOffsetOf(TpDisplayer, _cursorColor),
        XtRImmediate,
        (XtPointer) "white",
    },
};

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

// Reload resources

void TpDisplayer::reload(TpDisplayer *copy)
{
    if (_numWin != copy->_numWin)
	setNumWin(copy->_numWin);
    if (strcmp(_cursor, copy->_cursor)) {
	setCursor(copy->_cursor);
    }
    if (strcmp(_cursorColor, copy->_cursorColor)) {
        setCursorColor(copy->_cursorColor);
    }
}

TpDisplayer::TpDisplayer(Widget parent, const char *name, 
		TpMatchManager *mm, TpImageReference *ref)
	: UIComponent(name)
{
    _imageReference = ref;
    for (int i=0; i < TP_MAX_DISPLAYS; i++) {
       _win[i] = 0;
       _locks[i] = False;
    }

    _matchManager = mm;

    for (int j = 0; j < TP_MAX_IMAGES; j++)
	_image[j] = NULL;
    _nimages = 0;

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    if (_numWin > TP_MAX_DISPLAYS) {
        fprintf(stderr, "numImagesDisplayed cannot be greater than %d!\n",
							TP_MAX_DISPLAYS);
        fprintf(stderr, "Correct the resource file!\n");
        _numWin = TP_MAX_DISPLAYS;
    }
    if (_numWin < 1) {
        fprintf(stderr, "numImagesDisplayed cannot be less than 1!\n");
        fprintf(stderr, "Correct the resource file!\n");
        _numWin = 1;
    }

    char path[256];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
                                      _resources, XtNumber(_resources),
                                      path, new TpDisplayerPrefView());
    
    _form = XtVaCreateManagedWidget("dispForm", xmFormWidgetClass, _w, NULL);

    // Callee is responsible for deleting ImageData when it's done!

    for (int i = 1; i < theApplication->getArgc(); i++) {
	addImage(theApplication->getParam(i));
    }

    layoutComponents();
    showComponents();

    Widget shell = parent;
    do {
        shell = XtParent(shell);
    } while (shell && !XtIsShell(shell));
    XtVaSetValues(shell, XtNallowShellResize, False, NULL);
}

TpDisplayer::~TpDisplayer()
{
    // Empty
}

void TpDisplayer::layoutComponents() const
{
    for (int i = 0; i < _nimages; i++)
	XtVaSetValues(_image[i]->baseWidget(),
		XmNx,   0,
		XmNy,   0,
		XmNtopAttachment,	XmATTACH_NONE,
		XmNleftAttachment,	XmATTACH_NONE,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);

    if (_nimages < 1) return;

    for (int w = 0; w < _numWin; w++) {
	if (w >= _nimages) return;		// no more images
	if (_image[_win[w]] == NULL) continue;	// nothing here

	if (w == 0) {			// first one
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
	} else {
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	(w*100)/_numWin, // + 2,
		XmNrightAttachment,     XmATTACH_FORM,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
	}

	if (w == _numWin-1) {		// last one
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNrightAttachment,	XmATTACH_FORM,
		NULL);
	} else {
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNrightAttachment,     XmATTACH_POSITION,
		XmNrightPosition,       ((w+1)*100)/_numWin,
		NULL);
	}
    }
}

///////////////////////////////////////////////////////////////////
// showComponents: Manages only the components to be shown
///////////////////////////////////////////////////////////////////
void TpDisplayer::showComponents() const
{
    for (int w=0; w < _numWin; w++) {
	if (w >= _nimages) return;
        if (_numWin > w) {
	    assert ((_win[w] >= 0) && (_win[w] < _nimages) &&
		(_win[w] >= 0) && (_win[w] < _nimages));
	    _image[_win[w]]->manage();
        }
    }
}

///////////////////////////////////////////////////////////////////
// hideComponents: Unmanages the images so as to hide them.
///////////////////////////////////////////////////////////////////
void TpDisplayer::hideComponents() const
{
    for (int i = 0; i < _nimages; i++)
	_image[i]->unmanage();
}

int TpDisplayer::addImage(char *filename)
{
    if (_nimages >= TP_MAX_IMAGES) {
	printf("Can't load any more images!!\n");
	return -1;
    }
    char name[16];
    sprintf(name,"form%d", _nimages+1);
    TpSubDisplayer *sd = new TpSubDisplayer(_form, name, filename, 
					    _nimages + 1, _matchManager, 
					    this);
    if (sd->failed()) return -1;

    _image[_nimages] = sd;
    _imageReference->indicateLoadedImage(_nimages);
    if (_nimages < _numWin) {
	_win[_nimages] = _nimages;
	_imageReference->setToVisible(_nimages);
	_nimages++;
	layoutComponents();
	showComponents();
	return 0;
    }
    _nimages++;
    return 0;
}

int TpDisplayer::deleteImage(int n)
{
    if (n <= 0)
	return 0;
    if (n > _nimages)
	return 0;

    _matchManager->deleteAllPoints(_image[n-1]->getImageData());

    hideComponents();

    //!!!delete _image[n-1];
    for (int i = n; i < _nimages; i++) {
	_image[i-1] = _image[i];
	_image[i-1]->setNumber(i);
    }
    _nimages--;

    for (int j = 0; j < TP_MAX_DISPLAYS; j++) {
	_win[j] = (_nimages > j) ? j : 0;
    }

    layoutComponents();
    showComponents();

    return 0;
}

void TpDisplayer::shiftRight()
{
    if (_nimages == 1)
	return;

    hideComponents();
    _imageReference->setAllToInvisible();

    for (int w = _numWin-1; w >=0; w--) {
        if (_locks[w])
	    continue;			// don't shift this one
	// Find the next lower slot that isn't locked and transfer it over
	int prev;
	for (prev = w-1; prev >=0; prev--) {
	    if (!_locks[prev]) {
		break;		// found one
	    }
	}
	if (prev >= 0) {
	    _win[w] = _win[prev];	// shift it
	}
        else {
	    // We're the last in line, so gotta get a new image.  Decrement to
	    // a previous image, skipping anything already being shown (in any
	    // window, locked or not).  It's possible we don't find anything
	    // (say, nimages == numWin) in which case we do nothing.

	    int orig_win = _win[w];
	    Boolean good = False;
	    for (int i = 0; i < _nimages; i++) {	// don't try forever
	        _win[w]--;
	        if (_win[w] < 0) _win[w] = _nimages-1;
	        good = True;
	        for (int w2 = 0; w2 < _numWin; w2++) {
		    if (w2 == w) continue;
		    if (_win[w] == _win[w2]) {
		        good = False;
		        break;
		    }
	        }
		if (good)
		    break;
	    }
	    if (!good) {		// couldn't find anything!
		_win[w] = orig_win;
	    }
	}
    }
    DPR(("%d %d %d %d %d %d\n", _win[0], _win[1], _win[2], _win[3], _win[4], _win[5]));

    layoutComponents();
    showComponents();

    for (int i=0; i < _numWin; i++) {
        _imageReference->setToVisible(_win[i]);
    }
}


void TpDisplayer::shiftLeft()
{
    if (_nimages == 1)
	return;

    hideComponents();
    _imageReference->setAllToInvisible();

    for (int w = 0; w <_numWin; w++) {
        if (_locks[w])
	    continue;			// don't shift this one
	// Find the next higher slot that isn't locked and transfer it over
	int next;
	for (next = w+1; next < _numWin; next++) {
	    if (!_locks[next]) {
		break;		// found one
	    }
	}
	if (next < _numWin) {
	    _win[w] = _win[next];	// shift it
	}
        else {
	    // We're the last in line, so gotta get a new image.  Increment to
	    // a later image, skipping anything already being shown (in any
	    // window, locked or not).  It's possible we don't find anything
	    // (say, nimages == numWin) in which case we do nothing.

	    int orig_win = _win[w];
	    Boolean good = False;
	    for (int i = 0; i < _nimages; i++) {	// don't try forever
	        _win[w]++;
	        if (_win[w] >= _nimages) _win[w] = 0;
	        good = True;
	        for (int w2 = 0; w2 < _numWin; w2++) {
		    if (w2 == w) continue;
		    if (_win[w] == _win[w2]) {
		        good = False;
		        break;
		    }
	        }
		if (good)
		    break;
	    }
	    if (!good) {		// couldn't find anything!
		_win[w] = orig_win;
	    }
	}
    }
    DPR(("%d %d %d %d %d %d\n", _win[0], _win[1], _win[2], _win[3], _win[4], _win[5]));

    layoutComponents();
    showComponents();

    for (int i=0; i < _numWin; i++) {
        _imageReference->setToVisible(_win[i]);
    }
}

void TpDisplayer::setLock(int i)
{
    _locks[i] = True;
    _image[_win[i]]->setLock(True);
}

void TpDisplayer::unSetLock(int i)
{
    _locks[i] = False;
    _image[_win[i]]->setLock(False);
}

void TpDisplayer::setNumDisplays(int numWin)
{
    if (numWin <= 0 || numWin > TP_MAX_DISPLAYS) {
        printf("Error in setDisplayType\n");
        numWin = 1;
    }
    if (numWin > _nimages)
	numWin = _nimages;		// not more than num images
    setNumWin(numWin);
}

void TpDisplayer::setNumWin(int numWin)
{
    if (numWin == _numWin) return;
    int oldNumWin = _numWin;
    _numWin = numWin;
    if (_numWin > oldNumWin) {
	// fill in the new images.  We do this by temporarily locking all
	// the old ones, unlocking the new ones, and calling shiftLeft to
	// fill them in one at a time.
	Boolean temp_locks[TP_MAX_DISPLAYS];
	int i;
	for (i=0; i < TP_MAX_DISPLAYS; i++)
	    temp_locks[i] = _locks[i];
	for (i=0; i < oldNumWin; i++)
	    _locks[i] = True;
	for (i = oldNumWin; i < _numWin; i++) {
	    _locks[i] = False;		// may be none unlocked
	    _win[i] = 0;
	}
	for (i = oldNumWin; i < _numWin; i++) {
	    _numWin = i+1;
	    shiftLeft();
	    _locks[i] = True;
	    _numWin = numWin;
	}
	for (i=0; i < TP_MAX_DISPLAYS; i++)
	    _locks[i] = temp_locks[i];
    }

    hideComponents();
    layoutComponents();
    showComponents();
}

void TpDisplayer::setCursor(String newCursor)
{
    _cursor = sdup(newCursor);
    for (int i = 0; i < _numWin; i++) {
	_image[i]->setCursor(newCursor);
    }
}

void TpDisplayer::setCursorColor(String newCursorColor)
{
    _cursorColor = sdup(newCursorColor);
    for (int i = 0; i < _numWin; i++) {
        _image[i]->setCursorColor(newCursorColor);
    }
}

void TpDisplayer::newMatchSelected(TpMatch *match)
{
    if (match == NULL)
	return;
    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
	_image[apoint->getImageNumber()-1]->getZoomView()->setCenter(
	    apoint->getX(), apoint->getY());
	_image[apoint->getImageNumber()-1]->getImageView()->setCenter(
	    apoint->getX(), apoint->getY());
	_image[apoint->getImageNumber()-1]->getPosView()->displayValues();
    }
}

