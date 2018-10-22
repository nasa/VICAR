////////////////////////////////////////////////////////////////////
// HistBox.cc: Container for the histogram display
///////////////////////////////////////////////////////////////////
#include "HistGraphView.h"
#include "StatView.h"
#include "HistHorAxisView.h"
#include "HistVerAxisView.h"
#include "HistBox.h"
#include "Histogram.h"
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdlib.h>
#include <ctype.h>

// Class variables

int HistBox::_histBoxInit = False;

// Resources for this class

XtResource HistBox::_resources [ ] = {
 {
    (char *)XvicNorientation,
    (char *)XvicCOrientation,
    (char *)XvicROrientType,
    sizeof ( OrientType ),
    XtOffset ( HistBox *, _histOrient ),
    XmRImmediate,
    ( XtPointer ) HORIZONTAL,
 },
 {
    (char *)XvicNpopupDirection,
    (char *)XvicCPopupDirection,
    (char *)XvicRPopupDirectionType,
    sizeof ( PopupDirectionType ),
    XtOffset ( HistBox *, _popDirection ),
    XmRImmediate,
    ( XtPointer ) COLUMN,
 },
 {
    (char *)XvicNmethod,
    (char *)XvicCMethod,
    (char *)XvicRMethodType,
    sizeof ( MethodType ),
    XtOffset ( HistBox *, _method ),
    XmRImmediate,
    ( XtPointer ) POPUP,
 },
 {
    (char *)XvicNshowAxis,
    (char *)XvicCShowAxis,
    (char *)XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( HistBox *, _showAxis ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNshowStat,
    (char *)XvicCShowStat,
    (char *)XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( HistBox *, _showStat ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNshowHist,
    (char *)XvicCShowHist,
    (char *)XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( HistBox *, _showHist ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNverAxisDir,
    (char *)XvicCVerAxisDir,
    (char *)XvicRVerAxisDirType,
    sizeof ( VerAxisDirType ),
    XtOffset ( HistBox *, _verAxisDir ),
    XmRImmediate,
    ( XtPointer ) ASC,
 },
};

String HistBox::_defaults[] = {
    (char *)"*statView*shadowType:          		shadow_in",
    (char *)"*statView*isAligned:			True",
    (char *)"*statView*entryAlignment:			ALIGNMENT_END",
    (char *)"*statView*stDev.labelString:           	000.000",
    (char *)"*statView*mean.labelString:            	000.000",
    (char *)"*statView*stDev1.labelString:          	000.000",
    (char *)"*statView*mean1.labelString:           	000.000",
    (char *)"*statView*stDev2.labelString:          	000.000",
    (char *)"*statView*mean2.labelString:           	000.000",
     NULL,
  };


HistBox::HistBox(Widget parent, const char *name,
		Histogram *histR, Histogram *histG, Histogram *histB) 
	: UIComponent(name)
{
	// One-time class initialization

   	if (!_histBoxInit) {
      	   XtSetTypeConverter(XmRString, XvicRPopupDirectionType,
                &HistBox::CvtStringToPopupDirectionType, NULL, 0, XtCacheNone, NULL);

      	   XtSetTypeConverter(XmRString, XvicRMethodType,
                &HistBox::CvtStringToMethodType, NULL, 0, XtCacheNone, NULL);

           XtSetTypeConverter(XmRString, XvicROrientType,
                &HistBox::CvtStringToOrientType, NULL, 0, XtCacheNone, NULL);

      	   _histBoxInit = True;
	}

        // Save pointers to the histograms
        _histR = histR;
        _histG = histG;
        _histB = histB;

	// Load the histogram default resources into the database
	setDefaultResources ( parent, _defaults );

	// Create a form to hold the other widgets
	_w = XtVaCreateWidget ( _name, 
				xmFormWidgetClass, parent, 
				NULL);
	installDestroyHandler();

	getResources ( _resources, XtNumber ( _resources ) );


	/****************************************************************/
	/* 	POPUP 							*/
	/****************************************************************/
        if ( (_method == POPUP) && histR && histG && histB )
        {
		// Set up UI components
                _histBoxR = new HistBox(_w, "histbox", histR, NULL, NULL);
		_histBoxG = new HistBox(_w, "histbox", NULL, histG, NULL);
		_histBoxB = new HistBox(_w, "histbox", NULL, NULL, histB);

		// Separate statistics with a separator
                _sepRG = XtVaCreateManagedWidget ( "sepRG",
                        xmSeparatorWidgetClass, _w,
			NULL );
                _sepGB = XtVaCreateManagedWidget ( "sepGB",
                        xmSeparatorWidgetClass, _w,
			NULL );

		// Set up constraints
		setPopupDirectionType( _popDirection );
        }

	/****************************************************************/
        /*      BLEND or STACKED                                        */
        /****************************************************************/
	else
	{
	   	// Set up UI components
	   	_histHorAxisView = new HistHorAxisView (_w, "histHorAxisView", 
					histR, histG, histB, _histOrient );
	   	_histVerAxisView = new HistVerAxisView (_w, "histVerAxisView", 
					histR, histG, histB, _histOrient, _verAxisDir );

	   	_histGraphView   = new HistGraphView (_w, "histGraphView", 
					histR, histG, histB,
					_method, _histOrient, _verAxisDir );
	   	_statView        = new StatView (_w, "statView", 
					histR, histG, histB );
	

	   	// Set up constraints
	   	layComponents ();

		// manage components
		showComponents ();
        }

        // Work around ugly flashing effects as the window shrinks and
        // re-expands.
        Widget topLevel = (Widget) _w;
        while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
                topLevel = XtParent(topLevel);
        XtVaGetValues(topLevel, XtNallowShellResize, &_shell_resize, NULL);
        XtVaSetValues(topLevel, XtNallowShellResize, FALSE, NULL);
}

HistBox::~HistBox()
{
	Widget topLevel = (Widget) _w;
	while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
                topLevel = XtParent(topLevel);
	XtVaSetValues(topLevel, XtNallowShellResize, _shell_resize, NULL);

	if( _histBoxR ) delete _histBoxR;
	if( _histBoxG ) delete _histBoxG;
	if( _histBoxB ) delete _histBoxB;

	if( _sepRG ) XtDestroyWidget(_sepRG);
	if( _sepGB ) XtDestroyWidget(_sepGB);
}

void HistBox::showAxis (Boolean show) 
{ 
   _showAxis=show; 
   if ( (_method == POPUP) && _histR && _histG && _histB )
   {
     	_histBoxR->showAxis(show);
     	_histBoxG->showAxis(show);
     	_histBoxB->showAxis(show);
   }
   else
   {
	hideComponents();
	layComponents(); 
	showComponents(); 
   }
}

void HistBox::showHist (Boolean show) 
{ 
   _showHist=show; 
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) )
   {
     	_histBoxR->showHist(show);
     	_histBoxG->showHist(show);
     	_histBoxB->showHist(show);
   }
   else
   {
	hideComponents();
	layComponents(); 
	showComponents();
   }
}

void HistBox::showStat (Boolean show) 
{
   _showStat = show;
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) )
   {
     	_histBoxR->showStat(show);
     	_histBoxG->showStat(show);
     	_histBoxB->showStat(show);
   }
   else
   {
	hideComponents();
	layComponents(); 
	showComponents(); 
   }
}

/************************************************************************/
/*	setPopupDirectionType						*/
/************************************************************************/
void HistBox::setPopupDirectionType ( PopupDirectionType popDirection)
{
	_popDirection = popDirection;

	if ( (_method != POPUP) || (_popDirection != popDirection) ) 
	{
		setMethodType ( POPUP );
	}

        if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) )
        {
	   if (XtIsManaged(_w)) _histBoxR->unmanage();
	   if (XtIsManaged(_w)) _histBoxG->unmanage();
	   if (XtIsManaged(_w)) _histBoxB->unmanage();

           switch (_popDirection) {
           case ROW:

                XtVaSetValues   ( _histBoxR->baseWidget(),
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNrightAttachment,     XmATTACH_POSITION,
                        XmNrightPosition,       30,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );

                XtVaSetValues ( _sepRG,
                        XmNorientation,         XmVERTICAL,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_POSITION,
                        XmNleftPosition,        32,
                        XmNrightAttachment,     XmATTACH_NONE,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );

                XtVaSetValues   ( _histBoxG->baseWidget(),
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_POSITION,
                        XmNleftPosition,        35,
                        XmNrightAttachment,     XmATTACH_POSITION,
                        XmNrightPosition,       65,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );

                XtVaSetValues ( _sepGB,
                        XmNorientation,         XmVERTICAL,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_POSITION,
                        XmNleftPosition,        67,
                        XmNrightAttachment,     XmATTACH_NONE,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );

                XtVaSetValues   ( _histBoxB->baseWidget(),
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_POSITION,
                        XmNleftPosition,        70,
                        XmNrightAttachment,     XmATTACH_FORM,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );
                break;

           case COLUMN:

                XtVaSetValues   ( _histBoxR->baseWidget(),
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNbottomAttachment,    XmATTACH_POSITION,
                        XmNbottomPosition,      30,
                        XmNrightAttachment,     XmATTACH_FORM,
                        NULL );

                XtVaSetValues ( _sepRG,
                        XmNorientation,         XmHORIZONTAL,
                        XmNtopAttachment,       XmATTACH_POSITION,
                        XmNtopPosition,         32,
                        XmNbottomAttachment,    XmATTACH_NONE,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNrightAttachment,     XmATTACH_FORM,
                        NULL );

                XtVaSetValues   ( _histBoxG->baseWidget(),
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNtopAttachment,       XmATTACH_POSITION,
                        XmNtopPosition,         35,
                        XmNbottomAttachment,    XmATTACH_POSITION,
                        XmNbottomPosition,      65,
                        XmNrightAttachment,     XmATTACH_FORM,
                        NULL );

                XtVaSetValues ( _sepGB,
                        XmNorientation,         XmHORIZONTAL,
                        XmNtopAttachment,       XmATTACH_POSITION,
                        XmNtopPosition,         67,
                        XmNbottomAttachment,    XmATTACH_NONE,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNrightAttachment,     XmATTACH_FORM,
                        NULL );

                XtVaSetValues   ( _histBoxB->baseWidget(),
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNtopAttachment,       XmATTACH_POSITION,
                        XmNtopPosition,         70,
                        XmNrightAttachment,     XmATTACH_FORM,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );
                break;

           default:

                cerr << "Pop direction must be ROW or COLUMN\n";

           }
	}
	_histBoxR->manage();
        _histBoxG->manage();
        _histBoxB->manage();
}

/************************************************************************/
/*      setMethodType							*/
/************************************************************************/
void HistBox::setMethodType ( MethodType method )
{
	MethodType oldMethod = _method;
        _method = method;

	if ( (_method == POPUP) && ((oldMethod == BLEND) || (oldMethod == STACKED)) )
	{
	        // get current spike value
	        int current_spike = _histGraphView->getSpike();

		// Check if histogram has logarithmic scale
		Boolean logScale = _histGraphView->logScaleIsSet();

		// Unmanage and delete the old staff
		hideComponents();

                // Set up UI components
                _histBoxR = new HistBox(_w, "histbox", _histR, NULL, NULL);
                _histBoxG = new HistBox(_w, "histbox", NULL, _histG, NULL);
                _histBoxB = new HistBox(_w, "histbox", NULL, NULL, _histB);

		_histBoxR->_method = POPUP;
		_histBoxG->_method = POPUP;
		_histBoxB->_method = POPUP;

		_histBoxR->_popDirection = _popDirection;
		_histBoxG->_popDirection = _popDirection;
		_histBoxB->_popDirection = _popDirection;

		_histBoxR->setOrientType (_histOrient);
		_histBoxG->setOrientType (_histOrient);
		_histBoxB->setOrientType (_histOrient);

                // Set the spike value to the current value 
                // so that it is updated properly
		_histBoxR->setSpike(current_spike);
		_histBoxG->setSpike(current_spike);
		_histBoxB->setSpike(current_spike);

		// Set logarithmic scale if necessary
		_histBoxR->setLogScale(logScale);
		_histBoxG->setLogScale(logScale);
		_histBoxB->setLogScale(logScale);

		_histBoxR->setVerAxisDirType (_verAxisDir);
		_histBoxG->setVerAxisDirType (_verAxisDir);
		_histBoxB->setVerAxisDirType (_verAxisDir);

		_histBoxR->showStat (_showStat);
		_histBoxR->showAxis (_showAxis);
		_histBoxR->showHist (_showHist);

                _histBoxG->showStat (_showStat);
                _histBoxG->showAxis (_showAxis);
                _histBoxG->showHist (_showHist);

                _histBoxB->showStat (_showStat);
                _histBoxB->showAxis (_showAxis);
                _histBoxB->showHist (_showHist);

                // Separate statistics with a separator
                _sepRG = XtVaCreateManagedWidget ( "sepRG",
                        xmSeparatorWidgetClass, _w,
                        NULL );
                _sepGB = XtVaCreateManagedWidget ( "sepGB",
                        xmSeparatorWidgetClass, _w,
                        NULL );

                // Set up constraints
                setPopupDirectionType( _popDirection );
	}

	else if ( (oldMethod == POPUP) && ((_method == BLEND) || (_method == STACKED)) )
	{
	        // get current spike value
	        int current_spike = _histBoxR->getSpike();

		// Check if histogram has logarithmic scale
                Boolean logScale = _histGraphView->logScaleIsSet();

		// Unmanage and delete the old stuff
		_histBoxR->unmanage();
		_histBoxG->unmanage();
		_histBoxB->unmanage();
		XtUnmanageChild ( _sepRG );
		XtUnmanageChild ( _sepGB );
		XtDestroyWidget ( _sepRG );
		XtDestroyWidget ( _sepGB );

                // Set up UI components
                _histHorAxisView = new HistHorAxisView (_w, "histHorAxisView",
                                        _histR, _histG, _histB, _histOrient );
		_histVerAxisView = new HistVerAxisView (_w, "histVerAxisView",
					_histR, _histG, _histB, _histOrient, _verAxisDir );

                _histGraphView   = new HistGraphView (_w, "histGraphView",
                                        _histR, _histG, _histB,
                                        _method, _histOrient, _verAxisDir );

                // Set the spike value to the current value 
                // so that it is updated properly
		_histGraphView->setSpike(current_spike);
		_histGraphView->setLogScale(logScale);

                _statView        = new StatView (_w, "statView",
                                        _histR, _histG, _histB );

                // Set up constraints
		layComponents ();

                // manage components
		showComponents ();
	}

	else if ((_method == BLEND) || (_method == STACKED))
	{
		_histGraphView->setMethodType (method);
		_histGraphView->update();
	}
}

/************************************************************************/
/*      setOrientType                                                   */
/************************************************************************/
void HistBox::setOrientType ( OrientType histOrient )
{
        _histOrient = histOrient;

    if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) )
    {
	_histBoxR->setOrientType ( histOrient );
	_histBoxG->setOrientType ( histOrient );
	_histBoxB->setOrientType ( histOrient );
    }

    else
    {
	hideComponents();
	layComponents();
	showComponents();

	_histGraphView->setOrientType (histOrient);
	_histGraphView->update();

	if (_histHorAxisView) {
		_histHorAxisView->setOrientType (histOrient);
		_histHorAxisView->update();
	}
	if (_histVerAxisView) { 
		_histVerAxisView->setOrientType (histOrient);
        	_histVerAxisView->update();
	}
    }
}

/************************************************************************/
/*      setVerAxisDirType                                               */
/************************************************************************/
void HistBox::setVerAxisDirType ( VerAxisDirType verAxisDir )
{
   _verAxisDir = verAxisDir;
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) )
   {
        _histBoxR->setVerAxisDirType(verAxisDir);
        _histBoxG->setVerAxisDirType(verAxisDir);
        _histBoxB->setVerAxisDirType(verAxisDir);
   }
	
   else
   {
        _histGraphView->setVerAxisDir (verAxisDir);
        _histGraphView->update();

        _histVerAxisView->setVerAxisDir (verAxisDir);
        _histVerAxisView->update();
   }
}

/************************************************************************/
/*      setSpike                                                        */
/************************************************************************/
void HistBox::setSpike (int spike)
{
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) ) {
        _histBoxR->setSpike(spike);
        _histBoxG->setSpike(spike);
        _histBoxB->setSpike(spike);
   }
   else {
	_histGraphView->setSpike(spike); 
	_histGraphView->update();
   }
}

/************************************************************************/
/*      setLogScale                                                     */
/************************************************************************/
void HistBox::setLogScale(Boolean log)
{
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) ) {
	_histBoxR->setLogScale(log);
	_histBoxG->setLogScale(log);
	_histBoxB->setLogScale(log);
   }
   else {
	_histGraphView->setLogScale(log);
	_histGraphView->update();
   }
}

Boolean HistBox::logScaleIsSet()
{
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) ) 
	return _histBoxR->logScaleIsSet();
   else 
	return _histGraphView->logScaleIsSet();
}

/************************************************************************/
/*      getSpike                                                        */
/************************************************************************/
int HistBox::getSpike ()
{
   if ( (_method == POPUP) && (_histR) && (_histG) && (_histB) ) {
	int r = _histBoxR->getSpike(); 
	int g = _histBoxG->getSpike();
	int b = _histBoxB->getSpike();
        if ((r != g) || (r != b))
		cerr << "Spikes for red, green, and blue must be the same" << endl;
	return r;
   }
   return _histGraphView->getSpike(); 
}

/*************************************************************************/
/*      Manage Components within a histBox                               */
/*************************************************************************/
void HistBox::showComponents()
{
        if ( _showStat ) _statView->manage();
        else _statView->unmanage();

        if ( _showAxis )
        {
                if (_histOrient == VERTICAL) _histHorAxisView->manage();
                if (_histOrient == HORIZONTAL) _histVerAxisView->manage();
        }
        else
        {
                _histHorAxisView->unmanage();
                _histVerAxisView->unmanage();
        }

        if ( _showHist ) _histGraphView->manage();
	else _histGraphView->unmanage();
}

/************************************************************************/
/*      hideComponents                                                  */
/************************************************************************/
void HistBox::hideComponents()
{
	_histGraphView->unmanage();
	_histHorAxisView->unmanage();
	_histVerAxisView->unmanage();
	_statView->unmanage();
}

/********************************************************************************/
/*	Set Form Constraints							*/
/********************************************************************************/
void HistBox::layComponents()
{
           switch ( _histOrient ) {
           case HORIZONTAL:

                XtVaSetValues   ( _histVerAxisView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_FORM,
                          XmNleftAttachment,    XmATTACH_FORM,
                          XmNrightAttachment,   XmATTACH_NONE,
                          XmNbottomWidget,      _statView->baseWidget(),
                          XmNbottomAttachment,  XmATTACH_WIDGET,
                          NULL );

                XtVaSetValues   ( _histGraphView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_FORM,
                          XmNleftAttachment,    XmATTACH_WIDGET,
                          XmNleftWidget,        _histVerAxisView->baseWidget(),
                          XmNrightAttachment,   XmATTACH_FORM,
                          XmNbottomWidget,      _statView->baseWidget(),
                          XmNbottomAttachment,  XmATTACH_WIDGET,
                          NULL );

                XtVaSetValues   ( _statView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_NONE,
                          XmNleftAttachment,    XmATTACH_FORM,
                          XmNrightAttachment,   XmATTACH_FORM,
                          XmNbottomAttachment,  XmATTACH_FORM,
                          NULL );


               if ( _showHist && _showAxis && !_showStat )
               {
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );
			XtVaSetValues   ( _histVerAxisView->baseWidget(),
				XmNbottomAttachment,    XmATTACH_FORM,
				NULL );
                }

                else if ( _showHist && !_showAxis && _showStat )
                {
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNleftAttachment,    XmATTACH_FORM,
                                NULL );
                }

                else if ( _showHist && !_showAxis && !_showStat )
                {
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNleftAttachment,    XmATTACH_FORM,
                                NULL );
                }

                else if ( !_showHist && _showAxis && _showStat )
                {
                        XtVaSetValues   ( _histVerAxisView->baseWidget(),
                                XmNrightAttachment,   XmATTACH_FORM,
                                NULL );
                }

                else if ( !_showHist && _showAxis && !_showStat )
                {
                        XtVaSetValues   ( _histVerAxisView->baseWidget(),
                                XmNrightAttachment,   XmATTACH_FORM,
                                NULL );
                        XtVaSetValues   ( _histVerAxisView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );

                }

                else if ( !_showHist && !_showAxis && _showStat )
                {
                        XtVaSetValues   ( _statView->baseWidget(),
                                XmNtopAttachment,    XmATTACH_FORM,
                                NULL );
                }

                break;

           case VERTICAL:

                XtVaSetValues   ( _histGraphView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_FORM,
                          XmNleftAttachment,    XmATTACH_FORM,
                          XmNrightAttachment,   XmATTACH_FORM,
                          XmNbottomWidget,      _histHorAxisView->baseWidget(),
                          XmNbottomAttachment,  XmATTACH_WIDGET,
                          NULL );

                XtVaSetValues   ( _histHorAxisView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_NONE,
                          XmNleftAttachment,    XmATTACH_FORM,
                          XmNrightAttachment,   XmATTACH_FORM,
                          XmNbottomAttachment,  XmATTACH_WIDGET,
                          XmNbottomWidget,      _statView->baseWidget(),
                          NULL );

                XtVaSetValues   ( _statView->baseWidget(),
                          XmNtopAttachment,     XmATTACH_NONE,
                          XmNleftAttachment,    XmATTACH_FORM,
                          XmNrightAttachment,   XmATTACH_FORM,
                          XmNbottomAttachment,  XmATTACH_FORM,
                          NULL );

                if ( _showHist && _showAxis && !_showStat )
                {
                        XtVaSetValues   ( _histHorAxisView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );
                }

                else if ( _showHist && !_showAxis && _showStat )
                {
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_WIDGET,
                                XmNbottomWidget,        _statView->baseWidget(),
                                NULL );
                }

                else if ( _showHist && !_showAxis && !_showStat )
                {
                        XtVaSetValues   ( _histGraphView->baseWidget(),
                                XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );
                }

                else if ( !_showHist && _showAxis && _showStat )
                {
                        XtVaSetValues   ( _histHorAxisView->baseWidget(),
                                XmNtopAttachment,   XmATTACH_FORM,
                                NULL );
                }

                else if ( !_showHist && _showAxis && !_showStat )
                {
                        XtVaSetValues   ( _histHorAxisView->baseWidget(),
                                XmNtopAttachment,   XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
                                NULL );
                }

                else if ( !_showHist && !_showAxis && _showStat )
                {
                        XtVaSetValues   ( _statView->baseWidget(),
                                XmNtopAttachment,    XmATTACH_FORM,
                                NULL );
                }

                break;
           }
}
/********************************************************************************/
/*	Resource Converters for Xt						*/
/*	Cribbed from Motif ResConvert.c						*/
/********************************************************************************/

static Boolean StringsAreEqual(const char *in_str, const char *test_str)
{
   register int i;
   register int j;
   i = *in_str;
   if (((in_str[0] == 'X') || (in_str[0] == 'x')) &&
       ((in_str[1] == 'M') || (in_str[1] == 'm')))
        in_str +=2;

   for (;;)
   {
      i = *in_str;
      j = *test_str;

      if (islower (i)) i = toupper (i);
      if (i != j) return (False);
      if (i == 0) return (True);

      in_str++;
      test_str++;
   }
}


Boolean HistBox::CvtStringToPopupDirectionType(Display *dpy,
                        XrmValue *, Cardinal *num_args,
                        XrmValue *from_val, XrmValue *to_val, XtPointer *)
{
   char *in_str = (char *)(from_val->addr);
   static PopupDirectionType i;

   if (*num_args != 0)
      XtWarningMsg("wrongParameters","cvtStringToPresType","XtToolkitError",
                "String to PopupDirectionType conversion needs no extra arguments",
                (String*)NULL, (Cardinal*)NULL);

   if (StringsAreEqual(in_str, "ROW"))
      i = ROW;
   else if (StringsAreEqual(in_str, "COLUMN"))
      i = COLUMN;
   else
   {
      to_val->size = 0;
      to_val->addr = NULL;
      XtDisplayStringConversionWarning(dpy, (char *)from_val->addr,
                        XvicRPopupDirectionType);
      return(False);
   }
   if (to_val->addr != NULL) {
      if (to_val->size < sizeof(PopupDirectionType)) {
         to_val->size = sizeof(PopupDirectionType);
         return False;
      }
      *(PopupDirectionType *)(to_val->addr) = i;
   }
   else {
      to_val->addr = (XPointer)&i;
   }
   to_val->size = sizeof(PopupDirectionType);
   return True;
}

Boolean HistBox::CvtStringToMethodType(Display *dpy,
                        XrmValue *, Cardinal *num_args,
                        XrmValue *from_val, XrmValue *to_val, XtPointer *)
{
   char *in_str = (char *)(from_val->addr);
   static MethodType i;

   if (*num_args != 0)
      XtWarningMsg("wrongParameters","cvtStringToMethodType","XtToolkitError",
                "String to MethodType conversion needs no extra arguments",
                (String*)NULL, (Cardinal*)NULL);

   if (StringsAreEqual(in_str, "STACKED"))
      i = STACKED;
   else if (StringsAreEqual(in_str, "BLEND"))
      i = BLEND;
   else if (StringsAreEqual(in_str, "POPUP"))
      i = POPUP;
   else
   {
      to_val->size = 0;
      to_val->addr = NULL;
      XtDisplayStringConversionWarning(dpy, (char *)from_val->addr,
                        XvicRMethodType);
      return(False);
   }
   if (to_val->addr != NULL) {
      if (to_val->size < sizeof(MethodType)) {
         to_val->size = sizeof(MethodType);
         return False;
      }
      *(MethodType *)(to_val->addr) = i;
   }
   else {
      to_val->addr = (XPointer)&i;
   }
   to_val->size = sizeof(MethodType);
   return True;
}

Boolean HistBox::CvtStringToOrientType(Display *dpy,
                        XrmValue *, Cardinal *num_args,
                        XrmValue *from_val, XrmValue *to_val, XtPointer *)
{
   char *in_str = (char *)(from_val->addr);
   static OrientType i;

   if (*num_args != 0)
      XtWarningMsg("wrongParameters","cvtStringToOrient","XtToolkitError",
                "String to OrientType conversion needs no extra arguments",
                (String*)NULL, (Cardinal*)NULL);

   if (StringsAreEqual(in_str, "HORIZONTAL"))
      i = HORIZONTAL;
   else if (StringsAreEqual(in_str, "VERTICAL"))
      i = VERTICAL;
   else
   {
      to_val->size = 0;
      to_val->addr = NULL;
      XtDisplayStringConversionWarning(dpy, (char *)from_val->addr,
                        XvicROrientType);
      return(False);
   }
   if (to_val->addr != NULL) {
      if (to_val->size < sizeof(OrientType)) {
         to_val->size = sizeof(OrientType);
         return False;
      }
      *(OrientType *)(to_val->addr) = i;
   }
   else {
      to_val->addr = (XPointer)&i;
   }
   to_val->size = sizeof(OrientType);
   return True;
}

Boolean HistBox::CvtStringToVerAxisDirType(Display *dpy,
                        XrmValue *, Cardinal *num_args,
                        XrmValue *from_val, XrmValue *to_val, XtPointer *)
{
   char *in_str = (char *)(from_val->addr);
   static VerAxisDirType i;

   if (*num_args != 0)
      XtWarningMsg("wrongParameters","cvtStringToVerAxisDir","XtToolkitError",
                "String to VerAxisDirType conversion needs no extra arguments",
                (String*)NULL, (Cardinal*)NULL);

   if (StringsAreEqual(in_str, "ASC"))
      i = ASC;
   else if (StringsAreEqual(in_str, "DESC"))
      i = DESC;
   else
   {
      to_val->size = 0;
      to_val->addr = NULL;
      XtDisplayStringConversionWarning(dpy, (char *)from_val->addr,
                        XvicRVerAxisDirType);
      return(False);
   }
   if (to_val->addr != NULL) {
      if (to_val->size < sizeof(VerAxisDirType)) {
         to_val->size = sizeof(VerAxisDirType);
         return False;
      }
      *(VerAxisDirType *)(to_val->addr) = i;
   }
   else {
      to_val->addr = (XPointer)&i;
   }
   to_val->size = sizeof(VerAxisDirType);
   return True;
}

