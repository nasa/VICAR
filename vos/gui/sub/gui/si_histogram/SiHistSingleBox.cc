////////////////////////////////////////////////////////////////////
// SiHistSingleBox.cc: Container for the histogram display
///////////////////////////////////////////////////////////////////
#include "SiHistSingleBox.h"
#include "SiHistogram.h"
#include "SiHistGraph.h"
#include "SiHistAxis.h"
#include "SiHistStat.h"
#include "ErrorManager.h"
#include <Xm/RepType.h>
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdlib.h>
#include <ctype.h>

// Class variables

int SiHistSingleBox::_histBoxInit = False;

// Resources for this class

XtResource SiHistSingleBox::_resources [ ] = {
 {
    (char *)XvicNorientation,
    (char *)XvicCOrientation,
    (char *)XvicROrientType,
    sizeof ( OrientType ),
    XtOffset ( SiHistSingleBox *, _histOrient ),
    XmRImmediate,
    ( XtPointer ) HORIZONTAL,
 },
 {
    (char *)XvicNshowAxis,
    (char *)XvicCShowAxis,
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SiHistSingleBox *, _showAxis ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNshowStat,
    (char *)XvicCShowStat,
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SiHistSingleBox *, _showStat ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNshowHist,
    (char *)XvicCShowHist,
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SiHistSingleBox *, _showHist ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)XvicNverAxisDir,
    (char *)XvicCVerAxisDir,
    (char *)XvicRVerAxisDirType,
    sizeof ( VerAxisDirType ),
    XtOffset ( SiHistSingleBox *, _verAxisDir ),
    XmRImmediate,
    ( XtPointer ) ASC,
 },
};

String SiHistSingleBox::_defaults[] = {
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


SiHistSingleBox::SiHistSingleBox ( Widget parent, const char *name,
							SiHistogram *histR ) 
	: UIComponent ( name )
{
    // One-time class initialization

    if (!_histBoxInit) {

	XmRepTypeId id;

	static String OrientNames[] = {(char *)"horizontal",(char *)"vertical"};
	static unsigned char OrientValues[] = {HORIZONTAL, VERTICAL};
	id = XmRepTypeRegister((char *)XvicROrientType,
			OrientNames, OrientValues, XtNumber(OrientNames));
	XmRepTypeAddReverse(id);

	static String AscNames[] = {(char *)"asc", (char *)"desc"};
	static unsigned char AscValues[] = {ASC, DESC};
	id = XmRepTypeRegister((char *)XvicRVerAxisDirType,
			AscNames, AscValues, XtNumber(AscNames));
	XmRepTypeAddReverse(id);

	_histBoxInit = True;
    }

    _logScale = FALSE;		//!!! Move to resources eventually

    // Save pointer to the histogram
    _histR = histR;

    // Load the histogram default resources into the database
    setDefaultResources ( parent, _defaults );

    // Create a form to hold the other widgets
    _w = XtVaCreateWidget ( _name, 
			xmFormWidgetClass, parent, 
			NULL);
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    setShellResize ( FALSE );

    _axis = new SiHistAxis ( _w, "axis", _histR );
    _graph = new SiHistGraph ( _w, "graph", _histR );
    _statView = new SiHistStat ( _w, "statView", _histR, NULL, NULL );

    setHistBox();
    layComponents();
}

void SiHistSingleBox::setHistBox()
{
    // Set Axis

    _axis->setLimits ( (float)_histR->getLowerLimit(), (float)_histR->getUpperLimit() );
    _axis->setIntRange ( _histR->isIntRange() );
    if ( _histOrient == HORIZONTAL )
	_axis->setVertical ( TRUE );
    else 
	_axis->setVertical ( FALSE );
    if ( _verAxisDir == DESC )
	_axis->setAscending ( FALSE );
    else 
	_axis->setAscending ( TRUE );

    // Set Plot

    if ( _verAxisDir == ASC )
	_graph->setAscending ( TRUE );
    else
	_graph->setAscending ( FALSE );
    if ( _histOrient == HORIZONTAL )
	_graph->setHorizontal ( TRUE );
    else 
	_graph->setHorizontal ( FALSE );
    _graph->setLogScale ( _logScale );
}

Widget SiHistSingleBox::findShellWidget ( Widget w )
{
    Widget topLevel = w;
    while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
	topLevel = XtParent(topLevel);
    return topLevel;
}

void SiHistSingleBox::setShellResize ( Boolean value )
{
    // Work around ugly flashing effects as the window shrinks and
    // re-expands.
    Widget topLevel = findShellWidget ( _w );
    XtVaGetValues ( topLevel, 
		XtNallowShellResize, &_shell_resize, 
		NULL);
    XtVaSetValues ( topLevel, 
		XtNallowShellResize, value, 
		NULL);
}

void SiHistSingleBox::restoreShellResize()
{
    Widget topLevel = findShellWidget ( _w );
    XtVaSetValues ( topLevel, 
		XtNallowShellResize, _shell_resize, 
		NULL );
}

SiHistSingleBox::~SiHistSingleBox()
{
    Widget topLevel = (Widget) _w;
    while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
                topLevel = XtParent(topLevel);
    XtVaSetValues(topLevel, XtNallowShellResize, _shell_resize, NULL);
}

void SiHistSingleBox::showAxis ( Boolean show )
{ 
    _showAxis = show; 
}

void SiHistSingleBox::showStat ( Boolean show )
{
    _showStat = show;
}

void SiHistSingleBox::setOrientType ( OrientType histOrient )
{
    _histOrient = histOrient;

    if ( _histOrient == HORIZONTAL )
        _axis->setVertical ( TRUE );
    else
        _axis->setVertical ( FALSE );

    if ( _histOrient == HORIZONTAL )
        _graph->setHorizontal ( TRUE );
    else
        _graph->setHorizontal ( FALSE );
}

void SiHistSingleBox::setVerAxisDirType ( VerAxisDirType verAxisDir )
{
    _verAxisDir = verAxisDir;

    if ( _verAxisDir == DESC )
        _axis->setAscending ( FALSE );
    else
        _axis->setAscending ( TRUE );

    if ( _verAxisDir == ASC )
        _graph->setAscending ( TRUE );
    else
        _graph->setAscending ( FALSE );
}

void SiHistSingleBox::setSpike ( int spike )
{
    _graph->setSpike ( spike );
}

void SiHistSingleBox::setLogScale ( Boolean log )
{
    _logScale = log;
    _graph->setLogScale ( _logScale );
}

Boolean SiHistSingleBox::logScaleIsSet()
{
    return _logScale;
}

int SiHistSingleBox::getSpike()
{
    return _graph->getSpike();
}

void SiHistSingleBox::showComponents()
{
    if ( _showStat ) _statView->manage();
    else _statView->unmanage();

    if ( _showAxis ) _axis->manage();
    else _axis->unmanage();

   _graph->manage();
}

void SiHistSingleBox::hideComponents()
{
    _graph->unmanage();
    _axis->unmanage();
    _statView->unmanage();
}

void SiHistSingleBox::layComponents()
{
    switch ( _histOrient ) {
      case HORIZONTAL:
	XtVaSetValues ( _statView->baseWidget(),
			XmNtopAttachment,     XmATTACH_NONE,
			XmNleftAttachment,    XmATTACH_FORM,
			XmNrightAttachment,   XmATTACH_FORM,
			XmNbottomAttachment,  XmATTACH_FORM,
			NULL );

	XtVaSetValues ( _axis->baseWidget(),
                        XmNtopAttachment,	XmATTACH_FORM,
                        XmNleftAttachment,	XmATTACH_FORM,
                        XmNrightAttachment,	XmATTACH_NONE,
                        XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,	_statView->baseWidget(),
			NULL );

	XtVaSetValues ( _graph->baseWidget(),
                        XmNtopAttachment,     XmATTACH_FORM,
                        XmNleftAttachment,    XmATTACH_WIDGET,
                        XmNleftWidget,        _axis->baseWidget(),
                        XmNrightAttachment,   XmATTACH_FORM,
			XmNbottomAttachment,  XmATTACH_WIDGET,
                        XmNbottomWidget,      _statView->baseWidget(),
                        NULL );

	if ( _showHist && _showAxis && !_showStat ) {
	    XtVaSetValues ( _graph->baseWidget(),
                            XmNbottomAttachment,    XmATTACH_FORM,
                            NULL );
	    XtVaSetValues ( _axis->baseWidget(),
			    XmNbottomAttachment,    XmATTACH_FORM,
			    NULL );
	}

        else if ( _showHist && !_showAxis && _showStat ) {
            XtVaSetValues ( _graph->baseWidget(),
                            XmNleftAttachment,    XmATTACH_FORM,
                            NULL );
        }

        else if ( _showHist && !_showAxis && !_showStat ) {
            XtVaSetValues ( _graph->baseWidget(),
                            XmNbottomAttachment,    XmATTACH_FORM,
			    XmNleftAttachment,    XmATTACH_FORM,
                            NULL );
        }

      break;

      case VERTICAL:

	XtVaSetValues ( _graph->baseWidget(),
                        XmNtopAttachment,     XmATTACH_FORM,
                        XmNleftAttachment,    XmATTACH_FORM,
                        XmNrightAttachment,   XmATTACH_FORM,
			XmNbottomAttachment,  XmATTACH_WIDGET,
                        XmNbottomWidget,      _axis->baseWidget(),
                        NULL );

        XtVaSetValues ( _axis->baseWidget(),
                        XmNtopAttachment,     XmATTACH_NONE,
                        XmNleftAttachment,    XmATTACH_FORM,
                        XmNrightAttachment,   XmATTACH_FORM,
                        XmNbottomAttachment,  XmATTACH_WIDGET,
                        XmNbottomWidget,      _statView->baseWidget(),
                        NULL );

        XtVaSetValues ( _statView->baseWidget(),
                        XmNtopAttachment,     XmATTACH_NONE,
                        XmNleftAttachment,    XmATTACH_FORM,
                        XmNrightAttachment,   XmATTACH_FORM,
                        XmNbottomAttachment,  XmATTACH_FORM,
                        NULL );

        if ( _showHist && _showAxis && !_showStat ) {
	    XtVaSetValues ( _axis->baseWidget(),
                            XmNbottomAttachment,    XmATTACH_FORM,
                            NULL );
        }

        else if ( _showHist && !_showAxis && _showStat ) {
	    XtVaSetValues ( _graph->baseWidget(),
                            XmNbottomAttachment,    XmATTACH_WIDGET,
                            XmNbottomWidget,        _statView->baseWidget(),
                            NULL );
        }

        else if ( _showHist && !_showAxis && !_showStat ) {
             XtVaSetValues ( _graph->baseWidget(),
                             XmNbottomAttachment,    XmATTACH_FORM,
                             NULL );
        }

      break;
    }
}
