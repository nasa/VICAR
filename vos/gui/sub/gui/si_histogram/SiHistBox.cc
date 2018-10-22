////////////////////////////////////////////////////////////////////
// SiHistBox.cc: Container for the histogram display
///////////////////////////////////////////////////////////////////
#include "SiHistBox.h"
#include "SiHistogram.h"
#include "SiHistGraph.h"
#include "SiHistAxis.h"
#include "SiHistStat.h"
#include "ErrorManager.h"
#include <Xm/RepType.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdlib.h>
#include <ctype.h>

// Class variables

int SiHistBox::_histBoxInit = False;

// Resources for this class

XtResource SiHistBox::_resources [ ] = {
 {
    (char *)XvicNpopupDirection,
    (char *)XvicCPopupDirection,
    (char *)XvicRPopupDirectionType,
    sizeof ( PopupDirectionType ),
    XtOffset ( SiHistBox *, _popDirection ),
    XmRImmediate,
    ( XtPointer ) COLUMN,
 },
 {
    (char *)XvicNmethod,
    (char *)XvicCMethod,
    (char *)XvicRMethodType,
    sizeof ( MethodType ),
    XtOffset ( SiHistBox *, _method ),
    XmRImmediate,
    ( XtPointer ) BLEND,
 },
};

SiHistBox::SiHistBox(Widget parent, const char *name,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB) 
	: SiHistSingleBox ( parent, name, histR )
{
    // One-time class initialization

    if (!_histBoxInit) {

	XmRepTypeId id;

	static String PopupNames[] = {(char *)"row", (char *)"column"};
	static unsigned char PopupValues[] = {ROW, COLUMN};

	id = XmRepTypeRegister((char *)XvicRPopupDirectionType,
			PopupNames, PopupValues, XtNumber(PopupNames));
	XmRepTypeAddReverse(id);

	static String MethodNames[] =
			{(char *)"stacked", (char *)"blend", (char *)"popup"};
	static unsigned char MethodValues[] = {STACKED, BLEND, POPUP};
	id = XmRepTypeRegister((char *)XvicRMethodType,
			MethodNames, MethodValues, XtNumber(MethodNames));
	XmRepTypeAddReverse(id);

	_histBoxInit = True;
    }

    getResources ( _resources, XtNumber ( _resources ) );

    // Save pointers to the histograms
    _histG = histG;
    _histB = histB;

    _histBoxR = new SiHistSingleBox ( _w, "RedHist", _histR );
    _histBoxG = new SiHistSingleBox ( _w, "GrnHist", _histG );
    _histBoxB = new SiHistSingleBox ( _w, "BluHist", _histB );

    _sepRG = XtVaCreateWidget ( "sepRG",
                                xmSeparatorWidgetClass, _w,
                                NULL );

    _sepGB = XtVaCreateWidget ( "sepGB", 
				xmSeparatorWidgetClass, _w,
				NULL );

    delete _graph;
    _graph = new SiHistGraph ( _w, "graph", _histR, _histG, _histB );

    delete _statView;
    _statView = new SiHistStat ( _w, "statView", _histR, _histG, _histB );

    setHistBox();

    setOrientType ( _histOrient );
    setVerAxisDirType ( _verAxisDir );
    setMethodType ( _method );

    layComponents();
}

SiHistBox::~SiHistBox()
{
    // Empty
}

void SiHistBox::setHistBox()
{
    if ( _histR->getLowerLimit() != _histG->getLowerLimit() ||
	 _histR->getLowerLimit() != _histB->getLowerLimit() )
	theErrorManager->process ( Error, "HistBox", "Histograms are not the same size" );

    _histBoxR->setHistBox();
    _histBoxG->setHistBox();
    _histBoxB->setHistBox();

    SiHistSingleBox::setHistBox();
}

void SiHistBox::setPopupDirectionType ( PopupDirectionType popDirection )
{
    if ( (_popDirection == popDirection) && (_method == POPUP) )
	return;
    _popDirection = popDirection;
    setMethodType ( POPUP );
}

void SiHistBox::setMethodType ( MethodType method )
{
    hideComponents();

    _method = method;

    layComponents();

    showComponents();

    if ( _method == BLEND )
	_graph->setBlended ( TRUE );
    else if ( _method == STACKED )
	_graph->setBlended ( FALSE );

    _histBoxR->setHistBox();
    _histBoxG->setHistBox();
    _histBoxB->setHistBox();
}

void SiHistBox::showAxis ( Boolean show )
{
    hideComponents();

    _histBoxR->showAxis ( show );
    _histBoxG->showAxis ( show );
    _histBoxB->showAxis ( show );

    SiHistSingleBox::showAxis ( show );

    layComponents();
    showComponents();
}

void SiHistBox::showStat ( Boolean show )
{
    hideComponents();

    _histBoxR->showStat ( show );
    _histBoxG->showStat ( show );
    _histBoxB->showStat ( show );

    SiHistSingleBox::showStat ( show );

    layComponents();
    showComponents();
}

void SiHistBox::setOrientType ( OrientType histOrient )
{
    hideComponents();

    _histBoxR->setOrientType ( histOrient );
    _histBoxG->setOrientType ( histOrient );
    _histBoxB->setOrientType ( histOrient );

    SiHistSingleBox::setOrientType ( histOrient );

    layComponents();
    showComponents();
}

void SiHistBox::setVerAxisDirType ( VerAxisDirType verAxisDir )
{
    _histBoxR->setVerAxisDirType ( verAxisDir );
    _histBoxG->setVerAxisDirType ( verAxisDir );
    _histBoxB->setVerAxisDirType ( verAxisDir );

    SiHistSingleBox::setVerAxisDirType ( verAxisDir );
}

void SiHistBox::setSpike ( int spike )
{
    _histBoxR->setSpike ( spike );
    _histBoxG->setSpike ( spike );
    _histBoxB->setSpike ( spike );

     SiHistSingleBox::setSpike ( spike );
}

void SiHistBox::setLogScale ( Boolean log )
{
    _histBoxR->setLogScale ( log );
    _histBoxG->setLogScale ( log );
    _histBoxB->setLogScale ( log );

    SiHistSingleBox::setLogScale ( log );
}

void SiHistBox::showComponents()
{
    _histBoxR->showComponents();
    _histBoxG->showComponents();
    _histBoxB->showComponents();

    if ( _method == POPUP ) {
	_histBoxR->manage();
	_histBoxG->manage();
	_histBoxB->manage();

	XtManageChild ( _sepRG );
	XtManageChild ( _sepGB );
    }
    else {
	SiHistSingleBox::showComponents(); 
    }
}

void SiHistBox::hideComponents()
{
    _histBoxR->unmanage();
    _histBoxG->unmanage();
    _histBoxB->unmanage();

    XtUnmanageChild ( _sepRG );
    XtUnmanageChild ( _sepGB );

    SiHistSingleBox::hideComponents();
}

void SiHistBox::layComponents()
{
    if ( _method == POPUP ) {
        if ( _popDirection == ROW )
            layComponentsRow();
        else 
            layComponentsCol();

	_histBoxR->layComponents();
	_histBoxG->layComponents();
	_histBoxB->layComponents();

    }
    else {
	SiHistSingleBox::layComponents();
    }
}

void SiHistBox::layComponentsRow()
{
		XtVaSetValues ( _histBoxR->baseWidget(),
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

		XtVaSetValues ( _histBoxG->baseWidget(),
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
}

void SiHistBox::layComponentsCol()
{
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

		XtVaSetValues ( _histBoxG->baseWidget(),
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

		XtVaSetValues ( _histBoxB->baseWidget(),
                        	XmNleftAttachment,      XmATTACH_FORM,
                        	XmNtopAttachment,       XmATTACH_POSITION,
                        	XmNtopPosition,         70,
                        	XmNrightAttachment,     XmATTACH_FORM,
                        	XmNbottomAttachment,    XmATTACH_FORM,
                        	NULL );
}
