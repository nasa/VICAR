$!****************************************************************************
$!
$! Build proc for MIPL module si_histogram
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:38
$!
$! Execute by entering:		$ @si_histogram
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module si_histogram ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to si_histogram.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("si_histogram.imake") .nes. ""
$   then
$      vimake si_histogram
$      purge si_histogram.bld
$   else
$      if F$SEARCH("si_histogram.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake si_histogram
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @si_histogram.bld "STD"
$   else
$      @si_histogram.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_histogram.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_histogram.com -mixed -
	-s SiHistMenuCmd.cc SiHistWindow.cc SiHistogram.cc SiHistSingleBox.cc -
	   SiHistBox.cc SiHistGraph.cc SiHistAxis.cc SiHistStat.cc -
	   SiHistBtnInterface.cc SiHistSpikeDialog.cc SiHistSpikeArrowIf.cc -
	   SiImageToHistGlue.cc SiRawHistToStrHistGlue.cc -
	   SiLutToStrHistGlue.cc SiCollectStretchedHist.cc SiCollectHist.cc -
	   SiCollectHistBG.cc -
	-i si_histogram.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiHistMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistMenuCmd.cc: Command to bring up the histogram display
//////////////////////////////////////////////////////////
#include "SiHistMenuCmd.h"
#include "SiHistWindow.h"
#include "SiHistogram.h"

SiHistMenuCmd::SiHistMenuCmd ( const char *name, const char* titleName,
		int active, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB ) 
	: NoUndoCmd ( name, active )
{
    _created = FALSE;

    _histR = histR;
    _histG = histG;
    _histB = histB;

    _title = titleName;
}

void SiHistMenuCmd::doit()
{
    // Execute the following upon button activation.
    // Create histogram Window only once and then display it.
    // Set the Close button to the UNMAP state so the Window
    // is only unmanaged when it is closed and can therefore
    // be managed again when the user hits the command button.

    if (!_created) {                 // Do only once
	_histWindow = new SiHistWindow ( _title, _histR, _histG, _histB );
	_histWindow->initialize();
	XtVaSetValues ( _histWindow->baseWidget(), 
			XmNdeleteResponse, XmUNMAP, 
			NULL);

	_created = TRUE;
    }
    _histWindow->manage();
}      
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// SiHistWindow.cc:
////////////////////////////////////////////////////////////////////
#include "SiHistWindow.h"

#include "SiHistSetBlendCmd.h"
#include "SiHistSetStackCmd.h"
#include "SiHistSetRowCmd.h"
#include "SiHistSetColCmd.h"
#include "SiHistShowAxisCmd.h"
#include "SiHistShowStatCmd.h"
#include "SiHistSetAscAxisCmd.h"
#include "SiHistSetDescAxisCmd.h"
#include "SiHistSetHorGraphCmd.h"
#include "SiHistSetVerGraphCmd.h"
#include "SiHistSetLogScaleCmd.h"
#include "SiHistSpikeCmd.h"

#include "SiHistSpikeDialog.h"
#include "SpikeDialogCmd.h"
#include "MenuCmdList.h"
#include "SeparatorCmd.h"
#include "PopupMenu.h"
#include "SiHistogram.h"

#include "SiHistBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RepType.h>

SiHistWindow::SiHistWindow ( const char *name, SiHistogram *histR, 
		SiHistogram *histG, SiHistogram *histB ) 
	: MenuWindow ( name )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;

    _histBox = NULL;
    _popup = NULL;
}

SiHistWindow::~SiHistWindow()
{
    delete _histBox;
    delete _popup;
}

Widget SiHistWindow::createWorkArea ( Widget parent )
{
    _histBox = new SiHistBox ( parent, "histBox", _histR, _histG, _histB );

    // Create the Hist_VIEW Pulldown Menu objects
    
    CmdList *radList0 = new CmdList();
    _stackNoBlend = new SiHistSetStackCmd ( "StackNoBlend", TRUE, _histBox, radList0 );
    _stackBlend = new SiHistSetBlendCmd ( "StackBlend", TRUE, _histBox, radList0 );
    _row = new SiHistSetRowCmd ( "Row", TRUE, _histBox, radList0 );
    _column = new SiHistSetColCmd ( "Column", TRUE, _histBox, radList0 );

    radList0->add( _stackNoBlend );
    radList0->add( _stackBlend );
    radList0->add( _row );
    radList0->add( _column );

    _stats = new SiHistShowStatCmd ( "Show Stats", TRUE, _histBox );
    _axis = new SiHistShowAxisCmd ( "Show Axis", TRUE, _histBox );

    //  Create the Preferences Pulldown Menu objects as Radio Buttons
    //  Need to create 2 independent radio banks radList1 and radList2

    CmdList *radList1 = new CmdList();
    _vertical = new SiHistSetVerGraphCmd ( "Vertical HistGraph", TRUE, _histBox, radList1 );
    _horizontal = new SiHistSetHorGraphCmd ( "Horizontal HistGraph", TRUE, _histBox, radList1 );

    radList1->add( _vertical );
    radList1->add( _horizontal );

    CmdList *radList2 = new CmdList();
    _ascending = new SiHistSetAscAxisCmd ( "Ascending Axis Values", TRUE, _histBox, radList2 );    
    _descending = new SiHistSetDescAxisCmd ( "Descending Axis Values", TRUE, _histBox, radList2 );    

    radList2->add( _ascending );
    radList2->add( _descending );

    _vertical->addToActivationList (_ascending);
    _vertical->addToActivationList (_descending);

    _horizontal->addToDeactivationList (_ascending);
    _horizontal->addToDeactivationList (_descending);

    CustomDialog *dialog = new SiHistSpikeDialog (_histBox, "Spike");

   _spikeDialog = new SpikeDialogCmd( "Spike", TRUE, dialog );

   _logScale = new SiHistSetLogScaleCmd ("Log-Scaled Axis", TRUE, _histBox); 

    return (_histBox->baseWidget());
}

void SiHistWindow::createMenuPanes()
{
    MenuCmdList *cmdList;

    // Add Hist_View Pulldown Menu

    cmdList = new MenuCmdList("Hist_View");
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _stackNoBlend );
    cmdList->addRadioButton ( _stackBlend );   
    cmdList->addRadioButton ( _row );
    cmdList->addRadioButton ( _column );
    cmdList->addSeparator ();   
    cmdList->addCheckBox ( _stats );
    cmdList->addCheckBox ( _axis );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _horizontal );
    cmdList->addRadioButton ( _vertical );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _ascending );
    cmdList->addRadioButton ( _descending );
    cmdList->addSeparator ();   
    cmdList->addButton( _spikeDialog );
    cmdList->addSeparator ();
    cmdList->addCheckBox (_logScale );

    _menuBar->addCommands ( cmdList, FALSE );


    //  Add in PopupMenu for Button 3

    _popup = new PopupMenu(_histBox->baseWidget(), "Hist Popup", cmdList);
    _popup->attachPopup(_histBox->baseWidget());

    delete cmdList;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistogram.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiHistogram.cc: Histogram model class
////////////////////////////////////////////////////////////////
#include "SiHistogram.h"
#include "SiHistView.h"
#include "ViewMacros.h"
#include "ErrorManager.h"
#include <stdio.h>
#include <math.h>

////////////////////////////////////////////////////////////////
// Constructors/Destructors
////////////////////////////////////////////////////////////////
SiHistogram::SiHistogram ( double ll, double ul, double bs )
{
	_isIntRange = False;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul);
}

SiHistogram::SiHistogram ( int ll, int ul, double bs )
{
	_isIntRange = True;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul + 1);
}

SiHistogram::SiHistogram()
{
	_isIntRange = True;
	_useBinSize = False;
	_numBins = 256;
	ctor(0, 256);
}

void SiHistogram::ctor ( double ll, double ul )
{
	_lowerLimit = ll;
	_upperLimit = ul;

	if ( _upperLimit < _lowerLimit ) {
		theErrorManager->process ( Error, "histogram", 
			"Upper limit can't be less than lower limit!" );
		_lowerLimit = 0; _upperLimit = 256;
	}

	_numViews = 0;

	_views = NULL;
	_bin = NULL;

	allocate ();

	clear();
}

SiHistogram::~SiHistogram()
{
	if (_views) delete [] _views;
	delete [] _bin;
}

////////////////////////////////////////////////////////////////
// Views
////////////////////////////////////////////////////////////////
void SiHistogram::attachView ( SiHistView *view )
{
	AttachViewMacro(SiHistView, _views, _numViews, view);
	view->update();
}

void SiHistogram::detachView ( SiHistView *view )
{
	DetachViewMacro(SiHistView, _views, _numViews, view);
}

void SiHistogram::updateViews()
{
	for (int i=0; i<_numViews; i++)
		_views[i]->update ( );
}

////////////////////////////////////////////////////////////////
// Misc. manipulation
////////////////////////////////////////////////////////////////

void SiHistogram::allocate()
{
	if (_useBinSize)	// Calc numBins based on bin size
		_numBins = int ( (_upperLimit-_lowerLimit) / _binSize + 0.5 );
	else			// Calc _binSize based on number of bins
		_binSize = ( _upperLimit - _lowerLimit ) / _numBins;

	if (_bin)
		delete [] _bin;

	_bin = new int[_numBins];
	if ( !_bin ) 
		theErrorManager->process ( Error, "histogram", 
					   "Memory allocation error" );

	clear_noupdate();
}

void SiHistogram::clear()
{
	clear_noupdate();
	updateViews();
}

void SiHistogram::clear_noupdate()
{
        for  ( int i = 0; i < _numBins; i++ )
                _bin[i] = 0;
}

////////////////////////////////////////////////////////////////
// Overwrite [] operator and return the the bin count where the 
// parameter that is passed to the procedure belongs.  Note that 
// reference return creates lvalue.
////////////////////////////////////////////////////////////////

int &SiHistogram::operator[] ( int binNumber )
{
	if (binNumber < 0 || binNumber >= _numBins)
            binNumber = 0;
	return _bin[binNumber];
}

////////////////////////////////////////////////////////////////
// Functions that manipulate the histogram range (min/max/#bins/bin size)
////////////////////////////////////////////////////////////////
void SiHistogram::setBinSize (double bs)
{
        _binSize = bs;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setNumBins(int num)
{
	_numBins = num;
	_useBinSize = False;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

////////////////////////////////////////////////////////////////
// Functions on statistics calculation
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// Return histogram mean value
////////////////////////////////////////////////////////////////
double SiHistogram::getMean()
{
	double rsum0 = 0.0;
	double rsum1 = 0.0;
	double mean;

	for ( int i = 0; i < _numBins; i++ ) {
		double d = (double)_bin[i];
		rsum0 += d;
		rsum1 += d * i;
	}

	if (rsum0 == 0.0)
		mean = 0.0;
	else
		mean = rsum1 / rsum0;

	return mean;
}

////////////////////////////////////////////////////////////////
// Return histogram standard deviation value
////////////////////////////////////////////////////////////////
double SiHistogram::getStDev()
{
        double rsum0, rsum2;
        rsum0 = 0;
        rsum2 = 0;
        double stdev;
        double variance;

        for (int i=0; i<_numBins; i++)
        {
                rsum0 += (double)_bin[i];
                rsum2 += (double)_bin[i] * i * i;
        }
        if (rsum0 == 0.0)
                stdev = 0.0;
        else
        {
                variance = (rsum2 / rsum0) - (getMean() * getMean());
                if (variance < 0.0)
                        variance = 0.0;
                stdev = sqrt(variance);
        }
        return stdev;
}

int SiHistogram::getMaxValue()
{
	int maxv = 0;
	for ( int i = 0; i < _numBins; i++)
		maxv = max ( maxv, _bin[i]);

	return maxv;
}

////////////////////////////////////////////////////////////////
// Spike.
////////////////////////////////////////////////////////////////
int SiHistogram::spike ( int n )
{
	int oldmax = -1;
	int i, j;

	for ( i = 0; i < n; i++ ) {	// de-spike the histogram
		int maxval = -1;
		for ( j = 0; j < _numBins; j++ ) {
			if ((_bin[j] > maxval) 
			    && (oldmax == -1 || _bin[j] < oldmax))
				maxval = _bin[j];
		}
		if (maxval >= 0)
			oldmax = maxval;
	}

	return oldmax;
}

////////////////////////////////////////////////////////////////
// Assignment
////////////////////////////////////////////////////////////////
SiHistogram &SiHistogram::operator=(SiHistogram &hist)
{
	if (this == &hist)
		return *this;		// assignment to self

	_binSize = hist._binSize;
	_lowerLimit = hist._lowerLimit;
	_upperLimit = hist._upperLimit;
	_numBins = hist._numBins;
	_useBinSize = hist._useBinSize;
	_isIntRange = hist._isIntRange;

	allocate();

	for (int i = 0; i < _numBins; i++)
		_bin[i] = hist._bin[i];

	_numViews = 0;			// Don't transfer views
	if (_views)
		delete [] _views;
	_views = NULL;

	return *this;
}

// Caller should free!!!
int *SiHistogram::getAsArray()
{
    int *a = new int [_numBins];
    for ( int i = 0; i < _numBins; i++ )
	a[i] = _bin[i];
    return a;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSingleBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistGraph.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SiHistGraph.cc:  This is a view component that draws one or three 
// graphs.
//////////////////////////////////////////////////////////////////////////
#include "SiHistGraph.h"
#include "SiHistogram.h"

SiHistGraph::SiHistGraph ( Widget parent, const char *name, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgGraphView ( parent, name ),
	  SiHistView ( histR, histG, histB )
{
    int i;
    float *r = new float [histR->numBins()];
    for ( i = 0; i < histR->numBins(); i++ )
	r[i] = (float)((*histR)[i]);

    float *g = new float [histG->numBins()];
    for ( i = 0; i < histG->numBins(); i++ )
	g[i] = (float)((*histG)[i]);

    float *b = new float [histB->numBins()];
    for ( i = 0; i < histB->numBins(); i++ )
	b[i] = (float)((*histB)[i]);

    setDataSets ( r, g, b, histR->numBins() );

    delete [] r;
    delete [] g;
    delete [] b;

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

SiHistGraph::SiHistGraph ( Widget parent, const char *name,
                SiHistogram *histR )
        : SgGraphView ( parent, name ),
          SiHistView ( histR, NULL, NULL )
{
    int i;
    float *r = new float [histR->numBins()];
    for ( i = 0; i < histR->numBins(); i++ )
	r[i] = (float)((*histR)[i]);

    setDataSet ( r, histR->numBins() );

    delete [] r;

    _histR->attachView ( this );
}

SiHistGraph::~SiHistGraph()
{
    if ( _histR ) _histR->detachView ( this );
    if ( _histG ) _histG->detachView ( this );
    if ( _histB ) _histB->detachView ( this );
}

void SiHistGraph::update()
{
    int i;

    float *r = new float [_histR->numBins()];
    for ( i = 0; i < _histR->numBins(); i++ )
        r[i] = (float)((*_histR)[i]);

    if ( _histG == NULL ) {
	setDataSet ( r, _histR->numBins() );
	delete [] r;
	display();
	return;
    }
	
    float *g = new float [_histG->numBins()];
    for ( i = 0; i < _histG->numBins(); i++ )
        g[i] = (float)((*_histG)[i]);

    float *b = new float [_histB->numBins()];
    for ( i = 0; i < _histB->numBins(); i++ )
        b[i] = (float)((*_histB)[i]);

    setDataSets ( r, g, b, _histR->numBins() );

    delete [] r;
    delete [] g;
    delete [] b;

    display();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistAxis.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SiHistAxis.cc:  This is a view component that draws axis.
//////////////////////////////////////////////////////////////////////////
#include "SiHistAxis.h"
#include "SiHistogram.h"

SiHistAxis::SiHistAxis ( Widget parent, const char *name, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgAxisView ( parent, name ),
	  SiHistView ( histR, histG, histB )
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
	setLimits ( (int)_histR->getLowerLimit(), 
		    (int)_histR->getUpperLimit() );
    else 
	setLimits ( (float)_histR->getLowerLimit(), 
		    (float)_histR->getUpperLimit() );

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

SiHistAxis::SiHistAxis ( Widget parent, const char *name,
                SiHistogram *histR )
        : SgAxisView ( parent, name ),
          SiHistView ( histR, NULL, NULL )
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
        setLimits ( (int)_histR->getLowerLimit(), 
                    (int)_histR->getUpperLimit() );
    else
        setLimits ( (float)_histR->getLowerLimit(),
                    (float)_histR->getUpperLimit() );

    _histR->attachView ( this );
}

SiHistAxis::~SiHistAxis()
{
    if ( _histR ) _histR->detachView ( this );
    if ( _histG ) _histG->detachView ( this );
    if ( _histB ) _histB->detachView ( this );
}

void SiHistAxis::update()
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
        setLimits ( (int)_histR->getLowerLimit(), 
                    (int)_histR->getUpperLimit() );
    else
        setLimits ( (float)_histR->getLowerLimit(),
                    (float)_histR->getUpperLimit() );

    display();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistStat.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// SiHistStat.cc: Displays histogram statistics in text form
////////////////////////////////////////////////////////
#include "SiHistStat.h"
#include "SiHistogram.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <stdio.h>

SiHistStat::SiHistStat ( Widget parent, const char *name, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB ) 
	: UIComponent (name), 
	  SiHistView ( histR, histG, histB )
{
	_w  = XtVaCreateWidget ( _name,
                                xmRowColumnWidgetClass, parent,
				XmNpacking,		XmPACK_COLUMN,
				XmNorientation,		XmHORIZONTAL,
				XmNnumColumns,		2,
				NULL );
	installDestroyHandler();

	/****************************************************************
	*	Red histogram
	*	Data is in "label : value" display format
	****************************************************************/
	if ( _histR && !_histG && !_histB )
	{
		_labelMean  = XtVaCreateManagedWidget ( "labelM",
						xmLabelWidgetClass, _w,  
						NULL );

		_frameMean = XtVaCreateManagedWidget ( "frameMeanR",
						xmFrameWidgetClass, _w,
						NULL );
		_mean   = XtVaCreateManagedWidget     ( "mean",
						xmLabelWidgetClass, _frameMean,  
						NULL );


		_labelStDev = XtVaCreateManagedWidget ( "labelSD",
						xmLabelWidgetClass, _w,
						NULL );

		_frameStDev = XtVaCreateManagedWidget ( "frameStDevR",
						xmFrameWidgetClass, _w,
						NULL );
		_stDev  = XtVaCreateManagedWidget    ( "stDev",
						xmLabelWidgetClass, _frameStDev,
					  	NULL );
	}

	/****************************************************************
	*       Green histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( !_histR && _histG && !_histB )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameMean = XtVaCreateManagedWidget ( "frameMeanG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass, _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget ( "stDev",
                                                xmLabelWidgetClass, _frameStDev,
                                                NULL );
	}

	/****************************************************************
	*       Blue histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( !_histR && !_histG && _histB )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

		_frameMean = XtVaCreateManagedWidget ( "frameMeanB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass, _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget    ( "stDev",
                                                xmLabelWidgetClass, _frameStDev,
                                                NULL );
	}

	/******************************************************************
	*	Color histogram
	*	Statistics for two more colors is added, 
	*	6 column requires for 3 colors (label : value)
	******************************************************************/

	else if ( _histR && _histG && _histB )
	{
		XtVaSetValues ( _w, XmNnumColumns,  2, NULL );

                _labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

                _frameMean = XtVaCreateManagedWidget ( "frameMeanR",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );



        	_frameMean1 = XtVaCreateManagedWidget ( "frameMeanG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
        	_mean1   = XtVaCreateManagedWidget     ( "mean1",
                                                xmLabelWidgetClass,
                                                _frameMean1,
                                                NULL );



                _frameMean2 = XtVaCreateManagedWidget ( "frameMeanB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _mean2   = XtVaCreateManagedWidget     ( "mean2",
                                                xmLabelWidgetClass,
                                                _frameMean2,
                                                NULL );

		// Standard Deviation

                _labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );


                _frameStDev = XtVaCreateManagedWidget ( "frameStDevR",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev  = XtVaCreateManagedWidget    ( "StDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );


                _frameStDev1 = XtVaCreateManagedWidget ( "frameStDevG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev1  = XtVaCreateManagedWidget    ( "stDev1",
                                                xmLabelWidgetClass,
                                                _frameStDev1,
                                                NULL );


                _frameStDev2 = XtVaCreateManagedWidget ( "frameStDevB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev2  = XtVaCreateManagedWidget    ( "stDev2",
                                                xmLabelWidgetClass,
                                                _frameStDev2,
                                                NULL );

	}
        if (_histR) _histR->attachView(this);
        if (_histG) _histG->attachView(this);
        if (_histB) _histB->attachView(this);
}

SiHistStat::~SiHistStat()
{
	if (_histR) _histR->detachView(this);
        if (_histG) _histG->detachView(this);
        if (_histB) _histB->detachView(this);
}

void SiHistStat::setStat ( double m, double sd )
{
	char buf[50];
	XmString xmstr;

	sprintf ( buf, "%6.3f", m );
	xmstr = XmStringCreateSimple (buf);
	XtVaSetValues (_mean, XmNlabelString, xmstr, NULL);
	XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sd );
	xmstr = XmStringCreateSimple (buf);
	XtVaSetValues (_stDev, XmNlabelString, xmstr, NULL);
	XmStringFree (xmstr);

}

void SiHistStat::setStatColor ( double mR, double mG, double mB,
			      double sdR, double sdG, double sdB )
{
        char buf[50];
        XmString xmstr;

        sprintf ( buf, "%6.3f", mR );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdR );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", mG );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean1, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdG );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev1, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", mB );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean2, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdB );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev2, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);
}


void SiHistStat::update ()
{
	// Red histogram
        if ( (_histR != NULL) && (_histG == NULL) && (_histB == NULL) )
                setStat ( _histR->getMean(), _histR->getStDev() );

        // Green histogram
        else if ( (_histR == NULL) && (_histG != NULL) && (_histB == NULL) )
                setStat ( _histG->getMean(), _histG->getStDev() );

        // Blue histogram
        else if ( (_histR == NULL) && (_histG == NULL) && (_histB != NULL) )
                setStat ( _histB->getMean(), _histB->getStDev() );


	// Color RGB histogram
	if ( _histR && _histG && _histB )
                setStatColor ( _histR->getMean(), _histG->getMean(), _histB->getMean(),
                           _histR->getStDev(), _histG->getStDev(), _histB->getStDev() );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistBtnInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistBtnInterface.cc: A "push button" interface to a Cmd object.
///////////////////////////////////////////////////////////////
#include "SiHistBtnInterface.h"
#include "SiHistogram.h"
#include "SiHistGraph.h"

SiHistBtnInterface::SiHistBtnInterface ( Widget parent, Cmd *cmd,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgDrawAreaInterface ( parent, cmd )
{
   SiHistGraph *histGraphView = new SiHistGraph ( _w, "histGraphView",
		histR, histG, histB );
   histGraphView->manage();

   setDrawingAreaWidget ( histGraphView->baseWidget() );
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// SiHistSpikeDialog.cc: Source file for creating Spike Dialog Box
////////////////////////////////////////////////////////////////////////
#include "SiHistSpikeDialog.h"
#include "SiHistBox.h"
#include "SiHistogram.h"
#include "SgIntKeyinInterface.h"
#include "SiHistSpikeArrowIf.h"
#include "SiHistSpikeCmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

SiHistSpikeDialog::SiHistSpikeDialog ( SiHistBox *box, const char *name ) 
	: CustomDialog ( name, Invisible, Invisible, Invisible, 
			 Visible, Visible )
{
   _histBox = box;
}

Widget SiHistSpikeDialog::createWorkArea ( Widget parent)
{
   SiHistogram *histogram;
   histogram = _histBox->getHistR();

   Widget rc = XtVaCreateWidget("workArea", 
				xmRowColumnWidgetClass, parent,
				XmNorientation, 	XmHORIZONTAL,
				XmNpacking,     	XmPACK_TIGHT,
				NULL);

   Cmd *spikeCmd = new SiHistSpikeCmd ("SpikeCmd", TRUE, _histBox);

   SgIntKeyinInterface  *keyIn; 
   keyIn = new SgIntKeyinInterface ( rc, spikeCmd );

   CmdInterface *incButton;
   incButton = new SiHistSpikeArrowIf ( rc, 1, histogram,  spikeCmd );

   CmdInterface *decButton;
   decButton = new SiHistSpikeArrowIf ( rc, -1, histogram, spikeCmd );

   keyIn->manage();
   incButton->manage();
   decButton->manage();

   return rc;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeArrowIf.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistSpikeArrowIf.C: An arrow button spike interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "SiHistSpikeArrowIf.h"
#include "Cmd.h"
#include "SiHistogram.h"
#include <stdint.h>

SiHistSpikeArrowIf::SiHistSpikeArrowIf ( Widget parent, 
		int step, SiHistogram *hist, Cmd *cmd ) 
	: ArrowButtonInterface ( parent, cmd )
{
    _step = step;
    _histogram = hist;

    if ( _step < 0 )
	XtVaSetValues (_w, XmNarrowDirection, XmARROW_DOWN,
			NULL);
    else
	XtVaSetValues (_w, XmNarrowDirection, XmARROW_UP,
			NULL);

    setValue ( _cmd->getValue() );
}

void SiHistSpikeArrowIf::executeCmd(XtPointer)
{
  int value = (int) (uintptr_t)_cmd->getValue();
  value += _step;
  runCmd ( (CmdValue)(uintptr_t)value );
}

void SiHistSpikeArrowIf::setValue(CmdValue value)
{
    const int SPIKE_MIN_VALUE = 1;
    const int SPIKE_MAX_VALUE = _histogram->numBins();
  
    // Deactivate Increment Spike Button if spike value is 
    // greater than or equal to the number of bins

    int x = (int)(uintptr_t) value;

    if ( (_step > 0) && ( x >= SPIKE_MAX_VALUE)){
	deactivate();
    }
    else if ( (_step < 0) && ( x <= SPIKE_MIN_VALUE)){
	deactivate();
    }
    else 
	activate();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiImageToHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "SiImageToHistGlue.h"
#include "SiHistogram.h"
#include "SiCollectHistBG.h"
#include "ImageData.h"

SiImageToHistGlue::SiImageToHistGlue (ImageData *model,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
	: BasicImageView("glue", model)
{
   _histR = histR;
   _histG = histG;
   _histB = histB;

   _collectionActive = NULL;

   _model->attachView(this);
}

SiImageToHistGlue::~SiImageToHistGlue ( )
{
    // Detach itself from the model so that the are no more updates sent

    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the image changes,
// recompute the histogram.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void SiImageToHistGlue::update()
{
   updatePart(IMAGE_DATA_UPDATE_RANGE);		// Set hist limits

   // This will clear out the hists and return if the image isn't open
   SiCollectHistBG(_model, _histR, _histG, _histB, &_collectionActive);
}

void SiImageToHistGlue::updatePart(int flags)
{
   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      if (_model->getPixelType().isIntegral()) {
         if (_histR)
            _histR->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histG)
            _histG->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histB)
            _histB->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
      }
      else {
         if (_histR)
            _histR->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histG)
            _histG->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histB)
            _histB->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
      }
   }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiRawHistToStrHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// RawHistToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "SiRawHistToStrHistGlue.h"
#include "SiHistogram.h"
#include "LutView.h"
#include "SiHistView.h"
#include "SiCollectStretchedHist.h"

SiRawHistToStrHistGlue::SiRawHistToStrHistGlue ( 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: SiHistView ( histR, histG, histB )
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

void SiRawHistToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation

   SiCollectStretchedHist ( _histR, _histG, _histB,
                _strhistR, _strhistG, _strhistB,
                _lutR, _lutG, _lutB );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiLutToStrHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// LutToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "SiLutToStrHistGlue.h"
#include "SiHistogram.h"
#include "LutView.h"
#include "SiCollectStretchedHist.h"

SiLutToStrHistGlue::SiLutToStrHistGlue ( 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: LutView ( "glue", lutR, lutG, lutB )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    if (_lut) _lut ->attachView ( this);
    if (_lut1) _lut1->attachView ( this);
    if (_lut2) _lut2->attachView ( this);
}

void SiLutToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation

   SiCollectStretchedHist ( _histR, _histG, _histB,
		_strhistR, _strhistG, _strhistB,
		_lut, _lut1, _lut2);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectStretchedHist.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiCollectStretchedHist.cc
//////////////////////////////////////////////////////////////////////

#include "SiCollectStretchedHist.h"
#include "Lut.h"
#include "SiHistogram.h"

void SiCollectStretchedHist ( 
        SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
        SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
        Lut *lutR, Lut *lutG, Lut* lutB)
{
    SiCollectStretchedHist ( histR, strhistR, lutR);
    SiCollectStretchedHist ( histG, strhistG, lutG);
    SiCollectStretchedHist ( histB, strhistB, lutB);
}


void SiCollectStretchedHist ( SiHistogram *hist, SiHistogram *strhist, Lut *lut)
{
   int i, origTempValue, strTempValue;
   int *lut_vector;
   int str_i;

   // Copy primary histogram parameters to stretched hist

   if (hist->isIntRange())
      strhist->setLimits((int)hist->getLowerLimit(),(int)hist->getUpperLimit(),
		hist->numBins());
   else
      strhist->setLimits(hist->getLowerLimit(),hist->getUpperLimit(),
		hist->numBins());

   lut_vector = lut->getAsArray();

   strhist->clear_noupdate();

   for ( i=0; i<hist->numBins(); i++) {

	//  Do the translation  to get a stretched histogram model

	//!!!! FIX THIS when Lut is updated to more than 8 bits !!!!
	// Assume Lut value of 0..255 maps to full histogram range.
	// Pick corresponding value out of LUT.
      str_i = ((lut->getUpperLimit()-lut->getLowerLimit()+1) * i) /
							hist->numBins();

      origTempValue = hist->getBin ( i );
      strTempValue = strhist->getBin ( lut_vector[str_i] );
      strhist->setBin ( lut_vector[str_i], origTempValue + strTempValue);
   }

   strhist->updateViews();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectHist.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiCollectHist.cc: These subroutines can be used to fill in histogram
// model.  The caller should pass a pointer to existing histogram
// model object.
////////////////////////////////////////////////////////////////
#include "ImageDefs.h"
#include "ImageData.h"
#include "SiHistogram.h"
#include "SiCollectHist.h"
#include "ErrorDialogManager.h"
#include <iostream>
using namespace std;

void SiCollectHist(ImageData *imageModel, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   // GET INFO THAT WILL HELP YOU CREATE BUFFERS AND LOOP THRU PIXEL DATA
   int pixelSize = imageModel->getPixelSize(); 	   // number of bytes per pixel
   int numberSamples = imageModel->getNumbSamples();
   int numberLines = imageModel->getNumbLines();
   ModeType mode = imageModel->getMode();

   int lineWidth = pixelSize * numberSamples;  	   // calc size of each buffer

   if (mode == COLORmode) { 
      histR->clear();
      siGetHistPtr(histR, imageModel, numberLines, numberSamples, lineWidth, RED);
      histG->clear();
      siGetHistPtr(histG, imageModel, numberLines, numberSamples,lineWidth,GREEN);
      histB->clear();
      siGetHistPtr(histB, imageModel, numberLines, numberSamples, lineWidth,BLUE);
   }
   else if (mode == BWmode) { 
      histR->clear();
      siGetHistPtr(histR, imageModel,numberLines,numberSamples,lineWidth,BWcolor);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void siGetHistPtr (SiHistogram *hist, ImageData *data, 
		int numberLines, int numberSamples, int lineWidth,
		ColorType color)
{
   StatusType status;
   unsigned char * buffer;

   buffer = new unsigned char[lineWidth];

   // GET *ALL* LINES OF PIXELS FROM FILES (1 buffer for each band)
   // (NOTICE THAT EACH TIME THRU THE LOOP I USE THE SAME BUFFERS
   // YOU DON'T HAVE TO DO IT THIS WAY THOUGH

   for (int line = 0; line < numberLines; line++) {
   status = data->readLine(color, line, buffer);
      if (status != imSUCCESS) {
         if (!data->errorMsgIssued())
            theErrorDialogManager->post(data->getErrorMsg());
      }

      // COLLECT HISTOGRAM
      SiCollectHistLine(hist, buffer, numberSamples, data->getPixelType());


   }
   hist->updateViews();
}

#define LOOP(type)					\
   {							\
   type *buf = (type *)buffer;				\
   for (i=0; i<size; i++)				\
      hist->incBin(hist->getBinNumber(buf[i]));		\
   }

void SiCollectHistLine(SiHistogram *hist, unsigned char *buffer, int size,
		ImagePixelType type)
{
   int i;

   switch (type.get()) {
      case imBYTE:		// Special case for efficiency
         if (hist->getLowerLimit()==0 && hist->getUpperLimit()==255 &&
			hist->numBins() == 256) {
            imByte *buf = (imByte *)buffer;
            for (i=0; i<size; i++)
               hist->incBin(*buf++);
         }
         else {
            LOOP(imByte);
         }
         break;

      case imHALF:
         LOOP(imHalf);
         break;

      case imUHALF:
         LOOP(imUHalf);
         break;

      case imFULL:
         LOOP(imFull);
         break;

      case imUFULL:
         LOOP(imUFull);
         break;

      case imREAL:
         LOOP(imReal);
         break;

      case imDOUBLE:
         LOOP(imDouble);
         break;
   }
}

/////////////////////////////////////////////////////////////////////////////
// SiCollectHist: Fill in 8 bit image histogram given an array of 256 values.
/////////////////////////////////////////////////////////////////////////////

#define HISTSIZE 256

void SiCollectHist(int arrayR[HISTSIZE],int arrayG[HISTSIZE],int arrayB[HISTSIZE],
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      siGetHistPtr(histR, arrayR, HISTSIZE);
      histG->clear();
      siGetHistPtr(histG, arrayG, HISTSIZE);
      histB->clear();
      siGetHistPtr(histB, arrayB, HISTSIZE);
   }
   else if (arrayR) {
      histR->clear();
      siGetHistPtr(histR, arrayR, HISTSIZE);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void SiCollectHist(int *arrayR, int *arrayG, int *arrayB, int size, 
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      siGetHistPtr(histR, arrayR, size);
      histG->clear();
      siGetHistPtr(histG, arrayG, size);
      histB->clear();
      siGetHistPtr(histB, arrayB, size);
   }
   else if (arrayR ) {
      histR->clear();
      siGetHistPtr(histR, arrayR, size);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void siGetHistPtr (SiHistogram *hist, int *array, int size)
{
   if (size != hist->numBins()) {
      cerr << "Invalid CollectHist call, size="<<size<<", hist is "<<
						hist->numBins()<<endl;
      return;
   }
   for (int i = 0; i < size; i++) {
      hist->setBin(i, array[i]);
   }

   hist->updateViews();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectHistBG.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiCollectHistBG.cc - collect a histogram from an ImageData model
// in the background, using a WorkProc.
////////////////////////////////////////////////////////////////
#include "SiCollectHistBG.h"
#include "ImageData.h"
#include "SiHistogram.h"
#include "ImageTile.h"
#include "Application.h"
#include "ErrorDialogManager.h"
#include "SiCollectHist.h"

#ifndef MIN
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#endif

struct SiCollectHistBGState {
   int nl, ns;
   int tileWidth, tileHeight;
   int currentLine, currentSamp;
   ImageData *imageModel;
   SiHistogram *histR, *histG, *histB;
   ImageTile *tile;
   XtWorkProcId workProcId;
   void **activePtr;
};

static Boolean SiCollectHistBGWorkProc(XtPointer clientData);

////////////////////////////////////////////////////////////////
// Set up the state structure and register the work proc.
// The "active" is a pointer to a void * in the caller (it is
// really a pointer to the state structure).  This void * should
// be initialized to NULL before any hists are collected.  Then,
// the address of this pointer should be passed in to all calls
// to CollectHistBG that use the same imageModel and Histogram
// objects.  If there's a collection currently in progress, it
// is terminated and a new one is started.  The WorkProc will set
// active to NULL when it completes.
////////////////////////////////////////////////////////////////

void SiCollectHistBG(ImageData *imageModel, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		void **active)
{
   SiCollectHistBGState *cb;

   if (*active) {		// An old collection is still active
      SiCollectHistBGState *oldcb = (SiCollectHistBGState *)(*active);
      XtRemoveWorkProc(oldcb->workProcId);
      delete oldcb;
      *active = NULL;
   }

   histR->clear();
   histG->clear();
   histB->clear();

   if (!imageModel->isDataSourceOpened())
      return;			// nothing to do if no data available

   cb = new SiCollectHistBGState;
   if (cb == NULL)
      return;
   *active = (void *)cb;
   cb->activePtr = active;

   cb->imageModel = imageModel;
   cb->histR = histR;
   cb->histG = histG;
   cb->histB = histB;

   cb->nl = imageModel->getNumbLines();
   cb->ns = imageModel->getNumbSamples();
   imageModel->getSuggestedUnzoomedTileSize(cb->tileHeight, cb->tileWidth);

   imageModel->setUnzoomedTileSize(cb->tileHeight, cb->tileWidth);

   ZoomFactor zoom(1,1,1,1);
   ImageTile &tileRef = imageModel->createTile(zoom);
   cb->tile = &tileRef;

   cb->currentLine = 0;
   cb->currentSamp = 0;

   cb->workProcId = XtAppAddWorkProc(theApplication->appContext(),
			SiCollectHistBGWorkProc, (XtPointer) cb);
}

////////////////////////////////////////////////////////////////
// Work proc to gather the histogram.  Read one tile's worth of
// data, and add it to the histogram.  If we're done, update the
// histogram views, delete the state structure, and unregister
// the callback.
////////////////////////////////////////////////////////////////

static Boolean SiCollectHistBGWorkProc(XtPointer clientData)
{
   SiCollectHistBGState *cb = (SiCollectHistBGState *)clientData;
   StatusType status;

   int endLine, endSamp;
   int readWidth, readHeight;

   if (cb->currentLine < cb->nl && cb->currentSamp < cb->ns) {

      // Read the next tile

      endLine = MIN(cb->currentLine + cb->tileHeight - 1, cb->nl - 1);
      readHeight = endLine - cb->currentLine + 1;
      endSamp = MIN(cb->currentSamp + cb->tileWidth - 1, cb->ns - 1);
      readWidth = endSamp - cb->currentSamp + 1;

      status = cb->imageModel->readTile(cb->currentSamp, cb->currentLine,
			readWidth, readHeight, *cb->tile);
      if (status != imSUCCESS) {
         if (!cb->imageModel->errorMsgIssued()) {
            theErrorDialogManager->post(cb->imageModel->getErrorMsg());
         }
      }

      // Use the tile data to fill in the histogram

      if (cb->tile->getMode() == BWmode) {
         SiCollectHistFromTile(cb->histR, 0, *cb->tile, readWidth, readHeight);
      }
      else {
         SiCollectHistFromTile(cb->histR, 0, *cb->tile, readWidth, readHeight);
         SiCollectHistFromTile(cb->histG, 1, *cb->tile, readWidth, readHeight);
         SiCollectHistFromTile(cb->histB, 2, *cb->tile, readWidth, readHeight);
      }

      // Index to next tile

      cb->currentSamp += cb->tileWidth;
      if (cb->currentSamp >= cb->ns) {
         cb->currentLine += cb->tileHeight;
         cb->currentSamp = 0;
      }
   }

   // Are we done?

   if (cb->currentLine < cb->nl && cb->currentSamp < cb->ns)
      return False;			// no, call us again

   // We're done, clean up

   cb->histR->updateViews();
   cb->histG->updateViews();
   cb->histB->updateViews();

   *cb->activePtr = NULL;	// clears "active" in caller

   delete cb;

   return True;			// don't call us again

}

////////////////////////////////////////////////////////////////
// SiCollect histogram info for one tile's worth of data.
////////////////////////////////////////////////////////////////

void SiCollectHistFromTile(SiHistogram *hist, int bufferIndex, ImageTile &tile,
		int width, int height)
{
   int i;
   unsigned char *ptr;

   ptr = tile.getBufferPtr(bufferIndex) + tile.getByteOffset();

   for (i = 0; i < height; i++) {

      SiCollectHistLine(hist, ptr, width, tile.getPixelType());

      ptr += tile.getLineWidth();
   }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create si_histogram.imake
#define SUBROUTINE si_histogram
#define MODULE_LIST SiHistMenuCmd.cc \
   SiHistBox.cc SiHistSingleBox.cc SiHistWindow.cc \
   SiHistBtnInterface.cc \
   SiHistStat.cc SiHistGraph.cc SiHistAxis.cc \
   SiHistogram.cc

#define MODULE_LIST2 \
   SiHistSpikeDialog.cc SiHistSpikeArrowIf.cc \
   SiImageToHistGlue.cc SiRawHistToStrHistGlue.cc SiLutToStrHistGlue.cc \
   SiCollectStretchedHist.cc SiCollectHist.cc SiCollectHistBG.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
