$!****************************************************************************
$!
$! Build proc for MIPL module histview
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:25
$!
$! Execute by entering:		$ @histview
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
$ write sys$output "*** module histview ***"
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
$ write sys$output "Invalid argument given to histview.com file -- ", primary
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
$   if F$SEARCH("histview.imake") .nes. ""
$   then
$      vimake histview
$      purge histview.bld
$   else
$      if F$SEARCH("histview.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histview
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histview.bld "STD"
$   else
$      @histview.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histview.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histview.com -mixed -
	-s HistAxisView.cc HistBox.cc HistGraphView.cc HistHorAxisView.cc -
	   HistVerAxisView.cc StatView.cc HistLogVerAxis.cc -
	   HistBtnInterface.cc -
	-i histview.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create HistAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// HistAxisView.C:
////////////////////////////////////////////////////////
#include "HistAxisView.h"
#include "HistView.h"
#include "Histogram.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <stdio.h>
#include <ctype.h>
using std::cerr;
using std::endl;

// Resources for this class

XtResource HistAxisView::_resources [ ] = {
 {
    (char *)"fontname",
    (char *)"Fontname",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "6x10",
 },
 {
    (char *)"drawColor", 
    (char *)"DrawOffset",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistAxisView *, _drawColor ),
    XmRImmediate,
    ( XtPointer ) "black",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 7,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String HistAxisView::_defaults[] = {
    (char *)"*height:           30",
    (char *)"*width:            45",
     NULL,
};

HistAxisView::HistAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor ) : HistView (name)
{
	_hor = hor;

        if (hist)       _hist = hist;
        else if (hist1) _hist = hist1;
        else if (hist2) _hist = hist2;
	else {
		fprintf (stderr, "Can't create axis, ");
		fprintf (stderr, "histogram model doesn't exist\n");
		_hist = NULL;
	}

	if (hist1) _hist1 = hist1;
	else _hist1 = _hist;
	if (hist2) _hist2 = hist2;
	else _hist2 = _hist;

        // Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
                                NULL );
	installDestroyHandler();

	getResources ( _resources, XtNumber ( _resources ) );

        _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
					XmNleftAttachment,	XmATTACH_FORM,
                     			XmNtopAttachment,     	XmATTACH_FORM,
                     			XmNrightAttachment,   	XmATTACH_FORM,
                     			XmNbottomAttachment,  	XmATTACH_FORM,
					XmNborderWidth, 0,
                                        NULL );

	XtAddCallback ( _ruler, XmNexposeCallback,
			&HistAxisView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w, 0, 0);
	Font font = XLoadFont ( XtDisplay(_w), _fontname );
	XSetFont ( XtDisplay(_w), _gc, font );
	_fontStruct = XQueryFont ( XtDisplay(_w), font );
	if ( _fontStruct == 0 ) 
		cerr << "No such font: " << _fontname << endl;

	XColor exact;
	Colormap cmap = DefaultColormap ( XtDisplay(_w),
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor  color;
	XAllocNamedColor ( XtDisplay(_w), cmap, _drawColor, &color, &exact);
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

	if (_hist) _hist->attachView(this);
}

HistAxisView::~HistAxisView ()
{
	if (_hist) _hist->detachView(this);

	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay(_w), _fontStruct );
}

void  HistAxisView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        HistAxisView *obj = ( HistAxisView * ) clientData;
        obj->display();
}

void HistAxisView::update()
{
        if ( XtIsRealized(_w) )
                display ();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistGraphView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// HistGraphView.cc:  This is a view component that draws histogram.  
// BW histogram gets drawn in the appropriate color -- red, green, or blue.
// Color histogram is drawn with colors blended (displayBlend method) or 
// stacked on top of each other (displayStacked method).
//////////////////////////////////////////////////////////////////////////
#include "HistGraphView.h"
#include "Histogram.h"
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <math.h>
using std::cerr;
using std::endl;

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

// Resources for this class

XtResource HistGraphView::_resources [ ] = {
 {
    (char *)XvicNspike,
    (char *)XvicCSpike,
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _spike ),
    XmRImmediate,
    ( XtPointer ) 1,
 },

 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
 {
    (char *)"magentaColor",
    (char *)"MagentaColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _magenta ),
    XmRImmediate,
    ( XtPointer ) "magenta",
 },
 {
    (char *)"yellowColor",
    (char *)"YellowColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _yellow ),
    XmRImmediate,
    ( XtPointer ) "yellow",
 },
 {
    (char *)"cyanColor",
    (char *)"CyanColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _cyan ),
    XmRImmediate,
    ( XtPointer ) "cyan",
 },
 {
    (char *)"whiteColor",
    (char *)"WhiteColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _white ),
    XmRImmediate,
    ( XtPointer ) "white",
 },

 {
    (char *)"drawInsideTicks",
    (char *)"DrawInsideTicks",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( HistGraphView *, _drawInsideTicks ),
    XmRImmediate,
    ( XtPointer ) False,
 },
 {
    (char *)"insideTickColor",
    (char *)"InsideTickColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _insideTickColor ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"insideTickLength", 
    (char *)"InsideTickLength",
    XmRInt, 
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideTickLength ),
    XmRImmediate,
    ( XtPointer ) 10, 
 },
 {
    (char *)"insideMinorTickLength",
    (char *)"InsideMinorTickLength",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideMinorTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"insideTickInterval",
    (char *)"InsideTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideTickInterval ),
    XmRImmediate,
    ( XtPointer ) 64,
 },
 {
    (char *)"insideMinorTickInterval",
    (char *)"InsideMinorTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideMinorTickInterval ),
    XmRImmediate,
    ( XtPointer ) 16,
 },
};

HistGraphView::HistGraphView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		MethodType method, OrientType hor, VerAxisDirType verAxisDir ) 
	: HistView (name)
{
	_method = method;
	_hor = hor;
	_verAxisDir = verAxisDir;
	_log = False;

	_hist = hist;
	_hist1 = hist1;
	_hist2 = hist2;

	_w  = XtVaCreateWidget ( _name,
                                xmDrawingAreaWidgetClass, parent,
				NULL );
	installDestroyHandler ();

	getResources ( _resources, XtNumber ( _resources ) );

	XtAddCallback ( _w, XmNexposeCallback,
			&HistGraphView::displayCallback,
			( XtPointer ) this );

        _gc = XtGetGC ( _w, 0, 0 );


	if (_hist)	_hist->attachView(this);
	if (_hist1)	_hist1->attachView(this);
	if (_hist2)	_hist2->attachView(this);
}

HistGraphView::~HistGraphView()
{
	if (_hist)      _hist->detachView(this);
        if (_hist1)     _hist1->detachView(this);
        if (_hist2)     _hist2->detachView(this);

	if ( _w && _gc ) 
		XtReleaseGC ( _w, _gc );
}

void HistGraphView::displayCallback ( Widget,
				 XtPointer client_data,
				 XtPointer )
{
	HistGraphView *obj;
	obj = ( HistGraphView * ) client_data;
	if (obj != NULL)
		obj->update();
}

/************************************************************************
* update: Draw histogram.
************************************************************************/
void HistGraphView::update ( )
{
	if ( !XtIsRealized(_w) )
		return;

	// Make any resize cause expose event
	XSetWindowAttributes attrs;
	attrs.bit_gravity = ForgetGravity;
	XChangeWindowAttributes ( XtDisplay(_w),
		XtWindow(_w), CWBitGravity, &attrs );

	// Save geometry
	XtVaGetValues ( _w,
			XmNwidth,  &_width,
			XmNheight, &_height,
			NULL );

	// At least one histogram model must exist
	if (!_hist && !_hist1 && !_hist2) 
		return;

	// Always clear the window before drawing
	XClearWindow ( XtDisplay(_w), XtWindow(_w) );

	Boolean RedOnly   = (_hist1 == NULL) && (_hist2 == NULL);
	Boolean GreenOnly = (_hist == NULL) && (_hist2 == NULL);
	Boolean BlueOnly  = (_hist == NULL) && (_hist1 == NULL);

	if ( RedOnly || GreenOnly || BlueOnly )
		displayBW ( RedOnly, GreenOnly, BlueOnly);

	if ( _hist && _hist1 && _hist2 )
	    switch (_method) {
		case STACKED:	displayStacked();
		break;

		case BLEND:	displayBlend();
		break;

		default: 	cerr << "Undefined method" << endl;
		break;
	    }

        XSetForeground ( XtDisplay(_w), _gc, 
			 WhitePixel( XtDisplay(_w),
                                    DefaultScreen(XtDisplay(_w)) ) );
}

/************************************************************************
* displayBW: Draw Black & White Histogram
*
************************************************************************/
void HistGraphView::displayBW ( Boolean R, Boolean G, Boolean B )
{
	Histogram *h;	// Pointer to the histogram model to be displayed
	if ( R )          h = _hist; 
	else if ( G )     h = _hist1;
	else if ( B )     h = _hist2;
	else return;

        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor  color;
	if ( R ) {
        	XAllocNamedColor ( XtDisplay(_w), cmap, _red, &color, &exact);
	}
	else if ( G ) {
        	XAllocNamedColor ( XtDisplay(_w), cmap, _green, &color, &exact);
	}
	else if ( B ) {
		XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &color, &exact);
	}
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

        int nbins = h->numBins();
	double max_value = h->spike(_spike);
	if ((int)max_value == 0) 
		max_value = 1.0;

	if (_log == True) {
		if (_drawInsideTicks == True) {
			max_value = ceil(log10(max_value));
		}
		else {
			max_value = log10(max_value);
		}
		if ((int)max_value == 0)
			max_value = 1.0;
	}

	double ppb;				// pixels-per-bin
	if (_hor == VERTICAL) {
		if (nbins > 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}
        else {
		if (nbins > 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}


        if ( !max_value ) max_value = 1.0;

        for (int i=0; i<nbins; i++)
        {
                double value = (double)h->getBin(i);
		if (_log == True)
			if (value > 0) 
				value = log10(value);

		double ratio;
		if (max_value > 0)
                	ratio = value / max_value;
		else 
			ratio = 0;

		int x, y, width, height; 
                if (_hor == HORIZONTAL) {
		   x = 0;
		   if (_verAxisDir == DESC) {		// Descending axis
			y = int (i*ppb);
			width = int (_width*ratio) + 1;
			height = int (ppb) + 1;
		   }
		   else	{				// Ascending axis
			y = int (_height - ((i+1)*ppb));
			width = int (_width*ratio) + 1;
			height = int (ppb) + 1;
		   }
		}
		else {
			x = int(i*ppb);
			y = int (_height * (1 - ratio));
			width = int (ppb) + 1;
			height = int (_height*ratio) + 1;
		}
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                x, y, width, height);
	}

	if (_drawInsideTicks == True) {
                drawInsideTicks(max_value, ppb, nbins);
        }
}

/************************************************************************
* Display histogram using "Stacked Colors" method - for color images only.
* At each bin, the one with the max count goes into the back, and the one 
* with the min count goes in the front of the display.
************************************************************************/
void HistGraphView::displayStacked ( )
{
        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor colorR, colorG, colorB;
	XAllocNamedColor ( XtDisplay(_w), cmap, _red, &colorR, &exact);
	XAllocNamedColor ( XtDisplay(_w), cmap, _green, &colorG, &exact);
	XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &colorB, &exact);

        int nbins  = _hist->numBins();
        int nbins1 = _hist1->numBins();
        int nbins2 = _hist2->numBins();

        if ( nbins != nbins1 && nbins1 != nbins2 ) {
                cerr << "Number of bins is not the same!" << endl;
		return;
	}

        double ppb;				// pixels-per-bin
	if (_hor == HORIZONTAL) {
		if (nbins != 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}
        else {
		if (nbins != 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}

        double max_value = (double)MAX(MAX(_hist->spike(_spike), 
					   _hist1->spike(_spike)),
				       _hist2->spike(_spike));

	if ( max_value > 0 ) {
		if ( _log == True ) {
			if ( _drawInsideTicks == True ) {
				max_value = ceil(log10(max_value));
			}
			else {
				max_value = log10(max_value);
			}
		}
	}
        else 
		max_value = 1.0;

        for (int i = 0; i < nbins; i++)
        {
		// Get the value to draw
                double value  = (double)_hist->getBin(i);
                double value1 = (double)_hist1->getBin(i);
                double value2 = (double)_hist2->getBin(i);

		if (_log == True) {
                	if (value  > 0) value  = log10(value);
			if (value1 > 0) value1 = log10(value1);
			if (value2 > 0) value2 = log10(value2);
		}

                double ratio;
                double ratio1;
                double ratio2;

		if (max_value > 0)
		{
			ratio = value  / max_value;
			ratio1 = value1 / max_value;
			ratio2 = value2 / max_value;
		}
		else 
			ratio = ratio1 = ratio2 = 0.0;

		// identify the highest, lowest, and middle points in this bin
                const double mn = MIN ( MIN ( value, value1 ), value2 );
                const double mx = MAX ( MAX ( value, value1 ), value2 );
                double md;	// The remaining band is the middle value
                if (value != mn && value != mx) { md = value; }
                else if (value1 != mn && value1 != mx) { md = value1; }
                else if (value2 != mn &&  value2 != mx) { md = value2; }
		else if (value == value1 || value == value2) { md = value; }
		else { md = value1; }

                double curRatioMn = 0.0;
                double curRatioMd = 0.0;
                double curRatioMx = 0.0;

		// Draw the lower (minimum) portion of the histogram
                if ( mn == value )
                {
                        curRatioMn = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mn == value1 )
                {
                        curRatioMn = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }

                else if ( mn == value2 )
                {
                        curRatioMn = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: mn must be defined!" << endl;

		if (_hor == HORIZONTAL)
		  if (_verAxisDir == DESC)
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
				0,
				int (i*ppb),
				int (ceil(_width*curRatioMn)),
				int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*curRatioMn)),
                                int (ceil(ppb)) );
		else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(ppb)),  
				int (ceil(_height*curRatioMn)) );


                // Draw the middle portion of the histogram
                if ( md == value )
                {
                        curRatioMd = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( md == value1 )
                {
                        curRatioMd = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( md == value2 )
                {
                        curRatioMd = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: md must be defined!" << endl;

		if (_hor == HORIZONTAL)
		  if (_verAxisDir == DESC)
                        XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (_height - int((i+1)*ppb)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		else
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMd)),
                                int (ceil(ppb)),
				int (ceil(_height*curRatioMd - _height*curRatioMn)) );


                // Draw the top (max) portion of the histogram
                if ( mx == value )
                {
                        curRatioMx = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mx == value1 )
                {
                        curRatioMx = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( mx == value2 )
                {
                        curRatioMx = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: there must be a max point!" << endl;

		if (_hor == HORIZONTAL) 
		  if ( _verAxisDir == DESC)
                        XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMx - curRatioMd))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width * curRatioMd),
                                int (_height - int((i+1)*ppb)),
                                int (ceil(_width * (curRatioMx - curRatioMd))),
                                int (ceil(ppb)) );
                else 
	                XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMx)),
                                int (ceil(ppb)), 
				int (ceil(_height*curRatioMx-_height*curRatioMd)) );
        }

	if (_drawInsideTicks == True) {
                drawInsideTicks(max_value, ppb, nbins);
        }
}

/*******************************************************************************
* Blended Colors Method
*   Color arithmetics:
*	R + G + B = White
*	R + G = Yellow
*	R + B = Magenta
*	B + G = Cyan
*******************************************************************************/
void HistGraphView::displayBlend ( )
{
	// Allocate all necessary colors

        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					DefaultScreen ( XtDisplay(_w) ) );
	XColor colorR, colorG, colorB, colorM,  colorC, colorY, colorW;
        XAllocNamedColor ( XtDisplay(_w), cmap, _red, &colorR, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _green, &colorG, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &colorB, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _magenta, &colorM, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _cyan, &colorC, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _yellow, &colorY, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _white, &colorW, &exact );

	// Check bands for consistency

        int nbins  = _hist->numBins();
        int nbins1 = _hist1->numBins();
        int nbins2 = _hist2->numBins();

        if ( nbins != nbins1 && nbins1 != nbins2 ) {
                cerr << "Number of bins is not the same!" << endl;
		return;
	}

	// Calculate how many pixels each bin occupies

	double ppb;
	if (_hor == VERTICAL) {
		if (nbins != 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}
        else {
		if (nbins != 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}

	// Calculate maximum number of pixels

        double max_value = (double)MAX(MAX(_hist->spike(_spike), 
					   _hist1->spike(_spike)),
				       _hist2->spike(_spike));
        if ( max_value > 0 ) {
		if ( _log == True ) {
			if ( _drawInsideTicks == True ) {
				max_value = ceil(log10(max_value));
			}
			else {
				max_value = log10(max_value);
			}
		}
	}
        else
                max_value = 1.0;



	int i;
        for (i = 0; i < nbins; i++)
        {
                double value  = (double)_hist->getBin(i);
                double value1 = (double)_hist1->getBin(i);
                double value2 = (double)_hist2->getBin(i);

		if (_log == True) {
                        if (value  > 0) value =  log10(value);
                        if (value1 > 0) value1 = log10(value1);
                        if (value2 > 0) value2 = log10(value2);
                }

                double ratio;
                double ratio1;
                double ratio2;

		if (max_value != 0)
		{
			ratio  = value  / max_value;
			ratio1 = value1 / max_value;
			ratio2 = value2 / max_value;
		}
		else 
			ratio = ratio1 = ratio2 = 0.0;

                const double mn = MIN ( MIN ( value, value1 ), value2 );
                const double mx = MAX ( MAX ( value, value1 ), value2 );
                double md;
                if (value != mn && value != mx) { md=value; }
                else if (value1 != mn && value1 != mx) { md=value1; }
                else if (value2 != mn &&  value2 != mx) { md=value2; }
                else if (value == value1 || value == value2) { md = value; }
                else { md = value1; }

                double curRatioMn = 0.0;
                double curRatioMd = 0.0;
                double curRatioMx = 0.0;

		////////////////////////////////////////////////////////////
                if ( mn == value )
                {
                        curRatioMn = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }
                else if ( mn == value1 )
                {
                        curRatioMn = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }

                else if ( mn == value2 )
                {
                        curRatioMn = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }
                else cerr << "Something is very wrong !" << endl;

		if (_hor == HORIZONTAL)
		   if ( _verAxisDir == DESC)
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (i*ppb),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(ppb)) );
		else
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(ppb)),
				int (ceil(_height*curRatioMn)) );

		////////////////////////////////////////////////////////////
                if ( md == value )
                {
                        curRatioMd = ratio;
                        if (curRatioMn == ratio1) 
				XSetForeground ( XtDisplay(_w), _gc, colorM.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorY.pixel );
                }
                else if ( md == value1 )
                {
                        curRatioMd = ratio1;
                        if (curRatioMn == ratio) 
				XSetForeground ( XtDisplay(_w), _gc, colorC.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorY.pixel );
                }
                else if ( md == value2 )
                {
                        curRatioMd = ratio2;
                        if (curRatioMn == ratio) 
				XSetForeground ( XtDisplay(_w), _gc, colorC.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorM.pixel );
                }
                else cerr << "Something is very wrong 2!" << endl;

		if (_hor == HORIZONTAL)
		   if ( _verAxisDir == DESC )
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		   else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width*curRatioMn),
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
                else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                               	int (i*ppb),
				int (_height * (1 - curRatioMd)),
                               	int (ceil(ppb)),
				int (ceil(_height*(curRatioMd - curRatioMn))) );

		////////////////////////////////////////////////////////////
                if ( mx == value )
                {
                        curRatioMx = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mx == value1 )
                {
                        curRatioMx = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( mx == value2 )
                {
                        curRatioMx = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Something is very wrong 3!" << endl;

                if (_hor == HORIZONTAL) 
		    if ( _verAxisDir == DESC )
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int(i*ppb),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(ppb)) );
		    else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                _height - (int((i+1)*ppb)),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(ppb)) );
		else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(i*ppb), 
				int(_height * (1 - curRatioMx)),
                                int(ceil(ppb)), 
				int(ceil(_height*(curRatioMx-curRatioMd))) );
        }

	if (_drawInsideTicks == True) {
		drawInsideTicks(max_value, ppb, nbins);
	}
}


/////////////////////////////////////////////////////////////////////////
// These macros define code that repeats over and over
/////////////////////////////////////////////////////////////////////////

#define DRAW_HOR_LOG_TICK( a ) 					\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc, 		\
		int(_width*r), 0, int(_width*r), a); 		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		int(_width*r), _height-1-a, 			\
		int(_width*r), _height-1);

#define DRAW_VER_LOG_TICK( a ) 					\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		0, (int)(_height-1-(_height*r)),		\
		a, (int)(_height-1-(_height*r)) );		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		_width-1, (int)(_height-1-(_height*r)),		\
		_width-1-a, (int)(_height-1-(_height*r)) );

#define DRAW_BIN_TICK( i, interval, length )				\
        for (i = 0; i < numBins; i+=interval) {				\
            if (_hor == HORIZONTAL) {					\
                if ( _verAxisDir == DESC ) {				\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, (int)(i*ppb),			\
                                length, (int)(i*ppb) );			\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, (int)(i*ppb),			\
                                _width-1-length, (int)(i*ppb) );	\
                }							\
                else {							\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, (int)(_height-i*ppb),		\
                                length, (int)(_height-i*ppb) );		\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, (int)(_height-i*ppb),		\
                                _width-1-length, (int)(_height-i*ppb));	\
                }							\
            }								\
            else {							\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                (int)(i*ppb), _height-1,		\
                                (int)(i*ppb), _height-1-length );	\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                (int)(i*ppb), 0,			\
                                (int)(i*ppb), length );			\
            }								\
        }


/////////////////////////////////////////////////////////////////////////
// drawInsideTicks: Draw tick marks on the edge of the histogram 
// display.  Note that ticks specifying the DN count are displayed only 
// if logarithmic scale was used to draw a histogram.  Otherwise only 
// ticks along the bin axis are displayed.  Also note that ticks are 
// drawn on both left and right (top and bottom) edges.
/////////////////////////////////////////////////////////////////////////
void HistGraphView::drawInsideTicks(double maxValue, double ppb, int numBins)
{
	XColor exact, color;
	Colormap cmap = DefaultColormap ( XtDisplay(_w),
                                          DefaultScreen ( XtDisplay(_w) ) );
	XAllocNamedColor ( XtDisplay(_w), cmap, _insideTickColor, &color, &exact);
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, _width-1, 0 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, 0, _height-1 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, _height-1, _width-1, _height-1 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				_width-1, 0, _width-1, _height-1 );


	if (_log) {
	    	for (int i = 0; i < maxValue; i++) {
			double r; 

			// Draw long ticks at 10^i
			r = log10(1 * pow(10.0, (double)i)) / maxValue;
			if (_hor == HORIZONTAL) {
				DRAW_HOR_LOG_TICK(_insideTickLength);
			}
			else {
				DRAW_VER_LOG_TICK(_insideTickLength);
			}

			// Draw minor ticks at 2*10^i, 4*10^i, 6*10^i, 8*10^i
			for (int j = 2; j < 10; j+=2) {
				r = log10(j * pow(10.0, (double)i)) / maxValue;
				if (_hor == HORIZONTAL) {
					DRAW_HOR_LOG_TICK(_insideMinorTickLength);
				}
				else {
					DRAW_VER_LOG_TICK(_insideMinorTickLength);
				}
			}
		}
	}

	int i;
	DRAW_BIN_TICK(i, _insideTickInterval, _insideTickLength);
	DRAW_BIN_TICK(i, _insideMinorTickInterval, _insideMinorTickLength);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistHorAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// HistHorAxisView.C:
////////////////////////////////////////////////////////
#include "HistHorAxisView.h"
#include "Histogram.h"
#include <stdio.h>
#include <iostream>
using namespace std;

HistHorAxisView::HistHorAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor ) 
		: HistAxisView ( parent,name,hist, hist1, hist2, hor )
{
    // Empty
}

void HistHorAxisView::display ( )
{
 if ( XtIsRealized(_w) )
 {
	// make any resize cause expose event
        XSetWindowAttributes attrs;
        attrs.bit_gravity = ForgetGravity;
        XChangeWindowAttributes ( XtDisplay(_ruler),
               			XtWindow(_ruler), CWBitGravity, &attrs );

	// Remember geometry
        XtVaGetValues ( _ruler,
                        XmNwidth,  &_width,
                        XmNheight, &_height,
                        NULL );
 
   if (this->_hor == VERTICAL) {

        double min = _hist->getLowerLimit();		// min label
        double max = _hist->getUpperLimitBound();	// max label

        double lbl[16];		
        char buf[100][16];

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	// Draw a long line from side to side
	XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					0, 	_drawOffset, 
					_width, _drawOffset );

	// Calculate how many ticks we need at this screen width
	int numTicks = 16;
	if ( _width < _twoTicks ) numTicks = 2;
	else if ( _width < _fourTicks ) numTicks = 4;
	     else if ( _width < _eightTicks ) numTicks = 8;

	// These calculations are necessary to achieve acceptable precision 
	// in putting ticks along the ruler
	int step = int(_width) / numTicks;
	int temp = int(_width) % numTicks;
	double temp1 = (double)temp / (double)numTicks;
	double temp2 = temp1;

	// Draw ticks

	Dimension strOffset = 1;
	Dimension strWidth, strHeight;

	for ( int i=0; i<=numTicks; ++i)
	{
	   if ( ((i%2) == 0) && (i!=numTicks))  // every other tick is longer
	   	XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					int((i*step)+temp1), 
					_drawOffset,
					int((i*step)+temp1), 
					_drawOffset + _longTickLength);
	   else if (i ==numTicks)		// last label
		XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        _width - 1,
                                        _drawOffset,
                                        _width - 1,
                                        _drawOffset + _longTickLength);
	   else 
                XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        int((i*step)+temp1), 
					_drawOffset,
                                        int((i*step)+temp1), 
					_drawOffset + _shortTickLength);

	   // Draw a label to the tick, 
	   // all labels except for the first and last are centered around its tick
           lbl[i] = min + ((max - min) / numTicks ) * i;

           if (_hist->isIntRange()) {
	      // subtract 1 from the max label so range looks right
	      if (i == numTicks) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
	      else sprintf ( buf[i], "%d", (int)lbl[i]);
           }
           else
              sprintf(buf[i], "%.3g", lbl[i]);

	   strWidth = XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) );
	   strHeight = Dimension ( _fontStruct->ascent );

	   // draw label to every other tick but not the first or the last one
	   if ( ((i%2) == 0) && (i!=0) && (i!=numTicks) ) 
	   	XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				int((i*step) + temp1 - int(strWidth)/2), 
				_drawOffset + _longTickLength + strOffset + strHeight,
				buf[i], strlen(buf[i]) );
	   if ( i==0 )	// first label 
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                0,
                                _drawOffset + _longTickLength + strOffset + strHeight,
                                buf[i], strlen(buf[i]) );

	   if ( i== numTicks ) // last label
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				int((i*step)+temp1) - strWidth,
				_drawOffset + _longTickLength + strOffset + strHeight,
				buf[i], strlen(buf[i]) );	
	   temp1 = temp1 + temp2;
	}
   }
   else
	XtVaSetValues ( _ruler,
                        XmNheight,              0,
                        NULL );

 }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistVerAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// HistVerAxisView.cc:
////////////////////////////////////////////////////////
#include "HistVerAxisView.h"
#include "Histogram.h"
#include <stdio.h>
#include <iostream>
using namespace std;

HistVerAxisView::HistVerAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor, VerAxisDirType verAxisDir) 
		: HistAxisView (parent,name,hist,hist1,hist2, hor)
{
	_verAxisDir = verAxisDir;
}

void HistVerAxisView::display ( )
{
    if ( !XtIsRealized(_w) )
	return;

    // Make any resize cause expose event
    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay(_ruler),
                 		  XtWindow(_ruler), CWBitGravity, &attrs );

    // Save geometry
    XtVaGetValues ( _ruler,
                  XmNwidth,  &_width,
                  XmNheight, &_height,
                  NULL );
 
    if (_hor == HORIZONTAL) { 

        double min = _hist->getLowerLimit();		// min label
        double max = _hist->getUpperLimitBound();	// max label

        double lbl[16];
        char buf[100][16];

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	// Draw a long line from top to bottom
	XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
			_width - _drawOffset, 0, 
			_width - _drawOffset, _height );

	// Calculate how many ticks we need at this screen height
	int numTicks = 16;
	if ( _height < _twoTicks ) numTicks = 2;
	else if ( _height < _fourTicks ) numTicks = 4;
	     else if ( _height < _eightTicks ) numTicks = 8;

	// These calculations are necessary to achieve acceptable precision in putting
	// ticks along the ruler
	int step = int(_height) / numTicks;
	int temp = int(_height) % numTicks;
	double temp1 = (double)temp / (double)numTicks;
	double temp2 = temp1;

	// Draw ticks

	Dimension strOffset = 1;
	Dimension strWidth, strHeight;

	for ( int i=0; i<=numTicks; i++ )
	{
	   if ( (i % 2) == 0 && (i != numTicks))  // every other tick is longer
	   	XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					_width - _drawOffset - _longTickLength,
					int((i*step)+temp1),
					_width - _drawOffset,
					int((i*step)+temp1) );
	   else if (i == numTicks) 		// last tick
		XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
					_width - _drawOffset - _longTickLength,
					int((i*step)+temp1) - 1,
					_width - _drawOffset,
					int((i*step)+temp1) - 1 );
	   else 
                XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        _width - _drawOffset - _shortTickLength,
					int((i*step)+temp1),
                                        _width - _drawOffset, 
					int((i*step)+temp1) );

	   // Draw a label to the tick, the top label is positioned lower
	   // to be visible, the bottom label is above its tick
	   if (_verAxisDir == DESC)
	   {
             lbl[i] = min + ((max - min) / numTicks ) * i ;

             if (_hist->isIntRange()) {
	        // subtract 1 from the max label so range looks right
	        if ( i == numTicks ) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
                else sprintf ( buf[i], "%d", (int)lbl[i]);
             }
             else
                sprintf(buf[i], "%.3g", lbl[i]);

	     strWidth  = Dimension ( XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) ) );
	     strHeight = Dimension ( _fontStruct->ascent );

             // draw label to every other tick but not the first or the last one
             if ( ((i%2) == 0) && (i != numTicks) && (i != 0) )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1)+strHeight/2,
                             buf[i], strlen(buf[i]));
             if ( i == 0 )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1)+strHeight,
                             buf[i], strlen(buf[i]));
             if ( i == numTicks )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1),
                             buf[i], strlen(buf[i]));
	   }

	   else
	   {
             lbl[i] = max - (min + ((max - min) / numTicks ) * i);

             if (_hist->isIntRange()) {
	        // subtract 1 from the max label so range looks right
	        if ( i == 0 ) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
                else sprintf ( buf[i], "%d", (int)lbl[i]);
             }
             else
                sprintf(buf[i], "%.3g", lbl[i]);

	     strWidth  = Dimension ( XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) ) );
	     strHeight = Dimension ( _fontStruct->ascent );

	     // draw label to every other tick but not the first or the last one
	     if ( ((i%2) == 0) && (i != numTicks) && (i != 0) )
	   	XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				_width - strWidth - strOffset - _longTickLength - _drawOffset,
				int((i*step)+temp1)+strHeight/2, 
				buf[i], strlen(buf[i]));
	     if ( i == 0 )
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                _width - strWidth - strOffset - _longTickLength - _drawOffset,
                                int((i*step)+temp1)+strHeight,
                                buf[i], strlen(buf[i]));
	     if ( i == numTicks )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                _width - strWidth - strOffset - _longTickLength - _drawOffset,
                                int((i*step)+temp1),
                                buf[i], strlen(buf[i]));
	   }

	   temp1 = temp1 + temp2;
	}
    }

    else {
	XtVaSetValues ( _ruler, XmNwidth, 0, NULL );
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StatView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StatView.C: Displays histogram statistics in text form
////////////////////////////////////////////////////////
#include "StatView.h"
#include "Histogram.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <stdio.h>

StatView::StatView ( Widget parent, 
		     const char * name, 
		     Histogram *hist, 
		     Histogram *hist1, 
		     Histogram *hist2 ) : HistView (name)
{
	_hist = hist;
	_hist1 = hist1;
	_hist2 = hist2;

	_w  = XtVaCreateWidget ( _name,
                                xmRowColumnWidgetClass,
                                parent,
				XmNpacking,	XmPACK_COLUMN,
				XmNorientation,	XmHORIZONTAL,
				XmNnumColumns,	2,
				NULL );
	installDestroyHandler ();


	/****************************************************************
	*	Red histogram
	*	Data is in "label : value" display format
	****************************************************************/
	if ( (_hist != NULL) && (_hist1 == NULL) && (_hist2 == NULL) )
	{
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


		_labelStDev = XtVaCreateManagedWidget ( "labelSD",
						xmLabelWidgetClass, _w,
						NULL );

		_frameStDev = XtVaCreateManagedWidget ( "frameStDevR",
						xmFrameWidgetClass, _w,
						NULL );
		_stDev  = XtVaCreateManagedWidget    ( "stDev",
						xmLabelWidgetClass,
					  	_frameStDev,
					  	NULL );
	}

	/****************************************************************
	*       Green histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( (_hist == NULL) && (_hist1 != NULL) && (_hist2 == NULL) )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameMean = XtVaCreateManagedWidget ( "frameMeanG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget    ( "stDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );
	}

	/****************************************************************
	*       Blue histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( (_hist == NULL) && (_hist1 == NULL) && (_hist2 != NULL) )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

		_frameMean = XtVaCreateManagedWidget ( "frameMeanB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget    ( "stDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );
	}

	/******************************************************************
	*	Color histogram
	*	Statistics for two more colors is added, 
	*	6 column requires for 3 colors (label : value)
	******************************************************************/

	else if ( (_hist != NULL) && (_hist1 != NULL) && (_hist2 != NULL) )
	{
		XtVaSetValues ( _w,
				XmNnumColumns,  2,
				NULL );

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
        if (_hist)      _hist->attachView(this);
        if (_hist1)     _hist1->attachView(this);
        if (_hist2)     _hist2->attachView(this);
}

StatView::~StatView()
{
	if (_hist)      _hist->detachView(this);
        if (_hist1)     _hist1->detachView(this);
        if (_hist2)     _hist2->detachView(this);
}

void StatView::setStat ( double m, double sd )
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

void StatView::setStatColor ( double mR, double mG, double mB,
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


void StatView::update ()
{
	// Red histogram
        if ( (_hist != NULL) && (_hist1 == NULL) && (_hist2 == NULL) )
                setStat ( _hist->getMean(), _hist->getStDev() );

        // Green histogram
        else if ( (_hist == NULL) && (_hist1 != NULL) && (_hist2 == NULL) )
                setStat ( _hist1->getMean(), _hist1->getStDev() );

        // Blue histogram
        else if ( (_hist == NULL) && (_hist1 == NULL) && (_hist2 != NULL) )
                setStat ( _hist2->getMean(), _hist2->getStDev() );


	//Color RGB histogram
	if ( (_hist != NULL) && (_hist1 != NULL) && (_hist2 != NULL) )
                setStatColor ( _hist->getMean(), _hist1->getMean(), _hist2->getMean(),
                          _hist->getStDev(), _hist1->getStDev(), _hist2->getStDev() );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistLogVerAxis.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////
// HistVerAxisView.cc:  This module is designed for use on MPF 
// display.  It displays an vertical axis without tick marks, but 
// with labels indicating the DN count for a logarithmically scaled 
// histogram.  Labels are 1e1, 1e2, 1e3, etc.  User can set resources 
// *fontname and *drawOffset to specify position and appearance of 
// labels.  Note that this component will not work properly if 
// histogram's spike is greater than one.
//////////////////////////////////////////////////////////////////
#include "HistLogVerAxis.h"
#include "Histogram.h"
#include <stdio.h>
#include <math.h>

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

HistLogVerAxis::HistLogVerAxis ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor) 
		: HistAxisView (parent,name,hist,hist1,hist2, hor)
{
    // Empty
}

void HistLogVerAxis::display ( )
{
    if ( !XtIsRealized(_ruler) )
	return;

    // Make any resize cause expose event
    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay(_ruler),
		  XtWindow(_ruler), CWBitGravity, &attrs );

    // Save geometry
    XtVaGetValues ( _ruler,
                  XmNwidth,  &_width,
                  XmNheight, &_height,
                  NULL );
 
    if (_hor == HORIZONTAL) { 

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	double maxValue = 0.0;
	if (_hist && _hist1 && _hist2) 
	    maxValue = (double)MAX(MAX(_hist->spike(1),
                                           _hist1->spike(1)),
                                       _hist2->spike(1));
	else if (_hist)
	    maxValue = _hist->spike(1);
	else if (_hist1)
	    maxValue = _hist1->spike(1);
	else if (_hist2)
	    maxValue = _hist2->spike(1);

	if ( maxValue > 0 ) {
	    maxValue = ceil(log10(maxValue));
	}
	else {
	    maxValue = 1.0;
	}

	for (int i = 1; i <= (int)maxValue; i++) {
		char str [8];
		sprintf(str, "1e%d", i);
		double r;
		r = log10(1 * pow(10.0, (double)i)) / maxValue;
		int strHeight = _fontStruct->ascent;
		if (i != maxValue)
		    XDrawString ( XtDisplay(_ruler), XtWindow(_ruler), _gc,
			_drawOffset, (int)(_height-1-(_height*r)+strHeight/2),
			str, strlen(str) );
		else 
		    XDrawString ( XtDisplay(_ruler), XtWindow(_ruler), _gc,
			_drawOffset, (int)(_height-1-(_height*r)+strHeight),
			str, strlen(str) );
	}
    }
    else {
	XtVaSetValues ( _ruler, XmNwidth, 0, NULL );
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistBtnInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// HistBtnInterface.cc: A "push button" interface to a Cmd object.
///////////////////////////////////////////////////////////////
#include "HistBtnInterface.h"
#include "Histogram.h"
#include "HistGraphView.h"
#include <Xm/Frame.h>

HistBtnInterface::HistBtnInterface ( Widget parent, Cmd *cmd, 
		Histogram *histR, Histogram *histG, Histogram *histB ) 
	: SgDrawAreaInterface ( parent, cmd )
{
   HistGraphView *histGraphView = new HistGraphView(_w, "histGraphView",
                                        histR, histG, histB,
                                        BLEND, HORIZONTAL, ASC);
   histGraphView->manage();

   _drawingAreaWidget = histGraphView->baseWidget();

   XtAddCallback(histGraphView->baseWidget(),
		 XmNinputCallback, 
		 &CmdInterface::executeCmdCallback,
		 (XtPointer) this );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histview.imake
#define SUBROUTINE histview
#define MODULE_LIST HistBtnInterface.cc \
   HistAxisView.cc HistBox.cc \
   HistGraphView.cc HistHorAxisView.cc HistVerAxisView.cc StatView.cc \
   HistLogVerAxis.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP


$ Return
$!#############################################################################
