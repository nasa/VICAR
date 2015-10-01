$!****************************************************************************
$!
$! Build proc for MIPL module sg_components
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:33
$!
$! Execute by entering:		$ @sg_components
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
$ write sys$output "*** module sg_components ***"
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
$ write sys$output "Invalid argument given to sg_components.com file -- ", primary
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
$   if F$SEARCH("sg_components.imake") .nes. ""
$   then
$      vimake sg_components
$      purge sg_components.bld
$   else
$      if F$SEARCH("sg_components.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sg_components
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sg_components.bld "STD"
$   else
$      @sg_components.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_components.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_components.com -mixed -
	-s SgDrawAreaInterface.cc SgAxisView.cc SgGraphView.cc -
	   SgIntKeyinInterface.cc SgButtonPanel.cc SgColorChooserInterface.cc -
	   SgResourceConverter.cc -
	-i sg_components.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgDrawAreaInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgDrawAreaInterface.cc: A "push button" interface to a Cmd object.
// The surface of a push-button is a drawing area widget.
// This class is intended to have subclasses.  The subclass
// must provide the drawing area widget.
///////////////////////////////////////////////////////////////
#include "SgDrawAreaInterface.h"
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <assert.h>

SgDrawAreaInterface::SgDrawAreaInterface ( Widget parent, Cmd *cmd )
	: CmdInterface ( cmd )
{
   _w = XtVaCreateWidget ( _name, 
			   xmFrameWidgetClass, parent,
			   XmNshadowType, XmSHADOW_OUT,
			   NULL);
   installDestroyHandler();

   _drawingAreaWidget = NULL;

   // The _active member is set when each instance is registered
   // with an associated Cmd object. Now that a widget exists,
   // set the widget's sensitivity according to its active state.

    if ( _active )
        activate();
    else
        deactivate();
}

SgDrawAreaInterface::~SgDrawAreaInterface()
{
    if (_drawingAreaWidget)
	XtDestroyWidget(_drawingAreaWidget);
}

//////////////////////////////////////////////////////////////
// Set the drawing area widget and declare input callback on it.
/////////////////////////////////////////////////////////////
void SgDrawAreaInterface::setDrawingAreaWidget ( Widget w )
{
//    assert ( !XmIsDrawingArea ( w ) );

    _drawingAreaWidget = w;

    XtAddCallback ( _drawingAreaWidget, 
		XmNinputCallback,
		&CmdInterface::executeCmdCallback,
		(XtPointer) this );
}

//////////////////////////////////////////////////////////////
// Handle an event.  If it's a button press, we just invert the
// frame.  If it's a button release, we revert it, and if we're
// still in the window, we call the command.
//////////////////////////////////////////////////////////////
void SgDrawAreaInterface::executeCmd ( XtPointer callData )
{
   if ( _drawingAreaWidget == NULL ) 
	return;

   XmDrawingAreaCallbackStruct *cb = (XmDrawingAreaCallbackStruct *)callData;
 
   if ( cb->reason != XmCR_INPUT )
      return;
 
   if ( cb->event->type == ButtonPress ) {
      XButtonPressedEvent *xbp = (XButtonPressedEvent *)cb->event;
      if ( xbp->button != 1 )
         return;                        // Ignore all but button 1
      XtVaSetValues ( _w, XmNshadowType, XmSHADOW_IN, NULL );
   }
 
   if ( cb->event->type == ButtonRelease ) {
      XButtonReleasedEvent *xbr = (XButtonReleasedEvent *)cb->event;
      if ( xbr->button != 1 )
         return;                        // Ignore all but button 1
      XtVaSetValues ( _w, XmNshadowType, XmSHADOW_OUT, NULL );
 
      // If we're still over the button, run the command
 
      Dimension width, height;
      XtVaGetValues ( _drawingAreaWidget, 
		XmNwidth, &width, 
		XmNheight, &height,
		NULL );
      if ( (xbr->x >= 0) && (xbr->x < (int)width) &&
           (xbr->y >= 0) && (xbr->y < (int)height) ) {
         runCmd();
      }
   }
}

int SgDrawAreaInterface::operator== ( const SgDrawAreaInterface &dai )
{
   return this == &dai;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// SgAxisView.C: A component class to show a plot axis.
////////////////////////////////////////////////////////
#include "SgAxisView.h"
#include "ErrorManager.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <assert.h>
#include <stdio.h>
#include <ctype.h>

// Resources for this class

XtResource SgAxisView::_resources [ ] = {
 {
    (char *)"min",
    (char *)"Min",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgAxisView *, _min ),
    XmRString,
    ( XtPointer ) "0",
 },
 {
    (char *)"max",
    (char *)"Max",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgAxisView *, _max ),
    XmRString,
    ( XtPointer ) "255",
 },
 {
    (char *)"drawTicksOnly",
    (char *)"DrawTicksOnly",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgAxisView *, _ticksOnly ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"intRange",
    (char *)"IntRange",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgAxisView *, _intRange ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"fontList",
    (char *)"FontList",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "6x10",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"fistTickMargin",
    (char *)"FistTickMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _fistTickMargin ),
    XmRImmediate,
    ( XtPointer ) 0,
 },
 {
    (char *)"lastTickMargin",
    (char *)"LastTickMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _lastTickMargin ),
    XmRImmediate,
    ( XtPointer ) 0,
 },
 {
    (char *)"tickThickness",
    (char *)"TickThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _tickThickness ),
    XmRImmediate,
    ( XtPointer ) 1,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 8,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"strOffset",
    (char *)"StrOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _strOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String SgAxisView::_defaults[] = {
    (char *)"*height:           256",
    (char *)"*width:            40",
     NULL,
};

SgAxisView::SgAxisView ( Widget parent, const char *name, Boolean vertical ) 
		: UIComponent (name)
{
    _vertical = vertical;
    _ascending = TRUE;

    setDefaultResources ( parent, _defaults );

    _w = XtVaCreateWidget ( _name,
			    xmDrawingAreaWidgetClass, parent,
			    NULL );

    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		    &SgAxisView::displayCallback,
		    ( XtPointer ) this );

    // Allocate private GC 

    _gc = XCreateGC ( XtDisplay ( _w ), 
		RootWindowOfScreen ( XtScreen ( _w ) ),
		0L, NULL );

    // Modify GC, setting foreground, line attributes, and font

    Pixel pixel;
    XtVaGetValues ( _w, XmNforeground, &pixel, NULL );
    XSetForeground ( XtDisplay ( _w ), _gc, pixel );

    XSetLineAttributes ( XtDisplay(_w), _gc, _tickThickness, 
			 LineSolid, CapButt, JoinMiter );

    _fontStruct = XLoadQueryFont ( XtDisplay(_w), _fontname );
    if ( _fontStruct == NULL ) {
	theErrorManager->process ( Error, "axis", "No such font", _fontname);
	_fontStruct = XQueryFont ( XtDisplay(_w), XGContextFromGC(_gc) );
    }
    else {
	XSetFont ( XtDisplay(_w), _gc, _fontStruct->fid );
    }
}

SgAxisView::~SgAxisView()
{
	if ( _w && _gc )
		XFreeGC ( XtDisplay ( _w ), _gc );

        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay ( _w ), _fontStruct );
}

void SgAxisView::displayCallback ( Widget, XtPointer clientData, XtPointer )
{
        SgAxisView *obj = ( SgAxisView * ) clientData;
        obj->display();
}

void SgAxisView::display()
{
    if ( !XtIsRealized ( _w ) )
	return;

    // Make any resize cause expose event (see vol. 6A, p.346-7)

    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay ( _w ), XtWindow ( _w ),
                              CWBitGravity, &attrs );

    // Get current size

    Dimension width, height;
    XtVaGetValues ( _w,
		    XmNwidth,  &width,
                    XmNheight, &height,
                    NULL );

    // Always clear window before start drawing

    XClearWindow ( XtDisplay(_w), XtWindow(_w) );

    // Draw the ticks and the labels

    if ( _vertical == TRUE )
	drawVertical ( height, width );
    else 
	drawHorizontal ( width );
}

void SgAxisView::drawVertical ( Dimension height, Dimension width )
{
    Dimension drawHeight = height - _fistTickMargin - _lastTickMargin;

    // Draw a long line from side to side

    if ( _ticksOnly == FALSE )
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset, 
			_fistTickMargin,
			width - _drawOffset, 
			_fistTickMargin + drawHeight );

    // Calculate how many ticks we need at this screen width

    int numTicks = getNumTicks ( drawHeight );

    // Calculate distance between ticks, double precision

    double step = (double)drawHeight / (double)(numTicks-1);

    int i;
    for ( i = 0; i < numTicks - 1; i+=2 ) {

	// Draw a long tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset - _longTickLength,
			(int)(_fistTickMargin + (double)i * step),
			width - _drawOffset,
			(int)(_fistTickMargin + (double)i * step) );
    }

    for ( i = 1; i < numTicks - 1; i+=2 ) {

	// Draw a short tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset - _shortTickLength,
			(int)(_fistTickMargin + (double)i * step),
			width - _drawOffset,
			(int)(_fistTickMargin + (double)i * step) );
    }

    // Draw the last tick

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
		width - _drawOffset - _longTickLength,
		height - _lastTickMargin - 1,
		width - _drawOffset,
		height - _lastTickMargin - 1 );

    // Draw label to every other tick

    double lbl;
    char buf [16];

    for ( i = 0; i < numTicks; i+=2 ) {
	lbl = _min + (double(_max - _min) / (double)(numTicks-1) ) * (double)i;
	if ( _ascending )
	    lbl = (_max + _min) - lbl;

	if ( _intRange )
	    sprintf ( buf, "%d", (int)lbl );
	else
	    sprintf ( buf, "%.3g", lbl );

	Dimension strWidth = XTextWidth ( _fontStruct, buf, strlen(buf) );
	Dimension strHeight = Dimension ( _fontStruct->ascent );

	// First and last ticks are special cases

	if ( i > 0 && i < numTicks - 1 )
	    strHeight /= 2;
	else if ( i == numTicks - 1 )
	    strHeight = 0;

	XDrawString ( XtDisplay ( _w ), XtWindow ( _w ), _gc,
		width - _drawOffset - _longTickLength - _strOffset - strWidth,
		(int)(_fistTickMargin + (double)i * step + strHeight),
		buf, strlen(buf) );
    }
}

void SgAxisView::drawHorizontal ( Dimension width )
{
    Dimension drawWidth = width - _fistTickMargin - _lastTickMargin;

    // Draw a long line (a ruler?) from side to side

    if ( _ticksOnly == FALSE )
        XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			_fistTickMargin, 
			_drawOffset,
			width - _lastTickMargin, 
			_drawOffset );

    // Calculate how many ticks we need at this screen width

    int numTicks = getNumTicks ( drawWidth );

    // Calculate distance between ticks, double precision

    double step = (double)drawWidth / (double)(numTicks-1);

    int i;
    for ( i = 0; i < numTicks - 1; i+=2 ) {

	// Draw a long tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset, 
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset + _longTickLength );
    }

    for ( i = 1; i < numTicks - 1; i+=2 ) {

	// Draw a short tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset + _shortTickLength );
    }

    // Draw the last tick

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
		width - _lastTickMargin - 1, 
		_drawOffset,
		width - _lastTickMargin - 1,
		_drawOffset + _longTickLength );


    // Draw label to every other tick

    double lbl;
    char buf [16];

    for ( i = 0; i < numTicks; i+=2 ) {
        lbl = _min + (double(_max - _min) / (double)(numTicks-1) ) * (double)i;
        if ( _intRange )
	    sprintf ( buf, "%d", (int)lbl );
        else
            sprintf ( buf, "%.3g", lbl );

	Dimension strWidth = XTextWidth ( _fontStruct, buf, strlen(buf) );
	Dimension strHeight = Dimension ( _fontStruct->ascent );

	// First and last ticks are special cases

	if ( i == 0 )
	    strWidth = 0;
	else if ( i != numTicks - 1 )
	    strWidth /= 2;

	XDrawString (XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step - strWidth),
			_drawOffset + _longTickLength + _strOffset + strHeight,
			buf, strlen(buf) );
    }
}

int SgAxisView::getNumTicks ( Dimension drawWidth )
{
    if ( drawWidth < _twoTicks )
        return 3;
    else if ( drawWidth < _fourTicks )
        return 5;
    else if ( drawWidth < _eightTicks )
        return 9;
    else
        return 17;
}

void SgAxisView::setLimits ( int min, int max )
{ 
    assert ( min <= max );

    _min = (float)min;
    _max = (float)max;
}

void SgAxisView::setLimits ( float min, float max )
{ 
    assert ( min <= max );

    _min = min;
    _max = max;

    display();
}

void SgAxisView::setIntRange ( Boolean intRange )
{
    if ( _intRange == intRange )
	return;
    else
	_intRange = intRange;

    display();
}

void SgAxisView::setVertical ( Boolean vertical )
{
    if ( _vertical == vertical )
	return;
    else 
	_vertical = vertical;

    Dimension width;
    if ( _vertical ) {
	XtVaGetValues ( _w, 
		XmNheight, &width, 
		NULL);
	XtVaSetValues ( _w,
                XmNwidth, width,
                NULL);
    }
    else {
        XtVaGetValues ( _w,
                XmNwidth, &width,
                NULL);
        XtVaSetValues ( _w,
                XmNheight, width,
                NULL);
    }

    display();
}

void SgAxisView::setAscending ( Boolean ascending )
{
    if ( _ascending == ascending )
	return;
    else
	_ascending = ascending;

    display();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgGraphView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SgGraphView.cc:  This is a view component that draws one or three 
// graphs.
//////////////////////////////////////////////////////////////////////////
#include "SgGraphView.h"
#include "ErrorManager.h"
#include <Xm/DrawingA.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include <math.h>

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

// Resources for this class

XtResource SgGraphView::_resources [ ] = {
 {
    (char *)"blended",
    (char *)"Blended",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _blended ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"horizontal",
    (char *)"Horizontal",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _horizontal ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"ascending",
    (char *)"Ascending",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _ascending ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"spike",
    (char *)"Spike",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _spike ),
    XmRString,
    ( XtPointer ) "1",
 },
 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
 {
    (char *)"magentaColor",
    (char *)"MagentaColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _magenta ),
    XmRImmediate,
    ( XtPointer ) "magenta",
 },
 {
    (char *)"yellowColor",
    (char *)"YellowColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _yellow ),
    XmRImmediate,
    ( XtPointer ) "yellow",
 },
 {
    (char *)"cyanColor",
    (char *)"CyanColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _cyan ),
    XmRImmediate,
    ( XtPointer ) "cyan",
 },
 {
    (char *)"whiteColor",
    (char *)"WhiteColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _white ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"filled",
    (char *)"Filled",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _filled ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"maxValue",
    (char *)"MaxValue",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgGraphView *, _maxUserValue ),
    XmRString,
    ( XtPointer ) "-1",
 },
 {
    (char *)"topMargin",
    (char *)"TopMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgGraphView *, _topMargin ),
    XmRString,
    ( XtPointer ) "0",
 },
 {
    (char *)"logScale",
    (char *)"LogScale",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _log ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"drawInsideTicks",
    (char *)"DrawInsideTicks",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _drawInsideTicks ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"insideTickColor",
    (char *)"InsideTickColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _insideTickColor ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"insideMajorTickLength", 
    (char *)"InsideMajorTickLength",
    XmRInt, 
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideTickLength ),
    XmRString,
    ( XtPointer ) "10", 
 },
 {
    (char *)"insideMinorTickLength",
    (char *)"InsideMinorTickLength",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideMinorTickLength ),
    XmRString,
    ( XtPointer ) "5",
 },
 {
    (char *)"insideTickInterval",
    (char *)"InsideTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideTickInterval ),
    XmRString,
    ( XtPointer ) "64",
 },
 {
    (char *)"insideMinorTickInterval",
    (char *)"InsideMinorTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideMinorTickInterval ),
    XmRString,
    ( XtPointer ) "16",
 },
};

String SgGraphView::_defaults[] = {
    (char *)".height:           256",
    (char *)".width:            256",
    (char *)"*background:	black",
     NULL,
};

SgGraphView::SgGraphView ( Widget parent, const char *name, 
		float *r, float *g, float *b, int numElements )
	: UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = numElements;
    _singleGraph = FALSE;

    // Allocate memory for three arrays

    int i;
    for ( i = 0; i < 3; i++) {
	_dataSet[i] = new float [numElements];
	if ( _dataSet[i] == NULL )
	    theErrorManager->process ( Error, "SgGraphView", 
				       "Memory allocation error" );
    }

    // Make local copy of arrays.  If any of the arrays is NULL, 
    // fill it with zeros

    if ( r ) {
	for ( i = 0; i < numElements; i++ )
	    _dataSet[0][i] = r[i];
    }
    else {
	for ( i = 0; i < numElements; i++ )
	    _dataSet[0][i] = 0.0;
    }

    if ( g ) {
        for ( i = 0; i < numElements; i++ )
            _dataSet[1][i] = g[i];
    }
    else {
        for ( i = 0; i < numElements; i++ )
            _dataSet[1][i] = 0.0;
    }

    if ( b ) {
        for ( i = 0; i < numElements; i++ )
            _dataSet[2][i] = b[i];
    }
    else {
        for ( i = 0; i < numElements; i++ )
            _dataSet[2][i] = 0.0;
    }

    _w  = XtVaCreateWidget ( _name,
			     xmDrawingAreaWidgetClass, parent,
			     NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		    &SgGraphView::displayCallback,
		    ( XtPointer ) this );

    // Allocate private GC 

    _gc = XCreateGC ( XtDisplay ( _w ), 
                RootWindowOfScreen ( XtScreen ( _w ) ),
                0L, NULL );

    allocateColors();

    _maxValue = maxSpikeValue();
}

SgGraphView::SgGraphView ( Widget parent, const char *name,
                float *ds, int numElements )
        : UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = numElements;
    _singleGraph = TRUE;

    _dataSet[0] = new float [_numElements];
    if ( _dataSet[0] == NULL )
        theErrorManager->process ( Error, "SgGraphView", "Memory allocation error" );
    for ( int i = 0; i < _numElements; i++)
	_dataSet[0][i] = ds[i];

    _dataSet[1] = NULL;
    _dataSet[2] = NULL;

    _w  = XtVaCreateWidget ( _name,
                             xmDrawingAreaWidgetClass, parent,
                             NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
                    &SgGraphView::displayCallback,
                    ( XtPointer ) this );

    // Allocate private GC

    _gc = XCreateGC ( XtDisplay ( _w ),
                RootWindowOfScreen ( XtScreen ( _w ) ),
                0L, NULL );
    allocateColors();
}

SgGraphView::SgGraphView ( Widget parent, const char *name) : UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = 0;
    _singleGraph = TRUE;

    for ( int i = 0; i < 3; i++)
	_dataSet[i] = NULL;

    _w  = XtVaCreateWidget ( _name,
			     xmDrawingAreaWidgetClass, parent,
			     NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		&SgGraphView::displayCallback,
		( XtPointer ) this );

    // Allocate private GC

    _gc = XCreateGC ( XtDisplay ( _w ),
		RootWindowOfScreen ( XtScreen ( _w ) ),
		0L, NULL );
    allocateColors();
}

SgGraphView::~SgGraphView()
{
    for ( int i = 0; i < 3; i++)
	if ( _dataSet[i] )
		delete [] _dataSet[i];

    XFreeGC ( XtDisplay ( _w ), _gc );
}

void SgGraphView::displayCallback ( Widget, XtPointer clientData, XtPointer )
{
    SgGraphView *obj = ( SgGraphView * ) clientData;
    obj->display();
}

void SgGraphView::display()
{
    if ( !XtIsRealized ( _w ) )
	return;

    // Make any resize cause expose event

    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay(_w), XtWindow(_w), 
			      CWBitGravity, &attrs );

    // Get current size

    XtVaGetValues ( _w,
		    XmNwidth,  &_width,
		    XmNheight, &_height,
		    NULL );

    // Always clear the window before drawing

    XClearWindow ( XtDisplay ( _w ), XtWindow ( _w ) );

    if ( _singleGraph ) {
	if ( _filled )
	    displayOneFilled();
	else
	    displayOneEmpty();
    }
    else {
	if ( !_filled )
	    displayThreeEmpty();
	else {
	    if ( _blended )
		displayThreeBlended();
	    else 
		displayThreeStacked();
	}
    }
}

void SgGraphView::allocateColors()
{
    Colormap cmap = DefaultColormap ( XtDisplay ( _w ),
				DefaultScreen ( XtDisplay ( _w ) ) );

    XColor exact;

    XAllocNamedColor ( XtDisplay(_w), cmap, _red,     &_colorR, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _green,   &_colorG, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _blue,    &_colorB, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _magenta, &_colorM, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _cyan,    &_colorC, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _yellow,  &_colorY, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _white,   &_colorW, &exact );
}

void SgGraphView::setDrawingColor ( XColor color )
{
    XSetForeground ( XtDisplay(_w), _gc, color.pixel );
}

float SgGraphView::spike ( float *ds, int size, int spikeCount )
{
    float oldMaxValue = -1.0;
    for ( int i = 0; i < spikeCount; i++ ) {
	float maxValue = -1.0;
	for ( int j = 0; j < size; j++) {
	    if ( ( ds[j] > maxValue ) && ( oldMaxValue < 0 || ds[j] < oldMaxValue ) )
		maxValue = ds[j];
	}
	if ( maxValue >= 0 )
	    oldMaxValue = maxValue;
    }
    return oldMaxValue;
} 

void SgGraphView::displayOneFilled()
{
    if ( _dataSet[0] == NULL ) return;

    setDrawingColor(_colorR);
    float maxValue = spike ( _dataSet[0], _numElements, _spike );
    if ( _log && (maxValue > 0.0) )
	    maxValue = log10(maxValue);

    if ( maxValue < 1.0 ) maxValue = 1.0;

    float bw;				// bin width in pixels
    if ( _horizontal == FALSE )
	bw = (double)_width / (double)_numElements;
    else
	bw = (double)_height / (double)_numElements;

    for ( int i = 0; i < _numElements; i++) {
	float ratio = _dataSet[0][i] / maxValue;
	if ( _log && (_dataSet[0][i] > 0.0) )
	    ratio = log10(_dataSet[0][i]) / maxValue;

	int x, y, width, height; 
        if (_horizontal == TRUE) {
	    x = 0;
	    if ( _ascending == FALSE ) {		// Descending axis
		y = int(i * bw);
		width = int(_width * ratio) + 1;
		height = int(bw) + 1;
	    }
	    else {				// Ascending axis
		y = int (_height - ((i+1)*bw));
		width = int(_width * ratio) + 1;
		height = int(bw) + 1;
	    }
	}
	else {
	    x = int(i*bw);
	    y = int(_height * (1 - ratio));
	    width = int(bw) + 1;
	    height = int(_height*ratio) + 1;
	}

	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
			 x, y, width, height);
    }

    if (_drawInsideTicks == True)
	drawInsideTicks ( maxValue, bw, _numElements );
}

void SgGraphView::displayOneEmpty()
{
    XPoint *graph = new XPoint [_numElements * 2];
    float horScale = (float)_width / (float)_numElements;
    float verScale = (float)_height / arrayMax(_dataSet[0], _numElements);

    for ( int i = 0; i < _numElements; i++) {
	graph[i*2].x   = (int)(i * horScale);
	graph[i*2+1].x = (int)((i+1) * horScale);
	int y = _height - (int)(_dataSet[0][i] * verScale);
	if ( _log && (_dataSet[0][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[0][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorR);
    XDrawLines ( XtDisplay(_w), XtWindow(_w), 
		 _gc, graph, _numElements*2, CoordModeOrigin );
    delete [] graph;
}

void SgGraphView::displayThreeEmpty()
{
    XPoint *graph = new XPoint [_numElements * 2];
    float horScale = (float)_width / (float)_numElements;

    float rMax = arrayMax(_dataSet[0], _numElements);
    float gMax = arrayMax(_dataSet[1], _numElements);
    float bMax = arrayMax(_dataSet[2], _numElements);

    float rgbMax = MAX ( MAX ( rMax, gMax ), bMax );

    if ( _log && (rgbMax > 0.0) ) rgbMax = log10 ( rgbMax );

    float verScale = (float)_height / rgbMax;

    // Draw red

    int i, y;
    for ( i = 0; i < _numElements; i++) {
	graph[i*2].x   = (int)(i * horScale);
	graph[i*2+1].x = (int)((i+1) * horScale);
	y = _height - (int)(_dataSet[0][i] * verScale);
	if ( _log && (_dataSet[0][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[0][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorR);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		 _gc, graph, _numElements*2, CoordModeOrigin );

    // Draw green

    for ( i = 0; i < _numElements; i++) {
	y = _height - (int)(_dataSet[1][i] * verScale);
	if ( _log && (_dataSet[1][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[1][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorG);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		 _gc, graph, _numElements*2, CoordModeOrigin );

    // Draw blue

    for ( i = 0; i < _numElements; i++) {
	y = _height - (int)(_dataSet[2][i] * verScale);
	if ( _log && (_dataSet[2][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[2][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorB);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		_gc, graph, _numElements*2, CoordModeOrigin );
}

/////////////////////////////////////////////////////////////////////////
// Display histogram using "Stacked Colors" method - for color images only.
// At each bin, the one with the max count goes into the back, and the one 
// with the min count goes in the front of the display.
/////////////////////////////////////////////////////////////////////////
void SgGraphView::displayThreeStacked()
{
    float bw;                           // bin width in pixels
    if ( _horizontal == FALSE )
	bw = (float)_width / (float)_numElements;
    else
	bw = (float)_height / (float)_numElements;

    float maxValue = _maxValue;
    if ( _log && (_maxValue > 0.0) ) 
	maxValue = log10(_maxValue);

    for (int i = 0; i < _numElements; i++) {

	float redValue, grnValue, bluValue;
	if ( _log ) {
	    if ( _dataSet[0][i] )
		redValue = log10(_dataSet[0][i]);
	    else 
		redValue = _dataSet[0][i];
	    if ( _dataSet[1][i] )
		grnValue = log10(_dataSet[1][i]);
	    else 
		grnValue = _dataSet[1][i];
	    if ( _dataSet[2][i] )
		bluValue = log10(_dataSet[2][i]);
	    else 
		bluValue = _dataSet[2][i];
	}
	else {
	    redValue = _dataSet[0][i];
	    grnValue = _dataSet[1][i];
	    bluValue = _dataSet[2][i];
	}

	float ratioRed = redValue / maxValue;
	float ratioGrn = grnValue / maxValue;
	float ratioBlu = bluValue / maxValue;

	// identify the highest, lowest, and middle points in this bin
        const float mn = MIN ( MIN ( redValue, grnValue ), 
			       bluValue );
        const float mx = MAX ( MAX ( redValue, grnValue ), 
			       bluValue );
        const float md = midOfThree ( redValue, grnValue, bluValue );

        float curRatioMn = 0.0;
        float curRatioMd = 0.0;
        float curRatioMx = 0.0;

	// Draw the lower (minimum) portion of the histogram
        if ( mn == redValue ) {
	    curRatioMn = ratioRed;
            setDrawingColor ( _colorR );
        }
        else if ( mn == grnValue ) {
            curRatioMn = ratioGrn;
	    setDrawingColor ( _colorG );
	}
        else if ( mn == bluValue ) {
	    curRatioMn = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "Error: mn must be defined!" << endl;

	if ( _horizontal == TRUE ) {
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
				0,
				int (i*bw),
				int (ceil(_width*curRatioMn)),
				int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*curRatioMn)),
                                int (ceil(bw)) );
	}
	else
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(bw)),  
				int (ceil(_height*curRatioMn)) );


        // Draw the middle portion of the histogram
	if ( md == redValue ) {
	    curRatioMd = ratioRed;
	    setDrawingColor ( _colorR );
        }
        else if ( md == grnValue ) {
	    curRatioMd = ratioGrn;
	    setDrawingColor ( _colorG );
        }
        else if ( md == bluValue ) {
	    curRatioMd = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "SgGraphView: md must be defined!" << endl;

	if ( _horizontal == TRUE )
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*bw),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (_height - int((i+1)*bw)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	else
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMd)),
                                int (ceil(bw)),
				int (ceil(_height*curRatioMd - _height*curRatioMn)) );


        // Draw the top (max) portion of the histogram
	if ( mx == redValue ) {
	    curRatioMx = ratioRed;
	    setDrawingColor ( _colorR );
        }
        else if ( mx == grnValue ) {
	    curRatioMx = ratioGrn;
	    setDrawingColor ( _colorG );
	}
        else if ( mx == bluValue ) {
	    curRatioMx = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "Error: there must be a max point!" << endl;

	if ( _horizontal == TRUE ) 
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int (i*bw),
                                int (ceil(_width*(curRatioMx - curRatioMd))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width * curRatioMd),
                                int (_height - int((i+1)*bw)),
                                int (ceil(_width * (curRatioMx - curRatioMd))),
                                int (ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMx)),
                                int (ceil(bw)), 
				int (ceil(_height*curRatioMx-_height*curRatioMd)) );
    }

    if (_drawInsideTicks == TRUE) {
	drawInsideTicks(maxValue, bw, _numElements);
    }
}

/////////////////////////////////////////////////////////////////////////
// Blended Colors Method
//   Color arithmetics:
//	R + G + B = White
//	R + G = Yellow
//	R + B = Magenta
//	B + G = Cyan
/////////////////////////////////////////////////////////////////////////
void SgGraphView::displayThreeBlended()
{
    double bw;
    if (_horizontal == FALSE )
	bw = (float)_width / (float)_numElements;
    else
	bw = (float)_height / (float)_numElements;

    float maxValue = _maxValue;
    if ( _log && (_maxValue > 0.0) ) maxValue = log10(_maxValue);

    for (int i = 0; i < _numElements; i++) {


	float redValue, grnValue, bluValue;
        if ( _log ) {
            if ( _dataSet[0][i] )
                redValue = log10(_dataSet[0][i]);
            else
                redValue = _dataSet[0][i];
            if ( _dataSet[1][i] )
                grnValue = log10(_dataSet[1][i]);
            else
                grnValue = _dataSet[1][i];
            if ( _dataSet[2][i] )
                bluValue = log10(_dataSet[2][i]);
            else
                bluValue = _dataSet[2][i];
        }
	else {
	    redValue = _dataSet[0][i];
	    grnValue = _dataSet[1][i];
	    bluValue = _dataSet[2][i];
	}

	float ratioRed = redValue / maxValue;
	float ratioGrn = grnValue / maxValue;
	float ratioBlu = bluValue / maxValue;

	// Identify the highest, lowest, and middle points in this bin

	const float mn = MIN ( MIN ( redValue, grnValue ), 
			       bluValue );
        const float mx = MAX ( MAX ( redValue, grnValue ), 
                               bluValue );
        const float md = midOfThree ( redValue, grnValue, bluValue );

	float curRatioMn;
	float curRatioMd;
	float curRatioMx;

	// Draw the lower (minimum) portion of the histogram
	if ( mn == redValue ) {
	    curRatioMn = ratioRed;
	    setDrawingColor ( _colorW );
	}
	else if ( mn == grnValue ) {
	    curRatioMn = ratioGrn;
	    setDrawingColor ( _colorW );
	}
	else if ( mn == bluValue ) {
	    curRatioMn = ratioBlu;
	    setDrawingColor ( _colorW );
	}
	else {
	    char buf1[10];
	    sprintf ( buf1, "Can't locate min, skipping bin %d", i);
	    theErrorManager->process ( Error, "PlotView", buf1, "Location 1" );
	    continue;
	}

	if ( _horizontal == TRUE ) {
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (i*bw),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(bw)) );
	}
	else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(bw)),
				int (ceil(_height*curRatioMn)) );


	// Draw the middle portion of the histogram

	if ( md == redValue ) {
	    curRatioMd = ratioRed;
	    if (curRatioMn == ratioGrn)
		setDrawingColor ( _colorM );
	    else
		setDrawingColor ( _colorY );
	}
	else if ( md == grnValue ) {
	    curRatioMd = ratioGrn;
	    if (curRatioMn == ratioRed)
		setDrawingColor ( _colorC );
	    else
		setDrawingColor ( _colorY );
	}
	else if ( md == bluValue ) {
	    curRatioMd = ratioBlu;
	    if (curRatioMn == ratioRed)
		setDrawingColor ( _colorC );
	    else
		setDrawingColor ( _colorM );
	}
        else {
            char buf2[10];
            sprintf ( buf2, "Can't locate mid, skipping bin %d", i );
            theErrorManager->process ( Error, "PlotView", buf2, "Location 2" );
	    continue;
        }

	if ( _horizontal == TRUE )
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*bw),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	    else 
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width*curRatioMn),
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                             int (i*bw),
			     int (_height * (1 - curRatioMd)),
                             int (ceil(bw)),
			     int (ceil(_height*(curRatioMd - curRatioMn))) );

	// Draw the top (max) portion of the histogram
	if ( mx == redValue ) {
	    curRatioMx = ratioRed;
	    setDrawingColor ( _colorR );
	}
	else if ( mx == grnValue ) {
	    curRatioMx = ratioGrn;
	    setDrawingColor ( _colorG );
	}
	else if ( mx == bluValue ) {
	    curRatioMx = ratioBlu;
	    setDrawingColor ( _colorB );
	}
        else {
            char buf3[10];
            sprintf ( buf3, "Can't locate mid, skipping bin %d", i );
            theErrorManager->process ( Error, "PlotView", buf3, "Location 3" );
            continue;
        }

	if ( _horizontal == TRUE ) 
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width * curRatioMd),
                                int(i * bw),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(bw)) );
	    else 
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width * curRatioMd),
                                _height - (int((i+1)*bw)),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                             int(i * bw), 
			     int(_height * (1 - curRatioMx)),
                             int(ceil(bw)), 
			     int(ceil(_height*(curRatioMx-curRatioMd))) );
    }

    if (_drawInsideTicks == True) {
	drawInsideTicks(maxValue, bw, _numElements);
    }
}

float SgGraphView::maxSpikeValue()
{
    float sred, sgrn, sblu;
    sred = spike ( _dataSet[0], _numElements, _spike );
    sgrn = spike ( _dataSet[1], _numElements, _spike );
    sblu = spike ( _dataSet[2], _numElements, _spike );

    float maxValue = (float)MAX(MAX(sred, sgrn), sblu);
    if ( maxValue < 1.0 ) maxValue = 1.0;

    return maxValue;
}

void SgGraphView::setDataSet ( float *a, int numElements )
{
    _singleGraph = TRUE;
    _numElements = numElements;

    for ( int j = 0; j < 3; j++ ) {
	if (_dataSet[j]) delete [] _dataSet[j];
	_dataSet[j] = new float [_numElements];
    }

    for ( int i = 0; i < _numElements; i++ ) {
	_dataSet[0][i] = a[i];
	_dataSet[1][i] = 0;
	_dataSet[2][i] = 0;
    }
}

void SgGraphView::setDataSets ( float *r, float *g, float *b, int numElements )
{
    _singleGraph = FALSE;
    _numElements = numElements;

    for ( int j = 0; j < 3; j++ ) {
	if (_dataSet[j]) delete [] _dataSet[j];
	_dataSet[j] = new float [_numElements];
    }

    for ( int i = 0; i < _numElements; i++ ) {
	_dataSet[0][i] = r[i];
	_dataSet[1][i] = g[i];
	_dataSet[2][i] = b[i];
    }

    _maxValue = maxSpikeValue();

    display();
}

////////////////////////////////////////////////////////////////////////////
// setSpike: set the spike value and redisplay the plot
////////////////////////////////////////////////////////////////////////////
void SgGraphView::setSpike ( int spike )
{
    _spike = spike;
    _maxValue = maxSpikeValue();
    display();
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
		0, int(_height-1-(_height*r)),			\
		a, int(_height-1-(_height*r)) );		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		_width-1, int(_height-1-(_height*r)),		\
		_width-1-a, int(_height-1-(_height*r)) );

#define DRAW_BIN_TICK( i, interval, length )				\
        for (i = 0; i < numBins; i+=interval) {				\
            if (_horizontal == TRUE) {					\
                if ( _ascending == FALSE ) {				\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, int(i*bw),				\
                                length, int(i*bw) );			\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, int(i*bw),			\
                                _width-1-length, int(i*bw) );		\
                }							\
                else {							\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, int(_height-i*bw),			\
                                length, int(_height-i*bw) );		\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, int(_height-i*bw),		\
                                _width-1-length, int(_height-i*bw) );	\
                }							\
            }								\
            else {							\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                int(i*bw), _height-1,			\
                                int(i*bw), _height-1-length );		\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                int(i*bw), 0,				\
                                int(i*bw), length );			\
            }								\
        }


/////////////////////////////////////////////////////////////////////////
// drawInsideTicks: Draw tick marks on the edge of the histogram 
// display.  Note that ticks specifying the DN count are displayed only 
// if logarithmic scale was used to draw a histogram.  Otherwise only 
// ticks along the bin axis are displayed.  Also note that ticks are 
// drawn on both left and right (top and bottom) edges.
/////////////////////////////////////////////////////////////////////////
void SgGraphView::drawInsideTicks(double maxValue, double bw, int numBins)
{
    if ( _log == FALSE )
	return;

    Colormap cmap = DefaultColormap ( XtDisplay(_w),
				DefaultScreen ( XtDisplay(_w) ) );
    XColor color, exact;
    XAllocNamedColor ( XtDisplay(_w), cmap, _insideTickColor, &color, &exact );
    setDrawingColor ( color );

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, _width-1, 0 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, 0, _height-1 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, _height-1, _width-1, _height-1 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				_width-1, 0, _width-1, _height-1 );

    int i;
    for (i = 0; i < maxValue; i++) {
	double r; 

	// Draw long ticks at 10^i
	r = log10(1 * pow(10.0, (double)i)) / maxValue;
	if (_horizontal == TRUE) {
		DRAW_HOR_LOG_TICK(_insideTickLength);
	}
	else {
		DRAW_VER_LOG_TICK(_insideTickLength);
	}

	// Draw minor ticks at 2*10^i, 4*10^i, 6*10^i, 8*10^i
	for (int j = 2; j < 10; j+=2) {
	    r = log10(j * pow(10.0, (double)i)) / maxValue;
	    if ( _horizontal == TRUE ) {
		DRAW_HOR_LOG_TICK(_insideMinorTickLength);
	    }
	    else {
		DRAW_VER_LOG_TICK(_insideMinorTickLength);
	    }
	}
    }

    DRAW_BIN_TICK(i, _insideTickInterval, _insideTickLength);
    DRAW_BIN_TICK(i, _insideMinorTickInterval, _insideMinorTickLength);
}

float SgGraphView::midOfThree ( float a, float b, float c)
{
    if ( a <= b && a >= c ) return a;
    if ( a <= c && a >= b ) return a;
    if ( b <= a && b >= c ) return b;
    if ( b <= c && b >= a ) return b;
    if ( c <= a && c >= b ) return c;
    if ( c <= b && c >= a ) return c;
    return b;		// just to shut up compiler
}

float SgGraphView::arrayMax ( float *a, int size )
{
    float arrayMax = _maxUserValue;
    for ( int i = 0; i < size; i++ )
	arrayMax = MAX ( a[i], _maxUserValue );
    return arrayMax;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgIntKeyinInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgIntKeyinInterface.cc: An integer keyin interface to a Cmd object.
// Command should expect integer as a value.
///////////////////////////////////////////////////////////////
#include "SgIntKeyinInterface.h"
#include "KeyinView.h"
#include "Cmd.h"
#include "stdlib.h"
#include "stdio.h"
#include <stdint.h>
#include <Xm/RowColumn.h>

SgIntKeyinInterface::SgIntKeyinInterface ( Widget parent,
		Cmd *cmd ) : CmdInterface ( cmd )
{
    _w = XtCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			     NULL, 0 );
    installDestroyHandler();

    _keyin = new KeyinView(_w, "KeyinView");
    _keyin->manage();
    _keyin->installCallback(&CmdInterface::executeCmdCallback, 
			(XtPointer)this);

    setValue(_cmd->getValue());
}

SgIntKeyinInterface::~SgIntKeyinInterface()
{
    delete _keyin;
}

void SgIntKeyinInterface::executeCmd(XtPointer)
{
    char *strValue = _keyin->getFieldValue();
    int intValue = atoi(strValue);
    XtFree(strValue);

    runCmd((CmdValue) (uintptr_t) intValue);
}

void SgIntKeyinInterface::setValue(CmdValue value)
{
    char strValue[20];
    int intValue = (int) (uintptr_t)value;

    sprintf(strValue, "%d", intValue);

    _keyin->setFieldValue(strValue);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgButtonPanel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// SgButtonPanel.h: This is a row-column container that user can stack with
// commands, each represented by a command interface.
//////////////////////////////////////////////////////////////////////////////
#include "SgButtonPanel.h"
#include "ButtonInterface.h"
#include "OptionCmdMenu.h"
#include <Xm/RowColumn.h>

SgButtonPanel::SgButtonPanel(Widget parent, const char *name)
    : UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNpacking, XmPACK_TIGHT,
			  NULL);
    installDestroyHandler();
}

void SgButtonPanel::addCommands(Cmd *cmd)
{
    ButtonInterface *bi = new ButtonInterface(_w, cmd);
    bi->manage();
}

void SgButtonPanel::addOptionMenu(CmdList *cmdList)
{
    UIComponent *optionMenu = new OptionCmdMenu(_w, "option", cmdList);
    optionMenu->manage();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgColorChooserInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// SgColorChooserInterface.h:  A command interface that allows user to select 
// a color by either typing the value or using color chooser.  It then 
// executes command with value passed as a string (either standard X color
// name, like 'red', or hex value in the form #RRGGBB.
//////////////////////////////////////////////////////////////////////////////
#include "SgColorChooserInterface.h"
#include "ColorChooser.h"
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

static String resources[] = {
    (char *)"*button.labelString: Select Color",
    NULL,
};

SgColorChooserInterface::SgColorChooserInterface(Widget parent, Cmd *cmd)
	: CmdInterface(cmd)
{
    _oldValue = NULL;

    setDefaultResources(parent, resources);

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  NULL);
    installDestroyHandler();

    _text = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, _w,
				    NULL);

    _button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, _w, 
				      NULL);
    XtAddCallback(_button,
		  XmNactivateCallback,
		  &SgColorChooserInterface::pickColorCallback,
		  (XtPointer)this);
    _colorChooser = new ColorChooser(_w, "colorChooser");

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

    XtAddCallback(_text, XmNlosingFocusCallback, 
		  &CmdInterface::executeCmdCallback,
		  (XtPointer)this);
    XtAddCallback(_text, XmNactivateCallback, 
		  &CmdInterface::executeCmdCallback,
		  (XtPointer)this);
}

void SgColorChooserInterface::pickColorCallback(Widget, XtPointer clientData, 
						XtPointer)
{
    SgColorChooserInterface *obj = (SgColorChooserInterface *)clientData;
    ColorChooser *colorChooser = obj->_colorChooser;
    
    // Make color chooser to post a dialog and register a callback to return 
    // the results

    colorChooser->pickColor(&SgColorChooserInterface::colorSelectedCallback, 
			    NULL, clientData);
}

void SgColorChooserInterface::colorSelectedCallback(int red, int green, 
						    int blue,
						    void *clientData)
{
    SgColorChooserInterface *obj = (SgColorChooserInterface *)clientData;

    unsigned int c = (red << 16) | (green << 8) | blue;
    char *buf = new char[16];
    sprintf(buf, "#%06x", c);
    XmTextFieldSetString(obj->_text, buf);
    obj->runCmd((CmdValue) buf);
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::executeCmd(XtPointer)
{
    char *string;

    string = XmTextFieldGetString(_text);

    if (_oldValue && strcmp(string, _oldValue) == 0) {
        // Duplicate value, so don't execute the command again
        XtFree(string);
    }
    else {
        copyOldValue(string);
        runCmd((CmdValue) string);
    }
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::setValue(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_text, (char *)value);
    copyOldValue((char *)value);

    XColor xcolor;
    XParseColor(XtDisplay(_w), DefaultColormapOfScreen(XtScreen(_w)), 
		(char *)value, &xcolor);
    _colorChooser->setColor((unsigned)(xcolor.red >> 8), 
			    (unsigned)(xcolor.green >> 8), 
			    (unsigned)(xcolor.blue >> 8));
}

///////////////////////////////////////////////////////////////////
//  Allow execution of command after value is set.
///////////////////////////////////////////////////////////////////
void SgColorChooserInterface::setValueAndRun(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_text, (char *)value);
    executeCmd();
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::copyOldValue(char *string)
{
    if (_oldValue)
        delete _oldValue;
    if (string)
        _oldValue = strdup(string);
    else
        _oldValue = NULL;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgResourceConverter.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "SgResourceConverter.h"
#include "Application.h"
#ifndef NO_XPM
#include <X11/xpm.h>
#else
#include <X11/Xlib.h>
#endif
#include <ctype.h>
#include <stdio.h>

Boolean SgResourceConverter::_firstTime = True;

void SgResourceConverter::registerStringToPixmapConverter()
{
    if (_firstTime == True) {
        _firstTime = False;
 
        XtAppAddConverter(theApplication->appContext(),
                          XmRString, XmRPrimForegroundPixmap,
                          cvtStringToPixmap,
                          NULL, 0);
    }
}

void SgResourceConverter::cvtStringToPixmap(XrmValue *, 
					    Cardinal *,
					    XrmValue *fromVal, 
					    XrmValue *toVal)
{
    static Pixmap pixmap;
    char *image_name = (char *) (fromVal->addr);
    Screen *screen;
    char *name;
    Pixel foreground;
    Pixel background;
 
    if (stringsAreEqual(image_name, "unspecified_pixmap")) {
        pixmap = XmUNSPECIFIED_PIXMAP;
    }
    else {
        screen = XtScreen(theApplication->baseWidget());
        name = XtResolvePathname(XtDisplay(theApplication->baseWidget()),
                                 "bitmap", image_name, NULL,
                                 NULL, NULL, 0, NULL);
#ifndef NO_XPM
        if (name && strstr(image_name, ".xpm")) {
            Pixmap shapemask;
            int status = XpmReadFileToPixmap(theApplication->display(),
                                DefaultRootWindow(theApplication->display()),
                                name,
                                &pixmap, &shapemask,
                                NULL);
	    if (status != XpmSuccess || pixmap == 0) 
		pixmap = XmUNSPECIFIED_PIXMAP;
            if (shapemask != None)
                XFreePixmap(theApplication->display(), shapemask);
            XtFree(name);
        }
        else
#endif
        if (name) {
            XtVaGetValues(theApplication->baseWidget(),
                          XmNforeground, &foreground,
                          XmNbackground, &background,
                          NULL);
            pixmap = XmGetPixmap(screen, name, foreground, background);
        }
        else {
	    XtVaGetValues(theApplication->baseWidget(),
                          XmNforeground, &foreground,
                          XmNbackground, &background,
                          NULL);
            pixmap = XmGetPixmap(screen, image_name, foreground, background);
	}
    }
    (*toVal).size = sizeof (Pixmap);
    (*toVal).addr = (XPointer) &pixmap;
}

Boolean SgResourceConverter::stringsAreEqual(const char *in_str,
						const char *test_str)
{
    register char i ;
 
    if(((in_str[0] == 'X') || (in_str[0] == 'x'))
       && ((in_str[1] == 'M') || (in_str[1] == 'm'))) {
        in_str +=2;
    }
    do {
        i = (char) tolower( *in_str++);
	
        if (i != *test_str++) {
            return( False);
        }
    } while (i);
    
    return(True);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sg_components.imake
#define SUBROUTINE sg_components
#define MODULE_LIST SgDrawAreaInterface.cc SgAxisView.cc SgGraphView.cc \
   SgIntKeyinInterface.cc SgButtonPanel.cc SgColorChooserInterface.cc \
   SgResourceConverter.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_XPM
$ Return
$!#############################################################################
