$!****************************************************************************
$!
$! Build proc for MIPL module lutview
$! VPACK Version 1.8, Wednesday, June 12, 1996, 13:28:38
$!
$! Execute by entering:		$ @lutview
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
$ write sys$output "*** module lutview ***"
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
$ write sys$output "Invalid argument given to lutview.com file -- ", primary
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
$   if F$SEARCH("lutview.imake") .nes. ""
$   then
$      vimake lutview
$      purge lutview.bld
$   else
$      if F$SEARCH("lutview.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lutview
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lutview.bld "STD"
$   else
$      @lutview.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lutview.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lutview.com -mixed -
	-s LutAxisView.cc LutHorAxisView.cc LutVerAxisView.cc LutBox.cc -
	   LutBtnInterface.cc LutGraphView.cc -
	-i lutview.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LutAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutAxisView.cc:
////////////////////////////////////////////////////////
#include "LutAxisView.h"
#include "Lut.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream>
using namespace std;

// Resources for this class

XtResource LutAxisView::_resources [ ] = {
 {
    (char *)"fontname",
    (char *)"Fontname",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "9x15",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 7,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String LutAxisView::_defaults[] = {
    (char *)"*height:           40",
    (char *)"*width:            40",
     NULL,
};

LutAxisView::LutAxisView ( Widget parent, const char *name,
		Lut *lut, Lut *lut1, Lut *lut2 ) 
	: LutView (name, lut, lut1, lut2)
{
	// We need at least and only one lut, so just make sure 
	// that _lut is not empty.  User has to make sure that 
	// all three luts are the same size if not empty
	if (!_lut) {
	    if (_lut1)	_lut = _lut1;
	    else if (_lut2) _lut = _lut2;
	    else cerr << "All LUTs are empty!\n";
	}

	// Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
				XmNborderWidth,	0,
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
			&LutAxisView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w,0,0);
	Font font = XLoadFont ( XtDisplay(_w), "6x10" );
	XSetFont ( XtDisplay(_w), _gc, font );
	_fontStruct = XQueryFont ( XtDisplay(_w), font );
	if ( _fontStruct == 0 ) cerr << "No such font";
}

LutAxisView::LutAxisView ( Widget parent, const char *name, Lut *lut )
        : LutView (name, lut)
{
        // We need at least one lut, so just make sure
        // that _lut is not empty.
        if (!_lut)
            cerr << "LUT is empty!\n";

        // Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
                                XmNborderWidth, 0,
                                NULL );
        installDestroyHandler();

        getResources ( _resources, XtNumber ( _resources ) );

        _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
                                        XmNleftAttachment,      XmATTACH_FORM,
                                        XmNtopAttachment,       XmATTACH_FORM,
                                        XmNrightAttachment,     XmATTACH_FORM,
                                        XmNbottomAttachment,    XmATTACH_FORM,
                                        XmNborderWidth, 0,
                                        NULL );

        XtAddCallback ( _ruler, XmNexposeCallback,
                        &LutAxisView::displayCallback,
                        ( XtPointer ) this );

        _gc = XtGetGC ( _w,0,0);
        Font font = XLoadFont ( XtDisplay(_w), "6x10" );
        XSetFont ( XtDisplay(_w), _gc, font );
        _fontStruct = XQueryFont ( XtDisplay(_w), font );
        if ( _fontStruct == 0 ) cerr << "No such font";
}


LutAxisView::~LutAxisView ()
{
	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay(_w), _fontStruct );
}

void  LutAxisView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        LutAxisView *obj = ( LutAxisView * ) clientData;
        obj->update();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutHorAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutVerAxisView.C: This class implements update function
// that draws axis oriented vertically.
////////////////////////////////////////////////////////
#include "LutHorAxisView.h"
#include "Lut.h"
#include <stdio.h>
#include <iostream>
using namespace std;

void LutHorAxisView::update ( )
{
	if ( ! XtIsRealized(_w) ) return;

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
 
        int min = 0;
        int max = 256;

        int lbl[16];		
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
	   else if (i == numTicks)		// last label
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

	   // subtract previously added 1 from the nax label
	   if (i == numTicks) sprintf ( buf[i], "%d", lbl[i]-1);
	   else sprintf ( buf[i], "%d", lbl[i]);

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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutVerAxisView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutVerAxisView.C:
////////////////////////////////////////////////////////
#include "LutVerAxisView.h"
#include "Lut.h"
#include <stdio.h>
#include <iostream>
using namespace std;

void LutVerAxisView::update ( )
{
	if ( ! XtIsRealized(_w) ) return;

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
 
        int min = 0;		// min label
        int max = 256;		// max label (add 1 for convenience)

        int lbl[16];
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
             lbl[i] = max - (min + ((max - min) / numTicks ) * i);

	     // subtract previously added 1 from the nax label
	     if ( i == 0 ) sprintf ( buf[i], "%d", lbl[i]-1);
	     else sprintf ( buf[i], "%d", lbl[i]);

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

	   temp1 = temp1 + temp2;
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// LutBox.cc
///////////////////////////////////////////////////////////////////
#include "LutBox.h"
#include "LutGraphView.h"
#include "LutHorAxisView.h"
#include "LutVerAxisView.h"
#include "Lut.h"
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdlib.h>

LutBox::LutBox(Widget parent, 
		const char *name,
		Lut *lut1, Lut *lut2, Lut *lut3 ) : UIComponent(name)
{
	// Create a form to hold the other widgets
	_w = XtVaCreateWidget ( _name, 
				xmFormWidgetClass, parent, 
				NULL);
	installDestroyHandler();

	LutGraphView *lutGraphView = new LutGraphView (_w, "lutGraphView", lut1, lut2, lut3);
	LutAxisView *horAxisView = new LutHorAxisView (_w, "haxis", lut1, lut2, lut3);
	LutAxisView *verAxisView = new LutVerAxisView (_w, "vaxis", lut1, lut2, lut3); 

	XtVaSetValues   ( verAxisView->baseWidget(),
			XmNtopAttachment,       XmATTACH_FORM,
			XmNleftAttachment,      XmATTACH_FORM,
			XmNrightAttachment,     XmATTACH_NONE,
			XmNbottomAttachment,    XmATTACH_WIDGET,
			XmNbottomWidget,        horAxisView->baseWidget(),
			NULL );

	XtVaSetValues   ( lutGraphView->baseWidget(),
                        XmNtopAttachment,	XmATTACH_FORM,
                        XmNleftAttachment, 	XmATTACH_WIDGET,
			XmNleftWidget,		verAxisView->baseWidget(),
                        XmNrightAttachment,	XmATTACH_FORM,
                        XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,	horAxisView->baseWidget(),
                        NULL );

	XtVaSetValues   ( horAxisView->baseWidget(),
			XmNtopAttachment,     	XmATTACH_NONE,
			XmNleftAttachment,    	XmATTACH_FORM,
			XmNrightAttachment,   	XmATTACH_FORM,
			XmNbottomAttachment,  	XmATTACH_FORM,
			XmNleftOffset,		40,
			NULL );


	lutGraphView->manage();
	horAxisView->manage();
	verAxisView->manage();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutBtnInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// LutBtnInterface.cc: A "push button" interface to a Cmd object
//////////////////////////////////////////////////////////////
#include "LutBtnInterface.h"
#include "Lut.h"
#include "LutGraphView.h"

LutBtnInterface::LutBtnInterface ( Widget parent, Cmd *cmd, 
		Lut *lutR, Lut *lutG, Lut *lutB ) 
	: SgDrawAreaInterface ( parent, cmd )
{
   LutGraphView *lutGraphView = new LutGraphView (_w, "lutGraphView",
                                        lutR, lutG, lutB);
   lutGraphView->manage();

   _drawingAreaWidget = lutGraphView->baseWidget();
    
   XtAddCallback ( lutGraphView->baseWidget(),
		   XmNinputCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutGraphView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutGraphView.C:
////////////////////////////////////////////////////////
#include "LutGraphView.h"
#include "Lut.h"
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <iostream>
using namespace std;
#include <stdio.h>

// Resources for this class

XtResource LutGraphView::_resources [ ] = {
 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
};


LutGraphView::LutGraphView ( Widget parent, const char *name, 
		Lut *lut, Lut *lut1, Lut *lut2 ) 
	: LutView (name, lut, lut1, lut2)

{
	_w  = XtVaCreateWidget ( _name,
			xmDrawingAreaWidgetClass, parent,
			NULL );
	installDestroyHandler ();

	getResources ( _resources, XtNumber ( _resources ) );

	XtAddCallback ( _w, XmNexposeCallback,
			&LutGraphView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w, 0, 0 );

	if (_lut)  _lut->attachView(this);
	if (_lut1) _lut1->attachView(this);
	if (_lut2) _lut2->attachView(this);
}

LutGraphView::LutGraphView ( Widget parent, const char *name, Lut *lut )
        : LutView (name, lut)

{
        _w  = XtVaCreateWidget ( _name,
                        xmDrawingAreaWidgetClass, parent,
                        NULL );
        installDestroyHandler ();

        getResources ( _resources, XtNumber ( _resources ) );

        XtAddCallback ( _w, XmNexposeCallback,
                        &LutGraphView::displayCallback,
                        ( XtPointer ) this );

        _gc = XtGetGC ( _w, 0, 0 );

        if (_lut)  _lut->attachView(this);
}

LutGraphView::~LutGraphView()
{
	if ( _w && _gc ) 
		XtReleaseGC ( _w, _gc );
}

void LutGraphView::displayCallback ( Widget,
				 XtPointer client_data,
				 XtPointer )
{
	LutGraphView *obj;
	obj = ( LutGraphView * ) client_data;
	if (obj != NULL)
		obj->update();
}

void LutGraphView::update ( )
{
	if ( ! XtIsRealized(_w) ) return;

        XSetWindowAttributes attrs;
        attrs.bit_gravity = ForgetGravity;
        XChangeWindowAttributes ( XtDisplay(_w),
                        XtWindow(_w), CWBitGravity, &attrs );

	Colormap cmap = DefaultColormap ( XtDisplay(_w), DefaultScreen ( XtDisplay(_w) ) );
	XColor exact;

        XtVaGetValues ( _w, XmNwidth,   &_width,
                            XmNheight,  &_height,
                            NULL );

	_height--;

	XPoint graph [256];
	double horScale = (double)_width / (double)256;
	double verScale = (double)_height / (double)256;
 
	XClearWindow ( XtDisplay(_w), XtWindow(_w) ); // Always clear the window before drawing

	int x;
	if (_lut)
	{
		for ( x=0; x<256; x++ )
		{
			graph[x].x = (int) (x * horScale);
			graph[x].y = _height - (int) ( (*_lut)[x] * verScale );
		}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "red", &colorR, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	if (_lut1)
	{
        	for ( x=0; x<256; x++ )
        	{
                	graph[x].x = (int) (x * horScale);
                	graph[x].y = _height - (int) ( (*_lut1)[x] * verScale );
        	}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "green", &colorG, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	if (_lut2)
	{
        	for ( x=0; x<256; x++ )
        	{
                	graph[x].x = (int) (x * horScale);
                	graph[x].y = _height - (int) ( (*_lut2)[x] * verScale );
        	}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "blue", &colorB, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	XSetForeground ( XtDisplay(_w), _gc, WhitePixel( XtDisplay(_w), 
				DefaultScreen(XtDisplay(_w)) ) );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lutview.imake
#define SUBROUTINE lutview
#define MODULE_LIST LutAxisView.cc LutHorAxisView.cc LutBox.cc LutBtnInterface.cc \
	LutGraphView.cc LutVerAxisView.cc 
#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
