$!****************************************************************************
$!
$! Build proc for MIPL module pseudoview
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:32
$!
$! Execute by entering:		$ @pseudoview
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
$ write sys$output "*** module pseudoview ***"
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
$ write sys$output "Invalid argument given to pseudoview.com file -- ", primary
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
$   if F$SEARCH("pseudoview.imake") .nes. ""
$   then
$      vimake pseudoview
$      purge pseudoview.bld
$   else
$      if F$SEARCH("pseudoview.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pseudoview
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pseudoview.bld "STD"
$   else
$      @pseudoview.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudoview.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudoview.com -mixed -
	-s PseudoCmdInterface.cc PseudoColorView.cc WedgeOverlayView.cc -
	   InterpolationChooser.cc DnValueView.cc MarksToValueGlue.cc -
	   ValueToMarksGlue.cc MarksToColorGlue.cc ImageToPseudoGlue.cc -
	-i pseudoview.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// PseudoCmdInterface.cc: Fills pseudocolor LUT 
//////////////////////////////////////////////////////////////
#include "PseudoCmdInterface.h"
#include "Cmd.h"
#include "CmdList.h"
#include "PseudoValue.h"
#include "ColorModel.h"
#include "SwatchView.h"
#include "PseudoRGBController.h"
#include "WedgeView.h"
#include "MarksToValueGlue.h"
#include "ValueToMarksGlue.h"
#include "PseudoColorView.h"
#include "CursorModel.h"
#include "CursorColorController.h"
#include "PseudoMarks.h"
#include "DnValueView.h"
#include "RGBView.h"
#include "InterpolationChooser.h"
#include "MarksToColorGlue.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include "xvmaininc.h"		// for UNUSED

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
PseudoCmdInterface::PseudoCmdInterface ( Widget parent, Cmd *cmd, 
		PseudoValue *value, PseudoMarks *marks, Widget iw )
	: CmdInterface ( cmd )
{
   _iw = iw;

   _w = XtVaCreateWidget(_name, 
		xmFormWidgetClass, parent, 
		NULL );
   installDestroyHandler();

   /******************************************************************
   * Declare all models
   ******************************************************************/

   // Model for LUTs 
   _pseudoValue = value;

   // Model for RGB values (from MotifApp)
   ColorModel *bwModel    = new ColorModel;
   ColorModel *colorModel = new ColorModel;

   // Model for mark values 
   ColorModel *markBwModel    = new ColorModel;
   ColorModel *markColorModel = new ColorModel;

   /******************************************************************
   * Declare all views
   ******************************************************************/

   // Display a color swatch to reflect color model (from MotifApp)
   SwatchView *bwSwatch    = new SwatchView (_w, "bwSwatch");
   SwatchView *colorSwatch = new SwatchView (_w, "colorSwatch");

   // Glue class reacts to any change in marks by changing the pseudocolor value and vice versa
   BasicWedgeOverlay *UNUSED(marksToValueGlue) = new MarksToValueGlue (_w, 
		"marksToValueGlue", marks, _pseudoValue, this );
   BasicWedgeOverlay *UNUSED(valueToMarksGlue) = new ValueToMarksGlue (_w,
                "valueToMarksGlue", marks, _pseudoValue, this );


   // Create color representation of IN and OUT values
   _ps    = new PseudoColorView (_w, "pseudoColor", _pseudoValue);
   _wedge = new WedgeOverlayView (_w, "wedge");
   _pseudoValue->attachView(_ps);
   marks->attachView(_wedge);
   marks->attachView(_ps);

   // Display numerical value of current IN parameter
   ColorView *dnValueView = new DnValueView (_w, "dnValueView");
   bwModel->attachView(dnValueView);
   ColorView *textColorView = new RGBView (_w, "RGBView");
   colorModel->attachView(textColorView);

   Widget markDataFrame =  XtVaCreateManagedWidget("markFrame",
                xmFrameWidgetClass, _w,
                NULL );
   XtVaCreateManagedWidget ("markFrameLabel",
                xmLabelGadgetClass, markDataFrame,
                XmNchildType, XmFRAME_TITLE_CHILD,
                XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                NULL );
   Widget markDataForm =  XtVaCreateManagedWidget("markForm",
                xmFormWidgetClass, markDataFrame,
                NULL );

   // Create 3 sliders for editing mark values (from MotifApp)
   RGBController *rgbSliders = new PseudoRGBController (markDataForm, 
			markColorModel, "rgbConroller", marks);

   // Create color swatches for editing mark values
   SwatchView *markBwSwatch    = new SwatchView (markDataForm, "markBwSwatch");
   SwatchView *markColorSwatch = new SwatchView (markDataForm, "markColorSwatch");

   CursorModel *cursorBW = new CursorModel ( True, _wedge->getWidget() );
   CursorModel *cursorColor = new CursorModel ( True, _ps->getWidget() );

   // Change color model when current mark moves/created
   Widget colorImage = cursorColor->getImageWidget();
   Widget bwImage    = cursorBW->getImageWidget();
   CursorColorController *UNUSED(cursorColorView) = new CursorColorController 
		( colorImage, bwImage, colorModel, bwModel, _ps, marks, _pseudoValue );

   bwModel->attachView(bwSwatch);
   colorModel->attachView(colorSwatch);

   markColorModel->attachView(rgbSliders);
   markColorModel->attachView(markColorSwatch);
   markBwModel->attachView(markBwSwatch);

   ///////////////////////////////////////////////////////////
   // Component that chooses interpolation between two marks
   Widget ipFrame =  XtVaCreateManagedWidget("interpolationFrame",
                xmFrameWidgetClass, _w,
                NULL );
   XtVaCreateManagedWidget ("interpolationFrameLabel",
                xmLabelGadgetClass, ipFrame,
                XmNchildType, XmFRAME_TITLE_CHILD,
                XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                NULL );
   InterpolationChooser *interpolationChooser = new InterpolationChooser( ipFrame,
                "interpolationChooser", NONE, _pseudoValue, marks, this );
   marks->attachView(interpolationChooser);

   InterpolationChooser *marksToColorGlue = new MarksToColorGlue ( _w,
		"glue", NONE, _pseudoValue, marks, this,
		markBwModel, markColorModel );
   marks->attachView(marksToColorGlue);

   XtVaSetValues ( dnValueView->baseWidget(),
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( textColorView->baseWidget(),
                XmNleftAttachment,   XmATTACH_WIDGET,
		XmNleftWidget,	     dnValueView->baseWidget(),
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( bwSwatch->baseWidget(),
		XmNwidth,  50,
		XmNleftAttachment,   XmATTACH_WIDGET,
		XmNleftWidget,	     _wedge->baseWidget(),
		XmNleftOffset,       10,
		XmNrightAttachment,  XmATTACH_NONE,
		XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        textColorView->baseWidget(),
		XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNbottomWidget,     _wedge->baseWidget(),
		NULL );

   XtVaSetValues ( colorSwatch->baseWidget(),
		XmNwidth,  50,
                XmNleftAttachment,   XmATTACH_WIDGET,
		XmNleftWidget,	     _ps->baseWidget(),
		XmNleftOffset,	     10,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget,	     _ps->baseWidget(),
                XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNbottomWidget,     _ps->baseWidget(),
                NULL );

   XtVaSetValues ( _wedge->baseWidget(),
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        textColorView->baseWidget(),
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( _ps->baseWidget(),
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        _wedge->baseWidget(),
		XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( markDataFrame,
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        _ps->baseWidget(),
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( rgbSliders->baseWidget(),
                XmNleftAttachment,   XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_FORM,
                NULL );

   XtVaSetValues ( markBwSwatch->baseWidget(),
                XmNwidth,  50,
		XmNheight, 50,
                XmNleftAttachment,   XmATTACH_WIDGET,
                XmNleftWidget,       rgbSliders->baseWidget(),
                XmNleftOffset,       10,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( markColorSwatch->baseWidget(),
                XmNwidth,  50,
		XmNheight, 50,
                XmNleftAttachment,   XmATTACH_WIDGET,
                XmNleftWidget,       rgbSliders->baseWidget(),
                XmNleftOffset,       10,
                XmNrightAttachment,  XmATTACH_NONE,
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        markBwSwatch->baseWidget(),
		XmNtopOffset,	     10,
                XmNbottomAttachment, XmATTACH_NONE,
                NULL );

   XtVaSetValues ( ipFrame,
                XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, markDataFrame,
                XmNleftAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_NONE,
                XmNbottomAttachment, XmATTACH_FORM,
                NULL );

   // Load the initial pseudocolor value
   loadTable(_pseudoValue);

   // Set the "main" image widget
   XtAddCallback(iw, XvicNvisibleAreaCallback,
                &PseudoCmdInterface::imageWidgetChangeCallback, 
		(XtPointer) this);
   copyDisplayModeResources();

   colorSwatch->manage();
   bwSwatch->manage();
   _wedge->manage();
   _ps->manage();
   rgbSliders->manage();
   markBwSwatch->manage();
   markColorSwatch->manage();
   dnValueView->manage();
   textColorView->manage();
   interpolationChooser->manage();
}

////////////////////////////////////////////////////////////////////////
// Callback from the image widget to let us know something changed.
////////////////////////////////////////////////////////////////////////

void PseudoCmdInterface::imageWidgetChangeCallback(Widget, XtPointer clientData,
                                              XtPointer callData)
{
   PseudoCmdInterface *obj = (PseudoCmdInterface *)clientData;

   obj->imageWidgetChange(callData);
}

////////////////////////////////////////////////////////////////////////
// Something changed in the image widget.  Deal with it.
////////////////////////////////////////////////////////////////////////

void PseudoCmdInterface::imageWidgetChange(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if ((cb->reason == XvicCR_VISIBLE_AREA) && (cb->flags & XvicDITHER_CHANGED))
   {
	copyDisplayModeResources();
   }
}

void PseudoCmdInterface::copyDisplayModeResources()
{
   unsigned char dither;
   unsigned char colormap_policy, bwVisualType, pseudoVisualType;
   int redLevels, grnLevels, bluLevels;

   // Check if image widget is in pseudomode
   unsigned char lutType;
   XtVaGetValues(_iw,
		XvicNlutType, &lutType, 
		NULL);
   if (lutType != XvicPSEUDO) return;

   XtVaGetValues(_iw,
                XvicNditherMode, &dither,
                XvicNcolormapPolicy, &colormap_policy,
		XvicNredLevels, &redLevels,
		XvicNgreenLevels, &grnLevels,
		XvicNblueLevels, &bluLevels,
		XvicNbwVisualType, &bwVisualType, 
		XvicNpseudoVisualType, &pseudoVisualType,
                NULL);

   XtVaSetValues(_ps->getWidget(),
		XvicNbwVisualType, bwVisualType,
                XvicNpseudoVisualType, pseudoVisualType,
                XvicNpseudoDither, dither,
                XvicNcolormapPolicy, colormap_policy,
		XvicNredLevels, redLevels,
                XvicNgreenLevels, grnLevels,
                XvicNblueLevels, bluLevels,
                NULL);


   switch (dither) {
	case XvicNONE:
	break;

	case XvicORDERED:
	    XtVaSetValues(_wedge->getWidget(),
		XvicNbwDither, XvicORDERED,
		NULL);
	break;

	case XvicKAGELS:
	    XtVaSetValues(_wedge->getWidget(),
                XvicNbwDither, XvicORDERED,
                NULL);
   }

   switch (colormap_policy) {
        case XvicFULL:
	    XtVaSetValues(_wedge->getWidget(),
                XvicNcolormapPolicy, XvicFULL,
                NULL);
	break;
	case XvicHALF:
	    XtVaSetValues(_wedge->getWidget(),
                XvicNcolormapPolicy, XvicALLOC,
		XvicNgrayLevels, 8, 
                NULL);
	break;
	case XvicDITHER:
	    XtVaSetValues(_wedge->getWidget(),
                XvicNcolormapPolicy, XvicDITHER,
		XvicNgrayLevels, 8,
                NULL);
	break;
	case XvicALLOC:
	    XtVaSetValues(_wedge->getWidget(),
                XvicNcolormapPolicy, XvicALLOC,
                NULL);
	break;
   }
}

//////////////////////////////////////////////////////////////
// Actually run the command by creating a dynamically allocated
// copy of the stretchValue (because that's what Cmd likes).
//////////////////////////////////////////////////////////////
void PseudoCmdInterface::loadTable(PseudoValue *pseudoValue)
{
   runCmd( (CmdValue *) new PseudoValue(*pseudoValue) );
}

//////////////////////////////////////////////////////////////
// If any of the pseudocolor values changes, create a PseudoValue object
// and execute the command.
//////////////////////////////////////////////////////////////
void PseudoCmdInterface::executeCmd(XtPointer)
{
   PseudoValue *pseudoValue = new PseudoValue(*_pseudoValue);
   runCmd((CmdValue)pseudoValue);
}

//////////////////////////////////////////////////////////////
// Update the interface to match a given value
//////////////////////////////////////////////////////////////
void PseudoCmdInterface::setValue(CmdValue value)
{
   CmdInterface::setValue(value);		// Removes cmd from deferred list

   if (value != NULL)				// NULL means use saved default
      *_pseudoValue = *(PseudoValue *)value;	// Copy to version views use
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoColorView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// PseudoColorView.cc
////////////////////////////////////////////////////////////////
#include "PseudoColorView.h"
#include "PseudoValue.h"
#include "ColorModel.h"

PseudoColorView::PseudoColorView( Widget parent, const char *name, 
	PseudoValue *pseudoValue) :  WedgeOverlayView(parent, name)
{
	_pseudoValue = pseudoValue;

	XtVaSetValues(_iw, 
		XvicNlutType, XvicPSEUDO_ONLY,
		NULL);
	XvicImageSetColorLUT(_iw, 
		_pseudoValue->getRedAsArray(),
		_pseudoValue->getGrnAsArray(),
		_pseudoValue->getBluAsArray());
}

///////////////////////////////////////////////////////////////
//      update
///////////////////////////////////////////////////////////////
void PseudoColorView::update (PseudoValue *value)
{
	_pseudoValue = value;
	XvicImageSetColorLUT(_iw,
                _pseudoValue->getRedAsArray(),
                _pseudoValue->getGrnAsArray(),
                _pseudoValue->getBluAsArray());
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WedgeOverlayView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// WedgeOverlayView.cc
////////////////////////////////////////////////////////////////
#include "WedgeOverlayView.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "WedgeView.h"
#include <Xm/Frame.h>

XtResource WedgeOverlayView::_resources [ ] = {
 {
    (char *)"markColor",
    (char *)"MarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( WedgeOverlayView *, _markColor ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"selectedMarkColor",
    (char *)"SelectedMarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( WedgeOverlayView *, _selectedMarkColor ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"markLength",
    (char *)"MarkLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _markLength ),
    XmRImmediate,
    ( XtPointer ) 20,
 },
  {
    (char *)"markThickness",
    (char *)"MarkThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _markThickness ),
    XmRImmediate,
    ( XtPointer ) 1,
 },
  {
    (char *)"barThickness",
    (char *)"BarThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _barThickness ),
    XmRImmediate,
    ( XtPointer ) 10,
 },
};

WedgeOverlayView::WedgeOverlayView( Widget parent, const char * name )
		: BasicWedgeOverlay(name)
{
	_w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
	installDestroyHandler();

	getResources ( _resources, XtNumber ( _resources ) );

	WedgeView *wedge = new WedgeView(_w, "wedgeView");
	wedge->manage();
	_iw = wedge->getWidget();

	XtVaSetValues(_iw,
                XvicNlutType, XvicRAW,
                NULL);

	// Set color and GC for graphics overlay
        XColor xcolor;
        XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
                _markColor, &xcolor);
        _lineColor = XvicImageGetGrColor(_iw, &xcolor);

        XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
                _selectedMarkColor, &xcolor);
        _curLineColor = XvicImageGetGrColor(_iw, &xcolor);

        // Set GC for graphics overlay
        XGCValues values;
        values.line_width = _markThickness;
        _lineGC=XvicImageCreateGC(_iw, GCLineWidth, &values);
}

///////////////////////////////////////////////////////////////
//	update 
///////////////////////////////////////////////////////////////
void WedgeOverlayView::update (PseudoMarks *marks)
{
	int numMarks = marks->getNumMarks();
	int *m = marks->getDnAsArray();
	if (!m) return;                      // oops
	XvicImageEraseOverlay(_iw);
	for (int i = 0; i < numMarks; i++) {
	   if ( i != marks->getCurrent()-1 )
		XvicImageDrawLine(_iw, 0, _lineGC, _lineColor, 
			m[i], 0, m[i], _markLength);
	   else 
		XvicImageDrawLine(_iw, 0, _lineGC, _curLineColor,
			m[i], 0, m[i], _markLength);
	}
	int start = marks->getStart();
	int end = marks->getEnd();
	int start1 = marks->getStart1();
	int length = m[end] - m[start];
	if (length >= 0)
	    XvicImageFillRectangle(_iw, 0, _lineGC, _curLineColor, 
		m[start], 0, length, _barThickness); 
	else {
	    length = m[start1] - m[end];
	    XvicImageFillRectangle(_iw, 0, _lineGC, _curLineColor,
		m[start1]-length, 0, length, _barThickness);
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create InterpolationChooser.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// InterpolationChooser.cc: A component class to choose the interpolation
// on the interval.  The choices are No Interpolation, 
// Flat, Linear, Cubic Spline.  Only one option can be chosen
///////////////////////////////////////////////////////
#include "InterpolationChooser.h"
#include "PseudoValue.h"
#include "PseudoMarks.h"
#include "PseudoCmdInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

InterpolationChooser::InterpolationChooser(Widget parent, const char *name,
	InterpolationType type, PseudoValue *pseudoValue, PseudoMarks *marks,
	PseudoCmdInterface *pseudoCmdInterface)
	: UIComponent(name)
{
   _pseudoValue = pseudoValue;
   _pseudoMarks = marks;
   _type = type;
   _pseudoCmdInterface = pseudoCmdInterface;

   _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
		NULL );
   installDestroyHandler();

   _none = XtVaCreateManagedWidget("none", xmToggleButtonWidgetClass, _w,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, True,
		NULL);
   _flat = XtVaCreateManagedWidget("flat", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   _linear = XtVaCreateManagedWidget("linear", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   _cubs = XtVaCreateManagedWidget("cubs", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   XtSetSensitive(_cubs, False);	//!!!! temporary!

   valueChanged();

   XtAddCallback(_none, XmNvalueChangedCallback,
		&InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_flat, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_linear, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_cubs, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
   InterpolationChooser *obj = (InterpolationChooser *)clientData;

   obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the pseudocolor value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::valueChanged()
{
	// Set selected type for each of the mark on the interval
	if (XmToggleButtonGetState(_linear)) {
		_pseudoMarks->setInterpolation (LINEAR);
	}

	if (XmToggleButtonGetState(_flat)) {
		_pseudoMarks->setInterpolation (FLAT);
	}

	if (XmToggleButtonGetState(_none)) {
		_pseudoMarks->setInterpolation (NONE);
	}

	if (XmToggleButtonGetState(_cubs)) {
	   // Empty
	}

	_pseudoValue->regenerate(_pseudoMarks);

	_pseudoCmdInterface->loadTable(_pseudoValue);
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::setValue(InterpolationType type)
{
   if (type == NONE)			// We're it
      XmToggleButtonSetState(_none, True, False);
   else					// We're not it
      XmToggleButtonSetState(_none, False, False);

   if (type == FLAT)                     // We're it
      XmToggleButtonSetState(_flat, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_flat, False, False);

   if (type == LINEAR)                     // We're it
      XmToggleButtonSetState(_linear, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_linear, False, False);

   if (type == CUBS)                     // We're it
      XmToggleButtonSetState(_cubs, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_cubs, False, False);
}

void InterpolationChooser::update(PseudoMarks *marks)
{
   setValue(marks->getInterpolation());
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DnValueView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// DnValueView.cc: A display for numeric position value on bw scale(0-255)
///////////////////////////////////////////////////////////////
#include "DnValueView.h"
#include "KeyinView.h"
#include "ColorModel.h"
#include "stdio.h"
#include <Xm/RowColumn.h>

DnValueView::DnValueView (Widget parent, const char *name) : ColorView (name)
{
  _w = XtCreateWidget(_name, 
		xmRowColumnWidgetClass, parent, 
		NULL, 0 );
  installDestroyHandler();

  _position = new KeyinView(_w, name);
  _position->manage();
  _position->setFieldValue((char *)"0");
}

void DnValueView::update (ColorModel *color)
{
  char buf[100];
  sprintf(buf, "%3.3d", color->red());
  _position->setFieldValue ( buf );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MarksToValueGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// MarksToValueGlue: class that serves as a "glue" class between an
// PseudoMarks object and a PseudoValue object.  The class is a
// View to PseudoMarks, so whenever it receives an update() from PseudoMarks,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "MarksToValueGlue.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "PseudoCmdInterface.h"

MarksToValueGlue::MarksToValueGlue(Widget, const char *name, PseudoMarks *marks,
        PseudoValue *value, PseudoCmdInterface *pci) : BasicWedgeOverlay (name)
{
   _value = value;
   _pseudoCmdInterface = pci;

   _collectionActive = NULL;

   // This should be done by WedgeOverlayView!!!!
   marks->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the marks change,
// regenerate the pseudocolor tables.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void MarksToValueGlue::update(PseudoMarks *marks)
{
   if (_value) {
        _value->regenerate(marks);
        _pseudoCmdInterface->loadTable(_value);
   }
}

void MarksToValueGlue::update(PseudoValue *value)
 {
   value=0;  // Empty. Assigned to 0 to remove compiler warnings
 }
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ValueToMarksGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ValueToMarksGlue: Class that serves as a "glue" class between an
// Pseudovalue object and a PseudoMarks object.  The class is a
// View to PseudoValue, so whenever it receives an update() from PseudoValue,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ValueToMarksGlue.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "PseudoCmdInterface.h"

ValueToMarksGlue::ValueToMarksGlue(Widget, const char *name, PseudoMarks *marks,
	PseudoValue *value, PseudoCmdInterface *pci) : BasicWedgeOverlay (name)
{
   _marks = marks;
   _pseudoCmdInterface = pci;

   _collectionActive = NULL;

   value->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the marks change,
// regenerate the pseudocolor tables.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ValueToMarksGlue::update(PseudoValue *value)
{
   if (value) {
	// _marks->regenerate(marks);
        _pseudoCmdInterface->loadTable(value);
   }
}

void ValueToMarksGlue::update(PseudoMarks *marks)
{
	marks=0;  //assigned to 0 to elim. compiler warnings
	// WedgeOverlayView::update(marks);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MarksToColorGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// MarksToColorGlue.cc:
///////////////////////////////////////////////////////
#include "MarksToColorGlue.h"
#include "PseudoMarks.h"
#include "ColorModel.h"

void MarksToColorGlue::update(PseudoMarks *marks)
{
	_colorModel->setRgb(marks->getRed(), marks->getGrn(), marks->getBlu());
	_bwModel->setRgb(marks->getDn(), marks->getDn(), marks->getDn());
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToPseudoGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToPseudoGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Pseudocolor objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it disables the pseudocolor tool if the image is color and enables it
// if the image is b&w.  This class, even though it's a UIComponent,
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToPseudoGlue.h"
#include "Cmd.h"
#include "MenuDialog.h"
#include "ImageData.h"

ImageToPseudoGlue::ImageToPseudoGlue (ImageData *model,
				      Cmd *modeCmd, MenuDialog *dialog, 
				      Cmd *pseudoCmd, Cmd *postLutCmd, 
				      Cmd *postPseudoCmd)
    : BasicImageView("glue", model)
{
    _pseudoModeCmd = modeCmd;
    _pseudoDialog = dialog;
    _pseudoCmd = pseudoCmd;
    _postLutCmd = postLutCmd;
    _postPseudoCmd = postPseudoCmd;

    _model->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the image changes,
// recompute the histogram.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ImageToPseudoGlue::update()
{
    if ( (_model->isDataSourceOpened()) && (_model->getMode() == BWmode) ) {
	_pseudoModeCmd->activate();
	_pseudoCmd->activate();
	_postLutCmd->activate();
	_postPseudoCmd->activate();
    }
    else {		// Deactivate all the commands and pop-down dialog
	_pseudoModeCmd->execute((CmdValue)False);
	_pseudoModeCmd->deactivate();
	_pseudoDialog->unpost();
	_pseudoCmd->deactivate();
	_postLutCmd->deactivate();
	_postPseudoCmd->deactivate();
    }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pseudoview.imake
#define SUBROUTINE pseudoview
#define MODULE_LIST PseudoColorView.cc DnValueView.cc \
 InterpolationChooser.cc WedgeOverlayView.cc \
 PseudoCmdInterface.cc \
 MarksToValueGlue.cc ValueToMarksGlue.cc MarksToColorGlue.cc \
 ImageToPseudoGlue.cc 

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_P2SUB
$ Return
$!#############################################################################
