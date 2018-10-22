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
