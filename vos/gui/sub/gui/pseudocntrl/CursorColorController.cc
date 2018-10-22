////////////////////////////////////////////////////////////////
// CursorColorController.cc: This class controlls the color of
//		the bw Swatch (see Swatch in MotifApp library)
///////////////////////////////////////////////////////////////
#include "CursorColorController.h"
#include "CursorModel.h"
#include "BasicWedgeOverlay.h"
#include "ColorModel.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include <Xm/Xm.h>
#include <stdio.h>
#include <stdlib.h>

CursorColorController::CursorColorController (
	Widget colorImage, Widget bwImage,
	ColorModel *colorModel, ColorModel *bwModel,
	BasicWedgeOverlay *wedgeView, 
	PseudoMarks *pseudoMarks, PseudoValue *pseudoValue )
{
	_colorImage 	= colorImage;
	_bwImage	= bwImage;
	_bwModel 	= bwModel;
	_colorModel 	= colorModel;
	_wedgeView 	= wedgeView;
	_pseudoMarks 	= pseudoMarks;
	_pseudoValue 	= pseudoValue;
	_firstTime	= True;

        // Set all callbacks for tracking the cursor
        startTracking();
}

void CursorColorController::startTracking()
{
	// Add input callback to mark and move the points
	XtAddCallback ( _colorImage,
		XvicNinputCallback,
                &CursorColorController::inputCallback, 
		(XtPointer) this);

	XtAddCallback ( _bwImage,
		XvicNinputCallback,
		&CursorColorController::inputCallback,
		(XtPointer) this);

	// Add cursor callback to track the current dn value
	XtAddCallback ( _colorImage,
                XvicNcursorCallback,
                &CursorColorController::cursorCallback,
                (XtPointer) this);

        XtAddCallback ( _bwImage,
                XvicNcursorCallback,
                &CursorColorController::cursorCallback,
                (XtPointer) this);
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user moves the cursor
////////////////////////////////////////////////////////////////////////
void CursorColorController::cursorCallback(Widget, XtPointer clientData,
                                    XtPointer callData)
{
   CursorColorController *obj = (CursorColorController *)clientData;
   obj->cursor(callData);
}

void CursorColorController::cursor(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_CURSOR)
      return;                           // oops

   if ( (cb->x >= 0) && (cb->x <= 255) ) {
	getColorValues( cb->x, _red1, _grn1, _blu1 );
	_colorModel->setRgb ( _red1, _grn1, _blu1 );
	_dn1 = getBWValue( cb->x );
	_bwModel->setRgb ( _dn1, _dn1, _dn1 );
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user takes an action
////////////////////////////////////////////////////////////////////////
void CursorColorController::inputCallback(Widget, XtPointer clientData,
                                    XtPointer callData)
{
   CursorColorController *obj = (CursorColorController *)clientData;
   obj->input(callData);
}

void CursorColorController::input(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_INPUT)
      return;                           // oops

   if (cb->input_num_params != 2)
      return;                           // oops

   if (strcmp(cb->input_params[0], "mark_point") == 0) {
      if (strcmp(cb->input_params[1], "start") == 0) {
	getColorValues( cb->x, _red, _grn, _blu );
	if ( (cb->x >= 0) && (cb->x <= 255) ) {
	   if (_pseudoMarks->addMark(cb->x, 4, _red, _grn, _blu) == 1) {
		_red = _pseudoMarks->getRed();
		_grn = _pseudoMarks->getGrn();
		_blu = _pseudoMarks->getBlu();
	   }
	   _colorModel->setRgb ( _red, _grn, _blu );
	   _dn = getBWValue( cb->x );
	   _bwModel->setRgb ( _dn, _dn, _dn );
	}
      }
      else if (strcmp(cb->input_params[1], "drag") == 0) {
	if ( (cb->x >= 0) && (cb->x < 255) ) {
	   _pseudoMarks->moveMark(cb->x);
	}
      }
      else if (strcmp(cb->input_params[1], "release") == 0) {
	if ( (cb->x >= 0) && (cb->x <= 255) ) {
	   _itype = _pseudoMarks->getInterpolation();
	   _pseudoMarks->addMark(cb->x, 0, _red, _grn, _blu, _itype);
           _pseudoValue->setRedDn(cb->x, _red);
           _pseudoValue->setGrnDn(cb->x, _grn);
           _pseudoValue->setBluDn(cb->x, _blu);
           _colorModel->setRgb ( _red, _grn, _blu );
	   _dn = getBWValue( cb->x );
           _bwModel->setRgb ( _dn, _dn, _dn );
	}
	else 
	   _pseudoMarks->deleteMark();
	   _red = _pseudoMarks->getRed();
	   _grn = _pseudoMarks->getGrn();
	   _blu = _pseudoMarks->getBlu();
	   _colorModel->setRgb ( _red, _grn, _blu );
	   int newdn = _pseudoMarks->getDn(_pseudoMarks->getCurrent());
	   _dn = getBWValue( newdn );
	   _bwModel->setRgb ( _dn, _dn, _dn );
      }
   }

   if (strcmp(cb->input_params[0], "mark_interval") == 0) {
        if (strcmp(cb->input_params[1], "start") == 0) {
          _pseudoMarks->markInterval(cb->x);
        }
        else if (strcmp(cb->input_params[1], "drag") == 0) {
          _pseudoMarks->resizeInterval(cb->x);
        }
   }

}

void CursorColorController::getColorValues(int pos, int &red, int &grn, int &blu)
{
	int r[256], g[256], b[256];

	XvicImageGetColorLUT ( _colorImage, r, g, b );

        red = r[pos];
	grn = g[pos];
	blu = b[pos];
}

int CursorColorController::getBWValue(int x)
{
	return x;
}
