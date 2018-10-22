////////////////////////////////////////////////////////////////
// CursorColorController.h
//
///////////////////////////////////////////////////////////////
#ifndef CURSORCOLORCONTROLLER_H
#define CURSORCOLORCONTROLLER_H
#include <Xm/Xm.h>
#include "XvicImage.h"
#include "PseudoDefs.h"		// Only for InterpolationType

class ColorModel;
class BasicWedgeOverlay;
class PseudoMarks;
class PseudoValue;

class CursorColorController {

  private:

	Boolean _firstTime;

  protected:

	Widget		 _colorImage, _bwImage;
	ColorModel 	 *_colorModel, *_bwModel;
	BasicWedgeOverlay *_wedgeView;
	PseudoMarks 	 *_pseudoMarks;
	PseudoValue 	 *_pseudoValue;

	int _current;

	int _red, _grn, _blu, _dn;
	int _red1, _grn1, _blu1, _dn1;	// for button-up state
	InterpolationType _itype;

	static void inputCallback( Widget,
			XtPointer clientData,
			XtPointer callData);

	static void cursorCallback( Widget,
			XtPointer clientData,
			XtPointer callData);

	virtual void startTracking();

	virtual void input(XtPointer);
	virtual void cursor(XtPointer);

	virtual void getColorValues( int, int&, int&, int& );  // get RGB values
	virtual int getBWValue( int ); 

  public:

	CursorColorController ( Widget, Widget,
		ColorModel *, ColorModel *,
		BasicWedgeOverlay *, 
		PseudoMarks *, PseudoValue * );
  virtual ~CursorColorController() {}
};
#endif
