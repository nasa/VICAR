////////////////////////////////////////////////////////////////
// WedgeView.h
////////////////////////////////////////////////////////////////
#ifndef WEDGEVIEW_H
#define WEDGEVIEW_H
#include "BasicWedgeView.h"	// the base class for wedge views
#include "GuiDefs.h"
#include "XvicImage.h"

#ifndef MIN
#define MIN(x,y)	(((x) < (y)) ? (x) : (y))
#endif

class WedgeView : public BasicWedgeView {

  private:

	static XtResource _resources[];

	static  void exposeCallback ( Widget , 
				      XtPointer client_data, 
				      XtPointer call_data );
	static  void resizeCallback ( Widget , 
				      XtPointer client_data, 
				      XtPointer call_data );

  protected:

	Widget		_iw; 			 // Image widget, child of Scrolled Window
	int		_buffer_startLine;		// startLine number from most recent read;
	int		_buffer_startSample;	// startSample number from most recent read;
	OrientType	_orientation;
	int		_minPixelDN;
	int		_maxPixelDN;
	int		_numberOfSteps;
	XvicImageData 	_img;
	

	virtual void createWidget(Widget parent, int _view_height, int _view_width );
	virtual void resize( XvicImageCallbackStruct *  cb);
	virtual void expose( XvicImageCallbackStruct * cb);
	virtual void createBuffer();
	virtual void setWidgetStruct(XvicImageCallbackStruct * cb);

  public:

	WedgeView(Widget parent, const char * name,
				int view_height = 0, int view_width = 0 );
	~WedgeView() {XtDestroyWidget(_iw);  if (_img.bw_pixels) delete _img.bw_pixels;};

	Widget getWidget() { return _iw; };
	XvicImageData getImageData() { return _img; };

	virtual void setOrientation( OrientType  o = HORIZONTAL ) { _orientation = o; }
	virtual OrientType  getOrientation() { return _orientation; }

	virtual void setMinPixelValue( int min ) { _minPixelDN = min; }
	virtual int  getMinPixelValue() { return _minPixelDN; }

	virtual void setMaxPixelValue( int max ) { _maxPixelDN = max;  }
	virtual int  getMaxPixelValue() { return _maxPixelDN; }

	virtual void setNumberOfSteps( int nsteps ) { _numberOfSteps = nsteps; }
	virtual int  getNumberOfSteps() { return _numberOfSteps; }

	virtual const char *const className() { return ("WedgeView"); }
};
#endif
