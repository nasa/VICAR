////////////////////////////////////////////////////////////////
// WedgeView.cc
//	Note 
//		(1) that this view has no model!
//		(2) registers orientation converter. - old style 
////////////////////////////////////////////////////////////////
#include "WedgeView.h"
#include "WedgeResourceTypes.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#ifndef NO_XMU
  #if defined(vms) || defined(__VMS)
    extern "C" {
      #include <Xmu/Converters.h>
    }
  #else
    #include <X11/Xmu/Converters.h>
  #endif
#endif

///////////////////////////////////////////////////////////////
//	Synthetic resources
///////////////////////////////////////////////////////////////
XtResource WedgeView::_resources [ ] = {
{
    XtNorientation,
    XtCOrientation,
    XtROrientation,
    sizeof(XtOrientation),
    XtOffset(WedgeView *, _orientation),
    XmRString,
    (XtPointer)"HORIZONTAL",
 },
 { 
    (char *)"nsteps",
    (char *)"Nsteps",
    XmRInt,
    sizeof(int),
    XtOffset(WedgeView *, _numberOfSteps),
    XmRString,
    (XtPointer)"256",
 },
 {
    (char *)XvicNminPixelDN,
    (char *)XvicCMinPixelDN,
    XmRInt,
    sizeof(int),
    XtOffset(WedgeView *, _minPixelDN),
    XmRString,
    (XtPointer)"0",
 },
 {
    (char *)XvicNmaxPixelDN,
    (char *)XvicCMaxPixelDN,
    XmRInt,
    sizeof(int),
    XtOffset(WedgeView *, _maxPixelDN),
    XmRString,
    (XtPointer)"255",
 },
};

///////////////////////////////////////////////////////////////
//	Constructor
//		Set view_height & 
//		view_width only if these
//		values should override
//		resource file.
///////////////////////////////////////////////////////////////
WedgeView::WedgeView(Widget parent, const char *name, 
		     int view_height, int view_width)
    :  BasicWedgeView(name)
{
    // Initialize variables

    _img.bw_pixels = NULL;
    _img.red_pixels = NULL;
    _img.grn_pixels = NULL;
    _img.blu_pixels = NULL;
    _img.memory_control = XvicMEMORY_APPLIC;
    _img.height = 0;
    _img.width =  0;	
    _img.x = 0;
    _img.y = 0;
    _img.line_width = 0;
    _img.start_offset = 0;
    _iw = NULL;

    // Add conversion routine to handle orientation
    // (old style converter)

#ifndef NO_XMU
    XtAddConverter(XmRString,
		   XtROrientation,
		   (XtConverter)XmuCvtStringToOrientation, 
		   (XtConvertArgList) NULL, 
		   0);
#endif

    // Create image widget (& it's parent scrolled window)

    createWidget(parent, view_height, view_width);

    // Get resources (routine  in UIComponent)

    getResources(_resources, XtNumber(_resources));
}

///////////////////////////////////////////////////////////////
//      Create Widget ..
//              private routine. Called
//              by constructor.
///////////////////////////////////////////////////////////////
void WedgeView::createWidget(Widget parent, int view_height, int view_width)
{
    Arg args[20];
 
    // Set view/image size (if not 0)
 
    int n = 0;
 
    if (view_height != 0) {
        XtSetArg(args[n],
                 (char *)XvicNviewHeight, (Dimension) view_height);
        n++;
        XtSetArg(args[n],
               (char *)XvicNimageHeight, view_height); // image size = view size
        n++;
    }
 
    if (view_width != 0) {
        XtSetArg(args[n],
                 (char *)XvicNviewWidth, (Dimension) view_width);
        n++;
        XtSetArg(args[n],
		 (char *)XvicNimageWidth, view_width); // image size = view size
        n++;
    }
 
    XtSetArg(args[n], (char *)XvicNimageMode, XvicBW);
    n++;
 
    // Create scrolled window widget and it's image widget child
    // _iw is image widget chid
 
    _iw = XvicCreateImage(parent, _name, args, n);
    if (_iw) {
        _w = XtParent(_iw);
 
        // Manage iw here p311 in Vol 6A does this with scrolled text windows
 
        XtManageChild(_iw);
    }
 
    // Add callbacks
 
    if (_iw) {
        XtAddCallback(_iw,
                      XvicNexposeCallback,
                      &WedgeView::exposeCallback,
                      (XtPointer)this);
        XtAddCallback(_iw,
		      XvicNresizeCallback,
		      &WedgeView::resizeCallback,
		      (XtPointer) this );
        installDestroyHandler ();
    }
}

///////////////////////////////////////////////////////////////
//	Resize Callback
///////////////////////////////////////////////////////////////
void WedgeView::resizeCallback(Widget, XtPointer client_data, 
			       XtPointer call_data )
{
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
    WedgeView *view = (WedgeView *)client_data;
    view->resize( cb );
}

///////////////////////////////////////////////////////////////
//	Resize  
//		Called by resizeCallback
//		to  window
///////////////////////////////////////////////////////////////
void WedgeView::resize( XvicImageCallbackStruct * cb )
{
    // Get size of parent form

    Dimension width, height;
    XtVaGetValues(XtParent(_w), 
		  XmNwidth, &width,
		  XmNheight, &height,
		  NULL);

    // Set size in image widget

    XtVaSetValues(_w,
		  XvicNviewWidth, width, 
		  XvicNviewHeight, height,
		  XvicNimageWidth, (int)width, 
		  XvicNimageHeight, (int)height,
		  NULL);
    XtVaSetValues(_iw,
		  XvicNviewWidth, width, 
		  XvicNviewHeight, height,
		  XvicNimageWidth, (int) width, 
		  XvicNimageHeight, (int) height,
		  NULL);

    // Create buffer of wedge data

    createBuffer();

    // Fill in structure

    setWidgetStruct(cb);
   
    // Draw wedge in widget

    XvicImageWrite(_iw, &_img, False);
}

///////////////////////////////////////////////////////////////
//	Expose Callback
///////////////////////////////////////////////////////////////
void WedgeView::exposeCallback(Widget, 
			       XtPointer client_data, 
			       XtPointer call_data )
{
    WedgeView *view = (WedgeView *)client_data;
    XvicImageCallbackStruct *cb =(XvicImageCallbackStruct *)call_data;
    view->expose(cb);
}

///////////////////////////////////////////////////////////////
//	Expose
//		Called by exposeCallback
//		to redraw window
///////////////////////////////////////////////////////////////
void WedgeView::expose(XvicImageCallbackStruct * cb)
{
    // Create buffer of wedge data
    
    createBuffer();

    // Fill in structure

    setWidgetStruct(cb);
   
    // Draw wedge in widget

    XvicImageWrite(_iw, &_img, False);
}

///////////////////////////////////////////////////////////////
//	setWidgetStruct (& MISC INFO)
///////////////////////////////////////////////////////////////
void WedgeView::setWidgetStruct(XvicImageCallbackStruct * cb)
{
    int width, height;

//	SET CONSTANTS IN IMG STRUCT
    _img.start_offset = 0;
    _img.memory_control = XvicMEMORY_APPLIC;

//	GET HEIGHT,WIDTH
    XtVaGetValues( _iw,
		   XvicNtileWidth, &width, 
		   XvicNtileHeight, &height,
		   NULL);
    _img.width = width;
    _img.height = height;
    
//	GET LINE_WIDTH
    if (_orientation == HORIZONTAL ) 
	_img.line_width = _img.width;
    else 
	_img.line_width = 1;
    
//	GET NEXT LINE & SAMPLE, & START OFFSET
    _img.x	= cb->x;
    _img.y	= cb->y;
}

///////////////////////////////////////////////////////////////
//	CREATE BUFFER OF WEDGE DATA
//	(STUCT MUST BE SETUP FOR THIS TO WORK)
///////////////////////////////////////////////////////////////
void WedgeView::createBuffer()
{
    int	bufferSize;
    double curstep, stepsize;
    double curdnstep, dnstepsize;
    int pix;	
    
    // Delete previous buffer

    if (_img.bw_pixels != NULL )  
	delete _img.bw_pixels;
    
    // Calculate buffer size from view size

    Dimension view_width, view_height;  
    XtVaGetValues(_iw,
		  XvicNviewWidth, &view_width, 
		  XvicNviewHeight, &view_height,
		  NULL);
    if (_orientation == HORIZONTAL ) 
	bufferSize = (int) view_width;
    else 
	bufferSize = (int) view_height;
    
    // Set tile size 

    if (_orientation == HORIZONTAL ) 
	XtVaSetValues(_iw,
		      XvicNtileWidth, bufferSize, 
		      XvicNtileHeight, 1,
		      NULL);
    else 
	XtVaSetValues(_iw,
		      XvicNtileWidth, 1, 
		      XvicNtileHeight, bufferSize,
		      NULL);
    
    // Create buffer (for 1 dimension)

    _img.bw_pixels = new unsigned char[bufferSize ];
    
    // Get stepsize & dnstepsize

    if (_numberOfSteps <= 1) {
	stepsize = (_maxPixelDN-_minPixelDN);
	dnstepsize = (_maxPixelDN-_minPixelDN);
    }
    else {
	stepsize = (_maxPixelDN-_minPixelDN) / (double)_numberOfSteps;
	dnstepsize = (_maxPixelDN-_minPixelDN) / (double)(_numberOfSteps-1);
    }
    if (stepsize < 1.0)
	stepsize = 1.0;
    if (dnstepsize < 1.0)
	dnstepsize = 1.0;
    
    if (bufferSize == 1)
	_img.bw_pixels[0] = 0;			/* avoid divide by 0 below */
    else
	curstep = 0.0;			/* location of current step */
    curdnstep = 0.0;
    
    // Do lines of pixels

    for (int i = 0; i < bufferSize; i++) {
	pix = (int)((double)i * (double)(_maxPixelDN-_minPixelDN) / (double)(bufferSize-1));
	while (pix >= (int)(curstep+stepsize)) {
	    curstep += stepsize;
	    curdnstep += dnstepsize;
	}
	_img.bw_pixels[i] = (unsigned char) MIN(_maxPixelDN, 
						_minPixelDN + (int)curdnstep);
    }		
}
