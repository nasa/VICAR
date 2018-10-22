///////////////////////////////////////////////////////////////////
//  ImageDisplayer.cc
//
//	This Class opens the ImageModel & ImageDisplayView
//	(to get the ball rolling).  
//
//	WARNING:   See below  regarding a Motif
//	bug that affects this design.
/*
WARNING:    This component should use a FORM widget to contain its child.
However, there seems to be a bug in (at least) Motif 1.2.1's Form widgets.
If you have a form, with a child that's a form, with a child that's a
ScrolledWindow, some bizarre behavior results.  If the SW is adjusted such
that one scrollbar is managed and the other not,  and then adjusted again such
that neither are managed (via methods other than resizing the window), the
XtUnmanagedChild() call for the second scrollbar causes the window to expand
to be big enough to contain the other, *unmanaged*, scrollbar at its old
location!  This is completely bogus as unmanaged widgets should never take
part in geometry negotiations.

This bug seems to occur only when two adjacent Form's are in the hierarchy.
Changing one Form to a Frame causes the problem to disappear.  Inserting a
Frame between the two Forms also causes the problem to disappear.  In this
case, since there is only one child of this component, we replace the Form
with a Frame.  In general, if more than one child of this is needed (so we
have to use a Form), we may need to insert a Frame widget around the Form.
A Frame with XmNshadowThickness set to '0' is just as good as a Form with a
single child with all its attachments set to ATTACH_FORM.
*/
//
///////////////////////////////////////////////////////////////////
#include "ImageData.h"
#include "ImageDisplayer.h"
#include "BasicImageView.h"
#include "SideBar.h"
#include "ImageDisplayView.h"
#include "SiHistogram.h"
#include "Lut.h"
#include "ZoomCmdSet.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include "LatLonBar.h"

#include "CursorLatLonView.h"

//  Set Resources for XVICDISP
XtResource ImageDisplayer::_resources [] = {
  {
    (char *)"showSideBar",
    (char *)"ShowSideBar",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( ImageDisplayer *, _showSideBar ),
    XmRString,
     ( XtPointer ) "TRUE",
  }
};

///////////////////////////////////////////////////////////////////
//	Resource Defaults  .. widget resources
///////////////////////////////////////////////////////////////////
String ImageDisplayer::_defaults [ ] = {
        NULL,
};

///////////////////////////////////////////////////////////////////
//	Constructor:
//		to create Image View
///////////////////////////////////////////////////////////////////
ImageDisplayer::ImageDisplayer(Widget parent, const char * name,
		ImageData *imageData, Cmd *histCmd,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		Cmd *lutCmd, Lut *lutR, Lut *lutG, Lut *lutB,
		ZoomCmdSet *&zoomCmdSet)
	: UIComponent(name)
{
  // SET DEFAULT RESOURCES
  setDefaultResources ( parent, _defaults );
	  
  // CREATE A FORM TO HOLD IMAGE WIDGET AND CURSOR STUFF	
  _w = XtVaCreateWidget( _name, xmFormWidgetClass, parent, NULL);
  installDestroyHandler();
  
  // make a form to hold _latLonBar and _imageDisplayer
  _rightForm = XtVaCreateManagedWidget("rightForm", xmFormWidgetClass, _w,
				       NULL);
  
  // Load Resources
  getResources ( _resources, XtNumber ( _resources ) );

  // DISPLAY IMAGE
  _imageView = new ImageDisplayView( _rightForm,  "imageView", imageData,
				     0, 0 );

  // Set up the zoom commands (zoomCmdSet is returned to the caller)

  zoomCmdSet = new ZoomCmdSet(_imageView);

  // Create and display SIDEBAR or Image Toolbox

  _sideBar = new SideBar ( _w, "sideBar", _imageView, imageData,
			   histCmd, histR, histG, histB,
			   lutCmd,  lutR,  lutG,  lutB,
			   zoomCmdSet );

  // lat lon bar stuff:
  _latLonBar = new LatLonBar (_rightForm, "latLonBar", _imageView, imageData );
	
  _showLatLonBar = False;
  
  layoutComponents();
  showComponents();
}

///////////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////////
ImageDisplayer::~ImageDisplayer()
{
  //	delete _image;
  //	delete _histR;
  //	delete _histG;
  //	delete _histB;
  //	delete _lutR;
  //	delete _lutG;
  //	delete _lutB;
  delete _imageView;
}


///////////////////////////////////////////////////////////////////
//	layoutComponents: Layout the Side Bar, LatLonBar and Image
//      depending upon whether or not the user wants to see the Bars.
///////////////////////////////////////////////////////////////////
void ImageDisplayer::layoutComponents()
{

  if ( _showSideBar && !_showLatLonBar) {      // SHOW SIDEBAR & IMAGE (no L/L)
    XtVaSetValues ( _sideBar->baseWidget(),
		    XmNtopAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,	XmATTACH_NONE,
		    XmNbottomAttachment,	XmATTACH_NONE,
		    NULL );
    
    XtVaSetValues(_rightForm,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, _sideBar->baseWidget(),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    
    XtVaSetValues ( _imageView->baseWidget(),
		    XmNtopAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,     XmATTACH_FORM,
		    XmNbottomAttachment,    XmATTACH_FORM,
		    NULL );

    XtVaSetValues( _latLonBar->baseWidget(),
		   XmNtopAttachment,       XmATTACH_NONE,
		   XmNleftAttachment,      XmATTACH_NONE,
		   XmNrightAttachment,	XmATTACH_NONE,
		   XmNbottomAttachment,	XmATTACH_NONE,
		   NULL );
    

  } 
  else if (_showSideBar && _showLatLonBar) { // SHOW SIDEBAR, L/L and IMAGE
    XtVaSetValues ( _sideBar->baseWidget(),
		    XmNtopAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,	XmATTACH_NONE,
		    XmNbottomAttachment,	XmATTACH_NONE,
		    NULL );

    XtVaSetValues(_rightForm, // NEW
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, _sideBar->baseWidget(),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues (_latLonBar->baseWidget(),
                   XmNtopAttachment,    XmATTACH_FORM,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNbottomAttachment, XmATTACH_NONE,
                   NULL );
    
    XtVaSetValues ( _imageView->baseWidget(),
		    XmNtopAttachment,       XmATTACH_WIDGET,
		    XmNtopWidget,           _latLonBar->baseWidget(),
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,     XmATTACH_FORM,
		    XmNbottomAttachment,    XmATTACH_FORM,
		    NULL );
  }
  
  else if(!_showSideBar && _showLatLonBar) { // SHOW L/L & IMAGE, (no SIDEBAR)
    XtVaSetValues(_rightForm, // NEW
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues ( _latLonBar->baseWidget(),
		    XmNtopAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,	XmATTACH_FORM,
		    XmNbottomAttachment,	XmATTACH_NONE,
		    NULL );

    XtVaSetValues ( _sideBar->baseWidget(),
		    XmNtopAttachment,       XmATTACH_NONE,
		    XmNleftAttachment,      XmATTACH_NONE,
		    XmNrightAttachment,	XmATTACH_NONE,
		    XmNbottomAttachment,	XmATTACH_NONE,
		    NULL );
    
    XtVaSetValues ( _imageView->baseWidget(),
		    XmNtopAttachment,       XmATTACH_WIDGET,
		    XmNtopWidget,           _latLonBar->baseWidget(),
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,     XmATTACH_FORM,
		    XmNbottomAttachment,    XmATTACH_FORM,
		    NULL );
  }
  
  else {                                          // SHOW ONLY IMAGE
    XtVaSetValues ( _sideBar->baseWidget(),
		    XmNtopAttachment,       XmATTACH_NONE,
		    XmNleftAttachment,      XmATTACH_NONE,
		    XmNrightAttachment,	XmATTACH_NONE,
		    XmNbottomAttachment,	XmATTACH_NONE,
		    NULL );
    
    XtVaSetValues( _latLonBar->baseWidget(),
		   XmNtopAttachment,       XmATTACH_NONE,
		   XmNleftAttachment,      XmATTACH_NONE,
		   XmNrightAttachment,	XmATTACH_NONE,
		   XmNbottomAttachment,	XmATTACH_NONE,
		   NULL );
    
    XtVaSetValues(_rightForm,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    
    XtVaSetValues ( _imageView->baseWidget(),
		    XmNtopAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,      XmATTACH_FORM,
		    XmNrightAttachment,     XmATTACH_FORM,
		    XmNbottomAttachment,    XmATTACH_FORM,
		    NULL );
  }
}

///////////////////////////////////////////////////////////////////
//	showComponents: Manages only the components to be shown
///////////////////////////////////////////////////////////////////
void ImageDisplayer::showComponents()
{

  if ( _showSideBar ) _sideBar->manage();
  else  _sideBar->unmanage();
  
  if ( _showLatLonBar ) {
    _latLonBar->manage();
  }
  else
    _latLonBar->unmanage();
  
  _imageView->manage();
}

///////////////////////////////////////////////////////////////////
//	hideComponents: Unmanages the SideBar and the Image so as
//      to hide them.
///////////////////////////////////////////////////////////////////
void ImageDisplayer::hideComponents()
{
  // Hide Image View Components
  
  _imageView->unmanage();
  _sideBar->unmanage();
  _latLonBar->unmanage();
}

///////////////////////////////////////////////////////////////////
//	showSideBar: Set the _showSideBar variable and redisplay
//      the SideBar and Image accordingly.
///////////////////////////////////////////////////////////////////
void ImageDisplayer::showSideBar( Boolean show )
{
  
  // Set SideBar value and display accordingly
  
  _showSideBar = show;
  
  layoutComponents();
  showComponents();
}

void ImageDisplayer::showLatLonBar( Boolean show )
{
  _showLatLonBar = show;
  
  if (_showLatLonBar == True) 
    _latLonBar->getCursorLatLonView()->start();
  else
    _latLonBar->getCursorLatLonView()->stop();
  
  //  hideComponents();
  layoutComponents();
  showComponents();
}

int ImageDisplayer::IsLatLonBarDisplayed()
{
  if(_showLatLonBar==True)
    return 1;
  return 0;
}
