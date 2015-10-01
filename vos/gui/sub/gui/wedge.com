$!****************************************************************************
$!
$! Build proc for MIPL module wedge
$! VPACK Version 1.8, Tuesday, August 12, 1997, 18:15:44
$!
$! Execute by entering:		$ @wedge
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
$ write sys$output "*** module wedge ***"
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
$ write sys$output "Invalid argument given to wedge.com file -- ", primary
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
$   if F$SEARCH("wedge.imake") .nes. ""
$   then
$      vimake wedge
$      purge wedge.bld
$   else
$      if F$SEARCH("wedge.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake wedge
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @wedge.bld "STD"
$   else
$      @wedge.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create wedge.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack wedge.com -mixed -
	-s WedgeView.cc -
	-i wedge.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create WedgeView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create wedge.imake
#define SUBROUTINE wedge
#define MODULE_LIST WedgeView.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_XMU

$ Return
$!#############################################################################
