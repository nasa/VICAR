$!****************************************************************************
$!
$! Build proc for MIPL module xvdmisc
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:45
$!
$! Execute by entering:		$ @xvdmisc
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
$ write sys$output "*** module xvdmisc ***"
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
$ write sys$output "Invalid argument given to xvdmisc.com file -- ", primary
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
$   if F$SEARCH("xvdmisc.imake") .nes. ""
$   then
$      vimake xvdmisc
$      purge xvdmisc.bld
$   else
$      if F$SEARCH("xvdmisc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvdmisc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvdmisc.bld "STD"
$   else
$      @xvdmisc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdmisc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdmisc.com -mixed -
	-s ImageDisplayer.cc ImageWindow.cc -
	-i xvdmisc.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageDisplayer.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// ImageWindow.cc
///////////////////////////////////////////////////////////////


#ifdef ENABLE_SAGE
#include "SageApplication.h"
#else
#include "Application.h"
#endif
#include "ImageWindow.h"
#include "RotatedImageData.h"
#include "SiHistogram.h"
#include "Lut.h"
#include "ImageDisplayer.h"
#include "BasicImageView.h"
#include "CmdList.h"
#include "UndoCmd.h"
#include "MenuBar.h"
#include "MenuCmdList.h"
#include "QuitCmd.h"
#include "RadioCmd.h"
#include "NoOpCmd.h"
#include "OptionCmdMenu.h"
#include "SideBarCmd.h"
#include "BorderCmd.h"
#include "PrefDialog.h"
#include "PrefDialogCmd.h"
#include "PostPseudoCmd.h"
#include "StretchDialog.h"
#include "SiCursorStretchInterface.h"
#include "SiCursorStretchEnableCmd.h"
#include "MenuDialog.h"
#include "PseudoDialog.h"
#include "SiHistMenuCmd.h"
#include "LutMenuCmd.h"
#include "PanMenuCmd.h"
#include "MagMenuCmd.h"
#include "StretchCmd.h"
#include "PseudoCmd.h"
#include "PseudoModeCmd.h"
#include "PopupMenu.h"
#include "LoadFileCmd.h"
#include "LoadMenuCmd.h"
#include "SiImageToHistGlue.h"
#include "SiRawHistToStrHistGlue.h"
#include "SiLutToStrHistGlue.h"
#include "ImageToPseudoGlue.h"
#include "ImageToImageWindowGlue.h"
#include "ZoomCmdSet.h"
#include "ZoomDialog.h"
#include "ZoomSpecialCmd.h"
#include "ZoomFitCmd.h"
#include "PostZoomDialogCmd.h"
#include "PrintWidgetTreeCmd.h"
#include "HelpOnContextCmd.h"
#include "HelpSelfCmd.h"
#include "LutToImageWidgetGlue.h"
#include "MenuBarCmd.h"
#include "FullScreenDisplayCmd.h"
#include "DataRangeAutoCmd.h"
#include "DataRangeValueCmd.h"
#include "DataRangeDialog.h"
#include "ImageToRangeDialogGlue.h"
#include "PostDialogCmd.h"
#include "ErrorManager.h"
#include "SiRunScriptCmd.h"
#include "SiSaveAsCmd.h"
#include "SiSaveDialog.h"
#include "SiPrintCmd.h"
#include "SiPrintDialog.h"
#include <Xm/Form.h>
#include <stdlib.h>
#include <stdio.h>
#include "LatLonBar.h"
#include "LatLonBarCmd.h"
#include "CursorLatLonView.h"
#include "ReloadFileCmd.h"
#include "SiRotateImageCmd.h"
#include "ImageToReloadGlue.h"
#include "LabelCmd.h"
#include "ImageToLabelGlue.h"
#include "xvmaininc.h"		// for UNUSED

// SET RESOURCES FOR THE RESOURCE PRINT WIDGET TREE AND THE
//  THE ALLOCATED COLOR LEVELS THAT CAN BE SET BY THE USER
//  TO OVERRIDE THE DEFAULTS.  THE COLOR LEVELS ARE USED
//  CURRENTLY IN THE ColorMapsMode OBJECT WHICH IS ONE OF
//  THE OBJECTS USED IN THE PrefDialog OBJECT.  IT WAS DECIDED
//  TO PUT THE COLOR LEVEL RESOURCES AT THE APPLICATION LEVEL
//  JUST INCASE OTHER OBJECTS LIKE PAN TOOL MAY WANT TO USE THEM


XtResource ImageWindow::_resources[] = {
  {
    (char *)"enablePrintWidgetTree",
    (char *)"EnablePrintWidgetTree",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(ImageWindow, _enablePrintWidgetTree),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"allocRedLevels",
    (char *)"AllocRedLevels",
    XmRInt,
    sizeof ( int ),
    XtOffsetOf ( ImageWindow, _CMapRedLevels ),
    XmRImmediate,
    ( XtPointer ) 8,
  },
  {
    (char *)"allocGreenLevels",
    (char *)"AllocGreenLevels",
    XmRInt,
    sizeof ( int ),
    XtOffsetOf ( ImageWindow, _CMapGreenLevels ),
    XmRImmediate,
    ( XtPointer ) 8,
  },
  {
    (char *)"allocBlueLevels",
    (char *)"AllocBlueLevels",
    XmRInt,
    sizeof ( int ),
    XtOffsetOf ( ImageWindow, _CMapBlueLevels ),
    XmRImmediate,
    ( XtPointer ) 6,
  },
  {
    (char *)"allocGrayLevels",
    (char *)"AllocGrayLevels",
    XmRInt,
    sizeof ( int ),
    XtOffsetOf ( ImageWindow, _CMapGrayLevels ),
    XmRImmediate,
    ( XtPointer ) 0,
  },
  {
    (char *)"fullScreenDisplay",
    (char *)"FullScreenDisplay",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _showFullScreenDisplay ),
    XmRString,
    ( XtPointer ) "FALSE",
  },
  {
    (char *)"displayMotifBorder",
    (char *)"DisplayMotifBorder",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _showMotifBorder ),
    XmRString,
    ( XtPointer ) "TRUE",
  },
  {
    (char *)"displayMenuBar",
    (char *)"DisplayMenuBar",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _showMenuBar ),
    XmRString,
    ( XtPointer ) "TRUE",
  },
  {
    (char *)"autoMin",
    (char *)"AutoMin",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _autoMinRange ),
    XmRString,
    ( XtPointer ) "TRUE",
  },
  {
    (char *)"autoMax",
    (char *)"AutoMax",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _autoMaxRange ),
    XmRString,
    ( XtPointer ) "TRUE",
  },
  {
    (char *)"min",
    (char *)"Min",
    XmRString,
    sizeof ( String ),
    XtOffsetOf ( ImageWindow, _minValue ),
    XmRImmediate,
    ( XtPointer ) NULL,
  },
  {
    (char *)"max",
    (char *)"Max",
    XmRString,
    sizeof ( String ),
    XtOffsetOf ( ImageWindow, _maxValue ),
    XmRImmediate,
    ( XtPointer ) NULL,
  },
  {
    (char *)"fit",
    (char *)"Fit",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _setInitialZoomFit ),
    XmRString,
    ( XtPointer ) "FALSE",
  },
  {
    (char *)"enableScriptCommand",
    (char *)"EnableScriptCommand",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _enableScriptCommand ),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"enableSaveCommand",
    (char *)"EnableSaveCommand",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _enableSaveCommand ),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"enablePrintCommand",
    (char *)"EnablePrintCommand",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffsetOf ( ImageWindow, _enablePrintCommand ),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"scriptCommand",
    (char *)"ScriptCommand",
    XmRString,
    sizeof ( String ),
    XtOffsetOf ( ImageWindow, _scriptCommand ),
    XmRImmediate,
    (XtPointer) NULL,
  },
  {
    (char *)"saveCommand",
    (char *)"SaveCommand",
    XmRString,
    sizeof ( String ),
    XtOffsetOf ( ImageWindow, _saveCommand ),
    XmRImmediate,
    (XtPointer) NULL,
  },
  {
    (char *)"printCommand",
    (char *)"PrintCommand",
    XmRString,
    sizeof ( String ),
    XtOffsetOf ( ImageWindow, _printCommand ),
    XmRImmediate,
    (XtPointer) NULL,
  },
  {
    (char *)"cmdLineHelpReqd",
    (char *)"CmdLineHelpReqd",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(ImageWindow, _cmdLineHelpReqd),
    XmRImmediate,
    (XtPointer) False,
  }
};

ImageWindow::~ImageWindow()
{
  delete _image;
  
  delete _lutR;
  delete _lutG;
  delete _lutB;
  
  delete _histR;
  delete _histG;
  delete _histB;
  
  delete _strhistR;
  delete _strhistG;
  delete _strhistB;
  
  //	DELETE FILE PULLDOWN
  int size = _cmdListFile->size();
  for ( int i =0; i< size; i++ )
    delete _cmdListFile->operator[](i);
  delete _cmdListFile;
  
  //	DELETE ZOOM PULLDOWN
  delete _zoomCmdSet;
  
  //	DELETE WORKAREA
  delete _imageDisplayer;		
}

///////////////////////////////////////////////////////////////
// Create the main window, including most commands
///////////////////////////////////////////////////////////////

Widget ImageWindow::createWorkArea( Widget parent)
{
  String appName, appClass;
  XtGetApplicationNameAndClass(theApplication->display(),
			       &appName, &appClass);
  char *emTitle = new char [strlen(appName) + 15];
  sprintf (emTitle, "%s Error Manager", appName);
  ErrorManager *UNUSED(errMgr) = new ErrorManager(emTitle);
  delete [] emTitle;
  
  int i;
  
  // Get application resources.
  // There is  one is to determine whether or not to show
  // the Print Widget Tree command, 3 for the allocated colors,
  // and several for preferences setup.
  XtGetApplicationResources(theApplication->baseWidget(), (XtPointer)this,
			    _resources, XtNumber(_resources), NULL, 0);
  
  // Create all the models.
  // This task can not be done in the constructor because the application
  // would not be initialized yet and program crashes trying to execute
  // theApplication->getArgc()

    if (_cmdLineHelpReqd == True) {
      printf("\nusage: xvd [ options ] [ filename(s) | file.red .grn .blu ]\n");
      printf("         where options include:\n");
      printf("         -help         display this message\n");
      printf("         -fullscreen   run XVd in full-screen mode\n");
      printf("         -fit          set zoom factor to 'zoom to fit'\n");
      printf("         -min n        set the minimum data range value to n\n");
      printf("         -max n        set the maximum data range value to n\n");
      printf("         -height n     set window height to n\n");
      printf("         -width n      set window width to n\n");
      printf("         -x n          set horizontal window position to n\n");
      printf("         -y n          set vertical window position to n\n");
      printf("%s","\n");		// work around stupid g++ 3.3 bug
      
      // just bail out
      exit(0);
    }

    _image = new RotatedImageData();	// also an ImageDataWrapper
  

    // Set up the initial data range before loading
    // The _minValue, _maxValue resources are strings to we can check
    // for null.
    _image->setMinAuto(_autoMinRange);
    _image->setMaxAuto(_autoMaxRange);
    if (_minValue) {
	double min = atof(_minValue);
	_image->setDataMin(min);
	_image->setMinAuto(False);
    }
    if (_maxValue) {
	double max = atof(_maxValue);
	_image->setDataMax(max);
	_image->setMaxAuto(False);
    }

  // Create the file loading command
  
  _loadFileCmd = new LoadFileCmd("LoadFile", True, _image);
  _loadMenuCmd = new LoadMenuCmd("Open", True,
				 (LoadFileCmd *)_loadFileCmd);
  
  // Get the file name from the command line, if present, and load it.
  // If not present, automatically post the dialog box.    
  
  int len = 0;
  char *filename;
  if (theApplication->getArgc() > 1) {
    for (i = 1; i < theApplication->getArgc(); i++)
      len += strlen(theApplication->getParam(i)) + 1;	// +1 for comma
    filename = new char[len+1];		// +1 for null term
    strcpy(filename, "");
    for (i = 1; i < theApplication->getArgc(); i++) {
      if (i != 1)
	strcat(filename, ",");
      strcat(filename, theApplication->getParam(i));
    }
    
    _loadFileCmd->execute(filename);	// Cmd will free filename
  }
  else {
    // Put up the file dialog unless we're running under SAGE,
    // because it's just annoying in that environment.
#ifdef ENABLE_SAGE
    if (theSageApplication->clientCommManager() == NULL)
#endif
      {
	// We want to just call _loadMenuCmd->execute(), but that puts
	// the file dialog on the screen before the main window, so it
	// ends up behind the (mostly useless) main window.  So, we have
	// to register an event handler and wait for a VisibilityNotify
	// event from the main window, and then post the dialog.
	XtAddEventHandler(parent, VisibilityChangeMask, False,
			  ImageWindow::postFileLoadEventHandler, 
			  (XtPointer)this);
      }
  }
  
  // Create LUT model for each color band (initially - ramp)
  _lutR = new Lut();	// Red band also used as bw for single color images
  _lutG = new Lut();
  _lutB = new Lut();
  
  // Create pseudocolor LUTs
  _pseudoR = new Lut();
  _pseudoG = new Lut();
  _pseudoB = new Lut();
  
  // Create histogram models for 3 color band
  // For single color the other two will be set to NULL
  _histR = new SiHistogram();
  _histG = new SiHistogram();
  _histB = new SiHistogram();
  // Create histogram models for 3 color band stretched histograms
  // For single color the other two will be set to NULL
  _strhistR = new SiHistogram();
  _strhistG = new SiHistogram();
  _strhistB = new SiHistogram();
  
  _rawHistToStrHistGlue = new SiRawHistToStrHistGlue ( 
                                               _histR,    _histG,    _histB,
					       _strhistR, _strhistG, _strhistB,
					        _lutR,     _lutG,     _lutB );
  _lutToStrHistGlue = new SiLutToStrHistGlue (
					      _histR,    _histG,    _histB,
					      _strhistR, _strhistG, _strhistB,
					      _lutR,     _lutG,     _lutB );
  _imageToHistGlue = new SiImageToHistGlue(_image, _histR, _histG, _histB);
    
  _form = XtCreateManagedWidget( _name, 
				 xmFormWidgetClass, parent, 
				 NULL, 0);
    
  _labelDisplayCmd = new LabelCmd( "Label Display", TRUE, _image);
  ImageToLabelGlue *UNUSED(imageToLabelGlue) = new ImageToLabelGlue(_image,
                             _labelDisplayCmd);

  _histCmd = new SiHistMenuCmd( "Raw Hist", "Histogram", TRUE,
				_histR, _histG, _histB );
  _lutCmd = new LutMenuCmd( "Lookup Table View", TRUE,
			    _lutR, _lutG, _lutB);
  _stretchCmd = new StretchCmd( "Stretch", TRUE,
				_lutR, _histR, _lutG, _histG, _lutB, _histB);
  _pseudoGraphCmd = new LutMenuCmd( "Pseudocolor View", TRUE,
				    _pseudoR, _pseudoG, _pseudoB );
  _pseudoCmd = new PseudoCmd( "Pseudo", TRUE, _pseudoR, _pseudoG, _pseudoB );
  _stretchHist = new SiHistMenuCmd("Stretched Hist", "Stretched Histogram",
				   TRUE, _strhistR, _strhistG, _strhistB);
    
  // Note: _zoomCmdSet is actually created in ImageDisplayer and is
  // passed out (by reference) to this routine.
    
  _imageDisplayer = new ImageDisplayer( _form, "imageDisplayer", _image,
					_histCmd, _histR, _histG, _histB, 
					_lutCmd, _lutR, _lutG, _lutB,
					_zoomCmdSet);
  _imageView = _imageDisplayer->getImageView();
  
  // Set up stretch dialog and command
  
  CustomDialog *stretchDialog = new StretchDialog("Stretch Dialog", 
						  _stretchCmd, 
						  _lutR, _lutG, _lutB);
  _postStretchCmd = new PostDialogCmd("Stretch", True, stretchDialog);
  
  // Set up cursor stretch and command
  
  SiCursorStretchInterface *csif = new SiCursorStretchInterface(
	   _imageView->getWidget(), _stretchCmd, _imageView->getWidget() );
  _cursorStretchEnableCmd = new SiCursorStretchEnableCmd("Cursor Stretch",
							 TRUE, csif,
							 _stretchCmd);
    
  // Set up pseudo color dialog and command
  
  // Turn pseudocoloring on/off
  _pseudoModeCmd = new PseudoModeCmd( "PseudoMode", TRUE,
				      _imageView->getWidget() );
  
  MenuDialog *pseudoDialog = new PseudoDialog("Pseudocolor Dialog", 
					      _pseudoCmd, 
					      _pseudoModeCmd, 
					      _imageView->getWidget() );
  _postPseudoCmd = new PostPseudoCmd("Pseudo", True, pseudoDialog);
  _imageToPseudoGlue = new ImageToPseudoGlue(_image, _pseudoModeCmd, 
					     pseudoDialog, _pseudoCmd,
					     _pseudoGraphCmd, _postPseudoCmd);
  
  _panCmd = new PanMenuCmd("Pan", TRUE, _image, _imageView->getWidget(),
			   _lutR,    _lutG,    _lutB,
			   _pseudoR, _pseudoG, _pseudoB);

  // the FALSE value indicates that the menu item should be inactive
  // until we've loaded an image that can utilize that capability
  _latLonCmd = new LatLonBarCmd( "LatLonBar", FALSE, _imageDisplayer);

  _magCmd = new MagMenuCmd("Mag", TRUE, 
			_imageView->baseWidget(), 
			_image, _imageView->getWidget(),
			_lutR, _lutG, _lutB,
			_pseudoR, _pseudoG, _pseudoB, 
			_imageDisplayer->getSideBar()->getMagInfo(),
			_imageDisplayer->getSideBar()->getCursorPositionView(),
			_imageDisplayer->getSideBar()->getCursorDnView());
    
  // ATTACH IMAGE TO FORM
  XtVaSetValues( _imageDisplayer->baseWidget(), 
		 XmNtopAttachment,    XmATTACH_FORM,
		 XmNrightAttachment,  XmATTACH_FORM,
		 XmNleftAttachment,   XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL );
  
  _imageDisplayer->manage();
  
  //   Set up Preferences POPUP DIALOG BOX
  
  //   Create commands
  _sideBarCmd = new SideBarCmd("Display Side Bar", TRUE, _imageDisplayer);
  _borderCmd = new BorderCmd("Display Motif Border", TRUE, _imageView);
  _menuCmd = new MenuBarCmd("Display Menu Bar", TRUE, this, TRUE);
  _fullScreenDisplayCmd = new FullScreenDisplayCmd("Full-Screen Display",
					       TRUE, _sideBarCmd,
					       _borderCmd, _menuCmd,
					       (ImageDisplayView *)_imageView);
    
  XtInsertEventHandler(parent, VisibilityChangeMask, False,
		       ImageWindow::setUserPrefsEventHandler, 
		       (XtPointer)this, XtListHead);
  
  _prefDialog = new PrefDialog ( "Preferences", _sideBarCmd,
			_borderCmd, _menuCmd, _fullScreenDisplayCmd,
			_imageView->getWidget(), _imageView,
			_CMapRedLevels, _CMapGreenLevels, _CMapBlueLevels, 
			_CMapGrayLevels);
	
  _prefDialogCmd = new PrefDialogCmd( "Preferences", TRUE,  _prefDialog);
  
  _imageToImageWindowGlue = new ImageToImageWindowGlue(_image, this);
  
  // Set up the zoom commands
  
  _zoomDialog = new ZoomDialog("Set Zoom Factor", _zoomCmdSet);
  _zoomCmdSet->getZoomSpecialCmd()->setDialog(_zoomDialog);
  
  _postZoomCmd = new PostZoomDialogCmd("Zoom", True, _zoomDialog);
  
  if (_setInitialZoomFit)
    _zoomCmdSet->getZoomFitCmd()->execute((CmdValue)True);
  
  // Attach the LUTs to the image widget
  
  LutToImageWidgetGlue *UNUSED(lTIWG) = new LutToImageWidgetGlue(_lutR, _lutG, _lutB,
					        _imageView->getWidget(), True);
  LutToImageWidgetGlue *UNUSED(pseudoLTIWG) = new LutToImageWidgetGlue(_pseudoR,
						       _pseudoG,
						       _pseudoB,
						       _imageView->getWidget(),
						       False);

  // Set up Data Range dialog
    
  _minAutoCmd = new DataRangeAutoCmd("Minimum", True, _image,
				     False);
  _maxAutoCmd = new DataRangeAutoCmd("Maximum", True, _image,
				     True);
  _minValueCmd = new DataRangeValueCmd("Minimum Data Value", True, _image,
				       _minAutoCmd, False);
  _maxValueCmd = new DataRangeValueCmd("Maximum Data Value", True, _image,
				       _maxAutoCmd, True);
    
  DataRangeDialog *rangeDialog = new DataRangeDialog("Data Range",
			 _minAutoCmd, _maxAutoCmd, _minValueCmd, _maxValueCmd);
  _postRangeCmd = new PostDialogCmd("Data Range", True, rangeDialog);
  ImageToRangeDialogGlue *UNUSED(g) =new ImageToRangeDialogGlue(_image, rangeDialog);
  
  return (_form);
}

///////////////////////////////////////////////////////////////
// Create the menus for the main window
///////////////////////////////////////////////////////////////

void ImageWindow::createMenuPanes()
{
  MenuCmdList *cmdList;
  
  ////////
  // CREATE FILE PULLDOWN
  ////////
  
  cmdList = new MenuCmdList("File" );

  cmdList->addButton(_loadMenuCmd);    // the "Open" menu option

  _reloadCmd = new ReloadFileCmd("Reload", FALSE, _image, _loadFileCmd);
  cmdList->addButton( _reloadCmd );    // the "Reload" menu option
  
#ifndef __VMS		/* VMS supports neither save nor print */
  if (_enableSaveCommand) {
    Cmd *saveCmd = new SiSaveAsCmd("Save As", True,
			_imageView->getWidget(), _image, _saveCommand,
			_lutR, _lutG, _lutB, _pseudoR, _pseudoG, _pseudoB);
    CustomDialog *saveDialog = new SiSaveDialog("Save As Dialog", saveCmd);
    Cmd *postSaveCmd = new PostDialogCmd("Save As", False, saveDialog);
    // ImageToReloadGlue is used to enable/disable this command when a
    // file is actually present.  It is a generic cmd, despite the name...
    new ImageToReloadGlue(_image, postSaveCmd);
    cmdList->addButton(postSaveCmd);
  }

  if (_enablePrintCommand) {
    Cmd *printCmd = new SiPrintCmd("Print", True,
			_imageView->getWidget(), _image, _printCommand,
			_lutR, _lutG, _lutB, _pseudoR, _pseudoG, _pseudoB);
    CustomDialog *printDialog = new SiPrintDialog("Print Dialog", printCmd);
    Cmd *postPrintCmd = new PostDialogCmd("Print", False, printDialog);
    // ImageToReloadGlue is used to enable/disable this command when a
    // file is actually present.  It is a generic cmd, despite the name...
    new ImageToReloadGlue(_image, postPrintCmd);
    cmdList->addButton(postPrintCmd);
  }
#endif


  _quit = new QuitCmd( "Exit", TRUE ); // the "Exit" menu option
  cmdList->addButton( _quit );

  _menuBar->addCommands( cmdList );
  
  delete cmdList;
  
  ////////
  // CREATE EDIT PULLDOWN
  ////////
  
  cmdList = new MenuCmdList("Edit" );
  cmdList->addButton( theUndoCmd );
  cmdList->addButton( _postRangeCmd );
  cmdList->addButton( _prefDialogCmd );

  MenuCmdList *subCmdList = new MenuCmdList("Rotate Image");
  CmdList *radioList = new CmdList();
  Cmd *cmd = new SiRotateImageCmd("0", True, radioList, _image, ROTATE_NO);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);
  cmd = new SiRotateImageCmd("90", True, radioList, _image, ROTATE_CW);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);
  cmd = new SiRotateImageCmd("180", True, radioList, _image, ROTATE_FULL);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);
  cmd = new SiRotateImageCmd("270", True, radioList, _image, ROTATE_CCW);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);
  cmd = new SiRotateImageCmd("Flip NE_SW", True, radioList, _image, FLIP_NE_SW);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);
  cmd = new SiRotateImageCmd("Flip NW_SE", True, radioList, _image, FLIP_NW_SE);
  radioList->add(cmd);
  subCmdList->addRadioButton(cmd);

  cmdList->addSubmenu(subCmdList);
  // Don't delete radioList; the Radio button action needs it

  _menuBar->addCommands( cmdList );
  
  delete cmdList;
  delete subCmdList;
  
  ////////
  // CREATE TOOLS PULLDOWN
  ////////
  
  cmdList = new MenuCmdList("Tools" );
  
  cmdList->addButton( _labelDisplayCmd );
  cmdList->addButton( _histCmd );
  cmdList->addButton( _stretchHist );
  cmdList->addButton( _postStretchCmd );
  cmdList->addCheckBox( _cursorStretchEnableCmd );
  cmdList->addButton( _lutCmd );
  cmdList->addButton( _panCmd );
  cmdList->addCheckBox( _magCmd );
  cmdList->addButton( _postZoomCmd );
  cmdList->addButton( _postPseudoCmd );
  cmdList->addButton( _pseudoGraphCmd );
  cmdList->addCheckBox(_pseudoModeCmd);
  
  cmdList->addCheckBox( _latLonCmd ); // NEW
  
  if (_enableScriptCommand) {
    Cmd *scriptCmd = new SiRunScriptCmd("ScriptCommand", True,
					_imageView->getWidget(),
					_image, _scriptCommand);
    cmdList->addButton( scriptCmd );
  }
  
  _menuBar->addCommands( cmdList );
  
  ////////
  // Create PopupMenu for Button 3
  // Note: This uses the same list as the Tools menu, for starters
  ////////

  cmdList->addSeparator();
  cmdList->addButton(_prefDialogCmd);
  cmdList->addButton(_postRangeCmd);
  cmdList->addSeparator();
  cmdList->addCheckBox(_fullScreenDisplayCmd);
  cmdList->addCheckBox(_borderCmd);
  cmdList->addCheckBox(_menuCmd);
  cmdList->addCheckBox(_sideBarCmd);
  
  PopupMenu *popup = new PopupMenu(_form, "Image Popup", cmdList);
  popup->attachPopup(_form);
  
  delete cmdList;
  
  ////////
  // Create Help menu
  ////////
  
  Cmd *helpOnContextCmd = new HelpOnContextCmd("On Context", True, _w);
  Cmd *helpOnHelpCmd = new HelpSelfCmd("On Help", True, _menuBar->baseWidget(),
				       "*Help*On Help");
  Cmd *helpOnWindowCmd = new HelpSelfCmd("On Window", True,
					 _menuBar->baseWidget(),
					 "*Help*On Window");
  Cmd *helpOnKeysCmd = new HelpSelfCmd("On Keys", True, _menuBar->baseWidget(),
				       "*Help*On Keys");
  Cmd *helpOnVersionCmd = new HelpSelfCmd("On Version", True,
					  _menuBar->baseWidget(),
					  "*Help*On Version");

  Cmd *printWidgetTreeCmd = NULL;
  if (_enablePrintWidgetTree)
    printWidgetTreeCmd = new PrintWidgetTreeCmd("Print Widget Tree",
						True, _w);
  
  cmdList = new MenuCmdList("Help");
  
  cmdList->addButton(helpOnContextCmd);
  cmdList->addButton(helpOnHelpCmd);
  cmdList->addButton(helpOnWindowCmd);
  cmdList->addButton(helpOnKeysCmd);
  cmdList->addButton(helpOnVersionCmd);
  
  if (_enablePrintWidgetTree) {
    cmdList->addSeparator();
    cmdList->addButton(printWidgetTreeCmd);
  }
  
  _menuBar->addCommands(cmdList, True);
  
}

///////////////////////////////////////////////////////////////
// Call _loadMenuCmd->execute() in order to post the file selection
// dialog box.  This event handler is called via a VisibilityNotify
// event from the main window, so that the dialog will be posted
// only *after* the main window and thus appear on top in the
// stacking order.  Since we only want to do this once, we
// remove the event handler in here too.
///////////////////////////////////////////////////////////////

void ImageWindow::postFileLoadEventHandler(
			  Widget w, XtPointer clientData, XEvent *, Boolean *)
{
  ImageWindow *obj = (ImageWindow *)clientData;
  
  XtRemoveEventHandler(w, VisibilityChangeMask, False,
		       ImageWindow::postFileLoadEventHandler, (XtPointer)obj);
  
  obj->_loadMenuCmd->execute();		// Posts the dialog
}

//////////////////////////////////////////////////////////////
// Call this to process user defined preferences.
// This event handler is called via a VisibilityNotify
// event from the main window, so that the preferences will be
// processed during initialization.  This occurs *after* the 
// main window has been displayed and should really occur before 
// it is displayed, but the Motif Border needs to be realized also 
// before this can occur.  We need to come up with an event that
// occurs after the Motif Border has been realized and before the
// main window is displayed.  Since we only want to do this once, we
// remove the event handler in here too.
///////////////////////////////////////////////////////////////

void ImageWindow::setUserPrefsEventHandler(
			  Widget w, XtPointer clientData, XEvent *, Boolean *)
{
  ImageWindow *obj = (ImageWindow *)clientData;
  
  XtRemoveEventHandler(w, VisibilityChangeMask, False,
		       ImageWindow::setUserPrefsEventHandler, (XtPointer)obj);
  
  //   Check user preferences and execute commands for initial state.
  if (obj->_showFullScreenDisplay) {
    if (!obj->_showMotifBorder)
      obj->_borderCmd->SetValue((CmdValue)FALSE);
    if (!obj->_showMenuBar)
      obj->_menuCmd->SetValue((CmdValue)FALSE);
    obj->_fullScreenDisplayCmd->execute((CmdValue)TRUE);
  } else {
    if (!obj->_showMotifBorder)
      obj->_borderCmd->execute((CmdValue)FALSE);
    if (!obj->_showMenuBar)
      obj->_menuCmd->execute((CmdValue)FALSE);
  }
}

void ImageWindow::enableLatLonIFs(Boolean enable) 
{
  _imageDisplayer->getLatLonBar()->getCursorLatLonView()->update();

  _imageDisplayer->getLatLonBar()->getCursorLatLonView()
    ->enableLatLonSelect(enable);


  // deactivating the latLonCmd will un-display (hide) the latLonBar
  if( enable == True ) {
    _latLonCmd->activate();
  }
  else {
    _latLonCmd->undo();  // "un-press" the button
    _latLonCmd->deactivate();
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvdmisc.imake
#define SUBROUTINE xvdmisc
#define MODULE_LIST ImageDisplayer.cc ImageWindow.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

#ifdef ENABLE_SAGE
#define LIB_SAGE_CLIENT
#endif

#ifndef NO_PDS
#define LIB_PDS
#endif

$ Return
$!#############################################################################
