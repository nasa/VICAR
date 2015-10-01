$!****************************************************************************
$!
$! Build proc for MIPL module xvdmisc_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:46
$!
$! Execute by entering:		$ @xvdmisc_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module xvdmisc_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvdmisc_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xvdmisc_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xvdmisc_h.bld "STD"
$   else
$      @xvdmisc_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdmisc_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdmisc_h.com -mixed -
	-s ImageDisplayer.h ImageWindow.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageDisplayer.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  ImageDisplayer.h:
///////////////////////////////////////////////////////////////////
#ifndef IMAGEDISPLAYER_H
#define IMAGEDISPLAYER_H
#include "UIComponent.h"
#include "BasicImageView.h"
#include "SideBar.h"
#include <iostream>

//#include "LatLonBar.h"
class LatLonBar;
class Cmd;
class ImageData;
class SiHistogram;
class Lut;
class ZoomCmdSet;

class ImageDisplayer : public UIComponent {

private:

	BasicImageView 		*_imageView;
	SideBar 		*_sideBar;

        static XtResource _resources[];
	
protected:

	Widget _bb;

	Widget _rightForm;        // holds _latLonBar and _imageView

        static String _defaults[];

	LatLonBar *_latLonBar;
	Boolean _showLatLonBar; 

        Boolean _showSideBar;

        void layoutComponents();
        void showComponents();
        void hideComponents();

public:

	ImageDisplayer( Widget parent, const char * name, ImageData *,
			Cmd *, SiHistogram *, SiHistogram *, SiHistogram *,
			Cmd *, Lut *, Lut *, Lut *, ZoomCmdSet *& );
	~ImageDisplayer();

	virtual inline Widget getWidget(){ return _imageView->getWidget(); };
	virtual BasicImageView *getImageView() { return _imageView; }

	SideBar *getSideBar() { return _sideBar; }
	void showSideBar( Boolean );
	Boolean IsSideBarDisplayed() { return _showSideBar; }

	LatLonBar *getLatLonBar() { return _latLonBar; }
	void showLatLonBar( Boolean );
	int IsLatLonBarDisplayed();

	virtual const char * const className() { return ("ImageDisplayer"); } 
};
#endif


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageWindow.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEWINDOW_H
#define IMAGEWINDOW_H
#include "MenuWindow.h"
#include "PrefDialog.h"

class ImageDisplayer;
class SiHistogram;
class ImageData;
class RotatedImageData;
class Lut;
class SiImageToHistGlue;
class SiRawHistToStrHistGlue;
class SiLutToStrHistGlue;
class ImageToPseudoGlue;
class BasicImageView;

class ImageToImageWindowGlue;

class Cmd;
class CmdList;
class QuitCmd;
class ZoomCmdSet;
class ZoomDialog;
class BorderCmd;
class MenuBarCmd;
class LabelCmd;

class ImageWindow : public MenuWindow {

private:

	static XtResource _resources[];

	Widget _form;
	static void postFileLoadEventHandler(Widget, XtPointer,
					     XEvent *, Boolean *);
	static void setUserPrefsEventHandler(Widget, XtPointer,
					     XEvent *, Boolean *);

protected:

	PrefDialog              *_prefDialog;

	RotatedImageData	*_image;
	SiHistogram		*_histR, *_histG, *_histB;
	SiHistogram		*_strhistR, *_strhistG, *_strhistB;
	Lut			*_lutR, *_lutG, *_lutB;
	Lut                     *_pseudoR, *_pseudoG, *_pseudoB;
	SiImageToHistGlue		*_imageToHistGlue;
	SiRawHistToStrHistGlue	*_rawHistToStrHistGlue;
	SiLutToStrHistGlue	*_lutToStrHistGlue;
	ImageToPseudoGlue	*_imageToPseudoGlue;

	ImageToImageWindowGlue  *_imageToImageWindowGlue;

	Boolean                 _showFullScreenDisplay;
	Boolean                 _showMotifBorder;
	Boolean                 _showMenuBar;

	Boolean                 _cmdLineHelpReqd;

       	CmdList *               _cmdListFile;

	ImageDisplayer          *_imageDisplayer;

	QuitCmd *               _quit;
	Cmd *		        _loadFileCmd;
	Cmd *	 	        _loadMenuCmd;
	Cmd *		        _histCmd;
	Cmd *		        _lutCmd;
	Cmd *		        _stretchCmd;
	Cmd *		        _cursorStretchEnableCmd;
	Cmd *                   _pseudoCmd;
	Cmd *		        _pseudoGraphCmd;
	Cmd *		        _pseudoModeCmd;
	Cmd *		        _stretchHist;
	Cmd *		        _panCmd;
	Cmd *		        _magCmd;
	Cmd *		        _prefDialogCmd;
	Cmd *		        _fullScreenDisplayCmd;
	Cmd *		        _sideBarCmd;
	BorderCmd *    	        _borderCmd;
	MenuBarCmd *            _menuCmd;
	Cmd *		        _postStretchCmd;
	Cmd *		        _postPseudoCmd;
	Cmd *		        _postZoomCmd;
	ZoomCmdSet *	        _zoomCmdSet;
	ZoomDialog *	        _zoomDialog;

	Cmd *		        _minAutoCmd;
	Cmd *		        _maxAutoCmd;
	Cmd *		        _minValueCmd;
	Cmd *		        _maxValueCmd;
	Cmd *		        _postRangeCmd;
	Cmd *                   _latLonCmd;
	Cmd *                   _reloadCmd;
        LabelCmd *              _labelDisplayCmd;

	BasicImageView * _imageView;

	// Application resources

	Boolean		_enablePrintWidgetTree;
        int             _CMapRedLevels, _CMapGreenLevels, _CMapBlueLevels;
        int             _CMapGrayLevels;
	Boolean		_autoMinRange;
	Boolean		_autoMaxRange;
	String		_minValue;	// Strings so we can detect no-value
	String		_maxValue;
	Boolean		_setInitialZoomFit;
	Boolean		_enableScriptCommand;
	String		_scriptCommand;
	Boolean		_enableSaveCommand;
	String		_saveCommand;
	Boolean		_enablePrintCommand;
	String		_printCommand;

	virtual Widget createWorkArea( Widget parent );
	void createMenuPanes();

public:

	ImageWindow( const char * name ) : MenuWindow(name) { };

	~ImageWindow();
//	virtual void initialize();

	ImageData *getImageData() { return (ImageData *)_image; }
	BasicImageView *getImageView() { return _imageView; }
	
	ImageDisplayer *getImageDisplayer() { return _imageDisplayer; }
	
	SiHistogram *getHistR() { return _histR; }
	SiHistogram *getHistG() { return _histG; }
	SiHistogram *getHistB() { return _histB; }

	Lut	  *getLutR() { return _lutR; }
	Lut	  *getLutG() { return _lutG; }
	Lut	  *getLutB() { return _lutB; }

	Lut       *getPseudoR() { return _pseudoR; }
        Lut       *getPseudoG() { return _pseudoG; }
        Lut       *getPseudoB() { return _pseudoB; }

        Cmd       *getHistCmd() { return _histCmd; }
        Cmd       *getLutCmd() { return _lutCmd; }
	Cmd	  *getStretchCmd() { return _stretchCmd; }
	Cmd       *getPseudoCmd() { return _pseudoCmd; }
	Cmd	  *getPostStretchCmd() { return _postStretchCmd; }

	virtual void enableLatLonIFs(Boolean);
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
