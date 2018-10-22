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
