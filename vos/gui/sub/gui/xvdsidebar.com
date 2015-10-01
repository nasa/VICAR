$!****************************************************************************
$!
$! Build proc for MIPL module xvdsidebar
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:48
$!
$! Execute by entering:		$ @xvdsidebar
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
$ write sys$output "*** module xvdsidebar ***"
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
$ write sys$output "Invalid argument given to xvdsidebar.com file -- ", primary
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
$   if F$SEARCH("xvdsidebar.imake") .nes. ""
$   then
$      vimake xvdsidebar
$      purge xvdsidebar.bld
$   else
$      if F$SEARCH("xvdsidebar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvdsidebar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvdsidebar.bld "STD"
$   else
$      @xvdsidebar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdsidebar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdsidebar.com -mixed -
	-s HistMenuCmd.cc HistWindow.cc LutMenuCmd.cc LutWindow.cc -
	   PanMenuCmd.cc MagMenuCmd.cc SideBar.cc SideBarCmd.cc -
	   StretchMenuCmd.cc ImageSizeView.cc -
	-i xvdsidebar.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create HistMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// HistMenuCmd.cc: Example, dummy command class
//////////////////////////////////////////////////////////
#include "HistMenuCmd.h"
#include "HistWindow.h"
#include "Histogram.h"
#include <iostream>
using namespace std;

HistMenuCmd::HistMenuCmd ( char *name, char* titleName, int active, 
	Histogram *histR, Histogram *histG, Histogram *histB ) : 
	NoUndoCmd ( name, active )
{
    //  Save the histogram pointers and set value to create
    //  histogram once.

    _created = FALSE;
    _histR = histR;
    _histG = histG;
    _histB = histB;
    _title = titleName;
}

void HistMenuCmd::doit()
{
  // Execute the following upon button activation.
  // Create histogram Window only once and then display it.
  //  Set the Close button to the UNMAP state so the Window
  //  is only unmanaged when it is closed and can therefore
  //  be managed again when the user hits the command button.

  if (!_created) {                 // Do only once
    _histWindow = new HistWindow ( _histR, _histG, _histB, _title);
    _histWindow->initialize();
    XtVaSetValues(_histWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);

    _created = TRUE;
  }
  _histWindow->manage();
}      
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// HistWindow.cc:
////////////////////////////////////////////////////////////////////
#include "HistWindow.h"
// #include "MenuBar.h"
#include "NoOpCmd.h"
#include "SetStackNoBlendCmd.h"
#include "SetStackBlendCmd.h"
#include "SetPopUpDirRowCmd.h"
#include "SetPopUpDirColCmd.h"
#include "SetHorHistGraphCmd.h"
#include "SetVertHistGraphCmd.h"
#include "SetAscAxisCmd.h"
#include "SetDescAxisCmd.h"
#include "HistLogScaleCmd.h"
#include "SpikeDialog.h"
#include "SpikeDialogCmd.h"
#include "StatsCmd.h"
#include "AxisCmd.h"
#include "HistCmd.h"
#include "UndoCmd.h"
#include "QuitCmd.h"
#include "MenuCmdList.h"
#include "SeparatorCmd.h"
#include "PopupMenu.h"
#include "Histogram.h"
#include "HistBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RepType.h>

HistWindow::HistWindow ( Histogram *histR, Histogram *histG, Histogram *histB, 
				char *name ) : MenuWindow ( name )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;
}


Widget HistWindow::createWorkArea ( Widget parent )
{

    _form = XtVaCreateWidget("workArea", 
				xmFormWidgetClass, parent,
				NULL);

    _histBox = new HistBox(_form, "histBox", _histR, _histG, _histB);

    XtVaSetValues   ( _histBox->baseWidget(),
                     XmNtopAttachment,     XmATTACH_FORM,
                     XmNleftAttachment,    XmATTACH_FORM,
                     XmNrightAttachment,   XmATTACH_FORM,
                     XmNbottomAttachment,  XmATTACH_FORM,
                     NULL );
    // Create the Hist_VIEW Pulldown Menu objects
    
    CmdList *radList0 = new CmdList();
    _stackNoBlend = new SetStackNoBlendCmd ( "StackNoBlend", TRUE, _histBox, radList0 );
    _stackBlend = new SetStackBlendCmd ( "StackBlend", TRUE, _histBox, radList0 );
    _row = new SetPopUpDirRowCmd ( "Row", TRUE, _histBox, radList0 );
    _column = new SetPopUpDirColCmd ( "Column", TRUE, _histBox, radList0 );

    radList0->add( _stackNoBlend );
    radList0->add( _stackBlend );
    radList0->add( _row );
    radList0->add( _column );

    _stats = new StatsCmd ( "Show Stats", TRUE, _histBox );
    _axis = new AxisCmd ( "Show Axis", TRUE, _histBox );

    //  Create the Preferences Pulldown Menu objects as Radio Buttons
    //  Need to create 2 independent radio banks radList1 and radList2

    CmdList *radList1 = new CmdList();
    _vertical = new SetVertHistGraphCmd ( "Vertical HistGraph", TRUE, _histBox, radList1 );
    _horizontal = new SetHorHistGraphCmd ( "Horizontal HistGraph", TRUE, _histBox, radList1 );

    radList1->add( _vertical );
    radList1->add( _horizontal );

    CmdList *radList2 = new CmdList();
    _ascending = new SetAscAxisCmd ( "Ascending Axis Values", TRUE, _histBox, radList2 );    
    _descending = new SetDescAxisCmd ( "Descending Axis Values", TRUE, _histBox, radList2 );    

    radList2->add( _ascending );
    radList2->add( _descending );

    _vertical->addToActivationList (_ascending);
    _vertical->addToActivationList (_descending);

    _horizontal->addToDeactivationList (_ascending);
    _horizontal->addToDeactivationList (_descending);

    CustomDialog *dialog = new SpikeDialog (_histBox, "Spike");

   _spikeDialog = new SpikeDialogCmd( "Spike", TRUE, dialog );

   _logScale = new HistLogScaleCmd ("Log-Scaled Axis", TRUE, _histBox); 

   _histBox->manage();

   return (_form);
}

void HistWindow::createMenuPanes()
{
    MenuCmdList *cmdList;

    // Add Hist_View Pulldown Menu

    cmdList = new MenuCmdList("Hist_View");
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _stackNoBlend );
    cmdList->addRadioButton ( _stackBlend );   
    cmdList->addRadioButton ( _row );
    cmdList->addRadioButton ( _column );
    cmdList->addSeparator ();   
    cmdList->addCheckBox ( _stats );
    cmdList->addCheckBox ( _axis );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _horizontal );
    cmdList->addRadioButton ( _vertical );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _ascending );
    cmdList->addRadioButton ( _descending );
    cmdList->addSeparator ();   
    cmdList->addButton( _spikeDialog );
    cmdList->addSeparator ();
    cmdList->addCheckBox (_logScale );

    _menuBar->addCommands ( cmdList, FALSE );

    //  Add in PopupMenu for Button 3

    PopupMenu *popup = new PopupMenu(_form, "Hist Popup", cmdList);
    popup->attachPopup(_form);

    delete cmdList;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// LutMenuCmd.cc: Lookup table (LUT) command class for
//                Lookup Table View command from Image Menu
//////////////////////////////////////////////////////////
#include "LutMenuCmd.h"
#include "LutWindow.h"

//////////////////////////////////////////////////////////////
// LutMenuCmd: Saves pointers to the images histgram and
//         lookup table.  Might not need the histogram
//         pointers.
//////////////////////////////////////////////////////////////
LutMenuCmd::LutMenuCmd ( const char *name, int active, 
                Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

}

//////////////////////////////////////////////////////////////
// doit: Execute upon command activation.
//        Create lookup table window just once, set the Close
//        button to the UNMAP state and display the window by
//        managing it.
//////////////////////////////////////////////////////////////
void LutMenuCmd::doit()
{
  if (!_created) {
    _lutWindow = new LutWindow( "LutWindow", _lutR, _lutG, _lutB );
    _lutWindow->initialize();
    _created = TRUE;
    XtVaSetValues(_lutWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);
  }
  _lutWindow->manage();
}      



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// LutWindow.cc:
////////////////////////////////////////////////////////////////////
#include "LutWindow.h"
#include "Lut.h"
#include "LutBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>

LutWindow::LutWindow ( const char *name, 
		Lut *lutR, Lut *lutG, Lut *lutB ) : MainWindow ( name )
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}


Widget LutWindow::createWorkArea ( Widget parent )
{
   _form = XtVaCreateWidget("workArea", 
			xmFormWidgetClass, parent,
			NULL);

   _lutBox = new LutBox(_form, "lutBox", _lutR, _lutG, _lutB);

   XtVaSetValues   ( _lutBox->baseWidget(),
                     XmNtopAttachment,     XmATTACH_FORM,
                     XmNleftAttachment,    XmATTACH_FORM,
                     XmNrightAttachment,   XmATTACH_FORM,
                     XmNbottomAttachment,  XmATTACH_FORM,
                     NULL );

   _lutBox->manage();

   return (_form);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PanMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// PanMenuCmd.cc: Example, dummy command class
//////////////////////////////////////////////////////////
#include "PanMenuCmd.h"
#include "Lut.h"

PanMenuCmd::PanMenuCmd ( const char *name, int active, 
	ImageData *imageData, Widget widget,
	Lut *rlut, Lut *glut, Lut *blut, Lut *rplut, Lut *gplut, Lut *bplut)
	: NoUndoCmd ( name, active )
{
    _imageData = imageData;
    _imageViewWidget = widget;
    _created = FALSE;
    _rlut = rlut;
    _glut = glut;
    _blut = blut;
    _rplut = rplut;
    _gplut = gplut;
    _bplut = bplut;
}

void PanMenuCmd::doit()
{
  if (!_created) {
    _panToolWindow = new PanToolWindow("PanToolWindow", _imageData,
		_imageViewWidget,
		_rlut, _glut, _blut, _rplut, _gplut, _bplut );
    _panToolWindow->initialize();
    XtVaSetValues(_panToolWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);
    _created = TRUE;
  }
  _panToolWindow->manage();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MagMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// MagMenuCmd.cc: 
//////////////////////////////////////////////////////////
#include "MagMenuCmd.h"
#include "MagTool.h"
#include "ImageData.h"
#include "Lut.h"
#include "CursorModel.h"
#include "MagInfo.h"

MagMenuCmd::MagMenuCmd(const char *name, int active, Widget parent,
                ImageData *imageData, Widget iw,
                Lut *rlut, Lut *glut, Lut *blut,
		Lut *rpseudo, Lut *gpseudo, Lut *bpseudo,
                MagInfo *magInfo,
                CursorPositionView *posView, CursorDnView *dnView)
        : NoUndoCmd(name, active)
{
    _parent = parent;
    _imageData = imageData;
    _iw = iw;
    _rlut = rlut;
    _glut = glut;
    _blut = blut;
    _rpseudo = rpseudo;
    _gpseudo = gpseudo;
    _bpseudo = bpseudo;
    _posView = posView;
    _dnView = dnView;
    _magInfo = magInfo;

    _magTool = NULL;
}

MagMenuCmd::~MagMenuCmd()
{
    if (_magTool) {
        _magTool->unmanage();
        delete _magTool;
    }
}

void MagMenuCmd::doit()
{
    if (_value) {
        _magTool = new MagTool(_parent, "magTool", _imageData, _iw, 
			_rlut, _glut, _blut,
			_rpseudo, _gpseudo, _bpseudo,
                        _magInfo, _posView, _dnView);

        _magTool->manage();
    }
    else {
        if (!_magTool) return;
        _magTool->unmanage();
        delete _magTool;
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SideBar.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// SideBar.cc:
////////////////////////////////////////////////////////
#include "SideBar.h"
#include "BasicImageView.h"
#include "ImageDisplayView.h"
#include "SiHistBtnInterface.h"
#include "LutBtnInterface.h"
#include "SiHistMenuCmd.h"
#include "ImageWindow.h"
#include "CursorModel.h"
#include "CursorDnView.h"
#include "CursorPositionView.h"
#include "ZoomCmdSet.h"
#include "OptionCmdMenu.h"
#include "MagInfo.h"
#include "ImageSizeView.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>

SideBar::SideBar ( Widget parent, const char * name, BasicImageView *imageView,
		ImageData *imageData, Cmd *histCmd,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		Cmd *lutCmd, Lut *lutR, Lut *lutG, Lut *lutB,
		ZoomCmdSet *zoomCmdSet)
	: UIComponent (name)
{
	// CREATE SIDE BAR TOOLBOX FOR<

	_w  = XtVaCreateWidget ( _name,
                                xmFormWidgetClass,
                                parent,
				NULL );
	installDestroyHandler ();
	
	// CREATE CURSOR MODEL

	CursorModel *cursorModel = new CursorModel( True, imageView->getWidget() );

	// DISPLAY CURSOR POSITION

	unsigned char bitFlags = (unsigned char) 255;

	_cursorPositionView = new CursorPositionView( _w, "cursorPositionView", 
			  cursorModel, imageData,  bitFlags );
	XtVaSetValues(_cursorPositionView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			NULL);

	// DISPLAY CURSOR DN VALUE 

	_cursorDnView = new CursorDnView( _w, "cursorDnView", 
					cursorModel,  imageData,  bitFlags );
	XtVaSetValues(_cursorDnView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _cursorPositionView->baseWidget(),
			NULL);

	// Put separator between cursor stuff and zoom

	Widget sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, _w,
			NULL, 0);
	XtVaSetValues(sep1,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _cursorDnView->baseWidget(),
			NULL);

	// DISPLAY NUMBER OF LINES AND SAMPLES

	_imageSizeView = new ImageSizeView( _w, "imageSizeView", 
					imageData);
	XtVaSetValues(_imageSizeView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, sep1,
			NULL);

	// Put separator between cursor stuff and zoom

	Widget sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, _w,
			NULL, 0);
	XtVaSetValues(sep2,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _imageSizeView->baseWidget(),
			NULL);

	// Display zoom option menu

	OptionCmdMenu *zoomMenu = new OptionCmdMenu(_w, "zoomMenu",
				zoomCmdSet->getRadioList());
	XtVaSetValues(zoomMenu->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, sep2,
			NULL);

	// Display histogram "button" 

	CmdInterface *popHistBtn = new SiHistBtnInterface ( _w, histCmd,
                  histR, histG, histB );
	XtVaSetValues(popHistBtn->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, zoomMenu->baseWidget(),
			NULL);

	// Display LUT "button"

	CmdInterface *lutBtn = new LutBtnInterface ( _w, lutCmd,
		  lutR, lutG, lutB );
	XtVaSetValues(lutBtn->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, popHistBtn->baseWidget(),
			NULL);

	_magInfo = new MagInfo ( _w, "magInfo" );
	XtVaSetValues(_magInfo->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, lutBtn->baseWidget(),
			NULL);


	_cursorPositionView->manage();
	_cursorDnView->manage();
  	_imageSizeView->manage();
	zoomMenu->manage();
	popHistBtn->manage();
        lutBtn->manage();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SideBarCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// SideBarCmd.cc: Creates the SideBar Command for managing
//                whether the SideBar is displayed or not
//                from the dialog check box in Preferences
///////////////////////////////////////////////////////////
#include "SideBarCmd.h"
#include "ImageDisplayer.h"
#include <iostream>
using namespace std;
#include <stdint.h>

SideBarCmd::SideBarCmd( const char *name, int active, ImageDisplayer *obj )
		: Cmd ( name, active )
{
    // Save the image view and determine the current state to set
    //  the check box appropriately.

    _imageView = obj;
    int value = (int) _imageView->IsSideBarDisplayed();
    _value = (CmdValue) (uintptr_t) value;
    newValue();

}

void SideBarCmd::doit()
{
    // Save the old value for the undoit command and change to
    //  the new value;

    _oldValue = _imageView->IsSideBarDisplayed();
    _imageView->showSideBar( (_value != 0));

}      

void SideBarCmd::undoit()
{

     // Undo the command to the last SideBar state

    _imageView->showSideBar( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();

}       














$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// StretchMenuCmd.cc: Command class to execute the STRETCH
//                    command from the Image Menu
//////////////////////////////////////////////////////////
#include "StretchMenuCmd.h"
#include <Xm/Xm.h>
#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////
// StretchMenuCmd:
////////////////////////////////////////////////////////////
StretchMenuCmd::StretchMenuCmd ( char *name, int active, 
	Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

////////////////////////////////////////////////////////////
// doit:
////////////////////////////////////////////////////////////
void StretchMenuCmd::doit()
{
//  deactivate();
  if (!_created) {
//    _histWindow = new HistWindow( _histR, _histG, _histB, "HistWindow" );
//    _histWindow->initialize();
//    XtVaSetValues(_histWindow->baseWidget(), 
//		XmNdeleteResponse, XmUNMAP, 
//		NULL);

    _created = TRUE;
  }
//  _histWindow->manage();
}      
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageSizeView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageSizeView.cc
//
//	Class for views containing number of lines and 
//      samples in image
//
///////////////////////////////////////////////////////////////
#include "ImageSizeView.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <string.h>
#include <stdio.h>

///////////////////////////////////////////////////////////////
//	Constructor
///////////////////////////////////////////////////////////////
ImageSizeView::ImageSizeView(Widget parent, const char *name, 
			ImageData *imageSizeModel) 
		: BasicImageView(name, imageSizeModel)
{
   // CREATE ROW/COLUMN TO HOLD THESE SIZE DISPLAYS
   _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, NULL);
   installDestroyHandler ();
	
   _form = NULL;
   _label = NULL;
   _textfield = NULL;

   // CREATE IMAGE SIZE DISPLAYS
   addNewSubView("ImageSize");

   // ATTACH VIEW  TO DATA MODEL
   _model->attachView(this);
}

///////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////
ImageSizeView::~ImageSizeView ()
{
   _model->detachView(this);

}

///////////////////////////////////////////////////////////////
//	updateValue:
//		Update the image size value.
///////////////////////////////////////////////////////////////
void ImageSizeView::updateValue(int ns, int nl)
{
   char buf[132];

   sprintf(buf, "%2dx%2d", ns, nl);

   XmTextFieldSetString (_textfield, buf);
}

///////////////////////////////////////////////////////////////
//	addNewSubView:
//		create a single 
//		display group consisting of: label and a 
//		textfield. The textfield is used to show  
//              values that are updated as new image is
//		displayed. A name is 
//              automatically created as : 
//		"label" +  <displayName>,   etc.   
///////////////////////////////////////////////////////////////
void ImageSizeView::addNewSubView(const char * displayName)
{
   char name[132];

      _form = XtVaCreateManagedWidget("form", 
                  xmFormWidgetClass, _w, NULL);

      // CREATE LABEL
      strcpy(name, "label"); 
      strcat(name, displayName);
      _label = XtVaCreateManagedWidget(name, 
                  xmLabelWidgetClass, _form, 
                  XmNtopAttachment, XmATTACH_FORM,
                  XmNleftAttachment, XmATTACH_FORM,
                  NULL ); 

      // CREATE VALUE:  ITS REALLY A TEXTFIELD
      strcpy(name, "textfield"); 
      strcat(name, displayName);
      _textfield = XtVaCreateManagedWidget(name, 
                  xmTextFieldWidgetClass, _form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, _label,
                  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                  XmNleftWidget, _label,
                  XmNshadowThickness, 0,
                  XmNcolumns, 13,
                  XmNmarginHeight, 1,
                  NULL);
}

///////////////////////////////////////////////////////////////
//	update
//      display new image size
///////////////////////////////////////////////////////////////
void ImageSizeView::update()
{
   updateValue(_model->getNumbSamples(), _model->getNumbLines());
}


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvdsidebar.imake
#define SUBROUTINE xvdsidebar
#define MODULE_LIST  HistMenuCmd.cc HistWindow.cc \
   LutMenuCmd.cc LutWindow.cc PanMenuCmd.cc MagMenuCmd.cc \
   SideBar.cc SideBarCmd.cc \
   StretchMenuCmd.cc ImageSizeView.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
