$!****************************************************************************
$!
$! Build proc for MIPL module labelview
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:27
$!
$! Execute by entering:		$ @labelview
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
$ write sys$output "*** module labelview ***"
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
$ write sys$output "Invalid argument given to labelview.com file -- ", primary
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
$   if F$SEARCH("labelview.imake") .nes. ""
$   then
$      vimake labelview
$      purge labelview.bld
$   else
$      if F$SEARCH("labelview.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake labelview
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @labelview.bld "STD"
$   else
$      @labelview.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create labelview.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack labelview.com -mixed -
	-s LabelCmd.cc ListLabelCmd.cc ClearLabelCmd.cc LabelWindow.cc -
	   TextDisplayModel.cc TextDisplayView.cc ImageToLabelGlue.cc -
	   LabelClearEveryRunCmd.cc -
	-i labelview.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LabelCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
//
//   LabelCmd.cc: 
//
//   This is a class derived from NoUndoCmd.
//   It is for setting up the image label display window.
//
////////////////////////////////////////////////////////////
#include "LabelCmd.h"
#include "LabelWindow.h"
#include "TextDisplayView.h"
#include "TextDisplayModel.h"
#include "ImageDefs.h"
#include "ImageToLabelGlue.h"

LabelCmd::LabelCmd ( const char *name, int active, ImageData *imageData) 
	: NoUndoCmd ( name, active )
{
   _image = imageData;
   _created = FALSE;
   _labelWindow = NULL;
}

LabelCmd::~LabelCmd()
{
   if (_created) {
      _labelWindow->unmanage();
      delete _labelWindow;
      _created = FALSE;
   }
}

void LabelCmd::resetLabelWindow()
{
   if (_created) 
      if (_labelWindow)
         _labelWindow->reset();
}

void LabelCmd::doit()
{
   // Execute the following upon button activation.
   // Create window only once and then display it.
   // Set the Close button to the UNMAP state so the Window
   // is only unmanaged when it is closed and can therefore
   // be managed again when the user hits the command button.

   if (!_created) {                 // Do only once
      _labelWindow = new LabelWindow( "Image Label", _image);
      _labelWindow->initialize();
      _labelWindow->setDefault();
      XtVaSetValues( _labelWindow->baseWidget(), 
                     XmNdeleteResponse, XmUNMAP, 
		     NULL );

      _created = TRUE;
   }
   _labelWindow->manage();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ListLabelCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   ListLabelCmd.cc: 
//
//   This is a class derived from NoUndoCmd.
//   It displays image labels.
//
///////////////////////////////////////////////////////////////
#include "ListLabelCmd.h"
#include "BasicComponent.h"
#include "TextDisplayModel.h"
#include "ImageData.h"
#include "ImageLabel.h"

ListLabelCmd::ListLabelCmd ( const char *name, int active, char *key, TextDisplayModel *textM) 
	: NoUndoCmd ( name, active )
{
   _key = strdup(key);
   _textM = textM;
   _value = NULL;
   _maxLabelSize = 0;
}

void ListLabelCmd::doit()
{
   StatusType status;

   status = _textM->getImage()->getLabelSetValue(_value, _key, 
            &_maxLabelSize);
   if (_value)
      _textM->setText(_value, strlen(_value));
   _value[0] = '\0';
}

void ListLabelCmd::freeValue(CmdValue value)
{
   delete (char *)value;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ClearLabelCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   ClearLabelCmd.cc: 
//
//   This is a class derived from NoUndoCmd.
//   It clears image label display.
//
///////////////////////////////////////////////////////////////
#include "ClearLabelCmd.h"
#include "TextDisplayModel.h"

ClearLabelCmd::ClearLabelCmd ( const char *name, int active, TextDisplayModel *textM) 
	: NoUndoCmd ( name, active )
{
   _textM = textM;
}

void ClearLabelCmd::doit()
{
   _textM->clear();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LabelWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
//
//     LabelWindow.cc: 
//
//     This is a derived from MenuWindow class.
//     It sets up and manages the window for label display.
//
////////////////////////////////////////////////////////////////////
#include "LabelWindow.h"
#include "ImageLabel.h"
#include "ImageData.h"
#include "ListLabelCmd.h"
#include "TextDisplayModel.h"
#include "TextDisplayView.h"
#include "MenuBar.h"
#include "NoOpCmd.h"
#include "MenuCmdList.h"
#include "ClearLabelCmd.h"
#include "LabelClearEveryRunCmd.h"

#include "SgSearchAgainCmd.h"
#include "SgSearchTextWidgetCmd.h"
#include "SgSearchTextDialog.h"

#include "SgSaveTextWidgetCmd.h"
#include "PostSingleFileDialogCmd.h"
#include "PostDialogCmd.h"

#include <Xm/Form.h>

String LabelWindow::_defaults[] = {
   (char *)"*File.Save Label.labelString: Save Label...",
   (char *)"*File.Save Label.mnemonic: S",
   (char *)"*File.Save Label.accelerator: Ctrl<Key>S",
   (char *)"*File.Save Label.acceleratorText: Ctrl+S",
   (char *)"*Options.mnemonic: O",
   (char *)"*Options.Clear Output.mnemonic: C",
   (char *)"*Options.Clear Output.accelerator: Ctrl<Key>C",
   (char *)"*Options.Clear Output.acceleratorText: Ctrl+C",
   (char *)"*Options.Clear Output On Every Run.mnemonic: E",
   (char *)"*Options.Clear Output On Every Run.accelerator: Ctrl<Key>E",
   (char *)"*Options.Clear Output On Every Run.acceleratorText: Ctrl+E",
   (char *)"*Options.Find.mnemonic: F",
   (char *)"*Options.Find.accelerator: Ctrl<Key>F",
   (char *)"*Options.Find.acceleratorText: Ctrl+F",
   (char *)"*Options.Find Next.mnemonic: N",
   (char *)"*Options.Find Next.accelerator: Ctrl<Key>N",
   (char *)"*Options.Find Next.acceleratorText: Ctrl+N",
   NULL
};

LabelWindow::LabelWindow( const char *name, ImageData *image)
		: MenuWindow ( name )
{
   _searchDialog = NULL;
   _searchCmd = NULL;
   _textArea = NULL;
   _form = NULL;
   _clearOutputCmd = NULL;
   _clearEveryRunCmd = NULL;

   _view = new TextDisplayView( "Image Label", image);
   _textM = new TextDisplayModel(image);
   _view->registerTextModel(_textM);

}

LabelWindow::~LabelWindow( )
{
   delete _view;
   delete _textM;
   if (_clearOutputCmd)
      delete _clearOutputCmd;
   if (_clearEveryRunCmd)
      delete _clearEveryRunCmd;
   if (_searchDialog)
      delete _searchDialog;
   if (_saveOutputCmd)
      delete _saveOutputCmd;
   if (_searchCmd)
      delete _searchCmd;
}

Widget LabelWindow::createWorkArea( Widget parent )
{ 
   setDefaultResources(parent, _defaults);

   _form = XtVaCreateWidget( "workArea", 
			     xmFormWidgetClass, parent,
			     NULL );
   _textArea = _view->createTextArea( _form );
  
   XtVaSetValues( XtParent( _textArea ),
		  XmNtopAttachment, 	XmATTACH_FORM,
		  XmNleftAttachment,	XmATTACH_FORM,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  NULL );

   return( _form );
}

void LabelWindow::createMenuPanes( )
{

   // "Label" pulldown menu
  
   MenuCmdList *labelCmdList;
   labelCmdList = new MenuCmdList( "Label" );
  
   ImageLabel *labelRoot = _textM->getImage()->getLabelRoot();

   setLabelMenu(labelCmdList, labelRoot);
  
   _menuBar->addCommands ( labelCmdList, FALSE );
  
   delete labelCmdList;

 
   // "File" pulldown menu
   
   MenuCmdList *fileCmdList;
   fileCmdList = new MenuCmdList( "File" );
  
   _saveOutputCmd = new SgSaveTextWidgetCmd( "saveLabel", True, _textArea );
  
   Cmd *postOutputWinCmd = new PostSingleFileDialogCmd( "Save Label",
						       True, _saveOutputCmd);
   fileCmdList->add( postOutputWinCmd );
  
   _menuBar->addCommands ( fileCmdList, FALSE );
  
   delete fileCmdList;
 
   // "Options" pulldown menu
  
   MenuCmdList *optionsCmdList;
   optionsCmdList = new MenuCmdList( "Options" );
  
   _clearOutputCmd = new ClearLabelCmd( "Clear Output", True, _textM );
   optionsCmdList->add( _clearOutputCmd );

   _clearEveryRunCmd = new LabelClearEveryRunCmd( "Clear Output On Every Run",
                                               True, _view );
   optionsCmdList->addCheckBox( _clearEveryRunCmd );

   _searchCmd = new SgSearchTextWidgetCmd ("Search Widget",
					  True, _textArea);

   _searchDialog = new SgSearchTextDialog("Text Search", _searchCmd);

   Cmd *postSearchWindowCmd = new PostDialogCmd( "Find", True, _searchDialog );

   optionsCmdList->add( postSearchWindowCmd );
  
   _searchAgainCmd = new SgSearchAgainCmd("Find Next", False, _searchCmd);
   optionsCmdList->add(_searchAgainCmd);

   _menuBar->addCommands ( optionsCmdList, FALSE );
  
   delete optionsCmdList;
}


///////////////////////////////////////////////
// Set up the Label menu
///////////////////////////////////////////////
void LabelWindow::setLabelMenu(MenuCmdList *cmdList, ImageLabel *label)
{
   char key[MAX_IMAGE_LABEL_KEY_SIZE+1];
   ListLabelCmd *labelPrintCmd;
   MenuCmdList *labelSubList;

   if (!label) return;
   int childNumb = label->getChildNumb();
   if (childNumb > 0) {
      if (childNumb > 1) {
         for (int i=0; i<childNumb; i++) {
            // recursivly set up 
            ImageLabel *childLabel = label->getChildLabel(i);
            if (childLabel->getChildNumb() > 0) {
               labelSubList = 
                       new MenuCmdList(childLabel->getLabelName());
               setLabelMenu(labelSubList, label->getChildLabel(i));
               cmdList->addSubmenu(labelSubList);
            }
            else {
               strcpy(key, childLabel->getLabelKey());
               labelPrintCmd = new ListLabelCmd(childLabel->getLabelName(), 
                            TRUE, key, _textM);
               cmdList->addButton(labelPrintCmd);
            }
         }
      }
      else 
         setLabelMenu(cmdList, label->getChildLabel(0));
   }  
   else {           // leaf node
      strcpy(key, label->getLabelKey());
      labelPrintCmd = new ListLabelCmd(label->getLabelName(), 
                   TRUE, key, _textM);
      cmdList->addButton(labelPrintCmd);
   }

   return;
}

      
/////////////////////////////////////////////////////////
// Display the default formatted label list
/////////////////////////////////////////////////////////
void LabelWindow::setDefault()
{
   StatusType status;
   char *label;
   int maxsize;

   label = new char[1];
   label[0] = '\0';
   maxsize = 0;
   status = _textM->getImage()->getLabelSetValue(label, NULL, &maxsize);
   if (label) {
      _textM->setText(label, strlen(label));
      delete[] label;
   }
}

/////////////////////////////////////////////////////////
// Update the menubar when a new image is loaded
/////////////////////////////////////////////////////////
void LabelWindow::reset()
{
   Dimension width, height;
   Position currentX, currentY;

   XtVaGetValues( _textArea,
                  XmNwidth, &width,
                  XmNheight, &height,
                  XmNx, &currentX,
                  XmNy, &currentY,
                  NULL);

   if (_menuBar)
      delete _menuBar;

   // Specify the base widget of a MenuBar object
   // the XmMainWindow widget's menu bar.

   _menuBar = new MenuBar ( _main, "menubar" );

   XtVaSetValues ( _main,
                  XmNmenuBar, _menuBar->baseWidget(),
                  NULL);

   // Call derived class hook to add panes to the menu

   createMenuPanes();

   _menuBar->manage();

   XtVaSetValues (_textArea,
                  XmNwidth, width,
                  XmNheight, height,
                  XmNx, currentX,
                  XmNy, currentY,
                  NULL);

   _textM->clear();

   setDefault();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TextDisplayModel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//
//    TextDisplayModel.cc: 
//
//    This is the model class for text display .
//
///////////////////////////////////////////////////////////////////
#include "TextDisplayModel.h"
#include "TextDisplayView.h"
#include "ViewMacros.h"
#include <assert.h>

TextDisplayModel::TextDisplayModel(ImageData *imageData)
{
   _image = imageData;
   _text = NULL;
   _numViews = 0;
   _views = NULL;
}

TextDisplayModel::~TextDisplayModel()
{
   delete [] _text;
   delete [] _views;
}

void TextDisplayModel::attachView(TextDisplayView *view)
{
   AttachViewMacro(TextDisplayView, _views, _numViews, view);
   view->update();
}

void TextDisplayModel::detachView(TextDisplayView *view)
{
   DetachViewMacro(TextDisplayView, _views, _numViews, view);
}
 
void TextDisplayModel::updateViews(char *newText, int size, TextStyle style)
{
   for (int i = 0; i < _numViews; i++)
      _views[i]->addText(newText, size, style);
}

void TextDisplayModel::updateViews()
{
   for (int i = 0; i < _numViews; i++)
      _views[i]->update();
}

void TextDisplayModel::setText(char *newText, int size, TextStyle style)
{
   if (_text && (strlen(_text) > 0)) {
      char *tempText = new char [strlen(_text) + size + 1];
      strcpy(tempText, _text);
      strncat( tempText, newText, size);
      delete [] _text;
      _text = tempText;
   }
   else {
      delete [] _text;
      _text = new char[size+1];
      strncpy(_text, newText, size);
      _text[size] = '\0';
   }
    
   updateViews(newText, size, style);
}

void TextDisplayModel::clear()
{
   if (_text) {
      delete [] _text;
      _text = NULL;
   }
    
   updateViews();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TextDisplayView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
//
//    TextDisplayView.cc
//
//    This is a class derived from BasicImageView.
//    It is the view for text output.
//
/////////////////////////////////////////////////////////////
#include "TextDisplayView.h"
#include "UIComponent.h"
#include <Xm/Text.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

XtResource TextDisplayView::_resources[] = {
   {
      (char *)"newRunString",
      (char *)"NewRunString",
      XmRString,
      sizeof ( String ),
      XtOffset ( TextDisplayView *, _newRunString ),
      XmRString,
      ( XtPointer ) "--------------------------------------------------------------------------------\n",
   },
};

String TextDisplayView::_defaults[] = {
   (char *)"*text.editMode:			XmMULTI_LINE_EDIT",
   (char *)"*text.rows:			20",
   (char *)"*text.columns:			60",
   (char *)"*text.cursorPositionVisible:	False",
   (char *)"*text.editable:			False",
    NULL,
};

TextDisplayView::TextDisplayView(const char *name, ImageData *image)
	: BasicImageView( name, image )
{
   _textM = NULL;
   _textW = NULL;
   _clearEveryRun = True;
}

TextDisplayView::~TextDisplayView()
{
   if (_textW) XtDestroyWidget(_textW);
   if (_textM) _textM->detachView(this);
}

void TextDisplayView::update()
{
   if (_textW && _textM) {
      if (_textM->getText())
         XmTextSetString(_textW, _textM->getText());
      else 
         XmTextSetString(_textW, '\0');
	
      XmTextShowPosition(_textW, XmTextGetLastPosition(_textW));
   }
}

Widget TextDisplayView::createTextArea ( Widget parent )
{
   // Load the default resources into the database

   setDefaultResources ( parent, _defaults );
   XtGetApplicationResources(parent, (XtPointer)this,
		      _resources, XtNumber(_resources), NULL, 0);
    
   _textW = XmCreateScrolledText ( parent, (char *)"text", NULL, 0 );

   update();
    
   XtManageChild (_textW);
    
   return _textW;
}

void TextDisplayView::registerTextModel(TextDisplayModel *model)
{
   if (!model) return;
   _textM = model;
   _textM->attachView (this);
}

void TextDisplayView::unregisterTextModel()
{
   if (_textM)
      _textM->detachView(this);
   _textM = NULL;
}

void TextDisplayView::addText(char *newText, int size, TextStyle style)
{
   if (_textW) {
      if ( _clearEveryRun ) 
         XmTextSetString(_textW, '\0');

      XmTextPosition curPos = XmTextGetLastPosition(_textW);
      XmTextInsert(_textW, curPos, newText);
      XmTextPosition newCurPos = curPos + size;
      XmTextShowPosition(_textW, newCurPos);

      if (style == HIGHLIGHT)
         XmTextSetHighlight(_textW, curPos, newCurPos, 
		       XmHIGHLIGHT_SELECTED);
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToLabelGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//
//    ImageToLabelGlue.cc: 
//
//    This is a class derived form BasicImageView that serves as 
//    a "glue" class between an ImageData object and a TextDisplayModel object.
//
//    This class, though a UIComponent, creates no widget, and therefore
//    should never be managed.
//
////////////////////////////////////////////////////////////////////////
#include "ImageToLabelGlue.h"
#include "ImageData.h"
#include "LabelCmd.h"

ImageToLabelGlue::ImageToLabelGlue (ImageData *model, LabelCmd *labelCmd)
   : BasicImageView("glue", model)
{
   _labelCmd = labelCmd;
 
   _model->attachView(this);
}

ImageToLabelGlue::~ImageToLabelGlue ( )
{
   // Detach itself from the model so that the are no more updates sent
   _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// Whenever the image changes, reset the LabelCmd's image.   
////////////////////////////////////////////////////////////////////////
void ImageToLabelGlue::update()
{
   if (_model->isDataSourceOpened()) {
      _labelCmd->setImage(_model);
      if (_labelCmd->isCreated()) {
         _labelCmd->resetLabelWindow();
      }
   }
}

void ImageToLabelGlue::updatePart(int /* flags */) { }
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LabelClearEveryRunCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   LabelClearEveryRunCmd.cc: 
//
//   This is a class derived from Cmd.
//   It sets the toggled value for clearing label output
//   everytime new output is generated.
//
///////////////////////////////////////////////////////////////
#include "LabelClearEveryRunCmd.h"
#include "TextDisplayView.h"
#include <Xm/Xm.h>   

LabelClearEveryRunCmd::LabelClearEveryRunCmd ( const char *name, int active, TextDisplayView *textW) 
	: Cmd ( name, active, (CmdValue)1 )
{
   _view = textW;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void LabelClearEveryRunCmd::doit()
{
   if ( _value )
      _view->setClearEveryRun( True );
   else
      _view->setClearEveryRun( False );
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void LabelClearEveryRunCmd::undoit()
{
   _value = (CmdValue)(!_value);
   newValue( );
   if ( _value )
      _view->setClearEveryRun( True );
   else
      _view->setClearEveryRun( False );

}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create labelview.imake
#define SUBROUTINE labelview
#define MODULE_LIST \
        LabelCmd.cc ListLabelCmd.cc ClearLabelCmd.cc LabelWindow.cc \
        TextDisplayModel.cc TextDisplayView.cc ImageToLabelGlue.cc \
        LabelClearEveryRunCmd.cc

#define USES_C_PLUS_PLUS

#define GUI_SUBLIB

#define LIB_MOTIFAPP
#define LIB_MOTIF

#if 0
#define DEBUG
#define LIB_LOCAL
#endif
$ Return
$!#############################################################################
