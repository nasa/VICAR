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

