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
