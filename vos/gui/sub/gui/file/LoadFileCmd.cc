////////////////////////////////////////////////////////////////////////
// LoadFileCmd: A Command class that loads a file.  The Command value
// is a dynamically allocated single string, suitable for passing in to
// an ImageData subclass.  However, surrounding parentheses, if present,
// are removed (note: this modifies the input string!)
////////////////////////////////////////////////////////////////////////
#include "LoadFileCmd.h"
#include "ImageData.h"
#include "ErrorDialogManager.h"
#include <assert.h>

LoadFileCmd::LoadFileCmd(const char *name, int active, ImageData *imageData)
		: NoUndoCmd(name, active)
{
   _imageData = imageData;
}

void LoadFileCmd::doit()
{
   StatusType status;

   assert(_imageData != NULL);

   if (_imageData->isDataSourceOpened())
      _imageData->close();

   char *p = (char *)_value;
   if (*p == '(' && *(p+strlen(p)-1) == ')') {		// remove parentheses
      *(p+strlen(p)-1) = '\0';
      status = _imageData->open((char *)_value+1);
   }
   else
      status = _imageData->open((char *)_value);

   if (status != imSUCCESS) {
      if (!_imageData->errorMsgIssued()) {
         theErrorDialogManager->post(_imageData->getErrorMsg());
      }
   }
}

