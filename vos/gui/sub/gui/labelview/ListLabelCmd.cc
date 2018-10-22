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

