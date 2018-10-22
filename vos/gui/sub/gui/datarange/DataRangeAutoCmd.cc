////////////////////////////////////////////////////////////
// DataRangeAutoCmd.h: Set or clear automatic determination of
// data range in the image model.
///////////////////////////////////////////////////////////
#include "DataRangeAutoCmd.h"
#include "ImageData.h"
#include <stdint.h>

DataRangeAutoCmd::DataRangeAutoCmd(const char *name,int active,ImageData *image,
					Boolean isMax)
		: Cmd(name, active)
{
   _image = image;
   _isMax = isMax;

   // Check current auto state.

   if (isMax)
      _value = (CmdValue) ((uintptr_t) _image->isMaxAuto());
   else
      _value = (CmdValue) ((uintptr_t) _image->isMinAuto());

   _lastValue = _value;
   newValue();
}

void DataRangeAutoCmd::doit()
{
   if (_isMax) {
      _lastValue = (CmdValue) ((uintptr_t) _image->isMaxAuto());	// save for undo
      _image->setMaxAuto((_value != 0));
   }
   else {
      _lastValue = (CmdValue) ((uintptr_t) _image->isMinAuto());	// save for undo
      _image->setMinAuto((_value != 0));
   }
}

void DataRangeAutoCmd::undoit()
{
   _value = _lastValue;
   newValue();
   doit();
}

