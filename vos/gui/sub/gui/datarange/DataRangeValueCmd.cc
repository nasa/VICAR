////////////////////////////////////////////////////////////
// DataRangeValueCmd.cc: Sets the data range to the value specified
// in the string CmdValue.  Also, turns off auto mode.
///////////////////////////////////////////////////////////
#include "DataRangeValueCmd.h"
#include "ImageData.h"
#include "UIComponent.h"		// only for strdup()
#include <stdlib.h>
#include <stdio.h>

DataRangeValueCmd::DataRangeValueCmd(const char *name, int active,
				ImageData *image, Cmd *autoCmd, Boolean isMax)
		: Cmd(name, active)
{
   char buf[30];
   double val;

   _image = image;
   _autoCmd = autoCmd;
   _isMax = isMax;

   // Get current value

   if (isMax)
      val = _image->getMaxDataRange();
   else
      val = _image->getMinDataRange();

   _lastValue = val;
   sprintf(buf, "%g", val);
   _value = (CmdValue)strdup(buf);
   newValue();
}

void DataRangeValueCmd::doit()
{
   double val;

   if (_isMax)					// save for undo
      _lastValue = _image->getMaxDataRange();
   else
      _lastValue = _image->getMinDataRange();
   _lastAuto = _autoCmd->getValue();

   if (_value)
      val = atof((char *)_value);
   else
      val = 0.0;

   if (_isMax)
      _image->setDataMax(val);
   else
      _image->setDataMin(val);
   _autoCmd->execute((CmdValue) False);		// turn off auto
}

void DataRangeValueCmd::undoit()
{
   char buf[30];

   _autoCmd->execute(_lastAuto);

   sprintf(buf, "%g", _lastValue);
   _value = (CmdValue)strdup(buf);
   newValue();
   doit();
}

