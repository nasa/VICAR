/////////////////////////////////////////////////////////////
// DataRangeValueCmd.h: Sets the data range to the value specified
// in the string CmdValue.  Also, turns off auto mode.
/////////////////////////////////////////////////////////////
#ifndef DATARANGEVALUECMD_H
#define DATARANGEVALUECMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// Only for Boolean!!

class ImageData;

class DataRangeValueCmd : public Cmd {

 protected:

   double _lastValue;		// for undo
   CmdValue _lastAuto;		// for undo

   ImageData *_image;
   Cmd *_autoCmd;
   Boolean _isMax;		// True: max range, False: min range

   virtual void doit();   
   virtual void undoit(); 

 public:

   DataRangeValueCmd(const char *, int, ImageData *, Cmd *, Boolean isMax);

   virtual void freeValue(CmdValue value) { if (value) XtFree((char *)value); }

   virtual const char *const className () { return "DataRangeValueCmd"; }
};
#endif

