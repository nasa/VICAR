/////////////////////////////////////////////////////////////
// DataRangeAutoCmd.h: Set or clear automatic determination of
// data range in the image model.
/////////////////////////////////////////////////////////////
#ifndef DATARANGEAUTOCMD_H
#define DATARANGEAUTOCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// Only for Boolean!!

class ImageData;

class DataRangeAutoCmd : public Cmd {

 protected:

   CmdValue _lastValue;		// for undo
   ImageData *_image;
   Boolean _isMax;		// True: max range, False: min range

   virtual void doit();   
   virtual void undoit(); 

 public:

   DataRangeAutoCmd(const char *, int, ImageData *, Boolean isMax);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "DataRangeAutoCmd"; }
};
#endif

