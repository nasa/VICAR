/////////////////////////////////////////////////////////////////
// ZoomConstantCmd.cc
//
//	Is a (indirect) subclass of RadioCmd.  Used for reading in the
//	zoom values in the constructor and passing the
//	values in a   (CmdValue) ZoomValue structure 
//	to it's parent class.
//
////////////////////////////////////////////////////////////////
#include "ZoomConstantCmd.h"
#include "BasicImageView.h"

////////////////////////////////////////////////////////////////
// Constructor with zoom info
////////////////////////////////////////////////////////////////
ZoomConstantCmd::ZoomConstantCmd(const char *name, int active,
			int zoomIn, int zoomOut, 
			CmdList *radioCmdList, CmdValue startState,
			ZoomSpecialCmd *zSC, BasicImageView *imageView ) 
		: ZoomBaseCmd(name, active, startState, radioCmdList, zSC),
		  _zoomValue(zoomIn, zoomIn, zoomOut, zoomOut)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
ZoomConstantCmd::~ZoomConstantCmd()
{
   // empty
}

////////////////////////////////////////////////////////////////
// doit()
// Perform the requested zoom
////////////////////////////////////////////////////////////////
void ZoomConstantCmd::doit()
{

   // Do zoom only if value is True

   if (_value) {
      _imageView->setUserZoom(_zoomValue);
   }
} 

