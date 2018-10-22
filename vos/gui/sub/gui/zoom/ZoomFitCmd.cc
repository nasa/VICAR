/////////////////////////////////////////////////////////////////
// ZoomFitCmd.cc
//	
//	This class is a zoom control for performing a 
//	zoom-to-fit on the image.
//	Unlike ZoomConstantCmd,
//	it doesn't need a value to perform its function.
//	It implements the doit() and undoit() pure virtual
//	functions in its parent class: Cmd.
//
////////////////////////////////////////////////////////////////
#include "ZoomFitCmd.h"
#include "BasicImageView.h"

ZoomFitCmd::ZoomFitCmd(const char *name, int active, CmdList *radioCmdList,
			CmdValue startState, ZoomSpecialCmd *zSC,
			BasicImageView *imageView)
		: ZoomBaseCmd(name, active, startState, radioCmdList, zSC)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// doit()
// Set zoom-to-fit mode.  We don't need to worry about Undo because
// RadioCmd handles it for us.
////////////////////////////////////////////////////////////////
void ZoomFitCmd::doit()
{
   // Do zoom if radio cmd is True

   if (_value) {
      _imageView->setUserZoom2Fit();
   }	
} 

