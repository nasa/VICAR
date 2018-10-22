/////////////////////////////////////////////////////////////////
// ZoomCmd.cc
//	
//	ZoomCmd performs a zoom on the image. 
//	This is subclass of Cmd which
//	implements the doit() and undoit() pure virtual 
//	functions.
//
////////////////////////////////////////////////////////////////
#include "ZoomCmd.h"
#include "ZoomFactor.h"
#include "BasicImageView.h"

ZoomCmd::ZoomCmd(const char *name, int active, BasicImageView *imageView)
		: Cmd(name, active)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// doit()
// Perform the requested zoom on the image
////////////////////////////////////////////////////////////////
void ZoomCmd::doit()
{

   // Save previous zoom

   ZoomFactor &zoom = _imageView->getImageZoom();
   _undoXin  = zoom.getXIn();
   _undoXout = zoom.getXOut();
   _undoYin  = zoom.getYIn();
   _undoYout = zoom.getYOut();

   // Now do new zoom

   if (!_value)			// in case we're executed with NULL
      return;

   ZoomFactor *z = (ZoomFactor *)_value;
   _imageView->setUserZoom(*z);	
} 

////////////////////////////////////////////////////////////////
// undoit()
// Undo the previous zoom
////////////////////////////////////////////////////////////////
void ZoomCmd::undoit()
{
   ZoomFactor zoom(_undoXin, _undoYin, _undoXout, _undoYout);
   _imageView->setUserZoom(zoom);
} 

////////////////////////////////////////////////////////////////
// Free the ZoomFactor value
////////////////////////////////////////////////////////////////
void ZoomCmd::freeValue(CmdValue value)
{
   ZoomFactor *z = (ZoomFactor *)value;
   if (z)
      delete z;
}

////////////////////////////////////////////////////////////////
// Returns the Undo zoom factor
////////////////////////////////////////////////////////////////
ZoomFactor ZoomCmd::getUndoZoom()
{
   ZoomFactor zoom(_undoXin, _undoYin, _undoXout, _undoYout);
   return zoom;
}

