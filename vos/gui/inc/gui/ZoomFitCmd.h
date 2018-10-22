/////////////////////////////////////////////////////////////////
// ZoomFitCmd.h
//	
//	This class is a zoom control for performing a 
//	zoom-to-fit on the image.
//	Unlike ZoomConstantCmd,
//	it doesn't need a value to perform its function.
//	It implements the doit() and undoit() pure virtual
//	functions in its parent class: Cmd.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMFITCMD_H
#define ZOOMFITCMD_H
#include "ZoomBaseCmd.h"

class BasicImageView;
class ZoomSpecialCmd;

class ZoomFitCmd : public ZoomBaseCmd {

 protected:

   BasicImageView *_imageView;
	
   virtual void doit();

 public:

   ZoomFitCmd(const char *, int, CmdList *, CmdValue, ZoomSpecialCmd *,
			BasicImageView *);

   virtual ~ZoomFitCmd( ) {};
   virtual const char *const className() { return ("ZoomFitCmd"); }

};
#endif

