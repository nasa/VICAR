/////////////////////////////////////////////////////////////////
// ZoomConstantCmd.h
//
//	Is a (indirect) subclass of RadioCmd.  Used for reading in the
//	zoom values in the constructor and passing the
//	values in a (CmdValue) ZoomFactor structure
//	to its parent class.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMCONSTANTCMD_H
#define ZOOMCONSTANTCMD_H
#include "ZoomBaseCmd.h"
#include "ZoomFactor.h"
 
class BasicImageView;
class ZoomSpecialCmd;

class ZoomConstantCmd :  public ZoomBaseCmd  {
 
 protected:
 
   BasicImageView *_imageView;
   ZoomFactor _zoomValue;

   virtual void	doit();
 
 public:

   // Constructor with zoom values in arg list

   ZoomConstantCmd(const char * name, int active,  int zoomIn, int zoomOut, 
        	CmdList * radioCmdList, CmdValue startState,
		ZoomSpecialCmd *zSC, BasicImageView * imageView ); 

   virtual ~ZoomConstantCmd();
   virtual const char *const className() { return ("ZoomConstantCmd"); }
 
};
#endif

