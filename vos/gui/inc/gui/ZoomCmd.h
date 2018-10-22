/////////////////////////////////////////////////////////////////
// ZoomCmd.h
//	
//	ZoomCmd performs a zoom on the image. 
//	This is subclass of Cmd which
//	implements the doit() and undoit() pure virtual 
//	functions.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMCMD_H
#define ZOOMCMD_H
#include "Cmd.h"

class ZoomFactor;
class BasicImageView;

class ZoomCmd : public Cmd {

 protected:

   BasicImageView *_imageView;
   int _undoXin;
   int _undoXout;
   int _undoYin;
   int _undoYout;

   virtual void doit();
   virtual void undoit();

 public:

   // Constructor with no zoom values - used to defer until 'ok' button pressed
   ZoomCmd(const char *name, int active, BasicImageView *imageView);

   virtual void freeValue(CmdValue);

   virtual ZoomFactor getUndoZoom();

   virtual const char *const className() { return ("ZoomCmd"); }

};
#endif

