/////////////////////////////////////////////////////////////
// SiRunScriptCmd.h:  Runs a script (usually determined by resources)
// that is passed information about the state of the image display.
/////////////////////////////////////////////////////////////
#ifndef SIRUNSCRIPTCMD_H
#define SIRUNSCRIPTCMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>
#include <stdio.h>

class ImageData;

class SiRunScriptCmd : public NoUndoCmd {

 protected:

   Widget _xiw;
   ImageData *_imageData;

   char *_scriptToRun;

   virtual void doit();   

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   virtual void cleanup() { }			// nothing to do here

 public:

   SiRunScriptCmd(const char *, int, Widget xiw, ImageData *,
						const char *script);

   virtual const char *const className () { return "SiRunScriptCmd"; }
};
#endif

