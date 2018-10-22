/////////////////////////////////////////////////////////////
// SiSaveAsCmd.h:  Saves the image according to the SiSaveCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiRunStretchScriptCmd, adding the SiSaveCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! save.  It should be done internally, via save hooks in ImageData.
/////////////////////////////////////////////////////////////
#ifndef SISAVEASCMD_H
#define SISAVEASCMD_H
#include "SiRunStretchScriptCmd.h"

class SiSaveAsCmd : public SiRunStretchScriptCmd {

 protected:

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   // cleanup() not needed...

 public:

   SiSaveAsCmd(const char *name, int active, Widget xiw,
		ImageData *image, const char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiSaveAsCmd"; }
};
#endif

