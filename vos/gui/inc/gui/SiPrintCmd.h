/////////////////////////////////////////////////////////////
// SiPrintCmd.h:  Prints the image according to the SiPrintCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiSaveAsCmd, adding the SiPrintCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! print.  It should be done internally, via save hooks in ImageData.
/////////////////////////////////////////////////////////////
#ifndef SIPRINTCMD_H
#define SIPRINTCMD_H
#include "SiSaveAsCmd.h"

class SiPrintCmd : public SiSaveAsCmd {

 protected:

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   // cleanup() not needed...

 public:

   SiPrintCmd(const char *name, int active, Widget xiw,
		ImageData *image, char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiPrintCmd"; }
};
#endif

