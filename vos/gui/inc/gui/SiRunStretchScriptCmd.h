/////////////////////////////////////////////////////////////
// SiRunStretchScriptCmd.h:  Runs a script (usually determined
// by resources) that is passed information about the state of
// the image display.  This subclass adds information about the
// stretch and pseudocolor tables to the basic SiRunScriptCmd.
/////////////////////////////////////////////////////////////
#ifndef SIRUNSTRETCHSCRIPTCMD_H
#define SIRUNSTRETCHSCRIPTCMD_H
#include "SiRunScriptCmd.h"
#include <Xm/Xm.h>

class Lut;

class SiRunStretchScriptCmd : public SiRunScriptCmd {

 protected:

   Lut *_lutRed, *_lutGrn, *_lutBlu;
   Lut *_pseudoRed, *_pseudoGrn, *_pseudoBlu;

   char _stretch_file[256], _pseudo_file[256];

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   virtual void cleanup();

 public:

   SiRunStretchScriptCmd(const char *, int, Widget xiw, ImageData *,
		const char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual const char *const className () { return "SiRunStretchScriptCmd"; }
};
#endif

