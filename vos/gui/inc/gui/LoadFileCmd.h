/////////////////////////////////////////////////////////
// LoadFileCmd: A Command class that loads a file.  The Command value
// is a dynamically allocated single string, suitable for passing in to
// an ImageData subclass.
/////////////////////////////////////////////////////////
#ifndef LOADFILECMD_H
#define LOADFILECMD_H

#include "NoUndoCmd.h"

class ImageData;

class LoadFileCmd : public NoUndoCmd {

 protected:

   ImageData *_imageData;
    
 public:

   LoadFileCmd(const char *, int, ImageData *);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) { if (value) delete[] (char *)value; }

   virtual const char *const className () { return "LoadFileCmd"; }
};
#endif
