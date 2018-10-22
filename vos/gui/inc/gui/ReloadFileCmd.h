/////////////////////////////////////////////////////////
// ReloadFileCmd: A Command class, the purpose of which
// is simply to call execute(filename) on a LoadFileCmd
// object.
/////////////////////////////////////////////////////////
#ifndef RELOADFILECMD_H
#define RELOADFILECMD_H

#include "NoUndoCmd.h"

class ImageToReloadGlue;
class ImageData;

class ReloadFileCmd : public NoUndoCmd {

 protected:

   ImageData *_image;
   ImageToReloadGlue *_glue;
   Cmd *_loadFileCmd;

 public:

   ReloadFileCmd(const char*, int, ImageData*, Cmd *loadFileCmd);
   ~ReloadFileCmd();

   virtual void doit();
    
   virtual const char *const className () { return "ReloadFileCmd"; }
};
#endif
