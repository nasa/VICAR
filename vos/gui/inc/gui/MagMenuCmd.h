/////////////////////////////////////////////////////////////
// MagMenuCmd.h: Include file to handle the Pan command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef MAGMENUCMD_H
#define MAGMENUCMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>

class ImageData;
class MagTool;
class Lut;
class CursorPositionView;
class CursorDnView;
class MagInfo;

class MagMenuCmd : public NoUndoCmd {

  private:

     ImageData *_imageData;
     Widget _iw;
     Widget _parent;
     Lut *_rlut, *_glut, *_blut;
     Lut *_rpseudo, *_gpseudo, *_bpseudo;
     CursorPositionView *_posView;
     CursorDnView *_dnView;
     MagInfo *_magInfo;

     MagTool *_magTool;

  protected:
    
    virtual void doit();

  public:

    MagMenuCmd(const char *, int, Widget parent, ImageData *, Widget iw,
		Lut *, Lut *, Lut *, Lut *, Lut *, Lut *,
		MagInfo *magInfo, CursorPositionView *, CursorDnView *);
    virtual ~MagMenuCmd();

    virtual const char *const className () { return "MagMenuCmd"; }
};
#endif
