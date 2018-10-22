///////////////////////////////////////////////////////////////////
// TpContrastCmd.h
///////////////////////////////////////////////////////////////////
#ifndef TPCONTRASTCMD_H
#define TPCONTRASTCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class ImageData;
class TpWedgeOverlayView;

class TpContrastCmd : public Cmd {

  protected:

    Widget _iw;
    Widget _ziw;
    Widget _pan;

    void doit();
    void undoit() { }

  public:

    TpContrastCmd(const char *name, int active, Widget iw, Widget ziw, Widget pan,
		      ImageData *image);
    virtual ~TpContrastCmd() { }

    virtual const char *const className () { return "TpContrastCmd"; }
};

#endif
