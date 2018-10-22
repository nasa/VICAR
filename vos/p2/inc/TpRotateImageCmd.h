////////////////////////////////////////////////////////////
// TpRotateImageCmd.h: Rotate image.
////////////////////////////////////////////////////////////
#ifndef TpRotateImageCmd_H
#define TpRotateImageCmd_H
#include "RadioCmd.h"
#include "RotationDefs.h"

class TpSubDisplayer;

class TpRotateImageCmd : public RadioCmd {

  protected:

    TpSubDisplayer *_subDisplayer;
    RotationType _rotation;

    virtual void doit();
    
  public:

    TpRotateImageCmd(const char *, int, CmdList *, TpSubDisplayer *, RotationType);

    virtual const char *const className () { return "TpRotateImageCmd"; }
};
#endif
