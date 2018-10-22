////////////////////////////////////////////////////////////
// SiRotateImageCmd.h: Rotate image.
////////////////////////////////////////////////////////////
#ifndef SiRotateImageCmd_H
#define SiRotateImageCmd_H
#include "RadioCmd.h"
#include "RotationDefs.h"

class RotatedImageData;

class SiRotateImageCmd : public RadioCmd {

  protected:

    RotationType _rotation;
    RotatedImageData *_image;

    virtual void doit();
    
  public:

    SiRotateImageCmd(const char *, int, CmdList *,
					RotatedImageData *, RotationType);

    virtual const char *const className () { return "SiRotateImageCmd"; }
};
#endif
