/////////////////////////////////////////////////////////////
// LatLonBarCmd.h: (un)display the Lat/Lon Bar
/////////////////////////////////////////////////////////////
#ifndef LATLONBARCMD_H
#define LATLONBARCMD_H
#include "Cmd.h"

class ImageDisplayer;

class LatLonBarCmd : public Cmd {
  private:
    int _oldValue;    // for nostalgic purposes, we keep the old value
    ImageDisplayer *_imageView;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    LatLonBarCmd( const char *, int, ImageDisplayer * );
    virtual const char *const className () { return "LatLonBarCmd"; }
};
#endif
