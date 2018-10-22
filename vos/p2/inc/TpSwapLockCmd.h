////////////////////////////////////////////////////////////
// TpSwapLockCmd.h: Set image swap lock option on or off.
////////////////////////////////////////////////////////////
#ifndef TpSwapLockCmd_H
#define TpSwapLockCmd_H
#include "Cmd.h"

class TpDisplayer;

class TpSwapLockCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    int _imageNo;

    virtual void doit();
    virtual void undoit();

  public:

    TpSwapLockCmd(const char *, int, TpDisplayer *, int i);

    virtual const char *const className () { return "TpSwapLockCmd"; }
};
#endif
