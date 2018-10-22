///////////////////////////////////////////////////
// StretchListCmd.h
///////////////////////////////////////////////////
#ifndef STRETCHLISTCMD_H
#define STRETCHLISTCMD_H
#include "NoUndoCmd.h"
#include "TableValue.h"
#include "StretchValue.h"

class StretchCmdInterface;

class StretchListCmd : public NoUndoCmd {

  protected:

    StretchCmdInterface *_stretchCmdInterface;
    StretchType _stretchType;

  public:

    StretchListCmd(const char *, int, StretchCmdInterface *, StretchType);

    virtual void doit();

    void freeValue(CmdValue);
};
#endif

