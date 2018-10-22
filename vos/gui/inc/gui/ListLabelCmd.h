///////////////////////////////////////////////////////////////
//
//   ListLabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It displays image labels.
//
///////////////////////////////////////////////////////////////
#ifndef LISTLABELCMD_H
#define LISTLABELCMD_H
#include "ImageLabel.h"
#include "NoUndoCmd.h"

class TextDisplayModel;

class ListLabelCmd : public NoUndoCmd {

  protected:

    char *_key;

    char *_value;

    int _maxLabelSize;

    TextDisplayModel *_textM;

    virtual void doit();

  public:

    ListLabelCmd( const char *name, int active, char *key,
					TextDisplayModel *textM );

    virtual void freeValue (CmdValue);

    virtual const char *const className() { return ("ListLabelCmd"); }

};
#endif
