//////////////////////////////////////////////////////////////////////////////
// TpAddIdAsGenQualInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAddIdAsGenQualINTERFACE_H
#define TPAddIdAsGenQualINTERFACE_H
#include "CmdInterface.h"

class TpQualFormatValue;

class TpAddIdAsGenQualInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    static void addIdAsGenQualCallback(Widget, XtPointer, XtPointer);

  public:

    TpAddIdAsGenQualInterface(Widget parent, Cmd *cmd);
    virtual ~TpAddIdAsGenQualInterface() { }

    void setValue(CmdValue);

    void addIdAsGenQual();

};

#endif
