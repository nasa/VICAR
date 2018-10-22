//////////////////////////////////////////////////////////////////////////////
// TpAddCorrParmAsPntQualInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAddCorrParmAsPntQualINTERFACE_H
#define TPAddCorrParmAsPntQualINTERFACE_H
#include "CmdInterface.h"

class TpQualFormatValue;

class TpAddCorrParmAsPntQualInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    static void addCorrParmAsPntQualCallback(Widget, XtPointer, XtPointer);

  public:

    TpAddCorrParmAsPntQualInterface(Widget parent, Cmd *cmd);
    virtual ~TpAddCorrParmAsPntQualInterface() { }

    void setValue(CmdValue);

    void addCorrParmAsPntQual();

};

#endif
