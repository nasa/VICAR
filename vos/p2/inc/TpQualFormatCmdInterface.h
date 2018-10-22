//////////////////////////////////////////////////////////////////////////////
// TpQualFormatCmdInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATCMDINTERFACE_H
#define TPQUALFORMATCMDINTERFACE_H
#include "CmdInterface.h"
#include "TpQualifier.h"

class TpQualFormatValue;
class TpQualFormatView;
class KeyinView;

class TpQualFormatCmdInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    KeyinView *_numQualsView;
    TpQualFormatView *_qualView;

    static void addQualCallback(Widget, XtPointer, XtPointer);
    static void deleteQualCallback(Widget, XtPointer, XtPointer);
    static void setNumQualsCallback(Widget, XtPointer, XtPointer);

  public:

    TpQualFormatCmdInterface(Widget parent, Cmd *cmd);
    virtual ~TpQualFormatCmdInterface() { }

    void setValue(CmdValue);

    void addQual();
    void setNumQuals();
    void deleteQual();

    void setName(char *name, int numQual);
    void setUnit(char *unit, int numQual);
    void setType(TpQualType t, int numQual);
};

#endif
