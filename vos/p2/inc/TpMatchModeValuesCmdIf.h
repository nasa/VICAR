//////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmdIf.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODEVALUESCMDIF_H
#define TPSETMATCHMODEVALUESCMDIF_H
#include "CmdInterface.h"

class KeyinView;

class TpSetMatchModeValuesCmdIf : public CmdInterface {

  protected:

    KeyinView *_pmk;
    KeyinView *_lsm;
    KeyinView *_sw;
    KeyinView *_accuracy;
    KeyinView *_threshold;

    static void setPMKCallback(Widget, XtPointer, XtPointer);
    static void setLSMCallback(Widget, XtPointer, XtPointer);
    static void setSWCallback(Widget, XtPointer, XtPointer);
    static void setAccuracyCallback(Widget, XtPointer, XtPointer);
    static void setThresholdCallback(Widget, XtPointer, XtPointer);

  public:

    TpSetMatchModeValuesCmdIf(Widget parent, Cmd *cmd);
    virtual ~TpSetMatchModeValuesCmdIf() { }

    void setValue(CmdValue);

    void setPMK();
    void setLSM();
    void setSW();
    void setAccuracy();
    void setThreshold();
};

#endif
