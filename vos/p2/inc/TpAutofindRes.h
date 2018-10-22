//////////////////////////////////////////////////////////////////////////////
// TpAutofindResults
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDRESULTS_H
#define TPAUTOFINDRESULTS_H
#include "UIComponent.h"

class TpMatchManager;
class KeyinView;

class TpAutofindResults : public UIComponent {

  protected:

    TpMatchManager *_matchManager;
    KeyinView *_labels[6];

  public:

    TpAutofindResults(Widget, const char *, TpMatchManager *);
    virtual ~TpAutofindResults();

    void setValues(double [6]);

    virtual const char *const className() { return "TpAutofindResults"; }

};

#endif
