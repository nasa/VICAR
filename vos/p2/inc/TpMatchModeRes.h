//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResults
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODERESULTS_H
#define TPMATCHMODERESULTS_H
#include "UIComponent.h"

class TpMatchManager;
class KeyinView;

class TpMatchModeResults : public UIComponent {

  protected:

    TpMatchManager *_matchManager;
    KeyinView *_labels[5];
    Widget _dumpStdout;

  public:

    TpMatchModeResults(Widget, const char *, TpMatchManager *);
    virtual ~TpMatchModeResults();

    void setValues(float [5]);

    Boolean isDumpToStdout();	// True if we should dump results to stdout too
				// We should really dump in here, but for
				// headers and such we must dump outside.

    virtual const char *const className() { return "TpMatchModeResults"; }

};

#endif
