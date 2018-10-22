//////////////////////////////////////////////////////////////
// DnValueView.h: A display for a current mark numerical value
///////////////////////////////////////////////////////////////
#ifndef DNVALUEVIEW
#define DNVALUEVIEW
#include "ColorView.h"

class KeyinView;
class ColorModel;

class DnValueView : public ColorView {

  protected:

    KeyinView *_position;

  public:
    
    DnValueView ( Widget, const char * );
    virtual void update ( ColorModel * );

    virtual const char *const className() { return ("DnValueView"); }
};
#endif
