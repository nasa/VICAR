///////////////////////////////////////////////////////
// TpImageReference
///////////////////////////////////////////////////////
#ifndef TpImageReference_H
#define TpImageReference_H
#include "UIComponent.h"

class TpImageReference : public UIComponent {

  private:

  protected:

    int _numLabels;
    Widget *_labels1;
    Widget *_labels2;

    Pixel _fg, _bg;
    char *_bgVisible;
    char *_fgLoaded;

    void setToInvisible(Widget w);
    void setToVisible(Widget w);
    void indicateLoadedImage(Widget w);

  public:

    TpImageReference(Widget, const char *);
    virtual ~TpImageReference();

    void setToVisible(int i);
    void setAllToInvisible();
    void setReferencedImage(int i);
    void indicateLoadedImage(int i);

    virtual const char *const className() { return "TpImageReference"; }
};

#endif
