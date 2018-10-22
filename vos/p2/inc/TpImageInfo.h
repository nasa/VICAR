/////////////////////////////////////////////////////////////////////////////
// TpImageInfo.h: This component displays image filename and image number.
/////////////////////////////////////////////////////////////////////////////
#ifndef TPIMAGEINFO_H
#define TPIMAGEINFO_H
#include "UIComponent.h"

class TpImageInfo : public UIComponent {

  protected:

    Widget _imageNumber;

  public:

    TpImageInfo(Widget parent, const char *name, char *filename, int number);
    virtual ~TpImageInfo() { }

    void setNumber(int n);

    virtual const char *const className() { return "TpImageInfo"; }
};

#endif
