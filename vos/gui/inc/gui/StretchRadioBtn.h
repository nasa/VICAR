//////////////////////////////////////////////////////////////
// StretchRadioBtn.h: A component class to show a Stretch type radio button
/////////////////////////////////////////////////////////////
#ifndef STRETCHRADIOBTN_H
#define STRETCHRADIOBTN_H
#include "UIComponent.h"
#include "StretchValue.h"  // For StretchType definition

class StretchCmdInterface;

class StretchRadioBtn : public UIComponent {

  private:

    static void valueChangedCallback(Widget, XtPointer, XtPointer);

  protected:

    StretchType _type;
    StretchCmdInterface *_stretchCmdInterface;

  public:

    StretchRadioBtn(Widget, const char *, StretchType,
		    StretchCmdInterface *);

    virtual void valueChanged();
    virtual void setValue(StretchType);

    virtual const char *const className() { return "StretchRadioBtn"; }
};
#endif

