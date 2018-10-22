//////////////////////////////////////////////////////////////
// StretchCheckBtn.h: A component class to show a Stretch type check button
/////////////////////////////////////////////////////////////
#ifndef STRETCHCHECKBOX_H
#define STRETCHCHECKBOX_H
#include "UIComponent.h"
#include "StretchValue.h" // For StretchType definition

class StretchCmdInterface;

class StretchCheckBox : public UIComponent {

 private:

   static void valueChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   StretchType _type;
   StretchCmdInterface *_stretchCmdInterface;

 public:

   StretchCheckBox(Widget, const char *, StretchType, StretchCmdInterface *);

   virtual void valueChanged();
   virtual void setValue(Boolean);
   virtual StretchType getType() { return _type; }

   virtual const char *const className() { return "StretchCheckBox"; }
};
#endif

