/////////////////////////////////////////////////////////////
// KeyinViewBlanked.h: A component class to show keyin fields where
// the keyin field is blanked out (e.g. for a password)
/////////////////////////////////////////////////////////////
#ifndef KEYINVIEWBLANKED_H
#define KEYINVIEWBLANKED_H

#include "KeyinView.h"

class KeyinViewBlanked : public KeyinView {

  private:

    static void textModCallback ( Widget, XtPointer, XtPointer );

  protected:

    char *_blanked_text;
    char _blank_char;

  public:

    KeyinViewBlanked ( Widget, const char *, char blank_char = ' ');
    virtual ~KeyinViewBlanked();

    virtual void textMod(XtPointer);

    void setFieldValue(char *text);
    char *getFieldValue();

    virtual const char *const className() { return "KeyinViewBlanked"; }
};
#endif

