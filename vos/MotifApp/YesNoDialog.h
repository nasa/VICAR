///////////////////////////////////////////////////////////////
// YesNoDialog.h: Motif Question dialog that also contains No
// button.  Note that this dialog is not cached, it is created
// at every request and destroys itself when user presses any of
// the buttons.
///////////////////////////////////////////////////////////////
#ifndef YESNODIALOG_H
#define YESNODIALOG_H
#include "UIComponent.h"

typedef void (*DialogCallback) (void *);

class YesNoDialog : public UIComponent {

  protected:

    static String _defaults [];

    void *_clientData;
    DialogCallback _yes;
    DialogCallback _no;
    DialogCallback _cancel;
    DialogCallback _help;

    char *_question;

    virtual void manage();

    static void yesCallback (Widget, XtPointer, XtPointer);
    static void noCallback (Widget, XtPointer, XtPointer);
    static void cancelCallback (Widget, XtPointer, XtPointer);
    static void helpCallback (Widget, XtPointer, XtPointer);

    void yes();
    void no();
    void help();

  public:
    
    YesNoDialog(const char *name, void *, const char *question,
		DialogCallback, DialogCallback = NULL, 
		DialogCallback = NULL, DialogCallback = NULL);
    virtual ~YesNoDialog();

    virtual const char *const className ()  { return "YesNoDialog"; }
};
#endif
