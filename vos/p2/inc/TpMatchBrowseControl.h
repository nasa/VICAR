//////////////////////////////////////////////////////////////////////////////
// TpMatchBrowseControl.h: This component allows user to browse through 
// different matches, displays general qualifier for the current match and 
// allows user to change it.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHBROWSECONTROL_H
#define TPMATCHBROWSECONTROL_H
#include "UIComponent.h"
#include "TpQualifier.h" // for TpQualType def

class TpSelectionMgr;
class TpMatchManager;
class TpMatch;

class TpMatchBrowseControl : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    Boolean _enableGotoQual;

    static String _defaults[];

    TpSelectionMgr *_selMgr;
    TpMatchManager *_matchManager;

    Widget _numberField; // Match's running id number
    Widget _valueField;  // General qualifier's value
    Widget _gotoLabel;	// Label for Goto
    Widget _gotoField;	// Goto general qual value

    int _numberInt;

    void setNumberField(char *text);
    void setValueField(char *text);

    static void setNumberCallback(Widget, XtPointer, XtPointer);
    static void setValueCallback(Widget, XtPointer, XtPointer);
    static void gotoQualCallback(Widget, XtPointer, XtPointer);
    static void incNumberCallback(Widget, XtPointer, XtPointer);
    static void decNumberCallback(Widget, XtPointer, XtPointer);

    static void confirm(void *);

    void setNumber();
    void setNumber(int);
    void gotoQual();
    void gotoQual(char *);
    void setValue(Boolean confirmed);
    void displayValue();
    void incNumber();
    void decNumber();

  public:

    TpMatchBrowseControl(Widget parent, const char *name, 
			 TpSelectionMgr *, TpMatchManager *);
    virtual ~TpMatchBrowseControl() { }

    void newMatchSelected(TpMatch *);

    virtual const char *const className() { return "TpMatchBrowseControl"; }
};

#endif
