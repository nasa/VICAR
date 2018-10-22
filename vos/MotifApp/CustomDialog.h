//////////////////////////////////////////////////////////
// CustomDialog.h: A base class for custom dialogs.  Intended for use
// with delayed command execution on the Ok/Apply buttons.  Caching
// is not really supported, but an unposted dialog is saved for later use
// (i.e. each object supports one and only one dialog).  The dialog is
// not created until it is needed.
//////////////////////////////////////////////////////////
#ifndef CUSTOMDIALOG_H
#define CUSTOMDIALOG_H

#include "MainWindow.h"

class CmdList;

typedef enum { Visible, Default, Invisible } ButtonState;

class CustomDialog : public MainWindow {
    
  private:
    
    static String _defaults[];		// Resource defaults

    static void okCallback ( Widget, 
			    XtPointer, 
			    XtPointer );
    
    static void applyCallback ( Widget, 
				XtPointer, 
				XtPointer );

    static void resetCallback ( Widget, 
				XtPointer, 
				XtPointer );
    
    static void cancelCallback ( Widget, 
				XtPointer, 
				XtPointer );
    
    static void helpCallback ( Widget, 
			      XtPointer, 
			      XtPointer );
    
  protected:

    virtual void apply();
    virtual void reset();

    Widget makeOneButton(Widget, const char *, ButtonState, XtCallbackProc);

    Boolean _posted;
    
    Widget _body;
    Widget _actionArea;
    Widget _workArea;
    ButtonState _showOk, _showApply, _showReset, _showCancel, _showHelp;
    int _numActionWidgets;
    int _buttonNum;		// temporary used while creating buttons
    Dimension _buttonHeight;

    Widget _ok, _apply, _reset, _cancel, _help;

    // Deferred commands get put on this list

    CmdList *_applyCmdList;

    // Called to create the entire dialog and the action area.  Subclasses
    // should generally not override these.
    
    virtual Widget createDialog ( Widget );
    virtual Widget createBody ( Widget );
    virtual Widget createActionArea ( Widget );
    
    // Called to create the work area of the dialog.  Subclasses must
    // provide this function.

    virtual Widget createWorkArea ( Widget ) = 0;

    // Subclass should override this to provide functionality for Help

    virtual void help (Widget, XtPointer) { }

  public:
    
    // The subclass constructor should save what values it needs (Cmd
    // pointers, etc.).  The dialog is not created until it's posted the
    // first time.  Note that the ButtonState's are normally determined by
    // the subclass and are not arguments to the subclass constructor.

    CustomDialog ( const char *, ButtonState showOk = Default,
			         ButtonState showApply = Visible,
			         ButtonState showReset = Invisible,
			         ButtonState showCancel = Visible,
			         ButtonState showHelp = Visible );

    virtual ~CustomDialog();
    
    virtual void post ();

    virtual void unpost ();

    Boolean isPosted();

    CmdList *getApplyList() { return _applyCmdList; }
};
#endif
