//////////////////////////////////////////////////////////
// LogWindow: A dialog window that contains a scrolled 
// text window.
//////////////////////////////////////////////////////////
#ifndef LOGWINDOW_H
#define LOGWINDOW_H

#include "MainWindow.h"

class LogWindow : public MainWindow {

  private:
    
    Boolean _posted;
    Widget _textW;

    static String _defaults[];		// Resource defaults

  protected:
    
    // Called to create the entire dialog and the action area.  Subclasses
    // should generally not override these.
    
    Widget createWorkArea ( Widget );
    
  public:
    
    // The subclass constructor should save what values it needs (Cmd
    // pointers, etc.).  The dialog is not created until it's posted the
    // first time.  Note that the ButtonState's are normally determined by
    // the subclass and are not arguments to the subclass constructor.

    LogWindow(const char *);
    virtual ~LogWindow();

    void post(const char *text);
    void unpost();
    Boolean isPosted();
};
#endif
