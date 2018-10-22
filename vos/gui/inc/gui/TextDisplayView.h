/////////////////////////////////////////////////////////////
//
//    TextDisplayView.h
//
//    This is a class derived from BasicImageView.
//    It is the view for text output.
//
/////////////////////////////////////////////////////////////
#ifndef TEXTDISPLAYVIEW_H
#define TEXTDISPLAYVIEW_H

#include "TextDisplayModel.h"
#include <Xm/Xm.h>
#include "UIComponent.h"
#include "BasicImageView.h"
//#include "lists.h"

class CmdInterface;

class TextDisplayView : public BasicImageView {

 private:
    
    static XtResource _resources[];
    static String _defaults[];

    Widget _textW;
    Boolean _clearEveryRun;
    char *_newRunString;

 protected:

    TextDisplayModel *_textM;

 public:
  
    TextDisplayView(const char *, ImageData *);

    virtual ~TextDisplayView();

    void update();
    Widget createTextArea( Widget );

    void registerTextModel(TextDisplayModel *); 
    void unregisterTextModel();
    void addText(char *newText, int size, TextStyle style);
    void setClearEveryRun( Boolean value ) { _clearEveryRun = value; }
    Boolean getClearEveryRun( ) { return _clearEveryRun; }

    virtual const char *const className () { return "TextDisplayView"; }
};
#endif
