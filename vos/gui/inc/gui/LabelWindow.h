////////////////////////////////////////////////////////////////////
//
//     LabelWindow.h: 
//
//     This is a derived from MenuWindow class.
//     It sets up and manages the window for label display.
//
////////////////////////////////////////////////////////////////////
#ifndef LABELWINDOW_H
#define LABELWINDOW_H
#include "MenuWindow.h"

class Cmd;
class TextDisplayView;
class TextDisplayModel;
class ImageData;
class ImageLabel;

class SgSearchTextDialog;
class SgSearchAgainCmd;
class SgSearchTextCmd;

class LabelWindow : public MenuWindow {

  private:

    Widget _form, _textArea;

  protected:

    static String _defaults[];

    SgSearchTextDialog *_searchDialog;
    SgSearchAgainCmd *_searchAgainCmd;
    SgSearchTextCmd *_searchCmd;

    TextDisplayView *_view;
    TextDisplayModel *_textM;
    Cmd *_clearOutputCmd, *_clearEveryRunCmd, *_saveOutputCmd;
    void setLabelMenu( MenuCmdList *, ImageLabel * );

  public:

    Widget createWorkArea ( Widget );
    void   createMenuPanes();
    void   setDefault();
    void reset();
    
    LabelWindow( const char *name, ImageData *image );
    virtual ~LabelWindow();

    virtual const char *const className () { return "LabelWindow"; }

};
#endif
