////////////////////////////////////////////////////////////////////
// SiHistWindow.h: Histogram window component
////////////////////////////////////////////////////////////////////
#ifndef SiHistWINDOW_H
#define SiHistWINDOW_H
#include "MenuWindow.h"

class Cmd;
class SiHistogram;
class SiHistBox;
class PopupMenu;

class SiHistWindow : public MenuWindow {

  protected:
    
    SiHistogram *_histR, *_histG, *_histB;
    SiHistBox *_histBox;
    Widget _form;
    PopupMenu *_popup;

    Cmd *_stackNoBlend;
    Cmd *_stackBlend;
    Cmd *_row;
    Cmd *_column;
    Cmd *_spikeDialog;
    Cmd *_stats;
    Cmd *_axis;
    Cmd *_hist;
    Cmd *_horizontal;
    Cmd *_vertical;
    Cmd *_ascending;
    Cmd *_descending;
    Cmd *_logScale;

    virtual Widget createWorkArea ( Widget );
    virtual void createMenuPanes();

  public:

    SiHistWindow ( const char *, SiHistogram*, SiHistogram *, SiHistogram * );
    virtual ~SiHistWindow();
};
#endif
