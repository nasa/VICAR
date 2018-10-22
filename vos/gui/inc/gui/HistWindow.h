////////////////////////////////////////////////////////////////////
// HistWindow.h: Demonstrate Cmd and MenuBar classes
////////////////////////////////////////////////////////////////////
#ifndef HISTWINDOW_H
#define HISTWINDOW_H
#include "MenuWindow.h"

class Cmd;

class HistBox;
class Histogram;
class Separator;

class HistWindow : public MenuWindow {

  private:

    HistBox *_histBox;
  
  protected:
    
    Histogram *_histR, *_histG, *_histB;

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
    
    Widget _form;
    
  public:
    Widget createWorkArea ( Widget );
    void   createMenuPanes();
    
    HistWindow ( Histogram*, Histogram*, Histogram*, char * );
};
#endif
