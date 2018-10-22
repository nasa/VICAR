//////////////////////////////////////////////////////////////
//SpikeDialog.h: Include file to create the Spike Dialog 
//              Box
//////////////////////////////////////////////////////////////
#ifndef SPIKEDIALOG_H
#define SPIKEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class HistBox;
class Cmd;

class SpikeDialog : public CustomDialog {

  protected:

   Cmd *_SpikeCmd;
   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w);}
   
   HistBox *_histBox;

  public:

    SpikeDialog ( HistBox *, const char * );
    virtual Widget createWorkArea(Widget);

};

#endif
















