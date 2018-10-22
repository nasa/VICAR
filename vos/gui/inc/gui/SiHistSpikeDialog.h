//////////////////////////////////////////////////////////////
// SiHistSpikeDialog.h: Include file to create the Spike Dialog Box
//////////////////////////////////////////////////////////////
#ifndef SIHISTSPIKEDIALOG_H
#define SIHISTSPIKEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class SiHistBox;

class SiHistSpikeDialog : public CustomDialog {

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w);}
   
    SiHistBox *_histBox;

  public:

    SiHistSpikeDialog ( SiHistBox *, const char * );
    virtual ~SiHistSpikeDialog() { }

    virtual Widget createWorkArea(Widget);
};
#endif
