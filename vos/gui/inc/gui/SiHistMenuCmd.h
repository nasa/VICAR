/////////////////////////////////////////////////////////////
// SiHistMenuCmd.h: Include file to handle the HIST command
//                button from the Image widget.
//		This class is also used to deal with the 
//		Stretched Histograms
/////////////////////////////////////////////////////////////
#ifndef SiHistMENUCMD_H
#define SiHistMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class SiHistogram;

class SiHistMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_histWindow;
     SiHistogram *_histR, *_histG, *_histB;
     const char *_title;

  protected:
    
    virtual void doit();

  public:
    
    SiHistMenuCmd ( const char*, const char*, int, SiHistogram*, SiHistogram*, SiHistogram* );
    virtual const char *const className () { return "SiHistMenuCmd"; }
};
#endif
