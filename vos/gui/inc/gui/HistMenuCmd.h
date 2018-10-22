/////////////////////////////////////////////////////////////
// HistMenuCmd.h: Include file to handle the HIST command
//                button from the Image widget.
//		This class is also used to deal with the 
//		Stretched Histograms
/////////////////////////////////////////////////////////////
#ifndef HISTMENUCMD_H
#define HISTMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Histogram;

class HistMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_histWindow;
     Histogram *_histR, *_histG, *_histB;
     char *_title;

  protected:
    
    virtual void doit();

  public:
    
    HistMenuCmd ( char*, char*, int, Histogram*, Histogram*, Histogram* );
    virtual const char *const className () { return "HistMenuCmd"; }

};
#endif
