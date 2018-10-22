//////////////////////////////////////////////////////////////
// HistBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef HISTBTNINTERFACE_H
#define HISTBTNINTERFACE_H
#include "SgDrawAreaInterface.h"

class Histogram;

class HistBtnInterface : public SgDrawAreaInterface {

  public:
    
    HistBtnInterface ( Widget, Cmd*, Histogram*, Histogram*, Histogram* );
};
#endif
